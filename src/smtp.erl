-module(smtp).

%% SMTP primitives (RFC 5321)

%% generic tcp socket listen / close
-export([listener/1,
         close/1,
         close/2]).

%% internal tcp socket helpers
-export([listen/1,
         listen/2,
         listen/3,
         acceptor/2,
         register/2,
         register/3]).

%% smtp specific
-record(mta, {socket, handle, deliver, extensions, options, state}).
-export([mta/0,
         mta/1,
         mta_getopt/2,
         mta_getopt/3,
         mta_register/2,
         mta_register/3]).

%% internal mta / smtp helpers
-export([mta_accept/1,
         mta_listen/1,
         mta_respond/2,
         mta_handle/2,
         mta_default/3,
         mta_forget/1,
         mta_send/2,
         mta_close/1]).

-export([read_address/1,
         read_command/1,
         read_from_to/1,
         read_data/1,
         format_reply/1]).

%% generic tcp

listener(Port) ->
    spawn_link(fun () -> listen(Port) end).

listen(Port) ->
    listen(Port, [binary, {active, false}, {packet, line}, {send_timeout_close, true}]).

listen(Port, Options) ->
    listen(Port, Options, gen_tcp:listen(Port, Options)).

listen(Port, Options, {ok, Socket}) ->
    receive
        {register, From, Ref, Fun} ->
            From ! {Ref, acceptor(Socket, Fun)},
            listen(Port, Options, {ok, Socket});
        {close, Reason} ->
            gen_tcp:close(Socket),
            exit(Reason)
    end;
listen(_Port, _Options, Error) ->
    exit({listen, Error}).

repeater(Fun) ->
    spawn_link(fun () -> repeat(Fun) end).

repeat(Fun) ->
    Fun(), repeat(Fun).

acceptor(Socket, Fun) ->
    repeater(fun () -> Fun(gen_tcp:accept(Socket)) end).

register(Listener, Fun) ->
    register(Listener, Fun, infinity).

register(Listener, Fun, Timeout) ->
    Ref = make_ref(),
    Listener ! {register, self(), Ref, Fun},
    receive
        {Ref, Acceptor} ->
            {ok, Acceptor}
    after Timeout ->
            {error, timeout}
    end.

close(Listener) ->
    close(Listener, normal).

close(Listener, Reason) ->
    Listener ! {close, Reason}.

%% smtp specific

mta() ->
    mta(#{}).

mta(Conf) when is_function(Conf) ->
    #mta{handle=Conf(handle, fun (_, _, _) -> default end),
         deliver=Conf(deliver, fun (_) -> {"550", "5.3.2 Not configured"} end),
         extensions=Conf(extensions, ["8BITMIME", "PIPELINING", "SMTPUTF8", "STARTTLS"]),
         options=Conf(options, #{}),
         state=Conf(state, #{})};
mta(Conf) ->
    mta(fun (K, D) -> util:get(Conf, K, D) end).

mta_getopt(MTA, accept_timeout) ->
    mta_getopt(MTA, accept_timeout, 5 * 1000);
mta_getopt(MTA, client_timeout) ->
    mta_getopt(MTA, client_timeout, 5 * 60 * 1000);
mta_getopt(MTA, starttls_timeout) ->
    mta_getopt(MTA, starttls_timeout, 5 * 60 * 1000);
mta_getopt(MTA, ssl_options) ->
    mta_getopt(MTA, ssl_options, []);
mta_getopt(MTA, Key) ->
    mta_getopt(MTA, Key, undefined).

mta_getopt(#mta{options=Options}, Key, Default) ->
    util:get(Options, Key, Default).

mta_register(Listener, MTA) ->
    mta_register(Listener, MTA, mta_getopt(MTA, accept_timeout)).

mta_register(Listener, MTA, Timeout) ->
    register(Listener,
             fun ({ok, Socket}) ->
                     mta_accept(MTA#mta{socket={gen_tcp, Socket}});
                 ({error, closed}) ->
                     exit(normal);
                 ({error, timeout}) ->
                     exit(normal);
                 ({error, Reason}) ->
                     exit(Reason)
             end, Timeout).

mta_accept(MTA) ->
    mta_respond(greet, MTA).

mta_listen(#mta{socket={Module, Socket}} = MTA) ->
    mta_respond(Module:recv(Socket, 0, mta_getopt(MTA, client_timeout)), MTA).

mta_respond(Message, MTA) ->
    case mta_handle(Message, MTA) of
        {continue, Reply, NewMTA = #mta{}} ->
            mta_listen(mta_send(Reply, NewMTA));
        {continue, Reply, NewState} ->
            mta_listen(mta_send(Reply, MTA#mta{state=NewState}));
        {stop, Reply, normal} ->
            mta_close(mta_send(Reply, MTA));
        {stop, Reply, Reason} ->
            mta_close(mta_send(Reply, MTA)),
            exit(Reason)
    end.

mta_handle({ok, Packet}, #mta{state=#{data := _Data}} = MTA) ->
    mta_handle(read_data(Packet), MTA);
mta_handle({ok, Packet}, MTA) ->
    mta_handle(read_command(Packet), MTA);
mta_handle(Received, #mta{handle=Handle, state=State} = MTA) ->
    case Handle(Received, State, MTA) of
        default ->
            mta_default(Received, State, MTA);
        Result ->
            Result
    end.

mta_default(greet, State, _MTA) ->
    {continue, {"220", [hostname(), " ESMTP"]}, State};

mta_default({error, closed}, _State, _MTA) ->
    {stop, {"221", "2.0.0 Closed by client, goodbye"}, normal};
mta_default({error, timeout}, _State, _MTA) ->
    {stop, {"451", "4.4.2 Took too long, goodbye"}, normal};
mta_default({error, Reason}, _State, _MTA) ->
    {stop, {"500", "5.0.0 Unexpected failure"}, Reason};

mta_default({"STARTTLS", _Params}, _State, MTA) ->
    SSLOptions = mta_getopt(MTA, ssl_options),
    StartTLSTimeout = mta_getopt(MTA, starttls_timeout),
    case util:has(SSLOptions, [certfile, keyfile]) of
        true ->
            case mta_send({"220", "2.0.0 Ready"}, MTA) of
                #mta{socket={_Module, Socket}} = NewMTA ->
                    case ssl:ssl_accept(Socket, SSLOptions, StartTLSTimeout) of
                        {ok, SSLSocket} ->
                            {continue, noreply, mta_forget(NewMTA#mta{socket={ssl, SSLSocket}})};
                        {error, closed} ->
                            {stop, noreply, normal};
                        {error, timeout} ->
                            {stop, noreply, normal};
                        {error, Reason} ->
                            {stop, noreply, Reason}
                    end
            end;
        false ->
            {continue, {"454", "4.3.3 TLS keys not configured"}, MTA}
    end;

mta_default({"HELO", _Params}, State, _MTA) ->
    {continue, {"250", hostname()}, State};
mta_default({"EHLO", _Params}, State, #mta{extensions=Extensions}) ->
    {continue, [{"250", [hostname(), " at your service"]}|
                [{"250", Ext} || Ext <- Extensions]], State};
mta_default({"QUIT", _Params}, _State, _MTA) ->
    {stop, {"221", "2.0.0 Later"}, normal};

mta_default({"MAIL", Params}, State, _MTA) ->
    case read_from_to(Params) of
        {"FROM", {Address, _Rest}} ->
            Envelope = #{return_path => Address, recipients => [], size => 0},
            {continue, {"250", "2.1.0 OK"}, State#{envelope => Envelope}};
        _ ->
            {continue, {"555", "5.5.3 Syntax error"}, State}
    end;
mta_default({"RCPT", Params}, #{envelope := #{size := Size} = Envelope} = State, _MTA) when Size < 100 ->
    Recipients = util:get(Envelope, recipients, []),
    case read_from_to(Params) of
        {"TO", {Address, _Rest}} ->
            NewEnvelope = Envelope#{recipients => [Address|Recipients], size => Size + 1},
            {continue, {"250", "2.1.5 OK"}, State#{envelope => NewEnvelope}};
        _ ->
            {continue, {"555", "5.5.2 Syntax error"}, State}
    end;
mta_default({"RCPT", _Params}, #{envelope := _Envelope} = State, _MTA) ->
    {continue, {"452", "4.5.2 Too many recipients"}, State};
mta_default({"RCPT", _Params}, State, _MTA) ->
    {continue, {"503", "5.5.1 MAIL first"}, State};

mta_default({"DATA", _Params}, #{envelope := #{return_path := _ReturnPath,
                                               recipients := [_|_]}} = State, _MTA) ->
    {continue, {"354", "2.0.0 Go ahead"}, State#{data => <<>>}};
mta_default({"DATA", _Params}, State, _MTA) ->
    {continue, {"503", "5.5.1 MAIL and RCPT first"}, State};

mta_default({data, <<".\r\n">>}, State, #mta{deliver=Deliver}) ->
    {continue, Deliver(State), maps:without([envelope, data], State)};
mta_default({data, <<".", Line/binary>>}, #{data := Data} = State, _MTA) ->
    {continue, noreply, State#{data => <<Data/binary, Line/binary>>}};
mta_default({data, Line}, #{data := Data} = State, _MTA) ->
    {continue, noreply, State#{data => <<Data/binary, Line/binary>>}};

mta_default({"NOOP", _Params}, State, _MTA) ->
    {continue, {"250", "2.0.0 OK"}, State};
mta_default({"RSET", _Params}, State, _MTA) ->
    {continue, {"250", "2.0.0 Reset"}, maps:without([envelope], State)};
mta_default({"VRFY", _Params}, State, _MTA) ->
    {continue, {"252", "2.1.5 Send some mail, I'll try my best"}, State};

mta_default({_Verb, _Params}, State, _MTA) ->
    {continue, {"502", "5.5.1 Unrecognized command"}, State}.

mta_forget(#mta{state=State} = MTA) ->
    MTA#mta{state=maps:without([envelope, data], State)}.

mta_send(noreply, MTA) ->
    MTA;
mta_send(Reply, #mta{socket={Module, Socket}} = MTA) ->
    Module:send(Socket, format_reply(Reply)),
    MTA.

mta_close(#mta{socket={Module, Socket}}) ->
    Module:close(Socket).

hostname() ->
    util:ok(inet:gethostname()).

strip_crlf(Data) ->
    util:rstrip(util:rstrip(Data, $\n), $\r).

force_crlf(Data) ->
    <<(strip_crlf(Data))/binary, $\r, $\n>>.

read_address(<<$<, Data/binary>>) ->
    case Data of
        <<$@, Rest/binary>> ->
            {_, R1} = util:snap(Rest, <<$:>>),
            read_address(R1, unquoted, <<>>);
        <<Rest/binary>> ->
            read_address(Rest, unquoted, <<>>)
    end;
read_address(_) ->
    undefined.

read_address(<<$\\, C, Rest/binary>>, Quoted, Acc) ->
    read_address(Rest, Quoted, <<Acc/binary, C>>);
read_address(<<$", Rest/binary>>, unquoted, Acc) ->
    read_address(Rest, quoted, Acc);
read_address(<<$", Rest/binary>>, quoted, Acc) ->
    read_address(Rest, unquoted, Acc);
read_address(<<$>, Rest/binary>>, unquoted, Acc) ->
    {util:snap(Acc, <<$@>>), Rest};
read_address(<<C, Rest/binary>>, Quoted, Acc) ->
    read_address(Rest, Quoted, <<Acc/binary, C>>);
read_address(<<>>, _Quoted, _Acc) ->
    undefined.

read_command(Packet) ->
    {Verb, Params} = util:snap(strip_crlf(Packet), <<" ">>),
    {util:upper(util:str(Verb)), Params}.

read_from_to(Params) ->
    {FromTo, Rest} = util:snap(strip_crlf(Params), <<":">>),
    {util:upper(util:str(FromTo)), read_address(Rest)}.

read_data(Packet) ->
    {data, force_crlf(Packet)}.

format_reply([{Code, String},Next|Rest]) when element(1, Next) =:= Code ->
    [util:bin(Code), "-", String, "\r\n"|format_reply([Next|Rest])];
format_reply([{Code, String}]) ->
    [format_reply({Code, String})];
format_reply({Code, String}) ->
    [util:bin(Code), " ", String, "\r\n"].

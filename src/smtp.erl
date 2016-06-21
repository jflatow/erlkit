-module(smtp).
-author("Jared Flatow").

%% SMTP primitives (RFC 5321)

%% smtp types
-export_type([command/0,
              reply/0,
              client/0,
              server/0]).

-type command() :: [string()].
-type reply() :: [string()].

-type client() :: tuple().
-type options() :: #{}.
-type socket() :: {gen_tcp | ssl, gen_tcp:socket() | ssl:sslsocket()}.

-type server() :: tuple().
-type sstate() :: #{}.
-type handler() :: fun((command(), sstate(), server()) ->
                             {continue, reply(), sstate()} |
                             {stop, reply(), term()} |
                             default).
-type deliver() :: fun((sstate(), server()) -> reply()).

-record(client, {
          extensions :: [string()],
          options :: options(),
          socket :: socket()
         }).

-record(server, {
          extensions :: [string()],
          handler :: handler(),
          deliver :: deliver(),
          options :: options(),
          socket :: socket(),
          state :: sstate()
         }).

%% high-level smtp
-export([start_server/1,
         stop_server/1,
         domain_of/1,
         locate_mx/1,
         send_email/1,
         send_email/2]).

%% client / server options
-export([getopt/2,
         getopt/3,
         hasext/2]).

%% low-level smtp client
-export([client/0,
         client/1,
         client_connect/1,
         client_session/1,
         client_quit/1,
         client_send/2,
         client_recv/2,
         client_call/2,
         client_multi/2,
         client_multi/3,
         client_email/2,
         client_close/1,
         client_error/2]).

%% low-level smtp server
-export([server/0,
         server/1,
         server_register/2,
         server_register/3,
         server_accept/1,
         server_listen/1,
         server_respond/2,
         server_handle/2,
         server_default/3,
         server_reset/1,
         server_send/2,
         server_close/1]).

%% smtp helpers
-export([first_fail/1,
         force_crlf/1,
         strip_crlf/1,
         read_address/1,
         read_command/1,
         read_from_to/1,
         read_data/1,
         read_reply_line/1,
         format_address/1,
         format_command/1,
         format_reply/1]).

%% generic dns lookup
-export([dns_lookup/3]).

%% generic tcp helpers
-export([tcp_connect/3,
         tcp_connect/4,
         tcp_listener/1,
         tcp_listen/1,
         tcp_listen/2,
         tcp_listen/3,
         tcp_deafen/1,
         tcp_deafen/2,
         tcp_acceptor/2,
         tcp_register/2,
         tcp_register/3,
         tcp_sockname/1,
         tcp_peername/1]).

%% high-level smtp

domain_of(Addr) ->
    case smtp:read_address(format_address(Addr)) of %% XXX
        undefined ->
            undefined;
        {{_, Domain}, _} ->
            Domain
    end.

locate_mx(Domain) ->
    locate_mx(Domain, 100).

locate_mx(Domain, N) when is_list(Domain) ->
    case dns_lookup(Domain, [in], [mx, cname]) of
        [] ->
            [{0, Domain}];
        [{_, _}|_] = MXs ->
            lists:sort(MXs);
        [CName] when N > 0 ->
            locate_mx(CName, N - 1);
        _ ->
            []
    end;
locate_mx(Domain, N) ->
    locate_mx(util:str(Domain), N).

send_email(Desc) ->
    send_email(Desc, #{}).

send_email(Desc, Client = #client{options=#{host := _}}) ->
    case client_session(Client) of
        {ok, _, C1} ->
            case client_email(C1, Desc) of
                {ok, Replies, C2} ->
                    {ok, Replies, client_quit(C2)};
                {retry, Replies, C2} ->
                    {retry, Replies, client_quit(C2)};
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end;
send_email(Desc = {_From, [To|_], _Content}, Client = #client{options=O}) ->
    case locate_mx(domain_of(To)) of
        [] ->
            {error, {mx, To}, Client};
        [{_, MX}|_] ->
            send_email(Desc, Client#client{options=O#{host => MX}})
    end;
send_email(Desc, Conf) ->
    send_email(Desc, client(Conf)).

start_server(Server = #server{}) ->
    start_server(Server, getopt(Server, port));
start_server(Conf) ->
    start_server(server(Conf)).

start_server(Server, Port) ->
    start_server(Server, Port, getopt(Server, num_acceptors)).

start_server(Server, Port, NumAcceptors) ->
    Listener = tcp_listener(Port),
    _Acceptors = util:count(
                   fun (_, A) ->
                           [server_register(Server, Listener)|A]
                   end, [], NumAcceptors),
    {ok, Listener}.

stop_server(Listener) when is_pid(Listener) ->
    tcp_deafen(Listener, stop), ok.

%% options

getopt(Any, self) ->
    getopt(Any, self, localhost());
getopt(Any, port) ->
    getopt(Any, port, 25);
getopt(Any, ssl_options) ->
    getopt(Any, ssl_options, []);

getopt(Client = #client{}, host) ->
    getopt(Client, host, localhost());
getopt(Client = #client{}, starttls) ->
    getopt(Client, starttls, false);
getopt(Client = #client{}, {connect, timeout}) ->
    getopt(Client, {connect, timeout}, 10 * 1000);
getopt(Client = #client{}, {"DATA", timeout}) ->
    getopt(Client, {"DATA", timeout}, 2 * 60 * 1000);
getopt(Client = #client{}, {data, timeout}) ->
    getopt(Client, {data, timeout}, 10 * 60 * 1000);
getopt(Client = #client{}, {Verb, timeout}) ->
    getopt(Client, {Verb, timeout}, 5 * 60 * 1000);

getopt(Server = #server{}, num_acceptors) ->
    getopt(Server, num_acceptors, 16);
getopt(Server = #server{}, {accept, timeout}) ->
    getopt(Server, {accept, timeout}, 5 * 1000);
getopt(Server = #server{}, {client, timeout}) ->
    getopt(Server, {client, timeout}, 5 * 60 * 1000);
getopt(Server = #server{}, {starttls, timeout}) ->
    getopt(Server, {starttls, timeout}, 5 * 60 * 1000);

getopt(Any, Key) ->
    getopt(Any, Key, undefined).

getopt(#client{options=Options}, Key, Default) ->
    util:get(Options, Key, Default);
getopt(#server{options=Options}, Key, Default) ->
    util:get(Options, Key, Default).

hasext(#client{extensions=Extensions}, Key) ->
    hasext(Extensions, Key);
hasext(#server{extensions=Extensions}, Key) ->
    hasext(Extensions, Key);
hasext(Extensions, Key) ->
    util:has(util:def(Extensions, []), Key).

%% low-level smtp client

client() ->
    client(#{}).

client(Conf) when is_function(Conf) ->
    #client{options=Conf(options, #{})};
client(Conf) ->
    client(fun (K, D) -> util:get(Conf, K, D) end).

client_connect(Client = #client{}) ->
    client_connect(Client, getopt(Client, host));
client_connect(Conf) ->
    client_connect(client(Conf)).

client_connect(Client = #client{}, Host) ->
    client_connect(Client, Host, getopt(Client, port)).

client_connect(Client, Host, Port) ->
    client_connect(Client, Host, Port, getopt(Client, {connect, timeout})).

client_connect(Client, Host, Port, Timeout) ->
    case tcp_connect(Host, Port, Timeout) of
        {ok, Socket} ->
            client_recv(Client#client{socket={gen_tcp, Socket}}, [greet]);
        {error, Reason} ->
            {error, {connect, Reason}, Client}
    end.

client_session(Client) ->
    case client_connect(Client) of
        {ok, ["220"|_], C1} ->
            StartTLS = getopt(C1, starttls),
            case client_hello(C1) of
                {ok, ["250"|_], C2} when StartTLS ->
                    client_starttls(C2);
                {_, _, _} = Other ->
                    Other
            end;
        {ok, Reply, C1} ->
            client_error(C1, {greet, Reply});
        {error, _, _} = Error ->
            Error
    end.

client_hello(Client) ->
    case client_call(Client, ["EHLO", getopt(Client, self)]) of
        {ok, ["250"|_], _} = Result ->
            Result;
        {ok, [Code|_], C1} when Code =:= "502"; Code =:= "550" ->
            case client_call(C1, ["HELO", getopt(Client, self)]) of
                {ok, ["250"|_], _} = Result ->
                    Result;
                {ok, Reply, C2} ->
                    client_error(C2, {helo, Reply});
                {error, _, _} = Error ->
                    Error
            end;
        {ok, Reply, C1} ->
            client_error(C1, {ehlo, Reply});
        {error, _, _} = Error ->
            Error
    end.

client_starttls(Client) ->
    SSLOptions = getopt(Client, ssl_options),
    StartTLSTimeout = getopt(Client, {starttls, timeout}),
    case hasext(Client, "STARTTLS") of
        true ->
            case client_call(Client, ["STARTTLS"]) of
                {ok, ["220"|_], C1 = #client{socket={_Module, Socket}}} ->
                    case ssl:connect(Socket, SSLOptions, StartTLSTimeout) of
                        {ok, SSLSocket} ->
                            client_hello(C1#client{socket={ssl, SSLSocket}});
                        {error, Reason} ->
                            client_error(C1, {ssl_connect, Reason})
                        end;
                {ok, Reply, C1} ->
                    client_error(C1, {starttls, Reply});
                {error, _, _} = Error ->
                    Error
            end;
        false ->
            Client
    end.

client_quit(Client) ->
    client_close(client_send(Client, ["QUIT"])).

client_send(Client = #client{socket={Module, Socket}}, Command) ->
    Module:send(Socket, format_command(Command)),
    Client.

client_recv(Client, Command = [Verb|_]) ->
    client_recv(Client, Command, getopt(Client, {Verb, timeout})).

client_recv(Client, Command = [Verb|_], Timeout) ->
    case client_pile(Client, Timeout, []) of
        Reply = [_, _|Extensions] when Verb =:= "EHLO" ->
            {ok, Reply, Client#client{extensions=[util:str(E) || E <- Extensions]}};
        Reply when is_list(Reply) ->
            {ok, Reply, Client};
        {error, Reason} ->
            client_error(Client, {Command, Reason})
    end.

client_pile(Client = #client{socket={Module, Socket}}, Timeout, Acc) ->
    case Module:recv(Socket, 0, Timeout) of
        {ok, Packet} ->
            case read_reply_line(Packet) of
                {_, Text, more} ->
                    client_pile(Client, Timeout, [Text|Acc]);
                {Code, Text} ->
                    [Code|lists:reverse([Text|Acc])]
            end;
        {error, Reason} ->
            {error, Reason}
    end.

client_call(Client, Command) ->
    client_recv(client_send(Client, Command), Command).

client_multi(Client, Commands) ->
    client_multi(Client, Commands,
                 case hasext(Client, "PIPELINING") of
                     true ->
                         pipeline;
                     false ->
                         sequence
                 end).

client_multi(Client, Commands, sequence) ->
    client_multi(Client, Commands, fun client_call/2);
client_multi(Client, Commands, pipeline) ->
    client_multi(util:reduce(fun client_send/2, Client, Commands), Commands, fun client_recv/2);
client_multi(Client, Commands, Fun) when is_function(Fun) ->
    lists:foldl(
      fun (Command, {ok, Replies, C}) ->
              case Fun(C, Command) of
                  {ok, Reply, C1} ->
                      {ok, [Reply|Replies], C1};
                  {error, Reason, C1} ->
                      {error, [Reason|Replies], C1}
              end;
          (_Command, Error) ->
              Error
      end, {ok, [], Client}, Commands).

client_email(Client, {From, To, Content}) ->
    case client_multi(Client,
                      [["RSET"],
                       ["MAIL", ["FROM:", format_address(From)]]|
                      [["RCPT", ["TO:", format_address(T)]] || T <- To] ++
                      [["DATA"]]]) of
        {ok, [["354"|_]|_] = Replies, C1} ->
            case client_call(C1, [data, Content]) of
                {ok, ["250"|_] = Reply, C2} ->
                    {ok, [Reply|Replies], C2};
                {error, _, _} = Error ->
                    Error
            end;
        {ok, Replies, C1} ->
            case first_fail(lists:reverse(Replies)) of
                [[$4|_]|_] ->
                    {retry, Replies, C1};
                [[$5|_]|_] ->
                    client_error(C1, {email, Replies})
            end;
        {error, _, _} = Error ->
            Error
    end.

client_close(#client{socket={Module, Socket}} = Client) ->
    Module:close(Socket),
    Client.

client_error(Client, Reason) ->
    {error, Reason, client_close(Client)}.

%% low-level smtp server

server() ->
    server(#{}).

server(Conf) when is_function(Conf) ->
    #server{extensions=Conf(extensions, ["8BITMIME", "PIPELINING", "SMTPUTF8", "STARTTLS"]),
            handler=Conf(handler, fun (_, _, _) -> default end),
            deliver=Conf(deliver, fun (_, _) -> ["550", "5.3.2 Not configured"] end),
            options=Conf(options, #{}),
            state=Conf(state, #{})};
server(Conf) ->
    server(fun (K, D) -> util:get(Conf, K, D) end).

server_register(Server, Listener) ->
    server_register(Server, Listener, getopt(Server, {accept, timeout})).

server_register(Server, Listener, Timeout) ->
    tcp_register(Listener,
                 fun ({ok, Socket}) ->
                         server_accept(Server#server{socket={gen_tcp, Socket}});
                     ({error, closed}) ->
                         exit(normal);
                     ({error, timeout}) ->
                         exit(normal);
                     ({error, Reason}) ->
                         exit(Reason)
                 end, Timeout).

server_accept(Server) ->
    server_respond(greeting, Server).

server_listen(#server{socket={Module, Socket}} = Server) ->
    server_respond(Module:recv(Socket, 0, getopt(Server, {client, timeout})), Server).

server_respond(Message, Server) ->
    case server_handle(Message, Server) of
        {continue, Reply, NewServer = #server{}} ->
            server_listen(server_send(NewServer, Reply));
        {continue, Reply, NewState} ->
            server_listen(server_send(Server#server{state=NewState}, Reply));
        {stop, Reply, normal} ->
            server_close(server_send(Server, Reply));
        {stop, Reply, Reason} ->
            server_close(server_send(Server, Reply)),
            exit(Reason)
    end.

server_handle({ok, Packet}, #server{state=#{content := _}} = Server) ->
    server_handle(read_data(Packet), Server);
server_handle({ok, Packet}, Server) ->
    server_handle(read_command(Packet), Server);
server_handle(Received, #server{handler=Handler, state=State} = Server) ->
    case Handler(Received, State, Server) of
        default ->
            server_default(Received, State, Server);
        Result ->
            Result
    end.

server_default(greeting, State, Server) ->
    {continue, ["220", [getopt(Server, self), " ESMTP"]], State};

server_default({error, closed}, _State, _Server) ->
    {stop, ["221", "2.0.0 Closed by client, goodbye"], normal};
server_default({error, timeout}, _State, _Server) ->
    {stop, ["451", "4.4.2 Took too long, goodbye"], normal};
server_default({error, Reason}, _State, _Server) ->
    {stop, ["500", "5.0.0 Unexpected failure"], Reason};

server_default(["STARTTLS"|_], _State, Server) ->
    SSLOptions = getopt(Server, ssl_options),
    StartTLSTimeout = getopt(Server, {starttls, timeout}),
    case util:hasall(SSLOptions, [certfile, keyfile]) of
        true ->
            case server_send(Server, ["220", "2.0.0 Ready"]) of
                #server{socket={_Module, Socket}} = NewServer ->
                    case ssl:ssl_accept(Socket, SSLOptions, StartTLSTimeout) of
                        {ok, SSLSocket} ->
                            {continue, noreply, server_reset(NewServer#server{socket={ssl, SSLSocket}})};
                        {error, closed} ->
                            {stop, noreply, normal};
                        {error, timeout} ->
                            {stop, noreply, normal};
                        {error, Reason} ->
                            {stop, noreply, Reason}
                    end
            end;
        false ->
            {continue, ["454", "4.3.3 TLS keys not configured"], Server}
    end;

server_default(["HELO"|_], State, Server) ->
    {continue, ["250", getopt(Server, self)], server_reset(State)};
server_default(["EHLO"|_], State, Server = #server{extensions=Extensions}) ->
    {continue, ["250", [getopt(Server, self), " at your service"]|Extensions], server_reset(State)};
server_default(["QUIT"|_], _State, _Server) ->
    {stop, ["221", "2.0.0 Later"], normal};

server_default(["MAIL", From|_], State, _Server) ->
    case read_from_to(From) of
        {"FROM", {Address, _Rest}} ->
            Envelope = #{return_path => Address, recipients => [], size => 0},
            {continue, ["250", "2.1.0 OK"], State#{envelope => Envelope}};
        _ ->
            {continue, ["555", "5.5.3 Syntax error"], State}
    end;
server_default(["RCPT", To|_], #{envelope := #{size := Size} = Envelope} = State, _Server) when Size < 100 ->
    Recipients = util:get(Envelope, recipients, []),
    case read_from_to(To) of
        {"TO", {Address, _Rest}} ->
            NewEnvelope = Envelope#{recipients => [Address|Recipients], size => Size + 1},
            {continue, ["250", "2.1.5 OK"], State#{envelope => NewEnvelope}};
        _ ->
            {continue, ["555", "5.5.2 Syntax error"], State}
    end;
server_default(["RCPT"|_], #{envelope := _Envelope} = State, _Server) ->
    {continue, ["452", "4.5.2 Too many recipients"], State};
server_default(["RCPT"|_], State, _Server) ->
    {continue, ["503", "5.5.1 MAIL first"], State};

server_default(["DATA"|_], #{envelope := #{
                                      return_path := _ReturnPath,
                                      recipients := [_|_]}
                                   } = State, _Server) ->
    {continue, ["354", "2.0.0 Go ahead"], State#{content => <<>>}};
server_default(["DATA"|_], State, _Server) ->
    {continue, ["503", "5.5.1 MAIL and RCPT first"], State};

server_default([data, <<".\r\n">>], State, #server{deliver=Deliver} = Server) ->
    {continue, Deliver(State, Server), maps:without([envelope, content], State)};
server_default([data, <<".", Line/binary>>], #{content := Data} = State, _Server) ->
    {continue, noreply, State#{content => <<Data/binary, Line/binary>>}};
server_default([data,  Line], #{content := Data} = State, _Server) ->
    {continue, noreply, State#{content => <<Data/binary, Line/binary>>}};

server_default(["NOOP"|_], State, _Server) ->
    {continue, ["250", "2.0.0 OK"], State};
server_default(["RSET"|_], State, _Server) ->
    {continue, ["250", "2.0.0 Reset"], server_reset(State)};
server_default(["VRFY"|_], State, _Server) ->
    {continue, ["252", "2.1.5 Send some mail, I'll try my best"], State};

server_default([_Verb|_Params], State, _Server) ->
    {continue, ["502", "5.5.1 Unrecognized command"], State}.

server_reset(#server{state=State} = Server) ->
    Server#server{state=server_reset(State)};
server_reset(#{} = State) ->
    maps:without([envelope, content], State).

server_send(Server, noreply) ->
    Server;
server_send(#server{socket={Module, Socket}} = Server, Reply) ->
    Module:send(Socket, format_reply(Reply)),
    Server.

server_close(#server{socket={Module, Socket}} = Server) ->
    Module:close(Socket),
    Server.

%% protocol helpers

localhost() ->
    net_adm:localhost().

first_fail(Replies) ->
    util:first(Replies, fun ([[$2|_]|_]) -> false; (_) -> true end).

force_crlf(Data) ->
    <<(strip_crlf(Data))/binary, $\r, $\n>>.

strip_crlf(Data) ->
    str:rstrip(str:rstrip(Data, $\n), $\r).

read_address(<<$<, Data/binary>>) ->
    case Data of
        <<$@, Rest/binary>> ->
            {_, R1} = str:snap(Rest, <<$:>>),
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
    {str:snap(Acc, <<$@>>), Rest};
read_address(<<C, Rest/binary>>, Quoted, Acc) ->
    read_address(Rest, Quoted, <<Acc/binary, C>>);
read_address(<<>>, _Quoted, _Acc) ->
    undefined.

read_command(Packet) ->
    {Verb, Params} = str:snap(strip_crlf(Packet), <<" ">>),
    [str:upper(util:str(Verb)), Params].

read_from_to(Params) ->
    {FromTo, Rest} = str:snap(strip_crlf(Params), <<":">>),
    {str:upper(util:str(FromTo)), read_address(Rest)}.

read_data(Packet) ->
    [data, force_crlf(Packet)].

read_reply_line(<<Code:3/binary, "-", Text/binary>>) ->
    {util:str(Code), strip_crlf(Text), more};
read_reply_line(<<Packet/binary>>) ->
    {Code, Text} = str:snap(strip_crlf(Packet), <<" ">>),
    {util:str(Code), Text}.

format_address({_Name, Address}) ->
    format_address(Address);
format_address(Address) ->
    <<$<, (util:bin(Address))/binary, $>>>.

format_command([data|Data]) ->
    [util:join(Data, "\r\n"), "\r\n.\r\n"];
format_command([Verb|Params]) ->
    [util:join([Verb|Params], " "), "\r\n"].

format_reply([Code, String]) ->
    [util:bin(Code), " ", String, "\r\n"];
format_reply([Code, String|Rest]) ->
    [util:bin(Code), "-", String, "\r\n"|format_reply([Code|Rest])].

%% generic dns

dns_lookup(Name, Classes, Types) ->
    case inet_res:resolve(Name, any, any) of
        {ok, Msg} ->
            [inet_dns:rr(RR, data)
             || RR <- inet_dns:msg(Msg, anlist),
                util:has(Types, inet_dns:rr(RR, type)),
                util:has(Classes, inet_dns:rr(RR, class))];
        {error, _} = Error ->
            Error
    end.

%% generic tcp

tcp_connect(Host, Port, Timeout) ->
    tcp_connect(Host, Port, [binary, {active, false}, {packet, line}, {send_timeout_close, true}], Timeout).

tcp_connect(Host, Port, Options, Timeout) ->
    gen_tcp:connect(Host, Port, Options, Timeout).

tcp_listener(Port) ->
    spawn_link(fun () -> tcp_listen(Port) end).

tcp_listen(Port) ->
    tcp_listen(Port, [binary, {active, false}, {packet, line}, {send_timeout_close, true}]).

tcp_listen(Port, Options) ->
    tcp_listen(Port, Options, gen_tcp:listen(Port, Options)).

tcp_listen(Port, Options, {ok, Socket}) ->
    receive
        {register, From, Ref, Fun} ->
            From ! {Ref, tcp_acceptor(Socket, Fun)},
            tcp_listen(Port, Options, {ok, Socket});
        {close, Reason} ->
            gen_tcp:close(Socket),
            exit(Reason)
    end;
tcp_listen(_Port, _Options, {error, Reason}) ->
    exit({error, {listen, Reason}}).

tcp_deafen(Listener) ->
    tcp_deafen(Listener, normal).

tcp_deafen(Listener, Reason) ->
    Listener ! {close, Reason}.

tcp_acceptor(Socket, Fun) ->
    proc:repeater(fun () -> Fun(gen_tcp:accept(Socket)) end).

tcp_register(Listener, Fun) ->
    tcp_register(Listener, Fun, infinity).

tcp_register(Listener, Fun, Timeout) ->
    Ref = make_ref(),
    Listener ! {register, self(), Ref, Fun},
    receive
        {Ref, Acceptor} ->
            {ok, Acceptor}
    after Timeout ->
            {error, timeout}
    end.

tcp_sockname({gen_tcp, Socket}) ->
    inet:sockname(Socket);
tcp_sockname({ssl, Socket}) ->
    ssl:sockname(Socket);
tcp_sockname(_) ->
    undefined.

tcp_peername({gen_tcp, Socket}) ->
    inet:peername(Socket);
tcp_peername({ssl, Socket}) ->
    ssl:peername(Socket);
tcp_peername(_) ->
    undefined.

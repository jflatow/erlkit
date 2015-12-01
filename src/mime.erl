-module(mime).
-author("Jared Flatow").

%% Basic Internet Message / MIME parsing (RFC 5322 + 204{5,6,7})

-export([fold/3,
         parse/1,
         parse/2,
         parse/3,
         format/1,
         format/2]).

-export([boundary/1,
         boundary/2,
         header/2,
         header/3,
         date/1,
         from/1,
         sender/1,
         reply_to/1,
         to/1,
         cc/1,
         bcc/1,
         message_id/1,
         in_reply_to/1,
         references/1,
         subject/1,
         comments/1,
         keywords/1,
         content_type/1,
         content_transfer_encoding/1,
         contents/1,
         find_type/2,
         find_text/1,
         decode/1,
         decode/2,
         encode/1,
         encode/2,
         decode_word/1,
         encode_word/1]).

-export([normalize/1,
         read/2,
         maybe/2,
         either/2,
         skip_cfws/1,
         skip_comment/1,
         skip_spaces/1,
         wrap/2,
         wrap/3,
         wrap/4,
         split_headers/1,
         split_parts/1]).

-import(util, [bin/1,
               int/1,
               hexdigit/1,
               unhexdigit/1]).

-define(TAB, $\t).
-define(SP, $\s).
-define(CR, $\r).
-define(LF, $\n).
-define(CRLF, "\r\n").
-define(Q, $\").
-define(IS_ALPHA(C), ((C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z))).
-define(IS_DIGIT(C), ((C >= $0 andalso C =< $9))).
-define(IS_FWS(C), ((C >= 9 andalso C =< 13) orelse C =:= 32)).
-define(IS_NO_WS_CTL(C), ((C >= 1 andalso C =< 8) orelse
                          (C >= 11 andalso C =< 12) orelse
                          (C >= 14 andalso C =< 31) orelse C =:= 127)).
-define(IS_PRINTABLE(C), ((C >= ?SP andalso C =< $~ andalso C =/= $=))).

-define(IS_ATEXT(C), (?IS_ALPHA(C) orelse ?IS_DIGIT(C) orelse
                      C =:= $! orelse C =:= $# orelse
                      C =:= $$ orelse C =:= $% orelse
                      C =:= $& orelse C =:= $' orelse
                      C =:= $* orelse C =:= $+ orelse
                      C =:= $- orelse C =:= $/ orelse
                      C =:= $= orelse C =:= $? orelse
                      C =:= $^ orelse C =:= $_ orelse
                      C =:= $` orelse C =:= ${ orelse
                      C =:= $| orelse C =:= $} orelse
                      C =:= $~)).
-define(IS_DTEXT(C), (?IS_NO_WS_CTL(C) orelse
                      (C >= $! andalso C =< $Z) orelse
                      (C >= $^ andalso C =< $~))).

normalize(Name) when is_atom(Name) ->
    normalize(atom_to_list(Name));
normalize(Name) when is_binary(Name) ->
    normalize(binary_to_list(Name));
normalize(Name) ->
    string:to_lower(Name).

read_atext(<<C, Rest/binary>>, Acc) when ?IS_ATEXT(C) ->
    read_atext(Rest, <<Acc/binary, C>>);
read_atext(Rest, Acc) ->
    {Acc, Rest}.

read_dot_atext(<<C, Rest/binary>>, Acc) when ?IS_ATEXT(C) orelse C =:= $. ->
    read_dot_atext(Rest, <<Acc/binary, C>>);
read_dot_atext(Rest, Acc) ->
    {Acc, Rest}.

read_comment(<<$(, Rest/binary>>) ->
    read_comment(Rest, <<>>).

read_comment(<<$), Rest/binary>>, Acc) ->
    {Acc, Rest};
read_comment(<<C, Rest/binary>>, Acc) ->
    read_comment(Rest, <<Acc/binary, C>>).

read_quoted_string(<<?Q, Rest/binary>>) ->
    read_quoted_string(Rest, <<>>).

read_quoted_string(<<?Q, Rest/binary>>, Acc) ->
    {Acc, Rest};
read_quoted_string(<<$\\, C, Rest/binary>>, Acc) ->
    read_quoted_string(Rest, <<Acc/binary, C>>);
read_quoted_string(<<C, Rest/binary>>, Acc) ->
    read_quoted_string(Rest, <<Acc/binary, C>>).

read_angle_addr(<<$<, Rest/binary>>) ->
    read_angle_addr(Rest, <<>>).

read_angle_addr(<<$>, Rest/binary>>, Acc) ->
    {Addr, <<>>} = read(addr_spec, Acc),
    {Addr, Rest};
read_angle_addr(<<C, Rest/binary>>, Acc) ->
    read_angle_addr(Rest, <<Acc/binary, C>>).

read_domain_literal(<<$[, Rest/binary>>) ->
    read_domain_literal(Rest, <<>>).

read_domain_literal(<<$], Rest/binary>>, Acc) ->
    {Acc, Rest};
read_domain_literal(<<$\\, C, Rest/binary>>, Acc) ->
    read_domain_literal(Rest, <<Acc/binary, C>>);
read_domain_literal(<<C, Rest/binary>>, Acc) when ?IS_DTEXT(C); ?IS_FWS(C) ->
    read_domain_literal(Rest, <<Acc/binary, C>>).

read(atom, Data) ->
    case read_atext(skip_cfws(Data), <<>>) of
        {Atom, R0} when Atom =/= <<>> ->
            {Atom, skip_cfws(R0)}
    end;

read(dot_atom, Data) ->
    case read_dot_atext(skip_cfws(Data), <<>>) of
        {Atom, R0} when Atom =/= <<>> ->
            {Atom, skip_cfws(R0)}
    end;

read(word, Data) ->
    either([atom, quoted_string], Data);

read(phrase, Data) ->
    repeat(word, Data, {1, infinity});

read(comment, Data) ->
    read_comment(Data);

read(quoted_string, Data) ->
    read_quoted_string(Data);

read(address, Data) ->
    either([mailbox, group], Data, true);

read(mailbox, Data) ->
    case either([name_addr, addr_spec], Data, true) of
        {{name_addr, NameAddr}, Rest} ->
            {NameAddr, Rest};
        {{addr_spec, Addr}, Rest} ->
            {{undefined, Addr}, Rest}
    end;

read(name_addr, Data) ->
    {Name, R0} = maybe(display_name, Data),
    {Addr, R1} = read(angle_addr, R0),
    {{Name, Addr}, R1};

read(angle_addr, Data) ->
    {Addr, R0} = read_angle_addr(skip_cfws(Data)),
    {Addr, skip_cfws(R0)};

read(group, Data) ->
    {Name, <<$:, R0/binary>>} = read(display_name, Data),
    {List, <<$;, R1/binary>>} = maybe({list, mailbox}, skip_cfws(R0), []),
    {{Name, List}, skip_cfws(R1)};

read(display_name, Data) ->
    read(phrase, Data);

read(addr_spec, Data) ->
    {LocalPart, <<$@, R0/binary>>} = read(local_part, Data),
    {Domain, R1} = read(domain, R0),
    {{LocalPart, Domain}, R1};

read(local_part, Data) ->
    either([dot_atom, quoted_string], Data);

read(domain, Data) ->
    either([dot_atom, domain_literal], Data);

read(domain_literal, Data) ->
    {DomainLiteral, R0} = read_domain_literal(skip_cfws(Data)),
    {DomainLiteral, skip_cfws(R0)};

read(msg_id, Data) ->
    read(angle_addr, Data); %% close enough

read(msg_ids, Data) ->
    repeat(msg_id, Data, {1, infinity});

read(datetime, Timestamp) ->
    {_DoW, R0} = read(dow, skip_spaces(Timestamp)),
    {Date, R1} = read(date, skip_spaces(R0)),
    {Time, R2} = read(time, skip_spaces(R1)),
    {Offs, R3} = read(offs, skip_spaces(R2)),
    {{{Date, Time}, Offs}, skip_cfws(R3)};

read(dow, <<DoW:3/binary, ",", Rest/binary>>) ->
    {DoW, Rest};
read(dow, Timestamp) ->
    {<<>>, Timestamp};

read(date, Timestamp) ->
    {D, R0} = read(day, skip_spaces(Timestamp)),
    {M, R1} = read(month, skip_spaces(R0)),
    {Y, R2} = read(year, skip_spaces(R1)),
    {{Y, M, D}, R2};

read(day, <<A, B, Rest/binary>>) when ?IS_DIGIT(A), ?IS_DIGIT(B) ->
    {int([A, B]), Rest};
read(day, <<A, Rest/binary>>) ->
    {int([A]), Rest};

read(month, <<M:3/binary, Rest/binary>>) ->
    {case M of
         <<"Jan">> -> 1;
         <<"Feb">> -> 2;
         <<"Mar">> -> 3;
         <<"Apr">> -> 4;
         <<"May">> -> 5;
         <<"Jun">> -> 6;
         <<"Jul">> -> 7;
         <<"Aug">> -> 8;
         <<"Sep">> -> 9;
         <<"Oct">> -> 10;
         <<"Nov">> -> 11;
         <<"Dec">> -> 12
     end, Rest};

read(year, <<Y:4/binary, Rest/binary>>) ->
    {int(Y), Rest};

read(time, <<H:2/binary, ":", Mi:2/binary, ":", S:2/binary, Rest/binary>>) ->
    {{int(H), int(Mi), int(S)}, Rest};

read(offs, <<"+", H:2/binary, Mi:2/binary, Rest/binary>>) ->
    {[{-int(H), hours}, {-int(Mi), minutes}], Rest};
read(offs, <<"-", H:2/binary, Mi:2/binary, Rest/binary>>) ->
    {[{int(H), hours}, {int(Mi), minutes}], Rest};

read({list, Symbol}, Data) ->
    {Head, R0} = read(Symbol, Data),
    {Tail, R1} = repeat({comma, Symbol}, R0),
    {[Head|Tail], R1};

read({tokens, Symbol}, Data) ->
    {Head, R0} = read(Symbol, Data),
    {Tail, R1} = repeat(Symbol, R0),
    {[Head|Tail], R1};

read({comma, Symbol}, <<",", Data/binary>>) ->
    read(Symbol, Data).

maybe(Symbol, Data) ->
    maybe(Symbol, Data, undefined).

maybe(Symbol, Data, Default) ->
    abnf:maybe(fun read/2, Symbol, Data, Default).

repeat(Symbol, Data) ->
    repeat(Symbol, Data, {0, infinity}).

repeat(Symbol, Data, Bounds) ->
    abnf:repeat(fun read/2, Symbol, Data, Bounds).

either(Symbols, Data) ->
    either(Symbols, Data, false).

either(Symbols, Data, Tagged) ->
    abnf:either(fun read/2, Symbols, Data, Tagged).

skip_cfws(Data) ->
    skip_spaces(skip_comment(skip_spaces(Data))).

skip_comment(<<$(, _/binary>> = Data) ->
    element(2, read(comment, Data));
skip_comment(Data) ->
    Data.

skip_spaces(<<C, Rest/binary>>) when ?IS_FWS(C) ->
    skip_spaces(Rest);
skip_spaces(Binary) ->
    Binary.

wrap(Data, Every) ->
    wrap(Data, Every, <<?CRLF>>).

wrap(Data, Every, Break) ->
    wrap(Data, Every, Break, Break).

wrap(Data, Every, Break, Final) ->
    wrap(Data, Every, Break, Final, <<>>).

wrap(Data, Every, Break, Final, Acc) when size(Data) > Every ->
    <<Buf:Every/binary, Rest/binary>> = Data,
    wrap(Rest, Every, Break, Final, <<Acc/binary, Buf/binary, Break/binary>>);
wrap(Data, _, _, Final, Acc) ->
    <<Acc/binary, Data/binary, Final/binary>>.

boundary(Headerish) ->
    boundary(Headerish, error).

boundary(Headerish, Default) ->
    case content_type(Headerish) of
        {"multipart/" ++ _, Params} ->
            proplists:get_value("boundary", Params, Default);
        {_Type, _Params} ->
            error
    end.

header(Headerish, Name) ->
    header(Headerish, Name, undefined).

header(Headerish, Name, Default) ->
    header(Headerish, normalize(Name), Default, norm).

header({Headers, _Body}, Name, Default, Switch) ->
    header(Headers, Name, Default, Switch);
header([{Field, Value}|Headers], Name, Default, norm) ->
    case normalize(Field) of
        Name ->
            bin(Value);
        _ ->
            header(Headers, Name, Default, norm)
    end;
header([], _, Default, norm) ->
    Default.

date(Headerish) ->
    parse(datetime, header(Headerish, "date")).

from(Headerish) ->
    parse({list, mailbox}, header(Headerish, "from")).

sender(Headerish) ->
    parse(mailbox, header(Headerish, "sender", <<>>), undefined).

reply_to(Headerish) ->
    parse({list, address}, header(Headerish, "reply-to", <<>>), []).

to(Headerish) ->
    parse({list, address}, header(Headerish, "to", <<>>), []).

cc(Headerish) ->
    parse({list, address}, header(Headerish, "cc", <<>>), []).

bcc(Headerish) ->
    parse({list, address}, header(Headerish, "bcc", <<>>), []).

message_id(Headerish) ->
    parse(msg_id, header(Headerish, "message-id", <<>>), undefined).

in_reply_to(Headerish) ->
    parse({tokens, msg_id}, header(Headerish, "in-reply-to", <<>>), []).

references(Headerish) ->
    parse({tokens, msg_id}, header(Headerish, "references", <<>>), []).

subject(Headerish) ->
    skip_spaces(header(Headerish, "subject", <<>>)).

comments(Headerish) ->
    skip_spaces(header(Headerish, "comments", <<>>)).

keywords(Headerish) ->
    parse({tokens, msg_id}, header(Headerish, "keywords", <<>>), []).

content_type(Headerish) ->
    content_type(header(Headerish, "content-type"), undefined, []).

content_type(undefined, undefined, []) ->
    {"text/plain", []};
content_type(Raw, undefined, Params) ->
    [Type|Rest] = binary:split(skip_spaces(Raw), <<";">>),
    content_type(Rest, normalize(Type), Params);
content_type([], Type, Params) ->
    {Type, Params};
content_type([Param], Type, Params) ->
    case binary:split(skip_spaces(Param), <<"=">>) of
        [<<>>] ->
            content_type([], Type, Params);
        [Attribute, <<?Q, _/binary>> = Rest0] ->
            {Value,Rest1} = read_quoted_string(Rest0),
            [_Xtra|Rest2] = binary:split(Rest1, <<";">>),
            content_type(Rest2, Type, [{normalize(Attribute), Value}|Params]);
        [Attribute, Rest0] ->
            [Value|Rest1] = binary:split(Rest0, <<";">>),
            content_type(Rest1, Type, [{normalize(Attribute), Value}|Params])
    end.

content_transfer_encoding(Headerish) ->
    normalize(skip_spaces(header(Headerish, "content-transfer-encoding", "7bit"))).

contents({Headers, Body}) ->
    mimetype:decode(content_type(Headers), Body).

find_type(Type, {Headers, Body}) ->
    case content_type(Headers) of
        {Type, _Params} ->
            Body;
        _ ->
            find_type(Type, Body)
    end;
find_type(Type, {Preamble, [Part|Parts], Epilogue, Boundary}) ->
    case find_type(Type, Part) of
        undefined ->
            find_type(Type, {Preamble, Parts, Epilogue, Boundary});
        Found ->
            Found
    end;
find_type(_Type, _) ->
    undefined.

find_text(Body) ->
    find_type("text/plain", Body).

decode({Headers, Body}) ->
    decode(content_transfer_encoding(Headers), Body).

decode("base64", Body) ->
    case catch base64:decode(skip_spaces(Body)) of
        Error when is_tuple(Error) ->
            throw({invalid, {base64, Body}});
        Decoded ->
            Decoded
    end;
decode("quoted-printable", Body) ->
    decode("quoted-printable", Body, <<>>);
decode(_, Body) ->
    Body.

decode("quoted-printable", <<"=", ?CRLF, Rest/binary>>, Acc) ->
    decode("quoted-printable", Rest, Acc);
decode("quoted-printable", <<"=", H1, H2, Rest/binary>>, Acc) ->
    decode("quoted-printable", Rest, <<Acc/binary, (unhexdigit(H1) * 16 + unhexdigit(H2))>>);
decode("quoted-printable", <<C, Rest/binary>>, Acc) ->
    decode("quoted-printable", Rest, <<Acc/binary, C>>);
decode("quoted-printable", <<>>, Acc) ->
    Acc.

encode({Headers, Body}) ->
    encode(content_transfer_encoding(Headers), Body).

encode("base64", Body) ->
    wrap(base64:encode(Body), 76);
encode("quoted-printable", Body) ->
    encode("quoted-printable", Body, <<>>, 0);
encode(_, Body) ->
    Body.

encode("quoted-printable", Body, Acc, N) when N >= 75 ->
    encode("quoted-printable", Body, <<Acc/binary, $=, ?CRLF>>, 0);
encode("quoted-printable", <<?SP, C, Body/binary>>, Acc, _) when C =:= ?CR orelse C =:= ?LF ->
    encode("quoted-printable", <<C, Body/binary>>, <<Acc/binary, "=20">>, 0);
encode("quoted-printable", <<?TAB, C, Body/binary>>, Acc, _) when C =:= ?CR orelse C =:= ?LF ->
    encode("quoted-printable", <<C, Body/binary>>, <<Acc/binary, "=09">>, 0);
encode("quoted-printable", <<?CRLF, Body/binary>>, Acc, _) ->
    encode("quoted-printable", Body, <<Acc/binary, ?CRLF>>, 0);
encode("quoted-printable", <<C, Body/binary>>, Acc, _) when C =:= ?CR orelse C =:= ?LF ->
    encode("quoted-printable", Body, <<Acc/binary, ?CRLF>>, 0);
encode("quoted-printable", <<C, Body/binary>>, Acc, N) when not ?IS_PRINTABLE(C), N >= 73 ->
    encode("quoted-printable", <<C, Body/binary>>, <<Acc/binary, $=, ?CRLF>>, 0);
encode("quoted-printable", <<C, Body/binary>>, Acc, N) when not ?IS_PRINTABLE(C) ->
    H1 = hexdigit(C div 16),
    H2 = hexdigit(C rem 16),
    encode("quoted-printable", Body, <<Acc/binary, $=, H1, H2>>, N + 3);
encode("quoted-printable", <<C, Body/binary>>, Acc, N) ->
    encode("quoted-printable", Body, <<Acc/binary, C>>, N + 1);
encode("quoted-printable", <<>>, Acc, _) ->
    Acc.

decode_word(<<"=?", Rest/binary>> = Word) ->
    {_CharSet, R0} = str:snap(Rest, <<"?">>), %%
    {Encoding, R1} = str:snap(R0, <<"?">>),
    case str:snap(R1, -2) of
        {Encoded, <<"?=">>} when Encoding =:= <<"B">> ->
            decode("base64", Encoded);
        {Encoded, <<"?=">>} when Encoding =:= <<"Q">> ->
            decode("quoted-printable", Encoded); %% not quite
        _ ->
            Word
    end.

encode_word(Word) ->
    <<"=?utf-8?B?", (base64:encode(Word))/binary, "?=">>.

split_headers(Message) ->
    split_headers(Message, []).

split_headers(Message, Headers) ->
    split_headers(Message, Headers, <<>>).

split_headers(Message, Headers, Buffer) ->
    case str:snap(Message, <<?CRLF>>) of
        {<<>>, Body} when Headers =:= [] ->
            {[], Body};
        {Line, <<>>} ->
            {lists:reverse([parse_header(Buffer, Line)|Headers]), <<>>};
        {Line, <<?CRLF, Body/binary>>} ->
            {lists:reverse([parse_header(Buffer, Line)|Headers]), Body};
        {Line, <<Space, Rest/binary>>} when Space =:= 9; Space =:= 32 ->
            split_headers(Rest, Headers, <<Buffer/binary, Line/binary, ?CRLF, Space>>);
        {Line, Rest} ->
            split_headers(Rest, [parse_header(Buffer, Line)|Headers])
    end.

split_parts({Headers, Body}) ->
    case boundary(Headers) of
        error ->
            Body;
        Boundary ->
            clean_parts(binary:split(Body, <<"--", Boundary/binary>>, [global]), undefined, undefined, Boundary, [])
    end.

clean_parts([Preamble|Parts], undefined, undefined, Boundary, Acc) ->
    clean_parts(Parts, Preamble, undefined, Boundary, Acc);
clean_parts([<<?CRLF, Part/binary>>|Parts], Preamble, Epilogue, Boundary, Acc) ->
    clean_parts(Parts, Preamble, Epilogue, Boundary, [Part|Acc]);
clean_parts([<<"--", Epilogue/binary>>], Preamble, undefined, Boundary, Acc) ->
    {Preamble, lists:reverse(Acc), Epilogue, Boundary};
clean_parts([], Preamble, undefined, Boundary, Acc) ->
    {Preamble, lists:reverse(Acc), <<>>, Boundary};
clean_parts([Part|Parts], Preamble, Epilogue, Boundary, [Last|Acc]) ->
    clean_parts(Parts, Preamble, Epilogue, Boundary, [<<Last/binary, "--", Boundary/binary, Part/binary>>|Acc]).

fold(Message, Fun, Acc) ->
    {Headers, Body} = split_headers(Message),
    case split_parts({Headers, Body}) of
        {_, Parts, _, _} = Multi ->
            lists:foldl(fun (Part, A) ->
                                fold(Part, Fun, A)
                        end, Fun(content_type(Headers), {Headers, Multi}, Acc), Parts);
        <<_/binary>> ->
            Fun(content_type(Headers), {Headers, Body}, Acc)
    end.

parse(Message) ->
    {Headers, Body} = split_headers(Message),
    {Headers,
     case split_parts({Headers, Body}) of
         {Preamble, Parts, Epilogue, Boundary} ->
             {Preamble, [parse(Part) || Part <- Parts], Epilogue, Boundary};
         <<_/binary>> ->
             case content_type(Headers) of
                 {"text/" ++ _, _Params} ->
                     decode({Headers, Body});
                 {_Type, _Params} ->
                     Body
             end
     end}.

parse(Symbol, Data) ->
    try read(Symbol, Data) of
        {Value, <<>>} ->
            Value;
        _ ->
            throw({invalid, {Symbol, Data}})
    catch
        _:_ ->
            throw({invalid, {Symbol, Data}})
    end.

parse(Symbol, Data, Default) ->
    element(1, maybe(Symbol, Data, Default)).

parse_header(Buffer, Line) ->
    case binary:split(<<Buffer/binary, Line/binary>>, <<":">>) of
        [Field, Value] ->
            {Field, Value};
        [Invalid] ->
            throw({invalid, {header, Invalid}})
    end.

format(HB) ->
    <<(format(headers, HB))/binary, ?CRLF, (format(body, HB))/binary>>.

format(headers, {Headers, _Body}) ->
    << <<(bin(F))/binary, ":", (bin(V))/binary, ?CRLF>> || {F, V} <- Headers >>;

format(body, {_Headers, {Preamble, Parts, Epilogue, Boundary}}) ->
    <<Preamble/binary,
     (<< <<"--", Boundary/binary, ?CRLF, (format(P))/binary>> || P <- Parts >>)/binary,
     "--", Boundary/binary, "--",
     Epilogue/binary>>;
format(body, {Headers, {Parts, Boundary}}) ->
    format(body, {Headers, {<<>>, Parts, <<>>, Boundary}});
format(body, {Headers, Parts}) when is_list(Parts) ->
    format(body, {Headers, {<<>>, Parts, <<>>, boundary(Headers)}});
format(body, HB) ->
    iolist_to_binary(encode(HB));

format({list, Symbol}, List) ->
    bin(str:join([format(Symbol, I) || I <- List], ","));

format({tokens, Symbol}, List) ->
    bin(str:join([format(Symbol, I) || I <- List], " "));

format(mailbox, {undefined, Addr}) ->
    format(addr_spec, Addr);
format(mailbox, {Name, Addr}) ->
    <<(format(display_name, Name))/binary, " ", (format(angle_addr, Addr))/binary>>;

format(display_name, [C|_] = Name) when not is_integer(C) ->
    bin(str:join(Name, " "));
format(display_name, Name) ->
    bin(Name);

format(angle_addr, Addr) ->
    <<$<, (format(addr_spec, Addr))/binary, $>>>;

format(addr_spec, {LocalPart, Domain}) ->
    <<(format(local_part, LocalPart))/binary, $@, (format(domain, Domain))/binary>>;
format(addr_spec, Addr) ->
    bin(Addr);

format(local_part, LocalPart) ->
    case maybe(dot_atom, LocalPart) of
        {LocalPart, <<>>} ->
            LocalPart;
        _ ->
            format(quoted_string, LocalPart)
    end;

format(domain, Domain) ->
    case maybe(dot_atom, Domain) of
        {Domain, <<>>} ->
            Domain;
        _ ->
            format(domain_literal, Domain)
    end;

format(quoted_string, Data) ->
    <<$", (<< <<(case ?IS_DTEXT(C) of
                     true -> <<C>>;
                     false  -> <<$\\, C>>
                 end)/binary>> || <<C>> <= Data >>)/binary, $">>;

format(domain_literal, Data) ->
    <<$[, (<< <<(case ?IS_DTEXT(C) of
                     true -> <<C>>;
                     false  -> <<$\\, C>>
                 end)/binary>> || <<C>> <= Data >>)/binary, $]>>;

format(msg_id, MessageId) ->
    format(angle_addr, MessageId); %% close enough

format(datetime, {{D, T}, O}) ->
    <<(format(date, D))/binary, " ", (format(time, T))/binary, " ", (format(offs, O))/binary>>;
format(datetime, {D, T}) ->
    format(datetime, {{D, T}, [{0, hours}, {0, minutes}]});

format(date, {Y, M, D}) ->
    Mon = case M of
              01 -> <<"Jan">>;
              02 -> <<"Feb">>;
              03 -> <<"Mar">>;
              04 -> <<"Apr">>;
              05 -> <<"May">>;
              06 -> <<"Jun">>;
              07 -> <<"Jul">>;
              08 -> <<"Aug">>;
              09 -> <<"Sep">>;
              10 -> <<"Oct">>;
              11 -> <<"Nov">>;
              12 -> <<"Dec">>
          end,
    bin(io_lib:format("~2..0B ~s ~4..0B", [D, Mon, Y]));

format(time, {H, Mi, S}) ->
    bin(io_lib:format("~2..0B:~2..0B:~2..0B", [H, Mi, S]));

format(offs, [{H, hours}, {Mi, minutes}]) when H >= 0, Mi >= 0 ->
    bin(io_lib:format("-~2..0B~2..0B", [H, Mi]));
format(offs, [{H, hours}, {Mi, minutes}]) when H =< 0, Mi =< 0 ->
    bin(io_lib:format("+~2..0B~2..0B", [abs(H), abs(Mi)])).

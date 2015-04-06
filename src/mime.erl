-module(mime).

%% Basic MIME parsing (RFC 2822)

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
         content_type/1,
         content_transfer_encoding/1,
         decode/1,
         decode/2,
         encode/1,
         encode/2]).

-export([skip_spaces/1,
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
-define(IS_DIGIT(C), (C >= $0 andalso C =< $9)).
-define(IS_PRINTABLE(C), (C >= ?SP andalso C =< $~ andalso C =/= $=)).

normalize(Name) when is_atom(Name) ->
    normalize(atom_to_list(Name));
normalize(Name) when is_binary(Name) ->
    normalize(binary_to_list(Name));
normalize(Name) ->
    string:to_lower(Name).

read_quoted(<<?Q, Rest/binary>>) ->
    read_quoted(Rest, <<>>).

read_quoted(<<?Q, Rest/binary>>, Acc) ->
    [Acc, Rest];
read_quoted(<<$\\, C, Rest/binary>>, Acc) ->
    read_quoted(Rest, <<Acc/binary, C>>);
read_quoted(<<C, Rest/binary>>, Acc) ->
    read_quoted(Rest, <<Acc/binary, C>>).

read(datetime, Timestamp) ->
    {_DoW, R0} = read(dow, skip_spaces(Timestamp)),
    {Date, R1} = read(date, skip_spaces(R0)),
    {Time, R2} = read(time, skip_spaces(R1)),
    {Offs, R3} = read(offs, skip_spaces(R2)),
    {{{Date, Time}, Offs}, R3};

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
    {[{int(H), hours}, {int(Mi), minutes}], Rest}.

skip_spaces(<<C, Rest/binary>>) when C >= 9, C =< 13; C =:= 32 ->
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

boundary(Headers) ->
    boundary(Headers, error).

boundary(Headers, Default) ->
    case content_type(Headers) of
        {<<"multipart/",  _/binary>>, Params} ->
            proplists:get_value("boundary", Params, Default);
        {_Type, _Params} ->
            error
    end.

header(Headers, Name) ->
    header(Headers, Name, undefined).

header(Headers, Name, Default) ->
    header(Headers, normalize(Name), Default, norm).

header([{Field, Value}|Headers], Name, Default, norm) ->
    case normalize(Field) of
        Name ->
            Value;
        _ ->
            header(Headers, Name, Default, norm)
    end;
header([], _, Default, norm) ->
    Default.

content_type(Headers) ->
    content_type(header(Headers, "content-type"), undefined, []).

content_type(undefined, undefined, []) ->
    {"text/plain", []};
content_type(Raw, undefined, Params) ->
    [Type|Rest] = binary:split(skip_spaces(Raw), <<";">>),
    content_type(Rest, Type, Params);
content_type([], Type, Params) ->
    {Type, Params};
content_type([Param], Type, Params) ->
    case binary:split(skip_spaces(Param), <<"=">>) of
        [<<>>] ->
            content_type([], Type, Params);
        [Attribute, <<?Q, _/binary>> = Rest0] ->
            [Value,Rest1] = read_quoted(Rest0),
            [_Xtra|Rest2] = binary:split(Rest1, <<";">>),
            content_type(Rest2, Type, [{normalize(Attribute), Value}|Params]);
        [Attribute, Rest0] ->
            [Value|Rest1] = binary:split(Rest0, <<";">>),
            content_type(Rest1, Type, [{normalize(Attribute), Value}|Params])
    end.

content_transfer_encoding(Headers) ->
    normalize(skip_spaces(header(Headers, "content-transfer-encoding", "7bit"))).

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

split_headers(Message) ->
    split_headers(Message, []).

split_headers(Message, Headers) ->
    split_headers(Message, Headers, <<>>).

split_headers(Message, Headers, Buffer) ->
    case binary:split(Message, <<?CRLF>>) of
        [<<>>, Body] when Headers =:= [] ->
            {[], Body};
        [Line, <<>>] ->
            {lists:reverse([parse(header, Buffer, Line)|Headers]), <<>>};
        [Line, <<?CRLF, Body/binary>>] ->
            {lists:reverse([parse(header, Buffer, Line)|Headers]), Body};
        [Line, <<Space, Rest/binary>>] when Space =:= 9; Space =:= 32 ->
            split_headers(Rest, Headers, <<Buffer/binary, Line/binary, ?CRLF, Space>>);
        [Line, Rest] ->
            split_headers(Rest, [parse(header, Buffer, Line)|Headers])
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
                 {<<"text/", _/binary>>, _Params} ->
                     decode({Headers, Body});
                 {_Type, _Params} ->
                     Body
             end
     end}.

parse(datetime, Timestamp) ->
    element(1, read(datetime, bin(Timestamp))).

parse(header, Buffer, Line) ->
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

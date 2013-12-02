-module(mime).

%% Basic MIME parsing (RFC 2822)

-export([parse/1,
         parse/3,
         format/1,
         format/2]).

-export([content_type/1,
         content_transfer_encoding/1,
         decode/1,
         decode/2,
         split_headers/1,
         split_parts/1]).

-define(CRLF, "\r\n").
-define(Q, "\"").

lower(Bin) when is_binary(Bin) ->
    lower(binary_to_list(Bin));
lower(Str) when is_list(Str) ->
    string:to_lower(Str).

unhex(C) when C >= $0, C =< $9 -> C - $0;
unhex(C) when C >= $a, C =< $f -> C - $a + 10;
unhex(C) when C >= $A, C =< $F -> C - $A + 10;
unhex(C) -> throw({enohex, C}).

read_quoted(<<?Q, Rest/binary>>) ->
    read_quoted(Rest, <<>>).

read_quoted(<<?Q, Rest/binary>>, Acc) ->
    [Acc, Rest];
read_quoted(<<$\\, C, Rest/binary>>, Acc) ->
    read_quoted(Rest, <<Acc/binary, C>>);
read_quoted(<<C, Rest/binary>>, Acc) ->
    read_quoted(Rest, <<Acc/binary, C>>).

skip_spaces(<<C, Rest/binary>>) when C >= 9, C =< 13; C =:= 32 ->
    skip_spaces(Rest);
skip_spaces(Binary) ->
    Binary.

content_type(Headers) ->
    content_type(proplists:get_value("content-type", Headers), undefined, []).

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
            content_type(Rest2, Type, [{lower(Attribute), Value}|Params]);
        [Attribute, Rest0] ->
            [Value|Rest1] = binary:split(Rest0, <<";">>),
            content_type(Rest1, Type, [{lower(Attribute), Value}|Params])
    end.

content_transfer_encoding(Headers) ->
    lower(skip_spaces(proplists:get_value("content-transfer-encoding", Headers, "7bit"))).

decode({Headers, Body}) ->
    decode(content_transfer_encoding(Headers), Body).

decode("base64", Body) ->
    case catch base64:decode(skip_spaces(Body)) of
        Error when is_tuple(Error) ->
            throw({invalid_base64, Body});
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
    decode("quoted-printable", Rest, <<Acc/binary, (unhex(H1) * 16 + unhex(H2))>>);
decode("quoted-printable", <<C, Rest/binary>>, Acc) ->
    decode("quoted-printable", Rest, <<Acc/binary, C>>);
decode("quoted-printable", <<>>, Acc) ->
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
    case content_type(Headers) of
        {<<"multipart/",  _/binary>>, Params} ->
            Boundary = proplists:get_value("boundary", Params),
            clean_parts(binary:split(Body, <<"--", Boundary/binary>>, [global]), undefined, undefined, Boundary, []);
        {_Type, _Params} ->
            Body
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

parse(header, Buffer, Line) ->
    case binary:split(<<Buffer/binary, Line/binary>>, <<":">>) of
        [Field, Value] ->
            {lower(Field), Value};
        [Invalid] ->
            throw({invalid_header, Invalid})
    end.

format({Headers, Body}) ->
    <<(<< <<(format(header, H))/binary, ?CRLF>> || H <- Headers >>)/binary, ?CRLF, (format(body, Body))/binary>>.

format(header, {Field, Value}) ->
    <<(util:bin(Field))/binary, ":", (util:bin(Value))/binary>>;

format(body, {Preamble, Parts, Epilogue, Boundary}) ->
    <<Preamble/binary,
     (<< <<"--", Boundary/binary, ?CRLF, (format(P))/binary>> || P <- Parts >>)/binary,
     "--", Boundary/binary, "--",
     Epilogue/binary>>;
format(body, {Parts, Boundary}) ->
    format(body, {<<>>, Parts, <<>>, Boundary});
format(body, Body) when is_binary(Body) ->
    Body.

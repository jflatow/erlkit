-module(mime).

%% Basic MIME parsing (RFC 2822)

-export([parse/1,
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
         wrap_lines/3,
         split_headers/1,
         split_parts/1]).

-import(util, [bin/1,
               hexdigit/1,
               unhexdigit/1]).

-define(TAB, $\t).
-define(SP, $\s).
-define(CR, $\r).
-define(LF, $\n).
-define(CRLF, "\r\n").
-define(Q, $\").
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

skip_spaces(<<C, Rest/binary>>) when C >= 9, C =< 13; C =:= 32 ->
    skip_spaces(Rest);
skip_spaces(Binary) ->
    Binary.

wrap_lines(Data, Every, Break) ->
    wrap_lines(Data, Every, Break, <<>>).

wrap_lines(Data, Every, Break, Acc) when size(Data) > Every ->
    <<Buf:Every/binary, Rest/binary>> = Data,
    wrap_lines(Rest, Every, Break, <<Acc/binary, Buf/binary, Break/binary>>);
wrap_lines(Data, _, Break, Acc) ->
    <<Acc/binary, Data/binary, Break/binary>>.

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
    decode("quoted-printable", Rest, <<Acc/binary, (unhexdigit(H1) * 16 + unhexdigit(H2))>>);
decode("quoted-printable", <<C, Rest/binary>>, Acc) ->
    decode("quoted-printable", Rest, <<Acc/binary, C>>);
decode("quoted-printable", <<>>, Acc) ->
    Acc.

encode({Headers, Body}) ->
    encode(content_transfer_encoding(Headers), Body).

encode("base64", Body) ->
    wrap_lines(base64:encode(Body), 76, <<?CRLF>>);
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
            {Field, Value};
        [Invalid] ->
            throw({invalid_header, Invalid})
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
    iolist_to_binary(encode(HB)).

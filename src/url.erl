-module(url).

-export([empty/0,
         parse/1,
         parse/2,
         format/1]).

-export([encode/1,
         decode/1,
         escape/1,
         unescape/1]).

-import(util, [snap/2,
               hexdigit/1,
               unhexdigit/1]).

-define(UNRESERVED(C), ((C >= $A andalso C =< $Z) orelse
                        (C >= $a andalso C =< $z) orelse
                        (C >= $0 andalso C =< $9) orelse
                        (C =:= $- orelse C =:= $. orelse C =:= $_ orelse C =:= $~))).

empty() ->
    #{scheme => undefined,
      authority => undefined,
      path => undefined,
      query => undefined,
      fragment => undefined}.

parse(Input) ->
    parse(Input, #{}).

parse(Input, URL) when is_list(Input) ->
    parse(list_to_binary(Input), URL);
parse(Input, URL) ->
    parse(scheme, Input, maps:merge(empty(), URL), <<>>).

parse(scheme, <<C, Rest/bits>>, URL, Buf) when C =/= $:, C =/= $/, C =/= $?, C =/= $# ->
    parse(scheme, Rest, URL, <<Buf/bits, C>>);
parse(scheme, <<$:, Rest/bits>>, URL, Buf) ->
    parse(authority, Rest, URL#{scheme := Buf}, <<>>);
parse(scheme, <<>>, URL, Buf) ->
    parse(authority, Buf, URL, <<>>);

parse(authority, <<"//", Rest/bits>>, URL = #{authority := undefined}, <<>>) ->
    parse(authority, Rest, URL#{authority := true}, <<>>);
parse(authority, Rest, URL = #{authority := undefined}, <<>>) ->
    parse(path, Rest, URL, <<>>);

parse(authority, <<C, Rest/bits>>, URL, Buf) when C =/= $/, C =/= $?, C =/= $# ->
    parse(authority, Rest, URL, <<Buf/bits, C>>);
parse(authority, Rest, URL, Buf) ->
    parse(path, Rest, URL#{authority := Buf}, <<>>);

parse(path, <<C, Rest/bits>>, URL, Buf) when C =/= $?, C =/= $# ->
    parse(path, Rest, URL, <<Buf/bits, C>>);
parse(path, Rest, URL, Buf) ->
    parse(query, Rest, URL#{path := Buf}, <<>>);

parse(query, <<$?, Rest/bits>>, URL = #{query := undefined}, <<>>) ->
    parse(query, Rest, URL#{query := true}, <<>>);
parse(query, Rest, URL = #{query := undefined}, <<>>) ->
    parse(fragment, Rest, URL, <<>>);

parse(query, <<C, Rest/bits>>, URL, Buf) when C =/= $# ->
    parse(query, Rest, URL, <<Buf/bits, C>>);
parse(query, Rest, URL, Buf) ->
    parse(fragment, Rest, URL#{query := Buf}, <<>>);

parse(fragment, <<$#, Rest/bits>>, URL = #{fragment := undefined}, <<>>) ->
    parse(fragment, Rest, URL#{fragment := true}, <<>>);
parse(fragment, <<>>, URL = #{fragment := undefined}, <<>>) ->
    URL;

parse(fragment, <<C, Rest/bits>>, URL, Buf) ->
    parse(fragment, Rest, URL, <<Buf/bits, C>>);
parse(fragment, <<>>, URL, Buf) ->
    URL#{fragment := Buf}.

format(URL) ->
    format(scheme, URL, <<>>).

format(scheme, URL = #{scheme := Scheme}, Acc) when Scheme =/= undefined ->
    format(authority, URL, <<Acc/bits, Scheme/bits, $:>>);
format(scheme, URL, Acc) ->
    format(authority, URL, Acc);

format(authority, URL = #{authority := Authority}, Acc) when Authority =/= undefined ->
    format(path, URL, <<Acc/bits, "//", Authority/bits>>);
format(authority, URL, Acc) ->
    format(path, URL, Acc);

format(path, URL = #{path := Path}, Acc) when Path =/= undefined ->
    format(query, URL, <<Acc/bits, Path/bits>>);
format(path, URL, Acc) ->
    format(query, URL, Acc);

format(query, URL = #{query := Query}, Acc) when Query =/= undefined ->
    format(fragment, URL, <<Acc/bits, $?, Query/bits>>);
format(query, URL, Acc) ->
    format(fragment, URL, Acc);

format(fragment, #{fragment := Fragment}, Acc) when Fragment =/= undefined ->
    <<Acc/bits, $#, Fragment/bits>>;
format(fragment, _, Acc) ->
    Acc.

decode(<<>>) ->
    [];
decode(Data) when is_binary(Data) ->
    {KV, Rest} = snap(Data, <<$&>>),
    {K, V} = snap(KV, <<$=>>),
    [{unescape(K), unescape(V)}|decode(Rest)];
decode(Data) when is_list(Data) ->
    decode(iolist_to_binary(Data)).

encode([{K, V}|[_|_] = Rest]) ->
    [escape(K), $=, escape(V), $&|encode(Rest)];
encode([{K, V}]) ->
    [escape(K), $=, escape(V)];
encode([]) ->
    [].

escape(<<C, Rest/bits>>) when ?UNRESERVED(C) ->
    [C|escape(Rest)];
escape(<<" ", Rest/bits>>) ->
    [$+|escape(Rest)];
escape(<<Hi:4, Lo:4, Rest/bits>>) ->
    [$%, hexdigit(Hi), hexdigit(Lo)|escape(Rest)];
escape(<<>>) ->
    [];
escape(Atom) when is_atom(Atom) ->
    escape(atom_to_list(Atom));
escape(Int) when is_integer(Int) ->
    escape(integer_to_list(Int));
escape(List) when is_list(List) ->
    escape(iolist_to_binary(List)).

unescape(<<$+, Rest/bits>>) ->
    <<" ", (unescape(Rest))/bits>>;
unescape(<<$%, Hi, Lo, Rest/bits>>) ->
    <<(unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4)), (unescape(Rest))/bits>>;
unescape(<<C, Rest/bits>>) ->
    <<C, (unescape(Rest))/bits>>;
unescape(<<>>) ->
    <<>>;
unescape(String) when is_list(String) ->
    unescape(iolist_to_binary(String)).

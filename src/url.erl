-module(url).
-author("Jared Flatow").

-export_type([raw/0, url/0]).

-type raw() :: iodata().
-type url() :: map().

-export([empty/0,
         parse/1,
         parse/2,
         format/1]).

-export([rd/2]).
-export([up/2, up/3]).
-export([p/2, q/2, f/2, u/2, u/3, u/4]).
-export([pz/2, qu/2, qz/2, fu/2, fz/2]).

-export([encode/1,
         decode/1,
         escape/1,
         unescape/1,
         esc/1,
         enc/1]).

-export([browse/1,
         browse/2]).

-import(util, [hexdigit/1,
               unhexdigit/1]).

-define(EMPTY(V), (V =:= undefined orelse V =:= <<>>)).
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

format(scheme, URL = #{scheme := Scheme}, Acc) when not ?EMPTY(Scheme) ->
    format(authority, URL, <<Acc/bits, Scheme/bits, $:>>);
format(scheme, URL, Acc) ->
    format(authority, URL, Acc);

format(authority, URL = #{authority := Authority}, Acc) when not ?EMPTY(Authority) ->
    format(path, URL, <<Acc/bits, "//", Authority/bits>>);
format(authority, URL, Acc) ->
    format(path, URL, Acc);

format(path, URL = #{path := Path}, Acc) when not ?EMPTY(Path) ->
    format(query, URL, <<Acc/bits, Path/bits>>);
format(path, URL, Acc) ->
    format(query, URL, Acc);

format(query, URL = #{query := Query}, Acc) when not ?EMPTY(Query) ->
    format(fragment, URL, <<Acc/bits, $?, Query/bits>>);
format(query, URL, Acc) ->
    format(fragment, URL, Acc);

format(fragment, #{fragment := Fragment}, Acc) when not ?EMPTY(Fragment) ->
    <<Acc/bits, $#, Fragment/bits>>;
format(fragment, _, Acc) ->
    Acc.

rd(Field, URL) when not is_map(URL) ->
    rd(Field, parse(URL));
rd(p, URL) ->
    filename:split(util:defget(URL, path, <<>>));
rd(q, URL) ->
    decode(util:defget(URL, query, <<>>));
rd(f, URL) ->
    decode(util:defget(URL, fragment, <<>>));
rd(Field, URL) ->
    util:get(URL, Field).

up(URL, Opts) when not is_map(URL) ->
    up(parse(URL), Opts);
up(URL, Opts) ->
    util:fold(fun ({F, V}, U) -> up(F, U, V) end, URL, Opts).

up(Field, URL, Value) when not is_map(URL) ->
    up(Field, parse(URL), Value);
up(_, URL, undefined) ->
    URL;
up(p, URL, Parts) ->
    URL#{path => filename:join([<<$/>>|Parts])};
up(q, URL, Params) ->
    URL#{query => enc(Params)};
up(f, URL, Params) ->
    URL#{fragment => enc(Params)};
up(Field, URL, Value) ->
    URL#{Field => Value}.

p(URL, Parts) -> format(up(p, URL, Parts)).
q(URL, Params) -> format(up(q, URL, Params)).
f(URL, Params) -> format(up(f, URL, Params)).
u(URL, Opts) -> format(up(URL, Opts)).
u(URL, P, Q) -> u(URL, [{p, P}, {q, Q}]).
u(URL, P, Q, F) -> u(URL, [{p, P}, {q, Q}, {f, F}]).

pz(URL, Parts) ->
    p(URL, rd(p, URL) ++ Parts).

qu(URL, Params) ->
    q(URL, util:update(rd(q, URL), Params)).

qz(URL, Params) ->
    q(URL, rd(q, URL) ++ util:iter(Params)).

fu(URL, Params) ->
    f(URL, util:update(rd(f, URL), Params)).

fz(URL, Params) ->
    f(URL, rd(f, URL) ++ util:iter(Params)).

decode(<<>>) ->
    [];
decode(Data) when is_binary(Data) ->
    {KV, Rest} = str:snap(Data, <<$&>>),
    {K, V} = str:snap(KV, <<$=>>),
    [{unescape(K), unescape(V)}|decode(Rest)];
decode(Data) when is_list(Data) ->
    decode(list_to_binary(Data)).

encode(Params) when is_map(Params) ->
    encode(maps:to_list(Params));

encode([{K, V}|[_|_] = Rest]) ->
    [escape(K), $=, escape(V), $&|encode(Rest)];
encode([{K, V}]) ->
    [escape(K), $=, escape(V)];
encode([]) ->
    [].

escape(<<C, Rest/bits>>) when ?UNRESERVED(C) ->
    [C|escape(Rest)];
escape(<<Hi:4, Lo:4, Rest/bits>>) ->
    [$%, hexdigit(Hi), hexdigit(Lo)|escape(Rest)];
escape(<<>>) ->
    [];
escape(Atom) when is_atom(Atom) ->
    escape(atom_to_list(Atom));
escape(Int) when is_integer(Int) ->
    escape(integer_to_list(Int));
escape(List) when is_list(List) ->
    escape(list_to_binary(List)).

unescape(<<$+, Rest/bits>>) ->
    <<" ", (unescape(Rest))/bits>>;
unescape(<<$%, Hi, Lo, Rest/bits>>) ->
    <<(unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4)), (unescape(Rest))/bits>>;
unescape(<<C, Rest/bits>>) ->
    <<C, (unescape(Rest))/bits>>;
unescape(<<>>) ->
    <<>>;
unescape(String) when is_list(String) ->
    unescape(list_to_binary(String)).

esc(Data) -> iolist_to_binary(escape(Data)).
enc(List) -> iolist_to_binary(encode(List)).

browse(URL) ->
    browse(URL, os:type()).

browse(URL, OS) when is_map(URL) ->
    browse(format(URL), OS);
browse(URL, {unix, darwin}) ->
    os:cmd(str:format("open '~s'", [URL]));
browse(URL, {unix, _}) ->
    os:cmd(str:format("xdg-open '~s'", [URL]));
browse(URL, {win32, _}) ->
    os:cmd(str:format("start '~s'", [URL])).

-module(util).

-export([atom/1,
         atom/2,
         list/1,
         str/1,
         bin/1,
         flt/1,
         int/1,
         num/1,
         mod/2,
         hex/1,
         unhex/1,
         hexdigit/1,
         unhexdigit/1,
         ok/1,
         ok/2,
         def/2,
         delete/2,
         get/2,
         get/3,
         set/3,
         has/2,
         map/2,
         mutate/3,
         mutate/4,
         increment/2,
         increment/3,
         defmin/2,
         defmax/2,
         defget/2,
         defget/3,
         getdef/2,
         getdef/3,
         setdef/3,
         setdef/4,
         hasany/2,
         hasall/2,
         exists/2,
         ifndef/3,
         lookup/2,
         lookup/3,
         modify/3,
         modify/4,
         remove/2,
         replace/3,
         update/2,
         count/3,
         head/1,
         last/1,
         each/2,
         enum/1,
         enum/2,
         enum/3,
         fold/3,
         iter/1,
         join/2,
         join/3,
         roll/3,
         skip/2]).

atom(Any) ->
    atom(Any, false).

atom(Atom, _) when is_atom(Atom) ->
    Atom;
atom(Else, true) ->
    list_to_existing_atom(list(Else));
atom(Else, false) ->
    list_to_atom(list(Else)).

list(List) when is_list(List) ->
    List;
list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
list(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
list(Flt) when is_float(Flt) ->
    float_to_list(Flt);
list(Int) when is_integer(Int) ->
    integer_to_list(Int).

str(Str) when is_list(Str); is_binary(Str) ->
    str:format("~s", [Str]);
str(Any) ->
    list(Any).

bin(Bin) when is_binary(Bin) ->
    Bin;
bin(List) when is_list(List) ->
    iolist_to_binary(List);
bin(Else) ->
    bin(list(Else)).

flt(Flt) when is_float(Flt) ->
    Flt;
flt(List) when is_list(List) ->
    list_to_float(List);
flt(Else) ->
    flt(list(Else)).

int(Int) when is_integer(Int) ->
    Int;
int(List) when is_list(List) ->
    list_to_integer(List);
int(Else) ->
    int(list(Else)).

num(Num) when is_number(Num) ->
    Num;
num(Any) ->
    try flt(Any) catch error:badarg -> int(Any) end.

mod(X, Y) ->
    case X rem Y of
        R when R < 0 ->
            R + Y;
        R ->
            R
    end.

hex(List) when is_list(List) ->
    hex(list_to_binary(List));
hex(<<Q:4, Rest/bits>>) ->
    <<(hexdigit(Q)), (hex(Rest))/bits>>;
hex(<<>>) ->
    <<>>.

unhex(List) when is_list(List) ->
    unhex(list_to_binary(List));
unhex(<<H, L, Rest/bits>>) ->
    <<(unhexdigit(H) * 16 + unhexdigit(L)), (unhex(Rest))/bits>>;
unhex(<<>>) ->
    <<>>.

hexdigit(00) -> $0;
hexdigit(01) -> $1;
hexdigit(02) -> $2;
hexdigit(03) -> $3;
hexdigit(04) -> $4;
hexdigit(05) -> $5;
hexdigit(06) -> $6;
hexdigit(07) -> $7;
hexdigit(08) -> $8;
hexdigit(09) -> $9;
hexdigit(10) -> $A;
hexdigit(11) -> $B;
hexdigit(12) -> $C;
hexdigit(13) -> $D;
hexdigit(14) -> $E;
hexdigit(15) -> $F.

unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.

ok(Term) ->
    ok(Term, undefined).

ok({ok, Value}, _Default) ->
    Value;
ok(_, Default) ->
    Default.

def(undefined, Default) ->
    Default;
def(null, Default) ->
    Default;
def(nil, Default) ->
    Default;
def(<<>>, Default) ->
    Default;
def([], Default) ->
    Default;
def(Value, _) ->
    Value.

delete(Map, Key) when is_map(Map) ->
    maps:remove(Key, Map);
delete(List, Key) when is_list(List) ->
    proplists:delete(Key, List);
delete({Mod, Obj}, Key) ->
    Mod:delete(Obj, Key).

get(Obj, Key) ->
    get(Obj, Key, undefined).

get(Map, Key, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
get(List, Key, Default) when is_list(List) ->
    proplists:get_value(Key, List, Default);
get({Mod, Obj}, Key, Default) ->
    Mod:get(Obj, Key, Default).

set(Map, Key, Val) when is_map(Map) ->
    maps:put(Key, Val, Map);
set(List, Key, Val) when is_list(List) ->
    lists:keystore(Key, 1, List, {Key, Val});
set({Mod, Obj}, Key, Val) ->
    Mod:set(Obj, Key, Val).

has(Map, Key) when is_map(Map) ->
    maps:is_key(Key, Map);
has(List, Key) when is_list(List) ->
    proplists:is_defined(Key, List);
has({Mod, Obj}, Key) ->
    Mod:has(Obj, Key).

map(Map, Fun) when is_map(Map) ->
    maps:fold(fun (Key, Val, Acc) ->
                      Acc#{Key => Fun(Val)}
              end, #{}, Map);
map(List, Fun) ->
    lists:map(Fun, List).

mutate(Obj, Key, Fun) ->
    mutate(Obj, Key, Fun, undefined).

mutate(Obj, Key, Fun, Initial) ->
    set(Obj, Key, Fun(get(Obj, Key, Initial))).

increment(Obj, Key) ->
    increment(Obj, Key, 1).

increment(Obj, Key, Num) ->
    mutate(Obj, Key, fun (V) -> V + Num end, 0).

defmin(A, B) ->
    min(def(A, B), def(B, A)).

defmax(A, B) ->
    max(def(A, B), def(B, A)).

defget(Obj, Key) ->
    defget(Obj, Key, undefined).

defget(Obj, Key, Default) ->
    def(get(Obj, Key, Default), Default).

getdef(Maybe, Key) ->
    getdef(Maybe, Key, undefined).

getdef(Maybe, Key, Default) ->
    get(def(Maybe, []), Key, Default).

setdef(Maybe, Key, Val) ->
    setdef(Maybe, Key, Val, []).

setdef(Maybe, Key, Val, Empty) ->
    set(def(Maybe, Empty), Key, Val).

hasall(Obj, [Key|Keys]) ->
    case has(Obj, Key) of
        true ->
            hasall(Obj, Keys);
        false ->
            false
    end;
hasall(_, []) ->
    true.

hasany(Obj, [Key|Keys]) ->
    case has(Obj, Key) of
        true ->
            true;
        false ->
            hasany(Obj, Keys)
    end;
hasany(_, []) ->
    false.

exists(_, []) ->
    true;
exists(Obj, [Key|Path]) ->
    case has(Obj, Key) of
        true ->
            exists(get(Obj, Key), Path);
        false ->
            false
    end;
exists(Obj, Key) ->
    exists(Obj, [Key]).

ifndef(Obj, Path, Default) ->
    modify(Obj, Path,
           fun (undefined) ->
                   Default;
               (Defined) ->
                   Defined
           end).

lookup(Obj, Path) when is_list(Path) ->
    lists:foldl(fun (Key, Acc) ->
                        getdef(Acc, Key)
                end, Obj, Path);
lookup(Obj, Key) ->
    lookup(Obj, [Key]).

lookup(Obj, Path, Default) ->
    def(lookup(Obj, Path), Default).

modify(Obj, Path, Fun) ->
    modify(Obj, Path, Fun, #{}).

modify(Obj, [], _, _) ->
    Obj;
modify(Obj, [Key], Fun, Empty) when is_function(Fun) ->
    setdef(Obj, Key, Fun(getdef(Obj, Key)), Empty);
modify(Obj, [Key], Val, Empty) ->
    setdef(Obj, Key, Val, Empty);
modify(Obj, [Key|Path], Fun, Empty) ->
    setdef(Obj, Key, modify(getdef(Obj, Key), Path, Fun, Empty), Empty);
modify(Obj, Key, Fun, Empty) ->
    modify(Obj, [Key], Fun, Empty).

remove(Obj, []) ->
    Obj;
remove(Obj, [Key]) ->
    delete(Obj, Key);
remove(Obj, [Key|Path]) ->
    case get(Obj, Key) of
        undefined ->
            Obj;
        Val ->
            set(Obj, Key, remove(Val, Path))
    end;
remove(Obj, Key) ->
    remove(Obj, [Key]).

replace(Obj, Path, Fun) ->
    case exists(Obj, Path) of
        true ->
            modify(Obj, Path, Fun);
        false ->
            Obj
    end.

update(Old, New) when is_map(Old), is_map(New) ->
    maps:merge(Old, New);
update(Old, New) ->
    fold(fun ({K, V}, A) -> set(A, K, V) end, Old, New).

count(Fun, Acc, N) when is_number(N) ->
    count(Fun, Acc, {0, N});
count(Fun, Acc, {I, N}) when I < N ->
    count(Fun, Fun(I, Acc), {I + 1, N});
count(_, Acc, _) ->
    Acc.

head([H|_]) ->
    H;
head([]) ->
    undefined.

last(List) ->
    head(lists:reverse(List)).

each(Obj, Fun) ->
    fold(fun (I, _) -> Fun(I) end, nil, Obj).

enum(List) ->
    enum(List, 0).

enum([H|T], N) ->
    [{N, H}|enum(T, N + 1)];
enum([], _) ->
    [].

enum(Fun, Acc, List) ->
    element(2, lists:foldl(fun (I, {N, A}) ->
                                   {N + 1, Fun(N, I, A)}
                           end, {0, Acc}, List)).

fold(Fun, Acc, Map) when is_map(Map) ->
    maps:fold(fun (K, V, A) -> Fun({K, V}, A) end, Acc, Map);
fold(Fun, Acc, List) when is_list(List) ->
    lists:foldl(Fun, Acc, List).

iter(Map) when is_map(Map) ->
    maps:to_list(Map);
iter(List) when is_list(List) ->
    List.

join([A, B|Rest], Sep) ->
    [A, Sep|join([B|Rest], Sep)];
join(List, _Sep) ->
    List.

join([A|Rest], Sep, A) ->
    join(Rest, Sep, A);
join([A, B|Rest], Sep, B) ->
    join([A|Rest], Sep, B);
join([A, B|Rest], Sep, Skip) ->
    [A, Sep|join([B|Rest], Sep, Skip)];
join(List, _Sep, _Skip) ->
    List.

roll(Fun, Acc, [I|Rest]) ->
    case Fun(I, Acc) of
        {done, A} ->
            A;
        {continue, A} ->
            roll(Fun, A, Rest)
    end;
roll(_Fun, Acc, []) ->
    Acc;
roll(Fun, Acc, Iterable) ->
    roll(Fun, Acc, iter(Iterable)).

skip(N, [_|Tail]) when N > 0 ->
    skip(N - 1, Tail);
skip(_, Rest) ->
    Rest.

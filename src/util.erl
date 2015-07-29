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
         defmin/2,
         defmax/2,
         defget/2,
         defget/3,
         getdef/2,
         getdef/3,
         setdef/3,
         setdef/4,
         lookup/2,
         modify/3,
         modify/4,
         mutate/3,
         mutate/4,
         increment/2,
         increment/3,
         update/2,
         count/3,
         each/2,
         enum/1,
         enum/2,
         enum/3,
         fold/3,
         iter/1,
         join/2,
         join/3,
         roll/3,
         skip/2,
         snap/2,
         split/2,
         substr/2,
         substr/3,
         disfix/2,
         format/2,
         lstrip/2,
         rstrip/2,
         strip/2,
         lower/1,
         upper/1,
         replace/3,
         contains/2,
         startswith/2,
         endswith/2]).

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
    format("~s", [Str]);
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
    proplists:delete(Key, List).

get(Obj, Key) ->
    get(Obj, Key, undefined).

get(Map, Key, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
get(List, Key, Default) when is_list(List) ->
    proplists:get_value(Key, List, Default).

set(Map, Key, Val) when is_map(Map) ->
    maps:put(Key, Val, Map);
set(List, Key, Val) when is_list(List) ->
    lists:keystore(Key, 1, List, {Key, Val}).

has(Obj, [Key|Keys]) ->
    case get(Obj, Key) of
        undefined ->
            false;
        _ ->
            has(Obj, Keys)
    end;
has(_, []) ->
    true.

map(Map, Fun) when is_map(Map) ->
    maps:fold(fun (Key, Val, Acc) ->
                      Acc#{Key => Fun(Val)}
              end, #{}, Map);
map(List, Fun) ->
    lists:map(Fun, List).

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

lookup(Obj, Path) when is_list(Path) ->
    lists:foldl(fun (Key, Acc) ->
                        getdef(Acc, Key)
                end, Obj, Path);
lookup(Obj, Key) ->
    lookup(Obj, [Key]).

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

mutate(Obj, Key, Fun) ->
    mutate(Obj, Key, Fun, undefined).

mutate(Obj, Key, Fun, Initial) ->
    set(Obj, Key, Fun(get(Obj, Key, Initial))).

increment(Obj, Key) ->
    increment(Obj, Key, 1).

increment(Obj, Key, Num) ->
    mutate(Obj, Key, fun (V) -> V + Num end, 0).

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

snap(Data, Pos) when is_integer(Pos), Pos < 0 ->
    {substr(Data, 0, size(Data) + Pos), substr(Data, Pos)};
snap(Data, Pos) when is_integer(Pos) ->
    {substr(Data, 0, Pos), substr(Data, Pos)};
snap(Data, Sep) ->
    case binary:split(Data, Sep) of
        [A, B] ->
            {A, B};
        [A] ->
            {A, <<>>};
        [] ->
            {<<>>, <<>>}
    end.

split(<<>>, _) ->
    [];
split(Data, Sep) when is_binary(Data), is_integer(Sep) ->
    binary:split(Data, <<Sep>>, [global]);
split(Data, Seps) when is_binary(Data), is_list(Seps) ->
    binary:split(Data, [case Sep of
                            S when is_binary(S) ->
                                S;
                            S when is_integer(S) ->
                                <<S>>;
                            S ->
                                bin(S)
                        end || Sep <- Seps], [global]);
split(Data, Sep) when is_binary(Data) ->
    binary:split(Data, Sep, [global]);
split(Data, Sep) ->
    split(bin(Data), Sep).

substr(Data, N) ->
    substr(Data, N, size(Data)).

substr(Data, N, Len) when N < 0 ->
    substr(Data, max(0, size(Data) + N), Len);
substr(Data, N, Len) ->
    M = min(N, size(Data)),
    binary:part(Data, M, min(max(-M, Len), size(Data) - M)).

disfix(Prefix, Str) when is_list(Str) ->
    list(disfix(Prefix, bin(Str)));
disfix(Prefix, Bin) when is_binary(Bin), is_list(Prefix) ->
    disfix(bin(Prefix), Bin);
disfix(Prefix, Bin) when is_binary(Bin) ->
    Size = size(Prefix),
    case Bin of
        <<Prefix:Size/binary, Rest/binary>> ->
            Rest;
        _ ->
            Bin
    end.

format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

lstrip([C|Rest], C) ->
    lstrip(Rest, C);
lstrip(<<C, Rest/bits>>, C) ->
    lstrip(Rest, C);
lstrip(Seq, _) ->
    Seq.

rstrip([C|Rest], C) ->
    case rstrip(Rest, C) of
        [] ->
            [];
        Tail  ->
            [C|Tail]
    end;
rstrip([O|Rest], C) ->
    [O|rstrip(Rest, C)];
rstrip(<<_, _/bits>> = Bin, C) when is_binary(Bin) ->
    case binary:last(Bin) of
        C ->
            rstrip(binary:part(Bin, 0, size(Bin) - 1), C);
        _ ->
            Bin
    end;
rstrip(Seq, _) ->
    Seq.

strip(Seq, C) ->
    rstrip(lstrip(Seq, C), C).

lower(Bin) when is_binary(Bin) ->
    unicode:characters_to_binary(lower(unicode:characters_to_list(Bin)));
lower(Str) ->
    string:to_lower(Str).

upper(Bin) when is_binary(Bin) ->
    unicode:characters_to_binary(upper(unicode:characters_to_list(Bin)));
upper(Str) ->
    string:to_upper(Str).

replace(Str, Pat, Sub) when is_list(Pat); is_binary(Pat) ->
    re:replace(Str, Pat, Sub, [global]);
replace(Str, Pat, Sub) when is_list(Str) ->
    [case C of Pat -> Sub; _ -> C end || C <- Str];
replace(Bin, Pat, Sub) when is_binary(Bin) ->
    << <<(case C of Pat -> Sub; _ -> C end)>> || <<C>> <= Bin >>.

contains(Str, Pat) when is_list(Pat); is_binary(Pat) ->
    re:run(Str, Pat) =/= nomatch;
contains([Pat|_], Pat) ->
    true;
contains([_|Rest], Pat) ->
    contains(Rest, Pat);
contains(<<Pat, _/binary>>, Pat) ->
    true;
contains(<<_, Rest/binary>>, Pat) ->
    contains(Rest, Pat);
contains(_, _) ->
    false.

startswith(<<Prefix, _/binary>>, Prefix) when is_integer(Prefix) ->
    true;
startswith(Bin, Prefix) when is_binary(Bin) ->
    P = bin(Prefix),
    N = size(P),
    case Bin of
        <<P:N/binary, _/binary>> ->
            true;
        _ ->
            false
    end;
startswith([Prefix|_], Prefix) ->
    true;
startswith(List, Prefix) when is_list(List) ->
    lists:prefix(list(Prefix), List);
startswith(_, _) ->
    false.

endswith(Bin, Suffix) when is_binary(Bin), is_integer(Suffix) ->
    binary:last(Bin) =:= Suffix orelse endswith(Bin, bin(Suffix));
endswith(Bin, Suffix) when is_binary(Bin) ->
    S = bin(Suffix),
    N = size(Bin) - size(S),
    case Bin of
        <<_:N/binary, S/binary>> ->
            true;
        _ ->
            false
    end;
endswith(List, Suffix) when is_list(List) ->
    case lists:last(List) of
        Suffix ->
            true;
        _ ->
            lists:suffix(list(Suffix), List)
    end;
endswith(_, _) ->
    false.

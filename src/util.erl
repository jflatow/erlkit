-module(util).

-export([atom/1,
         atom/2,
         list/1,
         bin/1,
         flt/1,
         int/1,
         num/1,
         mod/2,
         hex/1,
         hexdigit/1,
         unhexdigit/1,
         get/2,
         get/3,
         set/3,
         modify/3,
         modify/4,
         increment/2,
         increment/3,
         update/2,
         count/3,
         enum/1,
         enum/2,
         enum/3,
         fold/3,
         iter/1,
         join/2,
         join/3,
         snap/2,
         disfix/2,
         lstrip/2,
         rstrip/2,
         strip/2,
         lower/1,
         upper/1]).

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

get(Obj, Key) ->
    get(Obj, Key, undefined).

get(Map, Key, Default) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Val} ->
            Val;
        error ->
            Default
    end;
get(List, Key, Default) when is_list(List) ->
    proplists:get_value(Key, List, Default);
get(Dict, Key, Default) when element(1, Dict) =:= dict -> %% NB: technically opaque
    case dict:find(Key, Dict) of
        {ok, Val} ->
            Val;
        error ->
            Default
    end.

set(Map, Key, Val) when is_map(Map) ->
    maps:put(Key, Val, Map);
set(List, Key, Val) when is_list(List) ->
    lists:keystore(Key, 1, List, {Key, Val});
set(Dict, Key, Val) when element(1, Dict) =:= dict -> %% NB: technically opaque
    dict:store(Key, Val, Dict).

modify(Obj, Key, Fun) ->
    modify(Obj, Key, Fun, undefined).

modify(Obj, Key, Fun, Initial) ->
    set(Obj, Key, Fun(get(Obj, Key, Initial))).

increment(Obj, Key) ->
    increment(Obj, Key, 1).

increment(Obj, Key, Num) ->
    modify(Obj, Key, fun (V) -> V + Num end, 0).

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
    lists:foldl(Fun, Acc, List);
fold(Fun, Acc, Dict) when element(1, Dict) =:= dict -> %% NB: technically opaque
    dict:fold(Fun, Acc, Dict).

iter(Map) when is_map(Map) ->
    maps:to_list(Map);
iter(List) when is_list(List) ->
    List;
iter(Dict) when element(1, Dict) =:= dict -> %% NB: technically opaque
    dict:to_list(Dict).

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

snap(Data, Sep) ->
    case binary:split(Data, Sep) of
        [A, B] ->
            {A, B};
        [A] ->
            {A, <<>>};
        [] ->
            {<<>>, <<>>}
    end.

disfix(Prefix, Str) when is_list(Str), is_binary(Prefix) ->
    disfix(binary_to_list(Prefix), Str);
disfix(Prefix, Str) when is_list(Str) ->
    Str -- Prefix;
disfix(Prefix, Bin) when is_binary(Bin), is_list(Prefix) ->
    disfix(list_to_binary(Prefix), Bin);
disfix(Prefix, Bin) when is_binary(Bin) ->
    Size = size(Prefix),
    case Bin of
        <<Prefix:Size/bits, Rest/bits>> ->
            Rest;
        _ ->
            Bin
    end.

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

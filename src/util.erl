-module(util).
-author("Jared Flatow").

-export([atom/1,
         atom/2,
         list/1,
         str/1,
         bin/1,
         flt/1,
         int/1,
         key/1,
         val/1,
         num/1,
         mod/2,
         hex/1,
         unhex/1,
         hexdigit/1,
         unhexdigit/1,
         match/2,
         ok/1,
         ok/2,
         op/2,
         bump/2,
         cons/2,
         item/2,
         nil/1,
         def/2,
         delete/2,
         get/2,
         get/3,
         set/2,
         set/3,
         pop/2,
         pop/3,
         has/2,
         map/2,
         mutate/3,
         mutate/4,
         decrement/2,
         decrement/3,
         increment/2,
         increment/3,
         deflike/2,
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
         lookup/2,
         lookup/3,
         modify/3,
         modify/4,
         accrue/3,
         accrue/4,
         accrue/5,
         create/3,
         create/4,
         remove/2,
         remove/3,
         swap/3,
         swap/4,
         either/1,
         either/2,
         except/2,
         filter/2,
         select/2,
         insert/2,
         update/2,
         suppress/2,
         all/2,
         any/2,
         chunk/2,
         chunk/4,
         count/3,
         first/1,
         first/2,
         first/3,
         range/2,
         range/3,
         head/1,
         last/1,
         tail/1,
         init/2,
         drop/2,
         push/2,
         default/3,
         delta/2,
         diff/2,
         edit/2,
         draw/1,
         draw/2,
         each/2,
         enum/1,
         enum/2,
         enum/3,
         fold/3,
         iter/1,
         join/2,
         join/3,
         pair/1,
         pair/2,
         pair/3,
         keys/1,
         keys/2,
         vals/1,
         vals/2,
         index/1,
         index/2,
         invert/1,
         invert/2,
         mapped/1,
         mapped/2,
         random/1,
         reduce/3,
         roll/3,
         skip/2,
         wrap/3]).

atom(Atom) when is_atom(Atom) ->
    Atom;
atom(Any) ->
    list_to_atom(list(Any)).

atom(Atom, _) when is_atom(Atom) ->
    Atom;
atom(Else, Default) ->
    try
        list_to_existing_atom(list(Else))
    catch
        error:badarg ->
            Default
    end.

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

key(Tuple) when is_tuple(Tuple) ->
    element(1, Tuple);
key(Any) ->
    Any.

val({_, Val}) ->
    Val;
val(Any) ->
    Any.

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

match(Value, Fun) when is_function(Fun) ->
    {Fun(Value), Value};
match(Value, Val) ->
    {Value =:= Val, Value}.

ok(Term) ->
    ok(Term, undefined).

ok({ok, Value}, _Default) ->
    Value;
ok(_, Default) ->
    Default.

op(A, {'+', X}) when is_number(X) ->
    def(A, 0) + X;
op(A, {'-', X}) when is_number(X) ->
    def(A, 0) - X;
op(A, {'*', X}) when is_number(X) ->
    def(A, 0) * X;
op(A, {'/', X}) when is_number(X) ->
    def(A, 0) / X;
op(A, {'+', X}) ->
    update(deflike(A, X), X);
op(A, {'-', X}) ->
    except(deflike(A, X), X);
op(_, {'=', X}) ->
    X;
op(A, {bump, Op}) ->
    bump(def(A, {0, undefined}), Op);
op(A, {item, N}) ->
    item(A, N);
op(A, {def, X}) ->
    def(A, X);
op(A, {addnew, X}) ->
    update(X, deflike(A, X));
op(A, {update, X}) ->
    update(deflike(A, X), X);
op(A, {except, X}) ->
    except(deflike(A, X), X);
op(A, {select, X}) ->
    select(deflike(A, X), X);
op(A, {append, X}) ->
    def(A, []) ++ X;
op(A, {prepend, X}) ->
    X ++ def(A, []);
op(A, {cons, H}) ->
    cons(H, def(A, []));
op(A, {init, H}) ->
    init(A, H);
op(A, {drop, H}) ->
    drop(def(A, []), H);
op(A, {push, H}) ->
    push(def(A, []), H);
op(A, {edit, X = {Add, _}}) ->
    edit(deflike(A, Add), X);
op(A, {Fun, X}) when is_function(Fun) ->
    Fun(A, X);
op(A, {M, F, X}) ->
    M:F(A, X).

bump({N, V}, Op) ->
    {N + 1, op(V, Op)}.

cons(H, T) ->
    [H|T].

item(undefined, _) ->
    undefined;
item(Tuple, N) when is_tuple(Tuple) ->
    element(N, Tuple);
item([I|_], 1) ->
    I;
item([_|Items], N) ->
    item(Items, N - 1);
item([], _) ->
    undefined.

nil(Map) when is_map(Map) ->
    #{};
nil(List) when is_list(List) ->
    [];
nil(Num) when is_number(Num) ->
    0;
nil(Bin) when is_binary(Bin) ->
    <<>>.

def(undefined, Default) ->
    Default;
def(null, Default) ->
    Default;
def(nil, Default) ->
    Default;
def(Value, _) ->
    Value.

delete(Map, Key) when is_map(Map) ->
    maps:remove(Key, Map);
delete(List, Key) when is_list(List) ->
    lists:filter(fun (I) -> key(I) =/= Key end, List).

get(Obj, Key) ->
    get(Obj, Key, undefined).

get(Map, Key, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
get(List, Key, Default) when is_list(List) ->
    first(List, fun (I) -> key(I) =:= Key end, fun val/1, Default).

set(Obj, Item) ->
    set(Obj, key(Item), val(Item)).

set(Map, Key, Val) when is_map(Map) ->
    maps:put(Key, Val, Map);
set([H|T], Key, Val) ->
    case key(H) of
        Key ->
            [{Key, Val}|T];
        _ ->
            [H|set(T, Key, Val)]
    end;
set([], Key, Val) ->
    [{Key, Val}].

pop(Obj, Key) ->
    pop(Obj, Key, undefined).

pop(Obj, Key, Default) ->
    {get(Obj, Key, Default), delete(Obj, Key)}.

has(Map, Key) when is_map(Map) ->
    maps:is_key(Key, Map);
has(List, Key) when is_list(List) ->
    any(List, fun (I) -> key(I) =:= Key end).

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

decrement(Obj, Path) ->
    decrement(Obj, Path, 1).

decrement(Obj, Path, Num) ->
    increment(Obj, Path, -Num).

increment(Obj, Path) ->
    increment(Obj, Path, 1).

increment(Obj, Path, Num) ->
    modify(Obj, Path, fun (V) -> def(V, 0) + Num end).

deflike(A, B) ->
    def(A, nil(B)).

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

exists(Obj = {Mod, _}, Path) ->
    Mod:exists(Obj, Path);
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

lookup(Obj, Path) ->
    lookup(Obj, Path, undefined).

lookup(Obj = {Mod, _}, Path, Default) when is_atom(Mod) ->
    Mod:lookup(Obj, Path, Default);
lookup(_, [], Default) ->
    Default;
lookup(Obj, [Key], Default) ->
    getdef(Obj, Key, Default);
lookup(Obj, [Key|Path], Default) ->
    case getdef(Obj, Key) of
        undefined ->
            Default;
        Val ->
            lookup(Val, Path, Default)
    end.

modify(Obj, Path, Fun) ->
    modify(Obj, Path, Fun, #{}).

modify(Obj = {Mod, _}, Path, Fun, _) when is_atom(Mod) ->
    Mod:modify(Obj, Path, Fun);
modify(Obj, [], Fun, _) when is_function (Fun) ->
    Fun(Obj);
modify(_, [], Val, _) ->
    Val;
modify(Obj, [Key], Fun, Empty) when is_function(Fun) ->
    setdef(Obj, Key, Fun(getdef(Obj, Key)), Empty);
modify(Obj, [Key], Val, Empty) ->
    setdef(Obj, Key, Val, Empty);
modify(Obj, [Key|Path], Fun, Empty) ->
    setdef(Obj, Key, modify(getdef(Obj, Key), Path, Fun, Empty), Empty).

accrue(Obj, Path, Change) ->
    accrue(Obj, Path, Change, fun op/2).

accrue(Obj, Path, Change, Op) ->
    accrue(Obj, Path, Change, Op, #{}).

accrue(Obj, Path, Changes, Op, Empty) when is_list(Changes) ->
    modify(Obj, Path, fun (Prior) -> reduce(Op, Prior, Changes) end, Empty);
accrue(Obj, Path, Change, Op, Empty) ->
    modify(Obj, Path, fun (Prior) -> Op(Prior, Change) end, Empty).

create(Obj, Path, Initial) ->
    create(Obj, Path, Initial, []).

create(Obj, Path, Initial, Opts) ->
    swap(Obj, Path, Initial, set(Opts, match, get(Opts, match, fun (V) -> V =:= undefined end))).

remove(Obj = {Mod, _}, Path) ->
    Mod:remove(Obj, Path);
remove(Obj, []) ->
    nil(Obj);
remove(Obj, [Key]) ->
    delete(Obj, Key);
remove(Obj, [Key|Path]) ->
    case get(Obj, Key) of
        undefined ->
            Obj;
        Val ->
            set(Obj, Key, remove(Val, Path))
    end.

remove(Obj, Path, Opts) ->
    case match(lookup(Obj, Path), get(Opts, match, fun (_) -> true end)) of
        {true, _} = Tag ->
            wrap(Tag, remove(Obj, Path), get(Opts, wrapped));
        {false, _} = Tag ->
            wrap(Tag, Obj, get(Opts, wrapped))
    end.

swap(Obj, Path, Swap) ->
    swap(Obj, Path, Swap, []).

swap(Obj, Path, Swap, Opts) ->
    case match(lookup(Obj, Path), get(Opts, match, fun (V) -> V =/= undefined end)) of
        {true, _} = Tag ->
            wrap(Tag, modify(Obj, Path, Swap), get(Opts, wrapped));
        {false, _} = Tag ->
            wrap(Tag, Obj, get(Opts, wrapped))
    end.

either(List) ->
    either(List, undefined).

either([{Obj, Path}|Rest], Default) ->
    case lookup(Obj, Path) of
        undefined ->
            either(Rest, Default);
        Val ->
            Val
    end;
either([], Default) ->
    Default.

except(Map, Exclude) when is_map(Map) ->
    maps:without(keys(Exclude), Map);
except(Obj, Exclude) ->
    reduce(fun delete/2, Obj, keys(Exclude)).

filter(Map, Filter) when is_map(Map) ->
    maps:filter(fun (K, V) -> Filter({K, V}) end, Map);
filter(List, Filter) when is_list(List) ->
    lists:filter(Filter, List).

select(Map, Include) when is_map(Map) ->
    maps:with(keys(Include), Map);
select(List = [H|_], Include) when not is_tuple(H) ->
    lists:filter(fun (I) -> has(Include, I) end, List);
select(Obj, Include) ->
    reduce(fun (A, K) ->
                   case get(Obj, K) of
                       undefined ->
                           A;
                       V ->
                           set(A, K, V)
                   end
           end, deflike(undefined, Obj), keys(Include)).

insert(Obj, Item) when not is_list(Obj); is_tuple(Item) ->
    set(Obj, Item);
insert([Item|List], Item) ->
    [Item|List];
insert([Other|List], Item) ->
    [Other|insert(List, Item)];
insert([], Item) ->
    [Item].

update(Old, New) when is_map(Old), is_map(New) ->
    maps:merge(Old, New);
update(Old, New) ->
    reduce(fun insert/2, Old, New).

suppress(Obj, ['*']) ->
    nil(Obj);
suppress(Obj, ['*'|Rest]) ->
    fold(fun (T, A) when is_tuple(T) ->
                 set(A, key(T), suppress(val(T), Rest));
             (O, A) when is_list(A) ->
                 lists:reverse([suppress(O, Rest)|A])
         end, nil(Obj), Obj);
suppress(Obj, [{'|', Choices}|Rest]) ->
    fold(fun (C, A) -> suppress(A, C ++ Rest) end, Obj, Choices);
suppress(Obj, [{'=', Literal}]) ->
    remove(Obj, [Literal]);
suppress(Obj, [{'=', Literal}|Rest]) ->
    modify(Obj, [Literal], fun (O) -> suppress(O, Rest) end);
suppress(Obj, [Literal]) ->
    remove(Obj, [Literal]);
suppress(Obj, [Literal|Rest]) ->
    modify(Obj, [Literal], fun (O) -> suppress(O, Rest) end);
suppress(Obj, []) ->
    Obj.

all(Map, Fun) when is_map(Map) ->
    all(maps:to_list(Map), Fun);
all(List, Fun) when is_list(List) ->
    lists:all(Fun, List).

any(Map, Fun) when is_map(Map) ->
    any(maps:to_list(Map), Fun);
any(List, Fun) when is_list(List) ->
    lists:any(Fun, List).

chunk(Obj, N) ->
    lists:reverse(chunk(fun (I, A) -> [I|A] end, [], Obj, N)).

chunk(Fun, Acc, Obj, N) ->
    case fold(fun (I, {K, C, A}) when K < N ->
                      {K + 1, [I|C], A};
                  (I, {_, C, A}) ->
                      {1, [I], Fun(lists:reverse(C), A)}
              end, {0, [], Acc}, Obj) of
        {0, [], A} ->
            A;
        {_, C, A} ->
            Fun(lists:reverse(C), A)
    end.

count(Fun, Acc, N) when is_number(N) ->
    count(Fun, Acc, {0, N});
count(Fun, Acc, {I, N}) when I < N ->
    count(Fun, Fun(I, Acc), {I + 1, N});
count(_, Acc, _) ->
    Acc.

first(List) ->
    head(List).

first(List, Fun) when is_function(Fun) ->
    first(List, Fun, undefined);
first(List, Default) ->
    first(List, fun (_) -> true end, Default).

first(List, Fun, Default) ->
    first(List, Fun, fun (I) -> I end, Default).

first([H|T], Fun, Val, Default) ->
    case Fun(H) of
        true ->
            Val(H);
        false ->
            first(T, Fun, Val, Default)
    end;
first([], _, _, Default) ->
    Default.

range(List, Start) ->
    range(List, Start, fun (_) -> false end).

range(List, Start, Stop) when not is_function(Start) ->
    range(List, fun (I) -> Start =< I end, Stop);
range(List, Start, Stop) when not is_function(Stop) ->
    range(List, Start, fun (I) -> I >= Stop end);
range(List, Start, Stop) ->
    range(List, Start, Stop, [], undefined).

range([H|T], Start, Stop, Acc, true) ->
    case Stop(H) of
        true ->
            Acc;
        false ->
            [H|range(T, Start, Stop, Acc, true)]
    end;
range([H|T], Start, Stop, Acc, _) ->
    case Start(H) of
        true ->
            [H|range(T, Start, Stop, Acc, true)];
        false ->
            range(T, Start, Stop, Acc, false)
    end;
range([], _, _, Acc, _) ->
    Acc.

head([H|_]) ->
    H;
head([]) ->
    undefined.

last(List) ->
    head(lists:reverse(List)).

tail([_|T]) ->
    T;
tail([]) ->
    undefined.

init(undefined, H) ->
    [H];
init([], H) ->
    [H];
init(List, _) ->
    List.

drop([H|T], H) ->
    T;
drop(List, _) ->
    List.

push(List = [H|_], H) ->
    List;
push(List, H) ->
    [H|List].

both(Obj, Path) ->
    {lookup(Obj, Path), Obj}.

default(Obj, Path, Default) ->
    both(create(Obj, Path, Default), Path).

delta(A, B) ->
    fold(fun (Item, Delta) ->
                 Key = key(Item),
                 Val = val(Item),
                 case get(B, Key) of
                     Val ->
                         delete(Delta, Key);
                     _ ->
                         Delta
                 end
         end, B, A).

diff(A, B) ->
    {delta(A, B), delta(B, A)}.

edit(A, {Added, Removed}) ->
    delta(Removed, update(A, Added)).

draw(Obj) ->
    pop(Obj, key(random(Obj))).

draw(Obj, Pref) ->
    case has(Obj, Pref) of
        true ->
            pop(Obj, Pref);
        false ->
            pop(Obj, key(random(Obj)))
    end.

each(Obj, Fun) ->
    fold(fun (I, _) -> Fun(I) end, nil, Obj).

enum(List) ->
    enum(List, 0).

enum([H|T], N) ->
    [{N, H}|enum(T, N + 1)];
enum([], _) ->
    [].

enum(Fun, Acc, Obj) ->
    element(2, fold(fun (I, {N, A}) ->
                            {N + 1, Fun(N, I, A)}
                    end, {0, Acc}, Obj)).

fold(Fun, Acc, Map) when is_map(Map) ->
    maps:fold(fun (K, V, A) -> Fun({K, V}, A) end, Acc, Map);
fold(Fun, Acc, List) when is_list(List) ->
    lists:foldl(Fun, Acc, List);
fold(Fun, Acc, Obj = {Mod, _}) when is_atom(Mod) ->
    Mod:fold(Fun, Acc, Obj);
fold(_, Acc, undefined) ->
    Acc.

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

pair(List) ->
    pair(fun (A, B) -> {A, B} end, List).

pair(Fun, List) ->
    lists:reverse(
      pair(fun ({A, B}, Acc) ->
                   [Fun(A, B)|Acc]
           end, [], List)).

pair(Fun, Acc, [A, B|Rest]) ->
    pair(Fun, Fun({A, B}, Acc), [B|Rest]);
pair(_, Acc, List) when is_list(List) ->
    Acc.

keys(Map) when is_map(Map) ->
    maps:keys(Map);
keys(List) when is_list(List) ->
    [key(Item) || Item <- List].

keys(Iter, Filter) when is_function(Filter) ->
    fold(fun (Item, Acc) ->
                 case Filter(Item) of
                     true ->
                         [key(Item)|Acc];
                     false ->
                         Acc
                 end
         end, [], Iter);
keys(Iter, Filter) ->
    keys(Iter, fun (I) -> val(I) =:= Filter end).

vals(Map) when is_map(Map) ->
    maps:values(Map);
vals(List) when is_list(List) ->
    [val(Item) || Item <- List].

vals(Iter, Filter) when is_function(Filter) ->
    fold(fun (Item, Acc) ->
                 case Filter(Item) of
                     true ->
                         [val(Item)|Acc];
                     false ->
                         Acc
                 end
         end, [], Iter);
vals(Iter, Filter) ->
    vals(Iter, fun (I) -> key(I) =:= Filter end).

index(Obj) ->
    index(Obj, fun invert/1).

index(Obj, Inv) ->
    fold(fun (I, A) -> [Inv(I)|A] end, [], Obj).

invert(Item) ->
    invert(Item, fun val/1).

invert(Item, Val) ->
    {Val(Item), key(Item)}.

mapped(Keys) ->
    mapped(Keys, false).

mapped(Keys, Val) ->
    fold(fun (Key, Acc) -> Acc#{Key => Val} end, #{}, Keys).

random(Int) when is_integer(Int) ->
    X = num:log2floor(Int),
    <<Y:X/integer-unit:8>> = crypto:strong_rand_bytes(X),
    (Int * Y) div (1 bsl (X * 8)) + 1;
random([_|_] = List) ->
    lists:nth(random(length(List)), List);
random([]) ->
    undefined;
random(Iter) ->
    random(iter(Iter)).

reduce(Fun, Acc, Map) when is_map(Map) ->
    maps:fold(fun (K, V, A) -> Fun(A, {K, V}) end, Acc, Map);
reduce(Fun, Acc, [H|T]) ->
    reduce(Fun, Fun(Acc, H), T);
reduce(_, Acc, []) ->
    Acc;
reduce(Fun, Acc, Obj) ->
    fold(fun (I, A) -> Fun(A, I) end, Acc, Obj).

roll(Fun, Acc, [I|Rest]) ->
    case Fun(I, Acc) of
        {stop, A} ->
            A;
        {continue, A} ->
            roll(Fun, A, Rest);
        A ->
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

wrap(Tag, Value, true) ->
    {Tag, Value};
wrap(_, Value, _) ->
    Value.

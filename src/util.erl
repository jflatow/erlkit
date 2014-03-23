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
         urlencode/1,
         quote_plus/1,
         unquote/1,
         count/3,
         enum/3,
         join/2,
         join/3,
         disfix/2,
         lstrip/2,
         rstrip/2,
         strip/2,
         lower/1,
         upper/1]).

-export([do/1,
         repeat/2,
         timeit/2]).

-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= $\. orelse C =:= $- orelse C =:= $~ orelse C =:= $_))).

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
hex(<<Q:4, Rest/bitstring>>) ->
    <<(hexdigit(Q)), (hex(Rest))/binary>>;
hex(<<>>) ->
    <<>>.

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.

urlencode(Props) ->
    list_to_binary(join([<<(quote_plus(K))/binary, $=, (quote_plus(V))/binary>> || {K, V} <- Props], $&)).

quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(IO) ->
    quote_plus(iolist_to_binary(IO), <<>>).

quote_plus(<<>>, Acc) ->
    Acc;
quote_plus(<<C, Rest/binary>>, Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, <<Acc/binary, C>>);
quote_plus(<<" ", Rest/binary>>, Acc) ->
    quote_plus(Rest, <<Acc/binary, "+">>);
quote_plus(<<Hi:4, Lo:4, Rest/binary>>, Acc) ->
    quote_plus(Rest, <<Acc/binary, "%", (hexdigit(Hi)), (hexdigit(Lo))>>).

unquote(String) when is_list(String) ->
    unquote(list_to_binary(String));
unquote(Binary) ->
    unquote(Binary, <<>>).

unquote(<<>>, Acc) ->
    Acc;
unquote(<<"+", Rest/binary>>, Acc) ->
    unquote(Rest, <<Acc/binary, " ">>);
unquote(<<"%", Hi, Lo, Rest/binary>>, Acc) ->
    unquote(Rest, <<Acc/binary, (unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4))>>);
unquote(<<C, Rest/binary>>, Acc) ->
    unquote(Rest, <<Acc/binary, C>>).

count(Fun, Acc, N) when is_number(N) ->
    count(Fun, Acc, {0, N});
count(Fun, Acc, {I, N}) when I < N ->
    count(Fun, Fun(I, Acc), {I + 1, N});
count(_, Acc, _) ->
    Acc.

enum(Fun, Acc, List) ->
    element(2, lists:foldl(fun (I, {N, A}) ->
                                   {N + 1, Fun(N, I, A)}
                           end, {0, Acc}, List)).

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

disfix(Prefix, Str) when is_list(Str), is_binary(Prefix) ->
    disfix(binary_to_list(Prefix), Str);
disfix(Prefix, Str) when is_list(Str) ->
    Str -- Prefix;
disfix(Prefix, Bin) when is_binary(Bin), is_list(Prefix) ->
    disfix(list_to_binary(Prefix), Bin);
disfix(Prefix, Bin) when is_binary(Bin) ->
    Size = size(Prefix),
    case Bin of
        <<Prefix:Size/binary, Rest/binary>> ->
            Rest;
        _ ->
            Bin
    end.

lstrip([C|Rest], C) ->
    lstrip(Rest, C);
lstrip(<<C, Rest/binary>>, C) ->
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
rstrip(<<_, _/binary>> = Bin, C) when is_binary(Bin) ->
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

do({F, A}) ->
    apply(F, A);
do({M, F, A}) ->
    apply(M, F, A).

repeat(D, N) ->
    repeat(D, N, undefined).

repeat(_, 0, Last) ->
    Last;
repeat(D, N, _) ->
    repeat(D, N - 1, do(D)).

timeit(D, N) ->
    timer:tc(fun repeat/2, [D, N]).

-module(util).

-export([mod/2,
         flt/1,
         int/1,
         num/1,
         hex/1,
         hexdigit/1,
         unhexdigit/1,
         urlencode/1,
         quote_plus/1,
         enum/3,
         join/2,
         lstrip/2,
         rstrip/2,
         strip/2,
         lower/1,
         upper/1]).

-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= $\. orelse C =:= $- orelse C =:= $~ orelse C =:= $_))).

mod(X, Y) ->
    case X rem Y of
        R when R < 0 ->
            R + Y;
        R ->
            R
    end.

flt(Flt) when is_float(Flt) ->
    Flt;
flt(Bin) when is_binary(Bin) ->
    flt(binary_to_list(Bin));
flt(Atom) when is_atom(Atom) ->
    flt(atom_to_list(Atom));
flt(List) when is_list(List) ->
    list_to_float(List).

int(Int) when is_integer(Int) ->
    Int;
int(Bin) when is_binary(Bin) ->
    int(binary_to_list(Bin));
int(Atom) when is_atom(Atom) ->
    int(atom_to_list(Atom));
int(List) when is_list(List) ->
    list_to_integer(List).

num(Num) when is_number(Num) ->
    Num;
num(Any) ->
    try flt(Any) catch error:badarg -> int(Any) end.

hex(List) when is_list(List) ->
    hex(list_to_binary(List));
hex(<<Q:4, Rest/bitstring>>) ->
    <<(hexdigit(Q)), (hex(Rest))/binary>>;
hex(<<>>) ->
    <<>>.

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $a + (C - 10).

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
quote_plus(<<$\s, Rest/binary>>, Acc) ->
    quote_plus(Rest, <<Acc/binary, $+>>);
quote_plus(<<Hi:4, Lo:4, Rest/binary>>, Acc) ->
    quote_plus(Rest, <<Acc/binary, $\%, (hexdigit(Hi)), (hexdigit(Lo))>>).

enum(Fun, Acc, List) ->
    element(2, lists:foldl(fun (I, {N, A}) ->
                                   {N + 1, Fun(N, I, A)}
                           end, {0, Acc}, List)).

join([A, B|Rest], Sep) ->
    [A, Sep|join([B|Rest], Sep)];
join(List, _Sep) ->
    List.

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

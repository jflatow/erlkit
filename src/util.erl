-module(util).

-export([ago/1,
         seconds/1,
         int/1,
         hexdigit/1,
         unhexdigit/1,
         urlencode/1,
         quote_plus/1,
         revjoin/3]).

-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= $\. orelse C =:= $- orelse C =:= $~ orelse C =:= $_))).

ago(Seconds) ->
    calendar:gregorian_seconds_to_datetime(seconds(calendar:universal_time()) - Seconds).

seconds(Seconds) when is_integer(Seconds) ->
    Seconds;
seconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime).

int(Int) when is_integer(Int) ->
    Int;
int(Bin) when is_binary(Bin) ->
    list_to_integer(binary_to_list(Bin));
int(Atom) when is_atom(Atom) ->
    list_to_integer(atom_to_list(Atom));
int(List) when is_list(List) ->
    list_to_integer(List).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.

urlencode(Props) ->
    RevPairs = lists:foldl(fun ({K, V}, Acc) ->
                                   [[quote_plus(K), $=, quote_plus(V)] | Acc]
                           end, [], Props),
    lists:flatten(revjoin(RevPairs, $&, [])).

quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Bin) when is_binary(Bin) ->
    quote_plus(binary_to_list(Bin));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(String) ->
    quote_plus(String, []).

quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), $\% | Acc]).

revjoin([], _Separator, Acc) ->
    Acc;
revjoin([S | Rest], Separator, []) ->
    revjoin(Rest, Separator, [S]);
revjoin([S | Rest], Separator, Acc) ->
    revjoin(Rest, Separator, [S, Separator | Acc]).

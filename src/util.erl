-module(util).

-export([ago/1,
         ago/2,
         month/1,
         seconds/1,
         int/1,
         hex/1,
         hexdigit/1,
         unhexdigit/1,
         urlencode/1,
         quote_plus/1,
         revjoin/3,
         write/2,
         write/3]).

-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= $\. orelse C =:= $- orelse C =:= $~ orelse C =:= $_))).

ago(Seconds) ->
    ago(calendar:universal_time(), Seconds).

ago(Time, Seconds) ->
    calendar:gregorian_seconds_to_datetime(seconds(Time) - Seconds).

month(<<"Jan">>) -> 1;
month(<<"Feb">>) -> 2;
month(<<"Mar">>) -> 3;
month(<<"Apr">>) -> 4;
month(<<"May">>) -> 5;
month(<<"Jun">>) -> 6;
month(<<"Jul">>) -> 7;
month(<<"Aug">>) -> 8;
month(<<"Sep">>) -> 9;
month(<<"Oct">>) -> 10;
month(<<"Nov">>) -> 11;
month(<<"Dec">>) -> 12;
month(Str) when is_list(Str) ->
    month(list_to_binary(Str)).

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

write(Path, Data) ->
    write(Path, Data, []).

write(Path, Data, Opts) ->
    Temp = proplists:get_value(temp, Opts, Path ++ ".p"),
    Dirs = proplists:get_value(dirs, Opts, true),
    case file:write_file(Temp, Data) of
        ok ->
            case file:rename(Temp, Path) of
                ok ->
                    ok;
                {error, Error} ->
                    {error, rename, Error}
            end;
        {error, enoent} when Dirs =:= true ->
            case filelib:ensure_dir(Temp) of
                ok ->
                    write(Path, Data, Opts);
                {error, Error} ->
                    {error, mkdir, Error}
            end;
        {error, Error} ->
            {error, write, Error}
    end.

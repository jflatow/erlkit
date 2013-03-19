-module(timedb).

-export([open/1,
         open/2,
         path/2,
         path/3,
         foldl/3,
         foldl/4,
         foldr/3,
         foldr/4,
         folditems/3,
         format/2,
         all/1,
         between/2,
         nafter/3,
         nbefore/3,
         log/2,
         log/3,
         usort/1]).

-import(util, [datetime/1, timestamp/1]).

-record(timedb, {root, opts}).

open(Root) ->
    open(Root, []).

open(Root, Opts) when is_binary(Root) ->
    open(binary_to_list(Root), Opts);
open(Root, Opts) ->
    #timedb{root=Root, opts=Opts}.

path(#timedb{root=Root, opts=Opts}, Time) ->
    path(Root, Time, proplists:get_value(by, Opts, day)).

path(Root, {{Y, M, D}, {H, Mi, S}}, second) ->
    lists:flatten(io_lib:format("~s/~4..0B/~2..0B/~2..0B/~2..0B/~2..0B/~2..0B", [Root, Y, M, D, H, Mi, S]));
path(Root, {{Y, M, D}, {H, Mi, _}}, minute) ->
    lists:flatten(io_lib:format("~s/~4..0B/~2..0B/~2..0B/~2..0B/~2..0B", [Root, Y, M, D, H, Mi]));
path(Root, {{Y, M, D}, {H, _, _}}, hour) ->
    lists:flatten(io_lib:format("~s/~4..0B/~2..0B/~2..0B/~2..0B", [Root, Y, M, D, H]));
path(Root, {{Y, M, D}, _}, day) ->
    lists:flatten(io_lib:format("~s/~4..0B/~2..0B/~2..0B", [Root, Y, M, D]));
path(Root, {{Y, M, _}, _}, month) ->
    lists:flatten(io_lib:format("~s/~4..0B/~2..0B", [Root, Y, M]));
path(Root, {{Y, _, _}, _}, year) ->
    lists:flatten(io_lib:format("~s/~4..0B", [Root, Y])).

lower(_, undefined) ->
    undefined;
lower(TimeDB, Time) ->
    path(TimeDB, Time).

upper(_, undefined) ->
    undefined;
upper(TimeDB, Time) ->
    path(TimeDB, Time) ++ "~".

foldl(TimeDB, Fun, Acc) ->
    foldl(TimeDB, Fun, Acc, {undefined, undefined}).

foldl(#timedb{root=Root} = TimeDB, Fun, Acc, {T1, T2}) ->
    path:foldl(Root, Fun, Acc, {lower(TimeDB, T1), upper(TimeDB, T2)}).

foldr(TimeDB, Fun, Acc) ->
    foldr(TimeDB, Fun, Acc, {undefined, undefined}).

foldr(#timedb{root=Root} = TimeDB, Fun, Acc, {T1, T2}) ->
    path:foldr(Root, Fun, Acc, {upper(TimeDB, T1), lower(TimeDB, T2)}).

folditems(Path, Fun, Acc) ->
    case file:read_file(Path) of
        {ok, Data} ->
            folditems(Data, Fun, Acc, 0);
        {error, enoent} ->
            Acc
    end.

folditems(<<>>, _Fun, Acc, _N) ->
    Acc;
folditems(<<Timestamp:19/binary, " ", A/binary>>, Fun, Acc, N) ->
    Uniq = list_to_binary([Timestamp, "-", integer_to_list(N)]),
    [S, B] = binary:split(A, <<" ">>),
    Size = list_to_integer(binary_to_list(S)),
    <<Data:Size/binary, "\n", C/binary>> = B,
    Time = datetime(Timestamp),
    folditems(C, Fun, Fun({Uniq, Time, Data}, Acc), N + 1).

format(Time, Data) ->
    io_lib:format("~s ~B ~s~n", [timestamp(Time), size(Data), Data]).

all(TimeDB) ->
    between(TimeDB, {undefined, undefined}).

between(TimeDB, {T1, T2}) ->
    foldl(TimeDB,
          fun (Path, Acc) ->
                  folditems(Path,
                            fun ({_, Time, _} = Item, Acc_) when
                                (T1 =:= undefined orelse Time >= T1) andalso
                                (T2 =:= undefined orelse Time =< T2) ->
                                    [Item|Acc_];
                                (_, Acc_) ->
                                    Acc_
                            end, Acc)
          end, [], {T1, T2}).

nafter(TimeDB, Id, Max) ->
    {_N, Items_} =
        foldl(TimeDB,
              fun (_, {N, _} = Acc) when N >= Max ->
                      {stop, Acc};
                  (Path, Acc) ->
                      folditems(Path,
                                fun ({Uniq, _, _} = Item, {N, Items}) when Uniq > Id; Id =:= undefined ->
                                        {N + 1, [Item|Items]};
                                    (_, Acc_) ->
                                        Acc_
                                end, Acc)
              end, {0, []}, {datetime(Id), undefined}),
    lists:sublist(lists:usort(Items_), Max).

nbefore(TimeDB, Id, Max) ->
    {N_, Items_} =
        foldr(TimeDB,
              fun (_, {N, _} = Acc) when N >= Max ->
                      {stop, Acc};
                  (Path, Acc) ->
                      folditems(Path,
                                fun ({Uniq, _, _} = Item, {N, Items}) when Uniq < Id; Id =:= undefined ->
                                        {N + 1, [Item|Items]};
                                    (_, Acc_) ->
                                        Acc_
                                end, Acc)
              end, {0, []}, {datetime(Id), undefined}),
    lists:nthtail(max(0, N_ - Max), lists:usort(Items_)).

log(TimeDB, Data) ->
    log(TimeDB, calendar:universal_time(), Data).

log(TimeDB, Time, <<Data/binary>>) ->
    Path = path(TimeDB, Time),
    case file:open(Path, [append, binary]) of
        {ok, File} ->
            ok = file:write(File, format(Time, Data)),
            ok = file:close(File); %% XXX: potential bottleneck
        {error, enoent} ->
            case filelib:ensure_dir(Path) of
                ok ->
                    log(TimeDB, Time, Data);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

usort(TimeDB) ->
    foldl(TimeDB,
          fun (Path, _) ->
                  Vals = folditems(Path, fun ({_, T, D}, V) -> [{T, D}|V] end, []),
                  path:write(Path, [format(T, D) || {T, D} <- lists:usort(Vals)])
          end, []).

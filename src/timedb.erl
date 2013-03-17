-module(timedb).

-export([open/1,
         path/2,
         between/2,
         nafter/3,
         nbefore/3,
         log/3]).

-import(util, [datetime/1, timestamp/1]).

-record(timedb, {root}).

open(Root) when is_binary(Root) ->
    open(binary_to_list(Root));
open(Root) ->
    #timedb{root=Root}.

path(#timedb{root=Root}, Time) ->
    path(Root, Time);
path(Root, {{Y, M, D}, _}) ->
    lists:flatten(io_lib:format("~s/~4..0B/~2..0B/~2..0B", [Root, Y, M, D])).

lower(_, undefined) ->
    undefined;
lower(TimeDB, Time) ->
    path(TimeDB, Time).

upper(_, undefined) ->
    undefined;
upper(TimeDB, Time) ->
    path(TimeDB, Time) ++ "~".

foldl(#timedb{root=Root} = TimeDB, Fun, Acc, {T1, T2}) ->
    path:foldl(Root, Fun, Acc, {lower(TimeDB, T1), upper(TimeDB, T2)}).

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

log(TimeDB, Time, <<Data/binary>>) ->
    Path = path(TimeDB, Time),
    case file:open(Path, [append, binary]) of
        {ok, File} ->
            ok = file:write(File, io_lib:format("~s ~B ~s~n", [timestamp(Time), size(Data), Data])),
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

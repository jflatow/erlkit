-module(timedb).

-export([open/1,
         datetime/1,
         timestamp/1,
         nextday/1,
         prevday/1,
         passtime/2,
         first/2,
         oldest/1,
         newest/1,
         path/2,
         between/2,
         nafter/3,
         nbefore/3,
         log/3]).

-import(util, [int/1]).

-record(timedb, {root}).

open(Root) when is_binary(Root) ->
    open(binary_to_list(Root));
open(Root) ->
    #timedb{root=Root}.

%% not very strict parsing of timestamps
datetime(Seconds) when is_integer(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds);
datetime(<<Y:4/binary, "/", M:2/binary, "/", D:2/binary, " ", H:2/binary, ":", Mi:2/binary, ":", S:2/binary, _/binary>>) ->
    {{int(Y), int(M), int(D)}, {int(H), int(Mi), int(S)}};
datetime(_) ->
    undefined.

timestamp({{Y, M, D}, {H, Mi, S}}) ->
    io_lib:format("~4..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Mi, S]).

nextday(Time) ->
    passtime(Time, 86400).

prevday(Time) ->
    passtime(Time, -86400).

passtime(Time, Seconds) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Time) + Seconds).

pathtime(Path) ->
    pathtime(lists:reverse(filename:split(Path)), acc).

pathtime([D, M, Y|_], acc) ->
    {{int(Y), int(M), int(D)}, {0, 0, 0}}.

first([], _) ->
    undefined;
first([File|Ordered], Order) ->
    case filelib:is_regular(File) of
        true ->
            File;
        false ->
            case file:list_dir(File) of
                {ok, [_|_] = Files} ->
                    first([filename:join(File, F) || F <- Order(Files), is_integer(catch list_to_integer(F))], Order);
                _ ->
                    first(Ordered, Order)
            end
    end.

oldest(#timedb{root=Root}) ->
    first([Root], fun lists:usort/1).

newest(#timedb{root=Root}) ->
    first([Root], fun (L) -> lists:reverse(lists:usort(L)) end).

path(#timedb{root=Root}, {{Y, M, D}, _}) ->
    io_lib:format("~s/~4..0B/~2..0B/~2..0B", [Root, Y, M, D]).

foldpaths(TimeDB, {{Date, _} = T, {Date, _}}, Fun, Acc) ->
    Fun(path(TimeDB, T), Acc);
foldpaths(TimeDB, {T1, T2}, Fun, Acc) when T1 < T2 ->
    foldpaths(TimeDB, {nextday(T1), T2}, Fun, Fun(path(TimeDB, T1), Acc));
foldpaths(_, {_, _}, _, Acc) ->
    Acc.

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

between(TimeDB, {undefined, T2}) ->
    case oldest(TimeDB) of
        undefined ->
            [];
        Path ->
            between(TimeDB, {pathtime(Path), T2})
    end;
between(TimeDB, {T1, undefined}) ->
    case newest(TimeDB) of
        undefined ->
            [];
        Path ->
            between(TimeDB, {T1, nextday(pathtime(Path))})
    end;
between(TimeDB, {T1, T2} = Range) ->
    foldpaths(TimeDB, Range,
              fun (Path, Acc) ->
                      folditems(Path,
                                fun ({_, Time, _} = Item, Acc_) when Time >= T1, Time =< T2 ->
                                        [Item|Acc_];
                                    (_, Acc_) ->
                                        Acc_
                                end, Acc)
              end, []).

%% Just get up to N items before or after a particular Id or partial Id (i.e. timestamp)
%% This is different than e.g. 'between(_, {undefined, T})' because we can jump out early once we have enough items.
%% We need to optimize for this case since it is quite common.

nafter(TimeDB, undefined, Max) ->
    nafter(TimeDB, undefined, Max, pathtime(oldest(TimeDB)));
nafter(TimeDB, Id, Max) ->
    nafter(TimeDB, Id, Max, datetime(Id)).

nafter(TimeDB, Id, Max, At) ->
    case newest(TimeDB) of
        undefined ->
            [];
        Path ->
            nafter(TimeDB, Id, Max, At, nextday(pathtime(Path)), {0, []})
    end.

nafter(_, _, Max, _, _, {N, Items}) when N >= Max ->
    lists:sublist(lists:usort(Items), Max);
nafter(_, _, _, At, Newest, {_, Items}) when At > Newest ->
    lists:usort(Items);
nafter(TimeDB, Id, Max, At, Newest, Acc) ->
    nafter(TimeDB, Id, Max, nextday(At), Newest,
           folditems(path(TimeDB, At),
                     fun ({Uniq, _, _} = Item, {N, Items}) when Uniq > Id; Id =:= undefined ->
                             {N + 1, [Item|Items]};
                         (_, Acc_) ->
                             Acc_
                     end, Acc)).

nbefore(TimeDB, undefined, Max) ->
    nbefore(TimeDB, undefined, Max, nextday(pathtime(newest(TimeDB))));
nbefore(TimeDB, Id, Max) ->
    nbefore(TimeDB, Id, Max, datetime(Id)).

nbefore(TimeDB, Id, Max, At) ->
    case oldest(TimeDB) of
        undefined ->
            [];
        Path ->
            nbefore(TimeDB, Id, Max, At, pathtime(Path), {0, []})
    end.

nbefore(_, _, Max, _, _, {N, Items}) when N >= Max ->
    lists:nthtail(N - Max, lists:usort(Items));
nbefore(_, _, _, At, Oldest, {_, Items}) when At < Oldest ->
    lists:usort(Items);
nbefore(TimeDB, Id, Max, At, Oldest, Acc) ->
    nbefore(TimeDB, Id, Max, prevday(At), Oldest,
            folditems(path(TimeDB, At),
                      fun ({Uniq, _, _} = Item, {N, Items}) when Uniq < Id; Id =:= undefined ->
                              {N + 1, [Item|Items]};
                          (_, Acc_) ->
                              Acc_
                      end, Acc)).

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

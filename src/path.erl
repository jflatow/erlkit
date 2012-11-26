-module(path).

-export([fold/3,
         fold/4,
         head/1,
         head/2,
         head/3,
         last/1,
         last/2,
         last/3]).

fold(Tree, Fun, Acc) ->
    fold(Tree, Fun, Acc, {undefined, undefined}).

fold(Path, Fun, Acc, Bounds) when is_binary(Path) ->
    fold([binary_to_list(Path)], Fun, Acc, Bounds);
fold([C|_] = Path, Fun, Acc, Bounds) when is_integer(C) ->
    fold([Path], Fun, Acc, Bounds);
fold([], _, Acc, _) ->
    Acc;
fold([Path|_], _, Acc, {_, Upper}) when Upper =/= undefined, Path >= Upper ->
    Acc;
fold([Path|Tail], Fun, Acc, {Lower, Upper}) when Lower =:= undefined; Lower =< Path ->
    case file:list_dir(Path) of
        {ok, Paths} ->
            fold([filename:join(Path, F) || F <- lists:usort(Paths)] ++ Tail, Fun, Acc, {Lower, Upper});
        {error, enotdir} ->
            fold(Tail, Fun, Fun(Path, Acc), {Lower, Upper})
    end;
fold([Path|Tail], Fun, Acc, {Lower, Upper}) ->
    case lists:prefix(Path, Lower) of
        true ->
            case file:list_dir(Path) of
                {ok, Paths} ->
                    fold([filename:join(Path, F) || F <- lists:usort(Paths)] ++ Tail, Fun, Acc, {Lower, Upper});
                _ ->
                    fold(Tail, Fun, Acc, {Lower, Upper})
            end;
        false ->
            fold(Tail, Fun, Acc, {Lower, Upper})
    end.

head(Tree) ->
    head(Tree, fun lists:usort/1).

head(Tree, Order) ->
    head(Tree, Order, fun (_) -> true end).

head(Path, Order, Filter) when is_binary(Path) ->
    head([Path], Order, Filter);
head([C|_] = Path, Order, Filter) when is_integer(C) ->
    head([Path], Order, Filter);
head([], _, _) ->
    undefined;
head([Path|Tail], Order, Filter) ->
    case filelib:is_regular(Path) of
        true ->
            Path;
        false ->
            case file:list_dir(Path) of
                {ok, [_|_] = Paths} ->
                    head([filename:join(Path, F) || F <- Order(Paths), Filter(F)] ++ Tail, Order, Filter);
                _ ->
                    head(Tail, Order, Filter)
            end
    end.

last(Tree) ->
    last(Tree, fun lists:usort/1).

last(Tree, Order) ->
    last(Tree, Order, fun (_) -> true end).

last(Tree, Order, Filter) ->
    head(Tree, fun (L) -> lists:reverse(Order(L)) end, Filter).

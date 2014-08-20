-module(path).

-export([foldl/3,
         foldl/4,
         foldl/5,
         foldr/3,
         foldr/4,
         foldr/5,
         foldlines/3,
         foldlines/4,
         lines/1,
         lines/2,
         join/2,
         joinl/2,
         test/2,
         test/3,
         head/1,
         head/2,
         last/1,
         last/2,
         list/1,
         rmrf/1,
         mkdir/1,
         write/2,
         write/3]).

-include_lib("kernel/include/file.hrl").

foldl(Tree, Fun, Acc) ->
    foldl(Tree, Fun, Acc, {undefined, undefined}).

foldl(Tree, Fun, Acc, {_, _} = Bounds) ->
    foldl(Tree, Fun, Acc, Bounds, fun lists:usort/1);
foldl(Tree, Fun, Acc, Order) when is_function(Order) ->
    foldl(Tree, Fun, Acc, {undefined, undefined}, Order).

foldl(_, _, {stop, Acc}, _, _) ->
    Acc;
foldl(Path, Fun, Acc, Bounds, Order) when is_binary(Path) ->
    foldl([binary_to_list(Path)], Fun, Acc, Bounds, Order);
foldl([C|_] = Path, Fun, Acc, Bounds, Order) when is_integer(C) ->
    foldl([Path], Fun, Acc, Bounds, Order);
foldl([], _, Acc, _, _) ->
    Acc;
foldl([Path|_], _, Acc, {_, Upper}, _) when Upper =/= undefined, Path >= Upper ->
    Acc;
foldl([Path|Tail], Fun, Acc, {Lower, Upper}, Order) when Lower =:= undefined; Path >= Lower ->
    case file:list_dir(Path) of
        {ok, Paths} ->
            foldl(joinl(Path, Order(Paths)) ++ Tail, Fun, Acc, {Lower, Upper}, Order);
        {error, enotdir} ->
            foldl(Tail, Fun, Fun(Path, Acc), {Lower, Upper}, Order);
        {error, enoent} ->
            foldl(Tail, Fun, Acc, {Lower, Upper}, Order)
    end;
foldl([Path|Tail], Fun, Acc, {Lower, Upper}, Order) when Path < Lower ->
    case lists:prefix(Path, Lower) of
        true ->
            case file:list_dir(Path) of
                {ok, Paths} ->
                    foldl(joinl(Path, Order(Paths)) ++ Tail, Fun, Acc, {Lower, Upper}, Order);
                _ ->
                    foldl(Tail, Fun, Acc, {Lower, Upper}, Order)
            end;
        false ->
            foldl(Tail, Fun, Acc, {Lower, Upper}, Order)
    end.

foldr(Tree, Fun, Acc) ->
    foldr(Tree, Fun, Acc, {undefined, undefined}).

foldr(Tree, Fun, Acc, {_, _} = Bounds) ->
    foldr(Tree, Fun, Acc, Bounds, fun (L) -> lists:reverse(lists:usort(L)) end);
foldr(Tree, Fun, Acc, Order) when is_function(Order) ->
    foldr(Tree, Fun, Acc, {undefined, undefined}, Order).

foldr(_, _, {stop, Acc}, _, _) ->
    Acc;
foldr(Path, Fun, Acc, Bounds, Order) when is_binary(Path) ->
    foldr([binary_to_list(Path)], Fun, Acc, Bounds, Order);
foldr([C|_] = Path, Fun, Acc, Bounds, Order) when is_integer(C) ->
    foldr([Path], Fun, Acc, Bounds, Order);
foldr([], _, Acc, _, _) ->
    Acc;
foldr([Path|Tail], Fun, Acc, {Upper, Lower}, Order) when Lower =/= undefined, Path < Lower ->
    case lists:prefix(Path, Lower) of
        true ->
            case file:list_dir(Path) of
                {ok, Paths} ->
                    foldr(joinl(Path, Order(Paths)) ++ Tail, Fun, Acc, {Upper, Lower}, Order);
                _ ->
                    foldr(Tail, Fun, Acc, {Upper, Lower}, Order)
            end;
        false ->
            Acc
    end;
foldr([Path|Tail], Fun, Acc, {Upper, Lower}, Order) when Upper =:= undefined; Path < Upper ->
    case file:list_dir(Path) of
        {ok, Paths} ->
            foldr(joinl(Path, Order(Paths)) ++ Tail, Fun, Acc, {Upper, Lower}, Order);
        {error, enotdir} ->
            foldr(Tail, Fun, Fun(Path, Acc), {Upper, Lower}, Order);
        {error, enoent} ->
            foldr(Tail, Fun, Acc, {Upper, Lower}, Order)
    end;
foldr([Path|Tail], Fun, Acc, {Upper, Lower}, Order) when Path >= Upper ->
    foldr(Tail, Fun, Acc, {Upper, Lower}, Order).

foldlines(Path, Fun, Acc) ->
    foldlines(Path, Fun, Acc, []).

foldlines(Path, Fun, Acc, Opts) when is_binary(Path); is_list(Path) ->
    case file:open(Path, [binary, read, raw, {read_ahead, 10 * 1024 bsl 10}]) of
        {ok, File} ->
            foldlines(File, Fun, Acc, Opts);
        {error, eisdir} ->
            foldl(Path, fun (P, A) -> foldlines(P, Fun, A, Opts) end, Acc);
        {error, enoent} ->
            Acc
    end;
foldlines(File, Fun, Acc, Opts) ->
    Trim = proplists:get_value(trim, Opts),
    case file:read_line(File) of
        {ok, Line} when Trim =:= true ->
            foldlines(File, Fun, Fun(util:rstrip(Line, $\n), Acc), Opts);
        {ok, Line} ->
            foldlines(File, Fun, Fun(Line, Acc), Opts);
        eof ->
            ok = file:close(File),
            Acc
    end.

lines(Path) ->
    lines(Path, []).

lines(Path, Opts) ->
    lists:reverse(foldlines(Path, fun (Line, Acc) -> [Line|Acc] end, [], Opts)).

join(Dir, Name) ->
    filename:join(Dir, Name).

joinl(Dir, Names) ->
    [join(Dir, Name) || Name <- Names].

test(Path, Opts) ->
    case file:read_file_info(Path) of
        {ok, Info} ->
            test(Path, Info, Opts);
        _ ->
            false
    end.

test(Path, Info, List) when is_list(List) ->
    lists:all(fun (X) -> test(Path, Info, X) end, List);
test(_, #file_info{type=Type}, directory) ->
    Type =:= directory;
test(_, #file_info{type=Type}, regular) ->
    Type =:= regular;
test(_, #file_info{mode=Mode}, executable) ->
    Mode band 8#00111 > 0.

head(Tree) ->
    head(Tree, fun lists:usort/1).

head(Path, Order) when is_binary(Path) ->
    head([Path], Order);
head([C|_] = Path, Order) when is_integer(C) ->
    head([Path], Order);
head([], _) ->
    undefined;
head([Path|Tail], Order) ->
    case filelib:is_regular(Path) of
        true ->
            Path;
        false ->
            case file:list_dir(Path) of
                {ok, [_|_] = Paths} ->
                    head(joinl(Path, Order(Paths)) ++ Tail, Order);
                _ ->
                    head(Tail, Order)
            end
    end.

last(Tree) ->
    last(Tree, fun lists:usort/1).

last(Tree, Order) ->
    head(Tree, fun (L) -> lists:reverse(Order(L)) end).


list(Path) ->
    case file:list_dir(Path) of
        {ok, Filenames} ->
            Filenames;
        {error, enoent} ->
            []
    end.

rmrf(Path) ->
    case file:delete(Path) of
        ok ->
            ok;
        {error, enoent} ->
            ok;
        {error, eperm} ->
            ok = case file:list_dir(Path) of
                     {ok, Paths} ->
                         lists:foldl(fun (P, ok) ->
                                             rmrf(join(Path, P))
                                     end, ok, Paths)
                 end,
            file:del_dir(Path)
    end.

mkdir(Path) ->
    filelib:ensure_dir(join(Path, sentinel)).

write(Path, Dump) ->
    write(Path, Dump, []).

write(Path, Dump, Opts) when is_binary(Path) ->
    write(binary_to_list(Path), Dump, Opts);
write(Path, Data, Opts) when is_binary(Data); is_list(Data) ->
    write(Path, fun (Temp) -> file:write_file(Temp, Data) end, Opts);
write(Path, Dump, Opts) when is_function(Dump) ->
    Temp = proplists:get_value(temp, Opts, Path ++ ".p"),
    Dirs = proplists:get_value(dirs, Opts, true),
    case Dump(Temp) of
        {error, enoent} when Dirs =:= true ->
            case filelib:ensure_dir(Temp) of
                ok ->
                    write(Path, Dump, Opts);
                {error, Error} ->
                    {error, {mkdir, Error}}
            end;
        {error, Error} ->
            {error, {write, Error}};
        Result ->
            case file:rename(Temp, Path) of
                ok ->
                    Result;
                {error, Error} ->
                    {error, {rename, Error}}
            end
    end.

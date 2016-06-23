-module(path).
-author("Jared Flatow").

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
         info/1,
         info/2,
         safe/1,
         size/1,
         size/2,
         test/2,
         test/3,
         next/1,
         next/2,
         prev/1,
         prev/2,
         head/1,
         head/2,
         last/1,
         last/2,
         follow/1,
         link/2,
         link/3,
         list/1,
         ls/1,
         ls/2,
         mkdir/1,
         read/1,
         read/2,
         rmrf/1,
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
foldl(Path, Fun, Acc, {Lower, Upper}, Order) when is_binary(Lower) ->
    foldl(Path, Fun, Acc, {binary_to_list(Lower), Upper}, Order);
foldl(Path, Fun, Acc, {Lower, Upper}, Order) when is_binary(Upper) ->
    foldl(Path, Fun, Acc, {Lower, binary_to_list(Upper)}, Order);
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
foldr(Path, Fun, Acc, {Lower, Upper}, Order) when is_binary(Lower) ->
    foldr(Path, Fun, Acc, {binary_to_list(Lower), Upper}, Order);
foldr(Path, Fun, Acc, {Lower, Upper}, Order) when is_binary(Upper) ->
    foldr(Path, Fun, Acc, {Lower, binary_to_list(Upper)}, Order);
foldr([C|_] = Path, Fun, Acc, Bounds, Order) when is_integer(C) ->
    foldr([Path], Fun, Acc, Bounds, Order);
foldr([], _, Acc, _, _) ->
    Acc;
foldr([Path|Tail], Fun, Acc, {Lower, Upper}, Order) when Lower =/= undefined, Path < Lower ->
    case lists:prefix(Path, Lower) of
        true ->
            case file:list_dir(Path) of
                {ok, Paths} ->
                    foldr(joinl(Path, Order(Paths)) ++ Tail, Fun, Acc, {Lower, Upper}, Order);
                _ ->
                    foldr(Tail, Fun, Acc, {Lower, Upper}, Order)
            end;
        false ->
            Acc
    end;
foldr([Path|Tail], Fun, Acc, {Lower, Upper}, Order) when Upper =:= undefined; Path < Upper ->
    case file:list_dir(Path) of
        {ok, Paths} ->
            foldr(joinl(Path, Order(Paths)) ++ Tail, Fun, Acc, {Lower, Upper}, Order);
        {error, enotdir} ->
            foldr(Tail, Fun, Fun(Path, Acc), {Lower, Upper}, Order);
        {error, enoent} ->
            foldr(Tail, Fun, Acc, {Lower, Upper}, Order)
    end;
foldr([Path|Tail], Fun, Acc, {Lower, Upper}, Order) when Path >= Upper ->
    foldr(Tail, Fun, Acc, {Lower, Upper}, Order).

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
    Trim = util:get(Opts, trim),
    case file:read_line(File) of
        {ok, Line} when Trim =:= true ->
            foldlines(File, Fun, Fun(str:rstrip(Line, $\n), Acc), Opts);
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

info(Path) ->
    info(Path, []).

info(Path, Opts) ->
    case util:get(Opts, follow) of
        true ->
            file:read_file_info(Path);
        _ ->
            file:read_link_info(Path)
    end.

safe(<<>>) ->
    true;
safe(Path) when is_binary(Path) ->
    [P || <<C, _/bits>> = P <- filename:split(Path), C =:= $. orelse C =:= $/] =:= [];
safe(Path) ->
    safe(util:bin(Path)).

size(Path) ->
    path:size(Path, []).

size(Path, Opts) ->
    case info(Path, Opts) of
        {ok, #file_info{size=Size, type=directory}} ->
            Size + lists:sum([path:size(P, Opts) || P <- list(Path)]);
        {ok, #file_info{size=Size}} ->
            Size;
        {error, enoent} ->
            0
    end.

test(Path, Features) ->
    test(Path, Features, []).

test(Path, Features, Opts) ->
    case info(Path, Opts) of
        {ok, Info} ->
            test(Path, Info, Features, Opts);
        {error, enoent} ->
            false
    end.

test(Path, Info, List, Opts) when is_list(List) ->
    lists:all(fun (X) -> test(Path, Info, X, Opts) end, List);
test(_, #file_info{type=Type}, directory, _) ->
    Type =:= directory;
test(_, #file_info{type=Type}, regular, _) ->
    Type =:= regular;
test(_, #file_info{type=Type}, symlink, _) ->
    Type =:= symlink;
test(_, #file_info{mode=Mode}, executable, _) ->
    Mode band 8#00111 > 0.

next(Path) ->
    next(Path, fun lists:usort/1).

next(Path, forward) ->
    next(Path);
next(Path, reverse) ->
    prev(Path);
next(Path, Order) when is_binary(Path) ->
    next(binary_to_list(Path), Order);
next(Path, Order) ->
    Parent = filename:dirname(Path),
    Name = filename:basename(Path),
    case file:list_dir(Parent) of
        {ok, Paths} ->
            case lists:dropwhile(fun (N) -> N =/= Name end, Order(Paths)) of
                [Name, Next|_] ->
                    join(Parent, Next);
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

prev(Path) ->
    prev(Path, fun lists:usort/1).

prev(Path, forward) ->
    prev(Path);
prev(Path, reverse) ->
    next(Path);
prev(Path, Order) ->
    next(Path, fun (L) -> lists:reverse(Order(L)) end).

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

follow(Path) ->
    case file:read_link_all(Path) of
        {ok, Linked} ->
            follow(filename:absname(Linked, filename:dirname(Path)));
        {error, einval} ->
            {ok, Path};
        {error, enoent} ->
            {error, {enoent, Path}}
    end.

link(Existing, New) ->
    link(Existing, New, []).

link(Existing, New, Opts) ->
    Dirs = util:get(Opts, dirs, true),
    case file:make_symlink(Existing, New) of
        {error, enoent} when Dirs =:= true ->
            case filelib:ensure_dir(New) of
                ok ->
                    file:make_symlink(Existing, New);
                {error, Error} ->
                    {error, {mkdir, Error}}
            end;
        Result ->
            Result
    end.

list(Path) ->
    joinl(Path, ls(Path)).

ls(Path) ->
    case file:list_dir(Path) of
        {ok, Filenames} ->
            Filenames;
        {error, enotdir} ->
            [];
        {error, enoent} ->
            []
    end.

ls(_, 0) ->
    [];
ls(Path, Depth) ->
    lists:foldl(fun (Name, Acc) ->
                        case ls(join(Path, Name), Depth - 1) of
                            [] ->
                                [[Name]] ++ Acc;
                            Cs ->
                                [[Name|C] || C <- Cs] ++ Acc
                        end
                end, [], lists:reverse(ls(Path))).

mkdir(Path) ->
    filelib:ensure_dir(join(Path, sentinel)).

read(Path) ->
    read(Path, undefined).

read(Path, Default) ->
    case file:read_file(Path) of
        {ok, Data} ->
            Data;
        {error, enoent} ->
            Default
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

write(Path, Dump) ->
    write(Path, Dump, []).

write(Path, Dump, Opts) when is_binary(Path) ->
    write(binary_to_list(Path), Dump, Opts);
write(Path, Data, Opts) when is_binary(Data); is_list(Data) ->
    write(Path, fun (Temp) -> file:write_file(Temp, Data) end, Opts);
write(Path, Dump, Opts) when is_function(Dump) ->
    Temp = util:get(Opts, temp, Path ++ ".p"),
    Dirs = util:get(Opts, dirs, true),
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

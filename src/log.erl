-module(log).

-export([open/1,
         open/2,
         close/1,
         flush/1,
         write/2,
         write/3,
         foldl/3,
         foldl/4,
         range/2,
         since/2]).

-behavior(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(Magic, "logfile:").
-define(OZero, 42).
-record(state, {root, path, file, offs, ckpt, depth, limit}).

%% external

open(Root) ->
    open(Root, []).

open(Root, Opts) when is_binary(Root) ->
    open(binary_to_list(Root), Opts);
open(Root, Opts) ->
    gen_server:start_link(?MODULE, [Root, Opts], []).

close(Log) ->
    gen_server:call(Log, {do, close}).

flush(Log) ->
    gen_server:call(Log, {do, flush}).

write(Log, Entry) ->
    write(Log, Entry, cast).

write(Log, Entry, call) ->
    gen_server:call(Log, {do, {write, Entry}});
write(Log, Entry, cast) ->
    gen_server:cast(Log, {do, {write, Entry}}).

foldl(Log, Fun, Acc) ->
    foldl(Log, Fun, Acc, {undefined, undefined}).

foldl(Log, Fun, Acc, {I1, I2}) ->
    case gen_server:call(Log, state) of
        #state{root=Root} = State ->
            Lower = {PLo, _} = lower(State, I1),
            Upper = {PHi, _} = upper(State, I2),
            path:foldl(Root,
                       fun (P, A) ->
                               foldentries(P, Fun, A, {Lower, Upper})
                       end, Acc, {PLo, filename:join(PHi, "~")})
    end.

range(Log, Range) ->
    foldl(Log, fun (Item, Acc) -> [Item|Acc] end, [], Range).

since(Log, Id) ->
    range(Log, {Id, undefined}).

%% gen_server

init([Root, Opts]) ->
    Depth = proplists:get_value(depth, Opts, 2),
    Limit = proplists:get_value(limit, Opts, 64 bsl 20),
    Last = last(Root, Depth),
    Path = util:strip(util:disfix(Root, Last), $/),
    case file(Last) of
        {ok, File} ->
            case do(verify, #state{root=Root,
                                   path=Path,
                                   file=File,
                                   depth=Depth,
                                   limit=Limit}) of
                {ok, State} ->
                    {ok, State};
                Error ->
                    {stop, Error}
            end;
        Error ->
            {stop, Error}
    end.

handle_call(state, _From, State) ->
    {reply, State, State};
handle_call({do, close}, _From, #state{file=File} = State) ->
    case do(flush, State) of
        {ok, S} ->
            case file:close(File) of
                ok ->
                    {stop, closed, ok, S};
                Error ->
                    {reply, Error, S}
            end;
        Error ->
            {reply, Error, State}
    end;
handle_call({do, What}, _From, State) ->
    {Reply, S} = do(What, State),
    {reply, Reply, S}.

handle_cast({do, What}, State) ->
    {_Ignore, S} = do(What, State),
    {noreply, S}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal

int_to_path(Int, Depth) ->
    int_to_path(Int, Depth, 36).

int_to_path(Int, Depth, Base) ->
    Str = list_to_binary(io_lib:format("~" ++ integer_to_list(2 * Depth) ++
                                       "." ++ integer_to_list(Base) ++ ".0B", [Int])),
    filename:join([B || <<B:2/binary>> <= Str]).

path_to_int(Path, Depth) ->
    path_to_int(Path, Depth, 36).

path_to_int(Path, Depth, Base) when is_binary(Path) ->
    path_to_int(binary_to_list(Path), Depth, Base);
path_to_int(Path, Depth, Base) ->
    Unit = Base * Base,
    element(2, lists:foldl(fun (B, {Pow, Int}) ->
                                   {Pow div Unit, Int + list_to_integer(B, Base) * Pow}
                           end, {num:pow(Unit, Depth - 1), 0}, filename:split(Path))).

str(List) when is_list(List) ->
    List;
str(Bin) when is_binary(Bin) ->
    binary_to_list(Bin).

lower(#state{root=Root}, undefined) ->
    {str(path:head(Root)), ?OZero};
lower(#state{root=Root}, {Path, Offs}) ->
    {str(filename:join(Root, Path)), Offs}.

upper(#state{root=Root, path=Path, offs=Offs}, undefined) ->
    {str(filename:join(Root, Path)), Offs};
upper(#state{root=Root}, {Path, Offs}) ->
    {str(filename:join(Root, Path)), Offs}.

last(Root, Depth) ->
    case path:last(Root) of
        undefined ->
            filename:join(Root, int_to_path(0, Depth));
        Path ->
            Path
    end.

file(Path) ->
    case file:open(Path, [read, append, raw, binary]) of
        {ok, File} ->
            {ok, File};
        {error, enoent} ->
            case filelib:ensure_dir(Path) of
                ok ->
                    file(Path);
                {error, Error} ->
                    {error, {mkdir, Error}}
            end;
        {error, Reason} ->
            {error, {open, Reason}}
    end.

header(File) ->
    case file:pread(File, 0, ?OZero) of
        {ok, <<?Magic, C0:128, $;, C1:128, $\n>>} ->
            {C0, C1};
        {error, Reason} ->
            {error, {pread, Reason}};
        _ ->
            case file:pwrite(File, 0, <<?Magic, ?OZero:128, $;, ?OZero:128, $\n>>) of
                ok ->
                    {?OZero, ?OZero};
                {error, Reason} ->
                    {error, {pwrite, Reason}}
            end
    end.

entry(File, Offs) ->
    case file:read(File, 4) of
        {ok, <<Size:32>>} ->
            case file:read(File, Size + 1) of
                {ok, <<Data:Size/binary, "\n">>} ->
                    {Data, Offs + Size + 5};
                {error, Reason} ->
                    {error, {read, Reason}};
                _ ->
                    {error, badentry}
            end;
        {error, Reason} ->
            {error, {read, Reason}};
        _ ->
            {error, badentry}
    end.

foldentries(Path, Fun, Acc, Range) ->
    case file:open(Path, [read, raw, binary]) of
        {ok, File} ->
            case file:position(File, ?OZero) of
                {ok, Offs} ->
                    foldentries(File, {Path, Offs}, Fun, Acc, Range);
                {error, Reason} ->
                    {stop, {error, {position, Reason}}}
            end;
        Error ->
            {stop, Error}
    end.

foldentries(_, Id, _, Acc, {_, Upper}) when Id >= Upper ->
    {stop, Acc};
foldentries(File, {Path, Offs} = Id, Fun, Acc, {Lower, _} = Range) ->
    case entry(File, Offs) of
        {error, badentry} ->
            Acc;
        {error, _} = Error ->
            {stop, Error};
        {_, Next} when Id < Lower ->
            foldentries(File, {Path, Next}, Fun, Acc, Range);
        {Data, Next} ->
            foldentries(File, {Path, Next}, Fun, Fun({Id, Data}, Acc), Range)
    end.

%% internal

do(flush, #state{file=File, offs=Offs, ckpt=Ckpt} = State) ->
    CPos = case Ckpt of 0 -> 8; 1 -> 25 end,
    case file:pwrite(File, CPos, <<Offs:128>>) of
        ok ->
            case file:position(File, Offs) of
                {ok, Offs} ->
                    {ok, State#state{ckpt=(Ckpt + 1) rem 2}};
                {error, Reason} ->
                    {{error, {position, Reason}}, State}
            end;
        {error, Reason} ->
            {{error, {pwrite, Reason}}, State}
    end;


do({repair, Checkpoint}, #state{file=File} = State) ->
    case file:position(File, Checkpoint) of
        {ok, Checkpoint} ->
            do(repair, State#state{offs=Checkpoint});
        {error, Reason} ->
            {{error, {position, Reason}}, State}
    end;

do(repair, #state{file=File, offs=Offs} = State) ->
    case entry(File, Offs) of
        {Data, Next} when is_binary(Data) ->
            do(repair, State#state{offs=Next});
        {error, badentry} ->
            case file:position(File, Offs) of
                {ok, Offs} ->
                    case file:truncate(File) of
                        ok ->
                            do(flush, State);
                        {error, Reason} ->
                            {{error, {truncate, Reason}}, State}
                    end;
                {error, Reason} ->
                    {{error, {position, Reason}}, State}
            end;
        Error ->
            {Error, State}
    end;

do(verify, #state{file=File} = State) ->
    case header(File) of
        {error, Reason} ->
            {{error, Reason}, State};
        {C0, C1} ->
            case file:pread(File, max(C0, C1) - 1, 2) of
                {error, Reason} ->
                    {{error, {pread, Reason}}, State};
                {ok, <<"\n">>} when C0 < C1 ->
                    {ok, State#state{offs=C1, ckpt=0}};
                {ok, <<"\n">>} ->
                    {ok, State#state{offs=C0, ckpt=1}};
                _ when C0 < C1 ->
                    do({repair, C0}, State#state{ckpt=0});
                _ ->
                    do({repair, C1}, State#state{ckpt=1})
            end
    end;

do({write, Entry}, #state{root=R, path=P, offs=O, depth=D, limit=L} = State) when O >= L ->
    Path = int_to_path(path_to_int(P, D) + 1, D),
    case file(filename:join(R, Path)) of
        {ok, File} ->
            case do(verify, State#state{path=Path, file=File}) of
                {ok, S} ->
                    do({write, Entry}, S);
                Error ->
                    {Error, State}
            end;
        Error ->
            {Error, State}
    end;
do({write, Entry}, #state{file=File, path=Path, offs=Offs} = State) ->
    Size = size(Entry),
    case file:write(File, <<Size:32, Entry/binary, "\n">>) of
        ok ->
            {{Path, Offs}, State#state{offs=Offs + Size + 5}};
        Error ->
            {Error, State}
    end.

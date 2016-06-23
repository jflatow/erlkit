-module(log).
-author("Jared Flatow").

-export_type([log/0,
              ref/0,
              mark/0,
              range/0,
              event/0,
              entry/0]).

-opaque log() :: pid().
-opaque ref() :: {binary(), integer()}.
-type mark() :: ref() | undefined.
-type range() :: {mark(), mark()}.
-type event() :: {ok | nil, binary()} | binary().
-type entry() :: {range(), event()}.

-export([open/1,
         open/2,
         close/1,
         purge/1,
         flush/1,
         locus/1,
         zilch/1,
         write/2,
         write/3,
         annul/2,
         bendl/3,
         bendl/4,
         bendl/5,
         bendr/3,
         bendr/4,
         bendr/5,
         foldl/3,
         foldl/4,
         foldl/5,
         foldr/3,
         foldr/4,
         foldr/5,
         foldx/5,
         limit/5,
         fetch/2,
         first/1,
         first/2,
         range/2,
         range/3,
         since/2,
         since/3,
         until/2,
         until/3,
         head/1,
         head/2,
         tail/1,
         tail/2]).

-export([int_to_path/2,
         int_to_path/3,
         path_to_int/2,
         path_to_int/3,
         mark_to_json/1,
         json_to_mark/1]).

-behavior(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(Magic, "logfile:").
-define(OZero, 42).
-record(state, {root, path, file, offs, ckpt, depth, chunk}).

%% external

open(Root) ->
    open(Root, []).

open(Root, Opts) ->
    gen_server:start_link(?MODULE, [str(Root), Opts], []).

close(Log) ->
    gen_server:call(Log, {do, close}).

purge(Log) ->
    gen_server:call(Log, {do, purge}).

flush(Log) ->
    gen_server:call(Log, {do, flush}).

locus(Log) ->
    case gen_server:call(Log, state) of
        #state{path=Path, offs=Offs} ->
            {Path, Offs}
    end.

zilch(Log) ->
    case gen_server:call(Log, state) of
        #state{path=Path} ->
            {Path, ?OZero}
    end.

write(Log, Event) ->
    write(Log, Event, call).

write(Log, Event, call) ->
    gen_server:call(Log, {do, {write, Event}});
write(Log, Event, cast) ->
    gen_server:cast(Log, {do, {write, Event}}).

annul(Log, {Rel, Offs}) ->
    case gen_server:call(Log, state) of
        #state{root=Root} ->
            case file:open(filename:join(Root, Rel), [read, write, raw, binary]) of
                {ok, File} ->
                    Result = strike(File, Offs),
                    ok = file:close(File),
                    Result;
                {error, Reason} ->
                    {error, {open, Reason}}
            end
    end.

bendl(Log, Fun, Acc) ->
    bendl(Log, Fun, Acc, {undefined, undefined}).

bendl(Log, Fun, Acc, Range) ->
    bendl(Log, Fun, Acc, Range, []).

bendl(Log, Fun, Acc, {I1, _} = Range, Opts) ->
    bendx(Log, Fun, Acc, Range, Opts, {I1, I1}, fun foldl/5).

bendr(Log, Fun, Acc) ->
    bendr(Log, Fun, Acc, {undefined, undefined}).

bendr(Log, Fun, Acc, Range) ->
    bendr(Log, Fun, Acc, Range, []).

bendr(Log, Fun, Acc, {_, I2} = Range, Opts) ->
    bendx(Log, Fun, Acc, Range, Opts, {I2, I2}, fun foldr/5).

bendx(Log, Fun, Acc, Range, Opts, Initial, Fold) ->
    Fold(Log,
         fun ({_, _}, {M, {stop, A}}) ->
                 {stop, {M, A}};
             ({M, _} = E, {_, A}) ->
                 {M, Fun(E, A)}
         end, {Initial, Acc}, Range, Opts).

foldl(Log, Fun, Acc) ->
    foldl(Log, Fun, Acc, {undefined, undefined}).

foldl(Log, Fun, Acc, Range) ->
    foldl(Log, Fun, Acc, Range, []).

foldl(Log, Fun, Acc, Range, Opts) ->
    foldx(Log, Fun, Acc, Range, util:set(Opts, reverse, false)).

foldr(Log, Fun, Acc) ->
    foldr(Log, Fun, Acc, {undefined, undefined}).

foldr(Log, Fun, Acc, Range) ->
    foldr(Log, Fun, Acc, Range, []).

foldr(Log, Fun, Acc, Range, Opts) ->
    foldx(Log, Fun, Acc, Range, util:set(Opts, reverse, true)).

foldx(Log, Fun, Acc, {I1, I2}, Opts) ->
    case gen_server:call(Log, state) of
        #state{root=Root, path=Path, offs=Offs} = S ->
            {Outer, Inner} =
                case util:get(Opts, reverse) of
                    true ->
                        {fun path:foldr/4, fun foldpathr/5};
                    _ ->
                        {fun path:foldl/4, fun foldpathl/5}
                end,
            Lower = {PLo, _} = lower(S, I1),
            Upper = {PHi, _} = upper(S, I2),
            case Lower >= {Path, Offs} of
                true ->
                    Acc;
                false ->
                    Rash = util:get(Opts, rash),
                    PRange = {filename:join(Root, PLo), filename:join([Root, PHi, "~"])},
                    Outer(Root,
                          fun (P, A) ->
                                  Inner({P, rel(Root, P)}, Fun, A, {Lower, Upper}, Rash)
                          end, Acc, PRange)
            end
    end.

limit(Log, Fun, Acc, Range, Opts) ->
    case util:get(Opts, limit) of
        undefined ->
            foldx(Log, Fun, Acc, Range, Opts);
        Limit ->
            element(2, foldx(Log,
                             fun (E, {N, A}) when N < Limit ->
                                     {N + 1, Fun(E, A)};
                                 (_, {N, A}) ->
                                     {stop, {N, A}}
                             end, {0, Acc}, Range, Opts))
    end.

fetch(Log, {Rel, Offs}) ->
    hd(range(Log, {{Rel, Offs}, {Rel, Offs + 1}})).

first(Log) ->
    first(Log, {undefined, undefined}).

first(Log, Range) ->
    util:head(range(Log, Range, #{limit => 1})).

range(Log, Range) ->
    range(Log, Range, []).

range(Log, Range, Opts) ->
    lists:reverse(limit(Log, fun util:cons/2, [], Range, Opts)).

since(Log, Id) ->
    since(Log, Id, []).

since(Log, Id, Opts) ->
    range(Log, {Id, undefined}, Opts).

until(Log, Id) ->
    until(Log, Id, []).

until(Log, Id, Opts) ->
    range(Log, {undefined, Id}, Opts).

head(Log) ->
    head(Log, []).

head(Log, Opts) ->
    range(Log, util:get(Opts, range, {undefined, undefined}), Opts).

tail(Log) ->
    tail(Log, []).

tail(Log, Opts) ->
    range(Log, util:get(Opts, range, {undefined, undefined}), util:set(Opts, reverse, true)).

%% gen_server

init([Root, Opts]) ->
    Depth = util:get(Opts, depth, 2),
    Chunk = util:get(Opts, chunk, 64 bsl 20),
    Last = last(Root, Depth),
    Path = rel(Root, Last),
    case file(Last) of
        {ok, File} ->
            case do(verify, #state{root=Root,
                                   path=Path,
                                   file=File,
                                   depth=Depth,
                                   chunk=Chunk}) of
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
handle_call({do, close}, _From, #state{file=File, root=Root} = State) ->
    case do(flush, State) of
        {ok, S} ->
            case file:close(File) of
                ok ->
                    {stop, normal, {ok, Root}, S};
                Error ->
                    {reply, Error, S}
            end;
        Error ->
            {reply, Error, State}
    end;
handle_call({do, purge}, _From, #state{file=File, root=Root} = State) ->
    _ = file:close(File),
    _ = path:rmrf(Root),
    {stop, normal, {ok, Root}, State};
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

mark_to_json(undefined) ->
    null;
mark_to_json({Rel, Pos}) ->
    [Rel, Pos].

json_to_mark(null) ->
    undefined;
json_to_mark([Rel, Pos]) ->
    {Rel, Pos}.

rel(Root, Path) ->
    str:strip(str:disfix(str(Root), str(Path)), $/).

str(Bin) when is_binary(Bin) ->
    Bin;
str(List) when is_list(List) ->
    list_to_binary(List).

lower(#state{root=Root}, undefined) ->
    {rel(Root, path:head(Root)), ?OZero};
lower(_, {Path, Offs}) ->
    {str(Path), Offs}.

upper(#state{path=Path, offs=Offs}, undefined) ->
    {str(Path), Offs};
upper(_, {Path, Offs}) ->
    {str(Path), Offs}.

last(Root, Depth) ->
    case path:last(Root) of
        undefined ->
            filename:join(Root, int_to_path(0, Depth));
        Path ->
            Path
    end.

file(Path) ->
    case file:open(Path, [read, write, raw, binary]) of
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

entry(File, Offs) when is_binary(File) ->
    case File of
        <<Size:32, Data:Size/binary, $\n, Rest/binary>> ->
            {Rest, {ok, Data}, Offs + Size + 5};
        <<Size:32, Data:Size/binary, $\^x, Rest/binary>> ->
            {Rest, {nil, Data}, Offs + Size + 5};
        _ ->
            {error, badentry}
    end;
entry(File, Offs) ->
    case file:read(File, 4) of
        {ok, <<Size:32>>} ->
            case file:read(File, Size + 1) of
                {ok, <<Data:Size/binary, $\n>>} ->
                    {File, {ok, Data}, Offs + Size + 5};
                {ok, <<Data:Size/binary, $\^x>>} ->
                    {File, {nil, Data}, Offs + Size + 5};
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

strike(File, Offs) ->
    case file:pread(File, Offs, 4) of
        {ok, <<Size:32>>} ->
            case file:pread(File, Offs + 4, Size + 1) of
                {ok, <<Data:Size/binary, $\n>>} ->
                    case file:pwrite(File, Offs + Size + 4, <<$\^x>>) of
                        ok ->
                            {ok, Data};
                        {error, Reason} ->
                            {error, {write, Reason}}
                    end;
                {ok, <<Data:Size/binary, $\^x>>} ->
                    {ok, Data};
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

foldpathl({Abs, Rel}, Fun, Acc, Range, Start) when is_integer(Start) ->
    case file:read_file(Abs) of
        {ok, <<_:Start/binary, Data/binary>>} ->
            foldentries(Data, {Rel, Start}, Fun, Acc, Range);
        {ok, _} ->
            {stop, {error, {position, Start}}};
        Error ->
            {stop, Error}
    end;
foldpathl({Abs, Rel}, Fun, Acc, {{Rel, OLo}, _} = Range, true) ->
    foldpathl({Abs, Rel}, Fun, Acc, Range, OLo);
foldpathl({Abs, Rel}, Fun, Acc, Range, _) ->
    foldpathl({Abs, Rel}, Fun, Acc, Range, ?OZero).

foldpathr(Paths, Fun, Acc, Range, Rash) ->
    case foldpathl(Paths, fun util:cons/2, [], Range, Rash) of
        {stop, Reversed} when is_list(Reversed) ->
            {stop, util:roll(Fun, Acc, Reversed)};
        Reversed when is_list(Reversed) ->
            util:roll(Fun, Acc, Reversed);
        Other ->
            Other
    end.

foldentries(_, _, _, {stop, Acc}, _) ->
    {stop, Acc};
foldentries(_, Id, _, Acc, {_, Upper}) when Id >= Upper ->
    {stop, Acc};
foldentries(File, {Rel, Offs} = Id, Fun, Acc, {Lower, _} = Range) ->
    case entry(File, Offs) of
        {error, badentry} ->
            Acc;
        {error, _} = Error ->
            {stop, Error};
        {Rest, _, Next} when Id < Lower ->
            foldentries(Rest, {Rel, Next}, Fun, Acc, Range);
        {Rest, Data, Next} ->
            foldentries(Rest, {Rel, Next}, Fun, Fun({{Id, {Rel, Next}}, Data}, Acc), Range)
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
        {_File, _Data, Next} ->
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
                {ok, <<B>>} when (B =:= $\n orelse B =:= $\^x), C0 < C1 ->
                    {ok, State#state{offs=C1, ckpt=0}};
                {ok, <<B>>} when (B =:= $\n orelse B =:= $\^x) ->
                    {ok, State#state{offs=C0, ckpt=1}};
                _ when C0 < C1 ->
                    do({repair, C0}, State#state{ckpt=0});
                _ ->
                    do({repair, C1}, State#state{ckpt=1})
            end
    end;

do({write, Event}, #state{root=R, file=F, path=P, offs=O, depth=D, chunk=C} = State) when O >= C ->
    Path = str(int_to_path(path_to_int(P, D) + 1, D)),
    case file:close(F) of
        ok ->
            case file(filename:join(R, Path)) of
                {ok, File} ->
                    case do(verify, State#state{path=Path, file=File}) of
                        {ok, S} ->
                            do({write, Event}, S);
                        Error ->
                            {Error, State}
                    end;
                Error ->
                    {Error, State}
            end;
        {error, Reason} ->
            {error, {close, Reason}, State}
    end;
do({write, Event}, #state{file=File, path=Path, offs=Offs} = State) ->
    {Data, EOD} = case Event of
                      <<D/binary>> ->
                          {D, $\n};
                      {ok, D} ->
                          {D, $\n};
                      {nil, D} ->
                          {D, $\^x}
                  end,
    Size = size(Data),
    Next = Offs + Size + 5,
    case file:pwrite(File, Offs, <<Size:32, Data/binary, EOD>>) of
        ok ->
            {{ok, {{Path, Offs}, {Path, Next}}}, State#state{offs=Next}};
        Error ->
            {Error, State}
    end.

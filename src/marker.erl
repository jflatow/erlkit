-module(marker).
-author("Jared Flatow").

-export([io/1,
         new/2,
         run/1]).

-type(error() :: {error, any()}).
-type(mark()  :: {mark, any()}).
-type(narg()  :: {mark(), any()} | mark()).
-type(readf() :: fun (() -> mark() | error())).
-type(savef() :: fun ((mark()) -> ok | error())).
-type(nextf() :: fun ((narg()) -> narg() | error())).

-record(marker, {mark :: mark() | undefined,
                 read :: readf(),
                 save :: savef(),
                 next :: nextf()}).

-type(state() :: #marker{} | {#marker{}, any()}).

-spec(io(any()) -> {readf(), savef()}).
io({Read, Save}) when is_function(Read), is_function(Save) ->
    {Read, Save};
io(Filename) ->
    {fun () ->
             case file:read_file(Filename) of
                 {ok, Data} ->
                     {mark, binary_to_term(Data)};
                 {error, enoent} ->
                     {mark, undefined};
                 Error ->
                     Error
             end
     end,
     fun ({mark, Mark}) ->
             case path:write(Filename, term_to_binary(Mark)) of
                 ok ->
                     ok;
                 Error ->
                     Error
             end
     end}.

-spec(new(nextf(), {readf(), savef()}) -> #marker{}).
new(Next, {Read, Save}) ->
    #marker{read=Read, save=Save, next=Next}.

-spec(run({state()}) -> state() | error()).
run(#marker{} = Marker) ->
    element(1, run({Marker, nil}));
run({#marker{mark=undefined, read=Read} = Marker, Data}) ->
    case Read() of
        {mark, _} = Mark ->
            run({Marker#marker{mark=Mark}, Data});
        {error, ERead} ->
            {error, {read, ERead}}
    end;
run({#marker{save=Save} = Marker, Data}) ->
    case next(Marker, Data) of
        {{mark, _} = Mark, D} ->
            case Save(Mark) of
                ok ->
                    {Marker#marker{mark=Mark}, D};
                {error, ESave} ->
                    {error, {save, ESave}}
            end;
        {error, ENext} ->
            {error, {next, ENext}}
    end.

next(#marker{mark=Mark, next=Next}, nil) ->
    {Next(Mark), nil};
next(#marker{mark=Mark, next=Next}, Data) ->
    Next({Mark, Data}).

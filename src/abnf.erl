-module(abnf).
-author("Jared Flatow").

-export([maybe/3,
         maybe/4,
         repeat/3,
         repeat/4,
         repeat/5,
         either/3,
         either/4,
         either/5]).

maybe(Read, Symbol, Stream) ->
    maybe(Read, Symbol, Stream, undefined).

maybe(Read, Symbol, Stream, Default) ->
    try Read(Symbol, Stream)
    catch
        _:_ ->
            {Default, Stream}
    end.

repeat(Read, Symbol, Stream) ->
    repeat(Read, Symbol, Stream, {0, infinity}).

repeat(Read, Symbol, Stream, Bounds) ->
    repeat(Read, Symbol, Stream, Bounds, {0, []}, die).

repeat(Read, Symbol, Stream, Bounds, Default) ->
    repeat(Read, Symbol, Stream, Bounds, {0, []}, {default, Default}).

repeat(_Read, _Symbol, Stream, {_Min, Max}, {N, Acc}, _OrElse) when N >= Max ->
    {lists:reverse(Acc), Stream};
repeat(Read, Symbol, Stream, {Min, Max}, {N, Acc}, OrElse) ->
    try Read(Symbol, Stream) of
        {Repr, Rest} ->
            repeat(Read, Symbol, Rest, {Min, Max}, {N + 1, [Repr|Acc]}, OrElse)
    catch
        _:_ when N >= Min ->
            {lists:reverse(Acc), Stream};
        _:_  ->
            fail(OrElse, {not_enough, Symbol})
    end.

either(Read, Symbols, Stream) ->
    either(Read, Symbols, Stream, false, Symbols).

either(Read, Symbols, Stream, Tagged) ->
    either(Read, Symbols, Stream, Tagged, Symbols, die).

either(Read, Symbols, Stream, Tagged, Default) ->
    either(Read, Symbols, Stream, Tagged, Symbols, {default, Default}).

either(Read, Symbols, Stream, Tagged, [Symbol|Alternatives], OrElse) ->
    try Read(Symbol, Stream) of
        {Repr, Rest} when Tagged ->
            {{Symbol, Repr}, Rest};
        {Repr, Rest} ->
            {Repr, Rest}
    catch
        _:_ ->
            either(Read, Symbols, Stream, Tagged, Alternatives, OrElse)
    end;
either(_Read, Symbols, _Stream, _Tagged, [], OrElse) ->
    fail(OrElse, {neither, Symbols}).

fail({default, Default}, _Error) ->
    Default;
fail(die, Error) ->
    throw(Error).

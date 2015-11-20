-module(perf).
-author("Jared Flatow").

-export([do/1,
         repeat/2,
         meantime/1,
         meantime/2,
         timeit/1,
         timeit/2,
         traceit/1,
         traceit/2]).

-define(N, 1000).

do({F, A}) ->
    apply(F, A);
do({M, F, A}) ->
    apply(M, F, A).

repeat(D, N) ->
    repeat(D, N, undefined).

repeat(_, 0, Last) ->
    Last;
repeat(D, N, _) ->
    repeat(D, N - 1, do(D)).

meantime(D) ->
    meantime(D, ?N).

meantime(D, N) ->
    {T, R} = timeit(D, N),
    {T / N, R}.

timeit(D) ->
    timeit(D, ?N).

timeit(D, N) ->
    timer:tc(fun repeat/2, [D, N]).

traceit(D) ->
    traceit(D, ?N div 10).

traceit(D, N) ->
    fprof:apply(fun repeat/2, [D, N]).

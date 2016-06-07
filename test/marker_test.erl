-module(marker_test).

-include_lib("eunit/include/eunit.hrl").

marker_test() ->
    {ok, L} = log:open("marker_test"),
    M0 = log:marker(L, fun (I, A) -> [I|A] end, "marker"),
    {ok, _} = util:count(fun (I, _) -> log:write(L, util:bin(I)) end, {ok, undefined}, 1000),
    {M1, D1} = marker:run({M0, []}),
    ?assertMatch(1000, length(D1)),

    {ok, _} = util:count(fun (I, _) -> log:write(L, util:bin(I)) end, {ok, undefined}, 1000),
    {_M, D2} = marker:run({M1, D1}),
    ?assertMatch(2000, length(D2)),

    ok = log:close(L),
    ok = path:rmrf("marker"),
    ok = path:rmrf("marker_test").

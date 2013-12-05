-module(marker_test).

-include_lib("eunit/include/eunit.hrl").

marker_test() ->
    Push = fun (I, A) -> [I|A] end,
    ok = path:rmrf("marker"),
    ok = path:rmrf("marker_test"),
    {ok, L} = log:open("marker_test"),
    M0 = marker:new(fun ({{mark, Mark}, Data}) ->
                            {{_, Next}, D} = log:bendl(L, Push, Data, {Mark, undefined}),
                            {{mark, Next}, D}
                    end, marker:io("marker")),

    ok = util:count(fun (I, ok) -> log:write(L, util:bin(I)) end, ok, 1000),
    {M1, D1} = marker:run({M0, []}),
    ?assertMatch(1000, length(D1)),

    ok = util:count(fun (I, ok) -> log:write(L, util:bin(I)) end, ok, 1000),
    {_M, D2} = marker:run({M1, D1}),
    ?assertMatch(2000, length(D2)),

    ok = log:close(L).

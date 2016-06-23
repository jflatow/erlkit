-module(log_test).

-include_lib("eunit/include/eunit.hrl").

log_test() ->
    {ok, Log} = log:open("log_test"),
    {ok, {A, _}} = util:count(fun (I, _) -> log:write(Log, util:bin(I)) end, {ok, undefined}, 1000),
    ?assertMatch(1000, length(log:since(Log, undefined))),
    {ok, {_, _}} = util:count(fun (I, _) -> log:write(Log, util:bin(I)) end, {ok, undefined}, 1000),
    ?assertMatch(2000, length(log:since(Log, undefined))),
    {ok, {_, B}} = util:count(fun (I, _) -> log:write(Log, util:bin(I)) end, {ok, undefined}, 100),
    ?assertMatch(2100, length(log:range(Log, {undefined, B}))),
    ?assertMatch({_, {ok, <<"0">>}}, log:first(Log)),
    ?assertMatch({_, {ok, <<"999">>}}, log:fetch(Log, A)),
    {ok, _} = log:annul(Log, A),
    ?assertMatch({_, {nil, <<"999">>}}, log:fetch(Log, A)),
    ?assertMatch([{_, {ok, <<"0">>}},
                  {_, {ok, <<"1">>}}], log:range(Log, {undefined, A}, #{limit => 2})),
    ?assertMatch([{_, {ok, <<"998">>}},
                  {_, {ok, <<"997">>}}], log:range(Log, {undefined, A}, #{limit => 2, reverse => true})),
    ?assertEqual(length(log:range(Log, {A, B})), length(log:range(Log, {A, B}, #{reverse => true}))),
    {ok, _} = log:purge(Log).

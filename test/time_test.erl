-module(time_test).

-include_lib("eunit/include/eunit.hrl").

datetime_test() ->
    TStamp = "2010-08-17",
    Time = {{2013, 10, 20}, {0, 0, 0}},
    ?assertEqual(Time, time:datetime({unix, time:unix(Time)})),
    ?assertMatch({{2010, 8, 17}, {0, 0, 0}}, time:parse(TStamp, rfc3339)),
    ?assertMatch({{2014, 1, 20}, {0, 0, 0}}, time:pass(Time, {3, months})).

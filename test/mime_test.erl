-module(mime_test).

-include_lib("eunit/include/eunit.hrl").

datetime_test() ->
    ?assertMatch(<<"04 Dec 2013 22:55:00 -0800">>, mime:format(datetime, mime:parse(datetime, <<"04 Dec 2013 22:55:00 -0800">>))),
    ?assertMatch({{2013, 01, 01}, {10, 20, 0}}, time:parse(<<"1 Jan 2013 10:20:00 +0000">>, rfc2822)),
    ?assertMatch({{2013, 12, 05}, {06, 55, 0}}, time:parse(<<"Wed, 4 Dec 2013 22:55:00 -0800">>, rfc2822)).

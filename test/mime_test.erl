-module(mime_test).

-include_lib("eunit/include/eunit.hrl").

datetime_test() ->
    ?assertMatch(<<"04 Dec 2013 22:55:00 -0800">>, mime:format(datetime, mime:parse(datetime, <<"04 Dec 2013 22:55:00 -0800">>))),
    ?assertMatch({{2013, 01, 01}, {10, 20, 0}}, time:parse(<<"1 Jan 2013 10:20:00 +0000">>, rfc2822)),
    ?assertMatch({{2013, 12, 05}, {06, 55, 0}}, time:parse(<<"Wed, 4 Dec 2013 22:55:00 -0800">>, rfc2822)).

address_test() ->
    ?assertMatch({mailbox, {[<<"Jared">>, <<"Flatow">>], {<<"jflatow">>, <<"example.com">>}}}, mime:parse(address, <<"Jared Flatow <jflatow@example.com>">>)),
    ?assertMatch({mailbox, {undefined, {<<"jflatow">>, <<"example.com">>}}}, mime:parse(address, <<"<jflatow@example.com>">>)),
    ?assertMatch({mailbox, {undefined, {<<"jflatow">>, <<"example.com">>}}}, mime:parse(address, <<"jflatow@example.com">>)),
    MBox = {[<<"Jared">>, <<"Flatow">>], {<<"jflatow@example.com">>, <<"example.com">>}},
    ?assertMatch(MBox, mime:parse(mailbox, mime:format(mailbox, MBox))),
    ?assertMatch([MBox, MBox], mime:parse({list, mailbox}, mime:format({list, mailbox}, [MBox, MBox]))),
    ?assertMatch(<<"Jared Flatow <jflatow@example.com>">>, mime:format(mailbox, {<<"Jared Flatow">>, <<"jflatow@example.com">>})),
    ?assertMatch(<<"jflatow@example.com">>, mime:format(mailbox, {undefined, <<"jflatow@example.com">>})).

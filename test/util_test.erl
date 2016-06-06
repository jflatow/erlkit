-module(util_test).

-import(util, [atom/1,
               atom/2,
               list/1,
               bin/1,
               flt/1,
               int/1,
               num/1,
               has/2,
               hasall/2,
               hasany/2,
               modify/3,
               remove/2]).

-include_lib("eunit/include/eunit.hrl").

atom_test() ->
    ?assertMatch(hello, atom(hello)),
    ?assertMatch(hello, atom("hello")),
    ?assertMatch(hello, atom(<<"hello">>)),
    ?assertMatch(hello, atom(<<"hello">>, true)),
    ?assertMatch('123', atom(123)),
    ?assertMatch(12.34, flt(atom(12.34))).

list_test() ->
    ?assertMatch("list", list(list)),
    ?assertMatch("list", list("list")),
    ?assertMatch("list", list(<<"list">>)),
    ?assertMatch("1234", list(1234)),
    ?assertMatch(12.345, flt(list(12.345))).

bin_test() ->
    ?assertMatch(<<"bin">>, bin(bin)),
    ?assertMatch(<<"bin">>, bin("bin")),
    ?assertMatch(<<"bin">>, bin(<<"bin">>)),
    ?assertMatch(<<"123">>, bin(123)),
    ?assertMatch(7654.3210, flt(bin(7654.3210))).

flt_test() ->
    ?assertMatch(1.2, flt('1.2')),
    ?assertMatch(1.2, flt("1.2")),
    ?assertMatch(1.2, flt(<<"1.2">>)),
    ?assertMatch(1.2, flt(1.2)).

int_test() ->
    ?assertMatch(123, int('123')),
    ?assertMatch(123, int("123")),
    ?assertMatch(123, int(<<"123">>)),
    ?assertMatch(123, int(123)).

num_test() ->
    ?assertMatch(1.2, num('1.2')),
    ?assertMatch(1.2, num("1.2")),
    ?assertMatch(1.2, num(<<"1.2">>)),
    ?assertMatch(1.2, num(1.2)),
    ?assertMatch(123, num('123')),
    ?assertMatch(123, num("123")),
    ?assertMatch(123, num(<<"123">>)),
    ?assertMatch(123, num(123)).

has_test() ->
    ?assert(has(#{x => 1}, x)),
    ?assert(has([{x, 1}], x)),
    ?assertNot(has(#{x => 1}, y)),
    ?assertNot(has([{x, 1}], y)),
    ?assert(hasall(#{x => 1, y => 2}, [x, y])),
    ?assert(hasall([{x, 1}, {y, 2}], [x, y])),
    ?assertNot(hasall(#{x => 1}, [x, y])),
    ?assertNot(hasall([{x, 1}], [x, y])),
    ?assert(hasany(#{x => 1}, [x, y])),
    ?assert(hasany([{x, 1}], [x, y])),
    ?assert(hasall([], [])),
    ?assertNot(hasany([], [])),
    ?assertNot(hasany(#{x => 1}, [])),
    ?assert(hasall([{x, 1}], [])),
    ?assertNot(hasall(#{}, [x])),
    ?assertNot(hasany([], [x])).

modify_test() ->
    ?assertEqual(#{x => 1}, modify(#{}, [x], fun (_) -> 1 end)),
    ?assertEqual(#{x => 1}, modify(#{x => 3}, [x], fun (_) -> 1 end)).

remove_test() ->
    ?assertEqual(#{}, remove(#{x => 1}, [x])),
    ?assertEqual(#{x => []}, remove(#{x => [{2, v}]}, [x, 2])).

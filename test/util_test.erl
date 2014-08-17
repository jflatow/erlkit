-module(util_test).

-import(util, [atom/1,
               atom/2,
               list/1,
               bin/1,
               flt/1,
               int/1,
               num/1,
               startswith/2,
               endswith/2]).

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

startswith_test() ->
    ?assertMatch(true, startswith(<<"abc">>, <<>>)),
    ?assertMatch(true, startswith(<<"abc">>, [])),
    ?assertMatch(true, startswith(<<"abc">>, a)),
    ?assertMatch(true, startswith(<<"abc">>, $a)),
    ?assertMatch(true, startswith(<<"abc">>, <<"a">>)),
    ?assertMatch(true, startswith(<<"abc">>, "a")),
    ?assertMatch(true, startswith(<<"abc">>, <<"abc">>)),
    ?assertMatch(true, startswith(<<"abc">>, "abc")),
    ?assertMatch(true, startswith(<<"123">>, 1)),
    ?assertMatch(true, startswith("abc", <<>>)),
    ?assertMatch(true, startswith("abc", [])),
    ?assertMatch(true, startswith("abc", a)),
    ?assertMatch(true, startswith("abc", $a)),
    ?assertMatch(true, startswith("abc", <<"a">>)),
    ?assertMatch(true, startswith("abc", "a")),
    ?assertMatch(true, startswith("abc", <<"abc">>)),
    ?assertMatch(true, startswith("abc", "abc")),
    ?assertMatch(true, startswith("123", 1)),
    ?assertMatch(false, startswith(<<"abc">>, b)),
    ?assertMatch(false, startswith(<<"abc">>, $b)),
    ?assertMatch(false, startswith(<<"abc">>, <<"b">>)),
    ?assertMatch(false, startswith(<<"abc">>, "b")),
    ?assertMatch(false, startswith(<<"abc">>, "abcd")),
    ?assertMatch(false, startswith(<<"123">>, 1.0)),
    ?assertMatch(false, startswith("abc", b)),
    ?assertMatch(false, startswith("abc", $b)),
    ?assertMatch(false, startswith("abc", <<"b">>)),
    ?assertMatch(false, startswith("abc", "b")),
    ?assertMatch(false, startswith("abc", <<"abcd">>)),
    ?assertMatch(false, startswith("123", 1.0)).

endswith_test() ->
    ?assertMatch(true, endswith(<<"abc">>, <<>>)),
    ?assertMatch(true, endswith(<<"abc">>, [])),
    ?assertMatch(true, endswith(<<"abc">>, c)),
    ?assertMatch(true, endswith(<<"abc">>, $c)),
    ?assertMatch(true, endswith(<<"abc">>, <<"c">>)),
    ?assertMatch(true, endswith(<<"abc">>, "c")),
    ?assertMatch(true, endswith(<<"abc">>, <<"abc">>)),
    ?assertMatch(true, endswith(<<"abc">>, "abc")),
    ?assertMatch(true, endswith(<<"123">>, 3)),
    ?assertMatch(true, endswith("abc", <<>>)),
    ?assertMatch(true, endswith("abc", [])),
    ?assertMatch(true, endswith("abc", c)),
    ?assertMatch(true, endswith("abc", $c)),
    ?assertMatch(true, endswith("abc", <<"c">>)),
    ?assertMatch(true, endswith("abc", "c")),
    ?assertMatch(true, endswith("abc", <<"abc">>)),
    ?assertMatch(true, endswith("abc", "abc")),
    ?assertMatch(true, endswith("123", 3)),
    ?assertMatch(false, endswith(<<"abc">>, b)),
    ?assertMatch(false, endswith(<<"abc">>, $b)),
    ?assertMatch(false, endswith(<<"abc">>, <<"b">>)),
    ?assertMatch(false, endswith(<<"abc">>, "b")),
    ?assertMatch(false, endswith(<<"abc">>, "abcd")),
    ?assertMatch(false, endswith(<<"123">>, 3.0)),
    ?assertMatch(false, endswith("abc", b)),
    ?assertMatch(false, endswith("abc", $b)),
    ?assertMatch(false, endswith("abc", <<"b">>)),
    ?assertMatch(false, endswith("abc", "b")),
    ?assertMatch(false, endswith("abc", <<"abcd">>)),
    ?assertMatch(false, endswith("123", 3.0)).

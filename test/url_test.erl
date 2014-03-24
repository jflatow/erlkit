-module(url_test).

-include_lib("eunit/include/eunit.hrl").

b(undefined) -> undefined;
b(X) -> util:bin(X).

u(S, A, P) -> u(S, A, P, undefined).
u(S, A, P, Q) -> u(S, A, P, Q, undefined).
u(S, A, P, Q, F) ->
    #{scheme => b(S),
      authority => b(A),
      path => b(P),
      query => b(Q),
      fragment => b(F)}.

p(U) -> url:parse(U).

parse_test() ->
    ?assertEqual(u("foo", "example.com:8042", "/over/there", "name=ferret", "nose"),
                 p("foo://example.com:8042/over/there?name=ferret#nose")),
    ?assertEqual(u("urn", undefined, "example:animal:ferret:nose"),
                 p("urn:example:animal:ferret:nose")),
    ?assertEqual(u("ftp", "ftp.is.co.za", "/rfc/rfc1808.txt"),
                 p("ftp://ftp.is.co.za/rfc/rfc1808.txt")),
    ?assertEqual(u("http", "www.ietf.org", "/rfc/rfc2396.txt"),
                 p("http://www.ietf.org/rfc/rfc2396.txt")),
    ?assertEqual(u("ldap", "[2001:db8::7]", "/c=GB", "objectClass?one"),
                 p("ldap://[2001:db8::7]/c=GB?objectClass?one")),
    ?assertEqual(u("mailto", undefined, "John.Doe@example.com"),
                 p("mailto:John.Doe@example.com")),
    ?assertEqual(u("news",undefined, "comp.infosystems.www.servers.unix"),
                 p("news:comp.infosystems.www.servers.unix")),
    ?assertEqual(u("tel", undefined, "+1-816-555-1212"),
                 p("tel:+1-816-555-1212")),
    ?assertEqual(u("telnet", "192.0.2.16:80", "/"),
                 p("telnet://192.0.2.16:80/")),
    ?assertEqual(u("urn", undefined, "oasis:names:specification:docbook:dtd:xml:4.1.2"),
                 p("urn:oasis:names:specification:docbook:dtd:xml:4.1.2")).

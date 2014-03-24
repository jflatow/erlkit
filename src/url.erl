-module(url).

-export([empty/0,
         parse/1,
         parse/2]).

empty() ->
    #{scheme => undefined,
      authority => undefined,
      path => undefined,
      query => undefined,
      fragment => undefined}.

parse(Input) ->
    parse(Input, #{}).

parse(Input, URL) when is_list(Input) ->
    parse(list_to_binary(Input), URL);
parse(Input, URL) ->
    parse(scheme, Input, maps:merge(empty(), URL), <<>>).

parse(scheme, <<C, Rest/binary>>, URL, Buf) when C =/= $:, C =/= $/, C =/= $?, C =/= $# ->
    parse(scheme, Rest, URL, <<Buf/binary, C>>);
parse(scheme, <<$:, Rest/binary>>, URL, Buf) ->
    parse(authority, Rest, URL#{scheme := Buf}, <<>>);
parse(scheme, <<>>, URL, Buf) ->
    parse(authority, Buf, URL, <<>>);

parse(authority, <<"//", Rest/binary>>, URL = #{authority := undefined}, <<>>) ->
    parse(authority, Rest, URL#{authority := true}, <<>>);
parse(authority, Rest, URL = #{authority := undefined}, <<>>) ->
    parse(path, Rest, URL, <<>>);

parse(authority, <<C, Rest/binary>>, URL, Buf) when C =/= $/, C =/= $?, C =/= $# ->
    parse(authority, Rest, URL, <<Buf/binary, C>>);
parse(authority, Rest, URL, Buf) ->
    parse(path, Rest, URL#{authority := Buf}, <<>>);

parse(path, <<C, Rest/binary>>, URL, Buf) when C =/= $?, C =/= $# ->
    parse(path, Rest, URL, <<Buf/binary, C>>);
parse(path, Rest, URL, Buf) ->
    parse(query, Rest, URL#{path := Buf}, <<>>);

parse(query, <<$?, Rest/binary>>, URL = #{query := undefined}, <<>>) ->
    parse(query, Rest, URL#{query := true}, <<>>);
parse(query, Rest, URL = #{query := undefined}, <<>>) ->
    parse(fragment, Rest, URL, <<>>);

parse(query, <<C, Rest/binary>>, URL, Buf) when C =/= $# ->
    parse(query, Rest, URL, <<Buf/binary, C>>);
parse(query, Rest, URL, Buf) ->
    parse(fragment, Rest, URL#{query := Buf}, <<>>);

parse(fragment, <<$#, Rest/binary>>, URL = #{fragment := undefined}, <<>>) ->
    parse(fragment, Rest, URL#{fragment := true}, <<>>);
parse(fragment, <<>>, URL = #{fragment := undefined}, <<>>) ->
    URL;

parse(fragment, <<C, Rest/binary>>, URL, Buf) ->
    parse(fragment, Rest, URL, <<Buf/binary, C>>);
parse(fragment, <<>>, URL, Buf) ->
    URL#{fragment := Buf}.

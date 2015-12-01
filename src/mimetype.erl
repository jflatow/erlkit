-module(mimetype).

-export([decode/2]).

decode({"application/json", _}, Data) ->
    json:decode(Data);
decode({"application/jwt", _}, Data) ->
    jwt:decode(Data);
decode({"application/x-www-form-urlencoded", _}, Data) ->
    url:decode(Data);
decode({"text/calendar", _}, Data) ->
    ical:parse(Data);
decode({"text/plain", _}, Data) ->
    Data;
decode({"text/" ++ _, _}, Data) ->
    Data;
decode({Type, _Params}, _Data) ->
    throw({unrecognized_mimetype, Type}).

-module(time).

-export([ago/1,
         ago/2,
         unow/0,
         month/1,
         datetime/1,
         seconds/1,
         pass/2,
         parse/1,
         parse/2,
         stamp/1,
         stamp/2]).

-export([read_rfc3339/1,
         read_rfc3339/2]).

-import(util, [int/1]).

-define(Second, 1).
-define(Minute, 60 * ?Second).
-define(Hour, 60 * ?Minute).
-define(Day, 24 * ?Hour).

ago(Elapsed) ->
    ago(unow(), Elapsed).

ago(Time, Elapsed) ->
    datetime(seconds(Time) - seconds(Elapsed)).

unow() ->
    calendar:universal_time().

month(<<"Jan">>) -> 1;
month(<<"Feb">>) -> 2;
month(<<"Mar">>) -> 3;
month(<<"Apr">>) -> 4;
month(<<"May">>) -> 5;
month(<<"Jun">>) -> 6;
month(<<"Jul">>) -> 7;
month(<<"Aug">>) -> 8;
month(<<"Sep">>) -> 9;
month(<<"Oct">>) -> 10;
month(<<"Nov">>) -> 11;
month(<<"Dec">>) -> 12;
month(Str) when is_list(Str) ->
    month(list_to_binary(Str)).

datetime(Seconds) when is_integer(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds);
datetime(Seconds) when is_float(Seconds) ->
    pass(datetime(trunc(Seconds)), Seconds - trunc(Seconds));
datetime({D, T, O}) when is_tuple(D), is_tuple(T) ->
    pass({D, T}, O);
datetime({_, _, _} = Now) ->
    calendar:now_to_universal_time(Now);
datetime({_, _} = DateTime) ->
    DateTime.

seconds({N, days}) ->
    N * ?Day;
seconds({N, hours}) ->
    N * ?Hour;
seconds({N, minutes}) ->
    N * ?Minute;
seconds({N, seconds}) ->
    N * ?Second;
seconds(Seconds) when is_number(Seconds) ->
    Seconds;
seconds({D, T, O}) ->
    seconds(datetime({D, T, O}));
seconds({D, {H, M, S}}) when is_float(S) ->
    seconds({D, {H, M, trunc(S)}}) + (S - trunc(S));
seconds({_, _} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime).

pass(Time, Elapse) ->
    datetime(seconds(Time) + seconds(Elapse)).

parse(Timestamp) when is_list(Timestamp) ->
    parse(list_to_binary(Timestamp));
parse(<<Y:4/binary, "/", M:2/binary, "/", D:2/binary, _, H:2/binary, ":", Mi:2/binary, ":", S:2/binary, _/binary>>) ->
    {{int(Y), int(M), int(D)}, {int(H), int(Mi), int(S)}};
parse(<<Y:4/binary, "/", M:2/binary, "/", D:2/binary, _/binary>>) ->
    {{int(Y), int(M), int(D)}, {0, 0, 0}};
parse(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary, _, H:2/binary, ":", Mi:2/binary, ":", S:2/binary, _/binary>>) ->
    {{int(Y), int(M), int(D)}, {int(H), int(Mi), int(S)}};
parse(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary, _/binary>>) ->
    {{int(Y), int(M), int(D)}, {0, 0, 0}};
parse(_) ->
    undefined.

parse(Timestamp, rfc3339) when is_list(Timestamp) ->
    parse(list_to_binary(Timestamp), rfc3339);
parse(Timestamp, rfc3339) ->
    {Time, _} = read_rfc3339(Timestamp),
    datetime(Time).

read_digits(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    read_digits(Rest, <<Acc/binary, C>>);
read_digits(Rest, Acc) ->
    {Acc, Rest}.

read_rfc3339(Timestamp) ->
    {Date, R0} = read_rfc3339(date, Timestamp),
    {Time, R1} = read_rfc3339(time, R0),
    {Offs, R2} = read_rfc3339(offs, R1),
    {{Date, Time, calendar:time_to_seconds(Offs)}, R2}.

read_rfc3339(date, <<Y:4/binary, "-", M:2/binary, "-", D:2/binary, Rest/binary>>) ->
    {{int(Y), int(M), int(D)}, Rest};
read_rfc3339(time, <<"T", H:2/binary, ":", M:2/binary, ":", S:2/binary, ".", R0/binary>>) ->
    {Frac, R1} = read_digits(R0, <<>>),
    {{int(H), int(M), int(S) + int(Frac) / (math:pow(10, size(Frac)))}, R1};
read_rfc3339(time, <<"T", H:2/binary, ":", M:2/binary, ":", S:2/binary, Rest/binary>>) ->
    {{int(H), int(M), int(S)}, Rest};
read_rfc3339(offs, <<"Z+", H:2/binary, ":", M:2/binary, Rest/binary>>) ->
    {{-int(H), -int(M), 0}, Rest};
read_rfc3339(offs, <<"Z-", H:2/binary, ":", M:2/binary, Rest/binary>>) ->
    {{int(H), int(M), 0}, Rest};
read_rfc3339(_, Rest) ->
    {{0, 0, 0}, Rest}.

stamp({{Y, M, D}, {H, Mi, S}}) ->
    list_to_binary(io_lib:format("~4..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Mi, S]));
stamp(Time) ->
    stamp(datetime(Time)).

stamp({{Y, M, D}, {H, Mi, S}}, rfc3339) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ+00:00", [Y, M, D, H, Mi, S]));
stamp(Time, rfc3339) ->
    stamp(datetime(Time)).

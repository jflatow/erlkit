-module(time).
-author("Jared Flatow").

-export([ago/1,
         ago/2,
         days/2,
         diff/2,
         pass/1,
         pass/2,
         unix/0,
         unix/1,
         unow/0,
         tai64/0,
         tai64/1,
         datetime/1,
         seconds/1,
         valid/1,
         range/1,
         parse/1,
         parse/2,
         stamp/1,
         stamp/2,
         wkday/1,
         wknum/1,
         wknum/2]).

-export([read_rfc3339/1,
         read_rfc3339/2]).

-export([clock_distance/2]).
-export([timer/0,
         timer_elapsed/1,
         timer_remaining/2,
         timeout/1,
         timeout_remaining/2]).

-import(util, [int/1,
               mod/2]).

-define(Second, 1).
-define(Minute, 60 * ?Second).
-define(Hour, 60 * ?Minute).
-define(Day, 24 * ?Hour).
-define(UnixEpoch, 62167219200).

ago(Elapsed) ->
    ago(unow(), Elapsed).

ago(Time, {N, Unit}) when is_integer(N); is_atom(Unit) ->
    pass(Time, {-N, Unit});
ago(Time, Elapsed) ->
    datetime(seconds(Time) - seconds(Elapsed)).

days(T1, T2) ->
    diff(T1, T2) div (?Day).

diff(T1, T2) ->
    seconds(T2) - seconds(T1).

pass(Elapse) ->
    pass(unow(), Elapse).

pass({{Y, M, D}, {H, Mi, S}}, {N, years}) ->
    valid({{Y + N, M, D}, {H, Mi, S}});
pass({{Y, M, D}, {H, Mi, S}}, {N, months}) ->
    valid({{case M + N of
                L when L < 1 -> Y + (L div 12) - 1;
                L when L > 0 -> Y + (L - 1) div 12
            end,
            case mod(M + N, 12) of
                0 -> 12;
                O -> O
            end, D}, {H, Mi, S}});
pass(Time, []) ->
    Time;
pass(Time, [H|Tail]) ->
    pass(pass(Time, H), Tail);
pass(Time, Elapse) ->
    datetime(seconds(Time) + seconds(Elapse)).

unix() ->
    unix(unow()).

unix(Time) ->
    seconds(Time) - ?UnixEpoch.

unow() ->
    calendar:universal_time().

tai64() ->
    tai64(unow()).

tai64(Time) ->
    unix(Time) + (1 bsl 62). %% NB: time is TAI not UTC

datetime(Seconds) when is_integer(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds);
datetime(Seconds) when is_float(Seconds) ->
    datetime(trunc(Seconds));
datetime({unix, Seconds}) ->
    datetime(Seconds + ?UnixEpoch);
datetime({tai64, TAI64}) ->
    datetime({unix, TAI64 - (1 bsl 62)}); %% NB: returns TAI not UTC
datetime({{D, T}, O}) when is_list(O) ->
    pass({D, T}, O);
datetime({_, _, _} = Now) ->
    calendar:now_to_universal_time(Now);
datetime({_, _} = DateTime) ->
    DateTime.

seconds({N, weeks}) ->
    N * ?Day * 7;
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
seconds({{D, T}, O}) when is_list(O) ->
    seconds(pass({D, T}, O));
seconds({Y, M, D} = Date) when is_number(Y), is_number(M), is_number(D) ->
    seconds({Date, {0, 0, 0}});
seconds({D, {H, M, S}}) when is_float(S) ->
    seconds({D, {H, M, trunc(S)}}) + (S - trunc(S));
seconds({_, _} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime).

range({Start, Stop}) ->
    range({Start, Stop, {1, days}});
range({Start, {{_, _, _}, {_, _, _}} = Stop, _}) when Start >= Stop ->
    [];
range({Start, {{_, _, _}, {_, _, _}} = Stop, Step}) ->
    [Start|range({pass(Start, Step), Stop, Step})];
range({Start, Duration, Step})  ->
    range({Start, pass(Start, Duration), Step}).

valid({{Y, M, D}, _} = DateTime) ->
    case calendar:last_day_of_the_month(Y, M) of
        Last when D > Last ->
            time:pass({Y, M, Last}, {D - Last, days});
        _ when D < 1 ->
            time:pass({Y, M, 1}, {1 - D, days});
        _ ->
            DateTime
    end.

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

parse(Timestamp, rfc2822) ->
    datetime(mime:parse(datetime, util:bin(Timestamp)));

parse(Timestamp, rfc3339) ->
    {Time, _} = read_rfc3339(util:bin(Timestamp)),
    datetime(Time);

parse(<<TAI64:8/big-unsigned-unit:8>>, tai64) ->
    datetime({tai64, TAI64});
parse(Timestamp, tai64) ->
    parse(util:unhex(util:bin(Timestamp)), tai64).

read_digits(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    read_digits(Rest, <<Acc/binary, C>>);
read_digits(Rest, Acc) ->
    {Acc, Rest}.

read_rfc3339(Timestamp) ->
    {Date, R0} = read_rfc3339(date, Timestamp),
    {Time, R1} = read_rfc3339(time, R0),
    {Offs, R2} = read_rfc3339(offs, R1),
    {{{Date, Time}, Offs}, R2}.

read_rfc3339(date, <<Y:4/binary, "-", M:2/binary, "-", D:2/binary, Rest/binary>>) ->
    {{int(Y), int(M), int(D)}, Rest};
read_rfc3339(time, <<"T", H:2/binary, ":", M:2/binary, ":", S:2/binary, ".", R0/binary>>) ->
    {Frac, R1} = read_digits(R0, <<>>),
    {{int(H), int(M), int(S) + int(Frac) / (math:pow(10, size(Frac)))}, R1};
read_rfc3339(time, <<"T", H:2/binary, ":", M:2/binary, ":", S:2/binary, Rest/binary>>) ->
    {{int(H), int(M), int(S)}, Rest};
read_rfc3339(time, Rest) ->
    {{0, 0, 0}, Rest};
read_rfc3339(offs, <<"Z", Rest/binary>>) ->
    {[], Rest};
read_rfc3339(offs, <<"+", H:2/binary, ":", Mi:2/binary, Rest/binary>>) ->
    {[{-int(H), hours}, {-int(Mi), minutes}], Rest};
read_rfc3339(offs, <<"-", H:2/binary, ":", Mi:2/binary, Rest/binary>>) ->
    {[{int(H), hours}, {int(Mi), minutes}], Rest};
read_rfc3339(offs, Rest) ->
    {[{0, hours}, {0, minutes}], Rest}.

stamp({{Y, M, D}, {H, Mi, S}}) ->
    list_to_binary(io_lib:format("~4..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Mi, S]));
stamp(Time) ->
    stamp(datetime(Time)).

stamp(Time, rfc2822) ->
    mime:format(datetime, Time);

stamp({{Y, M, D}, {H, Mi, S}}, rfc3339) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, Mi, S]));
stamp(Time, rfc3339) ->
    stamp(datetime(Time));

stamp(Time, tai64) ->
    util:hex(<<(tai64(Time)):8/big-unsigned-unit:8>>).

wkday(Time) ->
    calendar:day_of_the_week(element(1, datetime(Time))).

wknum(Time) ->
    wknum(Time, 1).

wknum(Time, WeekStart) ->
    {{Y, _, _} = Date, _} = datetime(Time),
    case days({Y, 1, 4}, Date) + mod(calendar:day_of_the_week({Y, 1, 4}) - WeekStart, 7) of
        Days when Days < 0 ->
            {Y - 1, 53};
        Days ->
            {Y, Days div 7 + 1}
    end.

clock_distance(X, Y) ->
    min(util:mod(X - Y, 24), util:mod(Y - X, 24)).

timer() ->
    erlang:system_time(milli_seconds).

timer_elapsed(Start) ->
    timer() - Start.

timer_remaining(Timeout, Start) ->
    timeout_remaining(Timeout, timer_elapsed(Start)).

timeout(Time) ->
    seconds(Time) * 1000.

timeout_remaining(infinity, _Elapsed) ->
    infinity;
timeout_remaining(Timeout, Elapsed) ->
    max(Timeout - Elapsed, 0).

-module(ical).

%% General parsing
-export([parse/1,
         parse/2,
         unfold/1]).

%% Recurrence manipulation
-export([rrule/1,
         final/2,
         final/3,
         recur/3]).

-export([period/2,
         expand/3,
         filter/3,
         finite/1]).

%% NB: BYYEARDAY, BYWEEKNO, BYSETPOS not yet supported
-record(rrule, {freq,
                until,
                count,
                interval,
                bysecond,
                byminute,
                byhour,
                byday,
                bymonthday,
                bymonth,
                wkst,
                params}).

-record(exdate, {dates, params}).

-define(CRLF, "\r\n").
-define(IS_WHITESPACE(X), (X =:= $\s orelse
                           X =:= $\t orelse
                           X =:= $\r orelse
                           X =:= $\n)).
-define(IS_WKDAY(X), (X =:= <<"MO">> orelse
                      X =:= <<"TU">> orelse
                      X =:= <<"WE">> orelse
                      X =:= <<"TH">> orelse
                      X =:= <<"FR">> orelse
                      X =:= <<"SA">> orelse
                      X =:= <<"SU">>)).

int(Bin) ->
    list_to_integer(binary_to_list(Bin)).

parse(Content) ->
    parse(calendar, [parse(prop, Line) || Line <- unfold(Content)]).

parse(calendar, [{<<"BEGIN">>, [], <<"VCALENDAR">> = G}|Props]) ->
    {{G, Cal}, []} = parse(group, Props, {G, []}),
    {calendar, Cal};

parse(prop, Line) when is_binary(Line) ->
    [NameParams, Value] = binary:split(Line, <<":">>),
    [Name|Params] = binary:split(NameParams, <<";">>, [global]),
    parse(prop, {Name, [binary:split(P, <<"=">>) || P <- Params], Value});

parse(prop, {<<"RRULE">>, Params, Recur}) ->
    parse(recur, binary:split(Recur, <<";">>, [global]),
          #rrule{params=Params,
                 interval=1,
                 wkst=1});
parse(prop, {<<"EXDATE">>, Params, Value}) ->
    #exdate{params=Params,
            dates=lists:usort([parse(date, D) || D <- parse(list, Value)])};
parse(prop, Property) ->
    Property;

parse(list, List) ->
    binary:split(List, <<",">>, [global]);

%% NB: only handles UTC times properly
parse(date, <<Y:4/binary, M:2/binary, D:2/binary, "T", H:2/binary, Mi:2/binary, S:2/binary, "Z">>) ->
    {{int(Y), int(M), int(D)}, {int(H), int(Mi), int(S)}};
parse(date, <<Y:4/binary, M:2/binary, D:2/binary, "T", H:2/binary, Mi:2/binary, S:2/binary>>) ->
    {{int(Y), int(M), int(D)}, {int(H), int(Mi), int(S)}}; %% XXX: what is local time zone?
parse(date, <<Y:4/binary, M:2/binary, D:2/binary>>) ->
    {{int(Y), int(M), int(D)}, {0, 0, 0}};

parse(day, <<"MO">>) -> 1;
parse(day, <<"TU">>) -> 2;
parse(day, <<"WE">>) -> 3;
parse(day, <<"TH">>) -> 4;
parse(day, <<"FR">>) -> 5;
parse(day, <<"SA">>) -> 6;
parse(day, <<"SU">>) -> 7;

parse(wday, <<O:3/binary, Day/binary>>) when ?IS_WKDAY(Day) ->
    {int(<<O/binary>>), parse(day, Day)};
parse(wday, <<O:2/binary, Day/binary>>) when ?IS_WKDAY(Day) ->
    {int(<<O/binary>>), parse(day, Day)};
parse(wday, <<O:1/binary, Day/binary>>) when ?IS_WKDAY(Day) ->
    {int(<<O/binary>>), parse(day, Day)};
parse(wday, Day) when ?IS_WKDAY(Day) ->
    {any, parse(day, Day)}.

parse(group, [{<<"BEGIN">>, [], G}|Props], {P, V}) ->
    {Group, Rest} = parse(group, Props, {G, []}),
    parse(group, Rest, {P, [Group|V]});
parse(group, [{<<"END">>, [], G}|Props], {G, V}) ->
    {{G, lists:reverse(V)}, Props};
parse(group, [Prop|Rest], {G, V}) ->
    parse(group, Rest, {G, [Prop|V]});

parse(recur, [<<"FREQ=SECONDLY">>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{freq=seconds});
parse(recur, [<<"FREQ=MINUTELY">>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{freq=minutes});
parse(recur, [<<"FREQ=HOURLY">>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{freq=hours});
parse(recur, [<<"FREQ=DAILY">>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{freq=days});
parse(recur, [<<"FREQ=WEEKLY">>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{freq=weeks});
parse(recur, [<<"FREQ=MONTHLY">>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{freq=months});
parse(recur, [<<"FREQ=YEARLY">>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{freq=years});

parse(recur, [<<"UNTIL=", End/binary>>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{until=parse(date, End)});
parse(recur, [<<"COUNT=", Num/binary>>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{count=int(Num)});
parse(recur, [<<"INTERVAL=", Num/binary>>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{interval=int(Num)});

parse(recur, [<<"BYSECOND=", L/binary>>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{bysecond=[int(I) || I <- parse(list, L)]});
parse(recur, [<<"BYMINUTE=", L/binary>>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{byminute=[int(I) || I <- parse(list, L)]});
parse(recur, [<<"BYHOUR=", L/binary>>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{byhour=[int(I) || I <- parse(list, L)]});
parse(recur, [<<"BYDAY=", L/binary>>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{byday=[parse(wday, I) || I <- parse(list, L)]});
parse(recur, [<<"BYMONTHDAY=", L/binary>>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{bymonthday=[int(I) || I <- parse(list, L)]});
parse(recur, [<<"BYMONTH=", L/binary>>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{bymonth=[int(I) || I <- parse(list, L)]});
parse(recur, [<<"WKST=", Day/binary>>|Rest], RRule) ->
    parse(recur, Rest, RRule#rrule{wkst=parse(day, Day)});
parse(recur, [_|Rest], RRule) ->
    parse(recur, Rest, RRule);
parse(recur, [], RRule) ->
    RRule.

unfold(<<>>) ->
    [];
unfold(Content) ->
    case binary:split(Content, <<?CRLF>>) of
        [Line, <<Space, Rest/binary>>] when ?IS_WHITESPACE(Space) ->
            [L|R] = unfold(Rest),
            [<<Line/binary, L/binary>>|R];
        [Line, Rest] ->
            [Line|unfold(Rest)]
    end.

%% Recurrence handling

rrule(Rules) when is_list(Rules) ->
    lists:sort(fun (#rrule{}, _) ->
                       true;
                   (#exdate{}, _) ->
                       false
               end, [rrule(Rule) || Rule <- Rules]);
rrule(Rule) ->
    parse(prop, Rule).

period({Date, Time}, #rrule{freq=seconds}) ->
    {Date, Time};
period({Date, {H, Mi, _}}, #rrule{freq=minutes}) ->
    {Date, {H, Mi, 0}};
period({Date, {H, _, _}}, #rrule{freq=hours}) ->
    {Date, {H, 0, 0}};
period({Date, _}, #rrule{freq=days}) ->
    {Date, {0, 0, 0}};
period({{Y, M, _}, _}, #rrule{freq=months}) ->
    {{Y, M, 1}, {0, 0, 0}};
period({{Y, _, _}, _}, #rrule{freq=years}) ->
    {{Y, 1, 1}, {0, 0, 0}};
period({Date, Time}, #rrule{freq=weeks, wkst=WeekStart}) ->
    case calendar:day_of_the_week(Date) of
        D when D < WeekStart ->
            time:ago({Date, Time}, {D - WeekStart + 7, days});
        D ->
            time:ago({Date, Time}, {D - WeekStart, days})
    end.

final(First, Rules) ->
    final(First, Rules, {undefined, undefined}).

final(First, Rules, {_, Max} = Range) ->
    case finite(Rules) of
        Finite when Finite =:= true; Max =/= undefined ->
            case recur(First, Rules, Range) of
                [_|_] = Rs ->
                    lists:last(Rs);
                [] ->
                    undefined
            end;
        false ->
            undefined
    end.

recur(First, Rules, Range) when is_list(Rules) ->
    lists:foldl(fun (#rrule{} = Rule, Acc) ->
                        lists:umerge(recur(First, Rule, Range), Acc);
                    (#exdate{dates=Dates}, Acc) ->
                        ordsets:subtract(Acc, Dates)
                end, [], Rules);

recur(First, #rrule{count=N} = RRule, Range) when is_integer(N) ->
    recur(period(First, RRule), First, RRule, Range);
recur(First, #rrule{until=undefined} = RRule, {Min, _} = Range) ->
    recur(period(max(First, Min), RRule), First, RRule, Range);
recur(First, #rrule{until=D} = RRule, {Min, undefined}) ->
    recur(period(max(First, Min), RRule), First, RRule, {Min, D});
recur(First, #rrule{until=D} = RRule, {Min, Max}) ->
    recur(period(max(First, Min), RRule), First, RRule, {Min, min(D, Max)}).

recur(Period, First, RRule, {Min, Max}) when ((Min =:= undefined orelse First >= Min) andalso
                                              (Max =:= undefined orelse First =< Max)) ->
    recur(Period, First, RRule, {max(First, Min), Max}, [First]);
recur(Period, First, RRule, {Min, Max}) ->
    recur(Period, First, RRule, {max(First, Min), Max}, []).

recur(Period, _, _, {_, Max}, Acc) when Max =/= undefined, Period > Max ->
    Acc;
recur(Period, First, #rrule{freq=Freq, interval=I, count=C} = RRule, Range, Acc) ->
    case lists:umerge(lists:usort([T || T <- expand(Period, First, RRule), filter(T, Range, RRule)]), Acc) of
        List when is_integer(C), length(List) >= C ->
            lists:sublist(List, C);
        List ->
            recur(time:pass(Period, {I, Freq}), First, RRule, Range, List)
    end.

either(undefined, Default) ->
    Default;
either(Value, _) ->
    Value.

wkdays(undefined) ->
    undefined;
wkdays(ByDays) ->
    [Day || {_, Day} <- ByDays].

modays(undefined, _) ->
    undefined;
modays(ByMonthDays, LastDay) ->
    lists:foldl(fun (Day, Acc) when Day < 0 ->
                        case LastDay + Day + 1 of
                            D when D =< LastDay ->
                                [D|Acc];
                            _ ->
                                Acc
                        end;
                    (Day, Acc) when Day > 0, Day =< LastDay ->
                        [Day|Acc];
                    (_, Acc) ->
                        Acc
                end, [], ByMonthDays).

expand(Period, _Proto, #rrule{freq=seconds}) ->
    [Period];

expand({D, {H, Mi, _}},
         {_, {_, _, Sec}},
         #rrule{freq=minutes,
                bysecond=BySeconds}) ->
    [{D, {H, Mi, S}} || S <- either(BySeconds, [Sec])];

expand({D, {H, _, _}},
         {_, {_, Min, Sec}},
         #rrule{freq=hours,
                byminute=ByMinutes,
                bysecond=BySeconds}) ->
    [{D, {H, Mi, S}} || Mi <- either(ByMinutes, [Min]),
                        S <- either(BySeconds, [Sec])];

expand({D, {_, _, _}},
         {_, {Hour, Min, Sec}},
         #rrule{freq=days,
                byhour=ByHours,
                byminute=ByMinutes,
                bysecond=BySeconds}) ->
    [{D, {H, Mi, S}} || H <- either(ByHours, [Hour]),
                        Mi <- either(ByMinutes, [Min]),
                        S <- either(BySeconds, [Sec])];

expand(Period,
         {Date, {Hour, Min, Sec}},
         #rrule{freq=weeks,
                byday=ByDays,
                byhour=ByHours,
                byminute=ByMinutes,
                bysecond=BySeconds}) ->
    WkDays = either(wkdays(ByDays), [calendar:day_of_the_week(Date)]),
    lists:foldl(fun ({D, _}, Acc) ->
                        case lists:member(calendar:day_of_the_week(D), WkDays) of
                            true ->
                                [{D, {H, Mi, S}} || H <- either(ByHours, [Hour]),
                                                    Mi <- either(ByMinutes, [Min]),
                                                    S <- either(BySeconds, [Sec])] ++ Acc;
                            false ->
                                Acc
                        end
                end, [], [time:pass(Period, {N, days}) || N <- lists:seq(0, 6)]);

expand({{Y, M, _}, _} = Period,
         {Date, {Hour, Min, Sec}},
         #rrule{freq=months,
                bymonthday=undefined,
                byday=ByDays,
                byhour=ByHours,
                byminute=ByMinutes,
                bysecond=BySeconds}) when ByDays =/= undefined ->
    WkDays = either(wkdays(ByDays), [calendar:day_of_the_week(Date)]),
    NumDays = calendar:last_day_of_the_month(Y, M),
    lists:foldl(fun ({D, _}, Acc) ->
                        case lists:member(calendar:day_of_the_week(D), WkDays) of
                            true ->
                                [{D, {H, Mi, S}} || H <- either(ByHours, [Hour]),
                                                    Mi <- either(ByMinutes, [Min]),
                                                    S <- either(BySeconds, [Sec])] ++ Acc;
                            false ->
                                Acc
                        end
                end, [], [time:pass(Period, {N, days}) || N <- lists:seq(0, NumDays - 1)]);

expand({{Y, M, _}, _},
         {{_, _, DoM}, {Hour, Min, Sec}},
         #rrule{freq=months,
                bymonthday=ByMonthDays,
                byhour=ByHours,
                byminute=ByMinutes,
                bysecond=BySeconds}) ->
    [{{Y, M, D}, {H, Mi, S}} || D <- either(modays(ByMonthDays, calendar:last_day_of_the_month(Y, M)), [DoM]),
                                H <- either(ByHours, [Hour]),
                                Mi <- either(ByMinutes, [Min]),
                                S <- either(BySeconds, [Sec])];

expand({{Y, _, _}, _} = Period,
         {Date, {Hour, Min, Sec}},
         #rrule{freq=years,
                byday=ByDays,
                byhour=ByHours,
                byminute=ByMinutes,
                bysecond=BySeconds}) when ByDays =/= undefined ->
    WkDays = either(wkdays(ByDays), [calendar:day_of_the_week(Date)]),
    NumDays = case calendar:is_leap_year(Y) of
                  true ->
                      366;
                  false ->
                      365
              end,
    lists:foldl(fun ({D, _}, Acc) ->
                        case lists:member(calendar:day_of_the_week(D), WkDays) of
                            true ->
                                [{D, {H, Mi, S}} || H <- either(ByHours, [Hour]),
                                                    Mi <- either(ByMinutes, [Min]),
                                                    S <- either(BySeconds, [Sec])] ++ Acc;
                            false ->
                                Acc
                        end
                end, [], [time:pass(Period, {N, days}) || N <- lists:seq(0, NumDays - 1)]);

expand({{Y, _, _}, _},
         {{_, Month, DoM}, {Hour, Min, Sec}},
         #rrule{freq=years,
                bymonth=ByMonths,
                bymonthday=ByMonthDays,
                byhour=ByHours,
                byminute=ByMinutes,
                bysecond=BySeconds}) ->
    [{{Y, M, D}, {H, Mi, S}} || M <- either(ByMonths, [Month]),
                                D <- either(modays(ByMonthDays, calendar:last_day_of_the_month(Y, M)), [DoM]),
                                H <- either(ByHours, [Hour]),
                                Mi <- either(ByMinutes, [Min]),
                                S <- either(BySeconds, [Sec])].

filter(Time, {Min, _}, _) when Min =/= undefined, Time < Min ->
    false;
filter(Time, {_, Max}, _) when Max =/= undefined, Time > Max ->
    false;
filter(Time, _Range, RRule) ->
    lists:foldl(fun (_, false) ->
                        false;
                    (Type, true) ->
                        filter_(Type, Time, RRule)
                end, true, [bymonth, bymonthday, byday, byhour, byminute]).

filter_(bymonth, {{_, M, _}, _}, #rrule{bymonth=ByMonths}) when ByMonths =/= undefined ->
    lists:member(M, ByMonths);
filter_(bymonthday, _Time, #rrule{freq=Freq, byday=undefined}) when Freq =:= years; Freq =:= months ->
    true;
filter_(bymonthday, {{_, _, D}, _}, #rrule{bymonthday=ByMonthDays}) when ByMonthDays =/= undefined ->
    lists:member(D, ByMonthDays);
filter_(byday, {{Y, M, D} = Date, _}, #rrule{freq=months, byday=ByDays}) when ByDays =/= undefined ->
    DoW = calendar:day_of_the_week(Date),
    WoM = (D + 6) div 7,
    RWoM = (D - calendar:last_day_of_the_month(Y, M)) div 7 - 1,
    (lists:member({any, DoW}, ByDays) orelse
     lists:member({WoM, DoW}, ByDays) orelse
     lists:member({RWoM, DoW}, ByDays));
filter_(byhour, {_, {H, _, _}}, #rrule{byhour=ByHours}) when ByHours =/= undefined ->
    lists:member(H, ByHours);
filter_(byminute, {_, {_, M, _}}, #rrule{byminute=ByMinutes}) when ByMinutes =/= undefined ->
    lists:member(M, ByMinutes);
filter_(_, _, _) ->
    true.

finite(Rules) when is_list(Rules) ->
    lists:all(fun finite/1, Rules);
finite(#rrule{count=N}) when is_integer(N) ->
    true;
finite(#rrule{until=D}) when D =/= undefined ->
    true;
finite(#exdate{}) ->
    true;
finite(_) ->
    false.
-module(tzif).
-author("Jared Flatow").

-export([timezone/1,
         timezone/2,
         to_timezone/1,
         to_timezone/2,
         from_timezone/1,
         from_timezone/2]).

-export([parse/1,
         straddle/1,
         straddle/2,
         transition/2]).

-record(tzif, {version, transitions, time_types, leaps, fallback}).
-record(ttinfo, {gmt_offset, abbreviation, is_dst, is_gmt, is_std}).

%% external

timezone(Name) ->
    timezone("/usr/share/zoneinfo", Name).

timezone(ZoneInfoDir, Name) ->
    case path:read(path:join(ZoneInfoDir, Name)) of
        undefined ->
            undefined;
        Data ->
            parse(Data)
    end.

to_timezone(TZif) ->
    to_timezone(TZif, time:unow()).

to_timezone(#tzif{} = TZif, Time) ->
    case straddle(TZif, Time) of
        {{_Before, #ttinfo{gmt_offset=Offset}}, _} ->
            time:pass(Time, Offset);
        {undefined, _} ->
            undefined
    end.

from_timezone(TZif) ->
    from_timezone(TZif, time:unow()).

from_timezone(TZif, Time) ->
    case straddle(TZif, Time) of
        {{_Before, #ttinfo{gmt_offset=Offset}}, _} ->
            Guess = time:ago(Time, Offset),
            case time:diff(Time, to_timezone(TZif, Guess)) of
                0 ->
                    Guess;
                Delta ->
                    time:ago(Guess, Delta)
            end;
        {undefined, _} ->
            undefined
    end.

%% low-level

parse(Data) ->
    case read_tzif(Data, {version, 0}) of
        {#tzif{version=Version}, Rest} when Version >= $2 ->
            {TZif, R1} = read_tzif(Rest, {version, Version}),
            {Fallback, <<>>} = read_fallback(R1, {version, Version}),
            TZif#tzif{fallback=Fallback};
        {TZif, <<>>} ->
            TZif
    end.

read_tzif(<<"TZif",
            Version:8,
            0:15/integer-unit:8,
            TTIsGMTCnt:32,
            TTIsStdCnt:32,
            LeapCnt:32,
            TimeCnt:32,
            TypeCnt:32,
            CharCnt:32,
            Rest/binary>>, V) ->
    {TransTimes, R0} = read_transition_times(Rest, TimeCnt, [], V),
    {TransTypes, R1} = read_transition_types(R0, TimeCnt, []),
    {TimeTypes0, R2} = read_time_type_info(R1, TypeCnt),
    {AbbrData, R3} = read_abbr_data(R2, CharCnt),
    {Leaps, R4} = read_leaps(R3, LeapCnt, V),
    {TimeTypes1, R5} = read_std_indicators(R4, TTIsStdCnt, TimeTypes0),
    {TimeTypes2, R6} = read_gmt_indicators(R5, TTIsGMTCnt, TimeTypes1),
    {#tzif{version=Version,
           transitions=make_transitions(TransTimes, TransTypes),
           time_types=make_time_types(TimeTypes2, AbbrData),
           leaps=Leaps}, R6}.

c_string(Data, Start) ->
    binary:part(Data, Start, c_strlen(Data, 0)).

c_strlen(<<0, _/binary>>, Length) ->
    Length;
c_strlen(<<_, Data/binary>>, Length) ->
    c_strlen(Data, Length + 1).

make_transitions(Times, Types) ->
    make_transitions(Times, Types, gb_trees:empty()).

make_transitions([Time|Times], [Type|Types], Acc) ->
    make_transitions(Times, Types, gb_trees:insert(Time, Type, Acc));
make_transitions([], [], Acc) ->
    Acc.

make_time_types(TimeTypes, AbbrData) ->
    array:map(fun (_Index, #ttinfo{abbreviation=AbbrIndex} = TTInfo) ->
                      TTInfo#ttinfo{abbreviation=c_string(AbbrData, AbbrIndex)}
              end, TimeTypes).

read_transition_times(Rest, 0, Acc, _V) ->
    {Acc, Rest};
read_transition_times(<<Time:4/integer-signed-unit:8, Rest/binary>>, N, Acc, {version, 0}) ->
    read_transition_times(Rest, N - 1, [Time|Acc], {version, 0});
read_transition_times(<<Time:8/integer-signed-unit:8, Rest/binary>>, N, Acc, {version, V}) ->
    read_transition_times(Rest, N - 1, [Time|Acc], {version, V}).

read_transition_types(Rest, 0, Acc) ->
    {Acc, Rest};
read_transition_types(<<Type:8/unsigned-integer, Rest/binary>>, N, Acc) ->
    read_transition_types(Rest, N - 1, [Type|Acc]).

read_time_type_info(Rest, N) ->
    read_time_type_info(Rest, 0, array:new(N), N).

read_time_type_info(Rest, N, Acc, N) ->
    {Acc, Rest};
read_time_type_info(<<GMTOff:4/integer-signed-unit:8, IsDST:8, AbbrInd:8, Rest/binary>>, K, Acc, N) ->
    TimeType = #ttinfo{gmt_offset=GMTOff, abbreviation=AbbrInd, is_dst=(IsDST > 0), is_gmt=false, is_std=false},
    read_time_type_info(Rest, K + 1, array:set(K, TimeType, Acc), N).

read_abbr_data(Rest, N) ->
    <<AbbrData:N/binary, R1/binary>> = Rest,
    {AbbrData, R1}.

read_leaps(Rest, N, V) ->
    read_leaps(Rest, N, gb_trees:empty(), V).

read_leaps(Rest, 0, Acc, _V) ->
    {Acc, Rest};
read_leaps(<<Time:4/integer-signed-unit:8, Seconds:4/integer-signed-unit:8, Rest/binary>>, N, Acc, {version, 0}) ->
    read_leaps(Rest, N - 1, gb_trees:insert(Time, Seconds, Acc), {version, 0});
read_leaps(<<Time:8/integer-signed-unit:8, Seconds:8/integer-signed-unit:8, Rest/binary>>, N, Acc, {version, V}) ->
    read_leaps(Rest, N - 1, gb_trees:insert(Time, Seconds, Acc), {version, V}).

read_std_indicators(Rest, 0, Acc) ->
    {Acc, Rest};
read_std_indicators(<<TTIndex:8, Rest/binary>>, N, Acc) ->
    TimeType = array:get(TTIndex, Acc),
    read_std_indicators(Rest, N - 1, array:set(TTIndex, TimeType#ttinfo{is_std=true}, Acc)).

read_gmt_indicators(Rest, 0, Acc) ->
    {Acc, Rest};
read_gmt_indicators(<<TTIndex:8, Rest/binary>>, N, Acc) ->
    TimeType = array:get(TTIndex, Acc),
    read_std_indicators(Rest, N - 1, array:set(TTIndex, TimeType#ttinfo{is_gmt=true}, Acc)).

read_fallback(<<"\n", Rest/binary>>, _V) ->
    {PosixTZ, <<"\n">>} = read_posix_tz(Rest, <<>>),
    {PosixTZ, <<>>}.

read_posix_tz(<<C, Rest/binary>>, Acc) when C =/= $\n ->
    read_posix_tz(Rest, <<Acc/binary, C>>);
read_posix_tz(Rest, Acc) ->
    {Acc, Rest}.

%% transition search

straddle_tree({_, Tree}, Key) ->
    straddle_tree(Tree, Key, {undefined, undefined}).

straddle_tree(nil, _Key, Acc) ->
    Acc;
straddle_tree({K, V, _, Bigger}, Key, {Below, Above}) when K =< Key ->
    straddle_tree(Bigger, Key, {util:defmax({K, V}, Below), Above});
straddle_tree({K, V, Smaller, _}, Key, {Below, Above}) when K > Key ->
    straddle_tree(Smaller, Key, {Below, util:defmin({K, V}, Above)}).

straddle(TZif) ->
    straddle(TZif, time:unow()).

straddle(#tzif{transitions=Transitions} = TZif, Time) ->
    {Below, Above} = straddle_tree(Transitions, time:unix(Time)),
    {transition(TZif, Below), transition(TZif, Above)}.

transition(#tzif{}, undefined) ->
    undefined;
transition(#tzif{time_types=TimeTypes}, {Time, TypeIndex}) ->
    {Time, array:get(TypeIndex, TimeTypes)};
transition(#tzif{transitions=Transitions} = TZif, Time) ->
    case gb_trees:lookup(Time, Transitions) of
        none ->
            undefined;
        {value, TypeIndex} ->
            transition(TZif, {Time, TypeIndex})
    end.

-module(str).
-author("Jared Flatow").

-export([def/2,
         join/2,
         snap/2,
         split/2,
         words/1,
         words/2,
         substr/2,
         substr/3,
         disfix/2,
         format/2,
         lstrip/2,
         rstrip/2,
         strip/2,
         lower/1,
         upper/1,
         replace/3,
         contains/2,
         startswith/2,
         endswith/2]).

-import(util, [bin/1, list/1]).

def(<<>>, Default) ->
    Default;
def([], Default) ->
    Default;
def(Value, Default) ->
    util:def(Value, Default).

join(List, Sep) ->
    util:join(List, Sep).

snap(Data, Pos) when is_integer(Pos), Pos < 0 ->
    {substr(Data, 0, size(Data) + Pos), substr(Data, Pos)};
snap(Data, Pos) when is_integer(Pos) ->
    {substr(Data, 0, Pos), substr(Data, Pos)};
snap(Data, Sep) ->
    case binary:split(Data, Sep) of
        [A, B] ->
            {A, B};
        [A] ->
            {A, <<>>};
        [] ->
            {<<>>, <<>>}
    end.

split(undefined, _) ->
    [];
split(<<>>, _) ->
    [];
split(Data, Sep) when is_binary(Data), is_integer(Sep) ->
    binary:split(Data, <<Sep>>, [global]);
split(Data, Seps) when is_binary(Data), is_list(Seps) ->
    binary:split(Data, [case Sep of
                            S when is_binary(S) ->
                                S;
                            S when is_integer(S) ->
                                <<S>>;
                            S ->
                                bin(S)
                        end || Sep <- Seps], [global]);
split(Data, Sep) when is_binary(Data) ->
    binary:split(Data, Sep, [global]);
split(Data, Sep) ->
    split(bin(Data), Sep).

words(Data) ->
    words(Data, "\\W+").

words(undefined, _) ->
    [];
words(Data, Sep) ->
    re:split(Data, Sep, [trim]).

substr(Data, N) ->
    substr(Data, N, size(Data)).

substr(Data, N, Len) when N < 0 ->
    substr(Data, max(0, size(Data) + N), Len);
substr(Data, N, Len) ->
    M = min(N, size(Data)),
    binary:part(Data, M, min(max(-M, Len), size(Data) - M)).

disfix(Prefix, Str) when is_list(Str) ->
    list(disfix(Prefix, bin(Str)));
disfix(Prefix, Bin) when is_binary(Bin), is_list(Prefix) ->
    disfix(bin(Prefix), Bin);
disfix(Prefix, Bin) when is_binary(Bin) ->
    Size = size(Prefix),
    case Bin of
        <<Prefix:Size/binary, Rest/binary>> ->
            Rest;
        _ ->
            Bin
    end.

format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

lstrip([C|Rest], C) ->
    lstrip(Rest, C);
lstrip(<<C, Rest/bits>>, C) ->
    lstrip(Rest, C);
lstrip(Seq, _) ->
    Seq.

rstrip([C|Rest], C) ->
    case rstrip(Rest, C) of
        [] ->
            [];
        Tail  ->
            [C|Tail]
    end;
rstrip([O|Rest], C) ->
    [O|rstrip(Rest, C)];
rstrip(<<_, _/bits>> = Bin, C) when is_binary(Bin) ->
    case binary:last(Bin) of
        C ->
            rstrip(binary:part(Bin, 0, size(Bin) - 1), C);
        _ ->
            Bin
    end;
rstrip(Seq, _) ->
    Seq.

strip(Seq, C) ->
    rstrip(lstrip(Seq, C), C).

lower(undefined) ->
    undefined;
lower(Bin) when is_binary(Bin) ->
    unicode:characters_to_binary(lower(unicode:characters_to_list(Bin)));
lower(Str) ->
    string:to_lower(Str).

upper(undefined) ->
    undefined;
upper(Bin) when is_binary(Bin) ->
    unicode:characters_to_binary(upper(unicode:characters_to_list(Bin)));
upper(Str) ->
    string:to_upper(Str).

replace(Str, Pat, Sub) when is_list(Pat); is_binary(Pat) ->
    case Str of
        _ when is_binary(Str) ->
            re:replace(Str, Pat, Sub, [global, {return, binary}]);
        _ when is_list(Str) ->
            re:replace(Str, Pat, Sub, [global, {return, list}])
    end;
replace(Str, Pat, Sub) when is_list(Str) ->
    [case C of Pat -> Sub; _ -> C end || C <- Str];
replace(Bin, Pat, Sub) when is_binary(Bin) ->
    << <<(case C of Pat -> Sub; _ -> C end)>> || <<C>> <= Bin >>.

contains(Str, Pat) when is_list(Pat); is_binary(Pat) ->
    re:run(Str, Pat) =/= nomatch;
contains([Pat|_], Pat) ->
    true;
contains([_|Rest], Pat) ->
    contains(Rest, Pat);
contains(<<Pat, _/binary>>, Pat) ->
    true;
contains(<<_, Rest/binary>>, Pat) ->
    contains(Rest, Pat);
contains(_, _) ->
    false.

startswith(<<Prefix, _/binary>>, Prefix) when is_integer(Prefix) ->
    true;
startswith(Bin, Prefix) when is_binary(Bin) ->
    P = bin(Prefix),
    N = size(P),
    case Bin of
        <<P:N/binary, _/binary>> ->
            true;
        _ ->
            false
    end;
startswith([Prefix|_], Prefix) ->
    true;
startswith(List, Prefix) when is_list(List) ->
    lists:prefix(list(Prefix), List);
startswith(_, _) ->
    false.

endswith(Bin, Suffix) when is_binary(Bin), is_integer(Suffix) ->
    binary:last(Bin) =:= Suffix orelse endswith(Bin, bin(Suffix));
endswith(Bin, Suffix) when is_binary(Bin) ->
    S = bin(Suffix),
    N = size(Bin) - size(S),
    case Bin of
        <<_:N/binary, S/binary>> ->
            true;
        _ ->
            false
    end;
endswith(List, Suffix) when is_list(List) ->
    case lists:last(List) of
        Suffix ->
            true;
        _ ->
            lists:suffix(list(Suffix), List)
    end;
endswith(_, _) ->
    false.

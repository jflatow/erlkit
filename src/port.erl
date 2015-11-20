-module(port).
-author("Jared Flatow").

-export([call/1,
         call/2,
         wait/2]).

call(Cmd) ->
    call(Cmd, infinity).

call(Cmd, Timeout) ->
    wait(open_port({spawn, Cmd}, [binary, exit_status, stderr_to_stdout]), Timeout).

wait(Port, Timeout) ->
    wait(Port, Timeout, []).

wait(Port, Timeout, Acc) ->
    receive
        {Port, {data, Data}} ->
            wait(Port, Timeout, [Data|Acc]);
        {Port, {exit_status, 0}} ->
            {ok, lists:reverse(Acc)};
        {Port, {exit_status, N}} ->
            {error, N, lists:reverse(Acc)}
    after Timeout ->
            {error, timeout, lists:reverse(Acc)}
    end.

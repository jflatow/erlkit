-module(proc).
-author("Jared Flatow").

-export([lock/3,
         untrap/2,
         repeater/1]).

lock(Key, Fun, Msg) ->
    case whereis(Key) of
        undefined ->
            spawn_link(fun () ->
                               try
                                   register(Key, self()),
                                   self() ! Msg,
                                   Fun()
                               catch
                                   error:badarg ->
                                       lock(Key, Fun, Msg)
                               end
                       end);
        Proc ->
            Proc ! Msg
    end.

untrap({'EXIT', _, normal}, Default) ->
    Default;
untrap({'EXIT', _, Reason}, _) ->
    exit(Reason);
untrap(_, Default) ->
    Default.

repeater(Fun) ->
    spawn_link(fun () -> repeat(Fun) end).

repeat(Fun) ->
    Fun(), repeat(Fun).

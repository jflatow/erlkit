-module(proc).

-export([lock/3]).

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

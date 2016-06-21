-module(smtp_test).

-include_lib("eunit/include/eunit.hrl").

smtp_test() ->
    spawn(
      fun () ->
              {ok, Server} =
                  smtp:start_server(
                    #{options => #{port => 9825},
                      deliver =>
                          fun (#{envelope := #{
                                   return_path := _Sender %% XXX: check SPF
                                  },
                                 content := Content}, _Server) ->
                                  case catch mime:parse(Content) of
                                      {invalid, _Reason} ->
                                          ["550", "5.6.1 Internet messages only, please"];
                                      _MIME -> %% XXX: check DKIM
                                          ["250", "2.0.0 OK"]
                                  end
                          end
                     }),
              Conf = #{options => #{host => "localhost", port => 9825}},
              Desc = {<<"jared@localhost">>,
                      [<<"jflatow@localhost">>],
                      mime:format({[{"From", " <jflatow@gmail.com>"},
                                    {"Subject", " Testing"}], <<"123">>})},
              ?assertMatch({ok, [["250"|_]|_], _}, smtp:send_email(Desc, Conf)),
              smtp:stop_server(Server)
      end).

%% coding: latin-1
%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
-module(wslogi_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(ensureExited(Pid),
        (fun() -> Old = process_flag(trap_exit, true),
                  exit(Pid, shutdown),
                  receive
                      {'EXIT', Pid, _} -> ?assert(true)
                  after 50 ->
                          ?assert(timeout)
                  end,
                  process_flag(trap_exit, Old)
         end)()).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

server_test_() ->
    {foreach,
     fun()    -> {ok, Pid} = wslogi_server:start_link(), Pid end,
     fun(Pid) -> ?ensureExited(Pid) end,
     [
      {"The set header can be acquired",
       fun() ->
               Headers = [{a, a}, {b, b}],
               ?assertEqual(ok,      wslogi_server:set_headers(Headers)),
               ?assertEqual(Headers, wslogi_server:get_headers())
       end},
      {"Nothing has been registered in the initial state",
       fun() ->
               ?assertEqual([], wslogi_server:get_headers())
       end},
      {"It can use delete",
       fun() ->
               Headers = [{a, a}, {b, b}, {c, c}],
               ?assertEqual(ok,       wslogi_server:set_headers(Headers)),
               ?assertEqual(ok,       wslogi_server:delete_headers([a, c])),
               ?assertEqual([{b, b}], wslogi_server:get_headers())
       end},
      {"If trying to delete a non-existent key, this key will be ignored",
       fun() ->
               Headers = [{a, a}, {b, b}],
               ?assertEqual(ok,       wslogi_server:set_headers(Headers)),
               ?assertEqual(ok,       wslogi_server:delete_headers([a, d])),
               ?assertEqual([{b, b}], wslogi_server:get_headers())
       end},
      {"It can use clear",
       fun() ->
               Headers = [{a, a}, {b, b}],
               ?assertEqual(ok, wslogi_server:set_headers(Headers)),
               ?assertEqual(ok, wslogi_server:clear_headers()),
               ?assertEqual([], wslogi_server:get_headers())
       end},
      {"If the process is dead, it deletes header",
       fun() ->
               ok = meck:new(wslogi_server, [no_link, passthrough]),

               Headers = [{a, a}, {b, b}],
               spawn(fun() -> ?assertEqual(ok, wslogi_server:set_headers(Headers)) end),
               %% MEMO: Waiting for be caught the DOWN message by wslogi_server.
               ?assertEqual(ok, meck:wait(1, wslogi_server, handle_info, '_', 1000)),
               ?assertEqual([], wslogi_server:get_headers()),

               _ = meck:unload()
       end}
      ]}.

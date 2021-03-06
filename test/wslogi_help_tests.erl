%% coding: latin-1
%%
%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
-module(wslogi_help_tests).
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

get_help_test_() ->
    {setup,
     fun()    -> {ok, Pid} = wslogi_help:start_link(), Pid end,
     fun(Pid) -> ?ensureExited(Pid) end,
     [
      {"It may be obtained contetns of priv/help.md",
       fun() ->
               {ok, Data} = file:read_file("../priv/help.md"),
               ?assertEqual(Data, wslogi_help:get_help())
       end}
     ]}.

coverage_test_() ->
    [
     ?_assertMatch({noreply, _}, wslogi_help:handle_call(a, self(), undefined)),
     ?_assertMatch({noreply, _}, wslogi_help:handle_cast(a, undefined)),
     ?_assertMatch({noreply, _}, wslogi_help:handle_info(a, undefined)),
     ?_assertEqual(ok,           wslogi_help:terminate(a, undefined)),
     ?_assertMatch({ok, _},      wslogi_help:code_change(old, undefined, extra))
    ].

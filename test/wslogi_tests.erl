%% coding: latin-1
%%
%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
-module(wslogi_tests).
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

wslogi_test_() ->
    {setup,
     fun()        -> {ok, Started} = application:ensure_all_started(cowboy), Started end,
     fun(Started) -> [application:stop(X) || X <- Started] end,
     [
      start_and_stop()
     ]}.

start_and_stop() ->
    [
     {"If it does not exist, stop returns error",
      fun() ->
              Port = 0,
              ?assertEqual({error, not_found}, wslogi:stop(Port))
      end},
     {"It can start and stop",
      fun() ->
              Port = 0,
              ?assertEqual(ok, wslogi:start(Port)),
              ?assertEqual(ok, wslogi:stop(Port))
      end}
    ].

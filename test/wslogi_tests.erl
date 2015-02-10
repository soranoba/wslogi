%% coding: latin-1
%%
%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
-module(wslogi_tests).
-include_lib("eunit/include/eunit.hrl").

-on_load(init/0).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

start_and_stop_test_() ->
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

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

-spec init() -> ok.
init() ->
    _ = application:ensure_all_started(cowboy),
    ok.

%% coding: latin-1
%%
%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
-module(wslogi_ws_handler_tests).
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

coverage_test_() ->
    [
     ?_assertMatch({upgrade, protocol, cowboy_websocket}, wslogi_ws_handler:init({tcp, http}, req, opt)),
     ?_assertMatch({ok, _, _}, wslogi_ws_handler:websocket_handle(data, req, state)),
     ?_assertEqual(ok, wslogi_ws_handler:websocket_terminate(reason, req, state))
    ].

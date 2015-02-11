%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
-module(wslogi_example_app).

-behaviour(application).

%%----------------------------------------------------------------------------------------------------------------------
%% 'application' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([start/2, stop/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'application' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
start(_StartType, _StartArgs) ->
    _ = wslogi:start(8080),

    Dispatch = cowboy_router:compile([{'_', [{"/", wslogi_example_handler, []}]}]),
    {ok, _}  = cowboy:start_http(?MODULE, 100, [{port, 8000}], [{env, [{dispatch, Dispatch}]}]),
    wslogi_example_sup:start_link().

%% @private
stop(_State) ->
    cowboy:stop_listener(?MODULE),
    ok.

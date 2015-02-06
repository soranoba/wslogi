%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc `wslogi' application module
%% @private
-module(wslogi_app).

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
    wslogi_sup:start_link().

%% @private
stop(_State) ->
    ok.

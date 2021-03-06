%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc `wslogi' application supervisor.
%% @private
-module(wslogi_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc start supervisor
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    Help  = wslogi_help,
    Loger = wslogi_server,

    {ok, {{one_for_one, 5, 10},
          [
           {Help, {Help,  start_link, []}, permanent, 1000, worker, [Help]},
           {Loger,{Loger, start_link, []}, permanent, 1000, worker, [Loger]}
          ]}}.

%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc A websocket logging framework for Erlang/OTP
-module(wslogi).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------

-export([
         start/1,
         stop/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functinos
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Start the websocket server for wslogi in this port.
-spec start(inet:port_number()) -> ok | {error, Reason :: term()}.
start(Port) ->
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", wslogi_ws_handler, []}]}]),
    case cowboy:start_http({?MODULE, Port}, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]) of
        {ok, _} -> ok;
        Other   -> Other
    end.

%% @doc Stop the websocket server of this port.
-spec stop(inet:port_number()) -> ok | {error, not_found}.
stop(Port) ->
    cowboy:stop_listener({?MODULE, Port}).

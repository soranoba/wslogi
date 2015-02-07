%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc cowboy websocket handler
%% @private
-module(wslogi_ws_handler).
-behaviour(cowboy_websocket_handler).

%%----------------------------------------------------------------------------------------------------------------------
%% 'cowboy_websocket_handler' Callback API
%%----------------------------------------------------------------------------------------------------------------------

-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records
%%----------------------------------------------------------------------------------------------------------------------

-record(?MODULE,
        {
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% 'cowboy_websocket_handler' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, #?MODULE{}}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, <<"That's what she said! ", Msg/binary>>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc cowboy websocket handler
%% @private
-module(wslogi_ws_handler).
-include("wslogi_internal.hrl").

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
          log_level = ?NONE :: wslogi_msg:log_level(),
          ip                :: binary(),
          path              :: binary()
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% 'cowboy_websocket_handler' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req0, _Opts) ->
    {{Ip, _}, Req1} = cowboy_req:peer(Req0),
    {Path,    Req2} = cowboy_req:path(Req1),
    {ok, Req2, #?MODULE{ip = Ip, path = Path}}.

%% @doc This function will call when received a message using websocket.
%% @private
websocket_handle({text, Msg}, Req, State) ->
    %% TODO: delete
    self() ! wslogi_msg:put(?ALERT, <<"Message">>),

    case wsclient_command(Msg, State) of
        {ok, Response, State} -> {reply, {text, Response}, Req, State};
        {error, State}        -> {ok, Req, State}
    end;
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%% @doc This function will call when received a message.
%% @private
websocket_info(Info, Req, #?MODULE{log_level = LogLevel} = State) ->
    case wslogi_msg:get(LogLevel, Info) of
        {ok, Msg} -> {reply, {text, Msg}, Req, State};
        error     -> {ok, Req, State}
    end.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Return a response in accordance with the command.
-spec wsclient_command(Msg :: binary(), State) -> {ok, Response :: binary(), State} | {error, State} when
      State :: #?MODULE{}.
wsclient_command(<<"help", _/binary>>, State) ->
    {ok, <<"dummy">>, State};
wsclient_command(_, #?MODULE{ip = Ip, path = Path} = State) ->
    {ok, <<(moyo_binary:to_binary(Ip))/binary, " ", Path/binary>>, State}.

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
          log_level = ?NONE   :: wslogi_msg:log_level(),
          ip                  :: inet:ip_address(),
          path_info           :: [binary()],
          do_watching = false :: boolean(),
          filter = []         :: [binary()]
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% 'cowboy_websocket_handler' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req0, _Opts) ->
    {{Ip, _},  Req1} = cowboy_req:peer(Req0),
    {PathInfo, Req2} = cowboy_req:path_info(Req1),
    {ok, Req2, #?MODULE{ip = Ip, path_info = PathInfo}}.

%% @doc This function will call when received a message using websocket.
%% @private
websocket_handle({text, Msg}, Req, State0) ->
    case wsclient_command(split_command(Msg), State0) of
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
-spec wsclient_command(Msg :: [binary()], State) -> {ok, Response :: binary(), State} | {error, State} when
      State :: #?MODULE{}.
wsclient_command([<<"help">> | _], State) ->
    {ok, wslogi_help:get_help(), State};
wsclient_command([<<"start">> | _], State) ->
    {ok, <<"[success] Start the print of logs\n">>, State#?MODULE{do_watching = true}};
wsclient_command([<<"stop">> | _], State) ->
    {ok, <<"[success] Stop the print of logs\n">>, State#?MODULE{do_watching = false}};
wsclient_command([<<"filter">>, <<"show">> | _], #?MODULE{filter = Filter} = State) ->
    Response = iolist_to_binary(filter_to_iolist(Filter)),
    {ok, Response, State};
wsclient_command([<<"filter">>, <<"add">>, H | T], #?MODULE{filter = Filter} = State) ->
    NewFilter = iolist_to_binary([H | [<<" ", X/binary>> || X <- T]]),
    {ok, <<"[success] New Filter: ", NewFilter/binary>>, State#?MODULE{filter = [NewFilter | Filter]}};
wsclient_command(_, State) ->
    {error, State}.

%% @doc Split the command and args
-spec split_command(binary()) -> [binary()].
split_command(Msg) ->
    binary:split(Msg, [<<" ">>, <<"\n">>], [global, trim]).

%% @doc Generate the response, command that is `filter show'.
-spec filter_to_iolist([binary()]) -> iodata().
filter_to_iolist(Filter) ->
    filter_to_iolist_impl(Filter, 1, []).

-spec filter_to_iolist_impl([binary()], non_neg_integer(), [binary()]) -> iodata().
filter_to_iolist_impl([], _, Result) ->
    lists:reverse(Result);
filter_to_iolist_impl([H | T], Num, Result) ->
    filter_to_iolist_impl(T, Num + 1, [ [integer_to_binary(Num), <<" : ">>, H, <<"\n">>] | Result]).

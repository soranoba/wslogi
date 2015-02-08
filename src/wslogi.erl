%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc A websocket logging framework for Erlang/OTP
-module(wslogi).
-include("wslogi_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------

-export([
         start/1,
         stop/1,

         %% Header
         set_headers/1,
         get_headers/0,
         delete_headers/1,
         clear_headers/0,

         %% Output log
         debug/2,     debug/3,
         verbose/2,   verbose/3,
         info/2,      info/3,
         notice/2,    notice/3,
         warning/2,   warning/3,
         error/2,     error/3,
         critical/2,  critical/3,
         alert/2,     alert/3,
         emergency/2, emergency/3
        ]).

-export_type([
              header_key/0,
              header_value/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------

-type header_key()   :: term().
%% @see wslogi:set_headers/1
-type header_value() :: term().
%% @see wslogi:set_headers/1

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

%% @doc Add the process-specific log header.
%%
%% Specific key-value
%% | key-value                 | description                            |
%% |:--------------------------|:---------------------------------------|
%% | `{ip, inet:ip_address()}' | It is used in the `filter ip' command. |
%%
-spec set_headers([{header_key(), header_value()}]) -> ok.
set_headers(KVs) ->
    wslogi_server:set_headers(KVs).

%% @doc Get the process-specific log header.
-spec get_headers() -> [{header_key(), header_value()}].
get_headers() ->
    wslogi_server:get_headers().

%% @doc Delete the process-specific log header corresponding to Keys.
-spec delete_headers([header_key()]) -> ok.
delete_headers(Keys) ->
    wslogi_server:delete_headers(Keys).

%% @doc Delete the process-specific all log header.
-spec clear_headers() -> ok.
clear_headers() ->
    wslogi_server:clear_headers().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functinos: Output log
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Output the log in debug level.
-spec debug(file:filename(), io:format(), [term()]) -> ok.
debug(Path, Format, Args) ->
    wslogi_msg:send(?DEBUG, Path, Format, Args).

%% @equiv debug("/",  Format, Args)
-spec debug(Format :: io:format(), Args :: [term()]) -> ok.
debug(Format, Args) ->
    debug("/", Format, Args).

%% @doc Output the log in verbose level.
-spec verbose(file:filename(), io:format(), [term()]) -> ok.
verbose(Path, Format, Args) ->
    wslogi_msg:send(?VERBOSE, Path, Format, Args).

%% @equiv verbose("/", Format, Args)
-spec verbose(Format :: io:format(), Args :: [term()]) -> ok.
verbose(Format, Args) ->
    verbose("/", Format, Args).

%% @doc Output the log in info level.
-spec info(file:filename(), io:format(), [term()]) -> ok.
info(Path, Format, Args) ->
    wslogi_msg:send(?INFO, Path, Format, Args).

%% @equiv info("/", Format, Args)
-spec info(Format :: io:format(), Args :: [term()]) -> ok.
info(Format, Args) ->
    info("/", Format, Args).

%% @doc Output the log in notice level.
-spec notice(file:filename(), io:format(), [term()]) -> ok.
notice(Path, Format, Args) ->
    wslogi_msg:send(?NOTICE, Path, Format, Args).

%% @equiv notice("/", Format, Args)
-spec notice(Format :: io:format(), Args :: [term()]) -> ok.
notice(Format, Args) ->
    notice("/", Format, Args).

%% @doc Output the log in warning level.
-spec warning(file:filename(), io:format(), [term()]) -> ok.
warning(Path, Format, Args) ->
    wslogi_msg:send(?WARNING, Path, Format, Args).

%% @equiv warning("/", Format, Args)
-spec warning(Format :: io:format(), Args :: [term()]) -> ok.
warning(Format, Args) ->
    warning("/", Format, Args).

%% @doc Output the log in error level.
-spec error(file:filename(), io:format(), [term()]) -> ok.
error(Path, Format, Args) ->
    wslogi_msg:send(?ERROR, Path, Format, Args).

%% @equiv error("/", Format, Args)
-spec error(Format :: io:format(), Args :: [term()]) -> ok.
error(Format, Args) ->
    ?MODULE:error("/", Format, Args).

%% @doc Output the log in critical level.
-spec critical(file:filename(), io:format(), [term()]) -> ok.
critical(Path, Format, Args) ->
    wslogi_msg:send(?CRITICAL, Path, Format, Args).

%% @equiv critical("/", Format, Args)
-spec critical(Format :: io:format(), Args :: [term()]) -> ok.
critical(Format, Args) ->
    critical("/", Format, Args).

%% @doc Output the log in alert level.
-spec alert(file:filename(), io:format(), [term()]) -> ok.
alert(Path, Format, Args) ->
    wslogi_msg:send(?ALERT, Path, Format, Args).

%% @equiv alert("/", Format, Args)
-spec alert(Format :: io:format(), Args :: [term()]) -> ok.
alert(Format, Args) ->
    alert("/", Format, Args).

%% @doc Output the log in emergency level.
-spec emergency(file:filename(), io:format(), [term()]) -> ok.
emergency(Path, Format, Args) ->
    wslogi_msg:send(?EMERGENCY, Path, Format, Args).

%% @equiv emergency("/", Format, Args)
-spec emergency(Format :: io:format(), Args :: [term()]) -> ok.
emergency(Format, Args) ->
    emergency("/", Format, Args).

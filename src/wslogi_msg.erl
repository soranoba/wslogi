%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc `wslogi' log message
%% @private

-module(wslogi_msg).
-include("wslogi_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------

-export([
         get/2,
         put/2,
         send/4,
         watch/1,
         unwatch/0
        ]).

-export_type([
              log_level/0,
              message/0,
              wslogi_msg/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types & Macros
%%----------------------------------------------------------------------------------------------------------------------

-type log_level()  :: 0..511.
-type message()    :: binary().
%% Log message entered by the user.
-type wslogi_msg() :: {'$wslogi', log_level(), message()}.
%% Log message format at `wslogi'.

-define(GPROC_NAME(Path), {p, l, {wslogi, Path}}).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Take out a message if matches the specific log level.
-spec get(log_level(), wslogi_msg()) -> {ok, message()} | error.
get(LogLevel, {'$wslogi', MsgLogLevel, Msg}) when LogLevel band MsgLogLevel > 0 ->
    {ok, Msg};
get(_, _) ->
    error.

%% @doc Specify the log level and create a message.
-spec put(log_level(), message()) -> wslogi_msg().
put(LogLevel, Msg) when LogLevel > ?NONE andalso LogLevel =< ?MAX_LEVEL andalso is_binary(Msg) ->
    {'$wslogi', LogLevel, Msg};
put(LogLevel, Msg) ->
    error(badarg, [LogLevel, Msg]).

%% @doc Send a log message
-spec send(log_level(), Path :: file:filename(), io:format(), Args :: [term()]) -> ok.
send(LogLevel, Path, Format, Args) ->
    Msg = list_to_binary(io_lib:format(Format, Args)),
    %% MEMO: "foo/bar/" -> <<"/foo/bar">>
    _   = gproc:send(?GPROC_NAME(filename:join([<<>>, Path])), ?MODULE:put(LogLevel, Msg)),
    ok.

%% @doc Monitor the messages that are sent to the path.
-spec watch(PathInfo :: [binary()]) -> ok.
watch(PathInfo) ->
    watch([<<"/">>], PathInfo).

%% @see watch/1
-spec watch([binary()], [binary()]) -> ok.
watch(MonitorPath, RestPath) ->
    true = gproc:reg(?GPROC_NAME(filename:join(MonitorPath))),
    case RestPath of
        []      -> ok;
        [H | T] -> watch(MonitorPath ++ [H], T)
    end.

%% @doc Calling process unmonitor the message that are send to the path.
-spec unwatch() -> ok.
unwatch() ->
    gproc:goodbye().

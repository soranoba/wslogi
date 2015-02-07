%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc `wslogi' log message
%% @private

-module(wslogi_msg).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------

-export([
         get/2,
         put/2
        ]).

-export_type([
              log_level/0,
              message/0,
              wslogi_msg/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------

-type log_level()  :: 0..511.
-type message()    :: binary().
%% Log message entered by the user.
-type wslogi_msg() :: {'$wslogi', log_level(), message()}.
%% Log message format at `wslogi'.

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
put(LogLevel, Msg) when LogLevel > 0 andalso LogLevel < 512 andalso is_binary(Msg) ->
    {'$wslogi', LogLevel, Msg};
put(LogLevel, Msg) ->
    error(badarg, [LogLevel, Msg]).

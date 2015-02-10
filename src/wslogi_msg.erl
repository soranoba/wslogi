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
         unwatch/0,
         message_to_binary/1
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
-type message()    :: {[{wslogi:header_key(), wslogi:header_value()}], {module(), Line :: non_neg_integer()}, Msg :: binary()}.
%% Log message entered by the user.
-type wslogi_msg() :: {'$wslogi', log_level(), message()}.
%% Log message format at `wslogi'.

-define(GPROC_NAME(Path), {p, l, ?GPROC_KEY(Path)}).
-define(GPROC_KEY(Path),  {wslogi, Path}).

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
put(LogLevel, Msg) when LogLevel > ?NONE andalso LogLevel =< ?MAX_LEVEL ->
    {'$wslogi', LogLevel, Msg};
put(LogLevel, Msg) ->
    error(badarg, [LogLevel, Msg]).

%% @doc Send a log message
-spec send(log_level(), Path :: file:filename(), io:format(), Args :: [term()]) -> ok.
send(LogLevel, Path, Format, Args) ->
    MsgBin = list_to_binary(io_lib:format(Format, Args)),
    Msg    = binary_to_message(MsgBin),
    %% MEMO: "foo/bar/" -> <<"/foo/bar">>
    _   = gproc:send(?GPROC_NAME(filename:join([<<>>, Path])), ?MODULE:put(LogLevel, Msg)),
    ok.

%% @doc Monitor the messages that are sent to the path.
-spec watch(PathInfo :: [binary()]) -> ok.
watch(PathInfo) ->
    L    = watch([<<"/">>], PathInfo),
    true = gproc:mreg(p, l, [{?GPROC_KEY(K), gproc:default(?GPROC_NAME(K))} || K <- L]),
    ok.

%% @see watch/1
-spec watch([binary()], [binary()]) -> [binary()].
watch([M | MonitorPath], [H | T]) ->
    watch([filename:join([M, H]), M | MonitorPath], T);
watch(MonitorPath, _) ->
    MonitorPath.

%% @doc Calling process unmonitor the message that are send to the path.
-spec unwatch() -> ok.
unwatch() ->
    gproc:goodbye().

-spec message_to_binary(message()) -> binary().
message_to_binary({Headers, {Mod, Line}, Msg}) ->
    HeaderBin = case list_to_binary([ [$, , " " | io_lib:format("~p: ~p", [X, Y])] || {X, Y} <- Headers]) of
                    <<_, _, H/binary>> -> H;
                    _                  -> <<>>
                end,
    LineInfo = list_to_binary(io_lib:format("~p:~p", [Mod, Line])),
    <<LineInfo/binary, " [", HeaderBin/binary, "] ", Msg/binary, "\n">>.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc `binary/0' to `message/0'
-spec binary_to_message(Msg :: binary()) -> message().
binary_to_message(Msg) when is_binary(Msg) ->
    try
        exit(get_trace)
    catch
        exit:get_trace ->
            StackTrace  = erlang:get_stacktrace(),
            {Mod, Line} = lists:foldl(fun({Mod0, _, _, [{file, _}, {line, L}]}, []) when Mod0 =/= ?MODULE -> {Mod0, L};
                                         (_, Acc) -> Acc
                                      end, [], StackTrace),
            {wslogi_server:get_headers(), {Mod, Line}, Msg}
    end.

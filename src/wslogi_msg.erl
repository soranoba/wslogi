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
         get_timestamp/1,
         get_headers/1,
         get_line/1,
         get_message_binary/1,

         get/2,
         put/2,
         send/4,
         watch/1,
         unwatch/0,
         message_to_binary/1,
         binary_to_message/1
        ]).

-export_type([
              log_level/0,
              headers/0,
              line/0,
              message/0,
              wslogi_msg/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types & Macros
%%----------------------------------------------------------------------------------------------------------------------

-type log_level()  :: 0..511.

-type headers()    :: [{wslogi:header_key(), wslogi:header_value()}].
-type line()       :: {module(), Line :: non_neg_integer()}.
-type message()    :: {calendar:datetime(), headers(), line(), Msg :: binary()}.
-type wslogi_msg() :: {'$wslogi', log_level(), message()}.

-define(GPROC_NAME(Path), {p, l, ?GPROC_KEY(Path)}).
-define(GPROC_KEY(Path),  {wslogi, Path}).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Retrieve the `erlang:timestamp/0' from the `message/0'
-spec get_timestamp(message()) -> erlang:timestamp().
get_timestamp({T, _, _, _}) -> T.

%% @doc Retrieve the `headers/0' from the `message/0'
-spec get_headers(message()) -> headers().
get_headers({_, H, _, _}) -> H.

%% @doc Retrieve the `line/0' from the `message/0'
-spec get_line(message()) -> line().
get_line({_, _, L, _}) -> L.

%% @doc Retrieve the message (`binary/0') from the `message/0'
-spec get_message_binary(message()) -> Msg :: binary().
get_message_binary({_, _, _, M}) -> M.

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

%% @doc `message/0' to `binary/0'
-spec message_to_binary(message()) -> binary().
message_to_binary({{{Ye, Mo, Da}, {Ho, Mi, Se}}, Headers, {Mod, Line}, Msg}) ->
    Datetime  = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Ye, Mo, Da, Ho, Mi, Se]),
    HeaderBin = string:join([io_lib:format("~p: ~p", [X, Y]) || {X, Y} <- Headers], ", "),
    LineInfo  = io_lib:format("~p:~p", [Mod, Line]),
    iolist_to_binary([Datetime, " ", LineInfo, " [", HeaderBin, "] ", Msg, "\n"]).

%% @doc `binary/0' to `message/0'
-spec binary_to_message(Msg :: binary()) -> message().
binary_to_message(Msg) when is_binary(Msg) ->
    try
        throw(get_trace)
    catch
        throw:get_trace ->
            StackTrace  = erlang:get_stacktrace(),
            {Mod, Line} = lists:foldl(fun({Mod0, _, _, [{file, _}, {line, L}]}, []) when Mod0 =/= ?MODULE -> {Mod0, L};
                                         (_, Acc) -> Acc
                                      end, [], StackTrace),
            {calendar:local_time(), wslogi_server:get_headers(), {Mod, Line}, Msg}
    end.

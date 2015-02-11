%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc cowboy websocket handler
%% @private
-module(wslogi_ws_handler).
-include("wslogi_internal.hrl").

-behaviour(cowboy_websocket_handler).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%----------------------------------------------------------------------------------------------------------------------
%% 'cowboy_websocket_handler' Callback API
%%----------------------------------------------------------------------------------------------------------------------

-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records
%%----------------------------------------------------------------------------------------------------------------------

-type filter() :: binary() | {Key :: wslogi:header_key(), binary()}.

-record(?MODULE,
        {
          log_level = ?MAX_LEVEL :: wslogi_msg:log_level(),
          ip                     :: inet:ip_address(),
          do_watching = false    :: boolean(),
          filters = []           :: [filter()]
        }).

-type state() :: #?MODULE{}.

-define(COND(Cond, TValue, FValue),
        case Cond of true -> TValue; false -> FValue end).

%%----------------------------------------------------------------------------------------------------------------------
%% 'cowboy_websocket_handler' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req0, _Opts) ->
    {{Ip, _},  Req1} = cowboy_req:peer(Req0),
    {PathInfo, Req2} = cowboy_req:path_info(Req1),

    ok = wslogi_msg:watch(PathInfo),
    {ok, Req2, #?MODULE{ip = Ip}}.

%% @doc This function will call when received a message using websocket.
%% @private
websocket_handle({text, Msg}, Req, State0) ->
    try wsclient_command(Msg, State0) of
        {ok, Response, State} -> {reply, {text, Response}, Req, State};
        {error, State}        -> {ok, Req, State}
    catch
        _:_ -> {ok, Req, State0}
    end;
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%% @doc This function will call when received a message.
%% @private
websocket_info(Info, Req, #?MODULE{do_watching = true, log_level = LogLevel, filters = Filters} = State) ->
    case wslogi_msg:get(LogLevel, Info) of
        {ok, Msg} ->
            case do_show_message(Filters, Msg) andalso wslogi_msg:message_to_binary(Msg) of
                false  -> {ok, Req, State};
                MsgBin -> {reply, {text, MsgBin}, State}
            end;
        error ->
            {ok, Req, State}
    end;
websocket_info(_, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% Command Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Return a response in accordance with the command.
-spec wsclient_command(Args :: binary(), state()) -> {ok, Response :: binary(), state()} | {error, state()}.
wsclient_command(Args, State) ->
    case shift(Args) of
        {<<"help">>,   _}    -> wsclient_help(State);
        {<<"start">>,  _}    -> wsclient_start(State);
        {<<"stop">>,   _}    -> wsclient_stop(State);
        {<<"level">>,  Rest} -> wsclient_level(Rest, State);
        {<<"filter">>, Rest} -> wsclient_filter(Rest, State);
        _                    -> {error, State}
    end.

%% @doc `help'
-spec wsclient_help(state()) -> {ok, Response :: binary(), state()}.
wsclient_help(State) ->
    {ok, wslogi_help:get_help(), State}.

%% @doc `start'
-spec wsclient_start(state()) -> {ok, Response :: binary(), state()}.
wsclient_start(State) ->
    {ok, <<"==> starting ....\n">>, State#?MODULE{do_watching = true}}.

%% @doc `stop'
-spec wsclient_stop(state()) -> {ok, Response :: binary(), state()}.
wsclient_stop(State) ->
    {ok, <<"\n==> stopping .... \n">>, State#?MODULE{do_watching = false}}.

%% @doc `level'
-spec wsclient_level(Args :: binary(), state()) -> {ok, Response :: binary(), state()}.
wsclient_level(Args, State) ->
    case shift(Args) of
        {<<"list">>, _} -> wsclient_level_list(State);
        {<<"show">>, _} -> wsclient_level_show(State);
        {<<N, _/binary>> = Number, _} when N >= $0, N =< $9 ->
            wsclient_level_number(binary_to_integer(Number), State);
        _ ->
            wsclient_level_bit(Args, State)
    end.

%% @doc `filter'
-spec wsclient_filter(Args :: binary(), state()) -> {ok, Response :: binary(), state()}.
wsclient_filter(Args, State) ->
    case shift(Args) of
        {<<"add">>,    Rest} -> wsclient_filter_add(Rest, State);
        {<<"remove">>, Rest} -> wsclient_filter_remove(Rest, State);
        {<<"show">>,   _}    -> wsclient_filter_show(State);
        {<<"header">>, Rest} -> wsclient_filter_header(Rest, State)
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'level' Command Functinos
%%----------------------------------------------------------------------------------------------------------------------

%% @doc `level list'
-spec wsclient_level_list(state()) -> {ok, Response :: binary(), state()}.
wsclient_level_list(State) ->
    {ok, <<"==> Available log level:\n",
           "  > none,    emergency, alert, critical, error\n",
           "  > warning, notice,    info,  verbose,  debug\n">>, State}.

%% @doc `level show'
-spec wsclient_level_show(state()) -> {ok, Response :: binary(), state()}.
wsclient_level_show(#?MODULE{log_level = LogLevel} = State) ->
    None      = ?COND(LogLevel =:=  ?NONE,          <<"none">>,      <<"    ">>     ),
    Emergency = ?COND(LogLevel band ?EMERGENCY > 0, <<"emergency">>, <<"         ">>),
    Alert     = ?COND(LogLevel band ?ALERT     > 0, <<"alert">>,     <<"     ">>    ),
    Critical  = ?COND(LogLevel band ?CRITICAL  > 0, <<"critical">>,  <<"        ">> ),
    Error     = ?COND(LogLevel band ?ERROR     > 0, <<"error">>,     <<"     ">>    ),
    Warning   = ?COND(LogLevel band ?WARNING   > 0, <<"warning">>,   <<"       ">>  ),
    Notice    = ?COND(LogLevel band ?NOTICE    > 0, <<"notice">>,    <<"      ">>   ),
    Info      = ?COND(LogLevel band ?INFO      > 0, <<"info">>,      <<"    ">>     ),
    Verbose   = ?COND(LogLevel band ?VERBOSE   > 0, <<"verbose">>,   <<"       ">>  ),
    Debug     = ?COND(LogLevel band ?DEBUG     > 0, <<"debug">>,     <<"     ">>    ),
    {ok, <<"==> Current log level:\n",
           "  > ", None/binary, "   , ", Emergency/binary, ", ", Alert/binary, ", ", Critical/binary, ", ", Error/binary, "\n",
           "  > ", Warning/binary, ", ", Notice/binary, "   , ", Info/binary, " , ", Verbose/binary, " , ", Debug/binary, "\n">>, State}.

%% @doc `level <number>'
-spec wsclient_level_number(non_neg_integer(), state()) -> {ok, Response :: binary(), state()}.
wsclient_level_number(Number, State0)->
    State1 = State0#?MODULE{log_level = (1 bsl Number - 1) band ?MAX_LEVEL},
    {ok, CurrentLogLevel, State2} = wsclient_level_show(State1),
    {ok, <<"==> Update log level\n", CurrentLogLevel/binary>>, State2}.

%% @doc `level [+ | -]<level> [ [+ | -]<level> ]'
-spec wsclient_level_bit(Args :: binary(), state()) -> {ok, Response :: binary(), state()}.
wsclient_level_bit(Args, #?MODULE{log_level = LogLevel0} = State0) ->
    case wsclient_level_bit_impl(Args, LogLevel0) of
        {error, Option} -> {ok, <<"[ERROR] ", Option/binary, "\n">>, State0};
        {ok, LogLevel}  ->
            {ok, CurrentLogLevel, State1} = wsclient_level_show(State0#?MODULE{log_level = LogLevel}),
            {ok, <<"==> Update log level\n", CurrentLogLevel/binary>>, State1}
    end.

%% @see wsclient_level_bit/2
-spec wsclient_level_bit_impl(Args :: binary(), wslogi_msg:log_level()) -> {ok, wslogi_msg:log_level()} | {error, BarArg :: binary()}.
wsclient_level_bit_impl(Args, LogLevel0) ->
    case shift(Args) of
        {undefined, _}   -> {ok, LogLevel0};
        {NewLevel, Rest} ->
            case update_level(NewLevel, LogLevel0) of
                {ok, LogLevel} -> wsclient_level_bit_impl(Rest, LogLevel);
                Err            -> Err
            end
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'filter' Command Functinos
%%----------------------------------------------------------------------------------------------------------------------

%% @doc `filter add'
-spec wsclient_filter_add(Arg :: binary(), state()) -> {ok, Response :: binary(), state()}.
wsclient_filter_add(Arg, #?MODULE{filters = Filters} = State0) ->
    Value0 = clean_tail_binary(Arg),
    Value1 = case Value0 of
                 <<X, _, _/binary>> when X =:= $\"; X =:= $\' ->
                     ?COND(binary:last(Value0) =:= X, binary:part(Value0, 1, byte_size(Value0) - 2), Value0);
                 _ ->
                     Value0
             end,
    {ok, CurrentFilters, State1} = wsclient_filter_show(State0#?MODULE{filters = [Value1 | Filters]}),
    {ok, <<"==> Update filters\n", CurrentFilters/binary>>, State1}.

%% @doc `filter show'
-spec wsclient_filter_show(state()) -> {ok, Response :: binary(), state()}.
wsclient_filter_show(#?MODULE{filters = Filters} = State) ->
    Response = iolist_to_binary([<<"==> Current filters:\n">> | filters_to_iolist(Filters)]),
    {ok, Response, State}.

%% @doc `filter remove'
-spec wsclient_filter_remove(Args :: binary(), state()) -> {ok, Response :: binary(), state()}.
wsclient_filter_remove(Args, #?MODULE{filters = Filters0} = State0) ->
    Filters = wsclient_filter_remove_impl(Args, Filters0),
    {ok, CurrentFilters, State} = wsclient_filter_show(State0#?MODULE{filters = Filters}),
    {ok, <<"==> Update filters\n", CurrentFilters/binary>>, State}.

%% @see wsclient_filter_remove/2
-spec wsclient_filter_remove_impl(Args :: binary(), Filters :: [filter()]) -> [filter()].
wsclient_filter_remove_impl(Args, Filters) ->
    case shift(Args) of
        {undefined, _} ->
            lists:filter(fun(X) -> X =/= [] end, Filters);
        {NumBin, Rest} ->
            try
                Number = binary_to_integer(NumBin),
                wsclient_filter_remove_impl(Rest, update_nth(Number, Filters, []))
            catch
                _:_ -> wsclient_filter_remove_impl(Rest, Filters)
            end
    end.

%% @doc `filter header'
-spec wsclient_filter_header(Args :: binary(), state()) -> {ok, Response :: binary(), state()}.
wsclient_filter_header(Args, #?MODULE{ip = Ip, filters = Filters} = State0) ->
    case shift(Args) of
        {KeyBin, Rest} when is_binary(KeyBin) ->
            try
                Key   = binary_to_existing_atom(KeyBin, utf8),
                Value = case clean_tail_binary(Rest) of
                            <<>> when KeyBin =:= <<"ip">> -> to_binary(Ip);
                            V                             -> V
                        end,
                {ok, CurrentFilters, State1}
                    =  wsclient_filter_show(State0#?MODULE{filters = [{Key, Value} | Filters]}),
                {ok, <<"==> Update filters\n", CurrentFilters/binary>>, State1}
            catch
                _:_ -> {ok, <<"[ERROR] Undefined key: ", KeyBin/binary, "\n">>, State0}
            end
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc It is determine whether to show the message.
-spec do_show_message([filter()], wslogi_msg:message()) -> boolean().
do_show_message(Filters, Message) -> do_show_message_impl(Filters, Message).

%% @see do_show_message/2
-spec do_show_message_impl([filter()], wslogi_msg:message()) -> boolean().
do_show_message_impl([], _) ->
    true;
do_show_message_impl([{Key, Value} | T], Message) ->
    Headers = wslogi_msg:get_headers(Message),
    case proplists:get_value(Key, Headers, undefined) of
        undefined -> false;
        V         ->
            case lists:flatten(io_lib:format("~p", [V])) =:= binary_to_list(Value) of
                true  -> do_show_message_impl(T, Message);
                false -> false
            end
    end;
do_show_message_impl([H | T], Message) ->
    MsgBin = wslogi_msg:get_message_binary(Message),
    case re:run(MsgBin, H) of
        {match, _} -> do_show_message_impl(T, Message);
        nomatch    -> false
    end.

%% @doc Remove the `"\n"' and `" "' from the end.
-spec clean_tail_binary(binary()) -> binary().
clean_tail_binary(Bin) ->
    clean_tail_binary_impl(<<>>, <<>>, Bin).

%% @see clean_tail_binary/1
-spec clean_tail_binary_impl(binary(), binary(), binary()) -> binary().
clean_tail_binary_impl(H, _, <<>>) ->
    H;
clean_tail_binary_impl(H, M, <<S, T/binary>>) when S =:= $\n; S =:= $  ->
    clean_tail_binary_impl(H, <<M/binary, S>>, T);
clean_tail_binary_impl(H, M, <<S, T/binary>>) ->
    clean_tail_binary_impl(<<H/binary, M/binary, S>>, <<>>, T).

%% @doc Update log level.
%%
%% LevelArg  : `[-]<level> [ [-]<level> ]' <br />
%% `<level>' : `<<"none">>, <<"debug">> ...' (Please see priv/help.md)
-spec update_level(LevelArg :: binary(), CurrentLevel :: wslogi_msg:log_level()) -> {ok, wslogi_msg:log_level()} | {error, BadArg :: binary()}.
update_level(<<"-", NewLevel/binary>>, CurrentLevel) ->
    case level_number(NewLevel) of
        {ok, N} -> {ok, (CurrentLevel band (bnot N)) band ?MAX_LEVEL};
        error   -> {error, NewLevel}
    end;
update_level(<<"+", NewLevel/binary>>, CurrentLevel) ->
    case level_number(NewLevel) of
        {ok, N} -> {ok, (CurrentLevel bor N) band ?MAX_LEVEL};
        error   -> {error, NewLevel}
    end;
update_level(NewLevel, _) ->
    case level_number(NewLevel) of
        {ok, N} -> {ok, N band ?MAX_LEVEL};
        error   -> {error, NewLevel}
    end.

%% @doc Convert to number from binary that represent the log level.
-spec level_number(LogLevel :: binary()) -> {ok, LogLevelNumber :: non_neg_integer()} | error.
level_number(<<"none">>)      -> {ok, ?NONE};
level_number(<<"emergency">>) -> {ok, ?EMERGENCY};
level_number(<<"alert">>)     -> {ok, ?ALERT};
level_number(<<"critical">>)  -> {ok, ?CRITICAL};
level_number(<<"error">>)     -> {ok, ?ERROR};
level_number(<<"warning">>)   -> {ok, ?WARNING};
level_number(<<"notice">>)    -> {ok, ?NOTICE};
level_number(<<"info">>)      -> {ok, ?INFO};
level_number(<<"verbose">>)   -> {ok, ?VERBOSE};
level_number(<<"debug">>)     -> {ok, ?DEBUG};
level_number(_)               -> error.

%% @doc Update the N-th value of list.
-spec update_nth(Index :: pos_integer(), Source :: list(), UpdateValue :: term()) -> list().
update_nth(Index, Source, UpdateValue) when Index > 0 ->
    update_nth_impl(Index - 1, Source, UpdateValue, []).

%% @see update_nth/3
-spec update_nth_impl(ShiftCount :: non_neg_integer(), Source :: list(), UpdateValue :: term(), ShiftList :: list()) -> list().
update_nth_impl(ShiftCount, [H | T], UpdateValue, ShiftList) when ShiftCount > 0 ->
    update_nth_impl(ShiftCount - 1, T, UpdateValue, [H | ShiftList]);
update_nth_impl(_, [_ | T], UpdateValue, ShiftList) ->
    lists:reverse(ShiftList, [UpdateValue | T]).

%% @doc Generate the response, command that is `filter show'.
-spec filters_to_iolist([filter()]) -> iodata().
filters_to_iolist(Filters) ->
    filters_to_iolist_impl(Filters, 1, []).

-spec filters_to_iolist_impl([filter()], non_neg_integer(), [filter()]) -> iodata().
filters_to_iolist_impl([], _, Result) ->
    lists:reverse(Result);
filters_to_iolist_impl([{K, V} | T], Num, Result) ->
    filters_to_iolist_impl([[to_binary(K), " ", V] | T], Num, Result);
filters_to_iolist_impl([H | T], Num, Result) ->
    filters_to_iolist_impl(T, Num + 1, [ ["  > ", integer_to_binary(Num), <<" : ">>, H, <<"\n">>] | Result]).

%% @doc Take out the one from the space-separated arguments.
-spec shift(binary()) -> {Arg :: binary() | undefined, Rest :: binary()}.
shift(Bin) ->
    case binary:split(Bin, [<<" ">>, <<"\n">>], [trim]) of
        []           -> {undefined, <<>>};
        [Arg ]       -> {Arg,       <<>>};
        [<<>>, Rest] -> shift(Rest);
        [Arg,  Rest] -> {Arg,       Rest}
    end.

%% @doc `term/0' to `binary/0'
-spec to_binary(term()) -> binary().
to_binary(Term) ->
    list_to_binary(io_lib:format("~p", [Term])).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

-ifdef(TEST).

websocket_handle_test_() ->
    State0 = #?MODULE{ip = {192,168,0,1}},
    {setup,
     fun() ->
             ok = meck:new(wslogi_help),
             ok = meck:expect(wslogi_help, get_help, 0, <<"HELP">>)
     end,
     fun(_) ->
             _ = meck:unload()
     end,
     [
      {"It can use the command help",
       ?_assertMatch({reply, {text, <<"HELP">>}, _, State0},
                     websocket_handle({text, <<"help \n">>}, req, State0))},
      {"It can use the command start",
       ?_assertMatch({reply, {text, _}, _, #?MODULE{do_watching = true}},
                     websocket_handle({text, <<"start">>}, req, State0))},
      {"It can use the command stop",
       ?_assertMatch({reply, {text, _}, _, #?MODULE{do_watching = false}},
                     websocket_handle({text, <<"stop">>}, req, State0#?MODULE{do_watching = true}))},
      {"It can use the command level show",
       fun() ->
               State = State0#?MODULE{log_level = ?DEBUG bor ?INFO},
               {reply, {text, Msg}, _, State} = websocket_handle({text, <<"level show">>}, req, State),
               ?assertMatch({_, _},  binary:match(Msg, <<"debug">>)),
               ?assertMatch({_, _},  binary:match(Msg, <<"info">>)),
               ?assertEqual(nomatch, binary:match(Msg, <<"alert">>))
       end},
      {"level show: It only print the 'none' when the level is not set",
       fun() ->
               State1 = State0#?MODULE{log_level = ?NONE bor ?DEBUG},
               {reply, {text, Msg1}, _, State1} = websocket_handle({text, <<"level show">>}, req, State1),
               State2 = State0#?MODULE{log_level = ?NONE},
               {reply, {text, Msg2}, _, State2} = websocket_handle({text, <<"level show">>}, req, State2),
               ?assertMatch(nomatch, binary:match(Msg1, <<"none">>)),
               ?assertMatch({_, _},  binary:match(Msg2, <<"none">>))
       end},
      {"It can use the command level list",
       ?_assertMatch({reply, {text, <<"==> Available log level:\n", _/binary>>}, _, State0},
                     websocket_handle({text, <<"level list">>}, req, State0))},
      {"It can use the command level <number>",
       fun() ->
               State = State0#?MODULE{log_level = ?DEBUG},
               ?assertMatch({reply, {text, _}, _, #?MODULE{log_level = 1 bsl 3 - 1}},
                            websocket_handle({text, <<"level 3">>}, req, State))
       end},
      {"level <number>: bad arguments",
       ?_assertMatch({ok, _, _}, websocket_handle({text, <<"level 3hoge">>}, req, State0))},
      {"level <number> <number>: The second arg is ignored",
       fun() ->
               State = State0#?MODULE{log_level = ?DEBUG},
               ?assertMatch({reply, {text, _}, _, #?MODULE{log_level = 1 bsl 3 - 1}},
                            websocket_handle({text, <<"level 3 1">>}, req, State))
       end},
      {"level <level>: Overwrite the level",
       fun() ->
               State = State0#?MODULE{log_level = ?DEBUG},
               ?assertMatch({reply, {text, _}, _, #?MODULE{log_level = ?INFO}},
                            websocket_handle({text, <<"level info">>}, req, State))
       end},
      {"level +<level>: Add a level",
       fun() ->
               State = State0#?MODULE{log_level = ?DEBUG},
               ?assertMatch({reply, {text, _}, _, #?MODULE{log_level = ?INFO bor ?DEBUG}},
                            websocket_handle({text, <<"level +info">>}, req, State))
       end},
      {"level -<level>: Subtract a level",
       fun() ->
               State = State0#?MODULE{log_level = ?INFO bor ?DEBUG},
               ?assertMatch({reply, {text, _}, _, #?MODULE{log_level = ?INFO}},
                            websocket_handle({text, <<"level -debug">>}, req, State))
       end},
      {"multiple level",
       fun() ->
               State = State0#?MODULE{log_level = ?INFO},
               ?assertMatch({reply, {text, _}, _, #?MODULE{log_level = ?DEBUG}},
                            websocket_handle({text, <<"level verbose -verbose +debug">>}, req, State))
       end},
      {"level [+|-]<level>: If it return the bad arguments, level is not changed.",
       fun() ->
               {reply, {text, Msg}, _, State0} = websocket_handle({text, <<"level debug +hoge">>}, req, State0),
               {reply, {text, _  }, _, State0} = websocket_handle({text, <<"level +debug hoge">>}, req, State0),
               {reply, {text, _  }, _, State0} = websocket_handle({text, <<"level debug -hoge">>}, req, State0),
               ?assertMatch({_, _}, binary:match(Msg, <<"ERROR">>))
       end},
      {"filter add",
       ?_assertMatch({reply, {text, _}, _, #?MODULE{filters = [<<"hoge">>]}},
                     websocket_handle({text, <<"filter add hoge">>}, req, State0))},
      {"filter add: Enclosed in double quotes",
       ?_assertMatch({reply, {text, _}, _, #?MODULE{filters = [<<"hoge \"fugo\"">>]}},
                     websocket_handle({text, <<"filter add \"hoge \"fugo\"\"">>}, req, State0))},
      {"filter remove",
       ?_assertMatch({reply, {text, _}, _, #?MODULE{filters = [<<"two">>]}},
                     websocket_handle({text, <<"filter remove 1 3">>}, req, State0#?MODULE{filters = [<<"one">>, <<"two">>, <<"three">>]}))},
      {"filter remove: It ignore, if it does not exist",
       ?_assertMatch({reply, {text, _}, _, #?MODULE{filters = [<<"two">>]}},
                     websocket_handle({text, <<"filter remove 1 4">>}, req, State0#?MODULE{filters = [<<"one">>, <<"two">>]}))},
      {"filter show",
       fun() ->
               {reply, {text, Msg}, _, _} = websocket_handle({text, <<"filter show">>}, req, State0#?MODULE{filters = [<<"hoge">>]}),
               ?assertMatch({_, _}, binary:match(Msg, <<"hoge">>))
       end},
      {"filter header: undefined key",
       fun() ->
               {reply, {text, Msg}, _, State0} = websocket_handle({text, <<"filter header asdoifja hoge">>}, req, State0),
               ?assertMatch({_, _}, binary:match(Msg, <<"ERROR">>))
       end},
      {"filter header",
       fun() ->
               _ = ip,
               ?assertMatch({reply, {text, _}, _, #?MODULE{filters = [{ip, <<"{192,168,0,1}">>}]}},
                            websocket_handle({text, <<"filter header ip {192,168,0,1}">>}, req, State0))
       end},
      {"filter header ip: Special case",
       fun() ->
               _ = ip,
               ?assertMatch({reply, {text, _}, _, #?MODULE{filters = [{ip, <<"{192,168,0,1}">>}]}},
                            websocket_handle({text, <<"filter header ip">>}, req, State0))
       end},
      {"Undefined command",
       ?_assertMatch({ok, _, _}, websocket_handle({text, <<"hoge">>}, req, State0))}
     ]}.

websocket_info_test_() ->
    Msg    = {calendar:local_time(), [{ip, {192,168,0,1}}], {?MODULE, ?LINE}, <<"message">>},
    State0 = #?MODULE{
                 ip          = {192,168,0,1},
                 filters     = [],
                 do_watching = true,
                 log_level   = ?DEBUG
                },
    [
     {"filteling: no filter",
      ?_assertMatch({reply, _, _}, websocket_info(wslogi_msg:put(?DEBUG, Msg), req, State0))},
     {"filteling: log_level",
      ?_assertMatch({ok, _, _},    websocket_info(wslogi_msg:put(?INFO, Msg), req, State0))},
     {"filteling: using word (partial match)",
      ?_assertMatch({reply, _, _}, websocket_info(wslogi_msg:put(?DEBUG, Msg), req, State0#?MODULE{filters = [<<"ssa">>]}))},
     {"filteling: using word (not match)",
      ?_assertMatch({ok, _, _},    websocket_info(wslogi_msg:put(?DEBUG, Msg), req, State0#?MODULE{filters = [<<"hoge">>]}))},
     {"filteling: using header",
      ?_assertMatch({reply, _, _}, websocket_info(wslogi_msg:put(?DEBUG, Msg), req, State0#?MODULE{filters = [{ip, <<"{192,168,0,1}">>}]}))},
     {"filteling: using header (not exist this key in headers)",
      ?_assertMatch({ok, _, _},    websocket_info(wslogi_msg:put(?DEBUG, Msg), req, State0#?MODULE{filters = [{hoge, <<"fugo">>}]}))},
     {"filteling: usgin header (not match)",
      ?_assertMatch({ok, _, _},    websocket_info(wslogi_msg:put(?DEBUG, Msg), req, State0#?MODULE{filters = [{ip, <<"{127,0,0,1}">>}]}))},
     {"filteling: A and B...",
      ?_assertMatch({ok, _, _},    websocket_info(wslogi_msg:put(?DEBUG, Msg), req, State0#?MODULE{filters = [<<"hoge">>, {ip, <<"{192,168,0,1}">>}]}))},
     {"filteling: do_watching",
      ?_assertMatch({ok, _, _},    websocket_info(wslogi_msg:put(?DEBUG, Msg), req, State0#?MODULE{do_watching = false}))}
    ].

level_number_test_() ->
    [
     ?_assertEqual({ok, ?NONE},      level_number(<<"none">>)),
     ?_assertEqual({ok, ?EMERGENCY}, level_number(<<"emergency">>)),
     ?_assertEqual({ok, ?ALERT},     level_number(<<"alert">>)),
     ?_assertEqual({ok, ?CRITICAL},  level_number(<<"critical">>)),
     ?_assertEqual({ok, ?ERROR},     level_number(<<"error">>)),
     ?_assertEqual({ok, ?WARNING},   level_number(<<"warning">>)),
     ?_assertEqual({ok, ?NOTICE},    level_number(<<"notice">>)),
     ?_assertEqual({ok, ?INFO},      level_number(<<"info">>)),
     ?_assertEqual({ok, ?VERBOSE},   level_number(<<"verbose">>)),
     ?_assertEqual({ok, ?DEBUG},     level_number(<<"debug">>)),
     ?_assertEqual(error,            level_number(<<"hoge">>))
    ].

shift_test_() ->
    [
     {"It ignore the beginning of the blank",
      ?_assertEqual({<<"hoge">>, <<"fugo  ">>}, shift(<<"  hoge fugo  ">>))},
     {"It can split with a newline",
      ?_assertEqual({<<"hoge">>, <<"fugo">>},   shift(<<"hoge\nfugo">>))},
     {"It return the undefined when input is blank",
      ?_assertEqual({undefined,  <<>>},         shift(<<>>))},
     {"It return the undefined when input consists only space and newline",
      ?_assertEqual({undefined,  <<>>},         shift(<<"      ">>))}
    ].

update_nth_test_() ->
    [
     {"It can update N-th value",
      ?_assertEqual([4,2,3], update_nth(1, [1,2,3], 4))},
     {"It is an error if index is less than or equal to 0",
      ?_assertError(_,       update_nth(0, [1,2,3], 3))},
     {"It is an error if index is out of range",
      ?_assertError(_,       update_nth(4, [1,2,3], 3))}
    ].


-endif.

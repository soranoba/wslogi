%% coding: latin-1
%%
%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
-module(wslogi_msg_tests).
-include("wslogi_internal.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(Message, <<"message">>).
-define(Headers, [{ip, {192,168,0,1}}, {method, <<"GET">>}]).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

wslogi_msg_test_() ->
    {setup,
     fun()        -> {ok, Started} = application:ensure_all_started(gproc), Started end,
     fun(Started) -> [application:stop(X) || X <- Started] end,
     [
      get_and_put_test_main(),
      message_test_main(),
      watch_and_send_test_main(),
      message_to_binary_test_main()
     ]}.

get_and_put_test_main() ->
    [
     {"The put messages can be get",
      fun() ->
              Msg = wslogi_msg:put(?DEBUG, ?Message),
              ?assertEqual({ok, ?Message}, wslogi_msg:get(?DEBUG, Msg))
      end},
     {"It can filter at log level",
      fun() ->
              Msg = wslogi_msg:put(?DEBUG, ?Message),
              ?assertEqual(error,          wslogi_msg:get(?INFO, Msg)),
              ?assertEqual({ok, ?Message}, wslogi_msg:get(?INFO bor ?DEBUG, Msg))
      end},
     {"It is thrown error if the log level is out of range",
      fun() ->
              ?assertError(_, wslogi_msg:put(?MAX_LEVEL + 1, ?Message)),
              ?assertError(_, wslogi_msg:put(-1, ?Message))
      end}
    ].

message_test_main() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"It can retrieve the timestamp from the message",
       fun() ->
               ?assertMatch({{_,_,_}, {_,_,_}}, wslogi_msg:get_timestamp(wslogi_msg:binary_to_message(?Message)))
       end},
      {"It can retrieve the headers from the message",
       fun() ->
               ?assertEqual(?Headers, wslogi_msg:get_headers(wslogi_msg:binary_to_message(?Message)))
       end},
      {"It can retrieve the headers from the message",
       fun() ->
               ?assertEqual({?MODULE, ?LINE}, wslogi_msg:get_line(wslogi_msg:binary_to_message(?Message)))
       end},
      {"It can retrieve the message (binary) from the message",
       fun() ->
               ?assertEqual(?Message, wslogi_msg:get_message_binary(wslogi_msg:binary_to_message(?Message)))
       end}
     ]}.

watch_and_send_test_main() ->
    {foreach, local,
     fun setup/0,
     fun teardown/1,
     [
      {"It can get the message of the specified path",
       fun() ->
               ?assertEqual(ok, wslogi_msg:send(?DEBUG, "/hoge/fugo", "~p", ["message"])),
               receive
                   Msg ->
                       {ok, M} = wslogi_msg:get(?DEBUG, Msg),
                       ?assertEqual(<<"\"message\"">>, wslogi_msg:get_message_binary(M))
               after 50 ->
                       ?assert(timeout)
               end
       end},
      {"It can get the message of the parent path",
       fun() ->
               ?assertEqual(ok, wslogi_msg:send(?DEBUG, "/", "hoge", [])),
               receive
                   Msg ->
                       {ok, M} = wslogi_msg:get(?DEBUG, Msg),
                       ?assertEqual(<<"hoge">>, wslogi_msg:get_message_binary(M))
               after 50 ->
                       ?assert(timeout)
               end
       end},
      {"It cannot get the message of the child path",
       fun() ->
               ?assertEqual(ok, wslogi_msg:send(?DEBUG, "/hoge/fugo/child", "child", [])),
               ?assertEqual(ok, wslogi_msg:send(?DEBUG, "/hoge/fugo", "specified", [])),
               receive
                   Msg ->
                       {ok, M} = wslogi_msg:get(?DEBUG, Msg),
                       ?assertEqual(<<"specified">>, wslogi_msg:get_message_binary(M))
               after 50 ->
                       ?assert(timeout)
               end
       end},
      {"All can be retrieved if they point to the same path",
       fun() ->
               ?assertEqual(ok, wslogi_msg:send(?DEBUG, "hoge/fugo",   "hoge", [])),
               ?assertEqual(ok, wslogi_msg:send(?DEBUG, "hoge/fugo/",  "hoge", [])),
               ?assertEqual(ok, wslogi_msg:send(?DEBUG, "/hoge/fugo/", "hoge", [])),
               [begin
                    receive
                        Msg ->
                            {ok, M} = wslogi_msg:get(?DEBUG, Msg),
                            ?assertEqual(<<"hoge">>, wslogi_msg:get_message_binary(M))
                    after 50 ->
                            ?assert(timeout)
                    end
                end || _ <- lists:seq(1, 3)]
       end}
      ]}.

message_to_binary_test_main() ->
    [
     {"Check the format (1)",
      fun() ->
              Msg = {{{2015,2,11}, {14,1,30}}, [{method, <<"GET">>}, {test, true}], {wslogi_msg, 120}, <<"message">>},
              ?assertEqual(
                 <<"2015-02-11 14:01:30 wslogi_msg:120 [method: <<\"GET\">>, test: true] message\n">>,
                 wslogi_msg:message_to_binary(Msg)
                )
      end},
     {"Check the format (2)",
      fun() ->
              Msg = {{{2015,2,11}, {14,1,30}}, [], {wslogi_msg, 120}, <<"message">>},
              ?assertEqual(
                 <<"2015-02-11 14:01:30 wslogi_msg:120 [] message\n">>,
                 wslogi_msg:message_to_binary(Msg)
                )
      end}
    ].

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

-spec setup() -> ok.
setup() ->
    ok = wslogi_msg:watch([<<"hoge">>, <<"fugo">>]),
    ok = meck:new(wslogi_server),
    ok = meck:expect(wslogi_server, get_headers, 0, ?Headers).

-spec teardown(term()) -> ok.
teardown(_) ->
    _  = meck:unload(),
    ok = wslogi_msg:unwatch().

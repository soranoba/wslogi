%% coding: latin-1
%%
%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
-module(wslogi_msg_tests).
-include("wslogi_internal.hrl").
-include_lib("eunit/include/eunit.hrl").

-on_load(init/0).
-define(Message,   <<"message">>).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

get_and_put_test_() ->
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
      end},
     {"It is thrown error if the message isn't a binary",
      fun() ->
              ?assertError(_, wslogi_msg:put(?DEBUG, "message"))
      end}
    ].

watch_and_send_test_() ->
    {foreach, local,
     fun()  -> ?assertEqual(ok, wslogi_msg:watch([<<"hoge">>, <<"fugo">>])) end,
     fun(_) -> ?assertEqual(ok, wslogi_msg:unwatch()) end,
     [
      {"It can get the message of the specified path",
       fun() ->
               ?assertEqual(ok, wslogi_msg:send(?DEBUG, "/hoge/fugo", "~p", ["message"])),
               receive
                   Msg -> ?assertEqual({ok, <<"\"message\"">>}, wslogi_msg:get(?DEBUG, Msg))
               after 50 ->
                       ?assert(timeout)
               end
       end},
      {"It can get the message of the parent path",
       fun() ->
               ?assertEqual(ok, wslogi_msg:send(?DEBUG, "/", "hoge", [])),
               receive
                   Msg -> ?assertEqual({ok, <<"hoge">>}, wslogi_msg:get(?DEBUG, Msg))
               after 50 ->
                       ?assert(timeout)
               end
       end},
      {"It cannot get the message of the child path",
       fun() ->
               ?assertEqual(ok, wslogi_msg:send(?DEBUG, "/hoge/fugo/child", "child", [])),
               ?assertEqual(ok, wslogi_msg:send(?DEBUG, "/hoge/fugo", "specified", [])),
               receive
                   Msg -> ?assertEqual({ok, <<"specified">>}, wslogi_msg:get(?DEBUG, Msg))
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
                        Msg -> ?assertEqual({ok, <<"hoge">>}, wslogi_msg:get(?DEBUG, Msg))
                    after 50 ->
                            ?assert(timeout)
                    end
                end || _ <- lists:seq(1, 3)]
       end}
      ]}.

-spec init() -> ok.
init() ->
    _ = application:ensure_all_started(gproc),
    ok.

%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
-module(wslogi_example_handler).

-behaviour(cowboy_http_handler).

%%----------------------------------------------------------------------------------------------------------------------
%% 'cowboy_http_handler' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/3, handle/2, terminate/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------------------------------------------------------

-record(?MODULE,
        {
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% 'cowboy_http_handler' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
init({_, http}, Req0, _) ->
    {{Ip, _Port}, Req1} = cowboy_req:peer(Req0),
    {Method,      Req2} = cowboy_req:method(Req1),

    ok = wslogi:set_headers([{ip, Ip}, {method, Method}]),
    _  = wslogi:info("starting handler... (method = ~p)", [Method]),
    {ok, Req2, #?MODULE{}}.

%% @private
handle(Req, State) ->
    {ok, Response} = cowboy_req:reply(200, [], <<"Hello World\n">>, Req),
    _  = wslogi:debug("reply: ~p", [Response]),
    {ok, Response, State}.

%% @private
terminate(_Reason, _Req, _State) ->
    ok.

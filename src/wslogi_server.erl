%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc This server that keep the header of each process.
%% @private
-module(wslogi_server).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/0,
         set_headers/1,
         get_headers/0,
         delete_headers/1,
         clear_headers/0
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types & Macros & Records
%%----------------------------------------------------------------------------------------------------------------------

-type key()   :: wslogi:header_key().
-type value() :: wslogi:header_value().

-record(?MODULE,
        {
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Start the wslogi_server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @see wslogi:set_headers/1
-spec set_headers([{key(), value()}]) -> ok.
set_headers(KVs) ->
    gen_server:call(?MODULE, {set_headers, KVs}).

%% @see wslogi:get_headers/0
-spec get_headers() -> [{key(), value()}].
get_headers() ->
    case ets:lookup(?MODULE, self()) of
        [{_, V}] -> V;
        []       -> []
    end.

%% @see wslogi:delete_headers/1
-spec delete_headers([key()]) -> ok.
delete_headers(Keys) ->
    gen_server:call(?MODULE, {delete_headers, Keys}).

%% @see wslogi:clear_headers/0
-spec clear_headers() -> ok.
clear_headers() ->
    gen_server:call(?MODULE, clear_headers).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
init([]) ->
    _ = ets:new(?MODULE, [named_table, protected]),
    {ok, #?MODULE{}}.

%% @private
handle_call({set_headers, KVs}, {From, _Ref}, State) ->
    Assoc0 = case ets:lookup(?MODULE, From) of
                 [] -> monitor(process, From), [];
                 A  -> A
             end,
    Assoc  = lists:ukeymerge(1, lists:ukeysort(1, KVs), Assoc0),
    true   = ets:insert(?MODULE, {From, Assoc}),
    {reply, ok, State};
handle_call({delete_headers, Keys}, {From, _Ref}, State) ->
    case ets:lookup(?MODULE, From) of
        []               -> ok;
        [{From, Assoc0}] ->
            Assoc  = lists:filter(fun({K, _}) -> not lists:member(K, Keys) end, Assoc0),
            true   = ets:insert(?MODULE, {From, Assoc})
    end,
    {reply, ok, State};
handle_call(clear_headers, {From, _Ref}, State) ->
    true = ets:delete(?MODULE, From),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    true = ets:delete(?MODULE, Pid),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

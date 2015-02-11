%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc This server return the usage of the command of wslogi.
%% @private
-module(wslogi_help).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/0,
         get_help/0
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records
%%----------------------------------------------------------------------------------------------------------------------

-record(?MODULE,
        {
          help = <<>> :: binary()
        }).

-define(HELP_FILE, "help.md").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Start the wslogi_help server.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Return the usage of the command of wslogi.
-spec get_help() -> binary().
get_help() ->
    gen_server:call(?MODULE, get_help).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
init([]) ->
    Dir        = get_priv(),
    Path       = filename:join([Dir, ?HELP_FILE]),
    {ok, Help} = file:read_file(Path),
    {ok, #?MODULE{help = Help}}.

%% @private
handle_call(get_help, _From, #?MODULE{help = Help} = State) ->
    {reply, Help, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Get the priv path of wslogi.
-spec get_priv() -> file:filename().
get_priv() ->
    case code:priv_dir(wslogi) of
        {error, bad_name} ->
            Compile = proplists:get_value(compile, ?MODULE:module_info()),
            Source  = proplists:get_value(source, Compile),
            filename:join([filename:dirname(filename:dirname(Source)), "priv"]);
        Path ->
            Path
    end.

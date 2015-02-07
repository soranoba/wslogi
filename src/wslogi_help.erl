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
    Help = case get_priv() of
               error     -> <<>>;
               {ok, Dir} ->
                   Path = filename:join([Dir, ?HELP_FILE]),
                   {ok, Help0} = file:read_file(Path),
                   Help0
           end,
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
-spec get_priv() -> {ok, file:filename()} | error.
get_priv() ->
    case code:priv_dir(wslogi) of
        {error, bad_name} ->
            case code:which(wslogi_app) of
                Atom when is_atom(Atom) -> error;
                File                    -> {ok, filename:join([filename:dirname(filename:dirname(File)), "priv"])}
            end;
        Path -> {ok, Path}
    end.

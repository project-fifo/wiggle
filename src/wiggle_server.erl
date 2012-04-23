%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 20 Apr 2012 by Heinz N. Gies <>
%%%-------------------------------------------------------------------
-module(wiggle_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    wiggle_storage:init(),
    init_templates(),
    Port = get_env_default(port, 8080),
    Acceptors = get_env_default(acceptors, 2),
    application:start(cowboy),
    %% {Host, list({Path, Handler, Opts})}
    Dispatch = [{'_', [{[<<"static">>, '...'], cowboy_http_static,
			[{directory, {priv_dir, wiggle, []}},
			 {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]},
		       {[<<"js">>, '...'], cowboy_http_static,
			[{directory, {priv_dir, wiggle, [<<"js">>]}},
			 {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]},
		       {[<<"css">>, '...'], cowboy_http_static,
			[{directory, {priv_dir, wiggle, [<<"css">>]}},
			 {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]},
		       {[<<"images">>, '...'], cowboy_http_static,
			[{directory, {priv_dir, wiggle, [<<"images">>]}},
			 {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]},
		       {'_', wiggle_handler, []}]}],
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(my_http_listener, Acceptors,
			  cowboy_tcp_transport, [{port, Port}],
			  cowboy_http_protocol, 
			  [{dispatch, Dispatch}]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_env_default(Key, Default) ->
    case  application:get_env(Key) of
	{ok, Res} ->
	    Res;
	_ ->
	    Default
    end.

init_templates() ->
    erlydtl:compile("templates/base.dtl", tpl_base),
    erlydtl:compile("templates/login.dtl", tpl_login),
    erlydtl:compile("templates/account.dtl", tpl_account),
    erlydtl:compile("templates/about.dtl", tpl_about),
    erlydtl:compile("templates/system.dtl", tpl_system),
    erlydtl:compile("templates/admin.dtl", tpl_admin),
    erlydtl:compile("templates/analytics.dtl", tpl_analytics),
    erlydtl:compile("templates/index.dtl", tpl_index).

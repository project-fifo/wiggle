%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 20 Apr 2012 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(wiggle_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, reload_templates/0]).

-define(SERVER, ?MODULE). 

-define(STATIC(Path), {[Path, '...'], cowboy_http_static,
		       [{directory, {priv_dir, wiggle, [Path]}},
			{mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}).

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
    Port = get_env_default(port, 8080),
    Acceptors = get_env_default(acceptors, 2),
    %% {Host, list({Path, Handler, Opts})}
    Dispatch = [{'_', [?STATIC(<<"js">>),
		       ?STATIC(<<"css">>),
		       ?STATIC(<<"img">>),
		       ?STATIC(<<"tpl">>),
		       {[<<"events">>], wiggle_events, []},
		       {[<<"machines">>, '...', <<"vnc">>], wiggle_wsproxy, []},
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

reload_templates() ->
    erlydtl:compile("apps/wiggle/templates/base.dtl", base_dtl),
    erlydtl:compile("apps/wiggle/templates/login.dtl", login_dtl),
    erlydtl:compile("apps/wiggle/templates/account.dtl", account_dtl),
    erlydtl:compile("apps/wiggle/templates/about.dtl", about_dtl),
    erlydtl:compile("apps/wiggle/templates/system.dtl", system_dtl),
    erlydtl:compile("apps/wiggle/templates/admin.dtl", admin_dtl),
    erlydtl:compile("apps/wiggle/templates/analytics.dtl", analytics_dtl),
    erlydtl:compile("apps/wiggle/templates/vnc.dtl", vnc_dtl),
    erlydtl:compile("apps/wiggle/templates/index.dtl", index_dtl).

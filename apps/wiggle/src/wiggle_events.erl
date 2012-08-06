-module(wiggle_events).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
	 websocket_info/3, websocket_terminate/3]).

init({_Any, http}, _Req, []) ->
    {upgrade, protocol, cowboy_http_websocket}.

handle(Req, State) ->
    {ok, Req1} = cowboy_http_req:reply(404, [], <<"not supported.">>, Req),
    {ok, Req1, State}.


terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, []) ->
    case wiggle_session:get(Req) of
	undefined ->
	    {ok, Req1} = cowboy_http_req:reply(401, [{'Content-Type', <<"text/html">>}], <<"">>, Req),
	    {shutdown, Req1};

	Auth  ->
	    case libsnarl:allowed(Auth, Auth, [service, wiggle, module, event]) of
		true ->
		    {ok, Req, undefined, hibernate},
		    gproc:reg({p, g, {user, Auth}});
		false ->
		    {ok, Req2} = cowboy_http_req:reply(401, [{'Content-Type', <<"text/html">>}],
						       <<"">>, Req),
		    {shutdown, Req2}
	    end
    end.


websocket_handle({text, JSON}, Req, State) ->
    Data = jsx:to_term(JSON),
    Type = proplists:get_value(<<"type">>, Data),
    UUID = proplists:get_value(<<"uuid">>, Data),
    case proplists:get_value(<<"action">>, Data) of
	<<"subscribe">> ->
	    try
		case Type of
		    <<"vm">> ->
			gproc:reg({p, g, {vm, UUID}});
		    <<"host">> ->
			gproc:reg({p, g, {host, UUID}});
		    _ ->
			ok
		end
	    catch
		_:_ ->
		    ok
	    end;
	<<"unsubscribe">> ->
	    try
		case Type of
		    <<"host">> ->
			gproc:reg({p, g, {host,UUID}});
		    _ ->
			ok
		end
	    catch
		_:_ ->
		    ok
	    end
    end,
    {ok, Req, State};

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({vm, state, UUID, NewState}, Req, State) ->
    Reply = [{event, <<"state change">>},
	     {uuid, UUID},
	     {state, ensure_bin(NewState)}],
    {reply, {text, jsx:to_json(Reply)}, Req, State};

websocket_info({vm, add, Data}, Req, State) ->
    Reply = [{event, <<"add vm">>},
	     {data, Data}],
    {reply, {text, jsx:to_json(Reply)}, Req, State};

websocket_info({msg, Type, Msg}, Req, State) ->
    websocket_info({msg, Type, Msg, 0}, Req, State);

websocket_info({msg, Type, Msg, Timeout}, Req, State) ->
    Reply = [{event, <<"message">>},
	     {type, type},
	     {text, ensure_bin(Msg)},
	     {timeout, Timeout}],
    {reply, {text, jsx:to_json(Reply)}, Req, State};


websocket_info({host, stats, UUID, Stats}, Req, State) ->
    Reply = [{event, <<"stat">>},
	     {uuid, UUID},
	     {stats, Stats}],
    {reply, {text, jsx:to_json(Reply)}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req,  _State) ->
    ok.

ensure_bin(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
ensure_bin(L) when is_list(L) ->
    list_to_binary(L);
ensure_bin(B) when is_binary(B) ->
    B.

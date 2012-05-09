%% Feel free to use, reuse and abuse the code in this file.

-module(wiggle_wsproxy).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
	 websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, []) ->
    case cowboy_http_req:header('Upgrade', Req) of
	{undefined, Req2} -> {ok, Req2, undefined};
	{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
	{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

handle(Req, State) ->
    {[<<"machines">>, ID, <<"vnc">>], Req1} = cowboy_http_req:path(Req),
    {ok, Page} = vnc_dtl:render([{<<"uuid">>, ID}]),
    {ok, Req2} =  cowboy_http_req:reply(200, [], Page, Req1),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, []) ->
    case wiggle_session:get(Req) of
	undefined ->
	    {ok, Req1} = cowboy_http_req:reply(401, [{'Content-Type', <<"text/html">>}], <<"">>, Req),
	    {shutdown, Req1};

	Auth  ->
	    {[<<"machines">>, ID, <<"vnc">>], Req1} = cowboy_http_req:path(Req),
	    case libsnarl:allowed(Auth, Auth, [vm, ID, vnc]) of
		{ok, _} ->
		    case sniffle:get_machine_info(Auth, binary_to_list(ID)) of
			{ok, Info} ->
			    VNC = proplists:get_value(<<"vnc">>, Info),
			    Port = proplists:get_value(<<"port">>, VNC),
			    Host = proplists:get_value(<<"host">>, VNC),
			    case gen_tcp:connect(binary_to_list(Host), Port, 
						 [binary,{nodelay, true}, {packet, 0}]) of
				{ok, Socket} ->
				    gen_tcp:controlling_process(Socket, self()),
				    Req2 = cowboy_http_req:compact(Req),
				    {ok, Req2, {Socket}, hibernate};
				_ ->
				    Req2 = cowboy_http_req:compact(Req),
				    {ok, Req2, undefined, hibernate}
			    end;
			_ ->
			    {ok, Req2} = cowboy_http_req:reply(505, [{'Content-Type', <<"text/html">>}],
							       <<"">>, Req1),
			    {shutdown, Req2}
		    end;
		_ ->
		    {ok, Req1} = cowboy_http_req:reply(401, [{'Content-Type', <<"text/html">>}],
						       <<"">>, Req),
		    {shutdown, Req1}
	    end
    end.

websocket_handle({text, Msg}, Req, {Socket} = State) ->
    gen_tcp:send(Socket, base64:decode(Msg)),
    {ok, Req, State};

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({tcp,_Socket,Data}, Req, State) ->
    {reply, {text, base64:encode(Data)}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, {Socket} = _State) ->
    Socket ! {ws, closed},
    gen_tcp:close(Socket),
    ok.

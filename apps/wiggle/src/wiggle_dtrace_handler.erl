-module(wiggle_dtrace_handler).

-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).

-export([init/3,
         handle/2,
         terminate/2]).
-export([websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

init({_Any, http}, Req, []) ->
    case cowboy_http_req:header('Upgrade', Req) of
        {undefined, Req2} -> {ok, Req2, undefined};
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

-record(state, {id, socket, config}).

handle(Req, State) ->
    {ok, Req1} =  cowboy_http_req:reply(200, [], <<"">>, Req),
    {ok, Req1, State}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, []) ->
    {[<<"api">>, _, <<"dtrace">>, ID, <<"stream">>], Req1} = cowboy_http_req:path(Req),
    {ok, Req2} = cowboy_http_req:set_resp_header(
                   <<"Access-Control-Allow-Headers">>,
                   <<"X-Snarl-Token">>, Req1),
    {ok, Req3} = cowboy_http_req:set_resp_header(
                   <<"Access-Control-Expose-Headers">>,
                   <<"X-Snarl-Token">>, Req2),
    {ok, Req4} = cowboy_http_req:set_resp_header(
                   <<"Allow-Access-Control-Credentials">>,
                   <<"true">>, Req3),
    {Token, Req5} = case cowboy_http_req:header(<<"X-Snarl-Token">>, Req4) of
                        {undefined, ReqX} ->
                            {TokenX, ReqX1} = cowboy_http_req:cookie(<<"X-Snarl-Token">>, ReqX),
                            {TokenX, ReqX1};
                        {TokenX, ReqX} ->
                            {ok, ReqX1} = cowboy_http_req:set_resp_header(<<"X-Snarl-Token">>, TokenX, ReqX),
                            {TokenX, ReqX1}
                    end,
    case libsnarl:allowed({token, Token}, [<<"dtrace">>, ID, <<"stream">>]) of
        true ->
            case libsniffle:dtrace_get(ID) of
                {ok, Obj} ->
                    {ok, Req, #state{id = ID, config = jsxd:get(<<"config">>, [], Obj)}};
                _ ->
                    {ok, Req6} = cowboy_http_req:reply(404,
                                                       [{'Content-Type', <<"text/html">>}],
                                                       <<"not found">>, Req5),
                    {shutdown, Req6}
            end;
        false ->
            {ok, Req6} = cowboy_http_req:reply(401, [{'Content-Type', <<"text/html">>}], <<"">>, Req5),
            {shutdown, Req6}
    end.



websocket_handle({text, <<"">>}, Req, State) ->
    {ok, Servers} = libsniffle:hypervisor_list(),
    case libsniffle:dtrace_run(State#state.id, [{<<"servers">>, Servers}]) of
        {ok, S} ->
            {reply, {text, jsx:encode([{<<"config">>, jsxd:merge([{<<"servers">>, Servers}], State#state.config)}])},
             Req, State#state{socket = S}};
        E ->
            {ok, Req1} = cowboy_http_req:reply(505, [{'Content-Type', <<"text/html">>}],
                                               list_to_binary(io_lib:format("~p", [E])), Req),
            {shutdown, Req1}
    end;

websocket_handle({text, Msg}, Req, State) ->
    Config = jsx:decode(Msg),
    {ok, Servers} = libsniffle:hypervisor_list(),
    Config1 = jsxd:update([<<"servers">>], fun(S) ->
                                                   S
                                           end, Servers, Config),
    case libsniffle:dtrace_run(State#state.id, Config1) of
        {ok, S} ->
            {reply, {text, jsx:encode([{<<"config">>, jsxd:merge(Config1, State#state.config)}])},
             Req, State#state{socket = S}};
        E ->
            {ok, Req1} = cowboy_http_req:reply(505, [{'Content-Type', <<"text/html">>}],
                                               list_to_binary(io_lib:format("~p", [E])), Req),
            {shutdown, Req1}
    end;

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({tcp, _Port, Data}, Req, State) ->
    case binary_to_term(Data) of
        {dtrace, ok} ->
            {ok, Req, State, hibernate};
        {dtrace, JSON} ->
            {reply, {text, jsx:encode(JSON)}, Req, State};
        _ ->
            {ok, Req, State, hibernate}
    end;

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, #state{socket = undefined} = _State) ->
    ok;

websocket_terminate(_Reason, _Req, #state{socket = Port} = _State) ->
    gen_tcp:close(Port),
    ok.

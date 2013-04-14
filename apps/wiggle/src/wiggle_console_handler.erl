-module(wiggle_console_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3,
         handle/2,
         terminate/2]).

-export([websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-ignore_xref([init/3,
              handle/2,
              terminate/2]).

init({_Any, http}, Req, []) ->
    case cowboy_req:header(<<"Upgrade">>, Req) of
        {undefined, Req2} -> {ok, Req2, undefined};
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

handle(Req, State) ->
    {ok, Req1} =  cowboy_req:reply(200, [], <<"">>, Req),
    {ok, Req1, State}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, []) ->
    {ID, Req1} = cowboy_req:binding(uuid, Req),
    Req2 = cowboy_req:set_resp_header(
                   <<"Access-Control-Allow-Headers">>,
                   <<"X-Snarl-Token">>, Req1),
    Req3 = cowboy_req:set_resp_header(
                   <<"Access-Control-Expose-Headers">>,
                   <<"X-Snarl-Token">>, Req2),
    Req4 = cowboy_req:set_resp_header(
                   <<"Allow-Access-Control-Credentials">>,
                   <<"true">>, Req3),
    {Token, Req5} = case cowboy_req:header(<<"X-Snarl-Token">>, Req4) of
                        {undefined, ReqX} ->
                            {TokenX, ReqX1} = cowboy_req:cookie(<<"X-Snarl-Token">>, ReqX),
                            {TokenX, ReqX1};
                        {TokenX, ReqX} ->
                            ReqX1 = cowboy_req:set_resp_header(<<"X-Snarl-Token">>, TokenX, ReqX),
                            {TokenX, ReqX1}
                    end,
    case libsnarl:allowed({token, Token}, [<<"vms">>, ID, <<"console">>]) of
        true ->
            case libsniffle:vm_get(ID) of
                {ok, VM} ->
                    case jsxd:get(<<"hypervisor">>, VM) of
                        {ok, HID} ->
                            case libsniffle:hypervisor_get(HID) of
                                {ok, H} ->
                                    {ok,HostBin} = jsxd:get(<<"host">>, H),
                                    Host = binary_to_list(HostBin),
                                    {ok, Port} = jsxd:get(<<"port">>, H),
                                    {ok, Console} = libchunter:console_open(Host, Port, ID, self()),
                                    {ok, Req5, {Console}};
                                _ ->
                                    {ok, Req6} = cowboy_req:reply(505, [], <<"could not find hypervisor">>, Req5),
                                    {shutdown, Req6}
                            end;
                        _ ->
                            {ok, Req6} = cowboy_req:reply(505, [],  <<"could not find hypervisor">>, Req5),
                            {shutdown, Req6}
                    end;
                E ->
                    {ok, Req6} = cowboy_req:reply(505, [], list_to_binary(io_lib:format("~p", [E])), Req5),
                    {shutdown, Req6}
            end;
        false ->
            {ok, Req6} = cowboy_req:reply(401, Req5),
            {shutdown, Req6}
    end.

websocket_handle({text, Msg}, Req, {Console} = State) ->
    libchunter_console_server:send(Console, Msg),
    {ok, Req, State};

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({data, Data}, Req, State) ->
    {reply, {text, Data}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, {Console} = _State) ->
    libchunter_console_server:close(Console),
    ok.

-module(wiggle_dtrace_stream).

-behaviour(cowboy_websocket_handler).

-export([init/3]).

-export([websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-ignore_xref([init/3]).

-record(state, {id, socket, config, encoder, decoder, type}).

init({_Andy, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

e(Code, Req) ->
    e(Code, <<"">>, Req).

e(Code, Msg, Req) ->
    {ok, Req1} = cowboy_req:reply(Code, [], Msg, Req),
    {shutdown, Req1}.

websocket_init(_Any, Req, []) ->
    {Proto, Req0} = case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
                        {ok, undefined, ReqR} ->
                            {<<"json">>, ReqR};
                        {ok, [], ReqR} ->
                            {<<"json">>, ReqR};
                        {ok, [P |_], ReqR} ->
                            lager:debug("[dtrace] Setting up protocol to ~s", [P]),
                            {P, cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, P, ReqR)}
                    end,
    {ID, Req1} = cowboy_req:binding(uuid, Req0),
    Req2 = wiggle_handler:set_access_header(Req1),
    {Encoder, Decoder, Type} = case Proto of
                                   <<"msgpack">> ->
                                       {fun(O) ->
                                                msgpack:pack(O, [jsx])
                                        end,
                                        fun(D) ->
                                                {ok, O} = msgpack:unpack(D, [jsx]),
                                                jsxd:from_list(O)
                                        end,
                                        binary};
                                   <<"json">> ->
                                       {fun(O) ->
                                                jsx:encode(O)
                                        end,
                                        fun(D) ->
                                                jsxd:from_list(jsx:decode(D))
                                        end, text}
                               end,
    case wiggle_handler:get_token(Req2) of
        {undefined, Req3} ->
            lager:info("[dtrace] Not authenticated!"),
            e(401, <<".">>, Req3);
        {Token, Req3} ->
            case libsnarl:allowed(Token, [<<"dtrace">>, ID, <<"stream">>]) of
                true ->
                    case ls_dtrace:get(ID) of
                        {ok, Obj} ->
                            lager:debug("[dtrace] Gotten object: ~p", [Obj]),
                            {ok, Req3, #state{id = ID, config = ft_dtrace:config(Obj),
                                              encoder = Encoder, decoder = Decoder, type = Type}};
                        _ ->
                            lager:info("[dtrace] Not found!"),
                            e(404, Req3)
                    end;
                false ->
                    lager:info("[dtrace] forbidden!"),

                    e(403, <<"forbidden">>, Req3)
            end
    end.

websocket_handle({Type, <<>>}, Req, State = #state{type = Type}) ->
    handle(null, Req, State);

websocket_handle({Type, M}, Req, State = #state{decoder = Dec, type = Type}) ->
    handle(Dec(M), Req, State);

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({tcp, _Port, Data}, Req, State  = #state{encoder = Enc, type = Type}) ->
    case binary_to_term(Data) of
        {dtrace, ok} ->
            {ok, Req, State, hibernate};
        {dtrace, JSON} ->
            {reply, {Type, Enc(JSON)}, Req, State};
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

handle(null, Req, State = #state{encoder = Enc, type = Type}) ->
    {ok, Servers} = ls_hypervisor:list(),
    case ls_dtrace:run(State#state.id, [{<<"servers">>, Servers}]) of
        {ok, S} ->
            {reply, {Type, Enc([{<<"config">>, jsxd:merge([{<<"servers">>, Servers}], State#state.config)}])},
             Req, State#state{socket = S}};
        E ->
            {ok, Req1} = cowboy_req:reply(505, [], list_to_binary(io_lib:format("~p", [E])), Req),
            {shutdown, Req1}
    end;

handle(Config, Req, State  = #state{encoder = Enc, type = Type}) ->
    lager:debug("[dtrace] handle(~p)", [Config]),
    {ok, Servers} = ls_hypervisor:list(),
    Config1 = case jsxd:get([<<"vms">>], [], Config) of
                  [] ->
                      Config;
                  VMs ->
                      VMs0 = [ls_vm:get(V) || V <- VMs],
                      Servers2 = [ft_vm:hypervisor(V) || {ok, V} <- VMs0],
                      Filter = [[<<"zonename">>, V] || V <- VMs],
                      jsxd:thread([{set, [<<"servers">>], lists:usort(Servers2)},
                                   {update, [<<"filter">>],
                                    fun (F) ->
                                            [{<<"and">>, [Filter | F]}]
                                    end, [{<<"and">>, Filter}]}], Config)
              end,
    Config2 = jsxd:update([<<"servers">>], fun(S) ->
                                                   S
                                           end, Servers, Config1),
    case ls_dtrace:run(State#state.id, Config2) of
        {ok, S} ->
            {reply, {Type, Enc(jsxd:merge(Config1, State#state.config))},
             Req, State#state{socket = S}};
        E ->
            {ok, Req1} = cowboy_req:reply(505, [], list_to_binary(io_lib:format("~p", [E])), Req),
            {shutdown, Req1}
    end.

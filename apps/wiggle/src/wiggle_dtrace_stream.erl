-module(wiggle_dtrace_stream).

-behaviour(cowboy_websocket_handler).

-include("wiggle.hrl").

-export([init/3]).

-export([websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-ignore_xref([init/3]).

-record(dstate, {id, socket, config, encoder, decoder, type,
                 token, scope_perms = [[<<"...">>]], state=connected}).

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
    case wiggle_handler:get_token(#state{}, Req2) of
        {#state{ token = undefined}, Req3} ->
            {ok, Req3, #dstate{id = ID, encoder = Encoder, decoder = Decoder,
                               type = Type}};
        {#state{token = Token, scope_perms = SPerms}, Req3} ->
            State = #dstate{id = ID, encoder = Encoder, decoder = Decoder,
                            type = Type, token = Token, scope_perms = SPerms,
                            state = authenticated},
            init(State, Req3)
    end.

websocket_handle({Type, M}, Req,
                 State = #dstate{state = connected, type = Type,
                                 encoder = Enc, decoder = Dec}) ->
    case auth(Dec(M), State) of
        {ok, S1} ->
            init(S1#dstate{state = authenticated}, Req);
        {error, S1} ->
            {reply, {Type, Enc([{<<"error">>, <<"denied">>}])}, Req, S1}
    end;

websocket_handle({Type, <<>>}, Req,
                 State = #dstate{state = authenticated, type = Type}) ->
    handle(null, Req, State);

websocket_handle({Type, M}, Req, State = #dstate{decoder = Dec, type = Type}) ->
    handle(Dec(M), Req, State);

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({tcp, _Port, Data}, Req,
               State = #dstate{state = authenticated, encoder = Enc, type = Type}) ->
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

websocket_terminate(_Reason, _Req, #dstate{socket = undefined} = _State) ->
    ok;

websocket_terminate(_Reason, _Req, #dstate{socket = Port} = _State) ->
    gen_tcp:close(Port),
    ok.

auth([{<<"bearer">>, Bearer}], State) ->
    case ls_oauth:verify_access_token(Bearer) of
        {ok, Context} ->
            case {proplists:get_value(<<"resource_owner">>, Context),
                  proplists:get_value(<<"scope">>, Context)} of
                {undefined, _} ->
                    {error, State};
                {UUID, Scope} ->
                    SPerms = wiggle_handler:scope_perms(ls_oauth:scope(Scope), []),
                    State1 = State#dstate{token = UUID, scope_perms = SPerms},
                    {ok, State1}
            end;
        _ ->
            {error, State}
    end;

auth([{<<"token">>, Token}], State) ->
    State1 = State#dstate{token = {token, Token}},
    {ok, State1}.

init(State = #dstate{id = ID}, Req) ->
    Permission = [<<"dtrace">>, ID, <<"stream">>],
    case allowed(Permission, State) of
        true ->
            case ls_dtrace:get(ID) of
                {ok, Obj} ->
                    lager:debug("[dtrace] Gotten object: ~p", [Obj]),
                    {ok, Req, State#dstate{config = ft_dtrace:config(Obj)}};
                _ ->
                    lager:info("[dtrace] Not found!"),
                    e(404, Req)
            end;
        false ->
            lager:info("[dtrace] forbidden!"),
            e(403, <<"forbidden">>, Req)
    end.

handle(null, Req, State = #dstate{encoder = Enc, type = Type}) ->
    {ok, Servers} = ls_hypervisor:list(),
    case ls_dtrace:run(State#dstate.id, [{<<"servers">>, Servers}]) of
        {ok, S} ->
            {reply, {Type, Enc([{<<"config">>, jsxd:merge([{<<"servers">>, Servers}], State#dstate.config)}])},
             Req, State#dstate{socket = S}};
        E ->
            {ok, Req1} = cowboy_req:reply(505, [], list_to_binary(io_lib:format("~p", [E])), Req),
            {shutdown, Req1}
    end;

handle(Config, Req, State  = #dstate{encoder = Enc, type = Type,
                                     token = Token}) ->
    lager:debug("[dtrace] handle(~p)", [Config]),
    case update_vms(Config, State) of
        {ok, Config1} ->
            {ok, Permissions} = wiggle_handler:get_persmissions(Token),
            Permission = [{must, 'allowed',
                           [<<"hypervisors">>, {<<"res">>, <<"uuid">>}, <<"get">>],
                           Permissions}],
            {ok, Servers} = ls_hypervisor:list(Permission, false),
            Config2 = jsxd:update([<<"servers">>], fun(S) ->
                                                           S
                                                   end, Servers, Config1),
            case ls_dtrace:run(State#dstate.id, Config2) of
                {ok, S} ->
                    {reply, {Type, Enc(jsxd:merge(Config1, State#dstate.config))},
                     Req, State#dstate{socket = S}};
                E ->
                    {ok, Req1} = cowboy_req:reply(505, [], list_to_binary(io_lib:format("~p", [E])), Req),
                    {shutdown, Req1}
            end;
        {error, denied} ->
            e(403, Req)
    end.

update_vms(Config, State) ->
    case jsxd:get([<<"vms">>], [], Config) of
        [] ->
            {ok, Config};
        VMs ->
            case test_vms(VMs, State) of
                true ->
                    VMs0 = [ls_vm:get(V) || V <- VMs],
                    Servers2 = [ft_vm:hypervisor(V) || {ok, V} <- VMs0],
                    Filter = [[<<"zonename">>, V] || V <- VMs],
                    jsxd:thread([{set, [<<"servers">>], lists:usort(Servers2)},
                                 {update, [<<"filter">>],
                                  fun (F) ->
                                          [{<<"and">>, [Filter | F]}]
                                  end, [{<<"and">>, Filter}]}], Config);
                false ->
                    {error, denied}
            end
    end.

test_vms([], _State) ->
    true;

test_vms([VM | R], State) ->
    allowed([<<"vms">>, VM, <<"get">>], State) andalso
        test_vms(R, State).

allowed(Permission, #dstate{token = Token, scope_perms = SPerms}) ->
    libsnarlmatch:test_perms(Permission, SPerms)
        andalso libsnarl:allowed(Token, Permission).


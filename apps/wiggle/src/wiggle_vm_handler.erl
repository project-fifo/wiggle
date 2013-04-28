%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.

-module(wiggle_vm_handler).

-include("wiggle.hrl").

-export([init/3,
         rest_init/2]).

-export([content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         resource_exists/2,
         delete_resource/2,
         forbidden/2,
         post_is_create/2,
         create_path/2,
         options/2,
         service_available/2,
         is_authorized/2,
         rest_terminate/2]).

-export([to_json/2,
         from_json/2,
         to_msgpack/2,
         from_msgpack/2]).

-ignore_xref([to_json/2,
              from_json/2,
              from_msgpack/2,
              to_msgpack/2,
              allowed_methods/2,
              content_types_accepted/2,
              content_types_provided/2,
              delete_resource/2,
              forbidden/2,
              init/3,
              create_path/2,
              post_is_create/2,
              is_authorized/2,
              options/2,
              service_available/2,
              resource_exists/2,
              rest_init/2,
              rest_terminate/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) ->
    wiggle_handler:initial_state(Req).

rest_terminate(_Req, State) ->
    ?M(?P(State), State#state.start),
    ok.

service_available(Req, State) ->
    {wiggle_handler:service_available(), Req, State}.

options(Req, State) ->
    Methods = allowed_methods(State#state.version, State#state.token, State#state.path),
    wiggle_handler:options(Req, State,Methods).

content_types_provided(Req, State) ->
    {wiggle_handler:provided(), Req, State}.

content_types_accepted(Req, State) ->
    {wiggle_handler:accepted(), Req, State}.

post_is_create(Req, State) ->
    {true, Req, State}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"OPTIONS">> | allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Vm]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Vm, <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Vm, <<"snapshots">>, _ID]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Vm, <<"snapshots">>]) ->
    [<<"GET">>, <<"POST">>].


resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [Vm, <<"snapshots">>, Snap]}) ->
    Start = now(),
    case libsniffle:vm_get(Vm) of
        not_found ->
            ?MSniffle(?P(State), Start),
            {false, Req, State};
        {ok, Obj} ->
            ?MSniffle(?P(State), Start),
            case jsxd:get([<<"snapshots">>, Snap], Obj) of
                undefined ->
                    {false, Req, State};
                {ok, _} ->
                    {true, Req, State#state{obj=Obj}}
            end
    end;
resource_exists(Req, State = #state{path = [Vm | _]}) ->
    Start = now(),
    case libsniffle:vm_get(Vm) of
        not_found ->
            ?MSniffle(?P(State), Start),
            {false, Req, State};
        {ok, Obj} ->
            ?MSniffle(?P(State), Start),
            {true, Req, State#state{obj=Obj}}
    end.

is_authorized(Req, State = #state{method = <<"OPTIONS">>}) ->
    {true, Req, State};

is_authorized(Req, State = #state{token = undefined}) ->
    {{false, <<"x-snarl-token">>}, Req, State};

is_authorized(Req, State) ->
    {true, Req, State}.

forbidden(Req, State = #state{method = <<"OPTIONS">>}) ->
    {false, Req, State};

forbidden(Req, State = #state{token = undefined}) ->
    {true, Req, State};

forbidden(Req, State = #state{method = <<"GET">>, path = []}) ->
    {allowed(State, [<<"cloud">>, <<"vms">>, <<"list">>]), Req, State};

forbidden(Req, State = #state{method = <<"POST">>, path = []}) ->
    {allowed(State, [<<"cloud">>, <<"vms">>, <<"create">>]), Req, State};

forbidden(Req, State = #state{method = <<"GET">>, path = [Vm]}) ->
    {allowed(State, [<<"vms">>, Vm, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Vm]}) ->
    {allowed(State, [<<"vms">>, Vm, <<"delete">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Vm]}) ->
    {ok, Decoded, Req1} = wiggle_handler:decode(Req),
    case Decoded of
        [{<<"action">>, <<"start">>}] ->
            {allowed(State, [<<"vms">>, Vm, <<"start">>]), Req1, State#state{body=Decoded}};
        [{<<"action">>, <<"stop">>}|_] ->
            {allowed(State, [<<"vms">>, Vm, <<"stop">>]), Req1, State#state{body=Decoded}};
        [{<<"action">>, <<"reboot">>}|_] ->
            {allowed(State, [<<"vms">>, Vm, <<"reboot">>]), Req1, State#state{body=Decoded}};
        _ ->
            {allowed(State, [<<"vms">>, Vm, <<"edit">>]), Req1, State#state{body=Decoded}}
    end;

forbidden(Req, State = #state{method = <<"GET">>, path = [Vm, <<"snapshots">>]}) ->
    {allowed(State, [<<"vms">>, Vm, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = <<"POST">>, path = [Vm, <<"snapshots">>]}) ->
    {allowed(State, [<<"vms">>, Vm, <<"snapshot">>]), Req, State};

forbidden(Req, State = #state{method = <<"GET">>, path = [Vm, <<"snapshots">>, _Snap]}) ->
    {allowed(State, [<<"vms">>, Vm, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Vm, <<"snapshots">>, _Snap]}) ->
    {ok, Decoded, Req1} = wiggle_handler:decode(Req),
    case Decoded of
        [{<<"action">>, <<"rollback">>}] ->
            {allowed(State, [<<"vms">>, Vm, <<"rollback">>]), Req1, State#state{body=Decoded}};
        _ ->
            {allowed(State, [<<"vms">>, Vm, <<"edit">>]), Req1, State}
    end;

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Vm, <<"snapshots">>, _Snap]}) ->
    {allowed(State, [<<"vms">>, Vm, <<"snapshot_delete">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Vm, <<"metadata">> | _]}) ->
    {ok, Decoded, Req1} = wiggle_handler:decode(Req),
    {allowed(State, [<<"vms">>, Vm, <<"edit">>]), Req1, State#state{body=Decoded}};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Vm, <<"metadata">> | _]}) ->
    {allowed(State, [<<"vms">>, Vm, <<"edit">>]), Req, State};

forbidden(Req, State) ->
    lager:error("Access to unknown path: ~p~n.", [State]),
    {true, Req, State}.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

to_json(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {jsx:encode(Reply), Req1, State1}.

to_msgpack(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {msgpack:pack(Reply, [jsx]), Req1, State1}.

handle_request(Req, State = #state{token = Token, path = []}) ->
    Start = now(),
    {ok, Permissions} = libsnarl:user_cache({token, Token}),
    ?MSnarl(?P(State), Start),
    Start1 = now(),
    {ok, Res} = libsniffle:vm_list([{must, 'allowed', [<<"vms">>, {<<"res">>, <<"uuid">>}, <<"get">>], Permissions}]),
    ?MSniffle(?P(State), Start1),
    {lists:map(fun ({E, _}) -> E end,  Res), Req, State};

handle_request(Req, State = #state{path = [_Vm, <<"snapshots">>], obj = Obj}) ->
    Snaps = jsxd:fold(fun(UUID, Snap, Acc) ->
                              [jsxd:set(<<"uuid">>, UUID, Snap) | Acc]
                      end, [], jsxd:get(<<"snapshots">>, [], Obj)),
    {Snaps, Req, State};

handle_request(Req, State = #state{path = [_Vm, <<"snapshots">>, Snap], obj = Obj}) ->
    {jsxd:get([<<"snapshots">>, Snap], null, Obj), Req, State};

handle_request(Req, State = #state{path = [_Vm], obj = Obj}) ->
    {Obj, Req, State}.


%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create_path(Req, State = #state{path = [], version = Version, token = Token}) ->
    {ok, Decoded, Req1} = wiggle_handler:decode(Req),
    try
        {ok, Dataset} = jsxd:get(<<"dataset">>, Decoded),
        {ok, Package} = jsxd:get(<<"package">>, Decoded),
        {ok, Config} = jsxd:get(<<"config">>, Decoded),
        try
            {ok, User} = libsnarl:user_get({token, Token}),
            {ok, Owner} = jsxd:get(<<"uuid">>, User),
            Start = now(),
            {ok, UUID} = libsniffle:create(Package, Dataset, jsxd:set(<<"owner">>, Owner, Config)),
            ?MSniffle(?P(State), Start),
            {<<"/api/", Version/binary, "/vms/", UUID/binary>>, Req1, State#state{body = Decoded}}
        catch
            G:E ->
                lager:error("Error creating VM(~p): ~p / ~p", [Decoded, G, E]),
                {ok, Req2} = cowboy_req:reply(500, Req1),
                {halt, Req2, State}
        end
    catch
        G1:E1 ->
            lager:error("Error creating VM(~p): ~p / ~p", [Decoded, G1, E1]),
            {ok, Req3} = cowboy_req:reply(400, Req1),
            {halt, Req3, State}
    end;

create_path(Req, State = #state{path = [Vm, <<"snapshots">>], version = Version}) ->
    {ok, Decoded, Req1} = wiggle_handler:decode(Req),
    Comment = jsxd:get(<<"comment">>, <<"">>, Decoded),
    Start = now(),
    {ok, UUID} = libsniffle:vm_snapshot(Vm, Comment),
    ?MSniffle(?P(State), Start),
    {<<"/api/", Version/binary, "/vms/", Vm/binary, "/snapshots/", UUID/binary>>, Req1, State#state{body = Decoded}}.

from_json(Req, #state{body = undefined} = State) ->
    handle_write(Req, State, []);

from_json(Req, #state{body = Decoded} = State) ->
    handle_write(Req, State, Decoded).

from_msgpack(Req, #state{body = undefined} = State) ->
    handle_write(Req, State, []);

from_msgpack(Req, #state{body = Decoded} = State) ->
    handle_write(Req, State, Decoded).

handle_write(Req, State = #state{path = [Vm, <<"metadata">> | Path]}, [{K, V}]) ->
    Start = now(),
    libsniffle:vm_set(Vm, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"start">>}]) ->
    Start = now(),
    libsniffle:vm_start(Vm),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"stop">>}]) ->
    Start = now(),
    libsniffle:vm_stop(Vm),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"stop">>}, {<<"force">>, true}]) ->
    Start = now(),
    libsniffle:vm_stop(Vm, [force]),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"reboot">>}]) ->
    Start = now(),
    libsniffle:vm_reboot(Vm),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"reboot">>}, {<<"force">>, true}]) ->
    Start = now(),
    libsniffle:vm_reboot(Vm, [force]),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"config">>, Config},
                                                {<<"package">>, Package}]) ->
    Start = now(),
    libsniffle:vm_update(Vm, Package, Config),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"config">>, Config}]) ->
    Start = now(),
    libsniffle:vm_update(Vm, undefined, Config),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"package">>, Package}]) ->
    Start = now(),
    libsniffle:vm_update(Vm, Package, []),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

handle_write(Req, State = #state{path = []}, _Body) ->
    {true, Req, State};

handle_write(Req, State = #state{path = [_Vm, <<"snapshots">>]}, _Body) ->
    {true, Req, State};

handle_write(Req, State = #state{path = [Vm, <<"snapshots">>, UUID]}, [{<<"action">>, <<"rollback">>}]) ->
    Start = now(),
    ok = libsniffle:vm_rollback_snapshot(Vm, UUID),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

handle_write(Req, State, _Body) ->
    lager:error("Unknown PUT request: ~p~n.", [State]),
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [Vm, <<"snapshots">>, UUID]}) ->
    Start = now(),
    ok = libsniffle:vm_delete_snapshot(Vm, UUID),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete_resource(Req, State = #state{path = [Vm]}) ->
    Start = now(),
    ok = libsniffle:vm_delete(Vm),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete_resource(Req, State = #state{path = [Vm, <<"metadata">> | Path]}) ->
    Start = now(),
    libsniffle:vm_set(Vm, [<<"metadata">> | Path], delete),
    ?MSniffle(?P(State), Start),
    {true, Req, State}.

allowed(State, Perm) ->
    Token = State#state.token,
    Start = now(),
    R = case libsnarl:allowed({token, Token}, Perm) of
            not_found ->
                true;
            true ->
                false;
            false ->
                true
        end,
    ?MSnarl(?P(State), Start),
    R.

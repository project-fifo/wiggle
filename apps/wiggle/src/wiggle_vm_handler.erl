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

rest_terminate(Req, State) ->
    {Path, _} = cowboy_req:path(Req),
    statman_histogram:record_value({Path, total}, State#state.start),
    ok.

service_available(Req, State) ->
    case {libsniffle:servers(), libsnarl:servers()} of
        {[], _} ->
            {false, Req, State};
        {_, []} ->
            {false, Req, State};
        _ ->
            {true, Req, State}
    end.

options(Req, State) ->
    Methods = allowed_methods(State#state.version, State#state.token, State#state.path),
    Req1 = cowboy_req:set_resp_header(
             <<"access-control-allow-methods">>,
             string:join(
               lists:map(fun erlang:binary_to_list/1,
                         [<<"HEAD">>, <<"OPTIONS">> | Methods]), ", "), Req),
    {ok, Req1, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json},
      {<<"application/x-msgpack">>, to_msgpack}
     ], Req, State}.

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
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    case libsniffle:vm_get(Vm) of
        not_found ->
            statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
            {false, Req1, State};
        {ok, Obj} ->
            statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
            case jsxd:get([<<"snapshots">>, Snap], Obj) of
                undefined ->
                    {false, Req1, State};
                {ok, _} ->
                    {true, Req1, State#state{obj=Obj}}
            end
    end;
resource_exists(Req, State = #state{path = [Vm | _]}) ->
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    case libsniffle:vm_get(Vm) of
        not_found ->
            statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
            {false, Req1, State};
        {ok, Obj} ->
            statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
            {true, Req1, State#state{obj=Obj}}
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
    {Path, Req1} = cowboy_req:path(Req),
    {allowed(State#state.token, [<<"cloud">>, <<"vms">>, <<"list">>], Path), Req1, State};

forbidden(Req, State = #state{method = <<"POST">>, path = []}) ->
    {Path, Req1} = cowboy_req:path(Req),
    {allowed(State#state.token, [<<"cloud">>, <<"vms">>, <<"create">>], Path), Req1, State};

forbidden(Req, State = #state{method = <<"GET">>, path = [Vm]}) ->
    {Path, Req1} = cowboy_req:path(Req),
    {allowed(State#state.token, [<<"vms">>, Vm, <<"get">>], Path), Req1, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Vm]}) ->
    {Path, Req1} = cowboy_req:path(Req),
    {allowed(State#state.token, [<<"vms">>, Vm, <<"delete">>], Path), Req1, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Vm]}) ->
    {Path, Req0} = cowboy_req:path(Req),
    {ok, Decoded, Req1} = wiggle_handler:decode(Req0),
    case Decoded of
        [{<<"action">>, <<"start">>}] ->
            {allowed(State#state.token, [<<"vms">>, Vm, <<"start">>], Path), Req1, State#state{body=Decoded}};
        [{<<"action">>, <<"stop">>}|_] ->
            {allowed(State#state.token, [<<"vms">>, Vm, <<"stop">>], Path), Req1, State#state{body=Decoded}};
        [{<<"action">>, <<"reboot">>}|_] ->
            {allowed(State#state.token, [<<"vms">>, Vm, <<"reboot">>], Path), Req1, State#state{body=Decoded}};
        _ ->
            {allowed(State#state.token, [<<"vms">>, Vm, <<"edit">>], Path), Req1, State#state{body=Decoded}}
    end;

forbidden(Req, State = #state{method = <<"GET">>, path = [Vm, <<"snapshots">>]}) ->
    {Path, Req1} = cowboy_req:path(Req),
    {allowed(State#state.token, [<<"vms">>, Vm, <<"get">>], Path), Req1, State};

forbidden(Req, State = #state{method = <<"POST">>, path = [Vm, <<"snapshots">>]}) ->
    {Path, Req1} = cowboy_req:path(Req),
    {allowed(State#state.token, [<<"vms">>, Vm, <<"snapshot">>], Path), Req1, State};

forbidden(Req, State = #state{method = <<"GET">>, path = [Vm, <<"snapshots">>, _Snap]}) ->
    {Path, Req1} = cowboy_req:path(Req),
    {allowed(State#state.token, [<<"vms">>, Vm, <<"get">>], Path), Req1, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Vm, <<"snapshots">>, _Snap]}) ->
    {Path, Req0} = cowboy_req:path(Req),
    {ok, Decoded, Req1} = wiggle_handler:decode(Req0),
    case Decoded of
        [{<<"action">>, <<"rollback">>}] ->
            {allowed(State#state.token, [<<"vms">>, Vm, <<"rollback">>], Path), Req1, State#state{body=Decoded}};
        _ ->
            {allowed(State#state.token, [<<"vms">>, Vm, <<"edit">>], Path), Req1, State}
    end;

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Vm, <<"snapshots">>, _Snap]}) ->
    {Path, Req1} = cowboy_req:path(Req),
    {allowed(State#state.token, [<<"vms">>, Vm, <<"snapshot_delete">>], Path), Req1, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Vm, <<"metadata">> | _]}) ->
    {Path, Req0} = cowboy_req:path(Req),
    {ok, Decoded, Req1} = wiggle_handler:decode(Req0),
    {allowed(State#state.token, [<<"vms">>, Vm, <<"edit">>], Path), Req1, State#state{body=Decoded}};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Vm, <<"metadata">> | _]}) ->
    {Path, Req1} = cowboy_req:path(Req),
    {allowed(State#state.token, [<<"vms">>, Vm, <<"edit">>], Path), Req1, State};

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
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    {ok, Permissions} = libsnarl:user_cache({token, Token}),
    statman_histogram:record_value({Path, {ext, <<"snarl">>}}, Start),
    Start1 = now(),
    {ok, Res} = libsniffle:vm_list([{must, 'allowed', [<<"vms">>, {<<"res">>, <<"uuid">>}, <<"get">>], Permissions}]),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start1),
    {lists:map(fun ({E, _}) -> E end,  Res), Req1, State};

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
        {Path, Req2} = cowboy_req:path(Req1),
        try
            {ok, User} = libsnarl:user_get({token, Token}),
            {ok, Owner} = jsxd:get(<<"uuid">>, User),
            Start = now(),
            {ok, UUID} = libsniffle:create(Package, Dataset, jsxd:set(<<"owner">>, Owner, Config)),
            statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
            {<<"/api/", Version/binary, "/vms/", UUID/binary>>, Req2, State#state{body = Decoded}}
        catch
            G:E ->
                lager:error("Error creating VM(~p): ~p / ~p", [Decoded, G, E]),
                {ok, Req3} = cowboy_req:reply(500, Req2),
                {halt, Req3, State}
        end
    catch
        G1:E1 ->
            lager:error("Error creating VM(~p): ~p / ~p", [Decoded, G1, E1]),
            {ok, Req4} = cowboy_req:reply(400, Req1),
            {halt, Req4, State}
    end;

create_path(Req, State = #state{path = [Vm, <<"snapshots">>], version = Version}) ->
    {ok, Decoded, Req1} = wiggle_handler:decode(Req),
    Comment = jsxd:get(<<"comment">>, <<"">>, Decoded),
    {Path, Req2} = cowboy_req:path(Req1),
    Start = now(),
    {ok, UUID} = libsniffle:vm_snapshot(Vm, Comment),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
    {<<"/api/", Version/binary, "/vms/", Vm/binary, "/snapshots/", UUID/binary>>, Req2, State#state{body = Decoded}}.

from_json(Req, #state{body = undefined} = State) ->
    handle_write(Req, State, []);

from_json(Req, #state{body = Decoded} = State) ->
    handle_write(Req, State, Decoded).

from_msgpack(Req, #state{body = undefined} = State) ->
    handle_write(Req, State, []);

from_msgpack(Req, #state{body = Decoded} = State) ->
    handle_write(Req, State, Decoded).

handle_write(Req, State = #state{path = [Vm, <<"metadata">> | Path]}, [{K, V}]) ->
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    libsniffle:vm_set(Vm, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
    {true, Req1, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"start">>}]) ->
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    libsniffle:vm_start(Vm),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
    {true, Req1, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"stop">>}]) ->
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    libsniffle:vm_stop(Vm),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
    {true, Req1, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"stop">>}, {<<"force">>, true}]) ->
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    libsniffle:vm_stop(Vm, [force]),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
    {true, Req1, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"reboot">>}]) ->
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    libsniffle:vm_reboot(Vm),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
    {true, Req1, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"reboot">>}, {<<"force">>, true}]) ->
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    libsniffle:vm_reboot(Vm, [force]),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
    {true, Req1, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"config">>, Config},
                                                {<<"package">>, Package}]) ->
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    libsniffle:vm_update(Vm, Package, Config),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
    {true, Req1, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"config">>, Config}]) ->
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    libsniffle:vm_update(Vm, undefined, Config),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
    {true, Req1, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"package">>, Package}]) ->
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    libsniffle:vm_update(Vm, Package, []),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
    {true, Req1, State};

handle_write(Req, State = #state{path = []}, _Body) ->
    {true, Req, State};

handle_write(Req, State = #state{path = [_Vm, <<"snapshots">>]}, _Body) ->
    {true, Req, State};

handle_write(Req, State = #state{path = [Vm, <<"snapshots">>, UUID]}, [{<<"action">>, <<"rollback">>}]) ->
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    ok = libsniffle:vm_rollback_snapshot(Vm, UUID),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
    {true, Req1, State};

handle_write(Req, State, _Body) ->
    lager:error("Unknown PUT request: ~p~n.", [State]),
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [Vm, <<"snapshots">>, UUID]}) ->
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    ok = libsniffle:vm_delete_snapshot(Vm, UUID),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
    {true, Req1, State};

delete_resource(Req, State = #state{path = [Vm]}) ->
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    ok = libsniffle:vm_delete(Vm),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
    {true, Req1, State};

delete_resource(Req, State = #state{path = [Vm, <<"metadata">> | Path]}) ->
    {Path, Req1} = cowboy_req:path(Req),
    Start = now(),
    libsniffle:vm_set(Vm, [<<"metadata">> | Path], delete),
    statman_histogram:record_value({Path, {ext, <<"sniffle">>}}, Start),
    {true, Req1, State}.

allowed(Token, Perm, Path) ->
    Start = now(),
    R = case libsnarl:allowed({token, Token}, Perm) of
            not_found ->
                true;
            true ->
                false;
            false ->
                true
        end,
    statman_histogram:record_value({Path, {ext, <<"snarl">>}}, Start),
    R.

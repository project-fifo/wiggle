-module(wiggle_vm_handler).

-include("wiggle.hrl").

-export([allowed_methods/3,
         get/1,
         permission_required/1,
         read/2,
         create/3,
         write/3,
         delete/2]).

-ignore_xref([allowed_methods/3,
              get/1,
              permission_required/1,
              read/2,
              create/3,
              write/3,
              delete/2]).


-define(GUARD_CALL(Call),
        case Call of
            ok ->
                true;
            GuardCallError ->
                lager:error("Error: ~p", [GuardCallError]),
                false
        end).

-define(LIB(Call),
        Start = now(),
        case Call of
            ok ->
                ?MSniffle(?P(State), Start),
                {true, Req, State};
            GuardCallError ->
                ?MSniffle(?P(State), Start),
                lager:error("Error: ~p", [GuardCallError]),
                {false, Req, State}
        end).

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Vm]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Vm, <<"owner">>]) ->
    [<<"PUT">>];

allowed_methods(_Version, _Token, [_Vm, <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Vm, <<"nics">>, _Mac]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Vm, <<"nics">>]) ->
    [<<"POST">>];

allowed_methods(_Version, _Token, [_Vm, <<"snapshots">>, _ID]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Vm, <<"snapshots">>]) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Vm, <<"backups">>, _ID]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Vm, <<"backups">>]) ->
    [<<"GET">>, <<"POST">>].

get(State = #state{path = [Vm, <<"backups">>, Snap]}) ->
    case wiggle_vm_handler:get(State#state{path=[Vm]}) of
        {ok, Obj} ->
            case jsxd:get([<<"backups">>, Snap], Obj) of
                undefined -> not_found;
                {ok, _} -> {ok, Obj}
            end;
        E ->
            E
    end;

get(State = #state{path = [Vm, <<"snapshots">>, Snap]}) ->
    case wiggle_vm_handler:get(State#state{path=[Vm]}) of
        {ok, Obj} ->
            case jsxd:get([<<"snapshots">>, Snap], Obj) of
                undefined -> not_found;
                {ok, _} -> {ok, Obj}
            end;
        E ->
            E
    end;

get(State = #state{path = [Vm, <<"nics">>, Mac]}) ->
    case wiggle_vm_handler:get(State#state{path=[Vm]}) of
        {ok, Obj} ->
            Macs = [jsxd:get([<<"mac">>], <<>>, N) ||
                       N <- jsxd:get([<<"config">>, <<"networks">>], [], Obj)],
            case lists:member(Mac, Macs) of
                true ->
                    {ok, Obj};
                _ ->
                    not_found
            end;
        E ->
            E
    end;

get(State = #state{path = [Vm | _]}) ->
    Start = now(),
    R = libsniffle:vm_get(Vm),
    ?MSniffle(?P(State), Start),
    R.

permission_required(#state{method = <<"GET">>, path = []}) ->
    {ok, [<<"cloud">>, <<"vms">>, <<"list">>]};

permission_required(#state{method = <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"vms">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [Vm]}) ->
    {ok, [<<"vms">>, Vm, <<"get">>]};

permission_required(#state{method = <<"DELETE">>, path = [Vm]}) ->
    {ok, [<<"vms">>, Vm, <<"delete">>]};

permission_required(#state{method = <<"POST">>, path = [Vm, <<"nics">>]}) ->
    {ok, [<<"vms">>, Vm, <<"edit">>]};

permission_required(#state{method = <<"PUT">>, path = [Vm, <<"nics">>, _]}) ->
    {ok, [<<"vms">>, Vm, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [Vm, <<"nics">>, _]}) ->
    {ok, [<<"vms">>, Vm, <<"edit">>]};

permission_required(#state{method = <<"GET">>, path = [Vm, <<"snapshots">>]}) ->
    {ok, [<<"vms">>, Vm, <<"get">>]};

permission_required(#state{method = <<"POST">>, path = [Vm, <<"snapshots">>]}) ->
    {ok, [<<"vms">>, Vm, <<"snapshot">>]};

permission_required(#state{method = <<"GET">>, path = [Vm, <<"snapshots">>, _Snap]}) ->
    {ok, [<<"vms">>, Vm, <<"get">>]};

permission_required(#state{method = <<"GET">>, path = [Vm, <<"backups">>]}) ->
    {ok, [<<"vms">>, Vm, <<"get">>]};

permission_required(#state{method = <<"POST">>, path = [Vm, <<"backups">>]}) ->
    {ok, [<<"vms">>, Vm, <<"backup">>]};

permission_required(#state{method = <<"GET">>, path = [Vm, <<"backups">>, _Snap]}) ->
    {ok, [<<"vms">>, Vm, <<"get">>]};

permission_required(#state{method = <<"PUT">>, path = [_Vm, <<"owner">>], body = undefiend}) ->
    {error, needs_decode};

permission_required(#state{method = <<"PUT">>, path = [Vm, <<"owner">>], body = Decoded}) ->
    case Decoded of
        [{<<"org">>, Owner}] ->
            {multiple,
             [[<<"vms">>, Vm, <<"edit">>],
              [<<"orgs">>, Owner, <<"edit">>]]};
        _ ->
            {ok, [<<"vms">>, Vm, <<"edit">>]}
    end;

permission_required(#state{method = <<"PUT">>, body = undefiend}) ->
    {error, needs_decode};

permission_required(#state{method = <<"PUT">>, body = Decoded, path = [Vm]}) ->
    case Decoded of
        [{<<"action">>, Act}] ->
            {ok, [<<"vms">>, Vm, Act]};
        _ ->
            {ok, [<<"vms">>, Vm, <<"edit">>]}
    end;

permission_required(#state{method = <<"PUT">>, body = Decoded,
                           path = [Vm, <<"snapshots">>, _Snap]}) ->
    case Decoded of
        [{<<"action">>, <<"rollback">>}] ->
            {ok, [<<"vms">>, Vm, <<"rollback">>]};
        _ ->
            {ok, [<<"vms">>, Vm, <<"edit">>]}
    end;

permission_required(#state{method = <<"DELETE">>, path = [Vm, <<"snapshots">>, _Snap]}) ->
    {ok, [<<"vms">>, Vm, <<"snapshot_delete">>]};

permission_required(#state{method = <<"PUT">>, body = Decoded,
                           path = [Vm, <<"backups">>, _Snap]}) ->
    case Decoded of
        [{<<"action">>, <<"rollback">>}|_] ->
            {ok, [<<"vms">>, Vm, <<"rollback">>]};
        _ ->
            {ok, [<<"vms">>, Vm, <<"edit">>]}
    end;

permission_required(#state{method = <<"DELETE">>, path = [Vm, <<"backups">>, _Snap]}) ->
    {ok, [<<"vms">>, Vm, <<"backup_delete">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [Vm, <<"metadata">> | _]}) ->
    {ok, [<<"vms">>, Vm, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [Vm, <<"metadata">> | _]}) ->
    {ok, [<<"vms">>, Vm, <<"edit">>]};

permission_required(_State) ->
    undefined.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

read(Req, State = #state{token = Token, path = []}) ->
    Start = now(),
    {ok, Permissions} = libsnarl:user_cache({token, Token}),
    ?MSnarl(?P(State), Start),
    Start1 = now(),
    {ok, Res} = libsniffle:vm_list([{must, 'allowed', [<<"vms">>, {<<"res">>, <<"uuid">>}, <<"get">>], Permissions}]),
    ?MSniffle(?P(State), Start1),
    {[ID || {_, ID} <- Res], Req, State};

read(Req, State = #state{path = [_Vm, <<"snapshots">>], obj = Obj}) ->
    Snaps = jsxd:fold(fun(UUID, Snap, Acc) ->
                              [jsxd:set(<<"uuid">>, UUID, Snap) | Acc]
                      end, [], jsxd:get(<<"snapshots">>, [], Obj)),
    {Snaps, Req, State};

read(Req, State = #state{path = [_Vm, <<"snapshots">>, Snap], obj = Obj}) ->
    case jsxd:get([<<"snapshots">>, Snap], null, Obj) of
        null ->
            {null, Req, State};
        SnapObj ->
            {jsxd:set(<<"uuid">>, Snap, SnapObj), Req, State}
    end;

read(Req, State = #state{path = [_Vm, <<"backups">>], obj = Obj}) ->
    Snaps = jsxd:fold(fun(UUID, Snap, Acc) ->
                              [jsxd:set(<<"uuid">>, UUID, Snap) | Acc]
                      end, [], jsxd:get(<<"backups">>, [], Obj)),
    {Snaps, Req, State};

read(Req, State = #state{path = [_Vm, <<"backups">>, Snap], obj = Obj}) ->
    case jsxd:get([<<"backups">>, Snap], null, Obj) of
        null ->
            {null, Req, State};
        SnapObj ->
            {jsxd:set(<<"uuid">>, Snap, SnapObj), Req, State}
    end;

read(Req, State = #state{path = [_Vm], obj = Obj}) ->
    {Obj, Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = Version, token = Token}, Decoded) ->
    try
        {ok, Dataset} = jsxd:get(<<"dataset">>, Decoded),
        {ok, Package} = jsxd:get(<<"package">>, Decoded),
        {ok, Config} = jsxd:get(<<"config">>, Decoded),
        %% If the creating user has advanced_create permissions they can pass
        %% 'requirements' as part of the config, if they lack the permission
        %% it simply gets removed.
        Config1 = case libsnarl:allowed(
                         {token, Token},
                         [<<"cloud">>, <<"vms">>, <<"advanced_create">>]) of
            true ->
                Config;
            _ ->
                jsxd:set(<<"requirements">>, [], Config)
        end,
        try
            {ok, User} = libsnarl:user_get({token, Token}),
            {ok, Owner} = jsxd:get(<<"uuid">>, User),
            Start = now(),
            {ok, UUID} = libsniffle:create(Package, Dataset, jsxd:set(<<"owner">>, Owner, Config1)),
            ?MSniffle(?P(State), Start),
            {{true, <<"/api/", Version/binary, "/vms/", UUID/binary>>}, Req, State#state{body = Decoded}}
        catch
            G:E ->
                lager:error("Error creating VM(~p): ~p / ~p", [Decoded, G, E]),
                {ok, Req1} = cowboy_req:reply(500, Req),
                {halt, Req1, State}
        end
    catch
        G1:E1 ->
            lager:error("Error creating VM(~p): ~p / ~p", [Decoded, G1, E1]),
            {ok, Req2} = cowboy_req:reply(400, Req),
            {halt, Req2, State}
    end;

create(Req, State = #state{path = [Vm, <<"snapshots">>], version = Version}, Decoded) ->
    Comment = jsxd:get(<<"comment">>, <<"">>, Decoded),
    Start = now(),
    {ok, UUID} = libsniffle:vm_snapshot(Vm, Comment),
    ?MSniffle(?P(State), Start),
    {{true, <<"/api/", Version/binary, "/vms/", Vm/binary, "/snapshots/", UUID/binary>>}, Req, State#state{body = Decoded}};

create(Req, State = #state{path = [Vm, <<"backups">>], version = Version}, Decoded) ->
    Comment = jsxd:get(<<"comment">>, <<"">>, Decoded),
    Opts = case jsxd:get(<<"xml">>, false, Decoded) of
               true ->
                   [xml];
               false ->
                   []
           end,
    Start = now(),
    {ok, UUID} = case jsxd:get(<<"parent">>, Decoded) of
                     {ok, Parent} ->
                         Opts1 = case jsxd:get(<<"delete">>, false, Decoded) of
                                     true ->
                                         [{delete, parent} | Opts];
                                     false ->
                                         Opts
                                 end,
                         libsniffle:vm_incremental_backup(Vm, Parent, Comment,
                                                          Opts1);
                     _ ->
                         Opts1 = case jsxd:get(<<"delete">>, false, Decoded) of
                                     true ->
                                         [delete | Opts];
                                     false ->
                                         Opts
                                 end,
                         libsniffle:vm_full_backup(Vm, Comment, Opts1)
                 end,
    ?MSniffle(?P(State), Start),
    {{true, <<"/api/", Version/binary, "/vms/", Vm/binary, "/backups/", UUID/binary>>}, Req, State#state{body = Decoded}};

create(Req, State = #state{path = [Vm, <<"nics">>], version = Version}, Decoded) ->
    {ok, Network} = jsxd:get(<<"network">>, Decoded),
    Start = now(),
    case libsniffle:vm_add_nic(Vm, Network) of
        ok ->
            ?MSniffle(?P(State), Start),
            {{true, <<"/api/", Version/binary, "/vms/", Vm/binary>>},
             Req, State#state{body = Decoded}};
        E ->
            ?MSniffle(?P(State), Start),
            lager:error("Error adding nic to VM(~p) on network(~p) / ~p", [Vm, Network, E]),
            {ok, Req1} = cowboy_req:reply(500, Req),
            lager:error("Could not add nic: ~P"),
            {halt, Req1, State}
    end.


write(Req, State = #state{path = [_, <<"nics">>]}, _Body) ->
    {true, Req, State};

write(Req, State = #state{path = [Vm, <<"owner">>]}, [{<<"org">>, Org}]) ->
    Start = now(),
    case libsnarl:org_get(Org) of
        {ok, _} ->
            R = libsniffle:vm_owner(Vm, Org),
            ?MSniffle(?P(State), Start),
            {R =:= ok, Req, State};
        _ ->
            ?MSniffle(?P(State), Start),
            lager:error("Error trying to assign org ~p since it does not "
                        "seem to exist", [Org]),
            {ok, Req1} = cowboy_req:reply(500, Req),
            lager:error("Could not add nic: ~P"),
            {halt, Req1, State}
    end;

write(Req, State = #state{path = [Vm, <<"nics">>, Mac]}, [{<<"primary">>, true}]) ->
    ?LIB(libsniffle:vm_primary_nic(Vm, Mac));

write(Req, State = #state{path = [Vm, <<"metadata">> | Path]}, [{K, V}]) ->
    ?LIB(libsniffle:vm_set(Vm, [<<"metadata">> | Path] ++ [K],
                           jsxd:from_list(V)));


write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"start">>}]) ->
    ?LIB(libsniffle:vm_start(Vm));

write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"stop">>}]) ->
    ?LIB(libsniffle:vm_stop(Vm));

write(Req, State = #state{path = [Vm]},
      [{<<"action">>, <<"stop">>}, {<<"force">>, true}]) ->
    ?LIB(libsniffle:vm_stop(Vm, [force]));

write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"reboot">>}]) ->
    ?LIB(libsniffle:vm_reboot(Vm));

write(Req, State = #state{path = [Vm]},
      [{<<"action">>, <<"reboot">>}, {<<"force">>, true}]) ->
    ?LIB(libsniffle:vm_reboot(Vm, [force]));

write(Req, State = #state{path = [Vm]}, [{<<"config">>, Config},
                                         {<<"package">>, Package}]) ->
    ?LIB(libsniffle:vm_update(Vm, Package, Config));

write(Req, State = #state{path = [Vm]}, [{<<"config">>, Config}]) ->
    ?LIB(libsniffle:vm_update(Vm, undefined, Config));

write(Req, State = #state{path = [Vm]}, [{<<"package">>, Package}]) ->
    ?LIB(libsniffle:vm_update(Vm, Package, []));

write(Req, State = #state{path = []}, _Body) ->
    {true, Req, State};

write(Req, State = #state{path = [_Vm, <<"snapshots">>]}, _Body) ->
    {true, Req, State};

write(Req, State = #state{path = [Vm, <<"snapshots">>, UUID]},
      [{<<"action">>, <<"rollback">>}]) ->
    ?LIB(libsniffle:vm_rollback_snapshot(Vm, UUID));

write(Req, State = #state{path = [_Vm, <<"backups">>]}, _Body) ->
    {true, Req, State};

write(Req, State = #state{path = [Vm, <<"backups">>, UUID]},
      [{<<"action">>, <<"rollback">>},
       {<<"hypervisor">>, Hypervisor}]) ->
    ?LIB(libsniffle:vm_restore_backup(Vm, UUID, Hypervisor));
write(Req, State = #state{path = [Vm, <<"backups">>, UUID]},
      [{<<"action">>, <<"rollback">>}]) ->
    ?LIB(libsniffle:vm_restore_backup(Vm, UUID));

write(Req, State, _Body) ->
    lager:error("Unknown PUT request: ~p~n.", [State]),
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [Vm, <<"snapshots">>, UUID]}) ->
    Start = now(),
    ok = libsniffle:vm_delete_snapshot(Vm, UUID),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Vm, <<"backups">>, UUID],
                           body=[{<<"location">>, <<"hypervisor">>}]}) ->
    Start = now(),
    ok = libsniffle:vm_delete_backup(Vm, UUID, hypervisor),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Vm, <<"backups">>, UUID]}) ->
    Start = now(),
    ok = libsniffle:vm_delete_backup(Vm, UUID, cloud),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Vm, <<"nics">>, Mac]}) ->
    Start = now(),
    ok = libsniffle:vm_remove_nic(Vm, Mac),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Vm],
                           body=[{<<"location">>, <<"hypervisor">>}]}) ->
    Start = now(),
    ok = libsniffle:vm_store(Vm),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Vm]}) ->
    Start = now(),
    ok = libsniffle:vm_delete(Vm),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Vm, <<"metadata">> | Path]}) ->
    Start = now(),
    libsniffle:vm_set(Vm, [<<"metadata">> | Path], delete),
    ?MSniffle(?P(State), Start),
    {true, Req, State}.

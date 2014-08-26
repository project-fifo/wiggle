-module(wiggle_vm_handler).

-include("wiggle.hrl").

-define(CACHE, vm).
-define(LIST_CACHE, vm_list).
-define(FULL_CACHE, vm_full_list).

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

allowed_methods(_Version, _Token, [?UUID(_Vm)]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [<<"dry_run">>]) ->
    [<<"PUT">>];

allowed_methods(_Version, _Token, [?UUID(_Vm), <<"hypervisor">>]) ->
    [<<"DELETE">>];

allowed_methods(_Version, _Token, [?UUID(_Vm), <<"owner">>]) ->
    [<<"PUT">>];

allowed_methods(_Version, _Token, [?UUID(_Vm), <<"services">>]) ->
    [<<"PUT">>, <<"GET">>];

allowed_methods(_Version, _Token, [?UUID(_Vm), <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [?UUID(_Vm), <<"nics">>, _Mac]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [?UUID(_Vm), <<"nics">>]) ->
    [<<"POST">>];

allowed_methods(_Version, _Token, [?UUID(_Vm), <<"snapshots">>, _ID]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [?UUID(_Vm), <<"snapshots">>]) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [?UUID(_Vm), <<"backups">>, _ID]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [?UUID(_Vm), <<"backups">>]) ->
    [<<"GET">>, <<"POST">>].

get(State = #state{path = [?UUID(Vm), <<"backups">>, Snap]}) ->
    case wiggle_vm_handler:get(State#state{path=[?UUID(Vm)]}) of
        {ok, Obj} ->
            case jsxd:get([Snap], ft_vm:backups(Obj)) of
                undefined -> not_found;
                {ok, _} -> {ok, Obj}
            end;
        E ->
            E
    end;

get(State = #state{path = [?UUID(Vm), <<"snapshots">>, Snap]}) ->
    case wiggle_vm_handler:get(State#state{path=[?UUID(Vm)]}) of
        {ok, Obj} ->
            case jsxd:get([Snap], ft_vm:snapshots(Obj)) of
                undefined -> not_found;
                {ok, _} -> {ok, Obj}
            end;
        E ->
            E
    end;

get(State = #state{path = [?UUID(Vm), <<"nics">>, Mac]}) ->
    case wiggle_vm_handler:get(State#state{path=[?UUID(Vm)]}) of
        {ok, Obj} ->
            Macs = [jsxd:get([<<"mac">>], <<>>, N) ||
                       N <- jsxd:get([<<"networks">>], [], ft_vm:config(Obj))],
            case lists:member(Mac, Macs) of
                true ->
                    {ok, Obj};
                _ ->
                    not_found
            end;
        E ->
            E
    end;

get(State = #state{path = [?UUID(Vm) | _]}) ->
    Start = now(),
    R = case application:get_env(wiggle, vm_ttl) of
            {ok, {TTL1, TTL2}} ->
                wiggle_handler:timeout_cache_with_invalid(
                  ?CACHE, Vm, TTL1, TTL2, not_found,
                  fun() -> ls_vm:get(Vm) end);
            _ ->
                ls_vm:get(Vm)
        end,
    ?MSniffle(?P(State), Start),
    R;

get(_State) ->
    not_found.


permission_required(#state{method = <<"PUT">>, path = [<<"dry_run">>]}) ->
    {ok, [<<"cloud">>, <<"vms">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = []}) ->
    {ok, [<<"cloud">>, <<"vms">>, <<"list">>]};

permission_required(#state{method = <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"vms">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [?UUID(Vm)]}) ->
    {ok, [<<"vms">>, Vm, <<"get">>]};


permission_required(#state{method = <<"DELETE">>, path = [?UUID(Vm)]}) ->
    {ok, [<<"vms">>, Vm, <<"delete">>]};

permission_required(#state{method = <<"DELETE">>, path = [?UUID(Vm), <<"hypervisor">>]}) ->
    {ok, [<<"vms">>, Vm, <<"delete">>]};

permission_required(#state{method = <<"POST">>, path = [?UUID(Vm), <<"nics">>]}) ->
    {ok, [<<"vms">>, Vm, <<"edit">>]};

permission_required(#state{method = <<"PUT">>, path = [?UUID(Vm), <<"nics">>, _]}) ->
    {ok, [<<"vms">>, Vm, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [?UUID(Vm), <<"nics">>, _]}) ->
    {ok, [<<"vms">>, Vm, <<"edit">>]};

permission_required(#state{method = <<"GET">>, path = [?UUID(Vm), <<"snapshots">>]}) ->
    {ok, [<<"vms">>, Vm, <<"get">>]};

permission_required(#state{method = <<"POST">>, path = [?UUID(Vm), <<"snapshots">>]}) ->
    {ok, [<<"vms">>, Vm, <<"snapshot">>]};

permission_required(#state{method = <<"GET">>, path = [?UUID(Vm), <<"services">>]}) ->
    {ok, [<<"vms">>, Vm, <<"get">>]};

permission_required(#state{method = <<"PUT">>, path = [?UUID(Vm), <<"services">>]}) ->
    {ok, [<<"vms">>, Vm, <<"edit">>]};

permission_required(#state{method = <<"GET">>, path = [?UUID(Vm), <<"snapshots">>, _Snap]}) ->
    {ok, [<<"vms">>, Vm, <<"get">>]};

permission_required(#state{method = <<"GET">>, path = [?UUID(Vm), <<"backups">>]}) ->
    {ok, [<<"vms">>, Vm, <<"get">>]};

permission_required(#state{method = <<"POST">>, path = [?UUID(Vm), <<"backups">>]}) ->
    {ok, [<<"vms">>, Vm, <<"backup">>]};

permission_required(#state{method = <<"GET">>, path = [?UUID(Vm), <<"backups">>, _Snap]}) ->
    {ok, [<<"vms">>, Vm, <<"get">>]};

permission_required(#state{method = <<"PUT">>, path = [?UUID(_Vm), <<"owner">>], body = undefiend}) ->
    {error, needs_decode};

permission_required(#state{method = <<"PUT">>, path = [?UUID(Vm), <<"owner">>], body = Decoded}) ->
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

permission_required(#state{method = <<"PUT">>, body = Decoded, path = [?UUID(Vm)]}) ->
    case Decoded of
        [{<<"action">>, Act}] ->
            {ok, [<<"vms">>, Vm, Act]};
        _ ->
            {ok, [<<"vms">>, Vm, <<"edit">>]}
    end;

permission_required(#state{method = <<"PUT">>, body = Decoded,
                           path = [?UUID(Vm), <<"snapshots">>, _Snap]}) ->
    case Decoded of
        [{<<"action">>, <<"rollback">>}] ->
            {ok, [<<"vms">>, Vm, <<"rollback">>]};
        _ ->
            {ok, [<<"vms">>, Vm, <<"edit">>]}
    end;

permission_required(#state{method = <<"DELETE">>, path = [?UUID(Vm), <<"snapshots">>, _Snap]}) ->
    {ok, [<<"vms">>, Vm, <<"snapshot_delete">>]};

permission_required(#state{method = <<"PUT">>, body = Decoded,
                           path = [?UUID(Vm), <<"backups">>, _Snap]}) ->
    case Decoded of
        [{<<"action">>, <<"rollback">>}|_] ->
            {ok, [<<"vms">>, Vm, <<"rollback">>]};
        _ ->
            {ok, [<<"vms">>, Vm, <<"edit">>]}
    end;

permission_required(#state{method = <<"DELETE">>, path = [?UUID(Vm), <<"backups">>, _Snap]}) ->
    {ok, [<<"vms">>, Vm, <<"backup_delete">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [?UUID(Vm), <<"metadata">> | _]}) ->
    {ok, [<<"vms">>, Vm, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [?UUID(Vm), <<"metadata">> | _]}) ->
    {ok, [<<"vms">>, Vm, <<"edit">>]};

permission_required(_State) ->
    undefined.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

read(Req, State = #state{token = Token, path = [], full_list=FullList, full_list_fields=Filter}) ->
    Start = now(),
    {ok, Permissions} = wiggle_handler:get_persmissions(Token),
    ?MSnarl(?P(State), Start),
    Start1 = now(),
    Permission = [{must, 'allowed',
                   [<<"vms">>, {<<"res">>, <<"uuid">>}, <<"get">>],
                   Permissions}],
    Res = wiggle_handler:list(fun ls_vm:list/2,
                              fun ft_vm:to_json/1, Token, Permission,
                              FullList, Filter, vm_list_ttl, ?FULL_CACHE,
                              ?LIST_CACHE),
    ?MSniffle(?P(State), Start1),
    {Res, Req, State};

read(Req, State = #state{path = [?UUID(_Vm), <<"snapshots">>], obj = Obj}) ->
    Snaps = jsxd:fold(fun(UUID, Snap, Acc) ->
                              [jsxd:set(<<"uuid">>, UUID, Snap) | Acc]
                      end, [], ft_vm:snapshots(Obj)),
    {Snaps, Req, State};

read(Req, State = #state{path = [?UUID(_Vm), <<"snapshots">>, SnapID], obj = Obj}) ->
    case jsxd:get([SnapID], ft_vm:snapshots(Obj)) of
        {ok, SnapObj} ->
            {jsxd:set(<<"uuid">>, SnapID, SnapObj), Req, State};
        _ ->
            {null, Req, State}

    end;

read(Req, State = #state{path = [?UUID(_Vm), <<"backups">>], obj = Obj}) ->
    Snaps = jsxd:fold(fun(UUID, Snap, Acc) ->
                              [jsxd:set(<<"uuid">>, UUID, Snap) | Acc]
                      end, [], ft_vm:backups(Obj)),
    {Snaps, Req, State};

read(Req, State = #state{path = [?UUID(_Vm), <<"backups">>, SnapID], obj = Obj}) ->
    case jsxd:get([SnapID], ft_vm:backups(Obj)) of
        {ok, SnapObj} ->
            {jsxd:set(<<"uuid">>, SnapID, SnapObj), Req, State};
        _ ->
            {null, Req, State}

    end;
read(Req, State = #state{path = [?UUID(_Vm), <<"services">>], obj = Obj}) ->
    {ft_vm:services(Obj), Req, State};

read(Req, State = #state{path = [?UUID(_Vm), <<"services">>, Service], obj = Obj}) ->
    {jsxd:get([Service], [{}], ft_vm:services(Obj)), Req, State};

read(Req, State = #state{path = [?UUID(_Vm)], obj = Obj}) ->
    {ft_vm:to_json(Obj), Req, State}.

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
                         Token,
                         [<<"cloud">>, <<"vms">>, <<"advanced_create">>]) of
                      true ->
                          Config;
                      _ ->
                          jsxd:set(<<"requirements">>, [], Config)
                  end,
        try
            {ok, User} = ls_user:get(Token),
            {ok, Owner} = jsxd:get(<<"uuid">>, User),
            Start = now(),
            {ok, UUID} = ls_vm:create(Package, Dataset, jsxd:set(<<"owner">>, Owner, Config1)),
            e2qc:teardown(?LIST_CACHE),
            e2qc:teardown(?FULL_CACHE),
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

create(Req, State = #state{path = [?UUID(Vm), <<"snapshots">>], version = Version}, Decoded) ->
    Comment = jsxd:get(<<"comment">>, <<"">>, Decoded),
    Start = now(),
    {ok, UUID} = ls_vm:snapshot(Vm, Comment),
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {{true, <<"/api/", Version/binary, "/vms/", Vm/binary, "/snapshots/", UUID/binary>>}, Req, State#state{body = Decoded}};

create(Req, State = #state{path = [?UUID(Vm), <<"backups">>], version = Version}, Decoded) ->
    Comment = jsxd:get(<<"comment">>, <<"">>, Decoded),
    Opts = case jsxd:get(<<"xml">>, true, Decoded) of
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
                         e2qc:evict(?CACHE, Vm),
                         e2qc:teardown(?FULL_CACHE),
                         ls_vm:incremental_backup(Vm, Parent, Comment,
                                                          Opts1);
                     _ ->
                         Opts1 = case jsxd:get(<<"delete">>, false, Decoded) of
                                     true ->
                                         [delete | Opts];
                                     false ->
                                         Opts
                                 end,
                         ls_vm:full_backup(Vm, Comment, Opts1)
                 end,
    ?MSniffle(?P(State), Start),
    {{true, <<"/api/", Version/binary, "/vms/", Vm/binary, "/backups/", UUID/binary>>}, Req, State#state{body = Decoded}};


create(Req, State = #state{path = [?UUID(Vm), <<"nics">>], version = Version}, Decoded) ->
    {ok, Network} = jsxd:get(<<"network">>, Decoded),
    Start = now(),
    case ls_vm:add_nic(Vm, Network) of
        ok ->
            ?MSniffle(?P(State), Start),
            e2qc:evict(?CACHE, Vm),
            e2qc:teardown(?FULL_CACHE),
            {{true, <<"/api/", Version/binary, "/vms/", Vm/binary>>},
             Req, State#state{body = Decoded}};
        E ->
            ?MSniffle(?P(State), Start),
            lager:error("Error adding nic to VM(~p) on network(~p) / ~p", [?UUID(Vm), Network, E]),
            {ok, Req1} = cowboy_req:reply(500, Req),
            lager:error("Could not add nic: ~P"),
            {halt, Req1, State}
    end.

write(Req, State = #state{path = [<<"dry_run">>], token = Token}, Decoded) ->
    lager:info("Starting dryrun."),
    try
        {ok, Dataset} = jsxd:get(<<"dataset">>, Decoded),
        {ok, Package} = jsxd:get(<<"package">>, Decoded),
        {ok, Config} = jsxd:get(<<"config">>, Decoded),
        %% If the creating user has advanced_create permissions they can pass
        %% 'requirements' as part of the config, if they lack the permission
        %% it simply gets removed.
        Config1 = case libsnarl:allowed(
                         Token,
                         [<<"cloud">>, <<"vms">>, <<"advanced_create">>]) of
                      true ->
                          Config;
                      _ ->
                          jsxd:set(<<"requirements">>, [], Config)
                  end,
        try
            {ok, User} = ls_user:get(Token),
            {ok, Owner} = jsxd:get(<<"uuid">>, User),
            Start = now(),
            case ls_vm:dry_run(Package, Dataset,
                                    jsxd:set(<<"owner">>, Owner, Config1)) of
                {ok, success} ->
                    {true, Req, State#state{body = Decoded}};
                E ->
                    lager:warning("Dry run failed with: ~p.", [E]),
                    {false, Req, State#state{body = Decoded}}
            end
        catch
            _G:_E ->
                {false, Req, State}
        end
    catch
        _G1:_E1 ->
            {false, Req, State}
    end;

write(Req, State = #state{path = [?UUID(Vm), <<"services">>]},
      [{<<"action">>, <<"enable">>},
       {<<"service">>, Service}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ls_vm:service_enable(Vm, Service),
    {true, Req, State};

write(Req, State = #state{path = [?UUID(Vm), <<"services">>]},
      [{<<"action">>, <<"disable">>},
       {<<"service">>, Service}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ls_vm:service_disable(Vm, Service),
    {true, Req, State};

write(Req, State = #state{path = [?UUID(Vm), <<"services">>]},
      [{<<"action">>, <<"clear">>},
       {<<"service">>, Service}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ls_vm:service_clear(Vm, Service),
    {true, Req, State};


write(Req, State = #state{path = [_, <<"nics">>]}, _Body) ->
    {true, Req, State};

write(Req, State = #state{path = [?UUID(Vm), <<"owner">>]}, [{<<"org">>, Org}]) ->
    Start = now(),
    case ls_org:get(Org) of
        {ok, _} ->
            e2qc:evict(?CACHE, Vm),
            e2qc:teardown(?FULL_CACHE),
            R = ls_vm:owner(Vm, Org),
            ?MSniffle(?P(State), Start),
            {R =:= ok, Req, State};
        E ->
            ?MSniffle(?P(State), Start),
            lager:error("Error trying to assign org ~p since it does not "
                        "seem to exist", [Org]),
            {ok, Req1} = cowboy_req:reply(500, Req),
            lager:error("Could not add nic: ~p", [E]),
            {halt, Req1, State}
    end;

write(Req, State = #state{path = [?UUID(Vm), <<"nics">>, Mac]}, [{<<"primary">>, true}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?LIB(ls_vm:primary_nic(Vm, Mac));

write(Req, State = #state{path = [?UUID(Vm), <<"metadata">> | Path]}, [{K, V}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?LIB(ls_vm:set_metadata(Vm,  [{Path ++ [K],
                                   jsxd:from_list(V)}]));


write(Req, State = #state{path = [?UUID(Vm)]}, [{<<"action">>, <<"start">>}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?LIB(ls_vm:start(Vm));

write(Req, State = #state{path = [?UUID(Vm)]}, [{<<"action">>, <<"stop">>}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?LIB(ls_vm:stop(Vm));

write(Req, State = #state{path = [?UUID(Vm)]},
      [{<<"action">>, <<"stop">>}, {<<"force">>, true}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?LIB(ls_vm:stop(Vm, [force]));

write(Req, State = #state{path = [?UUID(Vm)]}, [{<<"action">>, <<"reboot">>}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?LIB(ls_vm:reboot(Vm));

write(Req, State = #state{path = [?UUID(Vm)]},
      [{<<"action">>, <<"reboot">>}, {<<"force">>, true}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?LIB(ls_vm:reboot(Vm, [force]));

write(Req, State = #state{path = [?UUID(Vm)]}, [{<<"config">>, Config},
                                         {<<"package">>, Package}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?LIB(ls_vm:update(Vm, Package, Config));

write(Req, State = #state{path = [?UUID(Vm)]}, [{<<"config">>, Config}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?LIB(ls_vm:update(Vm, undefined, Config));

write(Req, State = #state{path = [?UUID(Vm)]}, [{<<"package">>, Package}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?LIB(ls_vm:update(Vm, Package, []));

write(Req, State = #state{path = []}, _Body) ->
    {true, Req, State};

write(Req, State = #state{path = [?UUID(_Vm), <<"snapshots">>]}, _Body) ->
    {true, Req, State};

write(Req, State = #state{path = [?UUID(Vm), <<"snapshots">>, UUID]},
      [{<<"action">>, <<"rollback">>}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?LIB(ls_vm:rollback_snapshot(Vm, UUID));

write(Req, State = #state{path = [?UUID(_Vm), <<"backups">>]}, _Body) ->
    {true, Req, State};

write(Req, State = #state{path = [?UUID(Vm), <<"backups">>, UUID]},
      [{<<"action">>, <<"rollback">>},
       {<<"hypervisor">>, Hypervisor}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?LIB(ls_vm:restore_backup(Vm, UUID, Hypervisor));

write(Req, State = #state{path = [?UUID(Vm), <<"backups">>, UUID]},
      [{<<"action">>, <<"rollback">>}]) ->
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?LIB(ls_vm:restore_backup(Vm, UUID));

write(Req, State, _Body) ->
    lager:error("Unknown PUT request: ~p~n.", [State]),
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [?UUID(Vm), <<"snapshots">>, UUID]}) ->
    Start = now(),
    ok = ls_vm:delete_snapshot(Vm, UUID),
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [?UUID(Vm), <<"backups">>, UUID],
                           body=[{<<"location">>, <<"hypervisor">>}]}) ->
    Start = now(),
    ok = ls_vm:delete_backup(Vm, UUID, hypervisor),
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [?UUID(Vm), <<"backups">>, UUID]}) ->
    Start = now(),
    ok = ls_vm:delete_backup(Vm, UUID, cloud),
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [?UUID(Vm), <<"nics">>, Mac]}) ->
    Start = now(),
    ok = ls_vm:remove_nic(Vm, Mac),
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [?UUID(Vm)],
                           body=[{<<"location">>, <<"hypervisor">>}]}) ->
    Start = now(),
    ok = ls_vm:store(Vm),
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [?UUID(Vm), <<"hypervisor">>]}) ->
    Start = now(),
    ok = ls_vm:store(Vm),
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [?UUID(Vm)]}) ->
    Start = now(),
    ok = ls_vm:delete(Vm),
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?LIST_CACHE),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [?UUID(Vm), <<"metadata">> | Path]}) ->
    Start = now(),
    ls_vm:set_metadata(Vm, [{Path, delete}]),
    e2qc:evict(?CACHE, Vm),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State}.

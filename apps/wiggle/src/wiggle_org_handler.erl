-module(wiggle_org_handler).
-include("wiggle.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([allowed_methods/3,
         get/1,
         trigger_required/1,
         read/2,
         create/3,
         write/3,
         delete/2]).

-ignore_xref([allowed_methods/3,
              get/1,
              trigger_required/1,
              read/2,
              create/3,
              write/3,
              delete/2]).

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Org]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Org, <<"triggers">>]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Org, <<"metadata">> | _]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Org, <<"triggers">> | _Trigger]) ->
    [<<"PUT">>, <<"DELETE">>].

get(State = #state{path = [Org, <<"triggers">> | Trigger]}) ->
    case {erlangify_trigger(Trigger), wiggle_org_handler:get(State#state{path = [Org]})} of
        {_, not_found} ->
            not_found;
        {[], {ok, Obj}} ->
            {ok, Obj};
        {P, {ok, Obj}} ->
            case lists:member(P, jsxd:get(<<"triggers">>, [], Obj)) of
                true ->
                    {ok, Obj};
                _ -> not_found
            end
    end;

get(State = #state{path = [Org | _]}) ->
    Start = now(),
    R = libsnarl:org_get(Org),
    ?MSnarl(?P(State), Start),
    R.

permission_required(#state{method = <<"GET">>, path = []}) ->
    {ok, [<<"cloud">>, <<"orgs">>, <<"list">>]};

permission_required(#state{method = <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"orgs">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [Org]}) ->
    {ok, [<<"orgs">>, Org, <<"get">>]};

permission_required(#state{method = <<"PUT">>, path = [Org]}) ->
    {ok, [<<"orgs">>, Org, <<"create">>]};

permission_required(#state{method = <<"DELETE">>, path = [Org]}) ->
    {ok, [<<"orgs">>, Org, <<"delete">>]};

permission_required(#state{method = <<"GET">>, path = [Org, <<"triggers">>]}) ->
    {ok, [<<"orgs">>, Org, <<"get">>]};

permission_required(#state{method = <<"PUT">>, path = [Org, <<"triggers">> | Trigger]}) ->
    P = erlangify_trigger(Trigger),
    {multiple, [[<<"orgs">>, Org, <<"grant">>],
                [<<"triggers">>, P, <<"revoke">>]]};

permission_required(#state{method = <<"DELETE">>, path = [Org, <<"triggers">> | Trigger]}) ->
    P = erlangify_trigger(Trigger),
    {multiple, [[<<"orgs">>, Org, <<"revoke">>],
                [<<"triggers">>, P, <<"revoke">>]]};

permission_required(#state{method = <<"PUT">>, path = [Org, <<"metadata">> | _]}) ->
    {ok, [<<"orgs">>, Org, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [Org, <<"metadata">> | _]}) ->
    {ok, [<<"orgs">>, Org, <<"edit">>]};

permission_required(_State) ->
    undefined.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

read(Req, State = #state{path = []}) ->
    Start = now(),
    %%    {ok, Triggers} = libsnarl:user_cache({token, Token}),
    {ok, Res} = libsnarl:org_list(), %{must, 'allowed', [<<"vm">>, {<<"res">>, <<"uuid">>}, <<"get">>], Triggers}),
    ?MSnarl(?P(State), Start),
    {Res, Req, State};

read(Req, State = #state{path = [_Org], obj = OrgObj}) ->
    OrgObj1 = jsxd:update(<<"triggers">>,
                            fun (Triggers) ->
                                    lists:map(fun jsonify_trigger/1, Triggers)
                            end, [], OrgObj),
    {OrgObj1, Req, State};

read(Req, State = #state{path = [_Org, <<"triggers">>], obj = OrgObj}) ->
    {lists:map(fun jsonify_trigger/1, jsxd:get(<<"triggers">>, [], OrgObj)), Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = Version}, Decoded) ->
    {ok, Org} = jsxd:get(<<"name">>, Decoded),
    Start = now(),
    {ok, UUID} = libsnarl:org_add(Org),
    ?MSnarl(?P(State), Start),
    {{true, <<"/api/", Version/binary, "/orgs/", UUID/binary>>}, Req, State#state{body = Decoded}}.

write(Req, State = #state{path = [Org, <<"metadata">> | Path]}, [{K, V}]) when is_binary(Org) ->
    Start = now(),
    libsnarl:org_set(Org, Path ++ [K], jsxd:from_list(V)),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [Org]}, _Body) ->
    Start = now(),
    ok = libsnarl:org_add(Org),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [Org, <<"triggers">> | Trigger]}, _Body) ->
    P = erlangify_trigger(Trigger),
    Start = now(),
    ok = libsnarl:org_add_trigger(Org, P),
    ?MSnarl(?P(State), Start),
    {true, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [Org, <<"metadata">> | Path]}) ->
    Start = now(),
    libsnarl:org_set(Org, Path, delete),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Org, <<"triggers">> | Trigger]}) ->
    P = erlangify_trigger(Trigger),
    Start = now(),
    ok = libsnarl:org_remove_trigger(Org, P),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Org]}) ->
    Start = now(),
    ok = libsnarl:org_delete(Org),
    ?MSnarl(?P(State), Start),
    {true, Req, State}.

%% Internal Functions

erlangify_trigger([Group | Permission]) ->
    {vm_create,
     {grant, group, Group,
      [<<"vms">>, placeholder | Permission]}}.

jsonify_trigger({vm_create,
                  {grant, group, Group,
                   [<<"vms">>, placeholder | Permission]}}) ->
    [{<<"group">>, Group},
     {<<"permission">>, Permission}].

-ifdef(TEST).

-endif.

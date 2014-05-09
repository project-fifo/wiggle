-module(wiggle_org_handler).
-include("wiggle.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Org]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Org, <<"triggers">>]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Org, <<"triggers">> | _Trigger]) ->
    [<<"POST">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Org, <<"metadata">> | _]) ->
    [<<"PUT">>, <<"DELETE">>].

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

permission_required(#state{method = <<"POST">>,
                           path = [_Org, <<"triggers">> | _],
                           body = undefined}) ->
    {error, needs_decode};

permission_required(#state{method = <<"POST">>,
                           path = [Org, <<"triggers">> | _],
                           body = [{<<"action">>, <<"role_grant">>},
                                   {<<"base">>, _},
                                   {<<"permission">>, _},
                                   {<<"target">>, Role}]}) ->
    {multiple, [[<<"orgs">>, Org, <<"edit">>],
                [<<"roles">>, Role, <<"grant">>]]};

permission_required(#state{method = <<"POST">>,
                           path = [Org, <<"triggers">> | _],
                           body = [{<<"action">>, <<"user_grant">>},
                                   {<<"base">>, _Base},
                                   {<<"permission">>, _Permission},
                                   {<<"target">>, User}]}) ->
    {multiple, [[<<"orgs">>, Org, <<"edit">>],
                [<<"users">>, User, <<"grant">>]]};

permission_required(#state{method = <<"POST">>,
                           path = [Org, <<"triggers">> | _],
                           body = [{<<"action">>, <<"join_role">>},
                                   {<<"target">>, Role}]}) ->
    {multiple, [[<<"orgs">>, Org, <<"edit">>],
                [<<"roles">>, Role, <<"join">>]]};

permission_required(#state{
                       method = <<"POST">>,
                       path = [Org, <<"triggers">> | _],
                       body = ([{<<"action">>, <<"join_org">>},
                                {<<"target">>, TargetOrg}])}) ->
    {multiple, [[<<"orgs">>, Org, <<"edit">>],
                [<<"orgs">>, TargetOrg, <<"join">>]]};

permission_required(#state{method = <<"DELETE">>,
                           path = [Org, <<"triggers">> | _]}) ->
    {ok, [<<"orgs">>, Org, <<"edit">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [Org, <<"metadata">> | _]}) ->
    {ok, [<<"orgs">>, Org, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>,
                           path = [Org, <<"metadata">> | _]}) ->
    {ok, [<<"orgs">>, Org, <<"edit">>]};

permission_required(State) ->
    lager:warning("Unknown permission request: ~p.", [State]),
    undefined.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

read(Req, State = #state{token = Token, path = [], full_list=FullList, full_list_fields=Filter}) ->
    Start = now(),
    {ok, Permissions} = wiggle_handler:get_persmissions(Token),
    ?MSnarl(?P(State), Start),
    Start1 = now(),
    {ok, Res} = libsnarl:org_list(
                  [{must, 'allowed',
                    [<<"orgs">>, {<<"res">>, <<"uuid">>}, <<"get">>],
                    Permissions}], FullList),
    ?MSnarl(?P(State), Start1),
    Res1 = case {Filter, FullList} of
               {_, false} ->
                   [ID || {_, ID} <- Res];
               {[], _} ->
                   [ID || {_, ID} <- Res];
               _ ->
                   [jsxd:select(Filter, ID) || {_, ID} <- Res]
           end,
    {Res1, Req, State};

read(Req, State = #state{path = [_Org], obj = OrgObj}) ->
    {OrgObj, Req, State};

read(Req, State = #state{path = [_Org, <<"triggers">>], obj = OrgObj}) ->
    {jsxd:get(<<"triggers">>, [], OrgObj), Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = Version}, Decoded) ->
    {ok, Org} = jsxd:get(<<"name">>, Decoded),
    Start = now(),
    {ok, UUID} = libsnarl:org_add(Org),
    ?MSnarl(?P(State), Start),
    {{true, <<"/api/", Version/binary, "/orgs/", UUID/binary>>},
     Req, State#state{body = Decoded}};

create(Req, State =
           #state{
              path = [Org, <<"triggers">>, Trigger],
              version = Version
             }, Event) ->
    P = erlangify_trigger(Trigger, Event),
    Start = now(),
    ok = libsnarl:org_add_trigger(Org, P),
    ?MSnarl(?P(State), Start),
    {{true, <<"/api/", Version/binary, "/orgs/", Org/binary>>},
     Req, State}.

write(Req, State = #state{path = [Org, <<"metadata">> | Path]}, [{K, V}])
  when is_binary(Org) ->
    Start = now(),
    libsnarl:org_set(Org, Path ++ [K], jsxd:from_list(V)),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [Org]}, _Body) ->
    Start = now(),
    ok = libsnarl:org_add(Org),
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

delete(Req, State = #state{path = [Org, <<"triggers">> , Trigger]}) ->
    Start = now(),
    ok = libsnarl:org_remove_trigger(Org, Trigger),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Org]}) ->
    Start = now(),
    ok = libsnarl:org_delete(Org),
    ?MSnarl(?P(State), Start),
    {true, Req, State}.

%% Internal Functions

erlangify_trigger(<<"user_create">>, Event) ->
    {user_create,
     erlangify_trigger(Event)};

erlangify_trigger(<<"dataset_create">>, Event) ->
    {dataset_create,
     erlangify_trigger(Event)};

erlangify_trigger(<<"vm_create">>, Event) ->
    {vm_create,
     erlangify_trigger(Event)}.

erlangify_trigger([{<<"action">>, <<"join_role">>},
                   {<<"target">>, Role}]) ->
    {join, role, Role};

erlangify_trigger([{<<"action">>, <<"join_org">>},
                   {<<"target">>, Org}]) ->
    {join, org, Org};

erlangify_trigger([{<<"action">>, <<"role_grant">>},
                   {<<"base">>, Base},
                   {<<"permission">>, Permission},
                   {<<"target">>, Target}]) ->
    {grant, role, Target,
     [Base, placeholder | Permission]};

erlangify_trigger([{<<"action">>, <<"user_grant">>},
                   {<<"base">>, Base},
                   {<<"permission">>, Permission},
                   {<<"target">>, Target}]) ->
    {grant, user, Target,
     [Base, placeholder | Permission]}.

-ifdef(TEST).

-endif.

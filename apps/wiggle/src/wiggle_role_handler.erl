-module(wiggle_role_handler).
-include("wiggle.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(CACHE, role).

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

allowed_methods(_Version, _Token, [_Role]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Role, <<"permissions">>]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Role, <<"metadata">> | _]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Role, <<"permissions">> | _Permission]) ->
    [<<"PUT">>, <<"DELETE">>].

get(State = #state{path = [Role, <<"permissions">> | Permission]}) ->
    case {erlangify_permission(Permission), wiggle_role_handler:get(State#state{path = [Role]})} of
        {_, not_found} ->
            not_found;
        {[], {ok, Obj}} ->
            {ok, Obj};
        {P, {ok, Obj}} ->
            case lists:member(P, jsxd:get(<<"permissions">>, [], Obj)) of
                true ->
                    {ok, Obj};
                _ -> not_found
            end
    end;

get(State = #state{path = [Role | _]}) ->
    Start = now(),
    TTL = application:get_env(wiggle, role_ttl, 60*1000*1000),
    R = wiggle_handler:timeout_cache_with_invalid(
          ?CACHE, Role, TTL, not_found,
          fun() -> libsnarl:role_get(Role) end),
    ?MSnarl(?P(State), Start),
    R.

permission_required(#state{method = <<"GET">>, path = []}) ->
    {ok, [<<"cloud">>, <<"roles">>, <<"list">>]};

permission_required(#state{method = <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"roles">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [Role]}) ->
    {ok, [<<"roles">>, Role, <<"get">>]};

permission_required(#state{method = <<"PUT">>, path = [Role]}) ->
    {ok, [<<"roles">>, Role, <<"create">>]};

permission_required(#state{method = <<"DELETE">>, path = [Role]}) ->
    {ok, [<<"roles">>, Role, <<"delete">>]};

permission_required(#state{method = <<"GET">>, path = [Role, <<"permissions">>]}) ->
    {ok, [<<"roles">>, Role, <<"get">>]};

permission_required(#state{method = <<"PUT">>, path = [Role, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {multiple, [[<<"roles">>, Role, <<"grant">>],
                [<<"permissions">>, P, <<"grant">>]]};

permission_required(#state{method = <<"DELETE">>, path = [Role, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {multiple, [[<<"roles">>, Role, <<"revoke">>],
                [<<"permissions">>, P, <<"revoke">>]]};

permission_required(#state{method = <<"PUT">>, path = [Role, <<"metadata">> | _]}) ->
    {ok, [<<"roles">>, Role, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [Role, <<"metadata">> | _]}) ->
    {ok, [<<"roles">>, Role, <<"edit">>]};

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
    {ok, Res} = libsnarl:role_list(
                  [{must, 'allowed',
                    [<<"roles">>, {<<"res">>, <<"uuid">>}, <<"get">>],
                    Permissions}], FullList),
    ?MSniffle(?P(State), Start1),
    Res1 = case {Filter, FullList} of
               {_, false} ->
                   [ID || {_, ID} <- Res];
               {[], _} ->
                   [ID || {_, ID} <- Res];
               _ ->
                   [jsxd:select(Filter, ID) || {_, ID} <- Res]
           end,
    {Res1, Req, State};

read(Req, State = #state{path = [_Role], obj = RoleObj}) ->
    RoleObj1 = jsxd:update(<<"permissions">>,
                            fun (Permissions) ->
                                    lists:map(fun jsonify_permissions/1, Permissions)
                            end, [], RoleObj),
    {RoleObj1, Req, State};

read(Req, State = #state{path = [_Role, <<"permissions">>], obj = RoleObj}) ->
    {lists:map(fun jsonify_permissions/1, jsxd:get(<<"permissions">>, [], RoleObj)), Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = Version}, Decoded) ->
    {ok, Role} = jsxd:get(<<"name">>, Decoded),
    Start = now(),
    {ok, UUID} = libsnarl:role_add(Role),
    ?MSnarl(?P(State), Start),
    {{true, <<"/api/", Version/binary, "/roles/", UUID/binary>>}, Req, State#state{body = Decoded}}.

%% TODO : This is a icky case it is called after post.
write(Req, State = #state{method = <<"POST">>, path = []}, _) ->
    {true, Req, State};

write(Req, State = #state{path = [Role, <<"metadata">> | Path]}, [{K, V}]) when is_binary(Role) ->
    Start = now(),
    e2qc:evict(?CACHE, Role),
    libsnarl:role_set(Role, Path ++ [K], jsxd:from_list(V)),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [Role]}, _Body) ->
    Start = now(),
    e2qc:evict(?CACHE, Role),
    ok = libsnarl:role_add(Role),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [Role, <<"permissions">> | Permission]}, _Body) ->
    P = erlangify_permission(Permission),
    Start = now(),
    e2qc:evict(?CACHE, Role),
    ok = libsnarl:role_grant(Role, P),
    ?MSnarl(?P(State), Start),
    {true, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [Role, <<"metadata">> | Path]}) ->
    Start = now(),
    e2qc:evict(?CACHE, Role),
    libsnarl:role_set(Role, Path, delete),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Role, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    Start = now(),
    e2qc:evict(?CACHE, Role),
    ok = libsnarl:role_revoke(Role, P),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Role]}) ->
    Start = now(),
    e2qc:evict(?CACHE, Role),
    ok = libsnarl:role_delete(Role),
    ?MSnarl(?P(State), Start),
    {true, Req, State}.

%% Internal Functions

erlangify_permission(P) ->
    lists:map(fun(E) ->
                      E
              end, P).

jsonify_permissions(P) ->
    lists:map(fun('...') ->
                      <<"...">>;
                 ('_') ->
                      <<"_">>;
                 (E) ->
                      E
              end, P).


-ifdef(TEST).

jsonify_permission_test() ->
    ?assertEqual([<<"_">>, <<"a">>, <<"...">>],
                 jsonify_permissions(['_', <<"a">>, '...'])).

-endif.

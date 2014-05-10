%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_user_handler).
-include("wiggle.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(CACHE, user).
-define(LIST_CACHE, user_list).

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

allowed_methods(_Version, _Token, [_Login]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Login, <<"metadata">> | _]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Login, <<"keys">>]) ->
    [<<"GET">>, <<"PUT">>];

allowed_methods(_Version, _Token, [_Login, <<"keys">>, _]) ->
    [<<"DELETE">>];

allowed_methods(_Version, _Token, [_Login, <<"yubikeys">>]) ->
    [<<"GET">>, <<"PUT">>];

allowed_methods(_Version, _Token, [_Login, <<"yubikeys">>, _]) ->
    [<<"DELETE">>];

allowed_methods(_Version, _Token, [_Login, <<"permissions">>]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Login, <<"permissions">> | _Permission]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Login, <<"roles">>]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Login, <<"roles">>, _Role]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Login, <<"orgs">>]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Login, <<"orgs">>, _Org]) ->
    [<<"PUT">>, <<"DELETE">>].

get(State = #state{path = [User, <<"permissions">> | Permission]}) ->
    case {erlangify_permission(Permission),
          wiggle_user_handler:get(State#state{path = [User]})} of
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

get(State = #state{method = <<"DELETE">>,
                   path = [User, <<"roles">>, Role]}) ->
    case wiggle_user_handler:get(State#state{path = [User]}) of
        not_found ->
            not_found;
        {ok, Obj} ->
            case lists:member(Role, jsxd:get(<<"roles">>, [], Obj)) of
                true ->
                    {ok, Obj};
                _ ->
                    not_found
            end
    end;

get(State = #state{method = <<"PUT">>, path = [User, <<"roles">>, Role]}) ->
    case wiggle_user_handler:get(State#state{path = [User]}) of
        not_found ->
            not_found;
        {ok, Obj} ->
            Start1 = now(),
            case libsnarl:role_get(Role) of
                not_found ->
                    ?MSnarl(?P(State), Start1),
                    not_found;
                {ok, _} ->
                    ?MSnarl(?P(State), Start1),
                    {ok, Obj}
            end
    end;

get(State = #state{path = [User | _]}) ->
    Start = now(),
    R = case application:get_env(wiggle, user_ttl) of
            {ok, {TTL1, TTL2}} ->
                wiggle_handler:timeout_cache_with_invalid(
                  ?CACHE, User, TTL1, TTL2, not_found,
                  fun() -> libsnarl:user_get(User) end);
            _ ->
                libsnarl:user_get(User)
        end,
    ?MSnarl(?P(State), Start),
    R.

permission_required(#state{method = <<"GET">>, path = []}) ->
    {ok, [<<"cloud">>, <<"users">>, <<"list">>]};

permission_required(#state{method = <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"users">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [User]}) ->
    {ok, [<<"users">>, User, <<"get">>]};

permission_required(#state{method = <<"PUT">>, path = [User]}) ->
    {ok, [<<"users">>, User, <<"passwd">>]};

permission_required(#state{method = <<"DELETE">>, path = [User]}) ->
    {ok, [<<"users">>, User, <<"delete">>]};

permission_required(#state{method = <<"GET">>,
                           path = [User, <<"permissions">>]}) ->
    {ok, [<<"users">>, User, <<"get">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [User, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {multiple, [[<<"users">>, User, <<"grant">>], P]};

permission_required(#state{method = <<"DELETE">>,
                           path = [User, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {multiple, [[<<"users">>, User, <<"revoke">>], P]};

permission_required(#state{method = <<"GET">>,
                           path = [User, <<"roles">>]}) ->
    {ok, [<<"users">>, User, <<"get">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [User, <<"roles">>, Role]}) ->
    {multiple, [[<<"users">>, User, <<"join">>],
                [<<"roles">>, Role, <<"join">>]]};

permission_required(#state{method = <<"DELETE">>,
                           path = [User, <<"roles">>, Role]}) ->
    {multiple, [[<<"users">>, User, <<"leave">>],
                [<<"roles">>, Role, <<"leave">>]]};

permission_required(#state{method = <<"GET">>,
                           path = [User, <<"orgs">>]}) ->
    {ok, [<<"users">>, User, <<"get">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [User, <<"orgs">>, Org]}) ->
    {multiple, [[<<"users">>, User, <<"join">>],
                [<<"orgs">>, Org, <<"join">>]]};

permission_required(#state{method = <<"DELETE">>,
                           path = [User, <<"orgs">>, Org]}) ->
    {multiple, [[<<"users">>, User, <<"leave">>],
                [<<"orgs">>, Org, <<"leave">>]]};

permission_required(#state{method = <<"PUT">>,
                           path = [User, <<"metadata">> | _]}) ->
    {ok, [<<"users">>, User, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>,
                           path = [User, <<"metadata">> | _]}) ->
    {ok, [<<"users">>, User, <<"edit">>]};

permission_required(#state{method = <<"GET">>,
                           path = [User, <<"keys">>]}) ->
    {ok, [<<"users">>, User, <<"get">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [User, <<"keys">>]}) ->
    {ok, [<<"users">>, User, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>,
                           path = [User, <<"keys">>, _KeyID]}) ->
    {ok, [<<"users">>, User, <<"edit">>]};

permission_required(#state{method = <<"GET">>,
                           path = [User, <<"yubikeys">>]}) ->
    {ok, [<<"users">>, User, <<"get">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [User, <<"yubikeys">>]}) ->
    {ok, [<<"users">>, User, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>,
                           path = [User, <<"yubikeys">>, _KeyID]}) ->
    {ok, [<<"users">>, User, <<"edit">>]};

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
                   [<<"users">>, {<<"res">>, <<"uuid">>}, <<"get">>],
                   Permissions}],
    Fun = wiggle_handler:list_fn(fun libsnarl:user_list/2, Permission,
                                 FullList, Filter),
    Res1 = case application:get_env(wiggle, user_list_ttl) of
               {ok, {TTL1, TTL2}} ->
                   wiggle_handler:timeout_cache(
                     ?LIST_CACHE, {Token, FullList, Filter}, TTL1, TTL2, Fun);
               _ ->
                   Fun()
           end,
    ?MSnarl(?P(State), Start1),
    {Res1, Req, State};

read(Req, State = #state{path = [_User], obj = UserObj}) ->
    UserObj1 = jsxd:update(<<"permissions">>,
                           fun (Permissions) ->
                                   lists:map(fun jsonify_permissions/1, Permissions)
                           end, [], UserObj),
    UserObj2 = jsxd:delete(<<"password">>, UserObj1),
    {UserObj2, Req, State};

read(Req, State = #state{path = [_User, <<"permissions">>], obj = UserObj}) ->
    {lists:map(fun jsonify_permissions/1, jsxd:get(<<"permissions">>, [], UserObj)), Req, State};

read(Req, State = #state{path = [_User, <<"roles">>], obj = UserObj}) ->
    {jsxd:get(<<"roles">>, [], UserObj), Req, State};

read(Req, State = #state{path = [_User, <<"orgs">>], obj = UserObj}) ->
    {jsxd:get(<<"orgs">>, [], UserObj), Req, State};

read(Req, State = #state{path = [_User, <<"keys">>], obj = UserObj}) ->
    {jsxd:get(<<"keys">>, [], UserObj), Req, State};

read(Req, State = #state{path = [_User, <<"yubikeys">>], obj = UserObj}) ->
    {jsxd:get(<<"yubikeys">>, [], UserObj), Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{token = Token, path = [], version = Version}, Decoded) ->
    {ok, Creator} = libsnarl:user_get(Token),
    {ok, CUUID} = jsxd:get(<<"uuid">>, Creator),
    {ok, User} = jsxd:get(<<"user">>, Decoded),
    {ok, Pass} = jsxd:get(<<"password">>, Decoded),
    Start = now(),
    {ok, UUID} = libsnarl:user_add(CUUID, User),
    e2qc:teardown(?LIST_CACHE),
    ?MSnarl(?P(State), Start),
    Start1 = now(),
    ok = libsnarl:user_passwd(UUID, Pass),
    ?MSnarl(?P(State), Start1),
    {{true, <<"/api/", Version/binary, "/users/", UUID/binary>>}, Req, State#state{body = Decoded}}.

write(Req, State = #state{path =  [User]}, [{<<"password">>, Password}]) ->
    Start = now(),
    e2qc:evict(?CACHE, User),
    e2qc:teardown(?LIST_CACHE),
    ok = libsnarl:user_passwd(User, Password),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

%% TODO : This is a icky case it is called after post.
write(Req, State = #state{method = <<"POST">>, path = []}, _) ->
    {true, Req, State};

write(Req, State = #state{path = [User, <<"metadata">> | Path]}, [{K, V}]) ->
    Start = now(),
    e2qc:evict(?CACHE, User),
    e2qc:teardown(?LIST_CACHE),
    libsnarl:user_set(User, Path ++ [K], jsxd:from_list(V)),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [User, <<"keys">>]}, [{KeyID, Key}]) ->
    case re:split(Key, " ") of
        [_,ID,_] ->
            try
                base64:decode(ID),
                Start = now(),
                e2qc:evict(?CACHE, User),
                e2qc:teardown(?LIST_CACHE),
                libsnarl:user_key_add(User, KeyID, Key),
                ?MSnarl(?P(State), Start),
                {true, Req, State}
            catch
                _:_ ->
                    {false, Req, State}
            end;
        _ ->
            {false, Req, State}
    end;

write(Req, State = #state{path = [User, <<"yubikeys">>]},
      [{<<"otp">>, <<_:33/binary, _/binary >>= OTP}]) ->
    Start = now(),
    e2qc:evict(?CACHE, User),
    e2qc:teardown(?LIST_CACHE),
    libsnarl:user_yubikey_add(User, OTP),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [_, <<"yubikeys">>]}, _) ->
    {false, Req, State};

write(Req, State = #state{path = [User, <<"roles">>, Role]}, _) ->
    Start = now(),
    e2qc:evict(?CACHE, User),
    e2qc:teardown(?LIST_CACHE),
    ok = libsnarl:user_join(User, Role),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [User, <<"orgs">>, Org]}, []) ->
    Start = now(),
    e2qc:evict(?CACHE, User),
    e2qc:teardown(?LIST_CACHE),
    ok = libsnarl:user_join_org(User, Org),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [User, <<"orgs">>, Org]}, [{}]) ->
    Start = now(),
    e2qc:evict(?CACHE, User),
    e2qc:teardown(?LIST_CACHE),
    ok = libsnarl:user_join_org(User, Org),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [User, <<"orgs">>, Org]},
      [{<<"active">>, true}]) ->
    Start = now(),
    e2qc:evict(?CACHE, User),
    e2qc:teardown(?LIST_CACHE),
    ok = libsnarl:user_join_org(User, Org),
    ok = libsnarl:user_select_org(User, Org),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [User, <<"permissions">> | Permission]}, _) ->
    P = erlangify_permission(Permission),
    Start = now(),
    e2qc:evict(?CACHE, User),
    e2qc:teardown(?LIST_CACHE),
    ok = libsnarl:user_grant(User, P),
    ?MSnarl(?P(State), Start),
    {true, Req, State}.


%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [User, <<"metadata">> | Path]}) ->
    Start = now(),
    e2qc:evict(?CACHE, User),
    e2qc:teardown(?LIST_CACHE),
    libsnarl:user_set(User, Path, delete),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [User, <<"keys">>, KeyID]}) ->
    Start = now(),
    e2qc:evict(?CACHE, User),
    e2qc:teardown(?LIST_CACHE),
    libsnarl:user_key_revoke(User, KeyID),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [User, <<"yubikeys">>, KeyID]}) ->
    Start = now(),
    e2qc:evict(?CACHE, User),
    e2qc:teardown(?LIST_CACHE),
    libsnarl:user_yubikey_remove(User, KeyID),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [_User, <<"sessions">>]}) ->
    Req1 = cowboy_req:set_resp_cookie(<<"x-snarl-token">>, <<"">>, [{max_age, 0}], Req),
    {true, Req1, State};

delete(Req, State = #state{path = [User, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    Start = now(),
    e2qc:teardown(?LIST_CACHE),
    e2qc:evict(?CACHE, User),
    ok = libsnarl:user_revoke(User, P),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [User]}) ->
    Start = now(),
    e2qc:evict(?CACHE, User),
    e2qc:teardown(?LIST_CACHE),
    ok = libsnarl:user_delete(User),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [User, <<"orgs">>, Org]}) ->
    Start = now(),
    e2qc:evict(?CACHE, User),
    e2qc:teardown(?LIST_CACHE),
    ok = libsnarl:user_leave_org(User, Org),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [User, <<"roles">>, Role]}) ->
    Start = now(),
    e2qc:evict(?CACHE, User),
    e2qc:teardown(?LIST_CACHE),
    ok = libsnarl:user_leave(User, Role),
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

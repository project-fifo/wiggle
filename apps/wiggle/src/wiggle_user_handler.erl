%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_user_handler).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/3,
         rest_init/2]).
-export([content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         delete_resource/2,
         resource_exists/2,
         service_available/2,
         forbidden/2,
         options/2,
         create_path/2,
         post_is_create/2,
         is_authorized/2]).

-export([to_json/2,
         from_json/2,
         to_msgpack/2,
         from_msgpack/2]).

-ignore_xref([to_json/2,
              from_json/2,
              from_msgpack/2,
              to_msgpack/2,
              post_is_create/2,
              allowed_methods/2,
              content_types_accepted/2,
              content_types_provided/2,
              delete_resource/2,
              service_available/2,
              forbidden/2,
              init/3,
              create_path/2,
              is_authorized/2,
              options/2,
              resource_exists/2,
              rest_init/2]).

-record(state, {path, method, version, token, content, reply, obj, body}).



init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Req, _) ->
    wiggle_handler:initial_state(Req).

post_is_create(Req, State) ->
    {true, Req, State}.

service_available(Req, State) ->
    case libsnarl:servers() of
        [] ->
            {false, Req, State};
        _ ->
            {true, Req, State}
    end.

options(Req, State) ->
    Methods = allowed_methods(State#state.version, State#state.token, State#state.path),
    Req1 = cowboy_req:set_resp_header(
             <<"Access-Control-Allow-Methods">>,
             string:join(
               lists:map(fun erlang:atom_to_list/1,
                         ['HEAD', 'GET', 'OPTIONS' | Methods]), ", "), Req),
    {ok, Req1, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json},
      {<<"application/x-msgpack">>, to_msgpack}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {wiggle_handler:accepted(), Req, State}.

allowed_methods(Req, State) ->
    {['HEAD', 'OPTIONS' | allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

allowed_methods(_Version, _Token, []) ->
    ['GET', 'POST'];

allowed_methods(_Version, _Token, [_Login]) ->
    ['GET', 'PUT', 'DELETE'];

allowed_methods(_Version, _Token, [_Login, <<"metadata">> | _]) ->
    ['PUT', 'DELETE'];

allowed_methods(_Version, _Token, [_Login, <<"permissions">>]) ->
    ['GET'];

allowed_methods(_Version, _Token, [_Login, <<"permissions">> | _Permission]) ->
    ['PUT', 'DELETE'];

allowed_methods(_Version, _Token, [_Login, <<"groups">>]) ->
    ['GET'];

allowed_methods(_Version, _Token, [_Login, <<"groups">>, _Group]) ->
    ['PUT', 'DELETE'].

resource_exists(Req, State = #state{path = [User, <<"permissions">> | Permission]}) ->
    case {erlangify_permission(Permission), libsnarl:user_get(User)} of
        {_, {ok, not_found}} ->
            {false, Req, State};
        {[], {ok, Obj}} ->
            {true, Req, State#state{obj = Obj}};
        {P, {ok, Obj}} ->
            {lists:member(P, jsxd:get(<<"permissions">>, [], Obj)), Req, State#state{obj = Obj}}
    end;

resource_exists(Req, State = #state{method = 'DELETE', path = [User, <<"groups">>, Group]}) ->
    case libsnarl:user_get(User) of
        {ok, not_found} ->
            {false, Req, State};
        {ok, Obj} ->
            {lists:member(Group, jsxd:get(<<"groups">>, [], Obj)), Req, State#state{obj = Obj}}
    end;

resource_exists(Req, State = #state{method = 'PUT', path = [User, <<"groups">>, Group]}) ->
    case libsnarl:user_get(User) of
        {ok, not_found} ->
            {false, Req, State};
        {ok, Obj} ->
            case libsnarl:group_get(Group) of
                not_found ->
                    {false, Req, State#state{obj = Obj}};
                {ok, _} ->
                    {true, Req, State#state{obj = Obj}}
            end
    end;

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [User | _]}) ->
    case libsnarl:user_get(User) of
        not_found ->
            {false, Req, State};
        {ok, Obj} ->
            {true, Req, State#state{obj = Obj}}
    end.


is_authorized(Req, State = #state{path = [_, <<"sessions">>]}) ->
    {true, Req, State};

is_authorized(Req, State = #state{method = 'OPTIONS'}) ->
    {true, Req, State};

is_authorized(Req, State = #state{token = undefined}) ->
    {{false, <<"X-Snarl-Token">>}, Req, State};

is_authorized(Req, State) ->
    {true, Req, State}.

forbidden(Req, State = #state{path = [_, <<"sessions">>]}) ->
    {false, Req, State};

forbidden(Req, State = #state{method = 'OPTIONS'}) ->
    {false, Req, State};

forbidden(Req, State = #state{token = undefined}) ->
    {true, Req, State};

forbidden(Req, State = #state{method = 'GET', path = []}) ->
    {allowed(State#state.token, [<<"cloud">>, <<"users">>, <<"list">>]), Req, State};

forbidden(Req, State = #state{method = 'POST', path = []}) ->
    {allowed(State#state.token, [<<"cloud">>, <<"users">>, <<"create">>]), Req, State};

forbidden(Req, State = #state{method = 'GET', path = [User]}) ->
    {allowed(State#state.token, [<<"users">>, User, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = 'PUT', path = [User]}) ->
    {allowed(State#state.token, [<<"users">>, User, <<"passwd">>]), Req, State};

forbidden(Req, State = #state{method = 'DELETE', path = [User]}) ->
    {allowed(State#state.token, [<<"users">>, User, <<"delete">>]), Req, State};

forbidden(Req, State = #state{method = 'GET', path = [User, <<"permissions">>]}) ->
    {allowed(State#state.token, [<<"users">>, User, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = 'PUT', path = [User, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {allowed(State#state.token, [<<"users">>, User, <<"grant">>])
     andalso allowed(State#state.token, [<<"permissions">>, P, <<"grant">>]), Req, State};

forbidden(Req, State = #state{method = 'DELETE', path = [User, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {allowed(State#state.token, [<<"users">>, User, <<"revoke">>])
     andalso allowed(State#state.token, [<<"permissions">>, P, <<"revoke">>]), Req, State};

forbidden(Req, State = #state{method = 'GET', path = [User, <<"groups">>]}) ->
    {allowed(State#state.token, [<<"users">>, User, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = 'PUT', path = [User, <<"groups">>, Group]}) ->
    {allowed(State#state.token, [<<"users">>, User, <<"join">>])
     andalso allowed(State#state.token, [<<"groups">>, Group, <<"join">>]), Req, State};

forbidden(Req, State = #state{method = 'DELETE', path = [User, <<"groups">>, Group]}) ->
    {allowed(State#state.token, [<<"users">>, User, <<"leave">>])
     andalso allowed(State#state.token, [<<"groups">>, Group, <<"leave">>]), Req, State};

forbidden(Req, State = #state{method = 'PUT', path = [User, <<"metadata">> | _]}) ->
    {allowed(State#state.token, [<<"users">>, User, <<"edit">>]), Req, State};

forbidden(Req, State = #state{method = 'DELETE', path = [User, <<"metadata">> | _]}) ->
    {allowed(State#state.token, [<<"users">>, User, <<"edit">>]), Req, State};

forbidden(Req, State) ->
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

handle_request(Req, State = #state{path = []}) ->
    {ok, Res} = libsnarl:user_list(),
    {Res, Req, State};

handle_request(Req, State = #state{path = [_User], obj = UserObj}) ->
    UserObj1 = jsxd:update(<<"permissions">>,
                           fun (Permissions) ->
                                   lists:map(fun jsonify_permissions/1, Permissions)
                           end, [], UserObj),
    UserObj2 = jsxd:delete(<<"password">>, UserObj1),
    {UserObj2, Req, State};

handle_request(Req, State = #state{path = [_User, <<"permissions">>], obj = UserObj}) ->
    {lists:map(fun jsonify_permissions/1, jsxd:get(<<"permissions">>, [], UserObj)), Req, State};

handle_request(Req, State = #state{path = [_User, <<"groups">>], obj = UserObj}) ->
    {jsxd:get(<<"groups">>, [], UserObj), Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create_path(Req, State = #state{path = [], version = Version}) ->
    {ok, Decoded, Req1} = wiggle_handler:decode(Req),
    {ok, User} = jsxd:get(<<"user">>, Decoded),
    {ok, Pass} = jsxd:get(<<"password">>, Decoded),
    {ok, UUID} = libsnarl:user_add(User),
    ok = libsnarl:user_passwd(UUID, Pass),
    {<<"/api/", Version/binary, "/users/", UUID/binary>>, Req1, State#state{body = Decoded}}.

from_json(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    io:format("[PUT] ~p", [Body]),
    {Reply, Req2, State1} = case Body of
                                <<>> ->
                                    handle_write(Req1, State, []);
                                _ ->
                                    Decoded = jsx:decode(Body),
                                    handle_write(Req1, State, Decoded)
                            end,

    {Reply, Req2, State1}.

from_msgpack(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    io:format("[PUT] ~p", [Body]),
    {Reply, Req2, State1} = case Body of
                                <<>> ->
                                    handle_write(Req1, State, []);
                                _ ->
                                    Decoded = msgpack:unpack(Body, [jsx]),
                                    handle_write(Req1, State, Decoded)
                            end,

    {Reply, Req2, State1}.



handle_write(Req, State = #state{path =  [User]}, [{<<"password">>, Password}]) ->
    ok = libsnarl:user_passwd(User, Password),
    {true, Req, State};

%% TODO : This is a icky case it is called after post.
handle_write(Req, State = #state{method = 'POST', path = []}, _) ->
    {true, Req, State};

handle_write(Req, State = #state{path = [User, <<"metadata">> | Path]}, [{K, V}]) ->
    libsnarl:user_set(User, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    {true, Req, State};

handle_write(Req, State = #state{path = [User, <<"groups">>, Group]}, _) ->
    ok = libsnarl:user_join(User, Group),
    {true, Req, State};

handle_write(Req, State = #state{path = [User, <<"permissions">> | Permission]}, _) ->
    P = erlangify_permission(Permission),
    ok = libsnarl:user_grant(User, P),
    {true, Req, State}.


%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [User, <<"metadata">> | Path]}) ->
    libsnarl:user_set(User, [<<"metadata">> | Path], delete),
    {true, Req, State};

delete_resource(Req, State = #state{path = [_User, <<"sessions">>]}) ->
    Req1 = cowboy_req:set_resp_cookie(<<"X-Snarl-Token">>, <<"">>, [{max_age, 0}], Req),
    {true, Req1, State};

delete_resource(Req, State = #state{path = [User, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    ok = libsnarl:user_revoke(User, P),
    {true, Req, State};

delete_resource(Req, State = #state{path = [User]}) ->
    ok = libsnarl:user_delete(User),
    {true, Req, State};

delete_resource(Req, State = #state{path = [User, <<"groups">>, Group]}) ->
    ok = libsnarl:user_leave(User, Group),
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

allowed(Token, Perm) ->
    case libsnarl:allowed({token, Token}, Perm) of
        not_found ->
            true;
        true ->
            false;
        false ->
            true
    end.

-ifdef(TEST).

jsonify_permission_test() ->
    ?assertEqual([<<"_">>, <<"a">>, <<"...">>],
                 jsonify_permissions(['_', <<"a">>, '...'])).

-endif.

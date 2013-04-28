%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_user_handler).
-include("wiggle.hrl").

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
              rest_init/2,
              rest_terminate/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) ->
    wiggle_handler:initial_state(Req).

rest_terminate(_Req, State) ->
    ?M(?P(State), State#state.start),
    ok.

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
             <<"access-control-allow-methods">>,
             string:join(
               lists:map(fun erlang:binary_to_list/1,
                         [<<"HEAD">>, <<"GET">>, <<"OPTIONS">> | Methods]), ", "), Req),
    {ok, Req1, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json},
      {<<"application/x-msgpack">>, to_msgpack}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {wiggle_handler:accepted(), Req, State}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"OPTIONS">> | allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Login]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Login, <<"metadata">> | _]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Login, <<"permissions">>]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Login, <<"permissions">> | _Permission]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Login, <<"groups">>]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Login, <<"groups">>, _Group]) ->
    [<<"PUT">>, <<"DELETE">>].

resource_exists(Req, State = #state{path = [User, <<"permissions">> | Permission]}) ->
    Start = now(),
    case {erlangify_permission(Permission), libsnarl:user_get(User)} of
        {_, not_found} ->
            ?MSnarl(?P(State), Start),
            {false, Req, State};
        {[], {ok, Obj}} ->
            ?MSnarl(?P(State), Start),
            {true, Req, State#state{obj = Obj}};
        {P, {ok, Obj}} ->
            ?MSnarl(?P(State), Start),
            {lists:member(P, jsxd:get(<<"permissions">>, [], Obj)), Req, State#state{obj = Obj}}
    end;

resource_exists(Req, State = #state{method = <<"DELETE">>, path = [User, <<"groups">>, Group]}) ->
    Start = now(),
    case libsnarl:user_get(User) of
        not_found ->
            ?MSnarl(?P(State), Start),
            {false, Req, State};
        {ok, Obj} ->
            ?MSnarl(?P(State), Start),
            {lists:member(Group, jsxd:get(<<"groups">>, [], Obj)), Req, State#state{obj = Obj}}
    end;

resource_exists(Req, State = #state{method = <<"PUT">>, path = [User, <<"groups">>, Group]}) ->
    Start = now(),
    case libsnarl:user_get(User) of
        not_found ->
            ?MSnarl(?P(State), Start),
            {false, Req, State};
        {ok, Obj} ->
            ?MSnarl(?P(State), Start),
            Start1 = now(),
            case libsnarl:group_get(Group) of
                not_found ->
                    ?MSnarl(?P(State), Start1),
                    {false, Req, State#state{obj = Obj}};
                {ok, _} ->
                    ?MSnarl(?P(State), Start1),
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

is_authorized(Req, State = #state{method = <<"OPTIONS">>}) ->
    {true, Req, State};

is_authorized(Req, State = #state{token = undefined}) ->
    {{false, <<"x-snarl-token">>}, Req, State};

is_authorized(Req, State) ->
    {true, Req, State}.

forbidden(Req, State = #state{path = [_, <<"sessions">>]}) ->
    {false, Req, State};

forbidden(Req, State = #state{method = <<"OPTIONS">>}) ->
    {false, Req, State};

forbidden(Req, State = #state{token = undefined}) ->
    {true, Req, State};

forbidden(Req, State = #state{method = <<"GET">>, path = []}) ->
    {allowed(State, [<<"cloud">>, <<"users">>, <<"list">>]), Req, State};

forbidden(Req, State = #state{method = <<"POST">>, path = []}) ->
    {allowed(State, [<<"cloud">>, <<"users">>, <<"create">>]), Req, State};

forbidden(Req, State = #state{method = <<"GET">>, path = [User]}) ->
    {allowed(State, [<<"users">>, User, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [User]}) ->
    {allowed(State, [<<"users">>, User, <<"passwd">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [User]}) ->
    {allowed(State, [<<"users">>, User, <<"delete">>]), Req, State};

forbidden(Req, State = #state{method = <<"GET">>, path = [User, <<"permissions">>]}) ->
    {allowed(State, [<<"users">>, User, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [User, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {allowed(State, [<<"users">>, User, <<"grant">>])
     andalso allowed(State, [<<"permissions">>, P, <<"grant">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [User, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {allowed(State, [<<"users">>, User, <<"revoke">>])
     andalso allowed(State, [<<"permissions">>, P, <<"revoke">>]), Req, State};

forbidden(Req, State = #state{method = <<"GET">>, path = [User, <<"groups">>]}) ->
    {allowed(State, [<<"users">>, User, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [User, <<"groups">>, Group]}) ->
    {allowed(State, [<<"users">>, User, <<"join">>])
     andalso allowed(State, [<<"groups">>, Group, <<"join">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [User, <<"groups">>, Group]}) ->
    {allowed(State, [<<"users">>, User, <<"leave">>])
     andalso allowed(State, [<<"groups">>, Group, <<"leave">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [User, <<"metadata">> | _]}) ->
    {allowed(State, [<<"users">>, User, <<"edit">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [User, <<"metadata">> | _]}) ->
    {allowed(State, [<<"users">>, User, <<"edit">>]), Req, State};

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
    Start = now(),
    {ok, Res} = libsnarl:user_list(),
    ?MSnarl(?P(State), Start),
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
    Start = now(),
    {ok, UUID} = libsnarl:user_add(User),
    ?MSnarl(?P(State), Start),
    Start1 = now(),
    ok = libsnarl:user_passwd(UUID, Pass),
    ?MSnarl(?P(State), Start1),
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
    Start = now(),
    ok = libsnarl:user_passwd(User, Password),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

%% TODO : This is a icky case it is called after post.
handle_write(Req, State = #state{method = <<"POST">>, path = []}, _) ->
    {true, Req, State};

handle_write(Req, State = #state{path = [User, <<"metadata">> | Path]}, [{K, V}]) ->
    Start = now(),
    libsnarl:user_set(User, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

handle_write(Req, State = #state{path = [User, <<"groups">>, Group]}, _) ->
    Start = now(),
    ok = libsnarl:user_join(User, Group),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

handle_write(Req, State = #state{path = [User, <<"permissions">> | Permission]}, _) ->
    P = erlangify_permission(Permission),
    Start = now(),
    ok = libsnarl:user_grant(User, P),
    ?MSnarl(?P(State), Start),
    {true, Req, State}.


%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [User, <<"metadata">> | Path]}) ->
    Start = now(),
    libsnarl:user_set(User, [<<"metadata">> | Path], delete),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete_resource(Req, State = #state{path = [_User, <<"sessions">>]}) ->
    Req1 = cowboy_req:set_resp_cookie(<<"x-snarl-token">>, <<"">>, [{max_age, 0}], Req),
    {true, Req1, State};

delete_resource(Req, State = #state{path = [User, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    Start = now(),
    ok = libsnarl:user_revoke(User, P),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete_resource(Req, State = #state{path = [User]}) ->
    Start = now(),
    ok = libsnarl:user_delete(User),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete_resource(Req, State = #state{path = [User, <<"groups">>, Group]}) ->
    Start = now(),
    ok = libsnarl:user_leave(User, Group),
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

allowed(State, Perm) ->
    Token = State#state.token,
    Start = now(),
    case libsnarl:allowed({token, Token}, Perm) of
        not_found ->
            ?MSnarl(?P(State), Start),
            true;
        true ->
            ?MSnarl(?P(State), Start),
            false;
        false ->
            ?MSnarl(?P(State), Start),
            true
    end.

-ifdef(TEST).

jsonify_permission_test() ->
    ?assertEqual([<<"_">>, <<"a">>, <<"...">>],
                 jsonify_permissions(['_', <<"a">>, '...'])).

-endif.

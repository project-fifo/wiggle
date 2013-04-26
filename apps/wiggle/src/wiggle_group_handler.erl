%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_group_handler).
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
         forbidden/2,
         service_available/2,
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
              allowed_methods/2,
              content_types_accepted/2,
              content_types_provided/2,
              delete_resource/2,
              forbidden/2,
              init/3,
              is_authorized/2,
              service_available/2,
              options/2,
              create_path/2,
              post_is_create/2,
              resource_exists/2,
              rest_init/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) ->
    wiggle_handler:initial_state(Req).

post_is_create(Req, State) ->
    {true, Req, State}.

service_available(Req, State) ->
    case  libsnarl:servers() of
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
                         [<<"HEAD">>, <<"OPTIONS">> | Methods]), ", "), Req),
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

allowed_methods(_Version, _Token, [_Group]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Group, <<"permissions">>]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Group, <<"metadata">> | _]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Group, <<"permissions">> | _Permission]) ->
    [<<"PUT">>, <<"DELETE">>].

resource_exists(Req, State = #state{path = [Group, <<"permissions">> | Permission]}) ->
    case {erlangify_permission(Permission), libsnarl:group_get(Group)} of
        {_, not_found} ->
            {false, Req, State};
        {[], {ok, Obj}} ->
            {true, Req, State#state{obj=Obj}};
        {P, {ok, Obj}} ->
            {lists:member(P, jsxd:get(<<"permissions">>, [], Obj)), Req, State#state{obj=Obj}}
    end;

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [Group | _]}) ->
    case libsnarl:group_get(Group) of
        not_found ->
            {false, Req, State};
        {ok, Obj} ->
            {true, Req, State#state{obj=Obj}}
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
    {allowed(State#state.token, [<<"cloud">>, <<"groups">>, <<"list">>]), Req, State};

forbidden(Req, State = #state{method = <<"POST">>, path = []}) ->
    {allowed(State#state.token, [<<"cloud">>, <<"groups">>, <<"create">>]), Req, State};

forbidden(Req, State = #state{method = <<"GET">>, path = [Group]}) ->
    {allowed(State#state.token, [<<"groups">>, Group, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Group]}) ->
    {allowed(State#state.token, [<<"groups">>, Group, <<"create">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Group]}) ->
    {allowed(State#state.token, [<<"groups">>, Group, <<"delete">>]), Req, State};

forbidden(Req, State = #state{method = <<"GET">>, path = [Group, <<"permissions">>]}) ->
    {allowed(State#state.token, [<<"groups">>, Group, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Group, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {allowed(State#state.token, [<<"groups">>, Group, <<"grant">>])
     andalso allowed(State#state.token, [<<"permissions">>, P, <<"grant">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Group, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {allowed(State#state.token, [<<"groups">>, Group, <<"revoke">>])
     andalso allowed(State#state.token, [<<"permissions">>, P, <<"revoke">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Group, <<"metadata">> | _]}) ->
    {allowed(State#state.token, [<<"groups">>, Group, <<"edit">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Group, <<"metadata">> | _]}) ->
    {allowed(State#state.token, [<<"groups">>, Group, <<"edit">>]), Req, State};

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
                                                %    {ok, Permissions} = libsnarl:user_cache({token, Token}),
    {ok, Res} = libsnarl:group_list(), %{must, 'allowed', [<<"vm">>, {<<"res">>, <<"uuid">>}, <<"get">>], Permissions}),
    {Res, Req, State};

handle_request(Req, State = #state{path = [_Group], obj = GroupObj}) ->
    GroupObj1 = jsxd:update(<<"permissions">>,
                            fun (Permissions) ->
                                    lists:map(fun jsonify_permissions/1, Permissions)
                            end, [], GroupObj),
    {GroupObj1, Req, State};

handle_request(Req, State = #state{path = [_Group, <<"permissions">>], obj = GroupObj}) ->
    {lists:map(fun jsonify_permissions/1, jsxd:get(<<"permissions">>, [], GroupObj)), Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create_path(Req, State = #state{path = [], version = Version}) ->
    {ok, Decoded, Req1} = wiggle_handler:decode(Req),
    {ok, Group} = jsxd:get(<<"name">>, Decoded),
    {ok, UUID} = libsnarl:group_add(Group),
    {<<"/api/", Version/binary, "/groups/", UUID/binary>>, Req1, State#state{body = Decoded}}.

from_json(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
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
    {Reply, Req2, State1} = case Body of
                                <<>> ->
                                    handle_write(Req1, State, []);
                                _ ->
                                    Decoded = msgpack:unpack(Body, [jsx]),
                                    handle_write(Req1, State, Decoded)
                            end,
    {Reply, Req2, State1}.

%% TODO : This is a icky case it is called after post.
handle_write(Req, State = #state{method = <<"POST">>, path = []}, _) ->
    {true, Req, State};

handle_write(Req, State = #state{path = [Group, <<"metadata">> | Path]}, [{K, V}]) ->
    libsnarl:group_set(Group, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    {true, Req, State};

handle_write(Req, State = #state{path = [Group]}, _Body) ->
    ok = libsnarl:group_add(Group),
    {true, Req, State};

handle_write(Req, State = #state{path = [Group, <<"permissions">> | Permission]}, _Body) ->
    P = erlangify_permission(Permission),
    ok = libsnarl:group_grant(Group, P),
    {true, Req, State}.


%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [Group, <<"metadata">> | Path]}) ->
    libsnarl:group_set(Group, [<<"metadata">> | Path], delete),
    {true, Req, State};

delete_resource(Req, State = #state{path = [Group, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    ok = libsnarl:group_revoke(Group, P),
    {true, Req, State};

delete_resource(Req, State = #state{path = [Group]}) ->
    ok = libsnarl:group_delete(Group),
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

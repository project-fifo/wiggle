-module(wiggle_group_handler).
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

allowed_methods(_Version, _Token, [_Group]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Group, <<"permissions">>]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Group, <<"metadata">> | _]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Group, <<"permissions">> | _Permission]) ->
    [<<"PUT">>, <<"DELETE">>].

get(State = #state{path = [Group, <<"permissions">> | Permission]}) ->
    case {erlangify_permission(Permission), wiggle_group_handler:get(State#state{path = [Group]})} of
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

get(State = #state{path = [Group | _]}) ->
    Start = now(),
    R = libsnarl:group_get(Group),
    ?MSnarl(?P(State), Start),
    R.

permission_required(#state{method = <<"GET">>, path = []}) ->
    {ok, [<<"cloud">>, <<"groups">>, <<"list">>]};

permission_required(#state{method = <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"groups">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [Group]}) ->
    {ok, [<<"groups">>, Group, <<"get">>]};

permission_required(#state{method = <<"PUT">>, path = [Group]}) ->
    {ok, [<<"groups">>, Group, <<"create">>]};

permission_required(#state{method = <<"DELETE">>, path = [Group]}) ->
    {ok, [<<"groups">>, Group, <<"delete">>]};

permission_required(#state{method = <<"GET">>, path = [Group, <<"permissions">>]}) ->
    {ok, [<<"groups">>, Group, <<"get">>]};

permission_required(#state{method = <<"PUT">>, path = [Group, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {multiple, [[<<"groups">>, Group, <<"grant">>],
                [<<"permissions">>, P, <<"grant">>]]};

permission_required(#state{method = <<"DELETE">>, path = [Group, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {multiple, [[<<"groups">>, Group, <<"revoke">>],
                [<<"permissions">>, P, <<"revoke">>]]};

permission_required(#state{method = <<"PUT">>, path = [Group, <<"metadata">> | _]}) ->
    {ok, [<<"groups">>, Group, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [Group, <<"metadata">> | _]}) ->
    {ok, [<<"groups">>, Group, <<"edit">>]};

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
    {ok, Res} = libsnarl:group_list(
                  [{must, 'allowed',
                    [<<"groups">>, {<<"res">>, <<"uuid">>}, <<"get">>],
                    Permissions}]),
    ?MSnarl(?P(State), Start1),
    {Res, Req, State};

read(Req, State = #state{path = [_Group], obj = GroupObj}) ->
    GroupObj1 = jsxd:update(<<"permissions">>,
                            fun (Permissions) ->
                                    lists:map(fun jsonify_permissions/1, Permissions)
                            end, [], GroupObj),
    {GroupObj1, Req, State};

read(Req, State = #state{path = [_Group, <<"permissions">>], obj = GroupObj}) ->
    {lists:map(fun jsonify_permissions/1, jsxd:get(<<"permissions">>, [], GroupObj)), Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = Version}, Decoded) ->
    {ok, Group} = jsxd:get(<<"name">>, Decoded),
    Start = now(),
    {ok, UUID} = libsnarl:group_add(Group),
    ?MSnarl(?P(State), Start),
    {{true, <<"/api/", Version/binary, "/groups/", UUID/binary>>}, Req, State#state{body = Decoded}}.

%% TODO : This is a icky case it is called after post.
write(Req, State = #state{method = <<"POST">>, path = []}, _) ->
    {true, Req, State};

write(Req, State = #state{path = [Group, <<"metadata">> | Path]}, [{K, V}]) when is_binary(Group) ->
    Start = now(),
    libsnarl:group_set(Group, Path ++ [K], jsxd:from_list(V)),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [Group]}, _Body) ->
    Start = now(),
    ok = libsnarl:group_add(Group),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [Group, <<"permissions">> | Permission]}, _Body) ->
    P = erlangify_permission(Permission),
    Start = now(),
    ok = libsnarl:group_grant(Group, P),
    ?MSnarl(?P(State), Start),
    {true, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [Group, <<"metadata">> | Path]}) ->
    Start = now(),
    libsnarl:group_set(Group, Path, delete),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Group, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    Start = now(),
    ok = libsnarl:group_revoke(Group, P),
    ?MSnarl(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Group]}) ->
    Start = now(),
    ok = libsnarl:group_delete(Group),
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

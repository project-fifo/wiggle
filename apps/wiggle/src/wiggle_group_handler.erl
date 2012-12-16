%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_group_handler).

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
	 options/2,
	 is_authorized/2]).

-export([to_json/2,
	 from_json/2]).

-ignore_xref([to_json/2,
	      from_json/2,
	      allowed_methods/2,
	      content_types_accepted/2,
	      content_types_provided/2,
	      delete_resource/2,
	      forbidden/2,
	      init/3,
	      is_authorized/2,
	      options/2,
	      resource_exists/2,
	      rest_init/2]).

-record(state, {path, method, version, token, content, reply}).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_http_rest}.

rest_init(Req, _) ->
    wiggle_handler:initial_state(Req, <<"groups">>).

options(Req, State) ->
    Methods = allowed_methods(Req, State, State#state.path),
    {ok, Req1} = cowboy_http_req:set_resp_header(
		   <<"Access-Control-Allow-Methods">>,
		   string:join(
		     lists:map(fun erlang:atom_to_list/1,
			       ['HEAD', 'OPTIONS' | Methods]), ", "), Req),
    {ok, Req1, State}.


content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json; charset=UTF-8">>, from_json}
     ], Req, State}.

allowed_methods(Req, State) ->
    {['HEAD', 'OPTIONS' | allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

allowed_methods(_Version, _Token, []) ->
    ['GET'];

allowed_methods(_Version, _Token, [_Group]) ->
    ['GET', 'PUT', 'DELETE'];

allowed_methods(_Version, _Token, [_Group, <<"permissions">>]) ->
    ['GET'];

allowed_methods(_Version, _Token, [_Group, <<"permissions">> | _Permission]) ->
    ['PUT', 'DELETE'].

resource_exists(Req, State = #state{path = [Group, <<"permissions">> | Permission]}) ->
    case {erlangify_permission(Permission), libsnarl:group_get(Group)} of
	{_, not_found} ->
	    {false, Req, State};
	{[], {ok, _}} ->
	    {true, Req, State};
	{P, {ok, {group, _Name, Permissions, _}}} ->
	    {lists:member(P, Permissions), Req, State}
    end;

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [Group | _]}) ->
    case libsnarl:group_get(Group) of
	{ok, not_found} ->
	    {false, Req, State};
	{ok, _} ->
	    {true, Req, State}
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

forbidden(Req, State = #state{path = []}) ->
    {allowed(State#state.token, [<<"groups">>]), Req, State};

forbidden(Req, State = #state{method = 'GET', path = [Group]}) ->
    {allowed(State#state.token, [<<"groups">>, Group, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = 'PUT', path = [Group]}) ->
    {allowed(State#state.token, [<<"groups">>, Group, <<"create">>]), Req, State};

forbidden(Req, State = #state{method = 'DELETE', path = [Group]}) ->
    {allowed(State#state.token, [<<"groups">>, Group, <<"delete">>]), Req, State};

forbidden(Req, State = #state{method = 'GET', path = [Group, <<"permissions">>]}) ->
    {allowed(State#state.token, [<<"groups">>, Group, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = 'PUT', path = [Group, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {allowed(State#state.token, [<<"groups">>, Group, <<"grant">>])
     andalso allowed(State#state.token, [<<"permissions">>, P, <<"grant">>]), Req, State};

forbidden(Req, State = #state{method = 'DELETE', path = [Group, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {allowed(State#state.token, [<<"groups">>, Group, <<"revoke">>])
     andalso allowed(State#state.token, [<<"permissions">>, P, <<"revoke">>]), Req, State};

forbidden(Req, State) ->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

to_json(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {jsx:encode(Reply), Req1, State1}.

handle_request(Req, State = #state{token = _Token, path = []}) ->
%    {ok, Permissions} = libsnarl:user_cache({token, Token}),
    {ok, Res} = libsnarl:group_list(), %{must, 'allowed', [<<"vm">>, {<<"res">>, <<"uuid">>}, <<"get">>], Permissions}),
    {Res, Req, State};

handle_request(Req, State = #state{path = [Group]}) ->
    {ok, {group, Name, Permissions, _}} = libsnarl:group_get(Group),
    {[{name, Name},
      {permissions, lists:map(fun jsonify_permissions/1, Permissions)}], Req, State};

handle_request(Req, State = #state{path = [Group, <<"permissions">>]}) ->
    {ok, {group, _Name, Permissions, _}} = libsnarl:group_get(Group),
    {lists:map(fun jsonify_permissions/1, Permissions), Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

from_json(Req, State) ->
    {ok, Body, Req1} = cowboy_http_req:body(Req),
    {Reply, Req2, State1} = case Body of
				<<>> ->
				    handle_write(Req1, State, []);
				_ ->
				    Decoded = jsx:decode(Body),
				    handle_write(Req1, State, Decoded)
			    end,
    {Reply, Req2, State1}.

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

delete_resource(Req, State = #state{path = [Group, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    ok = libsnarl:group_revoke(Group, P),
    {true, Req, State};

delete_resource(Req, State = #state{path = [Group]}) ->
    ok = libsnarl:group_delete(Group),
    {true, Req, State}.

%% Internal Functions

erlangify_permission(P) ->
    lists:map(fun(<<"...">>) ->
		      '...';
		 (<<"_">>) ->
		      '_';
		 (E) ->
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

erlangify_permission_test() ->
    ?assertEqual(['_', <<"a">>, '...'],
		 erlangify_permission([<<"_">>, <<"a">>, <<"...">>])).

jsonify_permission_test() ->
    ?assertEqual([<<"_">>, <<"a">>, <<"...">>],
		 jsonify_permissions(['_', <<"a">>, '...'])).

-endif.

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
    wiggle_handler:initial_state(Req, <<"users">>).

options(Req, State) ->
    Methods = allowed_methods(Req, State, State#state.path),
    {ok, Req1} = cowboy_http_req:set_resp_header(
		   <<"Access-Control-Allow-Methods">>,
		   string:join(
		     lists:map(fun erlang:atom_to_list/1,
			       ['HEAD', 'GET', 'OPTIONS' | Methods]), ", "), Req),
    {ok, Req1, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json; charset=UTF-8">>, from_json},
      {<<"application/json; charset=utf-8">>, from_json}
     ], Req, State}.

allowed_methods(Req, State) ->
    {['HEAD', 'OPTIONS' | allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

allowed_methods(_Version, _Token, []) ->
    ['GET'];

allowed_methods(_Version, _Token, [_Login]) ->
    ['GET', 'PUT', 'DELETE'];

allowed_methods(_Version, _Token, [_Login, <<"sessions">>]) ->
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
	{_, not_found} ->
	    {false, Req, State};
	{[], {ok, _}} ->
	    {true, Req, State};
	{P, {ok, {user, _Name, _, Permissions, _Groups, _}}} ->
	    {lists:member(P, Permissions), Req, State}
    end;

resource_exists(Req, State = #state{method = 'DELETE', path = [User, <<"groups">>, Group]}) ->
    case libsnarl:user_get(User) of
	not_found ->
	    {false, Req, State};
	{ok, {user, _Name, _, _Permissions, _, Groups}} ->
	    {lists:member(Group, Groups), Req, State}
    end;

resource_exists(Req, State = #state{method = 'PUT', path = [User, <<"groups">>, Group]}) ->
    case libsnarl:user_get(User) of
	not_found ->
	    {false, Req, State};
	{ok, _} ->
	    case libsnarl:group_get(Group) of
	        not_found ->
		    {false, Req, State};
		{ok, _} ->
		    {true, Req, State}
	    end
    end;

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [User | _]}) ->
    case libsnarl:user_get(User) of
	 not_found ->
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
    {allowed(State#state.token, [<<"users">>]), Req, State};

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

forbidden(Req, State) ->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

to_json(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {jsx:encode(Reply), Req1, State1}.

handle_request(Req, State = #state{path = []}) ->
    {ok, Res} = libsnarl:user_list(),
    {Res, Req, State};

handle_request(Req, State = #state{path = [User]}) ->
    {ok, {user, Name, _, Permissions, _, Groups}} = libsnarl:user_get(User),
    {[{name, Name},
      {permissions, lists:map(fun jsonify_permissions/1, Permissions)},
      {groups, Groups}], Req, State};

handle_request(Req, State = #state{path = [User, <<"permissions">>]}) ->
    {ok, {user, _Name, _, Permissions, _, _Groups}} = libsnarl:user_get(User),
    {lists:map(fun jsonify_permissions/1, Permissions), Req, State};

handle_request(Req, State = #state{path = [User, <<"groups">>]}) ->
    {ok, {user, _Name, _, _Permissions, _, Groups}} = libsnarl:user_get(User),
    {Groups, Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

from_json(Req, State) ->
    {ok, Body, Req1} = cowboy_http_req:body(Req),
    io:format("[PUT] ~p", [Body]),
    {Reply, Req2, State1} = case Body of
				<<>> ->
				    handle_write(Req1, State, []);
				_ ->
				    Decoded = jsx:decode(Body),
				    handle_write(Req1, State, Decoded)
			    end,

    {Reply, Req2, State1}.

handle_write(Req, State = #state{path = [User, <<"sessions">>]}, [{<<"password">>, Password}]) ->
    {ok, {token, Token}} = libsnarl:auth(User, Password),
    {ok, Req1} = cowboy_http_req:set_resp_header(<<"X-Snarl-Token">>, Token, Req),
    {ok, Req2} = cowboy_http_req:set_resp_cookie(<<"X-Snarl-Token">>, Token, [{max_age, 60*60*24*365}], Req1),
    {jsx:encode([{<<"token">>, Token}]), Req2, State};

handle_write(Req, State = #state{path =  [User]}, [{<<"password">>, Password}]) ->
    libsnarl:user_add(User),
    ok = libsnarl:user_passwd(User, Password),
    {true, Req, State};

handle_write(Req, State = #state{path = [User, <<"groups">>, Group]}, _) ->
    {ok, joined} = libsnarl:user_join(User, Group),
    {true, Req, State};

handle_write(Req, State = #state{path = [User, <<"permissions">> | Permission]}, _) ->
    P = erlangify_permission(Permission),
    ok = libsnarl:user_grant(User, P),
    {true, Req, State}.


%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [_User, <<"sessions">>]}) ->
    {ok, Req1} = cowboy_http_req:set_resp_cookie(<<"X-Snarl-Token">>, <<"">>, [{max_age, 0}], Req),
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

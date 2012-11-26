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


-record(state, {path, method, version, token, content, reply}).


init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_http_rest}.

rest_init(Req, _) ->
    {Method, Req1} = cowboy_http_req:method(Req),
    {[<<"api">>, Version, <<"users">> | Path], Req2} = cowboy_http_req:path(Req1),
    {ok, Req3} = cowboy_http_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req2),
    {Token, Req4} = case cowboy_http_req:header(<<"X-Snarl-Token">>, Req3) of
			{undefined, ReqX} ->
			    {undefined, ReqX};
			{TokenX, ReqX} ->
			    {ok, ReqX1} = cowboy_http_req:set_resp_header(<<"X-Snarl-Token">>, TokenX, ReqX),
			    {TokenX, ReqX1}
		    end,
    {ok, Req5} = cowboy_http_req:set_resp_header(
		   <<"Access-Control-Allow-Headers">>,
		   <<"Content-Type, X-Snarl-Token">>, Req4),
    {ok, Req6} = cowboy_http_req:set_resp_header(
		   <<"Access-Control-Expose-Headers">>,
		   <<"X-Snarl-Token">>, Req5),


    State =  #state{version = Version,
		    method = Method,
		    token = Token,
		    path = Path},
    io:format("[~p] - ~p~n", [Method, Path]),
    {ok, Req6, State}.


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
	{_, {reply, {ok, not_found}}} ->
	    {false, Req, State};
	{[], {reply, {ok, _}}} ->
	    {true, Req, State};
	{P, {reply, {ok, {user, _Name, _, Permissions, _Groups, _}}}} ->
	    {lists:member(P, Permissions), Req, State}
    end;

resource_exists(Req, State = #state{method = 'DELETE', path = [User, <<"groups">>, Group]}) ->
    case libsnarl:user_get(User) of
	{reply, {ok, not_found}} ->
	    {false, Req, State};
	{reply, {ok, {user, _Name, _, _Permissions, _, Groups}}} ->
	    {lists:member(Group, Groups), Req, State}
    end;

resource_exists(Req, State = #state{method = 'PUT', path = [User, <<"groups">>, Group]}) ->
    case libsnarl:user_get(User) of
	{reply, {ok, not_found}} ->
	    {false, Req, State};
	{reply, {ok, _}} ->
	    case libsnarl:group_get(Group) of
		{reply, {ok, not_found}} ->
		    {false, Req, State};
		{reply, {ok, _}} ->
		    {true, Req, State}
	    end
    end;

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [User | _]}) ->
    case libsnarl:user_get(User) of
	{reply, {ok, not_found}} ->
	    {false, Req, State};
	{reply, {ok, _}} ->
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
    {reply, {ok, Res}} = libsnarl:user_list(),
    {Res, Req, State};

handle_request(Req, State = #state{path = [User]}) ->
    {reply, {ok, {user, Name, _, Permissions, Groups, _}}} = libsnarl:user_get(User),
    {[{name, Name},
      {permissions, lists:map(fun jsonify_permissions/1, Permissions)},
      {groups, Groups}], Req, State};

handle_request(Req, State = #state{path = [User, <<"permissions">>]}) ->
    {reply, {ok, {user, _Name, _, Permissions, _Groups, _}}} = libsnarl:user_get(User),
    {lists:map(fun jsonify_permissions/1, Permissions), Req, State};

handle_request(Req, State = #state{path = [User, <<"groups">>]}) ->
    {reply, {ok, {user, _Name, _, _Permissions, _, Groups}}} = libsnarl:user_get(User),
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
    {reply, {ok, {token, Token}}} = libsnarl:auth(User, Password),
    {ok, Req1} = cowboy_http_req:set_resp_header(<<"X-Snarl-Token">>, Token, Req),
    {true, Req1, State};

handle_write(Req, State = #state{path =  [User]}, [{<<"password">>, Password}]) ->
    libsnarl:user_add(User),
    {reply, ok} = libsnarl:user_passwd(User, Password),
    {true, Req, State};

handle_write(Req, State = #state{path = [User, <<"groups">>, Group]}, []) ->
    {reply, {ok, joined}} = libsnarl:user_join(User, Group),
    {true, Req, State};

handle_write(Req, State = #state{path = [User, <<"permissions">> | Permission]}, []) ->
    P = erlangify_permission(Permission),
    {reply, ok} = libsnarl:user_grant(User, P),
    {true, Req, State}.


%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [User, <<"permissions">> | Permission]}) ->
    P = erlangify_permission(Permission),
    {reply, ok} = libsnarl:user_revoke(User, P),
    {true, Req, State};

delete_resource(Req, State = #state{path = [User]}) ->
    {reply, ok} = libsnarl:user_delete(User),
    {true, Req, State};

delete_resource(Req, State = #state{path = [User, <<"groups">>, Group]}) ->
    {reply, ok} = libsnarl:user_leave(User, Group),
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
	{reply,not_found} ->
	    true;
	{reply, true} ->
	    false;
	{reply, false} ->
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

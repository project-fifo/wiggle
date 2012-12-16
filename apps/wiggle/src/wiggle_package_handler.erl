%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_package_handler).

-export([init/3,
	 rest_init/2]).

-export([content_types_provided/2,
	 content_types_accepted/2,
	 allowed_methods/2,
	 resource_exists/2,
	 delete_resource/2,
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
    wiggle_handler:initial_state(Req, <<"packages">>).

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

allowed_methods(_Version, _Token, [_Package]) ->
    ['GET', 'PUT', 'DELETE'].

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [Package]}) ->
    case libsniffle:package_attribute_get(Package) of
	not_found ->
	    {false, Req, State};
	{ok, _} ->
	    {true, Req, State}
    end.

is_authorized(Req, State = #state{method = 'OPTIONS'}) ->
    {true, Req, State};

is_authorized(Req, State = #state{token = undefined}) ->
    {{false, <<"X-Snarl-Token">>}, Req, State};

is_authorized(Req, State) ->
    {true, Req, State}.

forbidden(Req, State = #state{method = 'OPTIONS'}) ->
    {false, Req, State};

forbidden(Req, State = #state{token = undefined}) ->
    {true, Req, State};

forbidden(Req, State = #state{path = []}) ->
    {allowed(State#state.token, [<<"packages">>]), Req, State};

forbidden(Req, State = #state{method = 'GET', path = [Package]}) ->
    {allowed(State#state.token, [<<"packages">>, Package, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = 'DELETE', path = [Package]}) ->
    {allowed(State#state.token, [<<"packages">>, Package, <<"delete">>]), Req, State};

forbidden(Req, State = #state{method = 'PUT', path = [Package]}) ->
    {allowed(State#state.token, [<<"packages">>, Package, <<"edit">>]), Req, State};

forbidden(Req, State) ->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

to_json(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {jsx:encode(Reply), Req1, State1}.

handle_request(Req, State = #state{token = Token, path = []}) ->
    {ok, Permissions} = libsnarl:user_cache({token, Token}),
    {ok, Res} = libsniffle:package_list({must, 'allowed', [<<"package">>, {<<"res">>, <<"name">>}, <<"get">>], Permissions}),
    {lists:map(fun ({_, E}) -> E end,  Res), Req, State};

handle_request(Req, State = #state{path = [Package]}) ->
    {ok, Res} = libsniffle:package_attribute_get(Package),
    {[{name, Package}| Res], Req, State}.


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

handle_write(Req, State = #state{path = [Package]}, Body) ->
    {<<"ram">>, Ram} = lists:keyfind(<<"ram">>, 1, Body),
    {<<"quota">>, Quota} = lists:keyfind(<<"quota">>, 1, Body),
    Data = [{<<"quota">>, Quota},
	    {<<"ram">>, Ram}],
    Data1 = case lists:keyfind(<<"cpu_cap">>, 1, Body) of
		{<<"cpu_cap">>, VCPUS} ->
		    [{<<"cpu_cap">>, VCPUS} | Data];
		_ ->
		    Data
	    end,
    ok = libsniffle:package_create(Package),
    ok = libsniffle:package_attribute_set(Package,Data1),
    {true, Req, State};

handle_write(Req, State, _Body) ->
    {fase, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [Package]}) ->
    ok = libsniffle:package_delete(Package),
    {true, Req, State}.

allowed(Token, Perm) ->
    case libsnarl:allowed({token, Token}, Perm) of
	not_found ->
	    true;
	true ->
	    false;
	false ->
	    true
    end.

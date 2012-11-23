%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_vm_handler).

-export([init/3, 
	 rest_init/2]).

-export([content_types_provided/2, 
	 content_types_accepted/2,
	 allowed_methods/2,
	 resource_exists/2,
	 delete_resource/2,
	 forbidden/2,
	 post_is_create/2,
	 create_path/2,
	 options/2,
	 is_authorized/2]).

-export([to_json/2,
	 from_json/2]).

-record(state, {path, method, version, token, content, reply}).
 
init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Req, _) ->
    {Method, Req1} = cowboy_http_req:method(Req),
    {[<<"api">>, Version, <<"vms">> | Path], Req2} = cowboy_http_req:path(Req1),
    {Token, Req3} = case cowboy_http_req:header(<<"X-Snarl-Token">>, Req2) of
			{undefined, ReqX} -> 
			    {undefined, ReqX};
			{TokenX, ReqX} ->
			    {ok, ReqX1} = cowboy_http_req:set_resp_header(<<"X-Snarl-Token">>, TokenX, ReqX),
			    {TokenX, ReqX1}
		    end,
    {ok, Req4} = cowboy_http_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req3),
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
    {ok, Req6, State}.

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

post_is_create(Req, State) ->
    {true, Req, State}.

allowed_methods(Req, State) ->
    {['HEAD', 'OPTIONS' | allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

allowed_methods(_Version, _Token, []) ->
    ['GET', 'POST'];

allowed_methods(_Version, _Token, [_Vm]) ->
    ['GET', 'PUT', 'DELETE'].

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [Vm]}) ->
    case libsniffle:vm_get(Vm) of
	{reply, {ok, not_found}} ->
	    {false, Req, State};
	{reply, _} ->
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

forbidden(Req, State = #state{method = 'GET', path = []}) ->
    {allowed(State#state.token, [<<"vms">>]), Req, State};

forbidden(Req, State = #state{method = 'POST', path = []}) ->
    {allowed(State#state.token, [<<"vms">>, <<"create">>]), Req, State};

forbidden(Req, State = #state{method = 'GET', path = [Vm]}) ->
    {allowed(State#state.token, [<<"vms">>, Vm, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = 'DELETE', path = [Vm]}) ->
    {allowed(State#state.token, [<<"vms">>, Vm, <<"delete">>]), Req, State};

forbidden(Req, State = #state{method = 'PUT', path = [Vm]}) ->
    {allowed(State#state.token, [<<"vms">>, Vm, <<"edit">>]), Req, State};

forbidden(Req, State) ->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

to_json(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {jsx:encode(Reply), Req1, State1}.

handle_request(Req, State = #state{path = []}) ->
    {reply, {ok, Res}} = libsniffle:vm_list(),
    {Res, Req, State};

handle_request(Req, State = #state{path = [Vm]}) ->
    {reply, {ok, {vm, Name, _, Hypervisor, Dict}}} = libsniffle:vm_get(Vm),
    {[{<<"uuid">>, Name}, 
      {<<"hypervisor">>, Hypervisor} | dict:to_list(Dict)], Req, State}.


%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------


create_path(Req, State = #state{path = [], version = Version, token = Token}) ->
    {ok, Body, Req1} = cowboy_http_req:body(Req),
    {Decoded, Req2} = case Body of
			  <<>> ->
			      {[], Req1};
			  _ ->
			      D = jsx:decode(Body),
			      {D, Req1}
		      end,
    {<<"dataset">>, Dataset} = lists:keyfind(<<"dataset">>, 1, Decoded),
    {<<"package">>, Package} = lists:keyfind(<<"package">>, 1, Decoded),
    {<<"config">>, Config} = lists:keyfind(<<"config">>, 1, Decoded),
    {reply, {ok, {user, Owner, _, _, _, _}}} = libsnarl:user_get({token, Token}),
    {reply, {ok, UUID}} = libsniffle:create(Package, Dataset, [{<<"owner">>, Owner} | Config]),
    {<<"/api/", Version/binary, "/vms/", UUID/binary>>, Req2, State}.


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

handle_write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"start">>}]) ->
    libsniffle:vm_start(Vm),
    {true, Req, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"stop">>}]) ->
    libsniffle:vm_stop(Vm),
    {true, Req, State};

handle_write(Req, State = #state{path = [Vm]}, [{<<"action">>, <<"reboot">>}]) ->
    libsniffle:vm_reboot(Vm),
    {true, Req, State};

handle_write(Req, State = #state{path = []}, _Body) ->
    {true, Req, State};

handle_write(Req, State, _Body) ->
    {fase, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [Vm]}) ->
    {reply, ok} = libsniffle:vm_delete(Vm),
    {true, Req, State}.

allowed(Token, Perm) ->
    case libsnarl:allowed({token, Token}, Perm) of
	{reply,not_found} ->
	    true;
	{reply, true} ->
	    false;
	{reply, false} ->
	    true
    end.

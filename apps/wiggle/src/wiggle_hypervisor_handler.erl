%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_hypervisor_handler).

-export([init/3, 
	 rest_init/2]).

-export([content_types_provided/2, 
	 content_types_accepted/2,
	 allowed_methods/2,
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
    {[<<"api">>, Version, <<"hypervisors">> | Path], Req2} = cowboy_http_req:path(Req1),
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

allowed_methods(Req, State) ->
    {['HEAD', 'OPTIONS' | allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

allowed_methods(_Version, _Token, []) ->
    ['GET'];

allowed_methods(_Version, _Token, [_Hypervisor]) ->
    ['GET'].

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [Hypervisor | _]}) ->
    case libsniffle:hypervisor_resource_get(Hypervisor) of
	{reply, not_found} ->
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

forbidden(Req, State = #state{path = []}) ->
    {allowed(State#state.token, [<<"hypervisors">>]), Req, State};

forbidden(Req, State = #state{method = 'GET', path = [Hypervisor]}) ->
    {allowed(State#state.token, [<<"hypervisors">>, Hypervisor, <<"get">>]), Req, State};

forbidden(Req, State) ->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

to_json(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {jsx:encode(Reply), Req1, State1}.

handle_request(Req, State = #state{path = []}) ->
    {reply, {ok, Res}} = libsniffle:hypervisor_list(),
    {Res, Req, State};

handle_request(Req, State = #state{path = [Hypervisor]}) ->
    {reply, Dict} = libsniffle:hypervisor_resource_get(Hypervisor),
    {dict:to_list(Dict), Req, State}.

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

handle_write(Req, State, _Body) ->
    {false, Req, State}.


%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

%% Internal Functions

allowed(Token, Perm) ->
    case libsnarl:allowed({token, Token}, Perm) of
	{reply,not_found} ->
	    true;
	{reply, true} ->
	    false;
	{reply, false} ->
	    true
    end.

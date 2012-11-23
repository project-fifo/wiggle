%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_iprange_handler).

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

-record(state, {path, method, version, token, content, reply}).
 
init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Req, _) ->
    {Method, Req1} = cowboy_http_req:method(Req),
    {[<<"api">>, Version, <<"ipranges">> | Path], Req2} = cowboy_http_req:path(Req1),
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

allowed_methods(_Version, _Token, [_Iprange]) ->
    ['GET', 'PUT', 'DELETE'].

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [Iprange]}) ->
    case libsniffle:iprange_get(Iprange) of
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

forbidden(Req, State = #state{path = []}) ->
    {allowed(State#state.token, [<<"ipranges">>]), Req, State};

forbidden(Req, State = #state{method = 'GET', path = [Iprange]}) ->
    {allowed(State#state.token, [<<"ipranges">>, Iprange, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = 'DELETE', path = [Iprange]}) ->
    {allowed(State#state.token, [<<"ipranges">>, Iprange, <<"delete">>]), Req, State};

forbidden(Req, State = #state{method = 'PUT', path = [Iprange]}) ->
    {allowed(State#state.token, [<<"ipranges">>, Iprange, <<"edit">>]), Req, State};

forbidden(Req, State) ->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

to_json(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {jsx:encode(Reply), Req1, State1}.

handle_request(Req, State = #state{path = []}) ->
    {reply, {ok, Res}} = libsniffle:iprange_list(),
    {Res, Req, State};

handle_request(Req, State = #state{path = [Iprange]}) ->
    {reply, {ok, {iprange, 
		  Name, 
		  Network,
		  Gateway,
		  Netmask,
		  First,
		  Last,
		  Current,
		  _,
		  Free}}} = libsniffle:iprange_get(Iprange),
    {[
      {name, Name},
      {network, ip_to_str(Network)},
      {gateway, ip_to_str(Gateway)},
      {netmask, ip_to_str(Netmask)},
      {first, ip_to_str(First)},
      {last, ip_to_str(Last)},
      {current, ip_to_str(Current)},
      {free, lists:map(fun(IP) -> ip_to_str(IP) end, Free)}
     ], Req, State}.


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

handle_write(Req, State = #state{path = [Iprange]}, Body) ->
    {<<"network">>, Network} = lists:keyfind(<<"network">>, 1, Body),
    {<<"gateway">>, Gateway} = lists:keyfind(<<"gateway">>, 1, Body),
    {<<"netmask">>, Netmask} = lists:keyfind(<<"netmask">>, 1, Body),
    {<<"first">>, First} = lists:keyfind(<<"first">>, 1, Body),
    {<<"last">>, Last} = lists:keyfind(<<"last">>, 1, Body),
						% We use the name as tag.
    {reply, ok} = libsniffle:iprange_create(Iprange, Network, Gateway, Netmask, First, Last, Iprange),
    {true, Req, State};

handle_write(Req, State, _Body) ->
    {fase, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [Iprange]}) ->
    {reply, ok} = libsniffle:iprange_delete(Iprange),
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


ip_to_str(Ip) ->
    <<A:8, B:8, C:8, D:8>> = <<Ip:32>>,
    list_to_binary(io_lib:format("~p.~p.~p.~p", [A, B, C, D])).

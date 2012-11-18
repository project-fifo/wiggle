%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_dataset_handler).

-export([init/3, 
	 rest_init/2]).

-export([content_types_provided/2, 
	 content_types_accepted/2,
	 allowed_methods/2,
	 resource_exists/2,
	 forbidden/2,
	 is_authorized/2]).
-export([to_json/2,
	 from_json/2]).

-record(state, {path, method, version, token, content, reply}).
 
init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_http_rest}.

rest_init(Req, _) ->
    {Method, Req1} = cowboy_http_req:method(Req),
    {[<<"api">>, Version, <<"datasets">> | Path], Req2} = cowboy_http_req:path(Req1),
    {Token, Req3} = case cowboy_http_req:header(<<"X-Snarl-Token">>, Req2) of
			{undefined, ReqX} -> 
			    {undefined, ReqX};
			{TokenX, ReqX} ->
			    {ok, ReqX1} = cowboy_http_req:set_resp_header(<<"X-Snarl-Token">>, TokenX, ReqX),
			    {TokenX, ReqX1}
		    end,
    State =  #state{version = Version, 
		    method = Method,
		    token = Token,
		    path = Path},
    {ok, Req3, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json; charset=UTF-8">>, from_json}
     ], Req, State}.

allowed_methods(Req, State) ->
    {['HEAD' | allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

allowed_methods(_Version, _Token, []) ->
    ['GET'];

allowed_methods(_Version, _Token, [_Dataset]) ->
    ['GET'].

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [Dataset]}) ->
    case libsniffle:dataset_attribute_get(Dataset) of
	{reply, not_found} ->
	    {false, Req, State};
	{reply, _} ->
	    {true, Req, State}
    end.

is_authorized(Req, State = #state{token = undefined}) -> 
    {{false, <<"X-Snarl-Token">>}, Req, State};

is_authorized(Req, State) -> 
    {true, Req, State}.


forbidden(Req, State = #state{token = undefined}) -> 
    {true, Req, State};

forbidden(Req, State = #state{path = []}) ->
    {allowed(State#state.token, [<<"datasets">>]), Req, State};

forbidden(Req, State = #state{method = 'GET', path = [Dataset]}) ->
    {allowed(State#state.token, [<<"datasets">>, Dataset, <<"get">>]), Req, State};

forbidden(Req, State) ->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

to_json(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {jsx:encode(Reply), Req1, State1}.

handle_request(Req, State = #state{path = []}) ->
    {reply, {ok, Res}} = libsniffle:dataset_list(),
    {Res, Req, State};

handle_request(Req, State = #state{path = [Dataset]}) ->
    {reply, Res} = libsniffle:dataset_attribute_get(Dataset),
    {Res, Req, State}.


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
    {fase, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

allowed(Token, Perm) ->
    case libsnarl:allowed({token, Token}, Perm) of
	{reply,not_found} ->
	    true;
	{reply, true} ->
	    false;
	{reply, false} ->
	    true
    end.

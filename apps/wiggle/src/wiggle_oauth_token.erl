-module(wiggle_oauth_token).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-ignore_xref([init/3]).
-ignore_xref([handle/2]).
-ignore_xref([terminate/3]).

-record(token_req, {
          grant_type,
          code,
          client_id,
          client_secret,
          redirect_uri
         }).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
	ok.

handle(Req, State) ->
    {ok, Req3} = case cowboy_req:method(Req) of
                     {<<"POST">>, Req2} ->
                         do_post(Req2);
                     {_, Req2} ->
                         cowboy_req:reply(405, Req2)
                 end,
    {ok, Req3, State}.

do_post(Req) ->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    do_vals(PostVals, Req2).

do_vals(Vals, Req) ->
    GrantType = proplists:get_value(<<"grant_type">>, Vals),
    Code = proplists:get_value(<<"code">>, Vals),
    RedirectURI = proplists:get_value(<<"redirect_uri">>, Vals),
    ClientID = proplists:get_value(<<"client_id">>, Vals),
    ClientSecret = proplists:get_value(<<"client_secret">>, Vals),
    TokenReq = #token_req{
                  grant_type = GrantType,
                  code = Code,
                  redirect_uri = RedirectURI,
                  client_id = ClientID,
                  client_secret = ClientSecret
                 },
    do_basic_auth(TokenReq, Req).

do_basic_auth(TokenReq, Req) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {<<"basic">>, {ClientID, ClientSecret}} ->
            TokenReq1 = TokenReq#token_req{client_id = ClientID,
                                           client_secret = ClientSecret},
            do_request(TokenReq1, Req);
        _ ->
            do_request(TokenReq, Req)
    end.

do_request(#token_req{grant_type = <<"authorization_code">>, code = Code,
                      client_id = ClientId, client_secret = ClientSecret,
                      redirect_uri = RedirectURI},
           Req) when is_binary(Code),
                     is_binary(ClientId),
                     is_binary(ClientSecret) ->
    case ls_oauth:authorize_code_grant(ClientId, ClientSecret, Code, RedirectURI) of
        {ok, {_AppContext, Authorization}} ->
            {ok, {_AppContext, Response}} =
                ls_oauth:issue_token_and_refresh(Authorization),
            {ok, AccessToken} = oauth2_response:access_token(Response),
            {ok, Type} = oauth2_response:token_type(Response),
            {ok, Expires} = oauth2_response:expires_in(Response),
            {ok, RefreshToken} = oauth2_response:refresh_token(Response),
            {ok, Scope} = oauth2_response:scope(Response),
            access_refresh_token_response(
              AccessToken, Type, Expires, RefreshToken, Scope, Req);
        {error, Error} ->
            json_error_response(Error, Req)
    end;

do_request(#token_req{grant_type = <<"authorization_code">>}, Req) ->
    json_error_response(invalid_request, Req);

do_request(_, Req) ->
    json_error_response(invalid_request, Req).

json_error_response(Error, Req) ->
    H = [{<<"content-type">>, <<"application/json">>}],
    case Error of
        invalid_client ->
            H1 = [{<<"WWW-Authenticate">>, <<"Basic">>} | H],
            cowboy_req:reply(
              401, H1, <<"{\"error\":\"invalid_client\"}">>, Req);
        unauthorized_client ->
            cowboy_req:reply(
              403, H, <<"{\"error\":\"unauthorized_client\"}">>, Req);
        Other ->
            Error = jsx:encode([{error, atom_to_binary(Other, utf8)}]),
            cowboy_req:reply(400, H, Error, Req)
    end.


access_refresh_token_response(AccessToken, Type, Expires, RefreshToken, Scope,
                              Req) ->
    JSON = [{<<"access_token">>, AccessToken},
            {<<"token_type">>, Type},
            {<<"expires_in">>, Expires},
            {<<"refresh_token">>, RefreshToken},
            {<<"scope">>, Scope}],
    cowboy_req:reply(200, [], jsx:encode(JSON), Req).

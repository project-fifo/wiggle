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
          redirect_uri,
          username,
          password,
          refresh_token,
          scope,
          payload
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
    GrantType = wiggle_oauth:decode_grant_type(
                  proplists:get_value(<<"grant_type">>, Vals)),
    Code = proplists:get_value(<<"code">>, Vals),
    RedirectURI = proplists:get_value(<<"redirect_uri">>, Vals),
    ClientID = proplists:get_value(<<"client_id">>, Vals),
    ClientSecret = proplists:get_value(<<"client_secret">>, Vals),
    %% Those are only used for the 4.3.2 Password grand
    Username = proplists:get_value(<<"username">>, Vals),
    Password = proplists:get_value(<<"password">>, Vals),
    Scope = proplists:get_value(<<"scope">>, Vals),
    %% Used for 6 - refresh access tokens
    RefreshToken = proplists:get_value(<<"refresh_token">>, Vals),
    TokenReq = #token_req{
                  grant_type = GrantType,
                  code = Code,
                  redirect_uri = RedirectURI,
                  client_id = ClientID,
                  client_secret = ClientSecret,
                  username = Username,
                  password = Password,
                  scope = Scope,
                  refresh_token = RefreshToken
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

do_request(TokenReq = #token_req{grant_type = authorization_code}, Req) ->
    do_authorization_code(TokenReq, Req);

%% 4.3.2
do_request(TokenReq = #token_req{grant_type = password}, Req) ->
    do_password(TokenReq, Req);

do_request(TokenReq = #token_req{grant_type = client_credentials}, Req) ->
    do_client_credentials(TokenReq, Req);

do_request(TokenReq = #token_req{grant_type = refresh_token}, Req) ->
    do_refresh_token(TokenReq, Req);



do_request(_, Req) ->
    wiggle_oauth:json_error_response(invalid_request, Req).

do_authorization_code(#token_req{code = Code, client_id = ClientId,
                                 client_secret = ClientSecret,
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
            wiggle_oauth:access_refresh_token_response(
              AccessToken, Type, Expires, RefreshToken, Scope, Req);
        {error, Error} ->
            wiggle_oauth:json_error_response(Error, Req)
    end;

do_authorization_code(_, Req) ->
    wiggle_oauth:json_error_response(invalid_request, Req).

%% 4.3.2
do_password(#token_req{username = Username, password = Password,
                        scope = Scope}, Req)
  when is_binary(Username),
       is_binary(Password) ->
    case ls_oauth:authorize_password(Username, Password, Scope) of
        {ok, {_AppContext, Authorization}} ->
            {ok, {_AppContext, Response}} =
                ls_oauth:issue_token(Authorization),
            {ok, AccessToken} = oauth2_response:access_token(Response),
            {ok, Type} = oauth2_response:token_type(Response),
            {ok, Expires} = oauth2_response:expires_in(Response),
            {ok, VerifiedScope} = oauth2_response:scope(Response),
            wiggle_oauth:access_token_response(
              AccessToken, Type, Expires, VerifiedScope, Req);
        {error, Error} ->
            wiggle_oauth:json_error_response(Error, Req)
    end;

do_password(_, Req) ->
    wiggle_oauth:json_error_response(invalid_request, Req).

do_client_credentials(#token_req{client_id = ClientId,
                                 client_secret = ClientSecret,
                                 scope = Scope}, Req)
  when is_binary(ClientId),
       is_binary(ClientSecret) ->
    case ls_oauth:authorize_client_credentials(ClientId, ClientSecret, Scope) of
        {ok, {_AppContext, Authorization}} ->
            {ok, {_AppContext, Response}} =
                ls_oauth:issue_token(Authorization),
            {ok, Token} =
                oauth2_response:access_token(Response),
            {ok, Type} = oauth2_response:token_type(Response),
            {ok, Expires} =
                oauth2_response:expires_in(Response),
            {ok, Scope} = oauth2_response:scope(Response),
            wiggle_oauth:access_token_response(
              Token, Type, Expires, Scope, Req);
        {error, Error} ->
            wiggle_oauth:json_error_response(Error, Req)
    end;
do_client_credentials(_, Req) ->
    wiggle_oauth:json_error_response(invalid_request, Req).


do_refresh_token(#token_req{ refresh_token = RefreshToken, client_id = ClientId,
                             client_secret = ClientSecret, scope = Scope}, Req)
  when is_binary(ClientId),
       is_binary(ClientSecret),
       is_binary(RefreshToken) ->
    case ls_oauth:refresh_access_token(ClientId, ClientSecret, RefreshToken,
                                       Scope) of
        {ok, {_AppContext, Response}} ->
            {ok, AccessToken} =
                oauth2_response:access_token(Response),
            {ok, Type} =
                oauth2_response:token_type(Response),
            {ok, Expires} =
                oauth2_response:expires_in(Response),
            {ok, ResponseScope} =
                oauth2_response:scope(Response),
            wiggle_oauth:access_token_response(
              AccessToken, Type, Expires, ResponseScope, Req);
        {error, Error} ->
            wiggle_oauth:json_error_response(Error, Req)
    end.

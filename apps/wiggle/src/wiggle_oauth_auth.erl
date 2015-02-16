-module(wiggle_oauth_auth).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-ignore_xref([init/3]).
-ignore_xref([handle/2]).
-ignore_xref([terminate/3]).


-record(auth_req, {
          response_type,
          client_id,
          redirect_uri,
          scope,
          username,
          password,
          state,
          method,
          bearer
         }).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
	ok.

handle(Req, State) ->
    {ok, Req3} = case cowboy_req:method(Req) of
                     %% TODO: This should prompt a permission form
                     %% And check for the bearer token
                     {<<"GET">>, Req2} ->
                         do_get(Req2);
                     %% TODO: This should do the actual redirect etc.
                     {<<"POST">>, Req2} ->
                         do_post(Req2);
                     {_, Req2} ->
                         cowboy_req:reply(405, Req2)
                 end,
    lager:info("[oath:auth] Request finished!"),
    {ok, Req3, State}.

do_get(Req) ->
    {QSVals, Req2} = cowboy_req:qs_vals(Req),
    AuthReq = #auth_req{method = get},
    do_vals(AuthReq, QSVals, Req2).

do_post(Req)->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    AuthReq = #auth_req{method = post},
    do_vals(AuthReq, PostVals, Req2).

do_vals(AuthReq, Vals, Req) ->
    ResponseType = wiggle_oauth:decode_response_type(
                     proplists:get_value(<<"response_type">>, Vals)),
    ClientID = proplists:get_value(<<"client_id">>, Vals),
    RedirectURI = proplists:get_value(<<"redirect_uri">>, Vals),
    Scope = proplists:get_value(<<"scope">>, Vals),
    State = proplists:get_value(<<"state">>, Vals),
    Username = proplists:get_value(<<"username">>, Vals),
    Password = proplists:get_value(<<"password">>, Vals),
    AuthReq1 = AuthReq#auth_req{
                 response_type = ResponseType,
                 client_id = ClientID,
                 redirect_uri = RedirectURI,
                 username = Username,
                 password = Password,
                 scope = Scope,
                 state = State},
    do_basic_auth(AuthReq1, Req).

do_basic_auth(AuthReq, Req) ->
    {ok, Auth, Req1} = cowboy_req:parse_header(<<"authorization">>, Req),
    case Auth of
        {<<"basic">>, {Username, Password}} ->
            AuthReq1 = AuthReq#auth_req{username = Username, password = Password},
            update_scope(AuthReq1, Req1);
        {<<"bearer">>, Bearer} ->
            AuthReq1 = AuthReq#auth_req{bearer = Bearer},
            update_scope(AuthReq1, Req1);
        _ ->
            update_scope(AuthReq, Req1)
    end.

update_scope(AuthReq = #auth_req{scope = undefined}, Req) ->
    do_request(AuthReq, Req);

update_scope(AuthReq = #auth_req{scope = Scope}, Req) ->
    ScopeS = binary_to_list(Scope),
    Scope1 = [list_to_binary(X) || X <- string:tokens(ScopeS, " ")],
    do_request(AuthReq#auth_req{scope = Scope1}, Req).

do_request(AuthReq = #auth_req{response_type = code, method = get}, Req) ->
    Params = build_params(AuthReq),
    {ok, Reply}  = oauth_login_form_dtl:render(Params),
    cowboy_req:reply(200, [], Reply, Req);

do_request(AuthReq = #auth_req{response_type = code}, Req) ->
    do_code(AuthReq, Req);

do_request(AuthReq = #auth_req{response_type = token}, Req) ->
    do_token(AuthReq, Req);

do_request(#auth_req{}, Req) ->
    wiggle_oauth:json_error_response(unsupported_response_type, Req).


%% 4.1.1
do_code(#auth_req{
           client_id = ClientID,
           redirect_uri = URI,
           username = Username,
           password = Password,
           scope = Scope,
           state = State}, Req)
  when is_binary(Username),
       is_binary(Password),
       is_binary(ClientID) ->
    case ls_oauth:authorize_code_request(
           Username, Password, ClientID, URI, Scope) of
        {ok, {_AppContext, Authorization}} ->
            {ok, {_AppContext2, Response}} = ls_oauth:issue_code(Authorization),
            {ok, Code} = oauth2_response:access_code(Response),
            wiggle_oauth:redirected_authorization_code_response(URI, Code, State, Req);
        {error, unauthorized_client} ->
            %% cliend_id is not registered or redirection_uri is not valid
            wiggle_oauth:json_error_response(unauthorized_client, Req);
        {error, Error} ->
            wiggle_oauth:redirected_error_response(URI, Error, State, Req)
    end;

do_code(#auth_req{redirect_uri = Uri, state = State}, Req) ->
    wiggle_oauth:redirected_error_response(Uri, invalid_request, State, Req).

do_token(#auth_req{
            client_id = ClientID,
            redirect_uri = URI,
            username = Username,
            password = Password,
            scope = Scope,
            state = State}, Req) 
  when is_binary(Username),
       is_binary(Password),
       is_binary(ClientID) ->
    case
        ls_oauth:authorize_password(Username, Password, ClientID, URI, Scope) of
        {ok, {_AppContext, Authorization}} ->
            {ok, {_AppContext, Response}} =
                ls_oauth:issue_token(Authorization),
            {ok, AccessToken} = oauth2_response:access_token(Response),
            {ok, Type} = oauth2_response:token_type(Response),
            {ok, Expires} = oauth2_response:expires_in(Response),
            {ok, VerifiedScope} = oauth2_response:scope(Response),
            wiggle_oauth:redirected_access_token_response(URI,
                                                          AccessToken,
                                                          Type,
                                                          Expires,
                                                          VerifiedScope,
                                                          State,
                                                          Req);
        {error, Error} ->
            wiggle_oauth:redirected_error_response(URI, Error, State, Req)
    end;

do_token(#auth_req{redirect_uri = Uri, state = State}, Req) ->
    wiggle_oauth:redirected_error_response(Uri, invalid_request, State, Req).

build_params(R = #auth_req{response_type = code}) ->
    build_params_code(R, [{response_type, <<"code">>}]);
build_params(R = #auth_req{response_type = token}) ->
    build_params_token(R, [{response_type, <<"token">>}]).


build_params_code(R = #auth_req{client_id = ClientID}, Acc)
  when ClientID =/= undefined ->
    build_params_code1(R, [{client_id, ClientID} | Acc]).

build_params_code1(R = #auth_req{redirect_uri = RedirectURI}, Acc)
  when RedirectURI =/= undefined ->
    build_params_code2(R, [{redirect_uri, RedirectURI} | Acc]);
build_params_code1(R, Acc) ->
    build_params_code2(R, Acc).


build_params_code2(R = #auth_req{scope = Scope}, Acc)
  when Scope =/= undefined ->
    build_params_code3(R, [{scope, Scope} | Acc]);
build_params_code2(R, Acc) ->
    build_params_code3(R, Acc).

build_params_code3(#auth_req{state = State}, Acc)
  when State =/= undefined ->
    [{state, State} | Acc];
build_params_code3(_R, Acc) ->
    Acc.

build_params_token(_, Acc) ->
    Acc.

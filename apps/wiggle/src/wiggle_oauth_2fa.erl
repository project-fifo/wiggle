-module(wiggle_oauth_2fa).

-include("wiggle_oauth.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-ignore_xref([init/3]).
-ignore_xref([handle/2]).
-ignore_xref([terminate/3]).


-record(mfa_req, {
          method,
          redirect_uri,
          otp_token,
          response_type,
          otp,
          state,
          token_data
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
    AuthReq = #mfa_req{method = get},
    do_vals(AuthReq, QSVals, Req2).

do_post(Req)->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    AuthReq = #mfa_req{method = post},
    do_vals(AuthReq, PostVals, Req2).

do_vals(AuthReq, Vals, Req) ->
    ResponseType = wiggle_oauth:decode_response_type(
                     proplists:get_value(<<"response_type">>, Vals)),
    RedirectURI = proplists:get_value(<<"redirect_uri">>, Vals),
    OTPToken = proplists:get_value(<<"fifo_otp_token">>, Vals),
    State = proplists:get_value(<<"state">>, Vals),
    OTP = proplists:get_value(<<"fifo_otp">>, Vals),
    AuthReq1 = AuthReq#mfa_req{
                 response_type = ResponseType,
                 redirect_uri = RedirectURI,
                 otp = OTP,
                 otp_token = OTPToken,
                 state = State},
    do_resolve_token(AuthReq1, Req).

do_resolve_token(AuthReq = #mfa_req{method = get}, Req) ->
    do_request(AuthReq, Req);

do_resolve_token(AuthReq = #mfa_req{method = post, otp_token = OTPToken}, Req) ->
    {ok, TokenData} = ls_token:get(OTPToken),
    ls_token:delete(OTPToken),
    do_request(AuthReq#mfa_req{token_data = TokenData}, Req).

do_request(AuthReq = #mfa_req{method = get}, Req) ->
    Params = build_params(AuthReq),
    {ok, Reply}  = oauth_2fa_form_dtl:render(Params),
    cowboy_req:reply(200, [], Reply, Req);

do_request(AuthReq = #mfa_req{response_type = code}, Req) ->
    do_code(AuthReq, Req);

do_request(AuthReq = #mfa_req{response_type = token}, Req) ->
    do_token(AuthReq, Req);

do_request(#mfa_req{}, Req) ->
    wiggle_oauth:json_error_response(unsupported_response_type, Req).
%% 4.1.1

do_code(#mfa_req{
           otp = OTP,
           redirect_uri = URI,
           state = State,
           token_data = {<<"code">>, UUID, Authorization, URI}}, Req)
  when is_binary(OTP) ->
    case ls_user:yubikey_check(UUID, OTP) of
        {ok, _UUID} ->
            {ok, Response} = ls_oauth:issue_code(Authorization),
            {ok, Code} = oauth2_response:access_code(Response),
            wiggle_oauth:redirected_authorization_code_response(URI, Code, State, Req);
        _ ->
            wiggle_oauth:redirected_error_response(URI, access_denied, State, Req)
    end;


do_code(#mfa_req{redirect_uri = Uri, state = State}, Req) ->
    wiggle_oauth:redirected_error_response(Uri, invalid_request, State, Req).

do_token(#mfa_req{
            redirect_uri = URI,
            otp = OTP,
            state = State,
            token_data = {<<"token">>, UUID, Authorization, URI}}, Req)
  when is_binary(OTP) ->
    case ls_user:yubikey_check(UUID, OTP) of
        {ok, _UUID} ->
            {ok, Response} = ls_oauth:issue_token(Authorization),
            {ok, AccessToken} = oauth2_response:access_token(Response),
            {ok, Type} = oauth2_response:token_type(Response),
            {ok, Expires} = oauth2_response:expires_in(Response),
            {ok, VerifiedScope} = oauth2_response:scope(Response),
            wiggle_oauth:redirected_access_token_response(
              URI, AccessToken, Type, Expires, VerifiedScope, State, Req);
        _ ->
            wiggle_oauth:redirected_error_response(URI, access_denied, State, Req)
    end;

do_token(#mfa_req{redirect_uri = Uri, state = State}, Req) ->
    wiggle_oauth:redirected_error_response(Uri, invalid_request, State, Req).

build_params(R = #mfa_req{response_type = code}) ->
    build_params(R, [{response_type, <<"code">>}]);

build_params(R = #mfa_req{response_type = token}) ->
    build_params(R, [{response_type, <<"token">>}]);

build_params(_) ->
    [{error, <<"illegal request type">>}].

build_params(R = #mfa_req{otp_token = OTPToken}, Acc)
  when OTPToken =/= undefined ->
    build_params1(R, [{fifo_otp_token, OTPToken} | Acc]);
build_params(_, _) ->
    [{error, <<"no_fifo_otp_token">>}].

build_params1(R = #mfa_req{redirect_uri = RedirectURI}, Acc)
  when RedirectURI =/= undefined ->
    build_params2(R, [{redirect_uri, RedirectURI} | Acc]);
build_params1(_, _) ->
    [{error, <<"no_redirect_uri">>}].

build_params2(#mfa_req{state = State}, Acc)
  when State =/= undefined ->
    [{state, State} | Acc];
build_params2(_R, Acc) ->
    Acc.

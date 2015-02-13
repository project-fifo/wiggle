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
          state
         }).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
	ok.

handle(Req, State) ->
    {ok, Req3} = case cowboy_req:method(Req) of
                     {<<"GET">>, Req2} ->
                         do_get(Req2);
                     {<<"POST">>, Req2} ->
                         do_post(Req2);
                     {_, Req2} ->
                         cowboy_req:reply(405, Req2)
                 end,
    {ok, Req3, State}.
%% #{echo := Echo} = cowboy_req:match_qs([echo], Req),
%% Req2 = echo(Method, Echo, Req),
%% {ok, Req2, Opts}.


do_get(Req) ->
    {QSVals, Req2} = cowboy_req:qs_vals(Req),
    do_vals(QSVals, Req2).

do_post(Req)->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    do_vals(PostVals, Req2).

do_vals(Vals, Req) ->
    ResponseType = proplists:get_value(<<"response_type">>, Vals),
    ClientID = proplists:get_value(<<"client_id">>, Vals),
    RedirectURI = proplists:get_value(<<"redirect_uri">>, Vals),
    Scope = proplists:get_value(<<"scope">>, Vals),
    State = proplists:get_value(<<"state">>, Vals),
    Username = proplists:get_value(<<"username">>, Vals),
    Password = proplists:get_value(<<"password">>, Vals),
    AuthReq = #auth_req{
                 response_type = ResponseType,
                 client_id = ClientID,
                 redirect_uri = RedirectURI,
                 username = Username,
                 password = Password,
                 scope = Scope,
                 state = State},
    do_basic_auth(AuthReq, Req).

do_basic_auth(AuthReq, Req) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {<<"basic">>, {Username, Password}} ->
            AuthReq1 = AuthReq#auth_req{username = Username, password = Password},
            update_scope(AuthReq1, Req);
        _ ->
            update_scope(AuthReq, Req)
    end.

update_scope(AuthReq = #auth_req{scope = undefined}, Req) ->
    do_request(AuthReq, Req);

update_scope(AuthReq = #auth_req{scope = Scope}, Req) ->
    ScopeS = binary_to_list(Scope),
    Scope1 = [list_to_binary(X) || X <- string:tokens(ScopeS, " ")],
    do_request(AuthReq#auth_req{scope = Scope1}, Req).

do_request(#auth_req{
              response_type = <<"code">>,
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
            redirected_authorization_code_response(URI, Code, State, Req);
        {error, unauthorized_client} ->
            %% cliend_id is not registered or redirection_uri is not valid
            json_error_response(unauthorized_client, Req);
        {error, Error} ->
            redirected_error_response(URI, Error, State, Req)
    end;
do_request(#auth_req{response_type = <<"code">>, redirect_uri = Uri,
                     state = State}, Req) ->
    redirected_error_response(Uri, invalid_request, State, Req);


do_request(#auth_req{}, Req) ->
    json_error_response(unsupported_response_type, Req).

redirected_authorization_code_response(Uri, Code, State, Req) ->
    Params = [{<<"code">>, Code}, {<<"state">>, State}],
    Location = <<Uri/binary, "?", (cow_qs:qs(Params))/binary>>,
    cowboy_req:reply(302, <<>>, [{location, Location}], Req).


json_error_response(Error, Req) ->
    H = [{<<"content-type">>, <<"application/json">>}],
    case Error of
        invalid_client ->
            H1 = [{<<"WWW-Authenticate">>, <<"Basic">>} | H],
            cowboy_req:reply(
              401, <<"{\"error\":\"invalid_client\"}">>, H1, Req);
        unauthorized_client ->
            cowboy_req:reply(
              403, <<"{\"error\":\"unauthorized_client\"}">>, H, Req);
        Other ->
            Error = jsx:encode([{error, atom_to_binary(Other, utf8)}]),
            cowboy_req:reply(
              400, Error, H, Req)
    end.

redirected_error_response(Uri, Error, undefined, Req) ->
    Params = [{<<"error">>, atom_to_binary(Error, utf8)}],
    Location = <<Uri/binary, "?", (cow_qs:qs(Params))/binary>>,
    cowboy_req:reply(302, <<>>, [{location, Location}], Req);

redirected_error_response(Uri, Error, State, Req) ->
    Params = [{<<"error">>, atom_to_binary(Error, utf8)}, {<<"state">>, State}],
    Location = <<Uri/binary, "?", (cow_qs:qs(Params))/binary>>,
    cowboy_req:reply(302, <<>>, [{location, Location}], Req).


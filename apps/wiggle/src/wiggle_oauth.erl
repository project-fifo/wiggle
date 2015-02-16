-module(wiggle_oauth).

-export([
         redirected_authorization_code_response/4,
         redirected_access_token_response/7,
         redirected_error_response/4,
         json_error_response/2,
         decode_grant_type/1,
         decode_response_type/1,
         access_token_response/5,
         access_refresh_token_response/6
         ]).

redirected_authorization_code_response(Uri, Code, State, Req) ->
    Params = [{<<"code">>, Code}, {<<"state">>, State}],
    Location = <<Uri/binary, "?", (cow_qs:qs(Params))/binary>>,
    cowboy_req:reply(302, [{<<"location">>, Location}], <<>>, Req).


json_error_response(Error, Req) ->
    H = [{<<"content-type">>, <<"application/json">>}],
    ErrorJSON = build_error(Error),
    ErrorBin = jsx:encode(ErrorJSON),
    {Code, H1} =
        case Error of
            invalid_client ->
                {401, [{<<"WWW-Authenticate">>, <<"Basic">>} | H]};
            unauthorized_client ->
                {403, H};
            _Other ->
                {400, H}
    end,
    cowboy_req:reply(Code, H1, ErrorBin, Req).

redirected_error_response(Uri, Error, undefined, Req) ->
    Params = [{<<"error">>, atom_to_binary(Error, utf8)}],
    Location = <<Uri/binary, "?", (cow_qs:qs(Params))/binary>>,
    cowboy_req:reply(302, [{<<"location">>, Location}], <<>>, Req);

redirected_error_response(Uri, Error, State, Req) ->
    Params = [{<<"error">>, atom_to_binary(Error, utf8)}, {<<"state">>, State}],
    Location = <<Uri/binary, "?", (cow_qs:qs(Params))/binary>>,
    cowboy_req:reply(302, [{<<"location">>, Location}], <<>>, Req).


access_refresh_token_response(AccessToken, Type, Expires, RefreshToken, Scope,
                              Req) ->
    JSON = [{<<"access_token">>, AccessToken},
            {<<"token_type">>, Type},
            {<<"expires_in">>, Expires},
            {<<"refresh_token">>, RefreshToken},
            {<<"scope">>, Scope}],
    cowboy_req:reply(200, [], jsx:encode(JSON), Req).


redirected_access_token_response(Uri, Token, Type, Expires, Scope,
                                 State, Req) ->
    Params = [{<<"access_token">>, Token},
              {<<"token_type">>, Type},
              {<<"expires_in">>, Expires},
              {<<"state">>, State},
              {<<"scope">>, Scope}],
    Location = <<Uri/binary, "#", (cow_qs:qs(Params))/binary>>,
    cowboy_req:reply(302, [{<<"location">>, Location}], <<>>, Req).

access_token_response(AccessToken, Type, Expires, Scope, Req) ->
    JSON = [{<<"access_token">>, AccessToken},
            {<<"token_type">>, Type},
            {<<"expires_in">>, Expires},
            {<<"scope">>, Scope}],
    cowboy_req:reply(200, [{<<"cache-control">>, <<"no-store">>}],
                     jsx:encode(JSON), Req).


build_error(Error) ->
    {ErrorName, Desc} = errod_description(Error),
    [{<<"error">>, ErrorName},
     {<<"error_description">>, Desc}].


errod_description(invalid_request) ->
    {<<"invalid_request">>,
     <<"The request is missing a required parameter, includes an ",
       "invalid parameter value, includes a parameter more than ",
       "once, or is otherwise malformed.">>};

errod_description(invalid_client) ->
    {<<"invalid_client">>,
     <<"Client authentication failed (e.g., unknown client, no ",
       "client authentication included, or unsupported ",
       "authentication method).  The authorization server MAY ",
       "return an HTTP 401 (Unauthorized) status code to indicate ",
       "which HTTP authentication schemes are supported.  If the ",
       "client attempted to authenticate via the \"Authorization\" ",
       "request header field, the authorization server MUST ",
       "respond with an HTTP 401 (Unauthorized) status code and ",
       "include the \"WWW-Authenticate\" response header field ",
       "matching the authentication scheme used by the client.">>};

errod_description(unauthorized_client) ->
    {<<"unauthorized_client">>,
     <<"The client is not authorized to request an authorization ",
       "code using this method.">>};

errod_description(access_denied) ->
    {<<"access_denied">>,
     <<"The resource owner or authorization server denied the ",
       "request.">>};

errod_description(unsupported_response_type) ->
    {<<"unsupported_response_type">>,
     <<"The authorization server does not support obtaining an ",
       "authorization code using this method.">>};

errod_description(invalid_scope) ->
    {<<"invalid_scope">>,
     <<"The requested scope is invalid, unknown, or malformed.">>};

errod_description(server_error) ->
    {<<"server_error">>,
     <<"The authorization server encountered an unexpected ",
       "condition that prevented it from fulfilling the request. ",
       "(This error code is needed because a 500 Internal Server ",
       "Error HTTP status code cannot be returned to the client ",
       "via an HTTP redirect.)">>};

errod_description(temporarily_unavailable) ->
    {<<"temporarily_unavailable">>,
     <<"The authorization server is currently unable to handle ",
       "the request due to a temporary overloading or maintenance ",
       "of the server.  (This error code is needed because a 503 ",
       "Service Unavailable HTTP status code cannot be returned ",
       "to the client via an HTTP redirect.)">>};

errod_description(Error) ->
    {<<"server_error">>,
     <<"An unknown error occourd: ", (atom_to_binary(Error, utf8))/binary>>}.

decode_grant_type(<<"password">>) ->
    password;
decode_grant_type(<<"client_credentials">>) ->
    client_credentials;
decode_grant_type(<<"authorization_code">>) ->
    authorization_code;
decode_grant_type(<<"refresh_token">>) ->
    refresh_token;
%% TODO: 4.5 Extension grants
decode_grant_type(_) ->
    unknown_grant_type.

decode_response_type(<<"code">>) ->
    code;
decode_response_type(<<"token">>) ->
    token;
decode_response_type(_) ->
    unknown_response_type.

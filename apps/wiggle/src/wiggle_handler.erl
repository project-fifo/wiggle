-module(wiggle_handler).

-export([
         initial_state/2,
         accepted/0
        ]).

-record(state, {path, method, version, token, content, reply}).

initial_state(Req, Component) ->
    {Method, Req1} = cowboy_http_req:method(Req),
    {[<<"api">>, Version, Component | Path], Req2} = cowboy_http_req:path(Req1),
    {ok, Req3} = cowboy_http_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req2),
    {Token, Req4} = case cowboy_http_req:header(<<"X-Snarl-Token">>, Req3) of
                        {undefined, ReqX} ->
                            {TokenX, ReqX1} = cowboy_http_req:cookie(<<"X-Snarl-Token">>, ReqX),
                            {TokenX, ReqX1};
                        {TokenX, ReqX} ->
                            {ok, ReqX1} = cowboy_http_req:set_resp_header(<<"X-Snarl-Token">>, TokenX, ReqX),
                            {TokenX, ReqX1}
                    end,
    {ok, Req5} = cowboy_http_req:set_resp_header(
                   <<"Access-Control-Allow-Headers">>,
                   <<"Content-Type, X-Snarl-Token">>, Req4),
    {ok, Req6} = cowboy_http_req:set_resp_header(
                   <<"Access-Control-Expose-Headers">>,
                   <<"X-Snarl-Token">>, Req5),
    {ok, Req7} = cowboy_http_req:set_resp_header(
                   <<"Allow-Access-Control-Credentials">>,
                   <<"true">>, Req6),
    State =  #state{version = Version,
                    method = Method,
                    token = Token,
                    path = Path},
    io:format("[~p] - ~p~n", [Method, Path]),
    {ok, Req7, State}.

accepted() ->
    [
     {<<"application/json; charset=UTF-8">>, from_json},
     {<<"application/json; charset=utf-8">>, from_json},
     {<<"application/json">>, from_json}
    ].

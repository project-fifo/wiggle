-module(wiggle_handler).

-include("wiggle.hrl").

-export([
         initial_state/1,
         provided/0,
         accepted/0,
         decode/1,
         get_token/1,
         set_access_header/1,
         allowed/2,
         options/3,
         service_available/0
        ]).

initial_state(Req) ->
    {Method, Req0} = cowboy_req:method(Req),
    {Version, Req1} = cowboy_req:binding(version, Req0),
    {Path, Req2} = cowboy_req:path_info(Req1),
    {Token, Req3} = get_token(Req2),
    {PathB, Req4} = cowboy_req:path(Req3),

    io:format("[~p] - ~p~n", [Method, Path]),
    State =  #state{
      version = Version,
      method = Method,
      token = Token,
      path = Path,
      start = now(),
      path_bin = PathB
     },
    {ok, set_access_header(Req4), State}.

set_access_header(Req) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    Req2 = cowboy_req:set_resp_header(
             <<"access-control-allow-headers">>,
             <<"content-type, x-snarl-token">>, Req1),
    Req3 = cowboy_req:set_resp_header(
             <<"access-control-expose-headers">>,
             <<"x-snarl-token">>, Req2),
    cowboy_req:set_resp_header(
      <<"allow-access-control-credentials">>,
      <<"true">>, Req3).

get_token(Req) ->
    case cowboy_req:header(<<"x-snarl-token">>, Req) of
        {undefined, ReqX} ->
            {TokenX, ReqX1} = cowboy_req:cookie(<<"x-snarl-token">>, ReqX),
            {TokenX, ReqX1};
        {TokenX, ReqX} ->
            ReqX1 = cowboy_req:set_resp_header(<<"x-snarl-token">>, TokenX, ReqX),
            {TokenX, ReqX1}
    end.


provided() ->
    [
     {<<"application/json">>, to_json},
     {<<"application/x-msgpack">>, to_msgpack}
    ].

accepted() ->
    [
     {<<"application/x-msgpack; charset=UTF-8">>, from_msgpack},
     {<<"application/x-msgpack; charset=utf-8">>, from_msgpack},
     {<<"application/x-msgpack;charset=utf-8">>, from_msgpack},
     {<<"application/x-msgpack; charset=UTF-8">>, from_msgpack},
     {<<"application/x-msgpack">>, from_msgpack},
     {<<"application/json; charset=UTF-8">>, from_json},
     {<<"application/json; charset=utf-8">>, from_json},
     {<<"application/json;charset=UTF-8">>, from_json},
     {<<"application/json;charset=utf-8">>, from_json},
     {<<"application/json">>, from_json}
    ].

decode(Req) ->
    {ContentType, Req0} =
        cowboy_req:header(<<"content-type">>, Req, {<<"application">>, <<"json">>, []}),
    {ok, Body, Req1} = cowboy_req:body(Req0),
    Decoded = case Body of
                  <<>> ->
                      [];
                  _ ->
                      case lists:keyfind(ContentType, 1, accepted()) of
                          {_, from_json} ->
                              jsxd:from_list(jsx:decode(Body));
                          {_, from_msgpack} ->
                              {ok, D} = msgpack:unpack(Body, [jsx]),
                              jsxd:from_list(D)
                      end
              end,
    {ok, Decoded, Req1}.


options(Req, State, Methods) ->
    Req1 = cowboy_req:set_resp_header(
             <<"access-control-allow-methods">>,
             string:join(
               lists:map(fun erlang:binary_to_list/1,
                         [<<"HEAD">>, <<"OPTIONS">> | Methods]), ", "), Req),
    {ok, Req1, State}.

allowed(State, Perm) ->
    Token = State#state.token,
    Start = now(),
    R = case libsnarl:allowed({token, Token}, Perm) of
            not_found ->
                true;
            true ->
                false;
            false ->
                true
        end,
    ?MSnarl(?P(State), Start),
    R.

service_available() ->
    case {libsniffle:servers(), libsnarl:servers()} of
        {[], _} ->
            false;
        {_, []} ->
            false;
        _ ->
            true
    end.

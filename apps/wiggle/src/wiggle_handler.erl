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
         service_available/0,
         encode/2
        ]).

initial_state(Req) ->
    {Method, Req0} = cowboy_req:method(Req),
    {Version, Req1} = cowboy_req:binding(version, Req0),
    {Path, Req2} = cowboy_req:path_info(Req1),
    {Token, Req3} = get_token(Req2),
    {PathB, Req4} = cowboy_req:path(Req3),
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
     {{<<"application">>, <<"x-msgpack">>, []}, read},
     {{<<"application">>, <<"json">>, []}, read}
    ].

accepted() ->
    [
     {{<<"application">>, <<"x-msgpack">>, '*'}, write},
     {{<<"application">>, <<"json">>, '*'}, write}
    ].
    %%  {<<"application/x-msgpack; charset=UTF-8">>, },
    %%  {<<"application/x-msgpack; charset=utf-8">>, from_msgpack},
    %%  {<<"application/x-msgpack;charset=utf-8">>, from_msgpack},
    %%  {<<"application/x-msgpack; charset=UTF-8">>, from_msgpack},
    %%  {<<"application/x-msgpack">>, from_msgpack},
    %%  {<<"application/json; charset=UTF-8">>, from_json},
    %%  {<<"application/json; charset=utf-8">>, from_json},
    %%  {<<"application/json;charset=UTF-8">>, from_json},
    %%  {<<"application/json;charset=utf-8">>, from_json},
    %%  {<<"application/json">>, from_json}
    %% ].

media_type(Req) ->
        case cowboy_req:meta(media_type, Req) of
            {{<<"application">>, <<"x-msgpack">>, _}, Req1} ->
                {msgpack, Req1};
            {{<<"application">>, <<"json">>, _}, Req1} ->
                {json, Req1}
        end.

decode(Req) ->
    {ContentType, Req0} = media_type(Req),
    {ok, Body, Req1} = cowboy_req:body(Req0),
    Decoded = case Body of
                  <<>> ->
                      [];
                  _ ->
                      case ContentType of
                          json ->
                              jsxd:from_list(jsx:decode(Body));
                          msgpack ->
                              {ok, D} = msgpack:unpack(Body, [jsx]),
                              jsxd:from_list(D)
                      end
              end,
    {ok, Decoded, Req1}.

encode(Body, Req) ->
    {ContentType, Req1} = media_type(Req),
    case ContentType of
        json ->
            {jsx:encode(Body), Req1};
        msgpack ->
            {msgpack:pack(Body), Req1}
    end.

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

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
         encode/2,
         get_persmissions/1,
         clear_permissions/1,
         timeout_cache_with_invalid/6,
         timeout_cache/5,
         list/9
        ]).

clear_permissions(#state{token = Token}) ->
        e2qc:evict(permissions, Token).

initial_state(Req) ->
    {Method, Req0} = cowboy_req:method(Req),
    {Version, Req1} = cowboy_req:binding(version, Req0),
    {Path, Req2} = cowboy_req:path_info(Req1),
    {Token, Req3} = get_token(Req2),
    {PathB, Req4} = cowboy_req:path(Req3),
    {FullList, Req5} = full_list(Req4),
    {FullListFields, Req6} = full_list_fields(Req5),
    State =  #state{
                version = Version,
                method = Method,
                token = Token,
                path = Path,
                start = now(),
                path_bin = PathB,
                full_list = FullList,
                full_list_fields = FullListFields
               },
    {ok, set_access_header(Req6), State}.

set_access_header(Req) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    Req2 = cowboy_req:set_resp_header(
             <<"access-control-allow-headers">>,
             <<"Authorization, content-type, x-snarl-token, x-full-list, x-full-list-fields">>, Req1),
    Req3 = cowboy_req:set_resp_header(
             <<"access-control-expose-headers">>,
             <<"x-snarl-token, x-full-list, x-full-list-fields">>, Req2),
    cowboy_req:set_resp_header(
      <<"access-control-allow-credentials">>,
      <<"true">>, Req3).

get_token(Req) ->
    case cowboy_req:header(<<"x-snarl-token">>, Req) of
        {undefined, ReqX} ->
            case cowboy_req:cookie(<<"x-snarl-token">>, ReqX) of
                {undefined, ReqX1} ->
                    case cowboy_req:header(<<"authorization">>, ReqX1) of
                        {undefined, ReqX2} ->
                            lager:warning("[auth] No authenticaiton req was: ~p.",
                                          [Req]),
                            {undefined, ReqX2};
                        {AuthorizationHeader, ReqX2} ->
                            Res = basic_auth(AuthorizationHeader),
                            {Res, ReqX2}
                    end;
                {TokenX, ReqX1} ->
                    {{token, TokenX}, ReqX1}
                end;
        {TokenX, ReqX} ->
            ReqX1 = cowboy_req:set_resp_header(<<"x-snarl-token">>, TokenX, ReqX),
            {{token, TokenX}, ReqX1}
    end.


basic_auth(AuthorizationHeader) ->
    case binary:split(AuthorizationHeader, <<$ >>) of
        [<<"Basic">>, EncodedCredentials] ->
            decoded_credentials(EncodedCredentials);
        _ ->
            undefined
    end.

decoded_credentials(EncodedCredentials) ->
    DecodedCredentials = base64:decode(EncodedCredentials),
    case binary:split(DecodedCredentials, <<$:>>) of
        [UUID, Password] ->
            case libsnarl:auth(UUID, Password) of
                {ok, UUID} ->
                    UUID;
                _ ->
                    lager:warning("[auth] Basic auth failed."),
                    undefined
            end;
        _ ->
            undefined
    end.


full_list(Req) ->
    case cowboy_req:header(<<"x-full-list">>, Req) of
        {<<"true">>, ReqX} ->
            {true, ReqX};
        {<<"True">>, ReqX} ->
            {true, ReqX};
        {_, ReqX} ->
            {false, ReqX}
    end.

full_list_fields(Req) ->
    case cowboy_req:header(<<"x-full-list-fields">>, Req) of
        {undefined, ReqX} ->
            {[], ReqX};
        {Fields, ReqX} ->
            {re:split(Fields, ","), ReqX}
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

media_type(Req) ->
    case cowboy_req:meta(media_type, Req) of
        {{<<"application">>, <<"x-msgpack">>, _}, Req1} ->
            {msgpack, Req1};
        {{<<"application">>, <<"json">>, _}, Req1} ->
            {json, Req1};
        {_, Req1} ->
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
            {msgpack:pack(Body, [jsx]), Req1}
    end.

options(Req, State, Methods) ->
    Req1 = cowboy_req:set_resp_header(
             <<"access-control-allow-methods">>,
             string:join(
               lists:map(fun erlang:binary_to_list/1,
                         [<<"HEAD">>, <<"OPTIONS">> | Methods]), ", "), Req),
    {ok, Req1, State}.

allowed(State = #state{token = Token}, Perm) ->
    Start = now(),
    R = case get_persmissions(Token) of
            not_found ->
                lager:warning("[auth] unknown Token for allowed: ~p", [Token]),
                true;
            {ok, Ps} ->
                not libsnarl:test(Perm, Ps)
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

%% Cache user permissions for up to 1s.
get_persmissions(Token) ->
    {TTL1, TTL2} = application:get_env(wiggle, token_ttl,
                                       {1000*1000, 10*1000*1000}),
    timeout_cache(permissions, Token, TTL1, TTL2,
                  fun () -> ls_user:cache(Token) end).

timeout_cache(Cache, Value, TTL1, TTL2, Fun) ->
    case application:get_env(wiggle, caching, true) of
        true ->
            timeout_cache_(Cache, Value, TTL1, TTL2, Fun);
        false ->
            Fun()
    end.

timeout_cache_(Cache, Value, TTL1, TTL2, Fun) ->
    CacheFun = fun() -> {now(), Fun()} end,
    {T0, R} = e2qc:cache(Cache, Value, CacheFun),
    case timer:now_diff(now(), T0)/1000 of
        Diff when Diff < TTL1 ->
            R;
        Diff when Diff < TTL2 ->
            e2qc:evict(Cache, Value),
            spawn(e2qc, cache, [Cache, Value, CacheFun]),
            R;
        _ ->
            e2qc:evict(Cache, Value),
            {_, R1} = e2qc:cache(Cache, Value, CacheFun),
            R1
    end.

%% This function lets us define a timedout cache with a invalid value
%% this is helpful since we don't want to cache not_found's.
timeout_cache_with_invalid(Cache, Value, TTL1, TTL2, Invalid, Fun) ->
    case timeout_cache(Cache, Value, TTL1, TTL2, Fun) of
        R when R =:= Invalid ->
            e2qc:evict(Cache, Value),
            R;
        R ->
            R
    end.

list(ListFn, ConvertFn, Token, Permission, FullList, Filter, TTLEntry, FullCache, ListCache) ->
    Fun = list_fn(ListFn, ConvertFn, Permission, FullList, Filter),
    case application:get_env(wiggle, TTLEntry) of
        {ok, {TTL1, TTL2}} ->
            case FullList of
                true ->
                    timeout_cache(FullCache, {Token, Filter}, TTL1, TTL2, Fun);
                _ ->
                    timeout_cache(ListCache, Token, TTL1, TTL2, Fun)
            end;
        _ ->
            Fun()
    end.

list_fn(ListFn, ConvertFn, Permission, FullList, Filter) ->
    fun () ->
            {ok, Res} = ListFn(Permission, FullList),
            case {Filter, FullList} of
                {_, false} ->
                    [ID || {_, ID} <- Res];
                {[], _} ->
                    [ConvertFn(Obj) || {_, Obj} <- Res];
                _ ->
                    [jsxd:select(Filter, ConvertFn(Obj)) || {_, Obj} <- Res]
            end
    end.

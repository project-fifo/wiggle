%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_network_handler).
-include("wiggle.hrl").

-define(CACHE, network).
-define(LIST_CACHE, network_list).
-define(FULL_CACHE, network_full_list).

-export([allowed_methods/3,
         get/1,
         permission_required/1,
         read/2,
         create/3,
         write/3,
         delete/2]).

-ignore_xref([allowed_methods/3,
              get/1,
              permission_required/1,
              read/2,
              create/3,
              write/3,
              delete/2]).

allowed_methods(_Version, _Token, [_Network, <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Network, <<"ipranges">>, _]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Network]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>].

get(State = #state{path = [Network | _]}) ->
    Start = now(),
    R = case application:get_env(wiggle, network_ttl) of
            {ok, {TTL1, TTL2}} ->
                wiggle_handler:timeout_cache_with_invalid(
                  ?CACHE, Network, TTL1, TTL2, not_found,
                  fun() -> libsniffle:network_get(Network) end);
            _ ->
                libsniffle:network_get(Network)
        end,
    ?MSniffle(?P(State), Start),
    R.

permission_required(#state{method = <<"GET">>, path = []}) ->
    {ok, [<<"cloud">>, <<"networks">>, <<"list">>]};

permission_required(#state{method = <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"networks">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [Network]}) ->
    {ok, [<<"networks">>, Network, <<"get">>]};

permission_required(#state{method = <<"DELETE">>, path = [Network]}) ->
    {ok, [<<"networks">>, Network, <<"delete">>]};

permission_required(#state{method = <<"PUT">>, path = [_Network]}) ->
    {ok, [<<"cloud">>, <<"networks">>, <<"create">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [Network, <<"ipranges">>,  _]}) ->
    {ok, [<<"networks">>, Network, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>,
                           path = [Network, <<"ipranges">>, _]}) ->
    {ok, [<<"networks">>, Network, <<"edit">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [Network, <<"metadata">> | _]}) ->
    {ok, [<<"networks">>, Network, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>,
                           path = [Network, <<"metadata">> | _]}) ->
    {ok, [<<"networks">>, Network, <<"edit">>]};

permission_required(_State) ->
    undefined.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

read(Req, State = #state{token = Token, path = [], full_list=FullList, full_list_fields=Filter}) ->
    Start = now(),
    {ok, Permissions} = wiggle_handler:get_persmissions(Token),
    ?MSnarl(?P(State), Start),
    Start1 = now(),
    Permission = [{must, 'allowed',
                   [<<"networks">>, {<<"res">>, <<"uuid">>}, <<"get">>],
                   Permissions}],
    Res = wiggle_handler:list(fun libsniffle:network_list/2,
                              fun ft_network:to_json/1, Token, Permission,
                              FullList, Filter, network_list_ttl, ?FULL_CACHE,
                              ?LIST_CACHE),
    ?MSniffle(?P(State), Start1),
    {Res, Req, State};

read(Req, State = #state{path = [_Network], obj = Obj}) ->
    {ft_network:to_json(Obj), Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = Version}, Data) ->
    {ok, Network} = jsxd:get(<<"name">>, Data),
    Start = now(),
    case libsniffle:network_create(Network) of
        {ok, UUID} ->
            ?MSniffle(?P(State), Start),
            e2qc:teardown(?LIST_CACHE),
            e2qc:teardown(?FULL_CACHE),
            {{true, <<"/api/", Version/binary, "/networks/", UUID/binary>>}, Req, State#state{body = Data}};
        duplicate ->
            ?MSniffle(?P(State), Start),
            {ok, Req1} = cowboy_req:reply(409, Req),
            {halt, Req1, State}
    end.

write(Req, State = #state{
                      path = [Network, <<"ipranges">>, IPrange]}, _Data) ->
    Start = now(),
    case libsniffle:network_add_iprange(Network, IPrange) of
        ok ->
            ?MSniffle(?P(State), Start),
            e2qc:evict(?CACHE, Network),
            e2qc:teardown(?FULL_CACHE),
            {true, Req, State};
        _ ->
            ?MSniffle(?P(State), Start),
            {false, Req, State}
    end;

write(Req, State = #state{method = <<"POST">>, path = []}, _) ->
    {true, Req, State};

write(Req, State = #state{path = [Network, <<"metadata">> | Path]}, [{K, V}]) ->
    Start = now(),
    ok = libsniffle:network_set(Network, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    e2qc:evict(?CACHE, Network),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

write(Req, State, _Body) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [Network, <<"metadata">> | Path]}) ->
    Start = now(),
    ok = libsniffle:network_set(Network, [<<"metadata">> | Path], delete),
    e2qc:evict(?CACHE, Network),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Network, <<"ipranges">>, IPRange]}) ->
    Start = now(),
    ok = libsniffle:network_remove_iprange(Network, IPRange),
    e2qc:evict(?CACHE, Network),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Network]}) ->
    Start = now(),
    ok = libsniffle:network_delete(Network),
    e2qc:evict(?CACHE, Network),
    e2qc:teardown(?LIST_CACHE),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State}.

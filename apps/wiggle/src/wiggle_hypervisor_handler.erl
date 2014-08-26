-module(wiggle_hypervisor_handler).
-include("wiggle.hrl").

-define(CACHE, hypervisor).
-define(LIST_CACHE, hypervisor_list).
-define(FULL_CACHE, hypervisor_full_list).

-export([allowed_methods/3,
         permission_required/1,
         get/1,
         read/2,
         write/3,
         delete/2]).

-ignore_xref([allowed_methods/3,
              get/1,
              permission_required/1,
              read/2,
              write/3,
              delete/2]).

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [?UUID(_Hypervisor)]) ->
    [<<"GET">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [?UUID(_Hypervisor), <<"config">>|_]) ->
    [<<"PUT">>];

allowed_methods(_Version, _Token, [?UUID(_Hypervisor), <<"characteristics">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [?UUID(_Hypervisor), <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [?UUID(_Hypervisor), <<"services">>]) ->
    [<<"PUT">>, <<"GET">>].

get(State = #state{path = [?UUID(Hypervisor) | _]}) ->
    Start = now(),
    R = case application:get_env(wiggle, hypervisor_ttl) of
            {ok, {TTL1, TTL2}} ->
                wiggle_handler:timeout_cache_with_invalid(
                  ?CACHE, Hypervisor, TTL1, TTL2, not_found,
                  fun() -> ls_hypervisor:get(Hypervisor) end);
            _ ->
                ls_hypervisor:get(Hypervisor)
        end,
    ?MSniffle(?P(State), Start),
    R.

permission_required(#state{path = []}) ->
    {ok, [<<"cloud">>, <<"hypervisors">>, <<"list">>]};

permission_required(#state{method = <<"GET">>, path = [?UUID(Hypervisor)]}) ->
    {ok, [<<"hypervisors">>, Hypervisor, <<"get">>]};

permission_required(#state{method = <<"DELETE">>, path = [?UUID(Hypervisor)]}) ->
    {ok, [<<"hypervisors">>, Hypervisor, <<"edit">>]};

permission_required(#state{method = <<"PUT">>, path = [?UUID(Hypervisor), <<"config">> | _]}) ->
    {ok, [<<"hypervisors">>, Hypervisor, <<"edit">>]};

permission_required(#state{method = <<"PUT">>, path = [?UUID(Hypervisor), <<"metadata">> | _]}) ->
    {ok, [<<"hypervisors">>, Hypervisor, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [?UUID(Hypervisor), <<"metadata">> | _]}) ->
    {ok, [<<"hypervisors">>, Hypervisor, <<"edit">>]};

permission_required(#state{method = <<"PUT">>, path = [?UUID(Hypervisor), <<"characteristics">> | _]}) ->
    {ok, [<<"hypervisors">>, Hypervisor, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [?UUID(Hypervisor), <<"characteristics">> | _]}) ->
    {ok, [<<"hypervisors">>, Hypervisor, <<"edit">>]};

permission_required(#state{method = <<"GET">>, path = [?UUID(Hypervisor), <<"services">>]}) ->
    {ok, [<<"hypervisors">>, Hypervisor, <<"get">>]};

permission_required(#state{method = <<"PUT">>, path = [?UUID(Hypervisor), <<"services">>]}) ->
    {ok, [<<"hypervisors">>, Hypervisor, <<"edit">>]};

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
                   [<<"hypervisors">>, {<<"res">>, <<"uuid">>}, <<"get">>],
                   Permissions}],
    Res = wiggle_handler:list(fun ls_hypervisor:list/2,
                              fun ft_hypervisor:to_json/1, Token, Permission,
                              FullList, Filter, hypervisor_list_ttl, ?FULL_CACHE,
                              ?LIST_CACHE),
    ?MSniffle(?P(State), Start1),
    {Res, Req, State};

read(Req, State = #state{path = [?UUID(_Hypervisor), <<"services">>], obj = Obj}) ->
    Services = jsxd:fold(fun(UUID, Snap, Acc) ->
                                 [jsxd:set(<<"uuid">>, UUID, Snap) | Acc]
                         end, [], ft_hypervisor:services(Obj)),
    {Services, Req, State};

read(Req, State = #state{path = [?UUID(_Hypervisor), <<"services">>, Service], obj = Obj}) ->
    {jsxd:get([Service], [{}], ft_hypervisor:services(Obj)), Req, State};

read(Req, State = #state{path = [?UUID(_Hypervisor)], obj = Obj}) ->
    {ft_hypervisor:to_json(Obj), Req, State}.


%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

write(Req, State = #state{path = [?UUID(Hypervisor), <<"config">>]},
      [{<<"alias">>, V}]) when is_binary(V) ->
    Start = now(),
    e2qc:evict(?CACHE, Hypervisor),
    e2qc:teardown(?FULL_CACHE),
    ls_hypervisor:alias(Hypervisor, V),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [?UUID(Hypervisor), <<"config">>]},
      [{<<"path">>, P}]) when is_list(P) ->
    Start = now(),
    e2qc:evict(?CACHE, Hypervisor),
    e2qc:teardown(?FULL_CACHE),
    ls_hypervisor:path(Hypervisor, path_to_erl(P)),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [?UUID(Hypervisor), <<"characteristics">> | Path]}, [{K, V}]) ->
    Start = now(),
    e2qc:evict(?CACHE, Hypervisor),
    e2qc:teardown(?FULL_CACHE),
    ls_hypervisor:set_characteristic(
      Hypervisor, [{Path ++ [K], jsxd:from_list(V)}]),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [?UUID(Hypervisor), <<"metadata">> | Path]}, [{K, V}]) ->
    Start = now(),
    e2qc:evict(?CACHE, Hypervisor),
    e2qc:teardown(?FULL_CACHE),
    ls_hypervisor:set_metadata(
      Hypervisor, [{Path ++ [K], jsxd:from_list(V)}]),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [?UUID(Hypervisor), <<"services">>]},
      [{<<"action">>, <<"enable">>},
       {<<"service">>, Service}]) ->
    e2qc:evict(?CACHE, Hypervisor),
    e2qc:teardown(?FULL_CACHE),
    ls_hypervisor:service_action(Hypervisor, enable, Service),
    {true, Req, State};

write(Req, State = #state{path = [?UUID(Hypervisor), <<"services">>]},
      [{<<"action">>, <<"disable">>},
       {<<"service">>, Service}]) ->
    e2qc:evict(?CACHE, Hypervisor),
    e2qc:teardown(?FULL_CACHE),
    ls_hypervisor:service_action(Hypervisor, disable, Service),
    {true, Req, State};

write(Req, State = #state{path = [?UUID(Hypervisor), <<"services">>]},
      [{<<"action">>, <<"clear">>},
       {<<"service">>, Service}]) ->
    e2qc:evict(?CACHE, Hypervisor),
    e2qc:teardown(?FULL_CACHE),
    ls_hypervisor:service_action(Hypervisor, clear, Service),
    {true, Req, State};

write(Req, State, _Body) ->
    {false, Req, State}.


%%--------------------------------------------------------------------
%% DELETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [?UUID(Hypervisor)]}) ->
    Start = now(),
    e2qc:evict(?CACHE, Hypervisor),
    e2qc:teardown(?FULL_CACHE),
    e2qc:teardown(?LIST_CACHE),
    ls_hypervisor:unregister(Hypervisor),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [?UUID(Hypervisor), <<"characteristics">> | Path]}) ->
    Start = now(),
    e2qc:evict(?CACHE, Hypervisor),
    e2qc:teardown(?FULL_CACHE),
    ls_hypervisor:set_characteristic(Hypervisor, [{Path, delete}]),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [?UUID(Hypervisor), <<"metadata">> | Path]}) ->
    Start = now(),
    e2qc:evict(?CACHE, Hypervisor),
    e2qc:teardown(?FULL_CACHE),
    ls_hypervisor:set_metadata(Hypervisor, [{Path, delete}]),
    ?MSniffle(?P(State), Start),
    {true, Req, State}.

%%--------------------------------------------------------------------
%% DELETE
%%--------------------------------------------------------------------
path_to_erl(P) ->
    [{N, C} || [{<<"cost">>, C}, {<<"name">>, N}] <- P, is_integer(C), is_binary(N), N /= <<>>].

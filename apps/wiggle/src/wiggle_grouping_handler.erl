%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_grouping_handler).
-include("wiggle.hrl").

-define(CACHE, grouping).
-define(LIST_CACHE, grouping_list).
-define(FULL_CACHE, grouping_full_list).

-export([allowed_methods/3,
         get/1,
         permission_required/1,
         read/2,
         create/3,
         write/3,
         delete/2]).

-behaviour(wiggle_rest_handler).

allowed_methods(_Version, _Token, [?UUID(_Grouping), <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [?UUID(_Grouping), <<"elements">>, _]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [?UUID(_Grouping), <<"groupings">>, _]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [?UUID(_Grouping)]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>].

get(State = #state{path = [?UUID(Grouping) | _]}) ->
    Start = now(),
    R = case application:get_env(wiggle, grouping_ttl) of
            {ok, {TTL1, TTL2}} ->
                wiggle_handler:timeout_cache_with_invalid(
                  ?CACHE, Grouping, TTL1, TTL2, not_found,
                  fun() -> ls_grouping:get(Grouping) end);
            _ ->
                ls_grouping:get(Grouping)
        end,
    ?MSniffle(?P(State), Start),
    R;

get(_State) ->
    not_found.

permission_required(#state{method = <<"GET">>, path = []}) ->
    {ok, [<<"cloud">>, <<"groupings">>, <<"list">>]};

permission_required(#state{method = <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"groupings">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [?UUID(Grouping)]}) ->
    {ok, [<<"groupings">>, Grouping, <<"get">>]};

permission_required(#state{method = <<"DELETE">>, path = [?UUID(Grouping)]}) ->
    {ok, [<<"groupings">>, Grouping, <<"delete">>]};

permission_required(#state{method = <<"PUT">>, path = [?UUID(_Grouping)]}) ->
    {ok, [<<"cloud">>, <<"groupings">>, <<"create">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [?UUID(Grouping), <<"elements">>,  _]}) ->
    {ok, [<<"groupings">>, Grouping, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>,
                           path = [?UUID(Grouping), <<"elements">>, _]}) ->
    {ok, [<<"groupings">>, Grouping, <<"edit">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [?UUID(Grouping), <<"groupings">>,  _]}) ->
    {ok, [<<"groupings">>, Grouping, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>,
                           path = [?UUID(Grouping), <<"groupings">>, _]}) ->
    {ok, [<<"groupings">>, Grouping, <<"edit">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [?UUID(Grouping), <<"metadata">> | _]}) ->
    {ok, [<<"groupings">>, Grouping, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>,
                           path = [?UUID(Grouping), <<"metadata">> | _]}) ->
    {ok, [<<"groupings">>, Grouping, <<"edit">>]};

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
                   [<<"groupings">>, {<<"res">>, <<"uuid">>}, <<"get">>],
                   Permissions}],
    Res = wiggle_handler:list(fun ls_grouping:list/2,
                              fun ft_grouping:to_json/1, Token, Permission,
                              FullList, Filter, grouping_list_ttl, ?FULL_CACHE,
                              ?LIST_CACHE),
    ?MSniffle(?P(State), Start1),
    {Res, Req, State};

read(Req, State = #state{path = [?UUID(_Grouping)], obj = Obj}) ->
    {ft_grouping:to_json(Obj), Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = Version, token=Token},
       [{<<"name">>, Name}, {<<"type">>, TypeS}] = Data) ->
    Type = case TypeS of
               <<"cluster">> ->
                   cluster;
               <<"stack">> ->
                   stack;
               _ ->
                   none
           end,
    Start = now(),
    case ls_grouping:add(Name, Type) of
        {ok, UUID} ->
            e2qc:teardown(?LIST_CACHE),
            e2qc:teardown(?FULL_CACHE),
            {ok, User} = ls_user:get(Token),
            case ft_user:active_org(User) of
                <<Org:36/binary>> ->
                    ls_org:execute_trigger(Org, grouping_create, UUID);
                _ ->
                    ok
            end,
            ?MSniffle(?P(State), Start),
            {{true, <<"/api/", Version/binary, "/groupings/", UUID/binary>>}, Req, State#state{body = Data}};
        duplicate ->
            ?MSniffle(?P(State), Start),
            {ok, Req1} = cowboy_req:reply(409, Req),
            {halt, Req1, State}
    end.

write(Req, State = #state{
                      path = [?UUID(Grouping), <<"elements">>, IPrange]}, _Data) ->
    Start = now(),
    case ls_grouping:add_element(Grouping, IPrange) of
        ok ->
            e2qc:evict(?CACHE, Grouping),
            e2qc:teardown(?FULL_CACHE),
            ?MSniffle(?P(State), Start),
            {true, Req, State};
        _ ->
            ?MSniffle(?P(State), Start),
            {false, Req, State}
    end;

write(Req, State = #state{
                      path = [?UUID(Grouping), <<"groupings">>, IPrange]}, _Data) ->
    Start = now(),
    case ls_grouping:add_grouping(Grouping, IPrange) of
        ok ->
            e2qc:evict(?CACHE, Grouping),
            e2qc:teardown(?FULL_CACHE),
            ?MSniffle(?P(State), Start),
            {true, Req, State};
        _ ->
            ?MSniffle(?P(State), Start),
            {false, Req, State}
    end;

write(Req, State = #state{method = <<"POST">>, path = []}, _) ->
    {true, Req, State};

write(Req, State = #state{path = [?UUID(Grouping), <<"metadata">> | Path]}, [{K, V}]) ->
    Start = now(),
    ok = ls_grouping:set_metadata(Grouping, [{Path ++ [K], jsxd:from_list(V)}]),
    e2qc:evict(?CACHE, Grouping),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

write(Req, State, _Body) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [?UUID(Grouping), <<"metadata">> | Path]}) ->
    Start = now(),
    ok = ls_grouping:set_metadata(Grouping, [{Path, delete}]),
    e2qc:evict(?CACHE, Grouping),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [?UUID(Grouping), <<"elements">>, Element]}) ->
    Start = now(),
    ok = ls_grouping:remove_element(Grouping, Element),
    e2qc:evict(?CACHE, Grouping),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [?UUID(Grouping), <<"groupings">>, Element]}) ->
    Start = now(),
    ok = ls_grouping:remove_grouping(Grouping, Element),
    e2qc:evict(?CACHE, Grouping),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [?UUID(Grouping)]}) ->
    Start = now(),
    ok = ls_grouping:delete(Grouping),
    e2qc:evict(?CACHE, Grouping),
    e2qc:teardown(?LIST_CACHE),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State}.

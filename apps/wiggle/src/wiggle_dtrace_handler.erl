-module(wiggle_dtrace_handler).
-include("wiggle.hrl").

-define(CACHE, dtrace).
-define(LIST_CACHE, dtrace_list).
-define(FULL_CACHE, dtrace_full_list).

-export([allowed_methods/3,
         get/1,
         permission_required/1,
         read/2,
         create/3,
         write/3,
         delete/2]).

-behaviour(wiggle_rest_handler).

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [?UUID(_Dtrace), <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [?UUID(_Dtrace)]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>].

get(State = #state{path = [?UUID(Dtrace) | _]}) ->
    Start = now(),
    R = case application:get_env(wiggle, dtrace_ttl) of
            {ok, {TTL1, TTL2}} ->
                wiggle_handler:timeout_cache_with_invalid(
                  ?CACHE, Dtrace, TTL1, TTL2, not_found,
                  fun() -> ls_dtrace:get(Dtrace) end);
            _ ->
                ls_dtrace:get(Dtrace)
        end,
    ?MSniffle(?P(State), Start),
    R;

get(_State) ->
    not_found.

permission_required(#state{method= <<"GET">>, path = []}) ->
    {ok, [<<"cloud">>, <<"dtraces">>, <<"list">>]};

permission_required(#state{method= <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"dtraces">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [?UUID(Dtrace)]}) ->
    {ok, [<<"dtraces">>, Dtrace, <<"get">>]};

permission_required(#state{method = <<"DELETE">>, path = [?UUID(Dtrace)]}) ->
    {ok, [<<"dtraces">>, Dtrace, <<"delete">>]};

permission_required(#state{method = <<"PUT">>, path = [?UUID(Dtrace)]}) ->
    {ok, [<<"dtraces">>, Dtrace, <<"edit">>]};

permission_required(#state{method = <<"PUT">>, path = [?UUID(Dtrace), <<"metadata">> | _]}) ->
    {ok, [<<"dtraces">>, Dtrace, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [?UUID(Dtrace), <<"metadata">> | _]}) ->
    {ok, [<<"dtraces">>, Dtrace, <<"edit">>]};

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
                   [<<"dtraces">>, {<<"res">>, <<"uuid">>}, <<"get">>],
                   Permissions}],
    Res = wiggle_handler:list(fun ls_dtrace:list/2,
                              fun ft_dtrace:to_json/1, Token, Permission,
                              FullList, Filter, dtrace_list_ttl, ?FULL_CACHE,
                              ?LIST_CACHE),
    ?MSniffle(?P(State), Start1),
    {Res, Req, State};

read(Req, State = #state{path = [?UUID(_Dtrace)], obj = Obj}) ->
    Obj1 = ft_dtrace:to_json(Obj),
    Obj2 = jsxd:update(<<"script">>, fun (S) ->
                                             list_to_binary(S)
                                     end, Obj1),
    {Obj2, Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = Version}, Data) ->
    {ok, Dtrace} = jsxd:get(<<"name">>, Data),
    {ok, Script} = jsxd:get(<<"script">>, Data),
    Script1 = binary_to_list(Script),
    Start = now(),
    case ls_dtrace:add(Dtrace, Script1) of
        {ok, UUID} ->
            e2qc:teardown(?LIST_CACHE),
            e2qc:teardown(?FULL_CACHE),
            ?MSniffle(?P(State), Start),
            case jsxd:get(<<"config">>, Data) of
                {ok, Config} ->
                    Start1 = now(),
                    ok = ls_dtrace:set_config(UUID, Config),
                    ?MSniffle(?P(State), Start1);
                _ ->
                    ok
            end,
            {{true, <<"/api/", Version/binary, "/dtrace/", UUID/binary>>}, Req, State#state{body = Data}};
        duplicate ->
            {ok, Req1} = cowboy_req:reply(409, Req),
            {halt, Req1, State}
    end.

%% TODO : This is a icky case it is called after post.
write(Req, State = #state{method = <<"POST">>, path = []}, _) ->
    {true, Req, State};

write(Req, State = #state{path = [?UUID(Dtrace), <<"metadata">> | Path]}, [{K, V}]) ->
    Start = now(),
    ok = ls_dtrace:set_metadata(Dtrace, [{Path ++ [K], jsxd:from_list(V)}]),
    e2qc:evict(?CACHE, Dtrace),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

write(Req, State, _Body) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [?UUID(Dtrace), <<"metadata">> | Path]}) ->
    Start = now(),
    ok = ls_dtrace:set_metadata(Dtrace, [{Path, delete}]),
    e2qc:evict(?CACHE, Dtrace),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [?UUID(Dtrace)]}) ->
    Start = now(),
    ok = ls_dtrace:delete(Dtrace),
    e2qc:evict(?CACHE, Dtrace),
    e2qc:teardown(?LIST_CACHE),
    e2qc:teardown(?FULL_CACHE),
    ?MSniffle(?P(State), Start),
    {true, Req, State}.

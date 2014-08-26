%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_iprange_handler).
-include("wiggle.hrl").

-define(CACHE, iprange).
-define(LIST_CACHE, iprange_list).
-define(FULL_CACHE, iprange_full_list).

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

allowed_methods(_Version, _Token, [?UUID(_Iprange), <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [?UUID(_Iprange)]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>].

get(State = #state{path = [?UUID(Iprange) | _]}) ->
    Start = now(),
    R = case application:get_env(wiggle, iprange_ttl) of
            {ok, {TTL1, TTL2}} ->
                wiggle_handler:timeout_cache_with_invalid(
                  ?CACHE, Iprange, TTL1, TTL2, not_found,
                  fun() -> ls_iprange:get(Iprange) end);
            _ ->
                ls_iprange:get(Iprange)
        end,
    ?MSniffle(?P(State), Start),
    R;

get(_State) ->
    not_found.

permission_required(#state{method = <<"GET">>, path = []}) ->
    {ok, [<<"cloud">>, <<"ipranges">>, <<"list">>]};

permission_required(#state{method = <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"ipranges">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [?UUID(Iprange)]}) ->
    {ok, [<<"ipranges">>, Iprange, <<"get">>]};

permission_required(#state{method = <<"DELETE">>, path = [?UUID(Iprange)]}) ->
    {ok, [<<"ipranges">>, Iprange, <<"delete">>]};

permission_required(#state{method = <<"PUT">>, path = [?UUID(_Iprange)]}) ->
    {ok, [<<"cloud">>, <<"ipranges">>, <<"create">>]};

permission_required(#state{method = <<"PUT">>, path = [?UUID(Iprange), <<"metadata">> | _]}) ->
    {ok, [<<"ipranges">>, Iprange, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [?UUID(Iprange), <<"metadata">> | _]}) ->
    {ok, [<<"ipranges">>, Iprange, <<"edit">>]};

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
                   [<<"ipranges">>, {<<"res">>, <<"uuid">>}, <<"get">>],
                   Permissions}],
    %% We can't use the wiggle_handler:list_fn/4 since we need to
    %% apply a transformation to the objects when full list is given.
    Fun = fun() ->
                  {ok, Res} = ls_iprange:list(Permission, FullList),
                  case {Filter, FullList} of
                      {_, false} ->
                          [ID || {_, ID} <- Res];
                      {[], _} ->
                          [to_json(Obj) || {_, Obj} <- Res];
                      _ ->
                          [jsxd:select(Filter, to_json(Obj)) || {_, Obj} <- Res]
                  end
          end,
    Res1 = case application:get_env(wiggle, iprange_list_ttl) of
               {ok, {TTL1, TTL2}} ->
                   case FullList of
                       true ->
                           wiggle_handler:timeout_cache(
                             ?FULL_CACHE, {Token, Filter}, TTL1, TTL2, Fun);
                       _ ->
                           wiggle_handler:timeout_cache(
                             ?LIST_CACHE, Token, TTL1, TTL2, Fun)
                   end;
               _ ->
                   Fun()
           end,
    ?MSnarl(?P(State), Start1),
    {Res1, Req, State};

read(Req, State = #state{path = [?UUID(_Iprange)], obj = Obj}) ->
    {to_json(Obj), Req, State}.

to_json(Obj) ->
    jsxd:thread([{update, <<"network">>, fun ip_to_str/1},
                 {update, <<"gateway">>, fun ip_to_str/1},
                 {update, <<"netmask">>, fun ip_to_str/1},
                 {update, <<"free">>,
                  fun (Free) ->
                          lists:map(fun ip_to_str/1, Free)
                  end},
                 {update, <<"used">>,
                  fun (Free) ->
                          lists:map(fun ip_to_str/1, Free)
                  end}], ft_iprange:to_json(Obj)).
%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = Version}, Data) ->
    {ok, Iprange} = jsxd:get(<<"name">>, Data),
    {ok, Network} = jsxd:get(<<"network">>, Data),
    {ok, Gateway} = jsxd:get(<<"gateway">>, Data),
    {ok, Netmask} = jsxd:get(<<"netmask">>, Data),
    {ok, First} = jsxd:get(<<"first">>, Data),
    {ok, Last} = jsxd:get(<<"last">>, Data),
    {ok, Tag} = jsxd:get(<<"tag">>, Data),
    Vlan = jsxd:get(<<"vlan">>, 0, Data),
    Start = now(),
    case ls_iprange:create(Iprange, Network, Gateway, Netmask, First, Last, Tag, Vlan) of
        {ok, UUID} ->
            ?MSniffle(?P(State), Start),
            e2qc:teardown(?LIST_CACHE),
            e2qc:teardown(?FULL_CACHE),
            {{true, <<"/api/", Version/binary, "/ipranges/", UUID/binary>>}, Req, State#state{body = Data}};
        duplicate ->
            ?MSniffle(?P(State), Start),
            {ok, Req1} = cowboy_req:reply(409, Req),
            {halt, Req1, State}
    end.

%% TODO : This is a icky case it is called after post.
write(Req, State = #state{method = <<"POST">>, path = []}, _) ->
    {true, Req, State};

write(Req, State = #state{path = [?UUID(Iprange), <<"metadata">> | Path]}, [{K, V}]) ->
    Start = now(),
    e2qc:evict(?CACHE, Iprange),
    e2qc:teardown(?FULL_CACHE),
    ls_iprange:set_metadata(Iprange, [{Path ++ [K], jsxd:from_list(V)}]),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

write(Req, State, _Body) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [?UUID(Iprange), <<"metadata">> | Path]}) ->
    Start = now(),
    e2qc:evict(?CACHE, Iprange),
    e2qc:teardown(?FULL_CACHE),
    ls_iprange:set_metadata(Iprange, [{Path, delete}]),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [?UUID(Iprange)]}) ->
    Start = now(),
    e2qc:evict(?CACHE, Iprange),
    e2qc:teardown(?LIST_CACHE),
    e2qc:teardown(?FULL_CACHE),
    ok = ls_iprange:delete(Iprange),
    ?MSniffle(?P(State), Start),
    {true, Req, State}.

ip_to_str(Ip) ->
    <<A:8, B:8, C:8, D:8>> = <<Ip:32>>,
    list_to_binary(io_lib:format("~p.~p.~p.~p", [A, B, C, D])).

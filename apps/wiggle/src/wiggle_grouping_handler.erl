%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_grouping_handler).
-include("wiggle.hrl").

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

allowed_methods(_Version, _Token, [_Grouping, <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Grouping, <<"elements">>, _]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Grouping, <<"groupings">>, _]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Grouping]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>].

get(State = #state{path = [Grouping | _]}) ->
    Start = now(),
    R = libsniffle:grouping_get(Grouping),
    ?MSniffle(?P(State), Start),
    R.

permission_required(#state{method = <<"GET">>, path = []}) ->
    {ok, [<<"cloud">>, <<"groupings">>, <<"list">>]};

permission_required(#state{method = <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"groupings">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [Grouping]}) ->
    {ok, [<<"groupings">>, Grouping, <<"get">>]};

permission_required(#state{method = <<"DELETE">>, path = [Grouping]}) ->
    {ok, [<<"groupings">>, Grouping, <<"delete">>]};

permission_required(#state{method = <<"PUT">>, path = [_Grouping]}) ->
    {ok, [<<"cloud">>, <<"groupings">>, <<"create">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [Grouping, <<"elements">>,  _]}) ->
    {ok, [<<"groupings">>, Grouping, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>,
                           path = [Grouping, <<"elements">>, _]}) ->
    {ok, [<<"groupings">>, Grouping, <<"edit">>]};

permission_required(#state{method = <<"PUT">>,
                           path = [Grouping, <<"metadata">> | _]}) ->
    {ok, [<<"groupings">>, Grouping, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>,
                           path = [Grouping, <<"metadata">> | _]}) ->
    {ok, [<<"groupings">>, Grouping, <<"edit">>]};

permission_required(_State) ->
    undefined.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

read(Req, State = #state{token = Token, path = [], full_list=FullList, full_list_fields=Filter}) ->
    Start = now(),
    {ok, Permissions} = libsnarl:user_cache(Token),
    ?MSnarl(?P(State), Start),
    Start1 = now(),
    {ok, Res} =
        libsniffle:grouping_list(
          [{must, 'allowed', [<<"groupings">>, {<<"res">>, <<"uuid">>}, <<"get">>], Permissions}], FullList),
    ?MSniffle(?P(State), Start1),
    Res1 = case {Filter, FullList} of
               {_, false} ->
                   [ID || {_, ID} <- Res];
               {[], _} ->
                   [ID || {_, ID} <- Res];
               _ ->
                   [jsxd:select(Filter, ID) || {_, ID} <- Res]
           end,
    {Res1, Req, State};

read(Req, State = #state{path = [_Grouping], obj = Obj}) ->
    {Obj, Req, State}.

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
    case libsniffle:grouping_add(Name, Type) of
        {ok, UUID} ->
            case libsnarl:user_active_org(Token) of
                {ok, Org} ->
                    libsnarl:org_execute_trigger(Org, grouping_create, UUID);
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
                      path = [Grouping, <<"elements">>, IPrange]}, _Data) ->
    Start = now(),
    case libsniffle:grouping_add_element(Grouping, IPrange) of
        ok ->
            ?MSniffle(?P(State), Start),
            {true, Req, State};
        _ ->
            ?MSniffle(?P(State), Start),
            {false, Req, State}
    end;

write(Req, State = #state{
                      path = [Grouping, <<"groupings">>, IPrange]}, _Data) ->
    Start = now(),
    case libsniffle:grouping_add_grouping(Grouping, IPrange) of
        ok ->
            ?MSniffle(?P(State), Start),
            {true, Req, State};
        _ ->
            ?MSniffle(?P(State), Start),
            {false, Req, State}
    end;

write(Req, State = #state{method = <<"POST">>, path = []}, _) ->
    {true, Req, State};

write(Req, State = #state{path = [Grouping, <<"metadata">> | Path]}, [{K, V}]) ->
    Start = now(),
    libsniffle:grouping_metadata_set(Grouping, Path ++ [K], jsxd:from_list(V)),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

write(Req, State, _Body) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [Grouping, <<"metadata">> | Path]}) ->
    Start = now(),
    libsniffle:grouping_metadata_set(Grouping, Path, delete),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Grouping, <<"elements">>, Element]}) ->
    Start = now(),
    libsniffle:grouping_remove_element(Grouping, Element),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Grouping, <<"groupings">>, Element]}) ->
    Start = now(),
    libsniffle:grouping_remove_element(Grouping, Element),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Grouping]}) ->
    Start = now(),
    ok = libsniffle:grouping_delete(Grouping),
    ?MSniffle(?P(State), Start),
    {true, Req, State}.

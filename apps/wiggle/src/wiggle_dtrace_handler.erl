-module(wiggle_dtrace_handler).
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

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Dtrace, <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Dtrace]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>].

get(State = #state{path = [Dtrace | _]}) ->
    Start = now(),
    R = libsniffle:dtrace_get(Dtrace),
    ?MSniffle(?P(State), Start),
    R.

permission_required(#state{method= <<"GET">>, path = []}) ->
    {ok, [<<"cloud">>, <<"dtraces">>, <<"list">>]};

permission_required(#state{method= <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"dtraces">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [Dtrace]}) ->
    {ok, [<<"dtraces">>, Dtrace, <<"get">>]};

permission_required(#state{method = <<"DELETE">>, path = [Dtrace]}) ->
    {ok, [<<"dtraces">>, Dtrace, <<"delete">>]};

permission_required(#state{method = <<"PUT">>, path = [Dtrace]}) ->
    {ok, [<<"dtraces">>, Dtrace, <<"edit">>]};

permission_required(#state{method = <<"PUT">>, path = [Dtrace, <<"metadata">> | _]}) ->
    {ok, [<<"dtraces">>, Dtrace, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [Dtrace, <<"metadata">> | _]}) ->
    {ok, [<<"dtraces">>, Dtrace, <<"edit">>]};

permission_required(_State) ->
    undefined.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

read(Req, State = #state{token = Token, path = [], full_list=FullList, full_list_fields=Filter}) ->
    Start = now(),
    {ok, Permissions} = libsnarl:user_cache({token, Token}),
    ?MSnarl(?P(State), Start),
    Start1 = now(),
    {ok, Res} = libsniffle:dtrace_list([{must, 'allowed', [<<"dtraces">>, {<<"res">>, <<"uuid">>}, <<"get">>], Permissions}], FullList),
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

read(Req, State = #state{path = [_Dtrace], obj = Obj}) ->
    Obj1 = jsxd:update(<<"script">>, fun (S) ->
                                             list_to_binary(S)
                                     end, Obj),
    {Obj1, Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = Version}, Data) ->
    {ok, Dtrace} = jsxd:get(<<"name">>, Data),
    {ok, Script} = jsxd:get(<<"script">>, Data),
    Script1 = binary_to_list(Script),
    Start = now(),
    case libsniffle:dtrace_add(Dtrace, Script1) of
        {ok, UUID} ->
            ?MSniffle(?P(State), Start),
            case jsxd:get(<<"config">>, Data) of
                {ok, Config} ->
                    Start1 = now(),
                    ok = libsniffle:dtrace_set(UUID, <<"config">>, Config),
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

write(Req, State = #state{path = [Dtrace, <<"metadata">> | Path]}, [{K, V}]) ->
    Start = now(),
    libsniffle:dtrace_set(Dtrace, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

write(Req, State, _Body) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [Dtrace, <<"metadata">> | Path]}) ->
    Start = now(),
    libsniffle:dtrace_set(Dtrace, [<<"metadata">> | Path], delete),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Dtrace]}) ->
    Start = now(),
    ok = libsniffle:dtrace_delete(Dtrace),
    ?MSniffle(?P(State), Start),
    {true, Req, State}.

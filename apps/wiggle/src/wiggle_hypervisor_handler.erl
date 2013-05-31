-module(wiggle_hypervisor_handler).
-include("wiggle.hrl").

-export([allowed_methods/3,
         get/1,
         permission_required/1,
         handle_request/2,
         create_path/3,
         handle_write/3,
         delete_resource/2]).

-ignore_xref([allowed_methods/3,
              get/1,
              permission_required/1,
              handle_request/2,
              create_path/3,
              handle_write/3,
              delete_resource/2]).

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Hypervisor]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Hypervisor, <<"characteristics">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Hypervisor, <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>].


get(State = #state{path = [Hypervisor | _]}) ->
    Start = now(),
    R = libsniffle:hypervisor_get(Hypervisor),
    ?MSniffle(?P(State), Start),
    R.

permission_required(#state{path = []}) ->
    {ok, [<<"cloud">>, <<"hypervisors">>, <<"list">>]};

permission_required(#state{method = <<"GET">>, path = [Hypervisor]}) ->
    {ok, [<<"hypervisors">>, Hypervisor, <<"get">>]};

permission_required(#state{method = <<"PUT">>, path = [Hypervisor, <<"metadata">> | _]}) ->
    {ok, [<<"hypervisors">>, Hypervisor, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [Hypervisor, <<"metadata">> | _]}) ->
    {ok, [<<"hypervisors">>, Hypervisor, <<"edit">>]};

permission_required(#state{method = <<"PUT">>, path = [Hypervisor, <<"characteristics">> | _]}) ->
    {ok, [<<"hypervisors">>, Hypervisor, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [Hypervisor, <<"characteristics">> | _]}) ->
    {ok, [<<"hypervisors">>, Hypervisor, <<"edit">>]};

permission_required(_State) ->
    undefined.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

handle_request(Req, State = #state{token = Token, path = []}) ->
    Start = now(),
    {ok, Permissions} = libsnarl:user_cache({token, Token}),
    ?MSnarl(?P(State), Start),
    Start1 = now(),
    {ok, Res} = libsniffle:hypervisor_list([{must, 'allowed', [<<"hypervisors">>, {<<"res">>, <<"name">>}, <<"get">>], Permissions}]),
    ?MSniffle(?P(State), Start1),
    {lists:map(fun ({E, _}) -> E end,  Res), Req, State};

handle_request(Req, State = #state{path = [_Hypervisor], obj = Obj}) ->
    {Obj, Req, State}.

create_path(Req, State = #state{path = [], version = _Version, token = _Token}, _Decoded) ->
    {ok, Req1} = cowboy_req:reply(500, Req),
    {halt, Req1, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

handle_write(Req, State = #state{path = [Hypervisor, <<"characteristics">> | Path]}, [{K, V}]) ->
    Start = now(),
    libsniffle:hypervisor_set(Hypervisor, [<<"characteristics">> | Path] ++ [K], jsxd:from_list(V)),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

handle_write(Req, State = #state{path = [Hypervisor, <<"metadata">> | Path]}, [{K, V}]) ->
    Start = now(),
    libsniffle:hypervisor_set(Hypervisor, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

handle_write(Req, State, _Body) ->
    {false, Req, State}.


%%--------------------------------------------------------------------
%% DELETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [Hypervisor, <<"characteristics">> | Path]}) ->
    Start = now(),
    libsniffle:hypervisor_set(Hypervisor, [<<"characteristics">> | Path], delete),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete_resource(Req, State = #state{path = [Hypervisor, <<"metadata">> | Path]}) ->
    Start = now(),
    libsniffle:hypervisor_set(Hypervisor, [<<"metadata">> | Path], delete),
    ?MSniffle(?P(State), Start),
    {true, Req, State}.

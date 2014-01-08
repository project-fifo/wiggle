%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_package_handler).
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

allowed_methods(_Version, _Token, [_Package, <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Package]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>].

get(State = #state{path = [Package | _]}) ->
    Start = now(),
    R = libsniffle:package_get(Package),
    ?MSniffle(?P(State), Start),
    R;

get(_) ->
    not_found.

permission_required(#state{method= <<"GET">>, path = []}) ->
    {ok, [<<"cloud">>, <<"packages">>, <<"list">>]};

permission_required(#state{method= <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"packages">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [Package]}) ->
    {ok, [<<"packages">>, Package, <<"get">>]};

permission_required(#state{method = <<"DELETE">>, path = [Package]}) ->
    {ok, [<<"packages">>, Package, <<"delete">>]};

permission_required(#state{method = <<"PUT">>, path = [_Package]}) ->
    {ok, [<<"cloud">>, <<"packages">>, <<"create">>]};

permission_required(#state{method = <<"PUT">>, path = [Package, <<"metadata">> | _]}) ->
    {ok, [<<"packages">>, Package, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [Package, <<"metadata">> | _]}) ->
    {ok, [<<"packages">>, Package, <<"edit">>]};

permission_required(_State) ->
    undefined.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

read(Req, State = #state{token = Token, path = [], full_list=FullList}) ->
    {ok, Permissions} = libsnarl:user_cache({token, Token}),
    {ok, Res} = libsniffle:package_list([{must, 'allowed', [<<"packages">>, {<<"res">>, <<"uuid">>}, <<"get">>], Permissions}], FullList),
    {[ID || {_, ID} <- Res], Req, State};

read(Req, State = #state{path = [_Package], obj = Obj}) ->
    {Obj, Req, State}.


%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = Version}, Data) ->
    Data1 = jsxd:select([<<"cpu_cap">>, <<"quota">>, <<"ram">>,
                         <<"requirements">>, <<"zfs_iox_priority">>,
                         <<"max_swap">>], Data),
    {ok, Package} = jsxd:get(<<"name">>, Data),
    case libsniffle:package_create(Package) of
        {ok, UUID} ->
            ok = libsniffle:package_set(UUID, Data1),
            {{true, <<"/api/", Version/binary, "/packages/", UUID/binary>>}, Req, State#state{body = Data1}};
        duplicate ->
            {ok, Req1} = cowboy_req:reply(409, Req),
            {halt, Req1, State}
    end.

%% TODO : This is a icky case it is called after post.

write(Req, State = #state{method = <<"POST">>, path = []}, _) ->
    {true, Req, State};

write(Req, State = #state{path = [Package, <<"metadata">> | Path]}, [{K, V}]) ->
    libsniffle:package_set(Package, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    {true, Req, State};

write(Req, State, _Body) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [Package, <<"metadata">> | Path]}) ->
    libsniffle:package_set(Package, [<<"metadata">> | Path], delete),
    {true, Req, State};

delete(Req, State = #state{path = [Package]}) ->
    ok = libsniffle:package_delete(Package),
    {true, Req, State}.

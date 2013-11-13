%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_dataset_handler).

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

allowed_methods(_Version, _Token, [_Dataset]) ->
    [<<"GET">>, <<"DELETE">>, <<"PUT">>];

allowed_methods(_Version, _Token, [_Dataset, <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>].

get(State = #state{path = [Dataset | _]}) ->
    Start = now(),
    R = libsniffle:dataset_get(Dataset),
    ?MSniffle(?P(State), Start),
    R.

permission_required(#state{method = <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"datasets">>, <<"create">>]};

permission_required(#state{path = []}) ->
    {ok, [<<"cloud">>, <<"datasets">>, <<"list">>]};

permission_required(#state{method = <<"GET">>, path = [Dataset]}) ->
    {ok, [<<"datasets">>, Dataset, <<"get">>]};

permission_required(#state{method = <<"PUT">>, path = [Dataset]}) ->
    {ok, [<<"datasets">>, Dataset, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [Dataset]}) ->
    {ok, [<<"datasets">>, Dataset, <<"delete">>]};

permission_required(#state{method = <<"PUT">>, path = [Dataset, <<"metadata">> | _]}) ->
    {ok, [<<"datasets">>, Dataset, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [Dataset, <<"metadata">> | _]}) ->
    {ok, [<<"datasets">>, Dataset, <<"edit">>]};

permission_required(_State) ->
    undefined.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

read(Req, State = #state{token = Token, path = []}) ->
    Start = now(),
    {ok, Permissions} = libsnarl:user_cache({token, Token}),
    ?MSnarl(?P(State), Start),
    Start1 = now(),
    {ok, Res} = libsniffle:dataset_list([{must, 'allowed', [<<"datasets">>, {<<"res">>, <<"dataset">>}, <<"get">>], Permissions}]),
    ?MSniffle(?P(State), Start1),
    {[ID || {_, ID} <- Res], Req, State};

read(Req, State = #state{path = [_Dataset], obj = Obj}) ->
    {Obj, Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = Version}, Decoded) ->
    case jsxd:from_list(Decoded) of
        [{<<"url">>, URL}] ->
            Start = now(),
            {ok, UUID} = libsniffle:dataset_import(URL),
            ?MSniffle(?P(State), Start),
            {{true, <<"/api/", Version/binary, "/datasets/", UUID/binary>>}, Req, State#state{body = Decoded}};
        [{<<"config">>, Config},
         {<<"snapshot">>, Snap},
         {<<"vm">>, Vm}] ->
            Start1 = now(),
            {ok, UUID} = libsniffle:vm_promote_snapshot(Vm, Snap, Config),
            ?MSniffle(?P(State), Start1),
            {{true, <<"/api/", Version/binary, "/datasets/", UUID/binary>>}, Req, State#state{body = Decoded}}
    end.

write(Req, State = #state{path = [Dataset, <<"metadata">> | Path]}, [{K, V}]) ->
    Start = now(),
    libsniffle:dataset_set(Dataset, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = [Dataset]}, [{K, V}]) ->
    Start = now(),
    libsniffle:dataset_set(Dataset, [K], jsxd:from_list(V)),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

write(Req, State = #state{path = []}, _Body) ->
    {true, Req, State};

write(Req, State, _Body) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DELETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [Dataset, <<"metadata">> | Path]}) ->
    Start = now(),
    libsniffle:dataset_set(Dataset, [<<"metadata">> | Path], delete),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Dataset]}) ->
    case libsniffle:dataset_get(Dataset) of
        {ok, D} ->
            case jsxd:get(<<"imported">>, D) of
                {ok, 1} ->
                    Start = now(),
                    ok = libsniffle:dataset_delete(Dataset),
                    ?MSniffle(?P(State), Start),
                    {true, Req, State};
                _ ->
                    {409, Req, State}
            end;
        _ ->
            {404, Req, State}
    end.

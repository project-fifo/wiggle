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
         delete/2,
         raw/1]).

-ignore_xref([allowed_methods/3,
              get/1,
              permission_required/1,
              read/2,
              create/3,
              write/3,
              delete/2,
              raw/1]).


allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Dataset]) ->
    [<<"GET">>, <<"DELETE">>, <<"PUT">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Dataset, <<"dataset.tar.gz">>]) ->
    [<<"PUT">>, <<"GET">>];

allowed_methods(_Version, _Token, [_Dataset, <<"dataset.tar.bz2">>]) ->
    [<<"PUT">>, <<"GET">>];

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

permission_required(#state{method = <<"POST">>, path = [Dataset]}) ->
    {ok, [<<"datasets">>, Dataset, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [Dataset, <<"dataset.tar.gz">>]}) ->
    {ok, [<<"datasets">>, Dataset, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [Dataset, <<"dataset.tar.bz2">>]}) ->
    {ok, [<<"datasets">>, Dataset, <<"export">>]};

permission_required(#state{method = <<"PUT">>, path = [Dataset, <<"dataset.tar.gz">>]}) ->
    {ok, [<<"datasets">>, Dataset, <<"create">>]};

permission_required(#state{method = <<"PUT">>, path = [Dataset, <<"dataset.tar.bz2">>]}) ->
    {ok, [<<"datasets">>, Dataset, <<"create">>]};

permission_required(#state{method = <<"DELETE">>, path = [Dataset]}) ->
    {ok, [<<"datasets">>, Dataset, <<"delete">>]};

permission_required(#state{method = <<"PUT">>, path = [Dataset, <<"metadata">> | _]}) ->
    {ok, [<<"datasets">>, Dataset, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [Dataset, <<"metadata">> | _]}) ->
    {ok, [<<"datasets">>, Dataset, <<"edit">>]};

permission_required(_State) ->
    undefined.


raw(#state{path=[_, <<"dataset.tar.gz">>], method = <<"PUT">>}) ->
    true;
raw(_) ->
    false.


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
    {Obj, Req, State};

read(Req, State = #state{path = [UUID, <<"dataset.tar.gz">>], obj = _Obj}) ->
    {ok, Req1} = cowboy_req:chunked_reply(200, Req),
    {ok, Idxs} = libsniffle:img_list(UUID),
    [begin
         {ok, Data} = libsniffle:img_get(UUID, Idx),
         case cowboy_req:chunk(Data, Req1) of
             ok ->
                 ok;
             {error, Reason} ->
                 lager:error("Export error: ~p", [Reason])
         end
     end || Idx <- Idxs],
    {Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [UUID], version = Version}, Decoded) ->
    case libsniffle:dataset_create(UUID) of
        duplicate ->
            {false, Req, State};
        _ ->
            D1 = transform_dataset(Decoded),
            libsniffle:dataset_set(UUID, [{<<"imported">>, 0} | D1]),
            {{true, <<"/api/", Version/binary, "/datasets/", UUID/binary>>},
             Req, State#state{body = Decoded}}
    end;

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

write(Req, State = #state{path = [UUID, <<"dataset.tar.gz">>]}, _) ->
    case libsniffle:dataset_get(UUID) of
        {ok, R} ->
            Size = jsxd:get(<<"image_size">>, 0, R),
            {Res, Req1} = import_dataset(UUID, 0, Size, Req),
            {Res, Req1, State};
        _ ->
            {false, Req, State}
    end;
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

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

transform_dataset(D1) ->
    {ok, ID} = jsxd:get(<<"uuid">>, D1),
    D2 = jsxd:thread(
           [{select,[<<"os">>, <<"metadata">>, <<"name">>,
                     <<"version">>, <<"description">>,
                     <<"disk_driver">>, <<"nic_driver">>]},
            {set, <<"dataset">>, ID},
            {set, <<"image_size">>,
             ensure_integer(jsxd:get(<<"image_size">>, 0, D1))},
            {set, <<"networks">>,
             jsxd:get(<<"requirements.networks">>, [], D1)}],
           D1),
    case jsxd:get(<<"os">>, D1) of
        {ok, <<"smartos">>} ->
            jsxd:set(<<"type">>, <<"zone">>, D2);
        {ok, _} ->
            jsxd:set(<<"type">>, <<"kvm">>, D2)
    end.

import_dataset(UUID, Idx, TotalSize, Req) ->
    case cowboy_req:stream_body(1024*1024, Req) of
        {ok, Data, Req1} ->
            Idx1 = Idx + 1,
            Done = (Idx1 * 1024*1024) / TotalSize,
            ok = libsniffle:img_create(UUID, Idx, Data),
            ok = libsniffle:dataset_set(UUID, <<"imported">>, Done),
            import_dataset(UUID, Idx1, TotalSize, Req1);
        {done, Req1} ->
            {true, Req1};
        {error, Reason} ->
            lager:error("Could not import dataset ~s: ", [UUID, Reason]),
            {false, Req}
    end.

ensure_integer(I) when is_integer(I) ->
    I;
ensure_integer(L) when is_list(L) ->
    list_to_integer(L);
ensure_integer(B) when is_binary(B) ->
    list_to_integer(binary_to_list(B)).

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
         raw_body/1,
         content_types_accepted/1]).

-ignore_xref([allowed_methods/3,
              get/1,
              permission_required/1,
              read/2,
              create/3,
              write/3,
              delete/2,
              raw_body/1,
              content_types_accepted/1]).

-define(WRETRY, 5).

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Dataset]) ->
    [<<"GET">>, <<"DELETE">>, <<"PUT">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Dataset, <<"dataset.gz">>]) ->
    [<<"PUT">>, <<"GET">>];

allowed_methods(_Version, _Token, [_Dataset, <<"dataset.bz2">>]) ->
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

permission_required(#state{method = <<"GET">>, path = [Dataset, <<"dataset.gz">>]}) ->
    {ok, [<<"datasets">>, Dataset, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [Dataset, <<"dataset.bz2">>]}) ->
    {ok, [<<"datasets">>, Dataset, <<"export">>]};

permission_required(#state{method = <<"PUT">>, path = [Dataset, <<"dataset.gz">>]}) ->
    {ok, [<<"datasets">>, Dataset, <<"create">>]};

permission_required(#state{method = <<"PUT">>, path = [Dataset, <<"dataset.bz2">>]}) ->
    {ok, [<<"datasets">>, Dataset, <<"create">>]};

permission_required(#state{method = <<"DELETE">>, path = [Dataset]}) ->
    {ok, [<<"datasets">>, Dataset, <<"delete">>]};

permission_required(#state{method = <<"PUT">>, path = [Dataset, <<"metadata">> | _]}) ->
    {ok, [<<"datasets">>, Dataset, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [Dataset, <<"metadata">> | _]}) ->
    {ok, [<<"datasets">>, Dataset, <<"edit">>]};

permission_required(_State) ->
    undefined.


raw_body(#state{path=[_, <<"dataset.gz">>], method = <<"PUT">>}) ->
    true;
raw_body(_) ->
    false.
content_types_accepted(#state{path=[_, <<"dataset.gz">>], method = <<"PUT">>}) ->
    [
     {{<<"application">>, <<"x-gzip">>, '*'}, write}
    ];
content_types_accepted(_) ->
    wiggle_handler:accepted().

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

read(Req, State = #state{token = Token, path = [], full_list=FullList, full_list_fields=Filter}) ->
    Start = now(),
    {ok, Permissions} = libsnarl:user_cache(Token),
    ?MSnarl(?P(State), Start),
    Start1 = now(),
    {ok, Res} = libsniffle:dataset_list([{must, 'allowed', [<<"datasets">>, {<<"res">>, <<"dataset">>}, <<"get">>], Permissions}], FullList),
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

read(Req, State = #state{path = [_Dataset], obj = Obj}) ->
    {Obj, Req, State};

read(Req, State = #state{path = [UUID, <<"dataset.gz">>], obj = _Obj}) ->
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
            libsniffle:dataset_set(UUID, [{<<"imported">>, 0},
                                          {<<"status">>, <<"pending">>}| D1]),
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

write(Req, State = #state{path = [UUID, <<"dataset.gz">>]}, _) ->
    case libsniffle:dataset_get(UUID) of
        {ok, R} ->
            Size = jsxd:get(<<"image_size">>, 1, R),
            libsniffle:dataset_set(UUID, <<"status">>, <<"importing">>),
            {Res, Req1} = import_dataset(UUID, 0, ensure_integer(Size), Req, undefined),
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
    Start = now(),
    case libsniffle:dataset_get(Dataset) of
        {ok, D} ->
            case jsxd:get(<<"status">>, D) of
                {ok, <<"importing">>} ->
                    ?MSniffle(?P(State), Start),
                    {409, Req, State};
                _ ->
                    ok = libsniffle:dataset_delete(Dataset),
                    ?MSniffle(?P(State), Start),
                    {true, Req, State}
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
           [{select,[<<"os">>, <<"metadata">>, <<"name">>, <<"version">>,
                     <<"description">>, <<"disk_driver">>, <<"nic_driver">>,
                     <<"users">>]},
            {set, <<"dataset">>, ID},
            {set, <<"image_size">>,
             ensure_integer(
               jsxd:get(<<"image_size">>,
                        jsxd:get([<<"files">>, 0, <<"size">>], 0, D1), D1))},
            {set, <<"networks">>,
             jsxd:get(<<"requirements.networks">>, [], D1)}],
           D1),
    D3 = case jsxd:get(<<"homepage">>, D1) of
             {ok, HomePage} ->
                 jsxd:set([<<"metadata">>, <<"homepage">>], HomePage, D2);
             _ ->
                 D2
         end,
    case jsxd:get(<<"os">>, D1) of
        {ok, <<"smartos">>} ->
            jsxd:set(<<"type">>, <<"zone">>, D3);
        {ok, _} ->
            jsxd:set(<<"type">>, <<"kvm">>, D3)
    end.

import_dataset(UUID, Idx, TotalSize, Req, WReq) ->
    case cowboy_req:stream_body(1024*1024, Req) of
        {ok, Data, Req1} ->
            case do_write(UUID, Idx, Data, WReq, 0) of
                {ok, WReq1} ->
                    Idx1 = Idx + 1,
                    Done = (Idx1 * 1024*1024) / TotalSize,
                    libsniffle:dataset_set(UUID, <<"imported">>, Done),
                    libhowl:send(UUID,
                                 [{<<"event">>, <<"progress">>},
                                  {<<"data">>, [{<<"imported">>, Done}]}]),
                    import_dataset(UUID, Idx1, TotalSize, Req1, WReq1);
                Reason ->
                    fail_import(UUID, Reason, Idx)
            end;
        {done, Req1} ->
            libsniffle:img_create(UUID, done, <<>>, WReq),
            ok = libsniffle:dataset_set(UUID, <<"imported">>, 1),
            libsniffle:dataset_set(UUID, <<"status">>, <<"imported">>),
            libhowl:send(UUID,
                         [{<<"event">>, <<"progress">>},
                          {<<"data">>, [{<<"imported">>, 1}]}]),
            {true, Req1};
        {error, Reason} ->
            fail_import(UUID, Reason, Idx),
            {false, Req}
    end.

do_write(_, _, _, _, ?WRETRY) ->
    {error, retry_exceeded};
do_write(UUID, Idx, Data, WReq, Retry) ->
    case libsniffle:img_create(UUID, Idx, Data, WReq) of
        {ok, WReq1} ->
            {ok, WReq1};
        Reason ->
            lager:warning("[~s:~p] Import Error: ~p",
                         [UUID, Retry, Reason]),
            do_write(UUID, Idx, Data, WReq, Retry + 1)
    end.

fail_import(UUID, Reason, Idx) ->
    lager:error("[~s] Could not import dataset: ~p", [UUID, Reason]),
    libhowl:send(UUID,
                 [{<<"event">>, <<"error">>},
                  {<<"data">>, [{<<"message">>, Reason},
                                {<<"index">>, Idx}]}]),
    libsniffle:dataset_set(UUID, <<"imported">>, <<"failed">>).

ensure_integer(I) when is_integer(I) ->
    I;
ensure_integer(L) when is_list(L) ->
    list_to_integer(L);
ensure_integer(B) when is_binary(B) ->
    list_to_integer(binary_to_list(B)).

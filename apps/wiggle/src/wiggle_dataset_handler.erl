%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_dataset_handler).

-export([init/3,
         rest_init/2]).

-export([content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         resource_exists/2,
         delete_resource/2,
         forbidden/2,
         options/2,
         post_is_create/2,
         create_path/2,
         service_available/2,
         is_authorized/2]).

-export([to_json/2,
         from_json/2,
         to_msgpack/2,
         from_msgpack/2]).

-ignore_xref([to_json/2,
              from_json/2,
              from_msgpack/2,
              to_msgpack/2,
              allowed_methods/2,
              content_types_accepted/2,
              content_types_provided/2,
              delete_resource/2,
              forbidden/2,
              init/3,
              post_is_create/2,
              create_path/2,
              is_authorized/2,
              options/2,
              service_available/2,
              resource_exists/2,
              rest_init/2]).
-record(state, {path, method, version, token, content, reply, obj, body}).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) ->
    wiggle_handler:initial_state(Req).

service_available(Req, State) ->
    case {libsniffle:servers(), libsnarl:servers()} of
        {[], _} ->
            {false, Req, State};
        {_, []} ->
            {false, Req, State};
        _ ->
            {true, Req, State}
    end.

options(Req, State) ->
    Methods = allowed_methods(State#state.version, State#state.token, State#state.path),
    Req1 = cowboy_req:set_resp_header(
             <<"access-control-allow-methods">>,
             string:join(
               lists:map(fun erlang:atom_to_list/1,
                         [<<"HEAD">>, <<"OPTIONS">> | Methods]), ", "), Req),
    {ok, Req1, State}.

post_is_create(Req, State) ->
    {true, Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json},
      {<<"application/x-msgpack">>, to_msgpack}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {wiggle_handler:accepted(), Req, State}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"OPTIONS">> | allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Dataset]) ->
    [<<"GET">>, <<"DELETE">>, <<"PUT">>];

allowed_methods(_Version, _Token, [_Dataset, <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>].

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [Dataset | _]}) ->
    case libsniffle:dataset_get(Dataset) of
        not_found ->
            {false, Req, State};
        {ok, Obj} ->
            {true, Req, State#state{obj = Obj}}
    end.

is_authorized(Req, State = #state{method = <<"OPTIONS">>}) ->
    {true, Req, State};

is_authorized(Req, State = #state{token = undefined}) ->
    {{false, <<"X-Snarl-Token">>}, Req, State};

is_authorized(Req, State) ->
    {true, Req, State}.

forbidden(Req, State = #state{method = <<"OPTIONS">>}) ->
    {false, Req, State};

forbidden(Req, State = #state{token = undefined}) ->
    {true, Req, State};

forbidden(Req, State = #state{method = <<"POST">>, path = []}) ->
    {allowed(State#state.token, [<<"cloud">>, <<"datasets">>, <<"create">>]), Req, State};

forbidden(Req, State = #state{path = []}) ->
    {allowed(State#state.token, [<<"cloud">>, <<"datasets">>, <<"list">>]), Req, State};


forbidden(Req, State = #state{method = <<"GET">>, path = [Dataset]}) ->
    {allowed(State#state.token, [<<"datasets">>, Dataset, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Dataset]}) ->
    {allowed(State#state.token, [<<"datasets">>, Dataset, <<"edit">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Dataset]}) ->
    {allowed(State#state.token, [<<"datasets">>, Dataset, <<"delete">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Dataset, <<"metadata">> | _]}) ->
    {allowed(State#state.token, [<<"datasets">>, Dataset, <<"edit">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Dataset, <<"metadata">> | _]}) ->
    {allowed(State#state.token, [<<"datasets">>, Dataset, <<"edit">>]), Req, State};

forbidden(Req, State) ->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

to_json(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {jsx:encode(Reply), Req1, State1}.

to_msgpack(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {msgpack:pack(Reply, [jsx]), Req1, State1}.

handle_request(Req, State = #state{token = Token, path = []}) ->
    {ok, Permissions} = libsnarl:user_cache({token, Token}),
    {ok, Res} = libsniffle:dataset_list([{must, 'allowed', [<<"datasets">>, {<<"res">>, <<"dataset">>}, <<"get">>], Permissions}]),
    {lists:map(fun ({E, _}) -> E end,  Res), Req, State};

handle_request(Req, State = #state{path = [_Dataset], obj = Obj}) ->
    {Obj, Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create_path(Req, State = #state{path = [], version = Version}) ->
    {ok, Decoded, Req1} = wiggle_handler:decode(Req),
    {ok, URL} = jsxd:get(<<"url">>, Decoded),
    {ok, UUID} = libsniffle:dataset_import(URL),
    {<<"/api/", Version/binary, "/datasets/", UUID/binary>>, Req1, State#state{body = Decoded}}.

from_json(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    {Reply, Req2, State1} = case Body of
                                <<>> ->
                                    handle_write(Req1, State, []);
                                _ ->
                                    Decoded = jsx:decode(Body),
                                    handle_write(Req1, State, Decoded)
                            end,
    {Reply, Req2, State1}.

from_msgpack(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    {Reply, Req2, State1} = case Body of
                                <<>> ->
                                    handle_write(Req1, State, []);
                                _ ->
                                    Decoded = msgpack:unpack(Body, [jsx]),
                                    handle_write(Req1, State, Decoded)
                            end,
    {Reply, Req2, State1}.

handle_write(Req, State = #state{path = [Dataset, <<"metadata">> | Path]}, [{K, V}]) ->
    libsniffle:dataset_set(Dataset, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    {true, Req, State};

handle_write(Req, State = #state{path = [Dataset]}, [{K, V}]) ->
    libsniffle:dataset_set(Dataset, [K], jsxd:from_list(V)),
    {true, Req, State};

handle_write(Req, State = #state{path = []}, _Body) ->
    {true, Req, State};

handle_write(Req, State, _Body) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DELETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [Dataset, <<"metadata">> | Path]}) ->
    libsniffle:dataset_set(Dataset, [<<"metadata">> | Path], delete),
    {true, Req, State};

delete_resource(Req, State = #state{path = [Dataset]}) ->
    ok = libsniffle:dataset_delete(Dataset),
    {true, Req, State}.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

allowed(Token, Perm) ->
    case libsnarl:allowed({token, Token}, Perm) of
        not_found ->
            true;
        true ->
            false;
        false ->
            true
    end.

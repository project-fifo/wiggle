%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_iprange_handler).

-export([init/3,
         rest_init/2]).

-export([content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         resource_exists/2,
         delete_resource/2,
         forbidden/2,
         service_available/2,
         options/2,
         create_path/2,
         post_is_create/2,
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
              service_available/2,
              is_authorized/2,
              options/2,
              create_path/2,
              post_is_create/2,
              resource_exists/2,
              rest_init/2]).

-record(state, {path, method, version, token, content, reply, obj, body}).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) ->
    wiggle_handler:initial_state(Req).

post_is_create(Req, State) ->
    {true, Req, State}.

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
             <<"Access-Control-Allow-Methods">>,
             string:join(
               lists:map(fun erlang:atom_to_list/1,
                         [<<"HEAD">>, <<"OPTIONS">> | Methods]), ", "), Req),
    {ok, Req1, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json},
      {<<"application/x-msgpack">>, to_msgpack}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {wiggle_handler:accepted(), Req, State}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"OPTIONS">> | allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

allowed_methods(_Version, _Token, [_Iprange, <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Iprange]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>].

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [Iprange | _]}) ->
    case libsniffle:iprange_get(Iprange) of
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

forbidden(Req, State = #state{method = <<"GET">>, path = []}) ->
    {allowed(State#state.token, [<<"cloud">>, <<"ipranges">>, <<"list">>]), Req, State};

forbidden(Req, State = #state{method = <<"POST">>, path = []}) ->
    {allowed(State#state.token, [<<"cloud">>, <<"ipranges">>, <<"create">>]), Req, State};

forbidden(Req, State = #state{method = <<"GET">>, path = [Iprange]}) ->
    {allowed(State#state.token, [<<"ipranges">>, Iprange, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Iprange]}) ->
    {allowed(State#state.token, [<<"ipranges">>, Iprange, <<"delete">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [_Iprange]}) ->
    {allowed(State#state.token, [<<"cloud">>, <<"ipranges">>, <<"create">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Iprange, <<"metadata">> | _]}) ->
    {allowed(State#state.token, [<<"ipranges">>, Iprange, <<"edit">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Iprange, <<"metadata">> | _]}) ->
    {allowed(State#state.token, [<<"ipranges">>, Iprange, <<"edit">>]), Req, State};

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
    {ok, Res} = libsniffle:iprange_list([{must, 'allowed', [<<"ipranges">>, {<<"res">>, <<"uuid">>}, <<"get">>], Permissions}]),
    {lists:map(fun ({E, _}) -> E end,  Res), Req, State};

handle_request(Req, State = #state{path = [_Iprange], obj = Obj}) ->
    {jsxd:thread([{update, <<"network">>, fun ip_to_str/1},
                  {update, <<"gateway">>, fun ip_to_str/1},
                  {update, <<"netmask">>, fun ip_to_str/1},
                  {update, <<"first">>, fun ip_to_str/1},
                  {update, <<"last">>, fun ip_to_str/1},
                  {update, <<"current">>, fun ip_to_str/1},
                  {update, <<"free">>,
                   fun (Free) ->
                           lists:map(fun ip_to_str/1, Free)
                   end}], Obj), Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create_path(Req, State = #state{path = [], version = Version}) ->
    {ok, Data, Req1} = wiggle_handler:decode(Req),
    {ok, Iprange} = jsxd:get(<<"name">>, Data),
    {ok, Network} = jsxd:get(<<"network">>, Data),
    {ok, Gateway} = jsxd:get(<<"gateway">>, Data),
    {ok, Netmask} = jsxd:get(<<"netmask">>, Data),
    {ok, First} = jsxd:get(<<"first">>, Data),
    {ok, Last} = jsxd:get(<<"last">>, Data),
    {ok, Tag} = jsxd:get(<<"tag">>, Data),
    Vlan = jsxd:get(<<"vlan">>, 0, Data),
    case libsniffle:iprange_create(Iprange, Network, Gateway, Netmask, First, Last, Tag, Vlan) of
        {ok, UUID} ->
            {<<"/api/", Version/binary, "/ipranges/", UUID/binary>>, Req1, State#state{body = Data}};
        duplicate ->
            {ok, Req2} = cowboy_req:reply(409, Req1),
            {halt, Req2, State}
    end.


from_json(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    {Reply, Req2, State1} = case Body of
                                <<>> ->
                                    handle_write(Req1, State, []);
                                _ ->
                                    Decoded = jsxd:from_list(jsx:decode(Body)),
                                    handle_write(Req1, State, Decoded)
                            end,
    {Reply, Req2, State1}.

from_msgpack(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    {Reply, Req2, State1} = case Body of
                                <<>> ->
                                    handle_write(Req1, State, []);
                                _ ->
                                    {ok, D} = msgpack:unpack(Body, [jsx]),
                                    Decoded = jsxd:from_list(D),
                                    handle_write(Req1, State, Decoded)
                            end,
    {Reply, Req2, State1}.

%% TODO : This is a icky case it is called after post.
handle_write(Req, State = #state{method = <<"POST">>, path = []}, _) ->
    {true, Req, State};

handle_write(Req, State = #state{path = [Iprange, <<"metadata">> | Path]}, [{K, V}]) ->
    libsniffle:iprange_set(Iprange, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    {true, Req, State};

handle_write(Req, State, _Body) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [Iprange, <<"metadata">> | Path]}) ->
    libsniffle:iprange_set(Iprange, [<<"metadata">> | Path], delete),
    {true, Req, State};

delete_resource(Req, State = #state{path = [Iprange]}) ->
    ok = libsniffle:iprange_delete(Iprange),
    {true, Req, State}.

allowed(Token, Perm) ->
    case libsnarl:allowed({token, Token}, Perm) of
        not_found ->
            true;
        true ->
            false;
        false ->
            true
    end.


ip_to_str(Ip) ->
    <<A:8, B:8, C:8, D:8>> = <<Ip:32>>,
    list_to_binary(io_lib:format("~p.~p.~p.~p", [A, B, C, D])).

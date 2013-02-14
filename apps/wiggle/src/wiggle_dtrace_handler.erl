%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_dtrace_handler).

-export([init/3,
         rest_init/2]).

-export([content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         resource_exists/2,
         service_available/2,
         delete_resource/2,
         forbidden/2,
         options/2,
         create_path/2,
         post_is_create/2,
         is_authorized/2]).

-export([to_json/2,
         from_json/2]).

-ignore_xref([to_json/2,
              from_json/2,
              allowed_methods/2,
              content_types_accepted/2,
              content_types_provided/2,
              delete_resource/2,
              forbidden/2,
              init/3,
              is_authorized/2,
              options/2,
              service_available/2,
              resource_exists/2,
              create_path/2,
              post_is_create/2,
              rest_init/2]).

-record(state, {path, method, version, token, content, reply, obj, body}).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Req, _) ->
    wiggle_handler:initial_state(Req, <<"dtraces">>).

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
    Methods = allowed_methods(Req, State, State#state.path),
    {ok, Req1} = cowboy_http_req:set_resp_header(
                   <<"Access-Control-Allow-Methods">>,
                   string:join(
                     lists:map(fun erlang:atom_to_list/1,
                               ['HEAD', 'OPTIONS' | Methods]), ", "), Req),
    {ok, Req1, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {wiggle_handler:accepted(), Req, State}.

allowed_methods(Req, State) ->
    {['HEAD', 'OPTIONS' | allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

allowed_methods(_Version, _Token, []) ->
    ['GET', 'POST'];

allowed_methods(_Version, _Token, [_Dtrace, <<"metadata">>|_]) ->
    ['PUT', 'DELETE'];

allowed_methods(_Version, _Token, [_Dtrace]) ->
    ['GET', 'PUT', 'DELETE'].

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [Dtrace | _]}) ->
    case libsniffle:dtrace_get(Dtrace) of
        {ok, not_found} ->
            {false, Req, State};
        {ok, Obj} ->
            {true, Req, State#state{obj = Obj}}
    end.

is_authorized(Req, State = #state{method = 'OPTIONS'}) ->
    {true, Req, State};

is_authorized(Req, State = #state{token = undefined}) ->
    {{false, <<"X-Snarl-Token">>}, Req, State};

is_authorized(Req, State) ->
    {true, Req, State}.

forbidden(Req, State = #state{method = 'OPTIONS'}) ->
    {false, Req, State};

forbidden(Req, State = #state{token = undefined}) ->
    {true, Req, State};

forbidden(Req, State = #state{method= 'GET', path = []}) ->
    {allowed(State#state.token, [<<"cloud">>, <<"dtraces">>, <<"list">>]), Req, State};

forbidden(Req, State = #state{method= 'POST', path = []}) ->
    {allowed(State#state.token, [<<"cloud">>, <<"dtraces">>, <<"create">>]), Req, State};

forbidden(Req, State = #state{method = 'GET', path = [Dtrace]}) ->
    {allowed(State#state.token, [<<"dtraces">>, Dtrace, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = 'DELETE', path = [Dtrace]}) ->
    {allowed(State#state.token, [<<"dtraces">>, Dtrace, <<"delete">>]), Req, State};

forbidden(Req, State = #state{method = 'PUT', path = [_Dtrace]}) ->
    {allowed(State#state.token, [<<"cloud">>, <<"dtraces">>, <<"create">>]), Req, State};

forbidden(Req, State = #state{method = 'PUT', path = [Dtrace, <<"metadata">> | _]}) ->
    {allowed(State#state.token, [<<"dtraces">>, Dtrace, <<"edit">>]), Req, State};

forbidden(Req, State = #state{method = 'DELETE', path = [Dtrace, <<"metadata">> | _]}) ->
    {allowed(State#state.token, [<<"dtraces">>, Dtrace, <<"edit">>]), Req, State};

forbidden(Req, State) ->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

to_json(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {jsx:encode(Reply), Req1, State1}.

handle_request(Req, State = #state{token = Token, path = []}) ->
    {ok, Permissions} = libsnarl:user_cache({token, Token}),
    {ok, Res} = libsniffle:dtrace_list([{must, 'allowed', [<<"dtraces">>, {<<"res">>, <<"uuid">>}, <<"get">>], Permissions}]),
    {lists:map(fun ({E, _}) -> E end,  Res), Req, State};

handle_request(Req, State = #state{path = [_Dtrace], obj = Obj}) ->
    {Obj, Req, State}.


%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create_path(Req, State = #state{path = [], version = Version}) ->
    {ok, Body, Req1} = cowboy_http_req:body(Req),
    {Data, Req2} = case Body of
                       <<>> ->
                           {[], Req1};
                       _ ->
                           D = jsxd:from_list(jsx:decode(Body)),
                           {D, Req1}
                   end,
    {ok, Dtrace} = jsxd:get(<<"name">>, Data),
    {ok, Script} = jsxd:get(<<"script">>, Data),
    case libsniffle:dtrace_add(Dtrace, Script) of
        {ok, UUID} ->
            case jsxd:get(<<"config">>, Data) of
                {ok, Config} ->
                    ok = libsniffle:dtrace_set(UUID, <<"config">>, Config);
                _ ->
                    ok
            end,
            {<<"/api/", Version/binary, "/dtraces/", UUID/binary>>, Req2, State};
        duplicate ->
            {ok, Req3} = cowboy_http_req:reply(409, Req2),
            {halt, Req3, State}
    end.

from_json(Req, State) ->
    {ok, Body, Req1} = cowboy_http_req:body(Req),
    {Reply, Req2, State1} = case Body of
                                <<>> ->
                                    handle_write(Req1, State, null);
                                _ ->
                                    Decoded = jsx:decode(Body),
                                    handle_write(Req1, State, Decoded)
                            end,
    {Reply, Req2, State1}.

%% TODO : This is a icky case it is called after post.

handle_write(Req, State = #state{method = 'POST', path = []}, _) ->
    {true, Req, State};

handle_write(Req, State = #state{path = [Dtrace, <<"metadata">> | Path]}, [{K, V}]) ->
    libsniffle:dtrace_set(Dtrace, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    {true, Req, State};

handle_write(Req, State, _Body) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [Dtrace, <<"metadata">> | Path]}) ->
    libsniffle:dtrace_set(Dtrace, [<<"metadata">> | Path], delete),
    {true, Req, State};

delete_resource(Req, State = #state{path = [Dtrace]}) ->
    ok = libsniffle:dtrace_delete(Dtrace),
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

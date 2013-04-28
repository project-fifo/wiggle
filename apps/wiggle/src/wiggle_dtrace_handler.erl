%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_dtrace_handler).
-include("wiggle.hrl").


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
         is_authorized/2,
         rest_terminate/2]).

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
              is_authorized/2,
              options/2,
              service_available/2,
              resource_exists/2,
              create_path/2,
              post_is_create/2,
              rest_init/2,
              rest_terminate/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) ->
    wiggle_handler:initial_state(Req).

rest_terminate(_Req, State) ->
    ?M(?P(State), State#state.start),
    ok.

post_is_create(Req, State) ->
    {true, Req, State}.

service_available(Req, State) ->
    {wiggle_handler:service_available(), Req, State}.

options(Req, State) ->
    Methods = allowed_methods(State#state.version, State#state.token, State#state.path),
    wiggle_handler:options(Req, State,Methods).

content_types_provided(Req, State) ->
    {wiggle_handler:provided(), Req, State}.

content_types_accepted(Req, State) ->
    {wiggle_handler:accepted(), Req, State}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"OPTIONS">> | allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Dtrace, <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Dtrace]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>].

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [Dtrace | _]}) ->
    Start = now(),
    case libsniffle:dtrace_get(Dtrace) of
        not_found ->
            ?MSniffle(?P(State), Start),
            {false, Req, State};
        {ok, Obj} ->
            ?MSniffle(?P(State), Start),
            {true, Req, State#state{obj = Obj}}
    end.

is_authorized(Req, State = #state{method = <<"OPTIONS">>}) ->
    {true, Req, State};

is_authorized(Req, State = #state{token = undefined}) ->
    {{false, <<"x-snarl-token">>}, Req, State};

is_authorized(Req, State) ->
    {true, Req, State}.

forbidden(Req, State = #state{method = <<"OPTIONS">>}) ->
    {false, Req, State};

forbidden(Req, State = #state{token = undefined}) ->
    {true, Req, State};

forbidden(Req, State = #state{method= <<"GET">>, path = []}) ->
    {wiggle_handler:allowed(State, [<<"cloud">>, <<"dtraces">>, <<"list">>]), Req, State};

forbidden(Req, State = #state{method= <<"POST">>, path = []}) ->
    {wiggle_handler:allowed(State, [<<"cloud">>, <<"dtraces">>, <<"create">>]), Req, State};

forbidden(Req, State = #state{method = <<"GET">>, path = [Dtrace]}) ->
    {wiggle_handler:allowed(State, [<<"dtraces">>, Dtrace, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Dtrace]}) ->
    {wiggle_handler:allowed(State, [<<"dtraces">>, Dtrace, <<"delete">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Dtrace]}) ->
    {wiggle_handler:allowed(State, [<<"dtraces">>, Dtrace, <<"edit">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Dtrace, <<"metadata">> | _]}) ->
    {wiggle_handler:allowed(State, [<<"dtraces">>, Dtrace, <<"edit">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Dtrace, <<"metadata">> | _]}) ->
    {wiggle_handler:allowed(State, [<<"dtraces">>, Dtrace, <<"edit">>]), Req, State};

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
    Start = now(),
    {ok, Permissions} = libsnarl:user_cache({token, Token}),
    ?MSnarl(?P(State), Start),
    Start1 = now(),
    {ok, Res} = libsniffle:dtrace_list([{must, 'allowed', [<<"dtraces">>, {<<"res">>, <<"uuid">>}, <<"get">>], Permissions}]),
    ?MSniffle(?P(State), Start1),
    {lists:map(fun ({E, _}) -> E end,  Res), Req, State};

handle_request(Req, State = #state{path = [_Dtrace], obj = Obj}) ->
    Obj1 = jsxd:update(<<"script">>, fun (S) ->
                                             list_to_binary(S)
                                     end, Obj),
    {Obj1, Req, State}.


%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create_path(Req, State = #state{path = [], version = Version}) ->
    {ok, Data, Req1} = wiggle_handler:decode(Req),
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
            {<<"/api/", Version/binary, "/dtrace/", UUID/binary>>, Req1, State#state{body = Data}};
        duplicate ->
            {ok, Req3} = cowboy_req:reply(409, Req1),
            {halt, Req3, State}
    end.

from_json(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    {Reply, Req2, State1} = case Body of
                                <<>> ->
                                    handle_write(Req1, State, null);
                                _ ->
                                    Decoded = jsx:decode(Body),
                                    handle_write(Req1, State, Decoded)
                            end,
    {Reply, Req2, State1}.

from_msgpack(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    {Reply, Req2, State1} = case Body of
                                <<>> ->
                                    handle_write(Req1, State, null);
                                _ ->
                                    Decoded = msgpack:unpack(Body, [jsx]),
                                    handle_write(Req1, State, Decoded)
                            end,
    {Reply, Req2, State1}.

%% TODO : This is a icky case it is called after post.

handle_write(Req, State = #state{method = <<"POST">>, path = []}, _) ->
    {true, Req, State};

handle_write(Req, State = #state{path = [Dtrace, <<"metadata">> | Path]}, [{K, V}]) ->
    Start = now(),
    libsniffle:dtrace_set(Dtrace, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

handle_write(Req, State, _Body) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [Dtrace, <<"metadata">> | Path]}) ->
    Start = now(),
    libsniffle:dtrace_set(Dtrace, [<<"metadata">> | Path], delete),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete_resource(Req, State = #state{path = [Dtrace]}) ->
    Start = now(),
    ok = libsniffle:dtrace_delete(Dtrace),
    ?MSniffle(?P(State), Start),
    {true, Req, State}.

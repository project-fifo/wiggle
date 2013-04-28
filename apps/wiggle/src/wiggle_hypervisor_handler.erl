%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_hypervisor_handler).
-include("wiggle.hrl").

-export([init/3,
         rest_init/2]).

-export([content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         resource_exists/2,
         forbidden/2,
         service_available/2,
         options/2,
         delete_resource/2,
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
              rest_init/2,
              rest_terminate/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) ->
    wiggle_handler:initial_state(Req).

rest_terminate(_Req, State) ->
    ?M(?P(State), State#state.start),
    ok.

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
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Hypervisor]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Hypervisor, <<"characteristics">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, [_Hypervisor, <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>].

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [Hypervisor | _]}) ->
    Start = now(),
    case libsniffle:hypervisor_get(Hypervisor) of
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

forbidden(Req, State = #state{path = []}) ->
    {wiggle_handler:allowed(State, [<<"cloud">>, <<"hypervisors">>, <<"list">>]), Req, State};

forbidden(Req, State = #state{method = <<"GET">>, path = [Hypervisor]}) ->
    {wiggle_handler:allowed(State, [<<"hypervisors">>, Hypervisor, <<"get">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Hypervisor, <<"metadata">> | _]}) ->
    {wiggle_handler:allowed(State, [<<"hypervisors">>, Hypervisor, <<"edit">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Hypervisor, <<"metadata">> | _]}) ->
    {wiggle_handler:allowed(State, [<<"hypervisors">>, Hypervisor, <<"edit">>]), Req, State};

forbidden(Req, State = #state{method = <<"PUT">>, path = [Hypervisor, <<"characteristics">> | _]}) ->
    {wiggle_handler:allowed(State, [<<"hypervisors">>, Hypervisor, <<"edit">>]), Req, State};

forbidden(Req, State = #state{method = <<"DELETE">>, path = [Hypervisor, <<"characteristics">> | _]}) ->
    {wiggle_handler:allowed(State, [<<"hypervisors">>, Hypervisor, <<"edit">>]), Req, State};

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
    {ok, Res} = libsniffle:hypervisor_list([{must, 'allowed', [<<"hypervisors">>, {<<"res">>, <<"name">>}, <<"get">>], Permissions}]),
    {lists:map(fun ({E, _}) -> E end,  Res), Req, State};

handle_request(Req, State = #state{path = [_Hypervisor], obj = Obj}) ->
    {Obj, Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

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
                                    {ok, Decoded} = msgpack:unpack(Body, [jsx]),
                                    handle_write(Req1, State, Decoded)
                            end,
    {Reply, Req2, State1}.

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

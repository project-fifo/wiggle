-module(wiggle_rest_handler).

-include("wiggle.hrl").

-export([init/3,
         rest_init/2,
         rest_terminate/2]).

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
         generate_etag/2,
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
              generate_etag/2,
              forbidden/2,
              init/3,
              rest_terminate/2,
              is_authorized/2,
              options/2,
              service_available/2,
              resource_exists/2,
              create_path/2,
              post_is_create/2,
              rest_init/2]).

init(_Transport, _Req, _) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, [Module]) ->
    {ok, Req1, State} = wiggle_handler:initial_state(Req),
    {ok, Req1, State#state{module = Module}}.

rest_terminate(_Req, State) ->
    statman_histogram:record_value({State#state.path_bin, total}, State#state.start),
    ok.

post_is_create(Req, State) ->
    {true, Req, State}.

service_available(Req, State) ->
    {wiggle_handler:service_available(), Req, State}.

options(Req, State = #state{module = M}) ->
    Methods = M:allowed_methods(State#state.version, State#state.token, State#state.path),
    wiggle_handler:options(Req, State,Methods).

content_types_provided(Req, State) ->
    {wiggle_handler:provided(), Req, State}.

content_types_accepted(Req, State) ->
    {wiggle_handler:accepted(), Req, State}.

allowed_methods(Req, State = #state{module = M}) ->
    {[<<"HEAD">>, <<"OPTIONS">> | M:allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{module = M}) ->
    case M:get(State) of
        not_found ->
            {false, Req, State};
        {ok, Obj} ->
            {true, Req, State#state{obj = Obj}}
    end.

generate_etag(Req, State = #state{obj = undefined}) ->
    {undefined, Req, State};

generate_etag(Req, State = #state{obj = Obj}) ->
    {base64:encode(crypto:md5(term_to_binary(Obj))), Req, State}.

is_authorized(Req, State = #state{method = <<"OPTIONS">>}) ->
    {true, Req, State};

is_authorized(Req, State = #state{method = <<"GET">>,
                                  module = wiggle_cloud_handler,
                                  path = [<<"connection">>]}) ->
    {true, Req, State};

is_authorized(Req, State = #state{method = <<"POST">>,
                              module = wiggle_session_handler,
                              path = []}) ->
    {true, Req, State};

is_authorized(Req, State = #state{token = undefined}) ->
    {{false, <<"x-snarl-token">>}, Req, State};

is_authorized(Req, State) ->
    {true, Req, State}.

forbidden(Req, State = #state{method = <<"OPTIONS">>}) ->
    {false, Req, State};

forbidden(Req, State = #state{method = <<"GET">>,
                              module = wiggle_cloud_handler,
                              path = [<<"connection">>]}) ->
    {false, Req, State};

forbidden(Req, State = #state{method = <<"POST">>,
                              module = wiggle_session_handler,
                              path = []}) ->
    {false, Req, State};

forbidden(Req, State = #state{token = undefined}) ->
    {true, Req, State};

forbidden(Req, State = #state{module = M}) ->
    case M:permission_required(State) of
        {error, needs_decode} ->
            {ok, Decoded, Req1} = wiggle_handler:decode(Req),
            forbidden(Req1, State#state{body = Decoded});
        undefined ->
            {true, Req, State};
        {ok, always} ->
            {false, Req, State};
        {multiple, Permissions} ->
            R = lists:foldl(fun(Permission, Acc) ->
                                    Acc andalso wiggle_handler:allowed(State, Permission)
                            end, true, Permissions),
            {R, Req, State};
        {ok, Permission} ->
            {wiggle_handler:allowed(State, Permission), Req, State}
    end.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

to_json(Req, State = #state{module = M}) ->
    {Reply, Req1, State1} = M:handle_request(Req, State),
    Start = now(),
    D = jsx:encode(Reply),
    ?MEx(?P(State), <<"jsx:encode">>, Start),
    {D, Req1, State1}.

to_msgpack(Req, State = #state{module = M}) ->
    {Reply, Req1, State1} = M:handle_request(Req, State),
    Start = now(),
    D = msgpack:pack(Reply, [jsx]),
    ?MEx(?P(State), <<"msgpack:encode">>, Start),
    {D, Req1, State1}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create_path(Req, State = #state{module = M, body = undefined}) ->
    {ok, Data, Req1} = wiggle_handler:decode(Req),
    M:create_path(Req1, State#state{body = Data}, Data);

create_path(Req, State = #state{module = M, body = Data}) ->
    M:create_path(Req, State, Data).

from_json(Req, State = #state{module = M, body = undefined}) ->
    {ok, Data, Req1} = wiggle_handler:decode(Req),
    M:handle_write(Req1, State#state{body = Data}, Data);

from_json(Req, State = #state{module = M, body = Data}) ->
    M:handle_write(Req, State, Data).

from_msgpack(Req, State = #state{module = M, body = undefined}) ->
    {ok, Data, Req1} = wiggle_handler:decode(Req),
    M:handle_write(Req1, State#state{body = Data}, Data);

from_msgpack(Req, State = #state{module = M, body = Data}) ->
    M:handle_write(Req, State, Data).

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{module = M}) ->
    M:delete_resource(Req, State).

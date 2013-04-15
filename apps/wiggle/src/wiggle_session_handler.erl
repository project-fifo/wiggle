%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_session_handler).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/3,
         rest_init/2]).
-export([content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         delete_resource/2,
         resource_exists/2,
         forbidden/2,
         options/2,
         post_is_create/2,
         service_available/2,
         create_path/2,
         is_authorized/2]).

-export([to_json/2,
         from_json/2,
         to_msgpack/2,
         from_msgpack/2]).

-ignore_xref([to_json/2,
              from_json/2,
              from_msgpack/2,
              to_msgpack/2,
              create_path/2,
              allowed_methods/2,
              content_types_accepted/2,
              post_is_create/2,
              content_types_provided/2,
              delete_resource/2,
              forbidden/2,
              init/3,
              service_available/2,
              is_authorized/2,
              options/2,
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
             <<"access-control-allow-methods">>,
             string:join(
               lists:map(fun erlang:binary_to_list/1,
                         [<<"HEAD">>, <<"GET">>, <<"OPTIONS">> | Methods]), ", "), Req),
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

allowed_methods(_Version, _Token, []) ->
    [<<"POST">>];

allowed_methods(_Version, _Token, [_Session]) ->
    [<<"GET">>, <<"POST">>, <<"DELETE">>].

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [Session]}) ->
    case libsnarl:user_get({token, Session}) of
        not_found ->
            {false, Req, State};
        {ok, Obj} ->
            {true, Req, State#state{obj = Obj}}
    end.

is_authorized(Req, State) ->
    {true, Req, State}.

forbidden(Req, State) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

to_json(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {jsx:encode(Reply), Req1, State1}.

to_msgpack(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {msgpack:pack(Reply, [jsx]), Req1, State1}.

handle_request(Req, State = #state{path = [Session], obj = Obj}) ->
    Obj1 = jsxd:thread([{set, <<"session">>, Session},
                        {delete, <<"password">>},
                        {update, <<"permissions">>,
                           fun (Permissions) ->
                                   lists:map(fun jsonify_permissions/1, Permissions)
                           end, []}],
                       Obj),
    {Obj1, Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create_path(Req, State = #state{path = [], version = Version}) ->
    {ok, Decoded, Req1} = wiggle_handler:decode(Req),
    {ok, User} = jsxd:get(<<"user">>, Decoded),
    {ok, Pass} = jsxd:get(<<"password">>, Decoded),
    case libsnarl:auth(User, Pass) of
        {ok, {token, UUID}} ->
            Req2 = cowboy_req:set_resp_cookie(<<"x-snarl-token">>, UUID,
                                              [{max_age, 364*24*60*60}], Req1),
            Req3 = cowboy_req:set_resp_header(<<"x-snarl-token">>, UUID, Req2),
            {<<"/api/", Version/binary, "/sessions/", UUID/binary>>, Req3, State#state{body = Decoded}};
        _ ->
            {ok, Req2} = cowboy_req:reply(403, [], <<"Forbidden!">>, Req1),
            {halt, Req2, State}
    end.

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

handle_write(Req, State, _) ->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [Session]}) ->
    libsnarl:token_delete(Session),
    {true, Req, State}.


jsonify_permissions(P) ->
    lists:map(fun('...') ->
                      <<"...">>;
                 ('_') ->
                      <<"_">>;
                 (E) ->
                      E
              end, P).

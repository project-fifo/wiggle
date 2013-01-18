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
         create_path/2,
         is_authorized/2]).

-export([to_json/2,
         from_json/2]).

-ignore_xref([to_json/2,
              from_json/2,
              create_path/2,
              allowed_methods/2,
              content_types_accepted/2,
              post_is_create/2,
              content_types_provided/2,
              delete_resource/2,
              forbidden/2,
              init/3,
              is_authorized/2,
              options/2,
              resource_exists/2,
              rest_init/2]).

-record(state, {path, method, version, token, content, reply, obj}).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Req, _) ->
    wiggle_handler:initial_state(Req, <<"sessions">>).

post_is_create(Req, State) ->
    {true, Req, State}.

options(Req, State) ->
    Methods = allowed_methods(Req, State, State#state.path),
    {ok, Req1} = cowboy_http_req:set_resp_header(
                   <<"Access-Control-Allow-Methods">>,
                   string:join(
                     lists:map(fun erlang:atom_to_list/1,
                               ['HEAD', 'GET', 'OPTIONS' | Methods]), ", "), Req),
    {ok, Req1, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json; charset=UTF-8">>, from_json},
      {<<"application/json; charset=utf-8">>, from_json}
     ], Req, State}.

allowed_methods(Req, State) ->
    {['HEAD', 'OPTIONS' | allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

allowed_methods(_Version, _Token, []) ->
    ['POST'];

allowed_methods(_Version, _Token, [_Session]) ->
    ['GET', 'POST'].

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
    {ok, Body, Req1} = cowboy_http_req:body(Req),
    {Decoded, Req2} = case Body of
                          <<>> ->
                              {[], Req1};
                          _ ->
                              D = jsx:decode(Body),
                              {D, Req1}
                      end,
    {ok, User} = jsxd:get(<<"user">>, Decoded),
    {ok, Pass} = jsxd:get(<<"password">>, Decoded),
    case libsnarl:auth(User, Pass) of
        {ok, {token, UUID}} ->
            {ok, Req3} = cowboy_http_req:set_resp_cookie(<<"X-Snarl-Token">>, UUID,
                                                         [{max_age, 364*24*60*60}], Req2),
            {<<"/api/", Version/binary, "/sessions/", UUID/binary>>, Req3, State};
        _ ->
            {ok, Req3} = cowboy_http_req:reply(403, [], <<"Forbidden!">>, Req2),
            {ok, Req3, State}
    end.

from_json(Req, State) ->
    {ok, Body, Req1} = cowboy_http_req:body(Req),
    {Reply, Req2, State1} = case Body of
                                <<>> ->
                                    handle_write(Req1, State, []);
                                _ ->
                                    Decoded = jsx:decode(Body),
                                    handle_write(Req1, State, Decoded)
                            end,

    {Reply, Req2, State1}.

handle_write(Req, State, _) ->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete_resource(Req, State = #state{path = [_Session]}) ->
    %% TODO
    {true, Req, State}.


jsonify_permissions(P) ->
    lists:map(fun('...') ->
                      <<"...">>;
                 ('_') ->
                      <<"_">>;
                 (E) ->
                      E
              end, P).

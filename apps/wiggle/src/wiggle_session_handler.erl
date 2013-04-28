%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_session_handler).
-include("wiggle.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([allowed_methods/3,
         get/1,
         permission_required/1,
         handle_request/2,
         create_path/3,
         handle_write/3,
         delete_resource/2]).

-ignore_xref([allowed_methods/3,
              get/1,
              permission_required/1,
              handle_request/2,
              create_path/3,
              handle_write/3,
              delete_resource/2]).

allowed_methods(_Version, _Token, []) ->
    [<<"POST">>];

allowed_methods(_Version, _Token, [_Session]) ->
    [<<"GET">>, <<"POST">>, <<"DELETE">>].


get(State = #state{path = [Session]}) ->
    Start = now(),
    R = libsnarl:user_get({token, Session}),
    ?MSnarl(?P(State), Start),
    R.

permission_required(_State) ->
    {ok, always}.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

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

create_path(Req, State = #state{path = [], version = Version}, Decoded) ->
    {ok, User} = jsxd:get(<<"user">>, Decoded),
    {ok, Pass} = jsxd:get(<<"password">>, Decoded),
    case libsnarl:auth(User, Pass) of
        {ok, {token, UUID}} ->
            Req1 = cowboy_req:set_resp_cookie(<<"x-snarl-token">>, UUID,
                                              [{max_age, 364*24*60*60}], Req),
            Req2 = cowboy_req:set_resp_header(<<"x-snarl-token">>, UUID, Req1),
            {<<"/api/", Version/binary, "/sessions/", UUID/binary>>, Req2, State#state{body = Decoded}};
        _ ->
            {ok, Req1} = cowboy_req:reply(403, [], <<"Forbidden!">>, Req),
            {halt, Req1, State}
    end.

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

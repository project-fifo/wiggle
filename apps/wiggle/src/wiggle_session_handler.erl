%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_session_handler).
-include("wiggle.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([allowed_methods/3,
         permission_required/1,
         get/1,
         create/3,
         read/2,
         write/3,
         delete/2]).

-ignore_xref([allowed_methods/3,
              permission_required/1,
              get/1,
              read/2,
              create/3,
              write/3,
              delete/2]).

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

read(Req, State = #state{path = [Session], obj = Obj}) ->
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

create(Req, State = #state{path = [], version = Version}, Decoded) ->
    {ok, User} = jsxd:get(<<"user">>, Decoded),
    {ok, Pass} = jsxd:get(<<"password">>, Decoded),
    R = case jsxd:get(<<"otp">>, Decoded) of
            {ok, OTP} ->
                libsnarl:auth(User, Pass, OTP);
            _ ->
                libsnarl:auth(User, Pass)
        end,
    case R of
        {ok, {token, UUID}} ->
            OneYear = 364*24*60*60,
            Req1 = cowboy_req:set_resp_cookie(<<"x-snarl-token">>, UUID,
                                              [{max_age, OneYear}], Req),
            Req2 = cowboy_req:set_resp_header(<<"x-snarl-token">>, UUID, Req1),
            {{true, <<"/api/", Version/binary, "/sessions/", UUID/binary>>},
             Req2, State#state{body = Decoded}};
        key_required ->
            {ok, Req1} = cowboy_req:reply(449, [], <<"Retry with valid parameters: user, password, otp.">>, Req),
            {halt, Req1, State};
        _ ->
            {ok, Req1} = cowboy_req:reply(401, [], <<"Forbidden!">>, Req),
            {halt, Req1, State}
    end.

write(Req, State, _) ->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [Session]}) ->
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

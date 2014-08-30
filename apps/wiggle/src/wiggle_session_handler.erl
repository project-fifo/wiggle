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

allowed_methods(_Version, _Token, [?UUID(_Session)]) ->
    [<<"GET">>, <<"POST">>, <<"DELETE">>].


get(State = #state{path = [?UUID(Session)]}) ->
    Start = now(),
    R = ls_user:get({token, Session}),
    ?MSnarl(?P(State), Start),
    R;

get(_State) ->
    not_found.

permission_required(_State) ->
    {ok, always}.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

read(Req, State = #state{path = [?UUID(Session)], obj = Obj}) ->
    Obj1 = jsxd:thread([{set, <<"session">>, Session},
                        {delete, <<"password">>}],
                       ft_user:to_json(Obj)),
    {Obj1, Req, State}.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = Version}, Decoded) ->
    case {jsxd:get(<<"user">>, Decoded), jsxd:get(<<"password">>, Decoded)} of
        {{ok, User}, {ok, Pass}} ->
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
            end;
        _ ->
            {ok, Req1} = cowboy_req:reply(400, [], <<"Missing JSON keys: user, password required.">>, Req),
            {halt, Req1, State}
    end.

write(Req, State, _) ->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [?UUID(Session)]}) ->
    libsnarl:token_delete(Session),
    {true, Req, State}.

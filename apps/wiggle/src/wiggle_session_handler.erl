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

-behaviour(wiggle_rest_handler).

allowed_methods(?V1, _Token, []) ->
    [<<"POST">>];

allowed_methods(?V2, _Token, []) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, [_Session]) ->
    [<<"GET">>, <<"POST">>, <<"DELETE">>].


get(State = #state{path = [Session]}) ->
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

read(Req, State = #state{path = [], obj = Obj, version = ?V2}) ->
    {wiggle_user_handler:to_json(Obj), Req, State};

read(Req, State = #state{path = [Session], obj = Obj, version = ?V1}) ->
    Obj1 = jsxd:thread([{set, <<"session">>, Session}],
                       wiggle_user_handler:to_json(Obj)),
    {Obj1, Req, State}.


%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = ?V1}, Decoded) ->
    case {jsxd:get(<<"user">>, Decoded), jsxd:get(<<"password">>, Decoded)} of
        {{ok, User}, {ok, Pass}} ->
            R = case jsxd:get(<<"otp">>, Decoded) of
                    {ok, OTP} ->
                        libsnarl:auth(User, Pass, OTP);
                    _ ->
                        libsnarl:auth(User, Pass)
                end,
            case R of
                {ok, {token, Session}} ->
                    Req2 = cowboy_req:set_resp_header(<<"x-snarl-token">>, Session, Req),
                    {{true, <<"/api/0.1.0/sessions/", Session/binary>>},
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
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [Session]}) ->
    libsnarl:token_delete(Session),
    {true, Req, State}.

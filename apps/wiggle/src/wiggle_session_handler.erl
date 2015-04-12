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

allowed_methods(?V2, _Token, [<<"one_time_token">>]) ->
    [<<"GET">>];

allowed_methods(?V1, _Token, [_Session]) ->
    [<<"GET">>, <<"DELETE">>];

allowed_methods(?V2, _Token, [_Session]) ->
    [<<"DELETE">>].

get(#state{path = [<<"one_time_token">>], version = ?V2,
           bearer = Bearer}) when is_binary(Bearer) ->
    {ok, {oauth2_token:generate('x-snarl-one-time-token'), Bearer}};

get(State = #state{path = [Session], version = ?V1}) ->
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

read(Req, State = #state{path = [<<"one_time_token">>],
                         obj = {OTT, Bearer}, version = ?V2}) ->
    Start = now(),
    {ok, OTT} = ls_token:add(OTT, 30, Bearer),
    ?MSnarl(?P(State), Start),
    {[{<<"expiery">>, 30}, {<<"token">>, OTT}], Req, State};

read(Req, State = #state{path = [], token = Token, version = ?V2}) ->
    {ok, Obj} = ls_user:get(Token),
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
                        case libsnarl:auth(User, Pass) of
                            {ok, UUID} ->
                                case ls_user:yubikeys(UUID) of
                                    {ok, []} ->
                                        {ok, UUID};
                                    _ ->
                                        key_required
                                end
                        end
                end,
            R2 = case R of
                     {ok, UUID1} ->
                         {ok, Tkn} = ls_user:make_token(UUID1),
                         {ok, {token, Tkn}};
                     E ->
                         E
                 end,
            case R2 of
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
    ls_token:delete(Session),
    {true, Req, State}.

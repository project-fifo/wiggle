%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2011, Heinz N. Gies
%%% @doc This module provides simple Session handling with the data
%%%      entirely stored in the cookie, aka on the client. The data
%%%      is encrypted using an AES 128 CBC encryption to prevent
%%%      tempering.
%%%      Any Erlang term can be used as Session data.
%%%
%%% @end
%%% Created :  9 Dec 2011 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(wiggle_session).

%% API
-export([get/1, set/2, del/1, get_session/3, set_session/4, rem_session/2]).

%%%===================================================================
%%% API
%%%===================================================================

get(Req) ->
    {ok, SessionCookieName} = application:get_env(wiggle, session_name),
    {ok, SessionKey} = application:get_env(wiggle, session_key),
    get_session(SessionCookieName, SessionKey, Req).


set(Req, Session) ->
    {ok, SessionCookieName} = application:get_env(wiggle, session_name),
    {ok, SessionKey} = application:get_env(wiggle, session_key),
    set_session(SessionCookieName, SessionKey, Req, Session).


del(Req) ->
    {ok, SessionCookieName} = application:get_env(wiggle, session_name),
    rem_session(SessionCookieName, Req).



%%--------------------------------------------------------------------
%% @doc Retrieves the session data from the cookies, returns either a
%% the term stored or undefined when no session was found.
%% SessionKey needs to be of the size 16 or a multiplyer from it.
%% @spec get_session(binary(), binary(), #http_req{}) 
%%       -> undefined | term()
%% @end
%%--------------------------------------------------------------------

get_session(SessionCookieName, SessionKey, Req) ->
    case cowboy_http_req:cookie(SessionCookieName,Req) of
	{undefined, _Req} -> 
	    undefined;
	{<<>>, _Req} ->
	    undefined;
	{SessionData, _Req} -> 
	    dec_term(SessionKey, SessionData)
    end.

%%--------------------------------------------------------------------
%% @doc Sets the session data as a cookie in the Request.
%% SessionKey needs to be of the size 16 or a multiplyer from it.
%% @spec set_session(binary(), binary(), #http_req{}, term()) 
%%       -> {ok, #http_req{}}
%% @end
%%--------------------------------------------------------------------

set_session(SessionCookieName, SessionKey, Req, Session) ->
    cowboy_http_req:set_resp_cookie(SessionCookieName, enc_term(SessionKey, Session), [{max_age, 365*24*3600}], Req).

%%--------------------------------------------------------------------
%% @doc Deletes the session data as a cookie in the Request.
%% @spec rem_session(binary(), binary(), #http_req{}, term()) 
%%       -> {ok, #http_req{}}
%% @end
%%--------------------------------------------------------------------

rem_session(SessionCookieName, Req) ->
    cowboy_http_req:set_resp_cookie(SessionCookieName, <<>>, [{max_age, 0}], Req).    

%%%===================================================================
%%% Internal functions
%%%===================================================================

aes_dec(Key, Text) when is_binary(Text) -> 
    <<Size:32, IVec:16/binary, Messag/binary>> = Text,
    <<Msg:Size/binary, _/binary>> = crypto:aes_cbc_128_decrypt(Key, IVec, Messag),
    Msg.

aes_enc(Key, Text) when is_binary(Text) -> 
    MsgSize = size(Text),
    Missing = (16 - (MsgSize rem 16)) * 8,
    IVec = crypto:aes_cbc_ivec(crypto:rand_bytes(16)),
    Enc = crypto:aes_cbc_128_encrypt(Key, IVec, <<Text/binary, 0:Missing>>),
    <<MsgSize:32, IVec:16/binary, Enc/binary>>.

enc_term(Key, Term) when is_binary(Key)->
    list_to_binary(edoc_lib:escape_uri(binary_to_list(base64:encode(aes_enc(Key, term_to_binary(Term)))))).

dec_term(Key, Term) when is_binary(Term), is_binary(Key) ->
    binary_to_term(aes_dec(Key, base64:decode(cowboy_http:urldecode(Term)))).


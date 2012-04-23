%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 21 Apr 2012 by Heinz N. Gies <>
%%%-------------------------------------------------------------------
-module(wiggle_storage).
-include_lib("stdlib/include/qlc.hrl"). 


-record(user, {id, name, passwd, key_id, key, admin}).


%% API
-export([init/0,
	 get_user/1,
	 add_user/6,
	 add_user/3,
	 get_session/1,
	 verify/2,
	 get_auth/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    case mnesia:create_table(user,
			     [{disc_copies, [node()]},
			      {attributes,
			       record_info(fields,user)}]) of
	{aborted,{already_exists,user}} ->
	    ok;
	_ ->
	    add_user("admin", "admin", true)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
add_user(UID, Pass, Admin) ->
    add_user(UID, UID, Pass, "", "", Admin).


add_user(UID, Name, Pass, KeyID, Key, Admin) ->
    Fun = fun() ->
		  mnesia:write(
		    #user{id = UID,
			  name = Name,
			  passwd = Pass,
			  key_id = KeyID,
			  key = Key,
			  admin = Admin})
	  end,
    mnesia:transaction(Fun).

get_user(ID) ->
    Fun =
        fun() ->
		mnesia:read({user, ID})
        end,
    case mnesia:transaction(Fun) of
	{atomic, [User]} -> 
	    {ok, User};
	{aborted, R} -> 
	    {error, R};
	_ -> 
	    {error, not_found}
    end.

    
get_auth(ID) ->
    case get_user(ID) of
	{ok, User} -> 
	    {ok, user_to_auth(User)};
	Error -> 
	    Error
    end.

get_session(ID) ->
    case get_user(ID) of
	{ok, User} -> 
	    {ok, {User#user.id, User#user.admin, user_to_auth(User)}};
	Error -> 
	    Error
    end.



verify(ID, Pass) ->
    Fun =
        fun() ->
		mnesia:match_object({user, ID, '_', Pass, '_', '_', '_'})
        end,
    case mnesia:transaction(Fun) of
	{atomic, [User]} -> 
	    {ok, {User#user.id, User#user.admin, user_to_auth(User)}};
	{aborted, R} -> 
	    {error, R};
	_ -> 
	    {error, not_found}
    end.
    

%%%===================================================================
%%% Internal functions
%%%===================================================================

user_to_auth(User) ->
    {ok, Host} = application:get_env(wiggle, host),
    {User#user.name,
     Host,
     User#user.key_id,
     User#user.key}.

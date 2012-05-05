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



-record(user, {id, name, passwd, priv_key, pub_key, admin}).
-record(config, {id, value}).


%% API
-export([init/0,
	 get_user/1,
	 get_user/2,
	 add_user/4,
	 add_user/3,
	 set_keys/3,
	 set_user/3,
	 get_session/1,
	 verify/2,
	 get_auth/1,
	 get_config/1,
	 set_config/2]).

-define(USER_GETTER(F), get_user(#user{F = Res}, F) -> Res).
-define(USER_SETTER(F), set_user(#user{id = UID} = User, F, Val) ->
	       {ok, User} = get_user(UID),
	       write(User#user{F = Val})).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

init() ->
    N = node(),
    mnesia:create_schema([N]),
    mnesia:start(),
    case mnesia:create_table(user,
			     [{disc_copies, [N]},
			      {attributes,
			       record_info(fields,user)}]) of
	{aborted,{already_exists,user}} ->
	    ok;
	_ ->
	    add_user("admin", "admin", true)
    end,
    case mnesia:create_table(config,
			     [{disc_copies, [N]},
			      {attributes,
			       record_info(fields,config)}]) of
	{aborted,{already_exists,user}} ->
	    ok;
	_ ->
	    set_config(api_host, "http://127.0.0.1")
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

    
add_user(UID, Pass, Admin) ->
    Res = add_user(UID, UID, Pass, Admin),
    wiggle_keymanager:create_key(UID),
    Res.


add_user(UID, Name, Pass, Admin) ->
    write(#user{id = UID,
		     name = Name,
		     passwd = crypto:sha256(Pass),
		     admin = Admin}).

set_config(ID, Value) ->
    write(#config{id = ID,
		  value = Value}).


set_keys(UID, Priv, Pub) ->
    {ok, User} = get_user(UID),
    write(User#user{priv_key = Priv, pub_key = Pub}).
    


?USER_SETTER(id);

?USER_SETTER(name);

set_user(#user{id = UID} = User, passwd, Val) ->
    {ok, User} = get_user(UID),
    Hash =  crypto:sha256(Val),
    write(User#user{passwd = Hash});
	
?USER_SETTER(priv_key);

?USER_SETTER(pub_key);

?USER_SETTER(admin);


set_user(_User, _Field, _Val) ->
    {error, does_not_exist}.



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

get_user(UID, Field) when is_list(UID) ->
    case get_user(UID) of
	{ok, User} ->
	    get_user(User, Field);
	Error ->
	    Error
    end;


?USER_GETTER(id);

?USER_GETTER(name);

?USER_GETTER(passwd);

?USER_GETTER(priv_key);

?USER_GETTER(pub_key);

?USER_GETTER(admin);

get_user(_User, _Field) ->
    {error, does_not_exist}.


get_config(ID) ->
    Fun =
        fun() ->
		mnesia:read({config, ID})
        end,
    case mnesia:transaction(Fun) of
	{atomic,[{config, ID, Value}]} ->
	    {ok, Value};
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
		mnesia:match_object({user, ID, '_', crypto:sha256(Pass), '_', '_', '_'})
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
    {ok, Host} = get_config(api_host),
    {ok, KeyID} = application:get_env(wiggle, key_id),
    {User#user.name,
     Host,
     KeyID,
     User#user.priv_key}.

write(Value) ->
    Fun = fun() ->
		  mnesia:write(Value)
	  end,
    mnesia:transaction(Fun).


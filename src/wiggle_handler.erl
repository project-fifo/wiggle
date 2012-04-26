%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 20 Apr 2012 by Heinz N. Gies <>
%%%-------------------------------------------------------------------
-module(wiggle_handler).

-behaviour(cowboy_http_handler).

%% Callbacks
-export([init/3, handle/2, terminate/2]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

handle(Req, State) ->
    {Path, Req2} = cowboy_http_req:path(Req),
    {Method, Req3} = cowboy_http_req:method(Req2),
    case wiggle_session:get(Req3) of
	undefined ->
	    case Path of
		[<<"login">>] ->
		    request(Method, Path, undefined, Req3, State);
		_ ->
		    login(Req3, State)
	    end;
	{UID, _, _} = Session ->
	    case wiggle_storage:get_user(UID) of
		{ok, _} ->
		    request(Method, Path, Session, Req3, State);
		_ ->
		    {ok, Req4} = wiggle_session:del(Req3),
		    login(Req4, State)
	    end
    end.

login(Req, State) ->
    {ok, Req2} =  cowboy_http_req:reply(200, [{<<"Refresh">>, <<"0; url=/login">>}], <<"">>, Req),
    {ok, Req2, State}.

request('GET', [<<"login">>], undefined, Req, State) ->
    {ok, Page} = tpl_login:render([]),
    {ok, Req1} = cowboy_http_req:reply(200, [], Page, Req),
    {ok, Req1, State};


request('POST', [<<"login">>], undefined, Req, State) ->
    {Vals, Req1} = cowboy_http_req:body_qs(Req),
    User = proplists:get_value(<<"login">>, Vals),
    Pass = proplists:get_value(<<"pass">>, Vals),
    case wiggle_storage:verify(binary_to_list(User), binary_to_list(Pass)) of
	{ok, {_, true, _} = Session} ->
	    {ok, Req2} = wiggle_session:set(Req1, Session),
	    {ok, Req3} = cowboy_http_req:reply(200, [{<<"Refresh">>, <<"0; url=/admin">>}], <<"">>, Req2),
	    {ok, Req3, State};
	{ok, {_, _, Auth} = Session} ->
	    case cloudapi:list_keys(Auth) of 
		{ok, _} ->
		    {ok, Req2} = wiggle_session:set(Req1, Session),
		    {ok, Req3} = cowboy_http_req:reply(200, [{<<"Refresh">>, <<"0; url=/">>}], <<"">>, Req2),
		    {ok, Req3, State};		
		_ ->
		    {ok, Req2} = wiggle_session:set(Req1, Session),
		    {ok, Req3} = cowboy_http_req:reply(200, [{<<"Refresh">>, <<"0; url=/account">>}], <<"">>, Req2),
		    {ok, Req3, State}
		end;
	_ ->
	    {ok, Page} = tpl_login:render([{<<"messages">>, 
					    [[{<<"text">>, <<"Login failed">>},
					      {<<"class">>, <<"error">>}]]}]),
	    {ok, Req2} = cowboy_http_req:reply(200, [], Page, Req1),
	    {ok, Req2, State}
    end;

request('POST', [<<"my">>, <<"machines">>], {_UUID, _Admin, Auth}, Req, State) ->
    {Vals, Req1} = cowboy_http_req:body_qs(Req),
    Name = proplists:get_value(<<"name">>, Vals),
    Package = proplists:get_value(<<"package">>, Vals),
    Dataset = proplists:get_value(<<"dataset">>, Vals),
    Obj = [{<<"package">>, Package}, {<<"dataset">>, Dataset}],
    Obj1 = case Name of
	       <<>> ->
		   Obj;
	       _ ->
		   [{<<"name">>, Name} | Obj]
	   end,

    case cloudapi:create_machine(Auth, Obj1) of
	{ok, Res} ->
	    io:format("~p~n", [Res]),
	    reply_json(Req1, Res, State);
	Error ->
	    io:format("~p~n", [Error]),
	    {ok, Req2} = cowboy_http_req:reply(500, [], <<"error">>, Req1),
	    {ok, Req2, State}
    end;

request('DELETE', [<<"my">>, <<"machines">>, VMUUID], {_UUID, _Admin, Auth}, Req, State) ->
    case cloudapi:delete_machine(Auth, VMUUID) of
	ok ->
	    {ok, Req1} = cowboy_http_req:reply(200, [], <<"">>, Req),
	    {ok, Req1, State};
	Error ->
	    io:format("~p~n", [Error]),
	    {ok, Req1} = cowboy_http_req:reply(500, [], <<"error">>, Req),
	    {ok, Req1, State}
    end;


request('GET', [<<"logout">>], {_UUID, _Admin, _Auth}, Req, State) ->
    {ok, Req1} = wiggle_session:del(Req),
    {ok, Req2} = cowboy_http_req:reply(200, [{<<"Refresh">>, <<"0; url=/login">>}], <<"">>, Req1),
    {ok, Req2, State};

request('GET', [], {_UUID, Admin, _Auth}, Req, State) ->
    {ok, Page} = tpl_index:render([{admin, Admin}]),
    {ok, Req2} = cowboy_http_req:reply(200, [], Page, Req),
    {ok, Req2, State};

request('GET', [<<"analytics">>], {_UUID, Admin, _Auth}, Req, State) ->
    {ok, Page} = tpl_analytics:render([{admin, Admin},{page, "analytics"}]),
    {ok, Req2} = cowboy_http_req:reply(200, [], Page, Req),
    {ok, Req2, State};

request('GET', [<<"system">>], {_UUID, Admin, _Auth}, Req, State) ->
    {ok, Page} = tpl_system:render([{admin, Admin},{page, "system"}]),
    {ok, Req2} = cowboy_http_req:reply(200, [], Page, Req),
    {ok, Req2, State};

request('GET', [<<"about">>], {_UUID, Admin, _Auth}, Req, State) ->
    {ok, Page} = tpl_about:render([{admin, Admin},
				   {page, "about"}]),
    {ok, Req2} = cowboy_http_req:reply(200, [], Page, Req),
    {ok, Req2, State};

request('GET', [<<"admin">>], {_, true, _Auth} , Req, State) ->
    {ok, Page} = tpl_admin:render([{admin, true},
				   {page, "admin"}]),
    {ok, Req2} = cowboy_http_req:reply(200, [], Page, Req),
    {ok, Req2, State};

request('POST', [<<"admin">>], {_, true, _Auth} , Req, State) ->
    {Vals, Req1} = cowboy_http_req:body_qs(Req),
    Name =  binary_to_list(proplists:get_value(<<"name">>, Vals)),
    Pass =  binary_to_list(proplists:get_value(<<"pass">>, Vals)),
    wiggle_storage:add_user(Name, Pass, false),    
    {ok, Page} = tpl_admin:render([{admin, true},
				   {page, "admin"}]),
    {ok, Req2} = cowboy_http_req:reply(200, [], Page, Req1),
    {ok, Req2, State};

request('GET', [<<"account">>], {UUID, Admin, Auth}, Req, State) ->
    {Name, _, _, _} = Auth,
    {ok, User} = wiggle_storage:get_user(UUID),
    Messages = case cloudapi:list_keys(Auth) of 
			{ok, _} ->
			    undefined;
			_ ->
			    [[{text, <<"You are not authenticated with the API backend.">>}, {class, <<"error">>}]]
		    end,
    {ok, Page} = tpl_account:render([{admin, Admin}, 
				     {name, Name}, 
				     {messages, Messages},
				     {priv_key,wiggle_storage:get_user(User, priv_key)},
				     {pub_key, wiggle_storage:get_user(User, pub_key)},
				     {page, "account"}]),
    {ok, Req2} = cowboy_http_req:reply(200, [], Page, Req),
    {ok, Req2, State};

request('POST', [<<"account">>], {UID,Admin,Auth}, Req, State) ->
    {Vals, Req1} = cowboy_http_req:body_qs(Req),
    {Name, _, KeyID, _} = Auth,
    {ok, User} = wiggle_storage:get_user(UID),
    Messages = case cloudapi:list_keys(Auth) of 
			{ok, _} ->
			    undefined;
			_ ->
			    [[{text, <<"You are not authenticated with the API backend.">>}, {class, <<"error">>}]]
		    end,
    case proplists:get_value(<<"action">>, Vals) of
	<<"authenticate">> ->
	    Pass = binary_to_list(proplists:get_value(<<"password">>, Vals)),
	    cloudapi:create_key(Auth, Pass, KeyID, wiggle_storage:get_user(User, pub_key)),
	    Messages1 = case cloudapi:list_keys(Auth) of 
			   {ok, _} ->
			       [[{text, <<"Authentication succeeded.">>}, {class, <<"success">>}]];
			   _ ->
			       [[{text, <<"You are not authenticated with the API backend.">>}, {class, <<"error">>}]]
		       end,
	    {ok, Page} = tpl_account:render([{admin, Admin}, 
					     {name, Name}, 
					     {messages, Messages1},
					     {priv_key,wiggle_storage:get_user(User, priv_key)},
					     {pub_key, wiggle_storage:get_user(User, pub_key)},
					     {page, "account"}]),
	    {ok, Req2} = cowboy_http_req:reply(200, [], Page , Req1),
	    {ok, Req2, State};
       	<<"name">> ->
	    NewName =  binary_to_list(proplists:get_value(<<"name">>, Vals)),
	    wiggle_storage:set_user(UID, name, NewName),
	    {ok, User} = wiggle_storage:get_user(UID),
	    {ok, Auth1} = wiggle_storage:get_auth(UID),
	    Messages1 = case cloudapi:list_keys(Auth1) of 
			    {ok, _} ->
				undefined;
			    _ ->
				[[{text, <<"You are not authenticated with the API backend.">>}, {class, <<"error">>}]]
			end,
	    {ok, Page} = tpl_account:render([{admin, Admin}, 
					     {name, Name}, 
					     {messages, Messages1},
					     {priv_key,wiggle_storage:get_user(User, priv_key)},
					     {pub_key, wiggle_storage:get_user(User, pub_key)},
					     {page, "account"}]),
	    {ok, Session} = wiggle_storage:get_session(UID),
	    {ok, Req2} = wiggle_session:set(Req1, Session),
	    {ok, Req3} = cowboy_http_req:reply(200, [], Page , Req2),
	    {ok, Req3, State};
	<<"pass">> ->
	    Pass = wiggle_storage:get_user(User, passwd),
	    case crypto:sha265(proplists:get_value(<<"old">>, Vals)) of
		Pass ->
		    case {proplists:get_value(<<"new">>, Vals), proplists:get_value(<<"confirm">>, Vals)} of
			{New, New} ->
			    {ok, Page} = tpl_account:render([{admin, Admin},
							     {messages, 
							      [Messages| [{text, <<"Password changed.">>},
									  {class, <<"success">>}]]},
							     {name, Name}, 
							     {priv_key,wiggle_storage:get_user(User, priv_key)},
							     {pub_key, wiggle_storage:get_user(User, pub_key)},
							     {page, "account"}]),
			    {ok, Req2} = cowboy_http_req:reply(200, [], Page , Req1),
			    wiggle_storage:set_user(UID, passwd, binary_to_list(New)),
			    {ok, Req2, State};
			_ ->
			    {ok, Page} = tpl_account:render([{admin, Admin},
							     {messages, 
							      [Messages|
							       [{text, <<"Passwords did not match.">>},
								{class, <<"error">>}]]},
							     {name, Name}, 
							     {priv_key,wiggle_storage:get_user(User, priv_key)},
							     {pub_key, wiggle_storage:get_user(User, pub_key)},
							     {page, "account"}]),
			    {ok, Req2} = cowboy_http_req:reply(200, [], Page , Req1),
			    {ok, Req2, State}
			end;
		_ ->
		    {ok, Page} = tpl_account:render([{admin, Admin},
						     {messages, 
						      [Messages|
						       [{text, <<"Old passwords did not match.">>},
							{class, <<"error">>}]]},
						     {name, Name}, 
						     {priv_key,wiggle_storage:get_user(User, priv_key)},
						     {pub_key, wiggle_storage:get_user(User, pub_key)},
						     {page, "account"}]),
		    {ok, Req2} = cowboy_http_req:reply(200, [], Page , Req1),
		    {ok, Req2, State}
		end
    end;

request('GET', [<<"my">>, <<"machines">>], {_UUID, _Admin, Auth}, Req, State) ->
    {ok, {Res, _, _}} = cloudapi:list_machines(Auth),
    reply_json(Req, Res, State);

request('GET', [<<"my">>, <<"machines">>, UUID], {_UUID, _Admin, Auth}, Req, State) ->
    {ok, Res} = cloudapi:get_machine(Auth, binary_to_list(UUID)),
    reply_json(Req, Res, State);

request('POST', [<<"my">>, <<"machines">>, UUID], {_UUID, _Admin, Auth}, Req, State) ->
    case cowboy_http_req:qs_val(<<"action">>, Req) of
	{<<"start">>, _} ->
	    cloudapi:start_machine(Auth, binary_to_list(UUID));
	{<<"reboot">>, _} ->
	    cloudapi:reboot_machine(Auth, binary_to_list(UUID));
	{<<"stop">>, __} ->
	    cloudapi:stop_machine(Auth, binary_to_list(UUID))
    end,
    {ok, Res} = cloudapi:get_machine(Auth, binary_to_list(UUID)),
    reply_json(Req, Res, State);

request('GET', [<<"my">>, <<"datasets">>], {_UUID, _Admin, Auth}, Req, State) ->
    {ok, Res} = cloudapi:list_datasets(Auth),
    reply_json(Req, Res, State);

request('GET', [<<"my">>, <<"packages">>], {_UUID, _Admin, Auth}, Req, State) ->
    {ok, Res} = cloudapi:list_packages(Auth),
    reply_json(Req, Res, State);


request(_, _Path, {_UUID, _Admin, _Auth}, Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(404, [], <<"not found!">>, Req),
    {ok, Req2, State}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

terminate(_Req, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

reply_json(Req, Data, State) ->
    {ok, Req2} = cowboy_http_req:reply(200,
				       [{<<"Content-Type">>, <<"application/json">>}], 
				       jsx:to_json(Data), Req),
    {ok, Req2, State}.

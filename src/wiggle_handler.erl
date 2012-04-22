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
	Auth ->
	    request(Method, Path, Auth, Req3, State)
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
	{ok, Auth} ->
	    {ok, Req2} = wiggle_session:set(Req1, Auth),
	    {ok, Req3} = cowboy_http_req:reply(200, [{<<"Refresh">>, <<"0; url=/">>}], <<"">>, Req2),
	    {ok, Req3, State};
	_ ->
	    {ok, Page} = tpl_login:render([{<<"messages">>, 
					    [[{<<"text">>, <<"Login failed">>},
					      {<<"class">>, <<"error">>}]]}]),
	    {ok, Req2} = cowboy_http_req:reply(200, [], Page, Req1),
	    {ok, Req2, State}
    end;

request('GET', [], Auth, Req, State) ->
    {ok, {Res, _, _}} = cloudapi:list_machines(Auth),
    {ok, Page} = tpl_index:render([{vms, Res}]),
    {ok, Req2} = cowboy_http_req:reply(200, [], Page, Req),
    {ok, Req2, State};

request('GET', [<<"account">>], Auth, Req, State) ->
    {_, _, KeyID, _} = Auth,
    {ok, Page} = tpl_account:render([{key_id, KeyID}]),
    {ok, Req2} = cowboy_http_req:reply(200, [], Page, Req),
    {ok, Req2, State};

request('POST', [<<"account">>], Auth, Req, State) ->
    {Vals, Req1} = cowboy_http_req:body_qs(Req),
    io:format("~p~n", [Vals]),
    {ID, _, _, _} = Auth,
    {ok, {user, ID, Name, Pass, KeyID, Key}} = wiggle_storage:get_user(ID),
    case proplists:get_value(<<"action">>, Vals) of
       	<<"key">> ->
	    {ok, Page} = tpl_account:render([{message, <<"Not yet implemented">>},
					     {key_id, KeyID}]),
	    {ok, Req2} = cowboy_http_req:reply(200, [], Page , Req1),
	    NewKey = case binary_to_list(proplists:get_value(<<"key">>, Vals)) of
			   "" -> 
			       Key;
			   NewKey_ ->
			     NewKey_
		     end,
	    NewKeyID =  binary_to_list(proplists:get_value(<<"key_id">>, Vals)),
	    wiggle_storage:add_user(Name, Pass, NewKeyID, NewKey),
	    {ok, Req2, State};
	<<"pass">> ->
	    case binary_to_list(proplists:get_value(<<"old">>, Vals)) of
		Pass ->
		    case {proplists:get_value(<<"new">>, Vals), proplists:get_value(<<"confirm">>, Vals)} of
			{New, New} ->
			    {ok, Page} = tpl_account:render([{message, <<"Password changed.">>},
							     {key_id, KeyID}]),
			    {ok, Req2} = cowboy_http_req:reply(200, [], Page , Req1),
			    wiggle_storage:add_user(Name, binary_to_list(New), KeyID, Key),
			    {ok, Req2, State};
			_ ->
			    {ok, Page} = tpl_account:render([{message, <<"New passwords do not match!">>},
							     {key_id, KeyID}]),
			    {ok, Req2} = cowboy_http_req:reply(200, [], Page , Req1),
			    {ok, Req2, State}
			end;
		_ ->
		    {ok, Page} = tpl_account:render([{message, <<"Old key did not match">>},
						     {key_id, KeyID}]),
		    {ok, Req2} = cowboy_http_req:reply(200, [], Page , Req1),
		    {ok, Req2, State}
		end
    end;
    
%    User = proplists:get_value(<<"login">>, Vals),
%    Pass = proplists:get_value(<<"pass">>, Vals),

%    {ok, Page} = tpl_account:render([{key_id, KeyID}]),



request('GET', [<<"my">>, <<"machines">>], Auth, Req, State) ->
    {ok, {Res, _, _}} = cloudapi:list_machines(Auth),
    reply_json(Req, Res, State);

request('GET', [<<"my">>, <<"machines">>, UUID], Auth, Req, State) ->
    {ok, Res} = cloudapi:get_machine(Auth, binary_to_list(UUID)),
    reply_json(Req, Res, State);

request('POST', [<<"my">>, <<"machines">>, UUID], Auth, Req, State) ->
    case cowboy_http_req:qs_val(<<"action">>, Req) of
	<<"start">> ->
	    cloudapi:start_machine(Auth, binary_to_list(UUID));
	<<"reboot">> ->
	    cloudapi:reboot_machine(Auth, binary_to_list(UUID));
	<<"stop">> ->
	    cloudapi:stop_machine(Auth, binary_to_list(UUID))
    end,
    {ok, Res} = cloudapi:get_machine(Auth, binary_to_list(UUID)),
    reply_json(Req, Res, State);

request('GET', [<<"my">>, <<"datasets">>], Auth, Req, State) ->
    {ok, Res} = cloudapi:list_datasets(Auth),
    reply_json(Req, Res, State);

request('GET', [<<"my">>, <<"packages">>], Auth, Req, State) ->
    {ok, Res} = cloudapi:list_packages(Auth),
    reply_json(Req, Res, State);


request('GET', _Path, _Auth, Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [], <<"Hello world!">>, Req),
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

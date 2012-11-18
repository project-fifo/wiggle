-module(wiggle_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
    Dispatch = [
		{'_', [
		       {[<<"api">>, '_', <<"users">>, '...'], wiggle_user_handler, []},
		       {[<<"api">>, '_', <<"groups">>, '...'], wiggle_group_handler, []}
		      ]}
	       ],
    {ok, _} = cowboy:start_listener(http, 100,
				    cowboy_tcp_transport, [{port, 8080}],
				    cowboy_http_protocol, [{dispatch, Dispatch}]
				   ),
    wiggle_sup:start_link().

stop(_State) ->
    ok.

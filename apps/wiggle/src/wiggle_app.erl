-module(wiggle_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, load/0]).

check_grid() ->
    case length(redgrid:nodes()) of
	1 ->
	    timer:sleep(100),
	    check_grid();
	_ ->
	    application:stop(gproc),
	    application:start(gproc)
    end.

    

load() ->
    application:start(sasl),
    application:start(alog),
    application:start(lager),
    application:start(redgrid),
    application:start(libsniffle),
    application:start(uuid),
    application:start(cowboy),
    application:start(gproc),
    application:start(compiler),
    application:start(syntax_tools),
    application:start(erlydtl),
    application:start(nicedecimal),
    application:start(jsx),
    application:start(libsniffle),
    application:start(libsnarl),
    application:start(wiggle),
    ok.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    spawn(fun check_grid/0),
    wiggle_sup:start_link().

stop(_State) ->
    ok.

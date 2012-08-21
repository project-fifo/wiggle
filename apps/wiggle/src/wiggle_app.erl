-module(wiggle_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, load/0]).

load() ->

    application:start(sasl),
    application:start(alog),
    application:start(lager),
    application:start(crypto),
    application:start(mdns),
    application:start(backyard),
    application:start(uuid),
    application:start(cowboy),
    application:start(compiler),
    application:start(syntax_tools),
    application:start(erlydtl),
    application:start(nicedecimal),
    application:start(jsx),
    application:start(libsniffle),
    application:start(libsnarl),
    application:start(statsderl),
    application:start(vmstats),
    application:start(wiggle),
    ok.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    wiggle_sup:start_link().

stop(_State) ->
    ok.

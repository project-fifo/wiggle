-module(wiggle).

-export([start/0]).

start() ->
    application:start(mdns_client_lib),
    application:start(libsnarl),
    application:start(libsniffle),
    application:start(jsx),
    application:start(lager),
    application:start(mimetypes),
    application:start(cowboy),
    application:start(wiggle).


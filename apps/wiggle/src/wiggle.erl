-module(wiggle).

-export([start/0]).

-ignore_xref([start/0]).

start() ->
    application:start(sasl),
    lager:start(),
    application:start(mdns_client_lib),
    application:start(libsnarlmatch),
    application:start(libchunter),
    application:start(libsnarl),
    application:start(libhowl),
    application:start(libsniffle),
    application:start(jsx),
    application:start(lager),
    application:start(mimetypes),
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    application:start(newrelic),
    application:start(mnesia),
    application:start(snmp),
    application:start(wiggle).


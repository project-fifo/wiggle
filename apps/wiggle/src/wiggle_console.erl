-module(wiggle_console).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         connections/1
        ]).

-ignore_xref([
              connections/1
             ]).

print_endpoints(Es) ->
    io:format("Hostname            "
              "                    "
              " Node               "
              " Errors    ~n"),
    io:format("--------------------"
              "--------------------"
              "----------"
              " ---------------~n", []),
    [print_endpoint(E) || E <- Es].

print_endpoint([{{Hostname, [{port,Port},{ip,IP}]}, _, Fails}]) ->
    HostPort = <<IP/binary, ":", Port/binary>>,
    io:format("~40s ~-19s ~9b~n", [Hostname, HostPort, Fails]).

connections(["snarl"]) ->
    io:format("Snarl endpoints.~n"),
    print_endpoints(libsnarl:servers());

connections(["howl"]) ->
    io:format("Howl endpoints.~n"),
    print_endpoints(libhowl:servers());

connections(["sniffle"]) ->
    io:format("Sniffle endpoints.~n"),
    print_endpoints(libhowl:servers());

connections([]) ->
    connections(["sniffle"]),
    io:format("~n"),
    connections(["snarl"]),
    io:format("~n"),
    connections(["howl"]).

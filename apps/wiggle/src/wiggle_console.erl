-module(wiggle_console).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         connections/1,
         cache_stats/1
        ]).

-ignore_xref([
              connections/1,
              cache_stats/1
             ]).


cache_stats([]) ->
    Caches = ["token", "package", "dataset", "dtrace", "hypervisor", "iprange",
              "network", "org", "role", "user", "vm", "grouping"],
    cache_stats(Caches);

cache_stats(Caches) ->
    io:format("Cache               "
              " Hits               "
              " Missed             "
              " Q1 Size            "
              " Q2 Size            ~n"),
    io:format("--------------------"
              " -------------------"
              " -------------------"
              " -------------------"
              " -------------------~n"),
    print_cache(token),
    [begin
         print_cache(list_to_atom(Cache)),
         print_cache(list_to_atom(Cache ++ "_list"))
     end || Cache <- Caches].


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

print_cache(Cache) ->
    [{hits,H},{misses,M},{q1size,Q1},{q2size,Q2}] = e2qc:stats(Cache),
    io:format("~20p ~-19b ~-19b ~-19b ~-19b~n", [Cache, H, M, Q1, Q2]).


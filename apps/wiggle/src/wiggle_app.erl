-module(wiggle_app).

-behaviour(application).

-include("wiggle_version.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(wiggle, port),
    {ok, Compression} = application:get_env(wiggle, compression),
    {ok, Acceptors} = application:get_env(wiggle, acceptors),

    case (catch eplugin:wait_for_init()) of
        {'EXIT', Why} ->
            lager:warning("Error waiting for eplugin init: ~p", [Why]),
            lager:warning("Your plugins are probably taking too long to load, "
                          "and some wiggle:dispatchs hooks may not run.");
        ok -> ok
    end,
    PluginDispatchs = eplugin:fold('wiggle:dispatchs', []),

    Dispatch = cowboy_router:compile(
                 [{'_', [{<<"/api/:version/oauth/token">>,
                          wiggle_oauth_token, []},
                         {<<"/api/:version/oauth/auth">>,
                          wiggle_oauth_auth, []},
                         {<<"/api/:version/users/[...]">>,
                          wiggle_rest_handler, [wiggle_user_handler]},
                         {<<"/api/:version/sessions/[...]">>,
                          wiggle_rest_handler, [wiggle_session_handler]},
                         {<<"/api/:version/roles/[...]">>,
                          wiggle_rest_handler, [wiggle_role_handler]},
                         {<<"/api/:version/orgs/[...]">>,
                          wiggle_rest_handler, [wiggle_org_handler]},
                         {<<"/api/:version/cloud/[...]">>,
                          wiggle_rest_handler, [wiggle_cloud_handler]},
                         {<<"/api/:version/hypervisors/[...]">>,
                          wiggle_rest_handler, [wiggle_hypervisor_handler]},
                         {<<"/api/:version/dtrace/:uuid/stream">>,
                          wiggle_dtrace_stream, []},
                         {<<"/api/:version/dtrace/[...]">>,
                          wiggle_rest_handler, [wiggle_dtrace_handler]},
                         {<<"/api/:version/vms/:uuid/console">>,
                          wiggle_console_handler, []},
                         {<<"/api/:version/vms/:uuid/vnc">>,
                          wiggle_vnc_handler, []},
                         {<<"/api/:version/vms/[...]">>,
                          wiggle_rest_handler, [wiggle_vm_handler]},
                         {<<"/api/:version/ipranges/[...]">>,
                          wiggle_rest_handler, [wiggle_iprange_handler]},
                         {<<"/api/:version/networks/[...]">>,
                          wiggle_rest_handler, [wiggle_network_handler]},
                         {<<"/api/:version/groupings/[...]">>,
                          wiggle_rest_handler, [wiggle_grouping_handler]},
                         {<<"/api/:version/datasets/[...]">>,
                          wiggle_rest_handler, [wiggle_dataset_handler]},
                         {<<"/api/:version/packages/[...]">>,
                          wiggle_rest_handler, [wiggle_package_handler]}] ++
                       PluginDispatchs
                  }]
                ),

    {ok, _} = cowboy:start_http(http, Acceptors, [{port, Port}],
                                [{compress, Compression},
                                 {env, [{dispatch, Dispatch}]}]),
    case application:get_env(wiggle, ssl) of
        {ok, true} ->
            {ok, SSLPort} = application:get_env(wiggle, ssl_port),
            {ok, SSLCA} = application:get_env(wiggle, ssl_cacertfile),
            {ok, SSLCert} = application:get_env(wiggle, ssl_certfile),
            {ok, SSLKey} = application:get_env(wiggle, ssl_keyfile),
            {ok, _} = cowboy:start_https(https, Acceptors,
                                         [{port, SSLPort},
                                          {cacertfile, SSLCA},
                                          {certfile, SSLCert},
                                          {keyfile, SSLKey}],
                                         [{compress, Compression},
                                          {env, [{dispatch, Dispatch}]}]);
        {ok, spdy} ->
            {ok, SSLPort} = application:get_env(wiggle, ssl_port),
            {ok, SSLCA} = application:get_env(wiggle, ssl_cacertfile),
            {ok, SSLCert} = application:get_env(wiggle, ssl_certfile),
            {ok, SSLKey} = application:get_env(wiggle, ssl_keyfile),
            {ok, _} = cowboy:start_spdy(spdy, Acceptors,
                                        [{port, SSLPort},
                                         {cacertfile, SSLCA},
                                         {certfile, SSLCert},
                                         {keyfile, SSLKey}],
                                        [{compress, Compression},
                                         {env, [{dispatch, Dispatch}]}]);
        _ ->
            ok
    end,
    R = wiggle_sup:start_link(),
    lager_watchdog_srv:set_version(?VERSION),
    wiggle_snmp_handler:start(),
    Schemas = load_schemas(),
    io:format("[schemas] Loaded schemas: ~p~n", [Schemas]),
    lager:info("[schemas] Loaded schemas: ~p", [Schemas]),
    R.

stop(_State) ->
    ok.

load_schemas() ->
    FileRegexp = ".*\.json$",
    filelib:fold_files(
      code:priv_dir(wiggle), FileRegexp, true,
      fun(File, Acc) ->
              io:format("Loading file: ~s~n", [File]),
              BaseName = filename:basename(File),
              Key = list_to_atom(filename:rootname(BaseName)),
              {ok, Bin} = file:read_file(File),
              JSX = jsx:decode(Bin),
              ok = jesse:add_schema(Key, JSX),
              [Key | Acc]
      end, []).

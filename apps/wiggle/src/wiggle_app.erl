-module(wiggle_app).

-behaviour(application).

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
                 [{'_', [{<<"/api/:version/users/[...]">>,
                          wiggle_rest_handler, [wiggle_user_handler]},
                         {<<"/api/:version/sessions/[...]">>,
                          wiggle_rest_handler, [wiggle_session_handler]},
                         {<<"/api/:version/groups/[...]">>,
                          wiggle_rest_handler, [wiggle_group_handler]},
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
    statman_server:add_subscriber(statman_aggregator),
    wiggle_snmp_handler:start(),
    R.

stop(_State) ->
    ok.

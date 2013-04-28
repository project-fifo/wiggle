-module(wiggle_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(wiggle, port),
    {ok, Acceptors} = application:get_env(wiggle, acceptors),

    Dispatch = cowboy_router:compile(
                 [{'_', [{<<"/api/:version/users/[...]">>, wiggle_user_handler, []},
                         {<<"/api/:version/sessions/[...]">>, wiggle_session_handler, []},
                         {<<"/api/:version/groups/[...]">>, wiggle_group_handler, []},
                         {<<"/api/:version/cloud/[...]">>, wiggle_cloud_handler, []},
                         {<<"/api/:version/hypervisors/[...]">>, wiggle_hypervisor_handler, []},
                         {<<"/api/:version/dtrace/:uuid/stream">>, wiggle_dtrace_stream, []},
                         {<<"/api/:version/dtrace/[...]">>, wiggle_dtrace_handler, []},
                         {<<"/api/:version/vms/:uuid/console">>, wiggle_console_handler, []},
                         {<<"/api/:version/vms/:uuid/vnc">>, wiggle_vnc_handler, []},
                         {<<"/api/:version/vms/[...]">>, wiggle_vm_handler, []},
                         {<<"/api/:version/ipranges/[...]">>, wiggle_iprange_handler, []},
                         {<<"/api/:version/datasets/[...]">>, wiggle_dataset_handler, []},
                         {<<"/api/:version/packages/[...]">>, wiggle_rest_handler, [wiggle_package_handler]}]}]
                ),
    {ok, _} = cowboy:start_http(http, Acceptors, [{port, Port}],
                                [{env, [{dispatch, Dispatch}]}]),

    R = wiggle_sup:start_link(),
    statman_server:add_subscriber(statman_aggregator),
    wiggle_snmp_handler:start(),
    otp_mib:load(snmp_master_agent),
    case application:get_env(newrelic,license_key) of
        undefined ->
            ok;
        _ ->
            newrelic_poller:start_link(fun newrelic_statman:poll/0)
    end,
    R.


stop(_State) ->
    ok.

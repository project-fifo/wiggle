-module(wiggle_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(wiggle, port),
    {ok, SSLPort} = application:get_env(wiggle, ssl_port),
    {ok, Acceptors} = application:get_env(wiggle, acceptors),

    Dispatch = [{'_', [{[<<"api">>, '_', <<"users">>, '...'], wiggle_user_handler, []},
                       {[<<"api">>, '_', <<"groups">>, '...'], wiggle_group_handler, []},
                       {[<<"api">>, '_', <<"cloud">>, '...'], wiggle_cloud_handler, []},
                       {[<<"api">>, '_', <<"hypervisors">>, '...'], wiggle_hypervisor_handler, []},
                       {[<<"api">>, '_', <<"vms">>, '_', <<"console">>], wiggle_console_handler, []},
                       {[<<"api">>, '_', <<"vms">>, '_', <<"vnc">>], wiggle_vnc_handler, []},
                       {[<<"api">>, '_', <<"vms">>, '...'], wiggle_vm_handler, []},
                       {[<<"api">>, '_', <<"ipranges">>, '...'], wiggle_iprange_handler, []},
                       {[<<"api">>, '_', <<"datasets">>, '...'], wiggle_dataset_handler, []},
                       {[<<"api">>, '_', <<"packages">>, '...'], wiggle_package_handler, []}]}],
    {ok, _} = cowboy:start_listener(http, Acceptors,
                                    cowboy_tcp_transport, [{port, Port}],
                                    cowboy_http_protocol, [{dispatch, Dispatch}]
                                   ),

    {ok, _} = cowboy:start_listener(https, Acceptors,
                                    cowboy_ssl_transport, [{port, SSLPort}
                                                           {certfile, "/var/db/fifo/project-fifo.net.crt"},
                                                           {keyfile, "/var/db/fifo/project-fifo.net.key"},
                                                          ],
                                    cowboy_http_protocol, [{dispatch, Dispatch}]
                                   ),
    wiggle_sup:start_link().

stop(_State) ->
    ok.

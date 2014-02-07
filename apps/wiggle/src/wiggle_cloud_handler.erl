-module(wiggle_cloud_handler).

-include("wiggle_version.hrl").
-include("wiggle.hrl").

-export([allowed_methods/3,
         permission_required/1,
         get/1,
         read/2]).

-ignore_xref([allowed_methods/3,
              permission_required/1,
              get/1,
              read/2]).

allowed_methods(_Version, _Token, [<<"connection">>]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>].

get(_) ->
    {ok, undefined}.

permission_required(#state{path = [<<"connection">>]}) ->
    {ok, always};

permission_required(#state{path = []}) ->
    {ok, [<<"cloud">>, <<"cloud">>, <<"status">>]};

permission_required(_State) ->
    undefined.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------
read(Req, State = #state{path = [<<"connection">>]}) ->
    Res = jsxd:thread([{set, <<"sniffle">>, length(libsniffle:servers())},
                       {set, <<"snarl">>, length(libsnarl:servers())},
                       {set, <<"howl">>, length(libhowl:servers())}],
                      []),
    {Res, Req, State};

read(Req, State = #state{path = []}) ->
    {Versions1, Metrics1, Warnings1} =
        case libsnarl:status() of
            {ok, {MetricsSna, WarningsSna}} ->
                case libsnarl:version() of
                    {ok, SnaVer} when is_binary(SnaVer) ->
                        {[{snarl, SnaVer}], MetricsSna, WarningsSna};
                    _ ->
                        {[], MetricsSna, [{<<"snarl">>, <<"down(version)">>} | WarningsSna]}
                end;
            _ ->
                {[], [], [{<<"snarl">>, <<"down(status)">>}]}
        end,
    {Versions2, Metrics2, Warnings2} =
        case libhowl:version() of
            {ok, HowlVer} when is_binary(HowlVer) ->
                {[{howl, HowlVer} | Versions1], Metrics1, Warnings1};
            _ ->
                {Versions1, Metrics1, [{<<"howl">>, <<"down">>} | Warnings1]}
        end,
    {Versions3, Metrics3, Warnings3} =
        case libsniffle:cloud_status() of
            {ok, {MetricsSni, WarningsSni}} ->
                case libsniffle:version() of
                    {ok, SniVer} when is_binary(SniVer) ->
                        {ok, Vms} = libsniffle:vm_list(),
                        {[{sniffle, SniVer} | Versions2],
                         [{<<"vms">>, length(Vms)} | MetricsSni] ++ Metrics2,
                         WarningsSni ++ Warnings2};
                    _ ->
                        {Versions2, MetricsSni ++ Metrics2,
                         WarningsSni ++ [{<<"sniffle">>, <<"down">>} | Warnings2]}
                end;
            _ ->
                {Versions2, Metrics2,
                 [{<<"sniffle">>, <<"down">>} | Warnings2]}
        end,
    {[{versions, [{wiggle, ?VERSION} | Versions3]},
      {metrics,  Metrics3},
      {warnings, Warnings3}], Req, State}.

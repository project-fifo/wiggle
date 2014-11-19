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
    Res = [
           {<<"sniffle">>, length(libsniffle:servers())},
           {<<"snarl">>, length(libsnarl:servers())},
           {<<"howl">>, length(libhowl:servers())}
          ],
    {Res, Req, State};

read(Req, State = #state{path = []}) ->
    {Versions1, Metrics1, Warnings1} =
        case {libsnarl:version(), libsnarl:status()} of
            {{ok, SnaVer}, {ok, {MetricsSna, WarningsSna}}}
              when is_binary(SnaVer) ->
                {[{snarl, SnaVer}], MetricsSna, WarningsSna};
            _ ->
                {[], [], [[{<<"category">>, <<"snarl">>},
                           {<<"element">>, <<"all">>},
                           {<<"message">>, <<"The Snarl subsystem could not be reached.">>}
                          ]]}
        end,
    {Versions2, Metrics2, Warnings2} =
        case libhowl:version() of
            {ok, HowlVer} when is_binary(HowlVer) ->
                {[{howl, HowlVer} | Versions1], Metrics1, Warnings1};
            _ ->
                {Versions1, Metrics1, [[{<<"category">>, <<"howl">>},
                                        {<<"element">>, <<"all">>},
                                        {<<"message">>, <<"The Howl subsystem could not be reached.">>}
                                       ] | Warnings1]}
        end,
    {Versions3, Metrics3, Warnings3} =
        case {libsniffle:version(), libsniffle:cloud_status()} of
            {{ok, SniVer}, {ok, {MetricsSni, WarningsSni}}}
              when is_binary(SniVer) ->
                {ok, Vms} = ls_vm:list(),
                {[{sniffle, SniVer} | Versions2],
                 [{<<"vms">>, length(Vms)} | MetricsSni] ++ Metrics2,
                 WarningsSni ++ Warnings2};
            _ ->
                {Versions2, Metrics2,
                 [[{<<"category">>, <<"sniffle">>},
                   {<<"element">>, <<"all">>},
                   {<<"message">>, <<"The Sniffle subsystem could not be reached.">>}
                  ] | Warnings2]}
        end,
    {[{versions, [{wiggle, ?VERSION} | Versions3]},
      {metrics,  Metrics3},
      {warnings, Warnings3}], Req, State}.

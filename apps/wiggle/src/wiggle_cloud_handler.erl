-module(wiggle_cloud_handler).

-include("wiggle_version.hrl").
-include("wiggle.hrl").

-export([allowed_methods/3,
         permission_required/1,
         handle_request/2]).

-ignore_xref([allowed_methods/3,
              permission_required/1,
              handle_request/2]).

allowed_methods(_Version, _Token, [<<"connection">>]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>].

permission_required(#state{path = [<<"connection">>]}) ->
    {ok, always};

permission_required(#state{path = []}) ->
    {ok, [<<"cloud">>, <<"cloud">>, <<"status">>]};

permission_required(_State) ->
    undefined.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

handle_request(Req, State = #state{path = [<<"connection">>]}) ->
    Res = jsxd:thread([{set, <<"sniffle">>, length(libsniffle:servers())},
                       {set, <<"snarl">>, length(libsnarl:servers())},
                       {set, <<"howl">>, length(libhowl:servers())}],
                      []),
    {Res, Req, State};

handle_request(Req, State = #state{path = []}) ->
    Start = now(),
    case libsniffle:cloud_status() of
        {ok, {Metrics, Warnings}} ->
            ?MSniffle(?P(State), Start),
            Start1 = now(),
            Vers0 = case libsnarl:version() of
                        {ok, SrvVer} when is_binary(SrvVer) ->
                            [{snarl, SrvVer}];
                        _ ->
                            [{snarl, <<"not connected">>}]
                    end,
            ?MSnarl(?P(State), Start1),
            Start2 = now(),
            Vers1 = case libsniffle:version() of
                        {ok, SrvVer1} when is_binary(SrvVer1) ->
                            [{sniffle, SrvVer1} | Vers0];
                        _ ->
                            [{sniffle, <<"not connected">>} | Vers0]
                    end,
            ?MSniffle(?P(State), Start2),
            Start3 = now(),
            Vers2 = case libhowl:version() of
                        {ok, SrvVer2} when is_binary(SrvVer2) ->
                            [{howl, SrvVer2} | Vers1];
                        _ ->
                            [{howl, <<"not connected">>} | Vers1]
                    end,
            ?MHowl(?P(State), Start3),
            Start4 = now(),
            {ok, Users} = libsnarl:user_list(),
            ?MSnarl(?P(State), Start4),
            Start5 = now(),
            {ok, Vms} = libsniffle:vm_list(),
            ?MSniffle(?P(State), Start5),
            {[{versions, [{wiggle, ?VERSION} | Vers2]},
              {metrics, [{<<"users">>, length(Users)},
                         {<<"vms">>, length(Vms)} |
                         Metrics]},
              {warnings, Warnings}], Req, State};
        _ ->
            {[{warnings, [{cloud, <<"down!">>}]}], Req, State}
    end.

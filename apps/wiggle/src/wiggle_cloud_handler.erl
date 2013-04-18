%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_cloud_handler).

-include("wiggle_version.hrl").

-export([init/3,
         rest_init/2]).

-export([content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         resource_exists/2,
         forbidden/2,
         options/2,
         service_available/2,
         is_authorized/2]).

-export([to_json/2,
         from_json/2,
         to_msgpack/2,
         from_msgpack/2]).

-ignore_xref([to_json/2,
              from_json/2,
              from_msgpack/2,
              to_msgpack/2,
              allowed_methods/2,
              content_types_accepted/2,
              content_types_provided/2,
              forbidden/2,
              init/3,
              is_authorized/2,
              service_available/2,
              options/2,
              resource_exists/2,
              rest_init/2]).

-record(state, {path, method, version, token, content, reply, obj, body}).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) ->
    wiggle_handler:initial_state(Req).

service_available(Req, State = #state{path = [<<"connection">>]}) ->
    {true, Req, State};

service_available(Req, State) ->
    case {libsniffle:servers(), libsnarl:servers()} of
        {[], _} ->
            {false, Req, State};
        {_, []} ->
            {false, Req, State};
        _ ->
            {true, Req, State}
    end.

options(Req, State) ->
    Methods = allowed_methods(State#state.version, State#state.token, State#state.path),
    Req1 = cowboy_req:set_resp_header(
             <<"access-control-allow-methods">>,
             string:join(
               lists:map(fun erlang:binary_to_list/1,
                         [<<"HEAD">>, <<"OPTIONS">> | Methods]), ", "), Req),
    {ok, Req1, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json},
      {<<"application/x-msgpack">>, to_msgpack}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {wiggle_handler:accepted(), Req, State}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"OPTIONS">> | allowed_methods(State#state.version, State#state.token, State#state.path)], Req, State}.

allowed_methods(_Version, _Token, [<<"connection">>]) ->
    [<<"GET">>];

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>].

resource_exists(Req, State = #state{path = []}) ->
    {true, Req, State};

resource_exists(Req, State = #state{path = [<<"connection">>]}) ->
    {true, Req, State}.

is_authorized(Req, State = #state{method = <<"OPTIONS">>}) ->
    {true, Req, State};

is_authorized(Req, State = #state{path = [<<"connection">>]}) ->
    {true, Req, State};

is_authorized(Req, State = #state{token = undefined}) ->
    {{false, <<"x-snarl-token">>}, Req, State};

is_authorized(Req, State) ->
    {true, Req, State}.

forbidden(Req, State = #state{method = <<"OPTIONS">>}) ->
    {false, Req, State};

forbidden(Req, State = #state{path = [<<"connection">>]}) ->
    {false, Req, State};

forbidden(Req, State = #state{token = undefined}) ->
    {true, Req, State};

forbidden(Req, State = #state{path = []}) ->
    {allowed(State#state.token, [<<"cloud">>, <<"cloud">>, <<"status">>]), Req, State};

forbidden(Req, State) ->
    {true, Req, State}.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

to_json(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {jsx:encode(Reply), Req1, State1}.

to_msgpack(Req, State) ->
    {Reply, Req1, State1} = handle_request(Req, State),
    {msgpack:pack(Reply, [jsx]), Req1, State1}.

handle_request(Req, State = #state{path = [<<"connection">>]}) ->
    Res = jsxd:thread([{set, <<"sniffle">>, length(libsniffle:servers())},
                       {set, <<"snarl">>, length(libsnarl:servers())},
                       {set, <<"howl">>, length(libhowl:servers())}],
                      []),
    {Res, Req, State};

handle_request(Req, State = #state{path = []}) ->
    case libsniffle:cloud_status() of
        {ok, {Metrics, Warnings}} ->
            Vers0 = case libsnarl:version() of
                        SrvVer when is_binary(SrvVer) ->
                            [{snarl, SrvVer}];
                        _ ->
                            [{snarl, <<"not connected">>}]
                    end,
            Vers1 = case libsniffle:version() of
                       SrvVer1 when is_binary(SrvVer1) ->
                            [{sniffle, SrvVer1} | Vers0];
                        _ ->
                            [{sniffle, <<"not connected">>} | Vers0]
                    end,
            Vers2 = case libhowl:version() of
                        SrvVer2 when is_binary(SrvVer2) ->
                            [{howl, SrvVer2} | Vers1];
                        _ ->
                            [{howl, <<"not connected">>} | Vers1]
                    end,
            {ok, Users} = libsnarl:user_list(),
            {ok, Vms} = libsniffle:vm_list(),
            {[{versions, [{wiggle, ?VERSION} | Vers2]},
              {metrics, [{<<"users">>, length(Users)},
                         {<<"vms">>, length(Vms)} |
                         Metrics]},
              {warnings, Warnings}], Req, State};
        _ ->
            {[{warnings, [{cloud, <<"down!">>}]}], Req, State}
    end.


%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

from_json(Req, State) ->
    {false, Req, State}.

from_msgpack(Req, State) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

allowed(Token, Perm) ->
    case libsnarl:allowed({token, Token}, Perm) of
        not_found ->
            true;
        true ->
            false;
        false ->
            true
    end.

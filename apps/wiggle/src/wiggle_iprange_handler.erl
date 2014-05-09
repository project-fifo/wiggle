%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(wiggle_iprange_handler).
-include("wiggle.hrl").

-export([allowed_methods/3,
         get/1,
         permission_required/1,
         read/2,
         create/3,
         write/3,
         delete/2]).

-ignore_xref([allowed_methods/3,
              get/1,
              permission_required/1,
              read/2,
              create/3,
              write/3,
              delete/2]).

allowed_methods(_Version, _Token, [_Iprange, <<"metadata">>|_]) ->
    [<<"PUT">>, <<"DELETE">>];

allowed_methods(_Version, _Token, []) ->
    [<<"GET">>, <<"POST">>];

allowed_methods(_Version, _Token, [_Iprange]) ->
    [<<"GET">>, <<"PUT">>, <<"DELETE">>].

get(State = #state{path = [Iprange | _]}) ->
    Start = now(),
    R = libsniffle:iprange_get(Iprange),
    ?MSniffle(?P(State), Start),
    R.

permission_required(#state{method = <<"GET">>, path = []}) ->
    {ok, [<<"cloud">>, <<"ipranges">>, <<"list">>]};

permission_required(#state{method = <<"POST">>, path = []}) ->
    {ok, [<<"cloud">>, <<"ipranges">>, <<"create">>]};

permission_required(#state{method = <<"GET">>, path = [Iprange]}) ->
    {ok, [<<"ipranges">>, Iprange, <<"get">>]};

permission_required(#state{method = <<"DELETE">>, path = [Iprange]}) ->
    {ok, [<<"ipranges">>, Iprange, <<"delete">>]};

permission_required(#state{method = <<"PUT">>, path = [_Iprange]}) ->
    {ok, [<<"cloud">>, <<"ipranges">>, <<"create">>]};

permission_required(#state{method = <<"PUT">>, path = [Iprange, <<"metadata">> | _]}) ->
    {ok, [<<"ipranges">>, Iprange, <<"edit">>]};

permission_required(#state{method = <<"DELETE">>, path = [Iprange, <<"metadata">> | _]}) ->
    {ok, [<<"ipranges">>, Iprange, <<"edit">>]};

permission_required(_State) ->
    undefined.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------

read(Req, State = #state{token = Token, path = [], full_list=FullList, full_list_fields=Filter}) ->
    Start = now(),
    {ok, Permissions} = wiggle_handler:get_persmissions(Token),
    ?MSnarl(?P(State), Start),
    Start1 = now(),
    {ok, Res} = libsniffle:iprange_list([{must, 'allowed', [<<"ipranges">>, {<<"res">>, <<"uuid">>}, <<"get">>], Permissions}], FullList),
    ?MSnarl(?P(State), Start1),
    Res1 = case {Filter, FullList} of
               {_, false} ->
                   [ID || {_, ID} <- Res];
               {[], _} ->
                   [to_json(Obj) || {_, Obj} <- Res];
               _ ->
                   [jsxd:select(Filter, to_json(Obj)) || {_, Obj} <- Res]
           end,
    {Res1, Req, State};

read(Req, State = #state{path = [_Iprange], obj = Obj}) ->
    {to_json(Obj), Req, State}.

to_json(Obj) ->
    jsxd:thread([{update, <<"network">>, fun ip_to_str/1},
                 {update, <<"gateway">>, fun ip_to_str/1},
                 {update, <<"netmask">>, fun ip_to_str/1},
                 {update, <<"first">>, fun ip_to_str/1},
                 {update, <<"last">>, fun ip_to_str/1},
                 {update, <<"current">>, fun ip_to_str/1},
                 {update, <<"free">>,
                  fun (Free) ->
                          lists:map(fun ip_to_str/1, Free)
                  end}], Obj).
%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------

create(Req, State = #state{path = [], version = Version}, Data) ->
    {ok, Iprange} = jsxd:get(<<"name">>, Data),
    {ok, Network} = jsxd:get(<<"network">>, Data),
    {ok, Gateway} = jsxd:get(<<"gateway">>, Data),
    {ok, Netmask} = jsxd:get(<<"netmask">>, Data),
    {ok, First} = jsxd:get(<<"first">>, Data),
    {ok, Last} = jsxd:get(<<"last">>, Data),
    {ok, Tag} = jsxd:get(<<"tag">>, Data),
    Vlan = jsxd:get(<<"vlan">>, 0, Data),
    Start = now(),
    case libsniffle:iprange_create(Iprange, Network, Gateway, Netmask, First, Last, Tag, Vlan) of
        {ok, UUID} ->
            ?MSniffle(?P(State), Start),
            {{true, <<"/api/", Version/binary, "/ipranges/", UUID/binary>>}, Req, State#state{body = Data}};
        duplicate ->
            ?MSniffle(?P(State), Start),
            {ok, Req1} = cowboy_req:reply(409, Req),
            {halt, Req1, State}
    end.

%% TODO : This is a icky case it is called after post.
write(Req, State = #state{method = <<"POST">>, path = []}, _) ->
    {true, Req, State};

write(Req, State = #state{path = [Iprange, <<"metadata">> | Path]}, [{K, V}]) ->
    Start = now(),
    libsniffle:iprange_set(Iprange, [<<"metadata">> | Path] ++ [K], jsxd:from_list(V)),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

write(Req, State, _Body) ->
    {false, Req, State}.

%%--------------------------------------------------------------------
%% DEETE
%%--------------------------------------------------------------------

delete(Req, State = #state{path = [Iprange, <<"metadata">> | Path]}) ->
    Start = now(),
    libsniffle:iprange_set(Iprange, [<<"metadata">> | Path], delete),
    ?MSniffle(?P(State), Start),
    {true, Req, State};

delete(Req, State = #state{path = [Iprange]}) ->
    Start = now(),
    ok = libsniffle:iprange_delete(Iprange),
    ?MSniffle(?P(State), Start),
    {true, Req, State}.

ip_to_str(Ip) ->
    <<A:8, B:8, C:8, D:8>> = <<Ip:32>>,
    list_to_binary(io_lib:format("~p.~p.~p.~p", [A, B, C, D])).

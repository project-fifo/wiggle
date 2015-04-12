-record(state, {
          module,
          %% The path of the reqest
          path,
          %% The method of the request
          method,
          %% The API version
          version,
          %% The token (either {token, ...} or user uuid)
          token,
          %% The object the reuqest is asking for (from the DB)
          obj,
          %% Body of the request (from the client)
          body,
          %% When the request was started.
          start,
          %% The whole path as a binary
          path_bin,
          %% The ETAG when generated
          etag,
          %% The bearer token when OAuth is used
          bearer,
          %% A cached set of permissons
          cached_perms,
          %% The permissions granted by the OAuth2 scope.
          %% If we don't have a scope aka don't use oatuh2 we always allow
          %% everything from a scope pov.
          scope_perms = [[<<"...">>]],
          %% Te full list header
          full_list = false,
          %% The full list fields
          full_list_fields=[]
         }).

-define(P(State), State#state.path_bin).
-define(MEx(Path, Service, Start), io_lib:format("~p~p", [Path, Start])).
%%-define(MEx(Path, Service, Start),
%%        statman_histogram:record_value({Path, {ext, Service}}, Start)).
-define(MSnarl(Path, Start), ?MEx(Path, <<"snarl">>, Start)).
-define(MSniffle(Path, Start), ?MEx(Path, <<"sniffle">>, Start)).
-define(MHowl(Path, Start), ?MEx(Path, <<"howl">>, Start)).
-define(M(Path, Start), ok).
-define(UUID(N), <<N:36/binary>>).

%-define(M(Path, Start), statman_histogram:record_value({Path, total}, Start)).
%-define(M(Path, Start), statman_histogram:record_value({Path, total}, Start)).

-define(V1, <<"0.1.0">>).
-define(V2, <<"0.2.0">>).

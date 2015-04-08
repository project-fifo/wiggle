%% This is dity hack, the 'a' record is nowhere exposed over an API
%% so we need to copy it from (a much too long url)
%% https://github.com/kivra/oauth2/blob/9dcabbbc9d749df5f78f41f1dc0ffbd711f10bf8/src/oauth2.erl#L59
-record(a, { client   = undefined
           , resowner = undefined
           , scope
           , ttl      = 0
           }).

-record(auth_code_resp, {
          authorization,
          state,
          uri
         }).

-record(access_token_resp, {
          authorization,
          state,
          uri
         }).

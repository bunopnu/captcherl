-module(captcherl).
-export([start/0]).

-define(APPLICATION, captcherl).
-define(DEPENDS_APP, [inets, ssl, jsone]).


-spec start() -> ok | {error, Reason}
              when Reason :: term().
start() ->
    _ = [ application:start(A) || A <- ?DEPENDS_APP ],
    application:start(?APPLICATION).

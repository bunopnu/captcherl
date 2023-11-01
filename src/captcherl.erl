-module(captcherl).

-define(APPLICATION, captcherl).
-define(DEPENDS_APP, [inets, ssl, jsone]).

-define(URL_TURNSTILE, "https://challenges.cloudflare.com/turnstile/v0/siteverify").
-define(URL_RECAPTCHA, "https://www.google.com/recaptcha/api/siteverify").
-define(URL_HCAPTCHA,  "https://api.hcaptcha.com/siteverify").

-export_type([captcha_services/0, request_response/0, api_request_data/0]).
-export([start/0, request/2, verify/2]).

-type captcha_services() :: turnstile | recaptcha | hcaptcha.
-type request_response() :: {ok, map()} | {error, term()}.
-type api_request_data() :: {Secret :: unicode:chardata(), Response :: unicode:chardata()} |
                            {Secret :: unicode:chardata(), Response :: unicode:chardata(), IpAddress :: unicode:chardata()}.


-spec start() -> ok | {error, term()}.
start() ->
    _ = [ application:start(A) || A <- ?DEPENDS_APP ],
    application:start(?APPLICATION).


-spec request(Service, Data) -> Result
              when Service :: captcha_services(),
                   Data :: api_request_data(),
                   Result :: request_response().
request(turnstile, Data) ->
    captcherl_base:request(?URL_TURNSTILE, Data);
request(recaptcha, Data) ->
    captcherl_base:request(?URL_RECAPTCHA, Data);
request(hcaptcha, Data) ->
    captcherl_base:request(?URL_HCAPTCHA, Data).


-spec verify(Service, Data) -> Result
              when Service :: captcha_services(),
                   Data :: api_request_data(),
                   Result :: boolean().
verify(Service, Data) ->
    case request(Service, Data) of
        {ok, #{<<"success">> := true}} ->
            true;
        _ ->
            false
    end.

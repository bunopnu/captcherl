%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
%%% @doc Simplifies the process of verifying CAPTCHA responses.
%%%
%%% Erlang library that provides easy-to-use functions for interacting with
%%% <b>Cloudflare Turnstile</b>, <b>reCAPTCHA</b> and <b>hCaptcha</b>.
%%%
%%% @author bunopnu
%%% @end
%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
-module(captcherl).

%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
%%%
%%%   Exports
%%%
%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~

-export_type([captcha_services/0, request_response/0, api_request_data/0]).
-export([start/0, request/3, request/2, verify/2]).

%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
%%%
%%%   Macros
%%%
%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~

-define(APPLICATION, captcherl).
-define(DEPENDS_APP, [inets, ssl, jsone]).

-define(URL_TURNSTILE, "https://challenges.cloudflare.com/turnstile/v0/siteverify").
-define(URL_RECAPTCHA, "https://www.google.com/recaptcha/api/siteverify").
-define(URL_HCAPTCHA,  "https://api.hcaptcha.com/siteverify").

%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
%%%
%%%   Public Types
%%%
%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~

-type captcha_services() :: turnstile | recaptcha | hcaptcha.
%% Representing the available CAPTCHA services.
-type request_response() :: {ok, map()} | {error, term()}.
%% Representing the response from a CAPTCHA service request.
-type api_request_data() :: {Secret :: binary(), Response :: binary()} |
                            {Secret :: binary(), Response :: binary(), IpAddress :: binary()}.
%% Representing the data required for making a request.

%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
%%%
%%%   Public Functions
%%%
%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~


%% ---------------------------------------------------------------------------
%% @doc Starts the application along with its dependent applications.
%% @end
%% ---------------------------------------------------------------------------
-spec start() -> ok | {error, term()}.
start() ->
    _ = [ application:start(A) || A <- ?DEPENDS_APP ],
    application:start(?APPLICATION).


%% ---------------------------------------------------------------------------
%% @doc Sends a request to the specified CAPTCHA service with HTTP options.
%%
%% The function behaviour remains the same as {@link request/2},
%% but it additionally allows you to pass custom HTTP options in the
%% `Opts' parameter.
%%
%% You can find more information about HTTP options from
%% <a href="https://www.erlang.org/doc/man/httpc#request-5" target="_blank">
%% httpc documentation</a>.
%%
%% @end
%% ---------------------------------------------------------------------------
-spec request(Service, Data, Opts) -> Result
              when Service :: captcha_services(),
                   Data :: api_request_data(),
                   Opts :: list(),
                   Result :: request_response().
request(turnstile, Data, Opts) ->
    captcherl_base:request(?URL_TURNSTILE, Data, Opts);
request(recaptcha, Data, Opts) ->
    captcherl_base:request(?URL_RECAPTCHA, Data, Opts);
request(hcaptcha, Data, Opts) ->
    captcherl_base:request(?URL_HCAPTCHA, Data, Opts).


%% ---------------------------------------------------------------------------
%% @doc Sends a request to the specified CAPTCHA service.
%%
%% == Parameters ==
%% <dl>
%%   <dt>Service</dt>
%%   <dd>The CAPTCHA service to send the request to.</dd>
%%   <dt>Data</dt>
%%   <dd>The request data to be sent to the CAPTCHA service.</dd>
%% </dl>
%%
%% == Returns ==
%% A map containing the response from the CAPTCHA service, which can have
%% different fields and values depending on the service.
%%
%% @end
%% ---------------------------------------------------------------------------
-spec request(Service, Data) -> Result
              when Service :: captcha_services(),
                   Data :: api_request_data(),
                   Result :: request_response().
request(Service, Data) ->
    Opts = [{ssl, [{verify, verify_peer},
                   {cacerts, public_key:cacerts_get()},
                   {customize_hostname_check, [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}]}],
    request(Service, Data, Opts).


%% ---------------------------------------------------------------------------
%% @doc Verifies the CAPTCHA response from the specified service.
%%
%% == Returns ==
%% A boolean representation of the verification status.
%%
%% @end
%% ---------------------------------------------------------------------------
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

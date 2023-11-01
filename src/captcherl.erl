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
-export([start/0, request/2, verify/2]).

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
-type api_request_data() :: {Secret :: unicode:chardata(), Response :: unicode:chardata()} |
                            {Secret :: unicode:chardata(), Response :: unicode:chardata(), IpAddress :: unicode:chardata()}.
%% Representing the data required for making a request.

%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
%%%
%%%   Public Functions
%%%
%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~


%% ---------------------------------------------------------------------------
%% @doc
%%
%% Starts the application along with its dependent applications.
%%
%% @end
%% ---------------------------------------------------------------------------
-spec start() -> ok | {error, term()}.
start() ->
    _ = [ application:start(A) || A <- ?DEPENDS_APP ],
    application:start(?APPLICATION).


%% ---------------------------------------------------------------------------
%% @doc
%%
%% Sends a request to the specified CAPTCHA service and returns the response.
%%
%% @end
%% ---------------------------------------------------------------------------
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


%% ---------------------------------------------------------------------------
%% @doc
%%
%% Verifies the CAPTCHA response from the specified service and returns a
%% boolean result.
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

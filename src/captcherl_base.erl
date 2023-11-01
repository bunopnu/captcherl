%%% @private
-module(captcherl_base).

%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
%%%
%%%   Exports
%%%
%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~

-export([request/2]).

%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
%%%
%%%   Public Functions
%%%
%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~


-spec request(Url, Data) -> Result
              when Url :: uri_string:uri_string(),
                   Data :: catpcherl:api_request_data(),
                   Result :: captcherl:request_response().
request(Url, Data) ->
    RequestData = build_request_data(Data),
    Request = build_request(Url, RequestData),
    Response = apply_request(Request),
    process_response(Response).


%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
%%%
%%%   Private Functions
%%%
%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~


build_request_data({Secret, Response, IpAddress}) ->
    uri_string:compose_query([{"secret", Secret},
                              {"response", Response},
                              {"remoteip", IpAddress}]);
build_request_data({Secret, Response}) ->
    uri_string:compose_query([{"secret", Secret},
                              {"response", Response}]).


build_request(Url, Body) ->
    Headers = [{"accept", "application/json"}],
    ContentType = "application/x-www-form-urlencoded",
    {Url, Headers, ContentType, Body}.


apply_request(Request) ->
    Opts = [{ssl, [{verify, verify_peer},
                   {cacerts, public_key:cacerts_get()},
                   {customize_hostname_check, [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}]}],
    httpc:request(post, Request, Opts, []).


process_response({ok, {_, _, Body}}) ->
    Data = jsone:decode(list_to_binary(Body)),
    {ok, Data};
process_response({error, Reason}) ->
    {error, Reason};
process_response(Other) ->
    {error, {unknown, Other}}.

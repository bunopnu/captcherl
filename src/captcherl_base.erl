%%% @private
-module(captcherl_base).

%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
%%%
%%%   Exports
%%%
%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~

-export([request/3]).

%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
%%%
%%%   Public Functions
%%%
%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~


-spec request(Url, Data, Opts) -> Result
              when Url :: uri_string:uri_string(),
                   Data :: catpcherl:api_request_data(),
                   Opts :: list(),
                   Result :: captcherl:request_response().
request(Url, Data, Opts) ->
    RequestData = build_request_data(Data),
    Request = build_request(Url, RequestData),
    Response = apply_request(Request, Opts),
    process_response(Response).


%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
%%%
%%%   Private Functions
%%%
%%% ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~


build_request_data({Secret, Response})
  when is_binary(Secret), is_binary(Response) ->
    uri_string:compose_query([{"secret", Secret},
                              {"response", Response}]);
build_request_data({Secret, Response, IpAddress})
  when is_binary(Secret), is_binary(Response), is_binary(IpAddress) ->
    uri_string:compose_query([{"secret", Secret},
                              {"response", Response},
                              {"remoteip", IpAddress}]).


build_request(Url, Body) ->
    Headers = [{"accept", "application/json"}],
    ContentType = "application/x-www-form-urlencoded",
    {Url, Headers, ContentType, Body}.


apply_request(Request, Opts) ->
    httpc:request(post, Request, Opts, []).


process_response({ok, {_, _, Body}}) ->
    Data = jsone:decode(list_to_binary(Body)),
    {ok, Data};
process_response({error, Reason}) ->
    {error, Reason};
process_response(Other) ->
    {error, {unknown, Other}}.

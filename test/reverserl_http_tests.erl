-module(reverserl_http_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

setup() ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    ok = application:start(reverserl),
    % This one is for the HTTP client used in the tests
    application:start(inets).

teardown(_) ->
    application:stop(inets),
    ok = application:stop(reverserl),
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto).

%% This describes a fixture with arguments such as setup/teardown
reverserl_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
         ?_test(simple_session_check())
     ]
    }.

simple_session_check() ->
    % Start by creating a new session
    {ok, {{_HttpVersion, 201, _Reason}, _Headers, Body}} = httpc:request(post, {"http://127.0.0.1:8080/reverserl", [], [], []}, [], []),
    SessionId = lists:flatten(Body),

    % The SessionId should not be empty
    false = [] =:= SessionId,

    % Now get ready to query the service
    String = "allo",
    {ok, {{_, 200, _}, _, Body2}} = httpc:request(get, {"http://127.0.0.1:8080/reverserl/" ++ SessionId ++ "?string=" ++ String, []}, [], []),
    "olla" = lists:flatten(Body2),

    % Then close the session
    {ok, {{_, 204, _}, _, []}} = httpc:request(delete, {"http://127.0.0.1:8080/reverserl/" ++ SessionId, []}, [], []),

    % The session should be truly closed - i.e. cannot be found anymore in reverserl_server
    error = reverserl_server:reverse(SessionId, ""),
    0 = reverserl_server:active_sessions(),
    1 = reverserl_server:sessions_serviced().


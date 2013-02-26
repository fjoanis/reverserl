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
    {ok, Result} = httpc:request(post, {"http://127.0.0.1:8080/reverserl", [], [], []}, [], []).

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
reverserl_simple_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
         fun() ->
            % Start by creating a new session
            {ok, {{_HttpVersion, 201, _Reason}, _Headers, Body}} = httpc:request('put', {"http://127.0.0.1:8080/reverserl", [], [], []}, [], []),
            SessionId = lists:flatten(Body),

            % The SessionId should not be empty
            false = [] =:= SessionId,

            % Now get ready to query the service
            String = "allo",
            {ok, {{_, 200, _}, _, Body2}} = httpc:request(get, {"http://127.0.0.1:8080/reverserl/" ++ SessionId ++ "?string=" ++ String, []}, [], []),
            "olla" = lists:flatten(Body2),
    
            1 = reverserl_server:active_sessions(),

            % Then close the session
            {ok, {{_, 204, _}, _, []}} = httpc:request(delete, {"http://127.0.0.1:8080/reverserl/" ++ SessionId, []}, [], []),

            % The session should be truly closed - i.e. cannot be found anymore in reverserl_server
            error = reverserl_server:reverse(SessionId, ""),
            0 = reverserl_server:active_sessions(),
            1 = reverserl_server:sessions_serviced()
        end
     ]
    }.

reverserl_multiple_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        fun() ->
            % Start by creating 20 sessions
            F = fun(_, Acc) ->
                {ok, {{_, 201, _}, _, Body}} = httpc:request('put', {"http://127.0.0.1:8080/reverserl", [], [], []}, [], []),
                SessionId = lists:flatten(Body),
                [SessionId|Acc]
            end,
            Sessions = lists:foldl(F, [], lists:seq(1, 20)),

            % Ensure all sessions were created
            20 = reverserl_server:active_sessions(),

            % Now run some queries against the sessions
            lists:foreach(fun(SessionId) ->
                {ok, {{_, 200, _}, _, Body2}} = httpc:request(get, {"http://127.0.0.1:8080/reverserl/" ++ SessionId ++ "?string=allo", []}, [], []),
                "olla" = lists:flatten(Body2)
                end,
                Sessions
            ),
            20 = reverserl_server:sessions_serviced(),

            % Shutdown the sessions
            lists:foreach(fun(SessionId) ->
                {ok, {{_, 204, _}, _, []}} = httpc:request(delete, {"http://127.0.0.1:8080/reverserl/" ++ SessionId, []}, [], [])
                end,
                Sessions
            ),
            0 = reverserl_server:active_sessions()
        end
     ]
    }.

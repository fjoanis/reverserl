-module(reverserl_session_tests).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

setup() ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    ok = application:start(reverserl).

teardown(_) ->
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
         ?_test(simple_session_check()),
         ?_test(simple_session_timeout_check()),
         ?_test(simple_session_timeout_after_op_check()),
         ?_test(prop_string_reversal_check())
     ]
    }.

simple_session_check() ->
    {ok, SessionPid} = reverserl_session:start_link(10000),
    "olla" = reverserl_session:reverse(SessionPid, "allo"),
    ok = reverserl_session:stop(SessionPid).

%% Ensures that once a session is created it times out if
%% nothing happens
simple_session_timeout_check() ->
    erlang:process_flag(trap_exit, true),
    {ok, SessionPid} = reverserl_session:start_link(2000),

    {_, SecsBefore, _} = erlang:now(),
    
    % Because we are trapping exits, an exit message should
    % arrive from the session since it timed out
    receive {'EXIT', SessionPid, _} -> ok
    end,
    
    {_, SecsAfter, _} = erlang:now(),

    2 = SecsAfter - SecsBefore,

    ok = reverserl_session:stop(SessionPid).

simple_session_timeout_after_op_check() ->
    erlang:process_flag(trap_exit, true),
    {ok, SessionPid} = reverserl_session:start_link(2000),

    {_, SecsBefore, _} = erlang:now(),
    
    % Sleep 1 second
    timer:sleep(1000),

    % Do a reverse operation, don't really care about result
    reverserl_session:reverse(SessionPid, "test"),

    % Because we are trapping exits, an exit message should
    % arrive from the session since it timed out
    receive {'EXIT', SessionPid, _} -> ok
    end,
    
    {_, SecsAfter, _} = erlang:now(),

    % Expect 3 seconds to have elapsed
    3 = SecsAfter - SecsBefore,

    ok = reverserl_session:stop(SessionPid).

string() -> list(oneof(lists:seq($a, $z) ++ lists:seq($A, $Z) ++ lists:seq($1, $9))).

prop_string_reversal(SessionPid) ->
    ?FORALL({Input}, {string()},
        begin
            lists:reverse(Input) =:= reverserl_session:reverse(SessionPid, Input)
        end).

prop_string_reversal_check() ->
    {ok, SessionPid} = reverserl_session:start_link(2000),
    true = eqc:quickcheck(prop_string_reversal(SessionPid)),
    ok = reverserl_session:stop(SessionPid).

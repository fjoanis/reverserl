%% @author Francis (Ottawa Erlang)

-module(reverserl_session).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, stop/1, reverse/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Timeout) ->
    gen_server:start_link(?MODULE, [Timeout], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

reverse(Pid, String) ->
    gen_server:call(Pid, {reverse, String}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Timeout]) ->
    io:format("reverserl_session:init~n"),
    erlang:process_flag(trap_exit, true),
    {ok, Timeout, Timeout}.

handle_call({reverse, String}, _From, Timeout) ->
    {reply, do_reverse(String), Timeout, Timeout}.

handle_cast(stop, Timeout) ->
    {stop, normal, Timeout}.

handle_info(timeout, Timeout) ->
    % Timeout detected, terminate the server
    {stop, normal, Timeout}.

terminate(Reason, _Timeout) ->
    io:format("reverserl_session:terminate - Reason: ~p~n", [Reason]),
    ok.

code_change(_OldVsn, Timeout, _Extra) ->
    {ok, Timeout}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_reverse([]) ->
    [];

%% Uncomment this to show a bug that would be caught by QuickCheck
%do_reverse([_, _, _, _, _, _, _]) ->
%    bug;

do_reverse(String) ->
    do_reverse(String, "").

do_reverse([], Result) ->
    Result;
do_reverse([H|T], Result) ->
    do_reverse(T, [H|Result]).


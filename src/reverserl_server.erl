%% @doc This Erlang module represents the core server of the
%% application. It handles incoming requests to create new sessions
%% and will also dispatch requests targetting pre-existing sessions
%% towards their appropriate Erlang processes. This module uses the
%% gen_server behaviour which means that it will behave like what
%% is described here: http://www.erlang.org/doc/man/gen_server.html
%%
%% @author Francis (Ottawa Erlang)

%% Comments start with a single percent sign (%). Double (i.e. %%) are
%% considered a good practice when commenting generic parts of the
%% code but this is most likely strictly cosmetic.

%% Obligatory module declaration. Name of the module must match the
%% file name.
-module(reverserl_server).

%% This indicates that this module will "inherit" the "behaviour"
%% described in gen_server. This is somewhat like a mix of
%% inheritence in OO or templating in C++. 
-behaviour(gen_server).

%% This is a simple macro definition that binds the value of the
%% pre-defined macro ?MODULE (i.e. reverserl_server) to the pre-
%% processed variable SERVER. To use it, use a question mark like
%% ?SERVER.
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

%% Exporting functions make them accesible to other modules. Their
%% arity also needs to be supplied.
%%
%% It is a best practice to group exports that logically belong
%% together on the same export line. In this case, the first one
%% represents the actual APIs that the outside module are expected
%% to call. The second one, further below, contains the gen_server
%% internal callbacks. Note that however this doesn't change anything
%% regarding the accessibility of the functions: they are still all
%% "public".
-export([start_link/0, create_session/0, delete_session/1, reverse/2,
         sessions_serviced/0, active_sessions/0, crash_server/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Start the reverserl_server process. This will also register
%% it with the local name server using the ?SERVER macro. That way,
%% we'll be able to reach it using its name, reverserl_server.
start_link() ->
    % Use the core gen_server API to do the actual start with
    % arguments we control here. 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc API function to create a new session
create_session() ->
    % Note the use of ?SERVER rather than the use of a PID.
    % This is because the process was registered using its name.
    gen_server:call(?SERVER, create_session).

reverse(SessionId, String) ->
    gen_server:call(?SERVER, {reverse, SessionId, String}).

delete_session(SessionId) ->
    gen_server:call(?SERVER, {delete_session, SessionId}).

sessions_serviced() ->
    gen_server:call(?SERVER, sessions_serviced).

active_sessions() ->
    gen_server:call(?SERVER, active_sessions).

crash_server() ->
    gen_server:call(?SERVER, unknown_will_crash).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {sessions_serviced = 0 :: integer(), sessions = undefined :: list({reference(), pid()})}).

%% @doc This function gets called when the server process will be
%% started. It is like its "constructor". The second element of the
%% returned tuple is the state that will be carried through the
%% lifetime of the server. Think of that state as being what would
%% hold the member variables of a classical OO object. This function
%% gets called in the "thread" context of the newly spawned server.
init(_Args) ->
    io:format("Inside reverserl_server's init function~n"),
    
    % This tells the Erlang VM that the current process TODO
    erlang:process_flag(trap_exit, true),

    % Note that, in the line above, the underscore (_) is used
    % prepended to the Args name. This is because this variable is
    % not used and we do not want the compiler to complain about it.
    % _ means "don't care"
    SessionTable = ets:new(sessions, []),

    {ok, #state{sessions = SessionTable}}.


handle_call(sessions_serviced, _From, State) ->
    {reply, State#state.sessions_serviced, State};
handle_call(active_sessions, _From, State) ->
    Result = ets:info(State#state.sessions, size),
    {reply, Result, State};
handle_call(create_session, _From, State) ->
    % Spawn (and link) a new session process
    {ok, SessionPid} = reverserl_session:start_link(30000),
    % Note: the following will not be globally unique so don't go
    % to production with it... (it should only be unique for this node)
    {A, B, C} = erlang:now(),
    SessionId = lists:flatten(io_lib:format("~p~p~p", [A, B, C])),
    io:format("Creating session ~p~n", [SessionId]),
    true = ets:insert(State#state.sessions, {SessionId, SessionPid}),
    NewNumSessions = State#state.sessions_serviced + 1,
    {reply, {ok, SessionId}, State#state{sessions_serviced = NewNumSessions}};

handle_call({reverse, SessionId, String}, _From, State) ->
    % Start by trying to find the session id
    Result = case find_session_pid(SessionId, State#state.sessions) of
        undefined ->
            % Session is not known
            io:format("Could not find session ~p~n", [SessionId]),
            error;
        SessionPid ->
            % Session is known
            reverserl_session:reverse(SessionPid, String)
    end,
    {reply, Result, State};

handle_call({delete_session, SessionId}, _From, State) ->
    % Start by trying to find the session id
    Result = case find_session_pid(SessionId, State#state.sessions) of
        undefined ->
            % Nothing to do...
            error;
        SessionPid ->
            io:format("Deleting session ~p~n", [SessionId]),
            reverserl_session:stop(SessionPid),
            ok
    end,
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', SessionPid, _Reason}, State) ->
    case find_session_id(SessionPid, State#state.sessions) of
        undefined ->
            ok;
        SessionId ->
            % The PID that terminated was known as a session, so now
            % remove it from the session table
            true = ets:delete(State#state.sessions, SessionId)
    end,
    {noreply, State}.

%% @doc This gets called when the process will be terminated. Like
%% an object destructor.
terminate(_Reason, _State) ->
    io:format("Inside reverserl_server's terminate function~n"),
    ok.

%% @doc This callback function is used when performing hot code
%% updates. We won't be using this for now. It allows us to modify
%% the current version's state and upgrade/downgrade it accordingly
%% when changing the version. The new state is then returned in the
%% result tuple.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

find_session_pid(SessionId, Sessions) ->
    case ets:lookup(Sessions, SessionId) of
        [{SessionId, SessionPid}] -> SessionPid;
        _ -> undefined
    end.

find_session_id(SessionPid, Sessions) ->
    case ets:match(Sessions, {'$1', SessionPid}) of
        [[SessionId]] -> SessionId;
        _ -> undefined
    end.

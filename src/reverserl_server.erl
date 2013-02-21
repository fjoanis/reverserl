%% @doc This Erlang module represents the core server of the
%% application. It handles incoming requests to create new sessions
%% and will also dispatch requests targetting pre-existing sessions
%% towards their appropriate Erlang processes. This module uses the
%% gen_server behaviour which means that it will behave like what
%% is described here: http://www.erlang.org/doc/man/gen_server.html
%%
%% @author Francis (Ottawa-Gatineau Erlang)

%% Comments start with a single percent sign (%). Double are
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
-export([start_link/0, reverse/2]).

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
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    % Use the core gen_server API to do the actual start with
    % arguments we control here. 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec reverse(list(), list()) -> list().
reverse(_SessionId, String) ->
    "TODO".

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {children :: list(pid())}).
-type state() :: #state{}.

%% @doc This function gets called when the server process will be
%% started. It is like its "constructor". The second element of the
%% returned tuple is the state that will be carried through the
%% lifetime of the server. Think of that state as being what would
%% hold the member variables of a classical OO object. This function
%% gets called in the "thread" context of the newly spawned server.
-spec init(_) -> {ok, state()}.
init(_Args) ->
    io:format("Inside reverserl_server's init function~n"),
    
    % This tells the Erlang VM that the current process TODO
    erlang:process_flag(trap_exit, true),

    % Note that, in the line above, the underscore (_) is used
    % prepended to the Args name. This is because this variable is
    % not used and we do not want the compiler to complain about it.
    % _ means "don't care"
    {ok, #state{}}.


-spec handle_call(_, _, state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(_, state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc This gets called when the process will be terminated. Like
%% an object destructor.
-spec terminate(_, state()) -> ok.
terminate(_Reason, _State) ->
    io:format("Inside reverserl_server's terminate function~n"),
    ok.

%% @doc This callback function is used when performing hot code
%% updates. We won't be using this for now. It allows us to modify
%% the current version's state and upgrade/downgrade it accordingly
%% when changing the version. The new state is then returned in the
%% result tuple.
-spec code_change(_, state(), _) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


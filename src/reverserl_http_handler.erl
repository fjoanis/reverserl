%% @author Francis (Ottawa Erlang)

-module(reverserl_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%% @doc This gets called when the request first arrives
init(_Transport, Req, []) ->
    {ok, Req, undefined}.

%% @doc This gets called when the request actually needs to be handled 
handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    {SessionIdRaw, _} = cowboy_req:binding(session_id, Req),

    SessionId = case SessionIdRaw of
        Binary when is_binary(Binary) ->
            binary_to_list(Binary);
        _ ->
            SessionIdRaw
    end,

    {ok, Reply} = case {Method, SessionId} of
        {<<"PUT">>, undefined} ->
            handle_post(Req);
        {<<"GET">>, undefined} ->
            io:format("Session has to be supplied~n"),
            cowboy_req:reply(404, [], <<>>, Req);
        {<<"GET">>, SessionId} ->
            {String, _} = cowboy_req:qs_val(<<"string">>, Req),
            handle_get(Req, SessionId, binary_to_list(String));
        {<<"DELETE">>, undefined} ->
            io:format("Session has to be supplied~n"),
            cowboy_req:reply(404, [], <<>>, Req);
        {<<"DELETE">>, SessionId} ->
            handle_delete(Req, SessionId);
        _ ->
            io:format("Invalid request received: ~p~n", [Req]),
            cowboy_req:reply(503, [], <<>>, Req)
    end,

    {ok, Reply, State}.

%% @doc This gets called when the request gets terminated (i.e. ends)
terminate(_Reason, _Req, _State) ->
    ok.

handle_post(Req) ->
    {ok, SessionId} = reverserl_server:create_session(),
    cowboy_req:reply(201, [], list_to_binary(SessionId), Req).

handle_get(Req, _, undefined) ->
    io:format("String parameter needs to be supplied~n"),
    cowboy_req:reply(503, [], <<>>, Req);
handle_get(Req, SessionId, String) ->
    case reverserl_server:reverse(SessionId, String) of
        error ->
            io:format("Could not reverse string~n"),
            cowboy_req:reply(503, [], <<>>, Req);
        Result ->
            cowboy_req:reply(200, [], list_to_binary(Result), Req)
    end.

handle_delete(Req, SessionId) ->
    case reverserl_server:delete_session(SessionId) of
        ok ->
            cowboy_req:reply(204, [], <<>>, Req);
        _ ->
            io:format("Could not find session to DELETE~n"),
            cowboy_req:reply(404, [], <<>>, Req)
    end.


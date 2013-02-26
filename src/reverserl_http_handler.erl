-module(reverserl_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%% @doc This gets called when the request first arrives
init(_Transport, Req, []) ->
    io:format("reverserl_http_handler:init~n"),
    {ok, Req, undefined}.

%% @doc This gets called when the request actually needs to be handled 
handle(Req, State) ->
    io:format("reverserl_http_handler:handle~n"),

    {Method, _} = cowboy_req:method(Req),
    {SessionId, _} = cowboy_req:binding(session_id, Req),

    {ok, Reply} = case {Method, SessionId} of
        {<<"POST">>, undefined} ->
            io:format("Creating new session...~n"),
            cowboy_req:reply(200, [], <<>>, Req);
        {<<"GET">>, undefined} ->
            cowboy_req:reply(404, [], <<>>, Req);
        {<<"GET">>, SessionId} ->
            cowboy_req:reply(200, [], <<>>, Req);
        {<<"DELETE">>, undefined} ->
            cowboy_req:reply(404, [], <<>>, Req);
        {<<"DELETE">>, SessionId} ->
            cowboy_req:reply(200, [], <<>>, Req);
        _ ->
            cowboy_req:reply(503, [], <<>>, Req)
    end,

    {ok, Reply, State}.

%% @doc This gets called when the request gets terminated (i.e. ends)
terminate(_Reason, _Req, _State) ->
    io:format("reverserl_http_handler:terminate~n"),
    ok.

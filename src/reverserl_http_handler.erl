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
    Result = list_to_binary(reverserl_server:reverse(0, "123")),
    {ok, Req2} = cowboy_req:reply(200, [], Result, Req),
    {ok, Req2, State}.

%% @doc This gets called when the request gets terminated (i.e. ends)
terminate(_Reason, _Req, _State) ->
    io:format("reverserl_http_handler:terminate~n"),
    ok.

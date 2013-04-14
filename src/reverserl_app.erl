%% @doc This module implements the OTP application behaviour for
%% reverserl 
%%
%% @author Francis (Ottawa Erlang)

-module(reverserl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/reverserl/[:session_id]", reverserl_http_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
        {env, [{dispatch, Dispatch}]}
    ]),

    reverserl_sup:start_link().

stop(_State) ->
    ok.

-module(chat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
                {'_', [{"/still", chat_still_handler, []}]}
                ]),
    cowboy:start_http(chat_listener, 10,
                      [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]),
    chat_sup:start_link().

stop(_State) ->
    ok.

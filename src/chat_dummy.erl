-module(chat_dummy).

-export([init/3,
         handle/2,
         terminate/3]).

-record(state, {}).

init({_Transport, _Protocol}, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    {ok, Req1} = cowboy_req:reply(200, [], <<"chat vivant :)">>, Req),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.

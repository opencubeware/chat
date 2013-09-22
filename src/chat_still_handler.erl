-module(chat_still_handler).

-export([init/3,
         handle/2,
         terminate/3]).

-record(state, {port}).

init({_Transport, _Protocol}, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    Req2 = case chat:capture() of
        {ok, Data} ->
            ContentType = [{<<"content-type">>, <<"image/jpeg">>}],
            {ok, Req1} = cowboy_req:reply(200, ContentType, Data, Req),
            Req1;
        {error, _} ->
            {ok, Req1} = cowboy_req:reply(500, [], [], Req),
            Req1
    end,
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

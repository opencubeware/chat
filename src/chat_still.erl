-module(chat_still).

-export([init/3,
         handle/2,
         terminate/3]).

-record(state, {}).

init({_Transport, _Protocol}, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    Req2 = case capture() of
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

capture() ->
    Cmd = "raspistill -w 1920 -h 1080 -q 100 -t 0 -vf -o -",
    Port = erlang:open_port({spawn, Cmd}, [binary, exit_status]),
    capture(Port, <<>>).

capture(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            capture(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, 0}} ->
            {ok, Acc};
        {Port, {exit_status, _}} ->
            {error, unknown}
    end.

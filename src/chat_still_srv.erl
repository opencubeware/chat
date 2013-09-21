-module(chat_still_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(capture, _From, State) ->
    Cmd = "raspistill -w 1920 -h 1080 -q 100 -t 0 -vf -o -",
    Port = erlang:open_port({spawn, Cmd}, [binary, exit_status]),
    Reply = capture(<<>>, Port),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
capture(Acc, Port) ->
    receive
        {Port, {data, Data}} ->
            capture(<<Acc/binary, Data/binary>>, Port);
        {Port, {exit_status, 0}} ->
            {ok, Acc};
        {Port, {exit_status, _}} ->
            {error, unknown}
    end.

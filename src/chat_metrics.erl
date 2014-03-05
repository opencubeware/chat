-module(chat_metrics).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_metrics/0]).

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

get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call(get_metrics, _From, State) ->
    Fps = folsom_metrics:get_histogram_statistics(fps),
    Starts = folsom_metrics:get_metric_value(camera_starts),
    Reply = [{fps, Fps}, {camera_starts, Starts}],
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
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

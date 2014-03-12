-module(chat_remote).

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

handle_call({start, Bitrate, Output}, _From, State) ->
    Reply = {ok, chat_camera:start(Bitrate, Output)},
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    Reply = {ok, chat_camera:stop()},
    {reply, Reply, State};
handle_call(preview, _From, State) ->
    {reply, handle_preview(), State};
handle_call(add_logo, _From, State) ->
    Reply = {ok, chat_overlay:add_logo(logo)},
    {reply, Reply, State};
handle_call({add_info, Competition, Event, Info}, _From, State) ->
    Reply = {ok, chat_overlay:add_info(Event, Competition, Info)},
    {reply, Reply, State};
handle_call({delete_segment, Id}, _From, State) ->
    Reply = {ok, chat_overlay:delete_segment(Id)},
    {reply, Reply, State};
handle_call(delete_segments, _From, State) ->
    Reply = {ok, chat_overlay:delete_segments()},
    {reply, Reply, State};
handle_call(status, _From, State) ->
    Reply = {ok, handle_status()},
    {reply, Reply, State};
handle_call(metrics, _From, State) ->
    Reply = {ok, handle_metrics()},
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
handle_preview() ->
    Cmd = lists:concat(["raspistill -w 640 -h 480 "
                        " -q 100 -t 1 -vf -hf -o -"]),
    Port = erlang:open_port({spawn, Cmd}, [binary, exit_status]),
    capture_preview(<<>>, Port).

capture_preview(Acc, Port) ->
    receive
        {Port, {data, Data}} ->
            capture_preview(<<Acc/binary, Data/binary>>, Port);
        {Port, {exit_status, 0}} ->
            {ok, Acc};
        {Port, {exit_status, _}} ->
            {error, unknown}
    end.

handle_status() ->
    try
        {running,
         [{bitrate, chat_camera:get_bitrate()},
          {segments, chat_overlay:segments()}]}
    catch exit:{noproc,_} ->
            not_running
    end.

handle_metrics() ->
    Fps = folsom_metrics:get_histogram_statistics(fps),
    Starts = folsom_metrics:get_metric_value(camera_starts),
    [{fps, Fps}, {camera_starts, Starts}].

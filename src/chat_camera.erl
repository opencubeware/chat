-module(chat_camera).

-behaviour(gen_server).

%% API
-export([start_link/2,
         start/2,
         stop/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {port}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Bitrate, Output) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Bitrate, Output], []).

start(Bitrate, Output) ->
    supervisor:terminate_child(chat_camera_sup, ?MODULE),
    supervisor:delete_child(chat_camera_sup, ?MODULE),
    Spec = {?MODULE, {?MODULE, start_link, [Bitrate, Output]},
            transient, 1000, worker, [?MODULE]},
    supervisor:start_child(chat_camera_sup, Spec).

stop() ->
    gen_server:call(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Bitrate, Output]) ->
    Cmd = cmd(Bitrate, Output),
    Port = erlang:open_port({spawn, Cmd}, [exit_status, binary]),
    chat_overlay:flush_buffer(),
    folsom_metrics:notify({camera_starts, {inc, 1}}),
    {ok, #state{port=Port}}.

handle_call(stop, _From, State) ->
    chat_overlay:delete_segments(),
    {stop, shutdown, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {exit_status, _}}, #state{port=Port}=State) ->
    {stop, chat_camera_exited, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
cmd(Bitrate, {rtmp, List}) when is_list(List) ->
    app(Bitrate) ++ " | " ++
    "ffmpeg -i - -vcodec copy -an -f flv " ++ List ++ " 2> /dev/null";
cmd(Bitrate, Other) ->
    app(Bitrate) ++ " > " ++ Other.

app(Bitrate) ->
    filename:join([priv, lib, chat_camera]) ++ " " ++ integer_to_list(Bitrate).


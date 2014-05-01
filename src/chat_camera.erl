-module(chat_camera).

-behaviour(gen_server).

%% API
-export([start_link/2,
         start/2,
         stop/0,
         get_bitrate/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {camera_port, ffmpeg_port,
                bitrate, via, buffer, buffer_size, buffer_left}).

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

get_bitrate() ->
    gen_server:call(?MODULE, get_bitrate).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Bitrate, Output]) ->
    process_flag(trap_exit, true),
    maybe_start_silence(Output),
    {CameraCmd, FfmpegCmd} = cmd(Bitrate, Output),
    {CameraPort, FfmpegPort} = open_ports(CameraCmd, FfmpegCmd),
    State = initial_state(CameraPort, FfmpegPort, Bitrate, Output),
    chat_overlay:flush_buffer(),
    folsom_metrics:notify({camera_starts, {inc, 1}}),
    {ok, State}.

handle_call(get_bitrate, _From, #state{bitrate=Bitrate}=State) ->
    {reply, Bitrate, State};
handle_call(stop, _From, State) ->
    chat_overlay:delete_segments(),
    {stop, shutdown, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, Data}}, #state{camera_port=Port,
                                         via=Pid,
                                         buffer=Buf,
                                         buffer_left=BLeft,
                                         buffer_size=BSize}=State) ->
    Size = byte_size(Data),
    NewBuf = <<Buf/binary, Data/binary>>,
    case BLeft-Size of
        Rem when Rem > 0 ->
            {noreply, State#state{buffer=NewBuf, buffer_left=Rem}};
        _ ->
            Pid ! {?MODULE, NewBuf},
            {noreply, State#state{buffer = <<>>, buffer_left=BSize}}
    end;
handle_info({Port, {exit_status, _}}, #state{ffmpeg_port=Port}=State) ->
    {stop, ffmpeg_exited, State};
handle_info({Port, {exit_status, _}}, #state{camera_port=Port}=State) ->
    {stop, chat_camera_exited, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    kill_processes(chat_silence),
    kill_processes(ffmpeg),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
open_ports(CameraCmd, FfmpegCmd) ->
    CameraPort = erlang:open_port({spawn, CameraCmd}, [exit_status, binary]),
    FfmpegPort = case FfmpegCmd of
        undefined ->
            undefined;
        _ ->
            erlang:open_port({spawn, FfmpegCmd}, [exit_status, binary])
    end,
    {CameraPort, FfmpegPort}.

maybe_start_silence({yt, _}) ->
    Cmd = filename:join([priv, lib, chat_silence]),
    erlang:open_port({spawn, Cmd}, [exit_status, binary]);
maybe_start_silence(_) ->
    ok.

initial_state(CameraPort, _FfmpegPort, Bitrate, {via, Pid, BSize}) ->
    #state{camera_port=CameraPort,
           bitrate=Bitrate,
           via=Pid,
           buffer_size=BSize,
           buffer_left=BSize,
           buffer = <<>>};
initial_state(CameraPort, FfmpegPort, Bitrate, _) ->
    #state{camera_port=CameraPort,
           ffmpeg_port=FfmpegPort,
           bitrate=Bitrate}.

cmd(Bitrate, {via, _, _}) ->
    {app(Bitrate), undefined};
cmd(Bitrate, {yt, List}) when is_list(List) ->
    os:cmd("mkfifo /tmp/camera.h264"),
    {app(Bitrate) ++ " > /tmp/camera.h264",
    "ffmpeg -ar 44100 -ac 1 -f s16le -i /tmp/silence.aac "
    "-i /tmp/camera.h264 -vcodec copy -acodec aac -ab 128k "
    "-f flv -strict experimental -shortest "
    ++ List ++ " 2> /dev/null "};
cmd(Bitrate, {rtmp, List}) when is_list(List) ->
    os:cmd("mkfifo /tmp/camera.h264"),
    {app(Bitrate) ++ " > /tmp/camera.h264",
    "ffmpeg -i /tmp/camera.h264 -vcodec copy -an -f flv "
     ++ List ++ " 2> /dev/null"};
cmd(Bitrate, Other) ->
    {app(Bitrate) ++ " > " ++ Other, undefined}.

app(Bitrate) ->
    filename:join([priv, lib, chat_camera]) ++ " " ++
    integer_to_list(Bitrate).

kill_processes(Name) ->
    os:cmd("kill $(ps ax | grep '" ++ atom_to_list(Name)
           ++ "' | awk '{print $1}')").

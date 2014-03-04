-module(chat_camera).

-behaviour(gen_server).

%% API
-export([start_link/1,
         start/1,
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
start_link(Output) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Output], []).

start(Output) ->
    supervisor:terminate_child(chat_sup, ?MODULE),
    supervisor:delete_child(chat_sup, ?MODULE),
    Spec = {?MODULE, {?MODULE, start_link, [Output]},
            transient, 1000, worker, [?MODULE]},
    supervisor:start_child(chat_sup, Spec).

stop() ->
    gen_server:call(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Output]) ->
    App = filename:join([priv, lib, chat_camera]),
    Cmd = App ++ " > " ++ Output,
    Port = erlang:open_port({spawn, Cmd}, [exit_status]),
    {ok, #state{port=Port}}.

handle_call(stop, _From, State) ->
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

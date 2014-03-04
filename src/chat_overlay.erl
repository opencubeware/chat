-module(chat_overlay).

%% API
-export([start_link/0,
         add_logo/0,
         add_logo/4,
         segments/0,
         delete_segment/1]).

%% Special process callbacks
-export([init/1,
         system_continue/3,
         system_terminate/4]).

-record(state, {segments = [], dummy}).

-define(NIF_TIMEOUT, 2000).

-on_load(on_load/0).
%% ===================================================================
%% Module on-load callback
%% ===================================================================
on_load() ->
    LibFile = filename:join(["priv", "lib", ?MODULE]),
    erlang:load_nif(LibFile, 0).

%% ===================================================================
%% API
%% ===================================================================
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

add_logo() ->
    LogoFile = filename:join(["priv", "assets", "logo_720.png"]),
    add_logo(LogoFile, 1125, 35, 0.4).

add_logo(File, X, Y, Alpha) ->
    call({add_logo, File, X, Y, Alpha}). 

segments() ->
    call(segments).

delete_segment(Id) ->
    call({delete_segment, Id}).
    
%% ===================================================================
%% Special server process logic
%% ===================================================================
init(Parent) ->
    register(?MODULE, self()),
    Debug = sys:debug_options([]),
    Segments = get_segments_nif(),
    State = #state{segments = Segments},
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(State, Parent, Debug).

loop(State, Parent, Debug) ->
    receive
        {request, Ref, From, Request} ->
            NewState = handle_request(Request, From, Ref, State),
            loop(NewState, Parent, Debug);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
        _ ->
            loop(State, Parent, Debug)
    end.

handle_request({add_logo, File, X, Y, Alpha}, From, Ref, State) ->
    {Reply, NewState} = handle_add_logo(File, X, Y, Alpha, State),
    From ! {reply, Ref, Reply},
    NewState;
handle_request({delete_segment, Id}, From, Ref, State) ->
    {Reply, NewState} = handle_delete_segment(Id, State),
    From ! {reply, Ref, Reply},
    NewState;
handle_request(segments, From, Ref, State) ->
    From ! {reply, Ref, State#state.segments},
    State;
handle_request(_Other, _From, _Ref, State) ->
    State.

handle_add_logo(File, X, Y, Alpha, State=#state{segments=Segs}) ->
    case wait_for(add_logo_nif(File, X, Y, Alpha)) of
        {ok, Segment} ->
            NewSegs = [Segment|Segs],
            NewState = State#state{segments=NewSegs},
            {{ok, Segment}, NewState};
        Other ->
            {Other, State}
    end.

handle_delete_segment(Id, State=#state{segments=Segs}) ->
    case wait_for(delete_segment_nif(Id)) of
        ok ->
            NewSegs = lists:delete(Id, Segs),
            NewState = State#state{segments=NewSegs},
            {ok, NewState};
        Other ->
            {Other, State}
    end.

system_continue(Parent, Debug, State) ->
    loop(State, Parent, Debug).

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

%% ===================================================================
%% Private functions
%% ===================================================================
call(Request) ->
    call(Request, 5000).

call(Request, Timeout) ->
    Ref = make_ref(),
    ?MODULE ! {request, Ref, self(), Request},
    receive
        {reply, Ref, Reply} -> Reply
    after Timeout ->
        {error, timeout}
    end.

wait_for(ok) ->
    receive
        Reply -> Reply
    after ?NIF_TIMEOUT ->
        {error, timeout}
    end;
wait_for(Other) ->
    Other.
%% ===================================================================
%% NIFs
%% ===================================================================
get_segments_nif() ->
    error(nif_not_loaded).

add_logo_nif(_File, _X, _Y, _Alpha) ->
    error(nif_not_loaded).

delete_segment_nif(_Id) ->
    error(nif_not_loaded).

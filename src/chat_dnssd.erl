-module(chat_dnssd).

%% API
-export([start_link/0]).

%% Special process callbacks
-export([init/1,
         system_continue/3,
         system_terminate/4]).

-record(state, {}).

-define(TIMEOUT, 5000).

%% ===================================================================
%% API
%% ===================================================================
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

%% ===================================================================
%% Special process logic
%% ===================================================================
init(Parent) ->
    register(?MODULE, self()),
    Debug = sys:debug_options([]),
    {ok, _} = register_service(),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(#state{}, Parent, Debug).

loop(State, Parent, Debug) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
        _ ->
            loop(State, Parent, Debug)
    end.

system_continue(Parent, Debug, State) ->
    loop(State, Parent, Debug).

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

%% ===================================================================
%% Private functions
%% ===================================================================
register_service() ->
    {ok, Hostname} = inet:gethostname(),
    NodeName = "chat@" ++ Hostname ++ ".local.",
    {ok, Ref} = dnssd:register(NodeName, "_chat._tcp", 4369),
    receive
        {dnssd, Ref, {register, _, {RegisteredName, _, _}}} ->
            RegisteredAtom = binary_to_atom(RegisteredName, utf8),
            {ok, _} = net_kernel:start([RegisteredAtom, longnames])
    after ?TIMEOUT ->
        {error, timeout}
    end.

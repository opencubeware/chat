
-module(chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
                {chat_metrics, {chat_metrics, start_link, []},
                 permanent, brutal_kill, worker, [chat_metrics]},
                {chat_overlay, {chat_overlay, start_link, []},
                 permanent, brutal_kill, worker, [chat_overlay]},
                {chat_dnssd, {chat_dnssd, start_link, []},
                 permanent, brutal_kill, worker, [chat_dnssd]},
                {chat_camera_sup, {chat_camera_sup, start_link, []},
                 permanent, 1500, supervisor, [chat_camera_sup]}
                ]} }.

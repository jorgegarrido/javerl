%% ----------------------------------------------
%% 
%% Jorge Garrido <jorge.garrido@morelosoft.com>
%%
%% javerl_sup.erl
%%
%% ----------------------------------------------

-module(javerl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start, Args},permanent, 2000, Type, [I]}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Port} = application:get_env(javerl, port),
    Args = [Port, {acceptor, loop}],
    Childs = [?CHILD(socket, worker, Args)],
    {ok, { {one_for_one, 1000, 3600}, Childs} }.


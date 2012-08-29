%% ----------------------------------------------
%%
%% Jorge Garrido <jorge.garrido@morelosoft.com>
%%
%% javerl_app.erl
%%
%% ----------------------------------------------

-module(javerl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    javerl_sup:start_link().

stop(_State) ->
    ok.


%% @author Mochi Media <dev@mochimedia.com>
%% @copyright blochi Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the blochi application.

-module(blochi_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for blochi.
start(_Type, _StartArgs) ->
    blochi_deps:ensure(),
    blochi_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for blochi.
stop(_State) ->
    ok.

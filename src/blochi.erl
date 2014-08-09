%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc blochi.

-module(blochi).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the blochi server.
start() ->
    blochi_deps:ensure(),
    ensure_started(crypto),
    application:start(blochi).


%% @spec stop() -> ok
%% @doc Stop the blochi server.
stop() ->
    application:stop(blochi).

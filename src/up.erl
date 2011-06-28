%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc up.

-module(up).
-behaviour(application).
-export([start/0, start/2, stop/0, stop/1]).

%% @spec start() -> ok
%% @doc Start the up server.
start() ->
    config:ensure(),
    ensure_started(crypto),
    application:start(up).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for up.
start(_Type, _StartArgs) ->
    config:ensure(),
    datastore:start(),
    sup:start_link().

%% @spec stop() -> ok
%% @doc Stop the up server.
stop() ->
    application:stop(up).

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for up.
stop(_State) ->
    ok.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.




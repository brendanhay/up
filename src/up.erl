%% Main OTP Application for the Super Uploader! 
%%
%% A thin semi-RESTful layer on top of [mochiweb](https://github.com/mochi/mochiweb), 
%% [erlydtl](https://github.com/evanmiller/erlydtl), 
%% and [mnesia](http://www.erlang.org/doc/man/mnesia.html).
%%
%% #### Basics:
%% * Simple HTTP verb [dispatching](dispatcher.html) mechanism.
%% * [Parsing](uploader.html) of an inbound multipart/form-data stream.
%% * Async [polling mechanism](progress.html) for retrieving json describing an upload's progress.
-module(up).

-behaviour(application).

-export([start/0, start/2, stop/0, stop/1]).

%% #### Starting:
%% 1. Ensure crypto has been started (before mochiweb).
%% 2. Ensure the dependency paths have been set and any dependencies loaded.
%% 3. Start the datastore (mnesia) and create a schema if neccessary.
%% 4. Finally, start the [main supervision tree](sup.html).
start() -> 
    ensure_started(crypto),
    ensure_started(up),
    logger:info("Start", "Waiting for connections").

start(_Type, _StartArgs) ->
    config:ensure(),
    datastore:start(), 
    sup:start().

%% #### Stopping:
%% To stop the system, the supervisor notifies the children of shutdown.
%% mnesia will deadlock if you attemp to stop it during an application shutdown,
%% so it can fend for itself.
stop() -> application:stop(up).

stop(_State) -> ok.

%% Handle attempting to start a previously started dependency.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.




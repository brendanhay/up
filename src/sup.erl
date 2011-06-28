%% Root of the supervision tree which manages:
%%
%% * [[logger.erl]] - Asynchronous logging
%% * [[dispatcher.erl]] - HTTP dispatching
%% * [[uploader.erl]] - Upload progress
-module(sup).

-behaviour(supervisor).

-export([start/0, init/1]).

%% #### Starting:
%% Call the OTP `supervisor` behaviour which calls back to `init/1` with no arguments
%% to start the supervision tree.
start() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Config = [{ip, {0,0,0,0}}, {port, 8080}],
    Specs = [spec(logger), spec(dispatcher, [Config], 5000, dynamic), spec(progress)],
    Strategy = {one_for_one, 10, 10},
    {ok, {Strategy, Specs}}.

%% Shorthand for generating supervision child' specs.
spec(Mod) -> spec(Mod, [], 1, [Mod]).

spec(Mod, Args, Restart, Mods) -> 
    {Mod, {Mod, start, Args}, permanent, Restart, worker, Mods}.
    

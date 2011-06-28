%% Manages a `dict` of `{Id, Percent}` tuples keyed by `dispatcher:tracking_key/0`
%% in a seperate process.
%% The state is updated by [[uploader.erl]] as new chunks of form data are received,
%% and polled from [[dispatcher.erl]] for progress information by the client.
-module(progress).

-behaviour(gen_server).

-export([start/0, report/1, update/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("records.hrl").

%% ### Starting:
%% Started by the supervision tree, using a call to our friendly `gen_server` 
%% which registers the process under the module name.
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Synchronously poll for progress from the state loop.
report(Key) when is_list(Key) -> report(list_to_binary(Key));
report(Key)                   -> gen_server:call(?MODULE, {report, Key}). 

%% Send an asynchronous message to the state loop.
%% Used by [[uploader.erl]] to update the internal progress state. 
update(Message) -> gen_server:cast(?MODULE, Message).

%% Get the progress for a given binary `Key`, and return
%% the retrieved values or `{0, 100}` if the `Key` doesn't exist. 
handle_call({report, Key}, _From, State) ->
    {Id, Percent} = case dict:find(Key, State) of 
                        error -> {0, 100};
                        {ok, {I, P}} -> {I, P}
                    end,
    {reply, [{id, Id}, {percent, Percent}], State}.

%% Track a new `Key` with `{Id, 0}` as the initial values.
handle_cast({started, Key, Id}, State) ->
    {noreply, dict:store(Key, {Id, 0}, State)};
%% Update a `Key` with a new percentage.
handle_cast({updated, Key, Percent}, State) -> 
    New = case dict:find(Key, State) of 
              error -> State; 
              {ok, {Id, _Prev}} -> dict:store(Key, {Id, Percent}, State)
          end,
    {noreply, New};
%% Remove the `Key` from `State`, effectively marking it as completed.
handle_cast({completed, Key}, State) ->
    New = case dict:find(Key, State) of 
              error -> State; 
              {ok, {Id, _Prev}} -> dict:store(Key, {Id, 101}, State)
          end,
    {noreply, New}.


%% *Callbacks for the `gen_server` behaviour:*
init(_Args) -> {ok, dict:new()}.

terminate(_Reason, _State) ->  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Any, State) -> {noreply, State}.



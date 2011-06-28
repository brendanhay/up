-module(progress).

-behaviour(gen_server).

-export([start/0, progress/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("records.hrl").


% External

start() ->  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

progress(Key) when is_list(Key) -> progress(list_to_binary(Key));
progress(Key)                   -> gen_server:call(?MODULE, {progress, Key}). 

% Gen Server

init(_Args) -> {ok, dict:new()}.

terminate(_Reason, _State) ->  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
 
handle_call({progress, Key}, _From, State) ->
    {Id, Percent} = case dict:find(Key, State) of 
                        error -> {0, 100};
                        {ok, {I, P}} -> {I, P}
                    end,
    {reply, [{id, Id}, {percent, Percent}], State}.

handle_cast({started, Key, Id}, State) ->
    {noreply, dict:store(Key, {Id, 0}, State)};
handle_cast({updated, Key, Percent}, State) -> 
    New = case dict:find(Key, State) of 
              error -> State; 
              {ok, {Id, _Prev}} -> dict:store(Key, {Id, Percent}, State)
          end,
    {noreply, New};
handle_cast({completed, _Key}, State) ->
%    {noreply, dict:erase(Key, State)}.
    {noreply, State}.

handle_info(_Any, State) -> {noreply, State}.


% Internal 


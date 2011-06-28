-module(logger).

-export([start/0, info/2, info/3, error/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


% External

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

info(Type, Format) -> info(Type, Format, []).

info(Type, Format, Args) -> gen_server:cast(?MODULE, {Type, Format, Args}). 

error(Report) -> error_logger:error_report(Report). % SASL

% Gen Server

init(_Args) -> {ok, []}.

terminate(_Reason, _State) ->  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
 
handle_call(_Message, _From, State) -> {reply, ok, State}.

handle_cast({Type, Format, Args}, State) ->
    {H, M, S} = erlang:time(),
    io:format(string:join(["~2..0B:~2..0B:~2..0B ~s: ", Format, "~n"], ""), [H, M, S, Type|Args]),
    {noreply, State}.

handle_info(_Any, State) -> {noreply, State}.

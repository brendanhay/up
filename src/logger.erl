%% Simple asynchronous logging server which either
%% logs an info message or dumps a `sasl` report.
-module(logger).

-export([start/0, info/2, info/3, error/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

%% #### Starting:
%% Called by [[sup.erl]] as part of a supervision tree, fires the `gen_server` behaviour
%% callback which in turn calls `init/1` with no arguments.
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Dump a `sasl` report to the console.
error(Report) -> error_logger:error_report(Report). 

%% Log a simple string message.
info(Type, Format) -> info(Type, Format, []).

%% Log a format string with arguments, ie: `"~p"` and `[{ok, "123"}]`.
info(Type, Format, Args) -> gen_server:cast(?MODULE, {Type, Format, Args}). 

%% *Callbacks for the OTP `gen_server` behaviour:*
init(_Args) -> {ok, []}.

terminate(_Reason, _State) ->  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
 
handle_call(_Message, _From, State) -> {reply, ok, State}.

handle_info(_Any, State) -> {noreply, State}.

%% Actual logging (via async cast) which prepends a format string
%% with the current hour/minute/second and pads it out suitably.
handle_cast({Type, Format, Args}, State) ->
    {H, M, S} = erlang:time(),
    io:format(string:join(["~2..0B:~2..0B:~2..0B ~s: ", Format, "~n"], ""), [H, M, S, Type|Args]),
    {noreply, State}.


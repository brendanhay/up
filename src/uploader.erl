-module(uploader).
-behaviour(gen_server).

-export([start/0, upload/1, progress/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
-include("records.hrl").


% External

start() ->  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

upload(Req) -> 
    Callback = callback({file, new_context(Req)}),
    mochiweb_multipart:parse_multipart_request(Req, Callback).

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

callback(Context) -> fun(Next) -> callback(Next, Context) end.

callback(Chunk, Context) -> callback(parse(Chunk, Context)).

parse({headers, Headers}, {_Any, Context}) -> headers(Context, Headers);
parse({body, Data}, {Field, Context})      -> body(Field, Context, Data);
parse(body_end, {Field, Context})          -> body_end(Field, Context);
parse(eof, {_Any, Context})                -> save(Context).

headers(Context, [Disposition|_]) ->
    case Disposition of
        {"content-disposition", {"form-data", [{"name", Field}]}} ->
            {list_to_atom(Field), Context};
        {"content-disposition", {"form-data", [{"name", _}, {"filename", Name}]}} ->
            Path = config:tmp_path([Context#con.key]),
            filelib:ensure_dir(Path),           
            Upload = Context#con.upload,
            file_started(Context#con{path=Path, upload=Upload#upload{name=Name}});            
         _Other ->
            {unknown, Context}
    end.

body(file, Context=#con{io=none, path=none}, _Data) ->
    {file, Context};
body(file, Context=#con{io=none, path=Path}, Data) ->
    case file:open(Path, [raw, write]) of
        {ok, IO} ->
            file:write(IO, Data),
            file_updated(Context#con{io=IO, done=size(Data)});
        {error, Error} ->
            logger:info(Error, "can't open ~p for writing", [Path]),
            exit(could_not_open_file_for_writing),
            {error, Context}
    end;
body(file, Context=#con{io=IO, done=Done}, Data) ->
    file:write(IO, Data),
    file_updated(Context#con{done=(Done + size(Data))});
body(key, Context, Data) ->
    {done, Context#con{key=Data}};
body(description, Context=#con{upload=Upload}, Data) ->
    {done, Context#con{upload=Upload#upload{description=Data}}};
body(_Unknown, Context, _Data) ->
    {done, Context}.

body_end(Field, Context=#con{io=IO}) -> 
    case Field of 
        file ->
            case IO of
                none -> 
                    ok;
                IO ->
                    file_completed(Context),
                    file:close(IO)
            end;
        Field -> ok
    end,
    {ok, Context}.
                                   
file_started(Context=#con{key=Key, upload=Upload}) -> 
    logger:info(Key, "~p bytes pending", [Context#con.total]),
    gen_server:cast(?MODULE, {started, Key, Upload#upload.id}), 
    {file, Context}.

file_updated(Context=#con{key=Key, total=Total, done=Done}) ->
    Percent = percentage(Done, Total),
    logger:info(Key, "~p% of ~p bytes", [Percent, Context#con.total]),
    gen_server:cast(?MODULE, {updated, Key, Percent}), 
    {file, Context}.

file_completed(Context=#con{key=Key}) -> 
    logger:info(Key, "completed"),
    gen_server:cast(?MODULE, {completed, Key}), 
    {file, Context}.

% TODO: Tidy this mess up
save(#con{key=Key, path=Path, upload=Upload}) ->
    To = config:upload_path(datastore:save(Upload)),
    ok = filelib:ensure_dir(To),
    {ok, _Size} = file:copy(Path, To),
    ok = file:delete(Path),
    logger:info(Key, "moved ~s to ~p", [Path, To]),
    ok.

percentage(Done, Total) -> truncate((Done / Total) * 100).

truncate(Float) -> % Gotta love the erlang math module
    [Str] = io_lib:format("~.2f", [Float]),
    {Float1, []} = string:to_float(Str),
    Float1.

new_context(Req) ->
    Total = list_to_integer(Req:get_header_value('Content-Length')),
    #con{total=Total, upload=datastore:build()}.

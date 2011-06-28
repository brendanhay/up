%% Provides parsing of multipart/form-data on the calling process
%% via callbacks which are chained based on previous or subsequent data.
%% Updates the registered process of [[progress.erl]] with upload statistics
%% each time a chunk of body data is parsed.
-module(uploader).

-export([start/1]).

-include("records.hrl").

%% ### Starting:
%% Passes a curried callback function and the incoming request to the `mochiweb_multipart`
%% module, which parses the incoming form-data boundary and fires the callback.
start(Req) -> 
    Callback = callback({file, new_context(Req)}),
    mochiweb_multipart:parse_multipart_request(Req, Callback).

%% ### Internal:

%% Each subsequent call of the callback wrapper should generate a new 
%% function to be applied when the next chunk is received.
%% `callback/1` partially applies the current context to a new function wrapper.
callback(Context) -> fun(Next) -> callback(Next, Context) end.

%% Runs the `parse/2` function patterns and wraps the result in 
%% using `callback/1` as the next callback to be fired by `mochiweb_multipart`.
callback(Chunk, Context) -> callback(parse(Chunk, Context)).

%% `{headers, ...}` matches the start of a boundary
parse({headers, Headers}, {_Any, Context}) -> headers(Context, Headers);
%% Matches chunks of binary body `Data`.
parse({body, Data}, {Field, Context})      -> body(Field, Context, Data);
%% The end of a form boundary.
parse(body_end, {Field, Context})          -> body_end(Field, Context);
%% No more processing to be done.
parse(eof, {_Any, Context})                -> save(Context).

%% Matches the `content-disposition` header and returns a `tuple` which
%% signifies which pattern for `body/3` to apply.
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

%% No `content-disposition` with a `filename` has been received.
body(file, Context=#con{io=none, path=none}, _Data) ->
    {file, Context};
%% A `content-disposition` header has been received, and a temporary path created,
%% start writing the first chunk of data or error if the temp file cannot be opened.
%% Notifies the progress monitor a new upload has started.
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
%% Write subsequent chunks of data and notify the progress monitor of the size.
body(file, Context=#con{io=IO, done=Done}, Data) ->
    file:write(IO, Data),
    file_updated(Context#con{done=(Done + size(Data))});
%% Apply the binary key to the data for later use for tracking in [[progress.erl]].
body(key, Context, Data) ->
    {done, Context#con{key=Data}};
%% Apply the description text to the upload record.
body(description, Context=#con{upload=Upload}, Data) ->
    {done, Context#con{upload=Upload#upload{description=Data}}};
%% Unknown field was received.
body(_Unknown, Context, _Data) ->
    {done, Context}.

%% End of a boundary received, close any open `IO` handles, and notify 
%% the progress monitor that the file has been completed.
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

%% Helpers for logging and notifying the progress monitor of various events
%% that only apply to any file inputs from the received form data.                                   
file_started(Context=#con{key=Key, upload=Upload}) -> 
    logger:info(Key, "~p bytes pending", [Context#con.total]),
    progress:notify({started, Key, Upload#upload.id}), 
    {file, Context}.

file_updated(Context=#con{key=Key, total=Total, done=Done}) ->
    Percent = percentage(Done, Total),
    logger:info(Key, "~p% of ~p bytes", [Percent, Context#con.total]),
    progress:notify({updated, Key, Percent}), 
    {file, Context}.

file_completed(Context=#con{key=Key}) -> 
    logger:info(Key, "completed"),
    progress:notify({completed, Key}), 
    {file, Context}.

%% Saves the `upload` record using [[datastore.erl]] and gets the
%% `upload_path` to copy the temporary file to.
save(#con{path=Path, upload=Upload}) ->
    To = config:upload_path(datastore:save(Upload)),
    move(Path, To).

%% Ensures the destination directory exists, copies the file to
%% its new location, and deletes the temporary file.
move(Path, To) ->
    ok = filelib:ensure_dir(To),
    {ok, _Size} = file:copy(Path, To),
    ok = file:delete(Path),    
    logger:info("Move", "~s to ~p", [Path, To]).

%% Get the transferred amount of bytes vs the total in a truncated
%% float to send to the progress monitor. Done in a fairly messy fashion
%% due to limitations of Erlang's math module.
percentage(Done, Total) -> truncate((Done / Total) * 100).

truncate(Float) -> 
    [Str] = io_lib:format("~.2f", [Float]),
    {Float1, []} = string:to_float(Str),
    Float1.

%% Create a new context using the request's `Content-Length` as a guideline
%% for the number of bytes that will be transferred.
%% `datastore:build/0` safely generates a new id for the upload record which is
%% used to pass the path prematurely to the client.
new_context(Req) ->
    Total = list_to_integer(Req:get_header_value('Content-Length')),
    #con{total=Total, upload=datastore:build()}.

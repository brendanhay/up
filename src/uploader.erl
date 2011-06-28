%% Provides parsing of multipart/form-data on the calling process
%% via callbacks which are chained/curried based on previous or subsequent data.
%% Updates the registered process of [[progress.erl]] with upload statistics.
%%
-module(uploader).

-export([start/1]).

-include("records.hrl").


start(Req) -> 
    Callback = callback({file, new_context(Req)}),
    mochiweb_multipart:parse_multipart_request(Req, Callback).


%% ### Internal

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

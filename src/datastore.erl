%% `mnesia` wrapper for the `upload` schema.
%% Enables basic querying and `upload` record management functions.
-module(datastore).

-export([start/0, stop/0, find/1, build/0, save/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").

%% ### Starting:
%% Start is called by `up` during the main otp callback.
%%
%% * Create a schema on the current node
%% * Start `mnesia` application
%% * Create a table for the `upload` record
%% All `mnesia` errors are unhandled as these can be safely called repeatedly.
start() ->
    mnesia:create_schema([node()]),
    Start = mnesia:start(),
    create_table(id, record_info(fields, id)),
    create_table(upload, record_info(fields, upload)),
    Start.

% No need to stop mnesia, otp and application:stop/1 manage that
stop() -> ok.

%% Find all stored/completed uploads.
find(all)                    -> mnesia:dirty_match_object(#upload{_ = '_'});
%% Perform a keyword search.
find({keyword, Keyword})     -> execute(fun search/1, [Keyword]);
%% Read a single row matching `Id`.
find(Id) when is_integer(Id) -> execute(fun mnesia:read/1, [{upload, Id}]);
%% Attempt to parse a non-integral `Id`, returning no rows on failure.
%% This is handled via a `404` in [[dispatcher.erl]] when no rows are found.
find(Id) ->
    case string:to_integer(Id) of
        {I, []} -> find(I);
        _Else -> []
    end.

%% Return a new `upload` record with it's `Id` already set, but unsaved.
build() -> #upload{id=next_upload_id(), created=erlang:localtime()}.

%% Save an `upload` record optionally setting its `Id` if none is specified.
%% Performs basic validation of a record.
save(Upload=#upload{name=none}) -> 
    Upload;
save(Upload=#upload{description=none}) -> 
    Upload;
save(Upload=#upload{id=none}) ->
    save(Upload#upload{id=next_upload_id()});
save(Upload=#upload{}) ->
    execute(fun mnesia:write/1, [Upload]),
    Upload.

%% Safely generate a new id for an upload, using a seperate table to store previous ids
%% as `mnesia` has no concept of an auto-incrementing key.
next_upload_id() ->
    mnesia:dirty_update_counter(id, upload, 1).

%% Create a table on the local node, storing a replica on disk.
create_table(Record, Info) ->
    mnesia:create_table(Record, [{disc_copies, [node()]}, {attributes, Info}]).     

%% Execute a `Fun` inside an `mnesia` transaction.
execute(Fun, Args) ->
    {atomic, Result} = mnesia:transaction(Fun, Args),
    Result.

%% Search using the `qlc` query interface performing a case-insensitive 
%% comparison on `Keywords` from `upload.description` field.
search(Keyword) ->
    qlc:eval(qlc:q([U || U <- mnesia:table(upload), match(U, Keyword)])).

match(#upload{description=Desc}, Keyword) ->
    Left = string:to_lower(Desc),
    Right = string:to_lower(Keyword),
    string:str(Left, Right) > 0.

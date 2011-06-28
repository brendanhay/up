-module(datastore).

-export([start/0, stop/0, find/1, build/0, save/1]).
-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").

% Exported

start() ->
    mnesia:create_schema([node()]),
    Start = mnesia:start(),
    create_table(id, record_info(fields, id)),
    create_table(upload, record_info(fields, upload)),
    Start.

% No need to stop mnesia, otp and application:stop/1 manage that
stop() -> ok.

find(all)                    -> mnesia:dirty_match_object(#upload{_ = '_'});
find({keyword, Keyword})     -> execute(fun search/1, [Keyword]);
find(Id) when is_integer(Id) -> execute(fun mnesia:read/1, [{upload, Id}]);
find(Id) ->
    case string:to_integer(Id) of
        {I, []} -> find(I);
        _Else -> []
    end.

build() -> #upload{id=next_upload_id(), created=erlang:localtime()}.

save(Upload=#upload{id=Id}) ->
    NewId = case Id of
                none -> next_upload_id();
                Id -> Id
            end,
    execute(fun mnesia:write/1, [Upload#upload{id=NewId}]),
    Upload.


% Internal

next_upload_id() ->
    mnesia:dirty_update_counter(id, upload, 1).

create_table(Record, Info) ->
    mnesia:create_table(Record, [{disc_copies, [node()]}, {attributes, Info}]).     

execute(Fun, Args) ->
    {atomic, Result} = mnesia:transaction(Fun, Args),
    Result.

search(Keyword) ->
    qlc:eval(qlc:q([U || U <- mnesia:table(upload), match(U, Keyword)])).

match(#upload{description=Desc}, Keyword) ->
    Left = string:to_lower(Desc),
    Right = string:to_lower(Keyword),
    string:str(Left, Right) > 0.

-module(dispatcher).

-export([start/1, stop/0, route/1, 'POST'/2, 'GET'/2]).
-include("records.hrl").

-define(HTML, "text/html").
-define(JS, "application/javascript").

% External

start(Options) ->
    mochiweb_http:start([{name, ?MODULE}, {loop, fun route/1} | Options]).

stop() -> mochiweb_http:stop(?MODULE).


% Routes

'POST'([], Req) ->
    uploader:upload(Req),
    js(Req, "progress", [{percentage, "100.0"}]).

'GET'([], Req) ->
    html(Req, "index", [{title, "Index"}, {uploads, datastore:find(all)}]);

'GET'(["new"], Req) ->
    html(Req, "new", [{title, "Index"}, {description, "Feed me!"}, {key, tracking_key()}]);

'GET'(["progress", Key], Req) ->
    js(Req, "progress", uploader:progress(Key));

'GET'(["show", Id], Req) ->
    case datastore:find(Id) of
        [] -> not_found(Req);
        [Upload] -> serve_upload(Req, Upload)
    end;

'GET'(Resource, Req) -> 
    % Check if the absolute path exists
    case filelib:is_regular(config:www_path(Resource)) of
        true -> Req:serve_file(string:join(Resource, "/"), config:www_path()); 
        false -> not_found(Req)
    end.
    

% Internal

route(Req) ->
    Method = Req:get(method),
    [$/|Path] = Req:get(path),
    logger:info(Method, "~s ~s", [Path, Req:get_header_value("Accept")]),
    try
        ?MODULE:Method(string:tokens(Path, "/"), Req)
    catch
        % TODO: catch non-existing method and route to 501 page
        Type:What -> error(Req, Path, Type, What)            
    end.    

html(Req, View, Vars) -> 
    logger:info("Html", "~s ~300p", [View, Vars]),
    Req:ok({?HTML, [], render:view(View, Vars)}).

js(Req, Partial, Vars) -> 
    logger:info("Js", "~s ~300p", [Partial, Vars]),
    Req:ok({?JS, [], render:partial(Partial, Vars)}).

error(Req, Path, Type, What) ->
    Report = ["web request failed", {path, Path}, {type, Type}, {what, What}, {trace, erlang:get_stacktrace()}],
    logger:error(Report),
    Req:respond({500, [{"Content-Type", ?HTML}], render:view("500", [{title, "Request failed, sorry"}])}).

not_found(Req) ->
    logger:info("404", Req:get(path)),
    Req:respond({404, [{"Content-Type", ?HTML}], render:view("404", [{title, "Route doesn't exist"}])}).

serve_upload(Req, #upload{id=Id, name=Name}) ->
    logger:info("Serve", "~p ~s ~s", [Id, Name,config:uploads_path([integer_to_list(Id)])]),
    Headers = [{"Content-Disposition", string:concat("attachment; filename=", Name)}],
    Req:serve_file(Name, config:uploads_path([integer_to_list(Id)]), Headers).

tracking_key() ->
    Chars = acceptable_chars(),
    CharLength = length(Chars),
    Fun = fun(_, Acc) -> [lists:nth(random:uniform(CharLength), Chars)|Acc] end,
    lists:foldl(Fun, [], lists:seq(1, 12)).

acceptable_chars() ->
    lists:flatten([lists:seq($1, $9), lists:seq($a, $z), lists:seq($A, $Z)]).    


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.

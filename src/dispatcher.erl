%% Semi RESTful event loop which dispatches to internal functions based on the received
%% HTTP verbs and path components.
-module(dispatcher).

-export([start/1, stop/0, route/1, 'POST'/2, 'GET'/2]).

-include("records.hrl").

%% Content-Type macros used in the various responses.
-define(HTML, "text/html").
-define(TEXT, "text/plain").
-define(JS, "application/javascript").

%% Calls `mochiweb_http:start/1` passing the function `route/1` to be executed when
%% an incoming connection is received.
start(Options) ->
    mochiweb_http:start([{name, ?MODULE}, {loop, fun route/1} | Options]).

%% `mochiweb` simply terminates the loop.
stop() -> mochiweb_http:stop(?MODULE).

%% ### Routes:

%% Renders `/templates/index.dtl` with all previous uploads.
'GET'([], Req) ->
    html(Req, "index", [{title, "Index"}, {uploads, datastore:find(all)}]);
%% Renders `/templates/new.dtl` with a default description and a new `tracking_key`.
'GET'(["new"], Req) ->
    html(Req, "new", [{title, "Index"}, {description, "Feed me!"}, {key, tracking_key()}]);
%% Returns a `JSON` response (`/templates/progress.dtl`) containing 
%% the `id` and `percent` completed for a current upload.
'GET'(["progress", Key], Req) ->
    js(Req, "progress", progress:report(Key));
%% Serves an existing `upload` for `Id` via `serve_upload/2` or a `404` page.
'GET'(["show", Id], Req) ->
    case datastore:find(Id) of
        [] -> not_found(Req);
        [Upload] -> serve_upload(Req, Upload)
    end;
%% Serves a static file or a `404` page if no matching resource is found.
'GET'(Resource, Req) -> 
    % Check if the absolute path exists
    case filelib:is_regular(config:www_path(Resource)) of
        true -> Req:serve_file(string:join(Resource, "/"), config:www_path()); 
        false -> not_found(Req)
    end.
    
%% Starts the upload process on the connected socket and returns an empty page when completed.
'POST'([], Req) ->
    uploader:start(Req), Req:respond({200, [{"Content-Type", ?TEXT}], "Upload completed."}).


%% Main event loop handler which is called by `mochiweb` when an incoming connection is received.
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


%% ### HTTP Responses:

%% `200`, `text/html`
html(Req, View, Vars) -> 
    logger:info("Html", "~s ~300p", [View, Vars]),
    Req:ok({?HTML, [], render:view(View, Vars)}).

%% `200`, `application/javascript`
js(Req, Partial, Vars) -> 
    logger:info("Js", "~s ~300p", [Partial, Vars]),
    Req:ok({?JS, [], render:partial(Partial, Vars)}).

%% `500`, `text/html` - renders `templates/500.dtl`
error(Req, Path, Type, What) ->
    Report = ["web request failed", {path, Path}, {type, Type}, {what, What}, {trace, erlang:get_stacktrace()}],
    logger:error(Report),
    Req:respond({500, [{"Content-Type", ?HTML}], render:view("500", [{title, "Request failed, sorry"}])}).

%% `404`, `text/html` - renders `templates/404.dtl`
not_found(Req) ->
    logger:info("404", Req:get(path)),
    Req:respond({404, [{"Content-Type", ?HTML}], render:view("404", [{title, "Route doesn't exist"}])}).

%% When serving `/show/<id>`, return a `Content-Disposition` attachment
%%  header with the file name to force `MSIE` to
%% prompt for download, and push the data over the current `mochiweb` socket.
serve_upload(Req, #upload{id=Id, name=Name}) ->
    logger:info("Serve", "~p ~s ~s", [Id, Name,config:uploads_path([integer_to_list(Id)])]),
    Headers = [{"Content-Disposition", string:concat("attachment; filename=", Name)}],
    Req:serve_file(Name, config:uploads_path([integer_to_list(Id)]), Headers).

%% Generate a psuedo random tracking key which is embedded as the first element
%% in a multipart form. 
%% [[uploader.erl]] uses the key it receives from the upload request to store updates
%% to the stream in [[progress.erl]] and the client uses the key to form a url for polling.
tracking_key() ->
    Chars = acceptable_chars(),
    CharLength = length(Chars),
    Fun = fun(_, Acc) -> [lists:nth(random:uniform(CharLength), Chars)|Acc] end,
    lists:foldl(Fun, [], lists:seq(1, 12)).

%% Generate an alphanumeric sequence of acceptable characters to use in a the tracking key.
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

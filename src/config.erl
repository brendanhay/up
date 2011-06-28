%% @author Mochi Media <dev@mochimedia.com> 
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Ensure that the relatively-installed dependencies are on the code
%%      loading path, and locate resources relative
%%      to this application's path.

-module(config).

-export([ensure/0, ensure/1]).
-export([tmp_path/1, template_path/1, www_path/0, www_path/1, uploads_path/1, upload_path/1]).

-define(TMP, "priv/tmp/").
-define(DATA, "priv/data/").
-define(WWW, "priv/www/").
-define(UPLOADS, ?WWW ++ "uploads/").
-define(TEMPLATES, "templates/").

-include("records.hrl").

%% @spec ensure() -> ok
%% @doc Ensure that the ebin and include paths for dependencies of
%%      this application are on the code path. Equivalent to
%%      ensure(?Module).
ensure() ->
    application:set_env(mnesia, dir, local_path([?DATA])),
    ensure(?MODULE).

%% @spec ensure(Module) -> ok
%% @doc Ensure that all ebin and include paths for dependencies
%%      of the application for Module are on the code path.
ensure(Module) ->
    code:add_paths(new_siblings(Module)),
    code:clash(),
    ok.

tmp_path(Parts) -> local_path([?TMP|Parts]).

template_path(View) -> local_path([?TEMPLATES, View ++ ".dtl"]).

www_path() -> www_path([]).

www_path(Parts) -> local_path([?WWW|Parts]).

upload_path(#upload{id=Id, name=Name}) -> uploads_path([integer_to_list(Id), Name]).

uploads_path(Parts) -> local_path([?UPLOADS|Parts]).


% Internal

local_path(Parts, Module) -> filename:join([get_base_dir(Module) | Parts]).

local_path(Parts) -> local_path(Parts, ?MODULE).

get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

deps_on_path() ->
    F = fun (X, Acc) ->
                ProjDir = filename:dirname(X),
                case {filename:basename(X),
                      filename:basename(filename:dirname(ProjDir))} of
                    {"ebin", "deps"} ->
                        [filename:basename(ProjDir) | Acc];
                    _ ->
                        Acc
                end
        end,
    ordsets:from_list(lists:foldl(F, [], code:get_path())).

new_siblings(Module) ->
    Existing = deps_on_path(),
    SiblingEbin = filelib:wildcard(local_path(["deps", "*", "ebin"], Module)),
    Siblings = [filename:dirname(X) || X <- SiblingEbin,
                           ordsets:is_element(
                             filename:basename(filename:dirname(X)),
                             Existing) =:= false],
    lists:filter(fun filelib:is_dir/1,
                 lists:append([[filename:join([X, "ebin"]),
                                filename:join([X, "include"])] ||
                                  X <- Siblings])).



%% Wrapper for `ErlyDTL` which enables:
%%
%% * Basic application layouts
%% * Nested templates
%% * Forced recompilation during development
-module(render).

-export([view/1, view/2, layout/1, partial/1, partial/2]).
-define(DEVELOPMENT, true).

%% Views are always rendered inside the application layout,
%% and the `{title, Title}` tuple is passed upwards.
view(Name) -> view(Name, []).

view(Name, Vars) ->
    [Title, Vars1] = title(Vars),
    Partial = partial(Name, Vars1),
    layout([Title, {content, Partial}]).

%% Renders the `templates/application.dtl` template with no parent.
layout(Vars) -> partial("application", Vars).

%% Renders a template with no parent.
partial(Name) -> partial(Name, []).

partial(Name, Vars) ->  
    Module = compile(Name),
    {ok, Content} = Module:render(Vars),
    Content.

%% Forces recompilation of templates during development everytime
%% they are requested, as opposed to being compiled once.
-ifdef(DEVELOPMENT).
compile(View) ->
    Path = config:template_path(View),
    Module = module_name(View),
    ok = erlydtl:compile(Path, Module),
    logger:info("Compile", "~s", [Path]),
    Module.
-else.
compile(View) -> module_name(View).
-endif.

%% Get the compiled template name, which by default when compiling
%% with `rebar` always ends in `_dtl`.
module_name(Name) -> 
    View = view_name(Name),
    list_to_atom(string:concat(View, "_dtl")).

%% Return `index` as the default if no view is specified.
view_name([])                            -> "index";
%% For a proper string, return `Name` unmodified.
view_name(Name=[C|_]) when is_integer(C) -> Name;
%% For a nested list, return the first item as `Name`.
view_name([Name|_])                      -> Name.

%% Check if the title supplied in `Vars` exists, and do one of the following:
%%
%% * Supply a default of ""
%% * Apply a format string any arguments are specified in the triple
%% * Return the specified title
title(Vars) ->
    K = title,
    Updated = case lists:keyfind(K, 1, Vars) of
        false             -> {K, ""};
        {K, Title, []}    -> {K, Title};
        {K, Format, Args} -> {K, io_lib:fwrite(Format, Args)};
        {K, Title}        -> {K, Title}
    end,
    [Updated, lists:keyreplace(K, 1, Vars, Updated)].


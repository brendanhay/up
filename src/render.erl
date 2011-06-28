-module(render).

-export([view/1, view/2, layout/1, partial/1, partial/2]).
-define(DEVELOPMENT, true).

view(Name) -> view(Name, []).

view(Name, Vars) ->
    [Title, Vars1] = title(Vars),
    Partial = partial(Name, Vars1),
    layout([Title, {content, Partial}]).

layout(Vars) -> partial("application", Vars).

partial(Name) -> partial(Name, []).

partial(Name, Vars) ->  
    Module = compile(Name),
    {ok, Content} = Module:render(Vars),
    Content.

% Internal

% Development hack to recompile templates
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

module_name(Name) -> 
    View = view_name(Name),
    list_to_atom(string:concat(View, "_dtl")).

view_name([])                            -> "index";
view_name(Name=[C|_]) when is_integer(C) -> Name;
view_name([Name|_])                      -> Name.

title(Vars) ->
    K = title,
    Updated = case lists:keyfind(K, 1, Vars) of
        false             -> {K, ""};
        {K, Title, []}    -> {K, Title};
        {K, Format, Args} -> {K, io_lib:fwrite(Format, Args)};
        {K, Title}        -> {K, Title}
    end,
    [Updated, lists:keyreplace(K, 1, Vars, Updated)].


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-endif.

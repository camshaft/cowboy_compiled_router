-module(cowboy_compiled_router).

-export([parse_transform/2]).
-export([compile/1]).

parse_transform(Forms, _Options) ->
  {ok, Patterns, [File, Module|Forms2]} = extract_patterns(Forms, [], [], '_'),
  {ok, Compiled} = compile(Patterns),
  {ok, Clauses} = to_clauses(Compiled, []),
  [_, Exports, MatchRequest|FunsRest] = template(),
  MatchRequest2 = add_clauses(Clauses, MatchRequest),
  [EOF|Forms3] = lists:reverse(Forms2),
  Forms4 = [File, Module, Exports] ++ lists:reverse(Forms3) ++ [MatchRequest2] ++ FunsRest ++ [EOF],
  Forms4.

extract_patterns([], Patterns, Forms, _Host) ->
  {ok, lists:reverse(Patterns), lists:reverse(Forms)};
extract_patterns([{attribute,_Line,host,Host}|Rest], Patterns, Forms, _) ->
  extract_patterns(Rest, Patterns, Forms, Host);
extract_patterns([{attribute,Line,Method,Conf}|Rest], Patterns, Forms, Host) when Method =:= all orelse Method =:= handle ->
  extract_patterns(Rest, [{'_', Host, Conf, Line}|Patterns], Forms, Host);
extract_patterns([{attribute,Line,Method,Conf}|Rest], Patterns, Forms, Host) when Method =:= get orelse Method =:= post orelse
                                                           Method =:= put orelse Method =:= delete ->
  extract_patterns(Rest, [{method_to_string(Method), Host, Conf, Line}|Patterns], Forms, Host);
extract_patterns([Other|Rest], Patterns, Forms, Host) ->
  extract_patterns(Rest, Patterns, [Other|Forms], Host).

method_to_string(Method) ->
  string:to_upper(atom_to_list(Method)).

compile(Patterns) ->
  {ok, lists:foldl(fun(Pattern, Acc) ->
    Acc ++ compile_path(Pattern)
  end, [], Patterns)}.

compile_path({Method, {Path, Module}}) ->
  compile_path({Method, '_', {Path, Module, []}});
compile_path({Method, {Path, Module, Args}}) ->
  compile_path({Method, '_', {Path, Module, Args}});
compile_path({Method, Host, {Path, Module}}) ->
  compile_path({Method, Host, {Path, Module, []}});
compile_path({Method, {Path, Module}, Line}) ->
  compile_path({Method, '_', {Path, Module, []}, Line});
compile_path({Method, {Path, Module, Args}, Line}) ->
  compile_path({Method, '_', {Path, Module, Args}, Line});
compile_path({Method, Host, {Path, Module}, Line}) ->
  compile_path({Method, Host, {Path, Module, []}, Line});
compile_path({Method, Host, {Path, Module, Args}, Line}) ->
  Routes = [
    {Host, [{Path, noop, []}]}
  ],
  lists:foldl(fun({HostParts, _, PathPatterns}, Acc) ->
    Acc ++ [{Method, HostParts, PathParts, Module, Args, Line} || {PathParts, [], noop, []} <- PathPatterns]
  end, [], cowboy_compiled_router_parser:parse(Routes)).

to_clauses([], Acc) ->
  {ok, lists:reverse(Acc)};
to_clauses([{Method, Host, Path, Module, Args, Line}|Rest], Acc) ->
  HostPattern = to_binding(Host, Line),
  PathPattern = to_binding(Path, Line),
  Clause = {
   clause,Line,
    [to_method(Method, Line),
     {match,Line,HostPattern,{var,Line,'__HostInfo'}},
     {match,Line,PathPattern,{var,Line,'__PathInfo'}}],
    [],
    [{tuple,Line,[{atom,Line,ok},
                  {atom,Line,Module},
                  to_args(Args,Line),
                  to_bindings(Host, Path, Line),
                  {var,Line,'__HostInfo'},
                  {var,Line,'__PathInfo'}]}]
  },
  to_clauses(Rest, [Clause|Acc]).

add_clauses(Clauses, Fun) ->
  Default = element(5, Fun),
  setelement(5, Fun, Clauses ++ Default).

to_bindings('_', Path, Line) ->
  to_bindings(Path, Line);
to_bindings(Host, Path, Line) ->
  to_bindings(Host ++ Path, Line).

to_bindings([], Line) ->
  {nil,Line};
to_bindings(['...'|Rest], Line) ->
  to_bindings(Rest, Line);
to_bindings([Part|Rest], Line) when is_atom(Part) ->
  {cons,Line,{tuple,Line,[{atom,Line,Part},{var,Line,Part}]}, to_bindings(Rest, Line)};
to_bindings([_|Rest], Line) ->
  to_bindings(Rest, Line).

to_method('_', Line) ->
  {var,Line,'_'};
to_method(Method, Line) when is_binary(Method) ->
  to_method(binary_to_list(Method), Line);
to_method(Method, Line) when is_list(Method) ->
  to_bin(Method, Line).

to_args(Args, _Line) ->
  erl_parse:abstract(Args).

to_binding('_', Line) ->
  {var,Line,'_'};
to_binding([], Line) ->
  {nil,Line};
to_binding([Part|Rest], Line) when is_binary(Part) ->
  to_binding([binary_to_list(Part)|Rest], Line);
to_binding([Part|Rest], Line) when is_list(Part) ->
  {cons,Line,to_bin(Part,Line),to_binding(Rest, Line)};
to_binding(['...'|_], Line) ->
  {var,Line,'_'};
to_binding([Part|Rest], Line) when is_atom(Part) ->
  {cons,Line,{var,Line,Part},to_binding(Rest, Line)}.

to_bin(Contents, Line) ->
  {bin,Line,[{bin_element,Line,{string,Line,Contents},default,default}]}.

template() ->
  {ok, Bin} = file:read_file(privdir:get(?MODULE) ++ "/template.erl"),
  {ok, Tokens, _} = erl_scan:string(binary_to_list(Bin)),
  parse_forms(Tokens).

parse_forms(Tokens) ->
  parse_forms(Tokens, [], []).

parse_forms([], [], Forms) ->
  lists:reverse(Forms);
parse_forms([{dot,_} = Dot|Rest], Acc, Forms) ->
  {ok, Form} = erl_parse:parse_form(lists:reverse([Dot|Acc])),
  parse_forms(Rest, [], [Form|Forms]);
parse_forms([Token|Rest], Acc, Forms) ->
  parse_forms(Rest, [Token|Acc], Forms).

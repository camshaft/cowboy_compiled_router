-module(cowboy_compiled_router).

-export([parse_transform/2]).
-export([compile/1]).

parse_transform(Forms, _Options) ->
  {ok, Patterns, [File, Module|Forms2]} = extract_patterns(Forms, [], [], '_'),
  {ok, Compiled} = compile(Patterns),
  {ok, Clauses} = to_clauses(Compiled, []),
  {ok, Resolved} = to_resolve_clauses(Compiled, [], []),
  [_, Exports, MatchParts, Resolve, DefaultExtractAction|FunsRest] = template(),
  ExtractAction = case lists:keyfind(extract_action,3,Forms2) of
    false -> [DefaultExtractAction];
    _ -> []
  end,
  MergedMatches = add_clauses(Clauses, MatchParts),
  MergedResolve = add_clauses(Resolved, Resolve),
  [EOF|Forms3] = lists:reverse(Forms2),
  Forms4 = [File, Module, Exports] ++ lists:reverse(Forms3) ++ [MergedMatches, MergedResolve] ++ ExtractAction ++ FunsRest ++ [EOF],
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

unique_clauses(Clauses) ->
  Filtered = lists:foldl(fun({ID, Clause}, Acc) ->
    lists:keystore(ID, 1, Acc, {ID, Clause})
  end, [], Clauses),
  [Clause || {_, Clause} <- lists:reverse(Filtered)].

to_resolve_clauses([], Clauses, Secondary) ->
  {ok, unique_clauses(Clauses) ++ unique_clauses(Secondary)};
to_resolve_clauses([{Method, Host, Path, Module, Args, Line}|Rest], Acc, SecondaryAcc) ->
  ResolvedMethod = case Method of
    '_' -> {atom,Line,'_'};
     _ -> to_bin(Method, Line)
  end,
  Fields = to_fields(Host, Path, Line),
  Action = get_action(Args, {atom,Line,undefined}),
  Res = case get_action(Args, false) of
    false ->
      [{tuple,Line,[{atom,Line,ok},ResolvedMethod,resolve_binding(Host, '__HostInfo', Line),resolve_binding(Path, '__PathInfo', Line)]}];
    Action ->
      [{tuple,Line,[{atom,Line,ok},ResolvedMethod,resolve_binding(Host, '__HostInfo', Line),resolve_binding(Path, '__PathInfo', Line),Action]}]
  end,

  ID = erlang:phash2([Module, filter_bindings(Host), filter_bindings(Path)]),

  MapClause = {erlang:phash2([ID, map]), {
    clause,Line,
    [{atom,Line,Module},
     {map,Line,Fields}],
    [],
    Res
  }},

  ListClause = {erlang:phash2([ID, list]), {
    clause,Line,
    [{atom,Line,Module},
     to_fields_list(Host, Path, Line)],
    [],
    Res
  }},

  case length(Fields) of
    0 ->
      to_resolve_clauses(Rest, [MapClause, ListClause|Acc], SecondaryAcc);
    _ ->
      ArgsList = to_args([Atom || Atom <- merge_lists(Host, Path), is_atom(Atom)], Line),
      Secondary = {erlang:phash2([ID, secondary]), {
        clause,Line,
        [{atom,Line,Module},
         {var,Line,'Params'}],
        [],
        [{call,Line,{atom,Line,resolve_error},[{atom,Line,Module},{var,Line,'Params'},ArgsList]}]
       }},
      to_resolve_clauses(Rest, [MapClause, ListClause|Acc], [Secondary|SecondaryAcc])
  end.

filter_bindings(Arg) when is_atom(Arg) ->
  [];
filter_bindings(List) when is_list(List) ->
  lists:sort([L || L <- List, is_atom(L)]).

resolve_binding(Part, Var, Line) ->
  case Part of
    '_' -> {atom,Line,'_'};
     _ -> to_binding(Part, Var, Line, atom)
  end.

merge_lists('_', Path) ->
  Path;
merge_lists(Host, Path) ->
  Host ++ Path.

to_fields(Host, Path, Line) ->
  to_fields(merge_lists(Host, Path), Line).

to_fields([], _Line) ->
  [];
to_fields(['...'|Rest], Line) ->
  to_fields(Rest, Line);
to_fields([Part|Rest], Line) when is_atom(Part) ->
  [{map_field_exact,Line,{atom,Line,Part},{var,10,Part}}|to_fields(Rest, Line)];
to_fields([_|Rest], Line) ->
  to_fields(Rest, Line).

to_fields_list(Host, Path, Line) ->
  to_fields_list(merge_lists(Host, Path), Line).

to_fields_list([], Line) ->
  {nil,Line};
to_fields_list(['...'|Rest], Line) ->
  to_fields_list(Rest, Line);
to_fields_list([Part|Rest], Line) when is_atom(Part) ->
  {cons,Line,{var,Line,Part}, to_fields_list(Rest, Line)};
to_fields_list([_|Rest], Line) ->
  to_fields_list(Rest, Line).

to_clauses([], Acc) ->
  {ok, lists:reverse(Acc)};
to_clauses([{Method, Host, Path, Module, Args, Line}|Rest], Acc) ->
  Action = get_action(Args, {var,Line,'_'}),
  HostPattern = to_binding(Host, '__HostInfo', Line),
  PathPattern = to_binding(Path, '__PathInfo', Line),
  Clause = {
   clause,Line,
    [to_method(Method, Line),
     HostPattern,
     PathPattern,
     Action],
    [],
    [{tuple,Line,[{atom,Line,ok},
                  {atom,Line,Module},
                  to_args(Args,Line),
                  to_bindings(Host, Path, Line),
                  to_info_var(Host, '__HostInfo', Line),
                  to_info_var(Path, '__PathInfo', Line)]}]
  },
  to_clauses(Rest, [Clause|Acc]).

to_info_var(Tokens, Var, Line) when is_list(Tokens) ->
  case lists:member('...', Tokens) of
    true ->
      {var, Line, Var};
    _ ->
      {nil, Line}
  end;
to_info_var(_, _, Line) ->
  {nil, Line}.

get_action(Args, Default) ->
  case catch fast_key:get(action, Args) of
    undefined ->
      Default;
    {'EXIT', _} ->
      Default;
    A ->
      erl_parse:abstract(A)
  end.

add_clauses(Clauses, Fun) ->
  Default = element(5, Fun),
  setelement(5, Fun, Clauses ++ Default).

to_bindings(Host, Path, Line) ->
  to_bindings(merge_lists(Host, Path), Line).

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

to_binding(Parts, InfoVar, Line) ->
  to_binding(Parts, InfoVar, Line, var).

to_binding('_', _, Line, var) ->
  {var,Line,'_'};
to_binding('_', _, Line, atom) ->
  {cons,Line,{atom,Line,'_'},{nil,Line}};
to_binding([], _, Line, _) ->
  {nil,Line};
to_binding([Part|Rest], InfoVar, Line, Out) when is_binary(Part) ->
  to_binding([binary_to_list(Part)|Rest], InfoVar, Line, Out);
to_binding([Part|Rest], InfoVar, Line, Out) when is_list(Part) ->
  {cons,Line,to_bin(Part,Line),to_binding(Rest, InfoVar, Line, Out)};
to_binding(['...'|_], InfoVar, Line, var) ->
  {var,Line,InfoVar};
to_binding(['...'|Rest], InfoVar, Line, atom) ->
  {cons,Line,{atom,Line,'...'},to_binding(Rest, InfoVar, Line, atom)};
to_binding([Part|Rest], InfoVar, Line, Out) when is_atom(Part) ->
  {cons,Line,{var,Line,Part},to_binding(Rest, InfoVar, Line, Out)}.

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

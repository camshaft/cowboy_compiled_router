-module(cowboy_compiled_router_parser).
-export([parse/1]).

%% graciously stolen from https://github.com/ninenines/cowboy/blob/999dc5b7c1665fb620c14f6303610793313efe58/src/cowboy_router.erl#L51

parse(Routes) ->
  compile(Routes, []).

compile([], Acc) ->
  lists:reverse(Acc);
compile([{Host, Paths}|Tail], Acc) ->
  compile([{Host, [], Paths}|Tail], Acc);
compile([{HostMatch, Fields, Paths}|Tail], Acc) ->
  HostRules = case HostMatch of
		'_' -> '_';
		_ -> compile_host(HostMatch)
              end,
  PathRules = compile_paths(Paths, []),
  Hosts = case HostRules of
            '_' -> [{'_', Fields, PathRules}];
            _ -> [{R, Fields, PathRules} || R <- HostRules]
          end,
  compile(Tail, Hosts ++ Acc).

compile_host(HostMatch) when is_list(HostMatch) ->
  compile_host(list_to_binary(HostMatch));
compile_host(HostMatch) when is_binary(HostMatch) ->
  compile_rules(HostMatch, $., [], [], <<>>).

compile_paths([], Acc) ->
  lists:reverse(Acc);
compile_paths([{PathMatch, Handler, Opts}|Tail], Acc) ->
  compile_paths([{PathMatch, [], Handler, Opts}|Tail], Acc);
compile_paths([{PathMatch, Fields, Handler, Opts}|Tail], Acc)
  when is_list(PathMatch) ->
  compile_paths([{iolist_to_binary(PathMatch),
                  Fields, Handler, Opts}|Tail], Acc);
compile_paths([{'_', Fields, Handler, Opts}|Tail], Acc) ->
  compile_paths(Tail, [{'_', Fields, Handler, Opts}] ++ Acc);
compile_paths([{<< $/, PathMatch/bits >>, Fields, Handler, Opts}|Tail],
              Acc) ->
  PathRules = compile_rules(PathMatch, $/, [], [], <<>>),
  Paths = [{lists:reverse(R), Fields, Handler, Opts} || R <- PathRules],
  compile_paths(Tail, Paths ++ Acc);
compile_paths([{PathMatch, _, _, _}|_], _) ->
  error({badarg, "The following route MUST begin with a slash: "
         ++ binary_to_list(PathMatch)}).

compile_rules(<<>>, _, Segments, Rules, <<>>) ->
  [Segments|Rules];
compile_rules(<<>>, _, Segments, Rules, Acc) ->
  [[Acc|Segments]|Rules];
compile_rules(<< S, Rest/bits >>, S, Segments, Rules, <<>>) ->
  compile_rules(Rest, S, Segments, Rules, <<>>);
compile_rules(<< S, Rest/bits >>, S, Segments, Rules, Acc) ->
  compile_rules(Rest, S, [Acc|Segments], Rules, <<>>);
compile_rules(<< $:, Rest/bits >>, S, Segments, Rules, <<>>) ->
  {NameBin, Rest2} = compile_binding(Rest, S, <<>>),
  Name = binary_to_atom(NameBin, utf8),
  compile_rules(Rest2, S, Segments, Rules, Name);
compile_rules(<< $:, _/bits >>, _, _, _, _) ->
  error(badarg);
compile_rules(<< $[, $., $., $., $], Rest/bits >>, S, Segments, Rules, Acc)
  when Acc =:= <<>> ->
  compile_rules(Rest, S, ['...'|Segments], Rules, Acc);
compile_rules(<< $[, $., $., $., $], Rest/bits >>, S, Segments, Rules, Acc) ->
  compile_rules(Rest, S, ['...', Acc|Segments], Rules, Acc);
compile_rules(<< $[, S, Rest/bits >>, S, Segments, Rules, Acc) ->
  compile_brackets(Rest, S, [Acc|Segments], Rules);
compile_rules(<< $[, Rest/bits >>, S, Segments, Rules, <<>>) ->
  compile_brackets(Rest, S, Segments, Rules);
%% Open bracket in the middle of a segment.
compile_rules(<< $[, _/bits >>, _, _, _, _) ->
  error(badarg);
%% Missing an open bracket.
compile_rules(<< $], _/bits >>, _, _, _, _) ->
  error(badarg);
compile_rules(<< C, Rest/bits >>, S, Segments, Rules, Acc) ->
  compile_rules(Rest, S, Segments, Rules, << Acc/binary, C >>).

%% Everything past $: until the segment separator ($. for hosts,
%% $/ for paths) or $[ or $] or end of binary is the binding name.
compile_binding(<<>>, _, <<>>) ->
  error(badarg);
compile_binding(Rest = <<>>, _, Acc) ->
  {Acc, Rest};
compile_binding(Rest = << C, _/bits >>, S, Acc)
  when C =:= S; C =:= $[; C =:= $] ->
  {Acc, Rest};
compile_binding(<< C, Rest/bits >>, S, Acc) ->
  compile_binding(Rest, S, << Acc/binary, C >>).

compile_brackets(Rest, S, Segments, Rules) ->
  {Bracket, Rest2} = compile_brackets_split(Rest, <<>>, 0),
  Rules1 = compile_rules(Rest2, S, Segments, [], <<>>),
  Rules2 = compile_rules(<< Bracket/binary, Rest2/binary >>,
                         S, Segments, [], <<>>),
  Rules ++ Rules2 ++ Rules1.

%% Missing a close bracket.
compile_brackets_split(<<>>, _, _) ->
  error(badarg);
%% Make sure we don't confuse the closing bracket we're looking for.
compile_brackets_split(<< C, Rest/bits >>, Acc, N) when C =:= $[ ->
  compile_brackets_split(Rest, << Acc/binary, C >>, N + 1);
compile_brackets_split(<< C, Rest/bits >>, Acc, N) when C =:= $], N > 0 ->
  compile_brackets_split(Rest, << Acc/binary, C >>, N - 1);
%% That's the right one.
compile_brackets_split(<< $], Rest/bits >>, Acc, 0) ->
  {Acc, Rest};
compile_brackets_split(<< C, Rest/bits >>, Acc, N) ->
  compile_brackets_split(Rest, << Acc/binary, C >>, N).

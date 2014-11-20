-module(template).

-export([execute/2,match/3,match/4,resolve/1,resolve/2]).

match_parts(_, _, badpath, _) ->
  {error, badrequest};
match_parts(_, _, _, _) ->
  {error, notfound}.

resolve(Module, Map) ->
  resolve_error(Module, Map, notfound).

%% default behavior
extract_action(Req, Env) ->
  {undefined, Req, Env}.

resolve(Module) ->
  resolve(Module, #{}).

resolve_error(_Module, Map, Expected) when is_list(Expected) ->
  {error, {missing_args, [A || A <- Expected, not maps:is_key(A, Map)]}};
resolve_error(_Module, _Map, Error) ->
  {error, Error}.

execute(Req, Env) ->
  [Method, Host, Path] = cowboy_req:get([method, host, path], Req),
  {Action, Req2, Env2} = extract_action(Req, Env),
  case match(Method, Host, Path, Action) of
    {ok, Handler, HandlerOpts, Bindings, HostInfo, PathInfo} ->
      Req3 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req2),
      Env3 = [{handler, Handler}, {handler_opts, HandlerOpts}|Env2],
      {ok, Req3, Env3};
    {error, notfound} ->
      {error, 404, Req2};
    {error, badrequest} ->
      {error, 400, Req2}
  end.

match(Method, Host, Path) ->
  match(Method, Host, Path, undefined).

match(Method, Host, <<>>, Action) ->
  match_parts(Method, split_host(Host, []), [], Action);
match(Method, Host, <<"/">>, Action) ->
  match_parts(Method, split_host(Host, []), [], Action);
match(Method, Host, <<"/", Path/binary>>, Action) ->
  match_parts(Method, split_host(Host, []), split_path(Path, []), Action);
match(_, _, _, _) ->
  {error, badrequest}.

split_path(<<"/", Path/binary>>, Acc) ->
  split_path(Path, Acc);
split_path(Path, Acc) ->
  try
    case binary:match(Path, <<"/">>) of
      nomatch when Path =:= <<>> ->
        lists:reverse([cow_qs:urldecode(S) || S <- Acc]);
      nomatch ->
        lists:reverse([cow_qs:urldecode(S) || S <- [Path|Acc]]);
      {Pos, _} ->
        << Segment:Pos/binary, _:8, Rest/bits >> = Path,
        split_path(Rest, [Segment|Acc])
    end
  catch
    error:badarg ->
      badrequest
  end.

split_host(Host, Acc) ->
  case binary:match(Host, <<".">>) of
    nomatch when Host =:= <<>> ->
      Acc;
    nomatch ->
      [Host|Acc];
    {Pos, _} ->
      << Segment:Pos/binary, _:8, Rest/bits >> = Host,
      false = byte_size(Segment) == 0,
      split_host(Rest, [Segment|Acc])
  end.

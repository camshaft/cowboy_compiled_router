-module(template).

-export([execute/2,match/3,resolve/1,resolve/2]).

match_parts(_, _, badpath) ->
  {error, badrequest};
match_parts(_, _, _) ->
  {error, notfound}.

resolve(Module, Map) ->
  resolve_error(Module, Map, notfound).

resolve(Module) ->
  resolve(Module, #{}).

resolve_error(_Module, Map, Expected) when is_list(Expected) ->
  {error, {missing_args, [A || A <- Expected, not maps:is_key(A, Map)]}};
resolve_error(_Module, _Map, Error) ->
  {error, Error}.

execute(Req, Env) ->
  [Method, Host, Path] = cowboy_req:get([method, host, path], Req),
  case match(Method, Host, Path) of
    {ok, Handler, HandlerOpts, Bindings, HostInfo, PathInfo} ->
      Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
      Env2 = [{handler, Handler}, {handler_opts, HandlerOpts}|Env],
      {ok, Req2, Env2};
    {error, notfound} ->
      {error, 404, Req};
    {error, badrequest} ->
      {error, 400, Req}
  end.

match(Method, Host, <<>>) ->
  match_parts(Method, split_host(Host, []), []);
match(Method, Host, <<"/">>) ->
  match_parts(Method, split_host(Host, []), []);
match(Method, Host, <<"/", Path/binary>>) ->
  match_parts(Method, split_host(Host, []), split_path(Path, []));
match(_, _, _) ->
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

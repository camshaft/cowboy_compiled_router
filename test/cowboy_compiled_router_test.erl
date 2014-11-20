-module(cowboy_compiled_router_test).

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform,cowboy_compiled_router}).

-get({"/anyhost", any_host_resource}).

-post({"/create", first_create_resource, [{action, first}]}).
-post({"/create", second_create_resource, [{action, second}]}).

-host("example.com").
  -get({"/", root_resource}).
  -get({"/users", users_resource, [{users, [<<"joe">>, <<"mike">>, <<"robert">>]}]}).
  -post({"/users", create_users_resource, []}).
  -get({"/users/:user", user_resource, []}).

-host("[...].example.co").
  -all({"/catchall", catchall_resource}).
  -get({"/optional/[:args]", optional_args}).

-host(":sub.example.com").
  -get({"/", sub_root_resource}).
  -get({"/home/[...]", rest_resource}).

-host("[:optionaldomain].other.com").
  -get({"/", other_domain}).

extract_action(Req, Env) ->
  {foo, Req, Env}.

match_test_() ->
  Tests = [
    {"GET", "foo.com", "/anyhost",
     {ok, any_host_resource, [], [], [<<"com">>, <<"foo">>], [<<"anyhost">>]}},
    {"GET", "foo.com", "///////anyhost//////",
     {ok, any_host_resource, [], [], [<<"com">>, <<"foo">>], [<<"anyhost">>]}},
    {"POST", "localhost", "/create", first,
     {ok, first_create_resource, [{action,first}], [], [<<"localhost">>], [<<"create">>]}},
    {"POST", "localhost", "/create", second,
     {ok, second_create_resource, [{action,second}], [], [<<"localhost">>], [<<"create">>]}},
    {"GET", "foo.com", "/anyhost1",
     {error, notfound}},
    {"GET", "example.com", "/users",
     {ok, users_resource, [{users, [<<"joe">>, <<"mike">>, <<"robert">>]}], [], [<<"com">>, <<"example">>], [<<"users">>]}},
    {"GET", "example.com", "/users/123",
     {ok, user_resource, [], [{user,<<"123">>}], [<<"com">>, <<"example">>], [<<"users">>, <<"123">>]}},
    {"ANYTHING", "can.go.here.example.co", "/catchall",
     {ok, catchall_resource, [], [], [<<"co">>, <<"example">>, <<"here">>, <<"go">>, <<"can">>], [<<"catchall">>]}},
    {"GET", "example.co", "/optional",
     {ok, optional_args, [], [], [<<"co">>, <<"example">>], [<<"optional">>]}},
    {"GET", "example.co", "/optional/arg",
     {ok, optional_args, [], [{args,<<"arg">>}], [<<"co">>, <<"example">>], [<<"optional">>, <<"arg">>]}},
    {"GET", "foo.example.com", "/",
     {ok, sub_root_resource, [], [{sub,<<"foo">>}], [<<"com">>, <<"example">>, <<"foo">>], []}},
    {"GET", "foo.example.com", "/home",
     {ok, rest_resource, [], [{sub,<<"foo">>}], [<<"com">>, <<"example">>, <<"foo">>], [<<"home">>]}},
    {"GET", "foo.example.com", "/home/1/2/3",
     {ok, rest_resource, [], [{sub,<<"foo">>}], [<<"com">>, <<"example">>, <<"foo">>], [<<"home">>,<<"1">>,<<"2">>,<<"3">>]}},
    {"GET", "bar.other.com", "/",
     {ok, other_domain, [], [{optionaldomain,<<"bar">>}], [<<"com">>, <<"other">>, <<"bar">>], []}}
  ],
  [{element(1,Test) ++ " " ++ element(2,Test) ++ element(3,Test), fun() ->
    case Test of
      {Method, Host, Path, Out} ->
        ?assertEqual(Out, match(list_to_binary(Method), list_to_binary(Host), list_to_binary(Path)));
      {Method, Host, Path, Action, Out} ->
        ?assertEqual(Out, match(list_to_binary(Method), list_to_binary(Host), list_to_binary(Path), Action))
    end
  end} || Test <- Tests].

resolve_test_() ->
  Tests = [
    {users_resource, [],
      {ok, <<"GET">>,[<<"com">>, <<"example">>],[<<"users">>]}},
    {user_resource, [<<"123">>],
      {ok, <<"GET">>,[<<"com">>, <<"example">>],[<<"users">>, <<"123">>]}},
    {user_resource, #{user => <<"123">>},
      {ok, <<"GET">>,[<<"com">>, <<"example">>],[<<"users">>, <<"123">>]}},
    {user_resource, #{other => <<"123">>},
      {error, {missing_args,[user]}}},
    {catchall_resource, [],
      {ok, '_', [<<"co">>, <<"example">>, '...'], [<<"catchall">>]}}
  ],
  [{lists:flatten(io_lib:format("~p ~p", [Module, Args])), fun() ->
    ?assertEqual(Out, resolve(Module, Args))
  end} || {Module, Args, Out} <- Tests].

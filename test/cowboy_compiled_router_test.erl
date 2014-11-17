-module(cowboy_compiled_router_test).

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform,cowboy_compiled_router}).

-get({"/anyhost", any_host_resource}).

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

router_test_() ->
  Tests = [
    {"GET", "foo.com", "/anyhost",
     {ok, any_host_resource, [], [], [<<"com">>, <<"foo">>], [<<"anyhost">>]}},
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
  [{Method ++ " " ++ Host ++ Path, fun() ->
    ?assertEqual(Out, match(list_to_binary(Method), list_to_binary(Host), list_to_binary(Path)))
  end} || {Method, Host, Path, Out} <- Tests].

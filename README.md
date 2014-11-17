cowboy_compiled_router
======================

compiled router for cowboy

Example
-------

Create a module with the parse_transform:

```erlang
-module(my_router).

-compile({parse_transform, cowboy_compiled_router}).

-get({"/", root_handler}).
-get({"/about-us", about_us_handler}).

-host("example.com").
  -post({"/", root_create_handler}).
  -get({"/users", users_handler, [{users, [<<"joe">>, <<"mike">>, <<"robert">>]}]}).
  -get({"/users/:user", user_handler}).

-host(":user.example.com").
  -get({"/", user_handler}).

-host('_').
  -handle({"/[...]", catchall_handler}).
```

Use `my_router` instead of `cowboy_router`:

```erlang
{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
  {middlewares, [
    my_router,
    cowboy_handler
  ]}
]),
```

Tests
-----

```sh
$ make test
```

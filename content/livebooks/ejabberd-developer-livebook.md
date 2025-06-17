# The ejabberd Developer Livebook


<!-- WARNING:: Don't edit this file. This is generated from the *.livemd, please edit that one instead -->

!!! info
    This page is designed to run interactively using [Livebook](https://livebook.dev/).
    Of course, you could simply reproduce the instructions manually yourself.
    But, if possible, install Livebook in your machine and get the full experience
    clicking on the button:

    [![Run in Livebook](https://livebook.dev/badge/v1/blue.svg)](https://livebook.dev/run/?url=https://processone.github.io/mkdocs/livebooks/ejabberd-developer-livebook.livemd)

```elixir
filename = "ejabberd.yml"

if File.exists?(filename) do
  Mix.install([
    {:ejabberd, "~> 24.2"},
    {:kino, "~> 0.12.3"}
  ])
else
  url = "https://raw.githubusercontent.com/processone/ejabberd/master/ejabberd.yml.example"

  Mix.install([:req]) &&
    File.write!(
      filename,
      String.replace(Req.get!(url).body, "starttls_required: true", "")
    )

  IO.puts("ejabberd.yml downloaded correctly, click 'Reconnect and setup' to download ejabberd.")
end
```

## Setup ejabberd inside livebook

This Livebook will download, compile and install ejabberd:

1. If you want to use a specific `ejabberd.yml` configuration file, copy it to your Livebook folder.

2. On top of this page, click `Setup`.

3. If `ejabberd.yml` is not found, it will be downloaded from ejabberd git repository.

4. Click `Reconnect and setup` to download ejabberd and all its dependencies. It will be compiled and started... it may take a pair of minutes.

Alternatively, if you already have ejabberd installed and running in your system, you can [connect livebook to your ejabberd node](#connect-livebook-to-your-ejabberd-node)

## Execute some Erlang code

Now that Livebook is connected a running ejabberd node, you can run Erlang and Elixir code from this page directly in your node.

For example, to run some erlang code, put your mouse over the new lines and click on `Evaluate`:

```erlang
ejabberd_admin:registered_vhosts().
```

Let's define the details of an account, we will later register it. You may change those values:

```erlang
Username = <<"user1">>,
Server = <<"localhost">>,
Password = <<"somepass123">>,
{Username, Server, Password}.
```

Now let's execute an Erlang function to register the account:

```erlang
ejabberd_auth:try_register(Username, Server, Password).
```

Let's check the account was registered:

```erlang
ejabberd_auth:get_users(<<"localhost">>).
```

And is the account's password the one we introduced?

```erlang
Password == ejabberd_auth:get_password(Username, Server).
```

Ok, enough for now, let's remove the account:

```erlang
ejabberd_auth:remove_user(Username, Server).
```

## Execute some Elixir code

The same code of the previous section can be executed using Elixir:

```elixir
:ejabberd_admin.registered_vhosts()
```

```elixir
username = <<"user1">>
server = <<"localhost">>
password = <<"somepass123">>
{username, server, password}
```

```elixir
:ejabberd_auth.try_register(username, server, password)
```

```elixir
:ejabberd_auth.get_users(server)
```

```elixir
password == :ejabberd_auth.get_password(username, server)
```

```elixir
:ejabberd_auth.remove_user(username, server)
```

## Run API commands

Let's run some ejabberd [API commands](https://docs.ejabberd.im/developer/ejabberd-api/) using the ejabberd_ctl frontend (there is is also [mod_http_api](https://docs.ejabberd.im/admin/configuration/modules/#mod_http_api) and [ejabberd_xmlrpc](https://docs.ejabberd.im/admin/configuration/listen/#ejabberd-xmlrpc)).

For example, the [status](https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#status) API command:

```erlang
ejabberd_ctl:process(["status"]).
```

How to register an account using ejabberd_ctl to call the API command

```elixir
command = ~c"register"
:ejabberd_ctl.process([command, username, server, password])
```

If you have ejabberd installed in the the system, and the `ejabberdctl` command-line script is available in your PATH, then you could also try to execute with:

```erlang
os:cmd("ejabberdctl status").
```

```elixir
:os.cmd(~c"ejabberdctl status")
```

## Draw process structure

Let's view the ejabberd process tree:

```elixir
Kino.Process.render_app_tree(:ejabberd, direction: :left_right)
```

Let's view the erlang processes that handle XMPP client connections. If this graph is empty, login to ejabberd with a client and reevaluate this code:

```elixir
Kino.Process.render_sup_tree(:ejabberd_c2s_sup, direction: :left_right)
```

And some information about the erlang process that handles the XMPP client session:

```elixir
[resource] = :ejabberd_sm.get_user_resources(username, server)
Elixir.Process.info(:ejabberd_sm.get_session_pid(username, server, resource))
```

## Connect Livebook to your ejabberd node

By default this livebook downloads, compiles and starts ejabberd by [setting up ejabberd sinde livebook](#setup-ejabberd-inside-livebook). If you already have ejabberd installed and would like to connect this livebook to your existing ejabberd node, follow those steps:

<!-- livebook:{"break_markdown":true} -->

### Get erlang node name

To connect Livebook to your running ejabberd node, you must know its erlang node name and its cookie.

The erlang node name is by default `ejabberd@localhost`. You can check this in many places, for example:

* Using `ejabberdctl status`:

```sh
$ ejabberdctl status
The node ejabberd@localhost is started with status: started
ejabberd 24.2.52 is running in that node
```

* In the `ejabberd.log` file, which contains a line like:

```sh
2024-03-26 13:18:35.019288+01:00 [info] <0.216.0>@ejabberd_app:start/2:63
ejabberd 24.2.52 is started in the node ejabberd@localhost in 0.91s
```

<!-- livebook:{"break_markdown":true} -->

### Setup ejabberd node

A Livebook can only connect to an Erlang node that has Elixir support. So, make sure you install not only Erlang, but also Elixir.

When compiling ejabberd, make sure to enable Elixir support. It should get enabled by default, but you can ensure this: either by `./configure --with-rebar=mix` or by `./configure --enable-elixir`.

Then start ejabberd with IEx shell: `ejabberdctl iexlive`

<!-- livebook:{"break_markdown":true} -->

### Get erlang cookie

The erlang cookie is a random string of capital letters required to connect to an existing erlang node.

You can get it in a running node, simply call:

<!-- livebook:{"force_markdown":true} -->

```elixir
:erlang.get_cookie()
:XQFOPGGPSNEZNUWKRZJU
```

<!-- livebook:{"break_markdown":true} -->

### Connect this livebook to your ejabberd node

Now that you have ejabberd running, and you know the information required to connect to it:

1. go to Livebook
2. in the left side bar, click the `Runtime settings` icon, or press `sr` keyboard shortcut
3. click the `Configure` button
4. click the `Attached node` button
5. introduce the erlang node name (`ejabberd@localhost`) and cookie (`XQFOPGGPSNEZNUWKRZJU`) of your ejabberd node
6. click the `Connect` button (it may say `Reconnect`)
7. If it connected successfully, it will now show memory consumption of that node

<!-- livebook:{"break_markdown":true} -->

### Test the connection

Now that Livebook is connected to your running ejabberd node, you can run Erlang and Elixir code from this page directly in your node.

For example, to run some erlang code, put your mouse over the new lines and click on `Evaluate`:

```erlang
ejabberd_admin:registered_vhosts().
```

The same code can be executed using Elixir:

```elixir
:ejabberd_admin.registered_vhosts()
```

## Stop ejabberd

Let' stop ejabberd insie livebook

```elixir
:ejabberd.stop()
```

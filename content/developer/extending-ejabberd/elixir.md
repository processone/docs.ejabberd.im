---
title: ejabberd for Elixir Developers
menu: Elixir Dev
toc: true
---

<div class="note-down">improved in <a href="/archive/21_07/">21.07</a></div>

# Building ejabberd with Mix

You can build ejabberd with [Elixir](https://elixir-lang.org/) `mix` tool.
This allows ejabberd to use Elixir libraries
and ejabberd modules written in Elixir.

Please note: Elixir 1.10.3 or higher is required to build a release.
Also, if using Erlang/OTP 24, then Elixir 1.11.4 or higher is required.

<!--- Code blocks in lists have indentation of 3 spaces at first ~~~ -->

1. Make sure you have the
   [requirements](/admin/installation/#requirements)
   installed. On MacOS you need to use Homebrew and
   [set up your environment](/admin/installation/#macos).

1. Clone ejabberd project from Github:

    ~~~ bash
    git clone https://github.com/processone/ejabberd.git
    cd ejabberd
    ~~~

1.  [Compile](/admin/installation/#compilation) ejabberd:

    ~~~ bash
    ./autogen.sh
    ./configure --with-rebar=mix
    make
    ~~~

1. [Build a development release](/admin/installation/#development-release):

    ~~~ bash
    make dev
    ~~~

1. There are many ways to start ejabberd, using the `ejabberdctl` or `ejabberd` scripts:

    ~~~ bash
    _build/prod/rel/ejabberd/bin/ejabberdctl iexlive
    _build/prod/rel/ejabberd/bin/ejabberdctl live
    _build/prod/rel/ejabberd/bin/ejabberd start_iex
    ...
    ~~~

1. You should see that ejabberd is properly started:

    ~~~ bash
    Erlang/OTP 23 [erts-11.1.8] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1]

    2021-08-03 13:37:36.561603+02:00 [info] Loading configuration from /home/bernar/e/git/ejabberd/_build/dev/rel/ejabberd/etc/ejabberd/ejabberd.yml
    2021-08-03 13:37:37.541688+02:00 [info] Configuration loaded successfully
    ...
    2021-08-03 13:37:40.201590+02:00 [info] ejabberd 21.7.9 is started in the node ejabberd@atenea in 3.86s
    2021-08-03 13:37:40.203678+02:00 [info] Start accepting TCP connections at [::]:5222 for ejabberd_c2s

    Interactive Elixir (1.10.3) - press Ctrl+C to exit (type h() ENTER for help)
    iex(ejabberd@localhost)1>
    ~~~

1. Now that ejabberd starts correctly, adapt to your needs the default ejabberd
   configuration file located at `_build/dev/rel/ejabberd/etc/ejabberd/ejabberd.yml`
   For example, enable this example Elixir ejabberd module:

    ~~~ yaml
    modules:
      'ModPresenceDemo': {}
      mod_adhoc: {}
      ...
    ~~~

<div class="note-down">new in <a href="https://www.process-one.net/blog/ejabberd-16-02-happy-leap-day/">16.02</a></div>

# Embedding ejabberd in an Elixir application

ejabberd is available as an Hex.pm
application: [ejabberd on hex.pm](https://hex.pm/packages/ejabberd).

This means you can build a customized XMPP messaging
platform with Elixir on top of ejabberd by leveraging ejabberd code
base in your app and providing only your custom modules.
This makes the management of your ejabberd plugins easier and cleaner.

To create your own application depending on ejabberd, you can go
through the following steps:


1. Create a new Elixir app using `mix`:

    ~~~ bash
    mix new ejapp
    cd ejapp
    ~~~

1. Add [ejabberd package](https://hex.pm/packages/ejabberd) as a
   dependency in your `mix.exs` file:

    ~~~ elixir
    defmodule Ejapp.MixProject do
    ...
      defp deps do
        [
         {:ejabberd, "~> 21.7"}
        ]
      end
    end
    ~~~

1. Compile everything:

    ~~~ bash
    mix do deps.get, compile
    ~~~

1. Create paths and files for ejabberd:

    ~~~ bash
    mkdir config
    mkdir logs
    mkdir mnesia
    wget -O config/ejabberd.yml https://raw.githubusercontent.com/processone/ejabberd/master/ejabberd.yml.example
    ~~~

1. Define those paths in `config/config.exs`:

    ~~~ elixir
    import Config
    config :ejabberd,
      file: "config/ejabberd.yml",
      log_path: 'logs/ejabberd.log'
    config :mnesia,
      dir: 'mnesia/'
    ~~~

1. Start your app, ejabberd will be started as a dependency:

    ~~~ bash
    iex -S mix
    ~~~

1. You should see that ejabberd is properly started:

    ~~~ bash
    Erlang/OTP 23 [erts-11.1.8] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1]
    
    Compiling 1 file (.ex)
    Generated ejapp app
    
    17:58:35.955 [info]  Loading configuration from config/ejabberd.yml
    
    17:58:36.459 [info]  Configuration loaded successfully
    ...
    17:58:39.897 [info]  ejabberd 21.7.0 is started in the node :nonode@nohost in 4.07s
    ...
    17:58:39.908 [info]  Start accepting TCP connections at [::]:5222 for :ejabberd_c2s
    
    Interactive Elixir (1.10.3) - press Ctrl+C to exit (type h() ENTER for help)
    iex(1)>
    ~~~

1. Register user from Elixir console:

    ~~~ elixir
    :ejabberd_auth.try_register("test", "localhost", "passw0rd")
    ~~~

1. You are all set, you can now connect with an XMPP client !

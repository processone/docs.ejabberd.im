---
title: ejabberd for Elixir Developers
menu: Elixir Development
order: 50
toc: true
---

# Building ejabberd with Mix

You can build ejabberd with Elixir `mix` tool directly. This can be
done as follow:

<!--- Code blocks in lists have indentation of 3 spaces at first ~~~ -->
1. Clone ejabberd project from Github:

    ~~~ python
    git clone https://github.com/processone/ejabberd.git
    ~~~

2. Make sure you have all dependencies installed. For example, on MacOS, you need to use Homebrew and [set up your 
   environment](https://docs.ejabberd.im/admin/installation/#macos).    

3. Build ejabberd with `mix` like any standard Elixir project:

    ~~~ python
    cd ejabberd/
    mix deps.get
    mix compile
    ~~~

4. Configure ejabberd. You can create `config/ejabberd.yml` file from scratch or, as shown, start from a template one:

    ~~~ python
    (cd config; wget https://gist.githubusercontent.com/mremond/e3a05f7a18f87c4c5145547a3fd3e9e1/raw/a412004b37c7fa7f1d1f7179b4cc73045b9b8da9/ejabberd.yml)
    ~~~

5. Adapt configuration file to your needs. You can modify the standard `config/ejabberd.yml` file or use 
the experimental Elixir config file `config/ejabberd.exs`. To use it, change the file to use in `config/config.exs`.

6. Start ejabberd with standard Elixir tools:

    ~~~ python
    iex -S mix
    ~~~

You should see that ejabberd is properly started:

  ~~~ python
  Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]
  Compiled lib/ejabberd.ex
  Compiled lib/ejabberd/hooks.ex
  Compiled lib/ejabberd/logger.ex
  Compiled lib/mod_presence_demo.ex
  Generated ejabberd app
  Consolidated List.Chars
  Consolidated String.Chars
  Consolidated Enumerable
  Consolidated Collectable
  Consolidated IEx.Info
  Consolidated Inspect
  19:24:52.462 [info] Application lager started on node nonode@nohost
  19:24:52.477 [info] Application sasl started on node nonode@nohost
  19:24:52.484 [info] Application fast_yaml started on node nonode@nohost
  19:24:52.501 [info] Application fast_tls started on node nonode@nohost
  19:24:52.516 [info] Application fast_xml started on node nonode@nohost
  19:24:52.519 [info] Application stringprep started on node nonode@nohost
  19:24:52.529 [info] Application ezlib started on node nonode@nohost
  19:24:52.533 [info] Application cache_tab started on node nonode@nohost
  19:24:52.689 [info] Application mnesia started on node nonode@nohost
  19:24:55.243 [info] FQDN used to check DIGEST-MD5 SASL authentication: MacBook-Pro-de-Mickael
  19:24:55.343 [info] Application iconv started on node nonode@nohost
  19:24:55.699 [info] Reusing listening port for {5222,{0,0,0,0},tcp}
  19:24:55.699 [info] Reusing listening port for {5269,{0,0,0,0},tcp}
  19:24:55.700 [info] Reusing listening port for {5280,{0,0,0,0},tcp}
  19:24:55.700 [info] ejabberd 16.06.1 is started in the node nonode@nohost
  19:24:55.700 [info] Application ejabberd started on node nonode@nohost
  Interactive Elixir (1.2.0) - press Ctrl+C to exit (type h() ENTER for help)
  iex(1)>
  ~~~

# Embedding ejabberd in an Elixir application

Starting from version 16.02, ejabberd is packaged as an Hex.pm
application: [ejabberd on hex.pm](https://hex.pm/packages/ejabberd).

It means that you can now build a customized XMPP messaging
platform with Elixir on top of ejabberd by leveraging ejabberd code
base in your app and providing only your custom modules.

This makes the management of your ejabberd plugins easier and cleaner.

To create your own application depending on ejabberd, you can go
through the following steps, starting with creating a new Elixir app with mix:

   ~~~ bash
      mix new ejapp
      * creating README.md
      * creating .gitignore
      * creating mix.exs
      * creating config
      * creating config/config.exs
      * creating lib
      * creating lib/ejapp.ex
      * creating test
      * creating test/test_helper.exs
      * creating test/ejapp_test.exs

      Your Mix project was created successfully.
      You can use "mix" to compile it, test it, and more:

      cd ejapp
      mix test

      Run "mix help" for more commands.
   ~~~


1. Get to your new app directory:

     ~~~ python
     cd ejapp
     ~~~

1. Add [ejabberd package](https://hex.pm/packages/ejabberd) as a
   dependency in your `mix.exs` file:

     ~~~ elixir
     defmodule Ejapp.Mixfile do
     ...
       defp deps do
         [{:ejabberd, "~> 19.2"}]
       end
     end
     ~~~

1. As you added ejabberd as a dependency, there is *no need* to explicitely start it in extra_applications list with 
latest Elixir and `mix` versions. ejabberd start will be properly inferred by `mix` and started accordingly.

1. Create or get ejabberd config file:

     ~~~ bash
     (cd config; wget https://gist.githubusercontent.com/mremond/e3a05f7a18f87c4c5145547a3fd3e9e1/raw/a412004b37c7fa7f1d1f7179b4cc73045b9b8da9/ejabberd.yml)
     ~~~

1. Define ejabberd configuration reference in `config/config.exs`:

     ~~~ elixir
     config :ejabberd,
       file: "config/ejabberd.yml",
       log_path: 'logs/ejabberd.log'

     # Customize Mnesia directory:
     config :mnesia,
       dir: 'mnesiadb/'
     ~~~

1. Create log dir:

     ~~~ bash
     mkdir logs
     ~~~

1. Compile everything:

     ~~~ bash
     mix do deps.get, compile
     ~~~

1. Start your app, ejabberd will be started as a dependency:

     ~~~ bash
     iex -S mix
     ~~~

1. Register user from Elixir console:

     ~~~ elixir
     :ejabberd_auth.try_register("test", "localhost", "passw0rd")
     ~~~

1. You are all set, you can now connect with an XMPP client !

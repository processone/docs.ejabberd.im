# ejabberd Modules Development

## Introduction

ejabberd is based on a modular architecture that makes it highly customizable and infinitely extensible.

Here is an overview of ejabberd internal architecture:

![image1](./images/ejabberd_internals.png)

## What is a module ?

Outside of a few infrastructure core components most of ejabberd features are developed as modules. Modules are used to extend the features of ejabberd (plugins).

## How to write a custom module ?

Ejabberd comes with a lot of modules, but sometimes you may need an unsupported feature from the official sources or maybe you need to write your own custom implementation for your very special needs.

Each modules is written in either Erlang or Elixir. To use them, you typically declare them in ejabberd configuration file. That's also the place where you can configure the module, by passing supported options to overload its default behaviour.

On ejabberd launch, the server will start all the declared modules. You can start (or stop) them manually from Erlang shell as well.

As a convention, module names starts with "mod_", but you can actually call them as you want.

## The `gen_mod` behaviour

All ejabberd modules are implementing the `gen_mod` behaviour. It
means that a module must provide the following API:

``` erlang
start(Host, Opts) -> ok
stop(Host) -> ok
depends(Host, Opts) -> []
mod_options(Host) -> []
```

Parameters are:

* Host = `string()`
* Opts = `[{Name, Value}]`
* Name = Value = `string()`

Host is the name of the virtual host running the module. The `start/2`
and `stop/1` functions are called for each virtual host at start and
stop time of the server.

`Opts` is a lists of options as defined in the configuration file for
the module. They can be retrieved with the `gen_mod:get_opt/3`
function.

## mod_hello_world

The following code shows the simplest possible module.

``` erlang title="mod_hello_world.erl"
-module(mod_hello_world).

-behaviour(gen_mod).

%% Required by ?INFO_MSG macros
-include("logger.hrl").

%% Required by ?T macro
-include("translate.hrl").

%% gen_mod API callbacks
-export([start/2, stop/1, depends/2, mod_options/1, mod_doc/0]).

start(_Host, _Opts) ->
    ?INFO_MSG("Hello, ejabberd world!", []),
    ok.

stop(_Host) ->
    ?INFO_MSG("Bye bye, ejabberd world!", []),
    ok.

depends(_Host, _Opts) ->
    [].

mod_options(_Host) ->
    [].

mod_doc() ->
    #{desc =>
          ?T("This is an example module.")}.
```

Now you have two ways to compile and install the module:
If you compiled ejabberd from source code, you can copy that source code file
with all the other ejabberd source code files, so it will be compiled and installed with them.
If you installed some compiled ejabberd package, you can create your own module dir, see
[Add module to ejabberd-modules](#add-module-to-ejabberd-modules).

You can enable your new module by adding it in the ejabberd config file.
Adding the following snippet in the config file will integrate the module in ejabberd module lifecycle management. It means the module will be started at ejabberd launch and stopped during ejabberd shutdown process:

``` yaml
modules:
  ...
  mod_hello_world: {}
```

Or you can start / stop it manually by typing the following commands in an Erlang shell running ejabberd:

* To manually start your module:

    ``` erlang
    gen_mod:start_module(<<"localhost">>, mod_hello_world, []).
    ```

* To manually stop your module:

    ``` erlang
    gen_mod:stop_module(<<"localhost">>, mod_hello_world).
    ```

When the module is started, either on ejabberd start or manually, you should see the following message in ejabberd log file:

``` python
19:13:29.717 [info] Hello, ejabberd world!
```

## Add module to ejabberd-modules

If you install ejabberd using the official ProcessOne installer,
it includes everything needed to build ejabberd modules on its own.

If using a container image, follow the specific steps to
[install your module in a container image](../../CONTAINER.md#install-your-module).

The value of `$HOME` vary depending on your ejabberd installation method,
check [ejabberd-modules](../../admin/guide/modules.md#ejabberd-modules) for details.

1. First, create this path

    ``` sh
    $HOME/.ejabberd-modules/sources/mod_hello_world/src/
    ```

2. and copy your source code to this location:

    ``` sh
    $HOME/.ejabberd-modules/sources/mod_hello_world/src/mod_hello_world.erl
    ```

3. Create a specification file in YAML format as mod_hello_world.spec
(see examples from ejabberd-contrib). So, create the file

    ``` sh
    $HOME/.ejabberd-modules/sources/mod_hello_world/mod_hello_world.spec
    ```

    with this content:

    ``` yaml title="mod_hello_world.spec"
    summary: "Hello World example module"
    ```

4. From that point you should see it as available module:

    ``` sh
    ejabberdctl modules_available
    mod_hello_world Hello World example module
    ```

5. Now you can install and uninstall that module like any other,
    as described in the previous section.

6. If you plan to publish your module, you should check if your module
    follows the policy and if it compiles correctly:

    ``` sh
    ejabberdctl module_check mod_mysupermodule
    ok
    ```

    If all is OK, your’re done ! Else, just follow the warning/error messages to fix the issues.

You may consider publishing your module as a tgz/zip archive or git repository,
and send your spec file for integration in ejabberd-contrib repository.
ejabberd-contrib will only host a copy of your
spec file and does not need your code to make it available to all ejabberd users.


## Next steps

From there, you know how to package a module to integrate it inside
ejabberd environment. Packaging a module allows you to:

* Integrate in ejabberd overall application life cycle, i.e. with the
  start and stop mechanism.
* Get data from ejabberd configuration file.

Now, to do useful stuff, you need to integrate with ejabberd data
flow. You have two mechanisms available from ejabberd modules:

* [Events and Hooks](../guide.md#hooks): This is to handle internal ejabberd triggers and
  subscribe to them to perform actions or provide data.

* [IQ Handlers](../guide.md#iq_handlers): This is a way to register ejabberd module to handle
  XMPP Info Queries. This is the XMPP way to provide new services.

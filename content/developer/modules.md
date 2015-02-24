---
title: ejabberd Modules Development | ejabberd documentation
---

# ejabberd Modules development

## Introduction

ejabberd is based on a modular architecture that makes it highly
customizable and infinitely extensible.

Here is an overview of ejabberd internal architecture:

[![][image-1]](/images/architect/ejabberd_internals.pdf)

[*Click for large size PDF*](/images/architect/ejabberd_internals.pdf)

## What is a module ?

Outside of a few infrastructure core components most of ejabberd
features are developed as modules. Modules are use to extend the
feature of ejabberd (plugins).

Each modules is written in either Erlang or Elixir. To use them, you
typically declare them in ejabberd configuration file. That's also the
place where you can configure the module, by passing supported options
to overload its default behaviour.

On ejabberd launch, the server will start all the declared
modules. You can start (or stop) them manually from Erlang shell as
well.

As a convention, module names starts with "mod_", but you can actually
call them as you want.

## The `gen_mod` behaviour

All ejabberd modules are implementing the `gen_mod` behaviour. It
means that a module must provide the following API:

    #!erlang
    start(Host, Opts) -> ok
    stop(Host) -> ok

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

    #!erlang
    -module(mod_hello_world).
    
    -behaviour(gen_mod).
    
    %% Required by ?INFO_MSG macros
    -include("logger.hrl").
    
    %% gen_mod API callbacks
    -export([start/2, stop/1]).
    
    start(_Host, _Opts) ->
        ?INFO_MSG("Hello, ejabberd world!", []),
        ok.
    
    stop(_Host) ->
        ?INFO_MSG("Bye bye, ejabberd world!", []),
        ok.

You can use it by adding it in the config file. Adding the following
snippet in the config file will integrate the module in ejabberd
module lifecycle management. It means the module will be started and
ejabberd launch and stopped during ejabberd shutdown process:

    #!yaml
    modules:
      ...
      mod_hello_world: {}

Or you can start / stop it manually by typind the following commands
from an Erlang shell running ejabberd:

* To manually start your module:

        #!erlang
        gen_mod:start_module(<<"localhost">>, mod_hello_world, []).

* To manually stop your module:

        #!erlang
        gen_mod:stop_module(<<"localhost">>, mod_hello_world).

When the module is started, either on ejabberd start or manually, you
should see the following message in ejabberd log file:

    19:13:29.717 [info] Hello, ejabberd world!

## Next steps

From there, you know how to package a module to integrate it inside
ejabberd environment. Packaging a module allows you to:

* Integrate in ejabberd overall application life cycle, i.e. with the
  start and stop mechanism.
* Get data from ejabberd configuration file.

Now, to do useful stuff, you need to integrate with ejabberd data
flow. You have two mechanisms available from ejabberd modules:

* [Events and Hooks](/developer/hooks/): This is to handle internal ejabberd triggers and
  subscribe to them to perform actions or provide data.

* [IQ Handlers](/developer/iq-handlers/): This is a way to register ejabberd module to handle
  XMPP Info Queries. This is the XMPP way to provide new services.

[image-1]:	/images/architect/ejabberd_internals.png

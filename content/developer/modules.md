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

    start(Host, Opts) -> ok
    stop(Host) -> ok

Parameters are:

* Host = string()
* Opts = [{Name, Value}]
* Name = Value = string()

Host is the name of the virtual host running the module. The `start/2`
and `stop/1` functions are called for each virtual host at start and
stop time of the server.

`Opts` is a lists of options as defined in the configuration file for
the module. They can be retrieved with the `gen_mod:get_opt/3`
function. 

## mod_hello_world

The following code shows the simplest possible module.

TODO

You can use it by adding it in the config file:

TODO

Or you can start / stop it manually by typind the following commands
from an Erlang shell running ejabberd:

TODO

[image-1]:	/images/architect/ejabberd_internals.png

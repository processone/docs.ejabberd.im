---
title: ejabberd Modules Development
menu: Modules Development
order: 10
toc: true
---

# Introduction

ejabberd is based on a modular architecture that makes it highly
customizable and infinitely extensible.

Here is an overview of ejabberd internal architecture:

[![][image-1]](/static/images/architect/ejabberd_internals.pdf)

[*Click for large size PDF*](/static/images/architect/ejabberd_internals.pdf)

# What is a module ?

Outside of a few infrastructure core components most of ejabberd
features are developed as modules. Modules are use to extend the
feature of ejabberd (plugins).

# How to write a custom module ?

Ejabberd comes with a lot of modules, but sometimes you may need an
unsupported feature from the official sources or maybe you need to
write your own custom implementation for your very special needs.

Each modules is written in either Erlang or Elixir. To use them, you
typically declare them in ejabberd configuration file. That's also the
place where you can configure the module, by passing supported options
to overload its default behaviour.

On ejabberd launch, the server will start all the declared
modules. You can start (or stop) them manually from Erlang shell as
well.

As a convention, module names starts with "mod_", but you can actually
call them as you want.

# The `gen_mod` behaviour

All ejabberd modules are implementing the `gen_mod` behaviour. It
means that a module must provide the following API:

~~~ erlang
start(Host, Opts) -> ok
stop(Host) -> ok
~~~

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

# mod_hello_world

The following code shows the simplest possible module.

~~~ erlang
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
~~~

You can use it by adding it in the config file. Adding the following
snippet in the config file will integrate the module in ejabberd
module lifecycle management. It means the module will be started and
ejabberd launch and stopped during ejabberd shutdown process:

~~~ yaml
modules:
  ...
  mod_hello_world: {}
~~~

Or you can start / stop it manually by typind the following commands
from an Erlang shell running ejabberd:

* To manually start your module:

  ~~~ erlang
gen_mod:start_module(<<"localhost">>, mod_hello_world, []).
~~~

* To manually stop your module:

  ~~~ erlang
gen_mod:stop_module(<<"localhost">>, mod_hello_world).
~~~

When the module is started, either on ejabberd start or manually, you
should see the following message in ejabberd log file:

~~~ python
19:13:29.717 [info] Hello, ejabberd world!
~~~

# Working with the ejabberd module repository

ejabberd is able to fetch module sources by itself, compile with correct flags
and install in a local repository, without any external dependencies. You do not
need to know Erlang and have it installed in order to use the contributed modules.
The contributed modules repository is ejabberd-contrib on github. It contains
most used contribution and also can link to any other external repository.
This works with ejabberd modules written in Erlang and will also support new Elixir modules.

# Basic commands

As a user, this is how it works:

First you need to get/update the list of available modules. This must be done
before anything else to let module related features to work correctly:

~~~ bash
$ ejabberdctl modules_update_specs
~~~

You should repeat this command at regular interval depending on your needs.
Basically running modules_update_specs once a week is enough to keep in sync
but you may prefer to manually call this only before you need to install a
new module or an attended upgrade.
Then you can list available modules

~~~ bash
$ ejabberdctl modules_available
...
mod_admin_extra Additional ejabberd commands
mod_archive Supports almost all the XEP-0136 version 0.6 except otr
mod_cron Execute scheduled commands
mod_log_chat Logging chat messages in text files
...
~~~

Let’s give `mod_cron` a try:

~~~ bash
$ ejabberdctl module_install mod_cron
ok
~~~

This command installs `mod_cron` from ejabberd-contrib repository. An example default
configuration is installed in:

~~~
$HOME/.ejabberd-modules/mod_cron/conf/mod_cron.yml
~~~

All you have to do is to copy paste the module and add the values in there in the
proper place in your ejabberd.yml config file. Be careful, the snippet can include ACLs,
listeners and module configuration, that you have to put in the right place in your
config file.

Now, check your new module is installed:

~~~ bash
$ ejabberdctl modules_installed
mod_cron
~~~

And finally, you can remove it:

~~~ bash
$ ejabberdctl module_uninstall mod_cron
ok
~~~

# Managing your own modules
As a developper, you still need Erlang and Ejabberd if you install everything from
sources, but you may even not need Erlang if you installed ejabberd from official
ProcesOne installer. The official installer includes everything needed to build
ejabberd modules on its own.

First you can work on your own module by creating a repository in

~~~
$HOME/.ejabberd-modules/sources/mod_mysupermodule
~~~

and creating a specification file in YAML format as mod_mysupermodule.spec
(see examples from ejabberd-contrib). From that point you should see it as available module.

Before commiting your code, you should check if your module follows the policy and if it
compiles correctly:

~~~ bash
$ ejabberdctl module_check mod_mysupermodule
ok
~~~

if all is OK, your’re done ! Else, just follow the warning/error messages to fix the issues.

You can keep your repository private in this location, ejabberd see it as an available module,
or you can publish it as a tgz/zip archive or git repository, and send your spec file for
integration in ejabberd-contrib repository. ejabberd-contrib will only host a copy of your
spec file and does not need your code to make it available to all ejabberd users.


# Status

Please note this is provided as a beta version. We want the work in progress to be released
early to gather feedback from developers and users.

For now, you need to edit the configuration snippet provided in module’s conf directory and
copy it into your ejabberd’s main configuration. Then you’ll need to restart ejabberd or
manually start the module.

However, our plan is to keep iterating on the tool and to make our best to make module
installation as easy as possible and avoid need to change main configuration:
ejabberd should be able to include module configuration snippets on the fly in a near future.

# Next steps

From there, you know how to package a module to integrate it inside
ejabberd environment. Packaging a module allows you to:

* Integrate in ejabberd overall application life cycle, i.e. with the
  start and stop mechanism.
* Get data from ejabberd configuration file.

Now, to do useful stuff, you need to integrate with ejabberd data
flow. You have two mechanisms available from ejabberd modules:

* [Events and Hooks](/developer/guide/#hooks): This is to handle internal ejabberd triggers and
  subscribe to them to perform actions or provide data.

* [IQ Handlers](/developer/guide/#iq-handlers): This is a way to register ejabberd module to handle
  XMPP Info Queries. This is the XMPP way to provide new services.

[image-1]:	/static/images/architect/ejabberd_internals.png

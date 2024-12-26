# Add More Modules

## `ejabberd-modules`

ejabberd starts automatically modules installed in `.ejabberd-modules`,
in addition to all the [modules included](../configuration/modules.md) with ejabberd.
There are [API commands](../../developer/ejabberd-api/admin-tags.md#modules)
to compile, install, upgrade and uninstall those additional modules.

!!! info
    The exact path to the `ejabberd-modules` directory in your ejabberd installation may be:

    - `$HOME/.ejabberd-modules` when [compiling source code](../install/source.md) or using [installers](../install/binary-installer.md)
    - `/opt/ejabberd/.ejabberd-modules` in the [`ejabberd` container image](../../CONTAINER.md)
    - `/home/ejabberd/.ejabberd-modules` in the [`ecs` container image](../../CONTAINER.md)

    That path can be modified using the variable
    [CONTRIB_MODULES_PATH](https://github.com/processone/ejabberd/blob/master/ejabberdctl.cfg.example#L180)
    in the `ejabberdctl.cfg` configuration file.

To get new modules in `ejabberd-modules`:

- If you [develop your own module](../../developer/extending-ejabberd/modules.md),
  you can [add your module to ejabberd-modules](../../developer/extending-ejabberd/modules.md#add-module-to-ejabberd-modules)
  and let ejabberd compile, install and start it.

- Tell ejabberd to download the [ejabberd-contrib](#ejabberd-contrib) git repository,
  which contains many additional ejabberd modules written in Erlang/Elixir.

## `ejabberd-contrib`

`ejabberd-contrib` is a git repository that hosts
a collection of contributed modules for ejabberd written in Erlang/Elixir.
Check the [ejabberd-contrib GitHub page](https://github.com/processone/ejabberd-contrib).

Furthermore, in the `extra` directory of that repository there are
references to other modules hosted in other git repositories.

First of all, let's
[get/update the modules source code](../../developer/ejabberd-api/admin-api.md#modules_update_specs):

``` sh
ejabberdctl modules_update_specs
```

## Modules Management

Once you have placed the modules source code in `ejabberd-modules`, you can:

- [list modules](#list-modules)
- [install a module](#install-module)
- [uninstall a module](#uninstall-module), or upgrade it

## List Modules

Get a list of all the [modules available](../../developer/ejabberd-api/admin-api.md#modules_available)
to install:

``` sh
ejabberdctl modules_available

...
mod_cron        Execute scheduled commands
mod_default_contacts    Auto-add roster contacts on registration
mod_default_rooms       Auto-bookmark rooms on registration
mod_deny_omemo  Prevent OMEMO sessions from being established
mod_ecaptcha    Generate CAPTCHAs using ecaptcha
...
```

What modules are currently [installed](../../developer/ejabberd-api/admin-api.md#modules_installed):

``` sh
ejabberdctl modules_installed
```

## Install Module

Let’s [install a module](../../developer/ejabberd-api/admin-api.md#module_install):

``` sh
ejabberdctl module_install mod_cron

Module mod_cron has been installed and started.
It's configured in the file:
  /home/ejabberd/.ejabberd-modules/mod_cron/conf/mod_cron.yml
Configure the module in that file, or remove it
and configure in your main ejabberd.yml
```

That command performs several tasks:

- downloads any Erlang/Elixir dependencies specified in the modules's `rebar.config` file
- compiles the module and its dependencies (if not yet already compiled)
- installs it (inside `ejabberd-modules`)
- copies the default module configuration file (if any)
- and starts the module (if there was a default configuration file

As a result, now `.ejabberd-modules` contains a new directory `mod_cron/`
with the binary `*.beam` files and the default module configuration.

The default module configuration file, if it exists, will be read by ejabberd
when it starts.
If you prefer to keep all the configuration in your main `ejabberd.yml` file,
move the content of that file,
but remember that the file will be overwritten if you install or upgrade the module.

## Uninstall Module

And finally, you can [uninstall the module](../../developer/ejabberd-api/admin-api.md#module_uninstall):

``` sh
ejabberdctl module_uninstall mod_cron
```

By the way, you can [upgrade the module](../../developer/ejabberd-api/admin-api.md#module_upgrade),
which essentially uninstalls and installs the same module with one single command call:

``` sh
ejabberdctl module_upgrade mod_cron
```

## Dependencies in container

When a module in `ejabberd-modules` depends on an Erlang or Elixir library,
it is defined in the `rebar.config` file.
To download those dependencies during [module installation](#install-module),
either `git` or `mix` is required,
but none of them are available in the `ejabberd` or the `ecs`
[container images](../../CONTAINER.md).
Consequently, the module installation will fail.
The solution is quite simple: install `git` or `mix`.

For example, let's start an `ejabberd` container
and try to install a module with dependencies:

``` sh
podman run --name ejabberd -d -p 5222:5222 -p 5280:5280 ghcr.io/processone/ejabberd

podman exec ejabberd ejabberdctl module_install mod_ecaptcha
```

An error message like this will appear:
``` sh
Fetching dependency ecaptcha: /bin/sh: git: not found
/bin/sh: cd: line 1: can't cd to ecaptcha: No such file or directory
/bin/sh: git: not found
Module mod_ecaptcha has been installed and started.
It's configured in the file:
  /opt/ejabberd/.ejabberd-modules/mod_ecaptcha/conf/mod_ecaptcha.yml
Configure the module in that file, or remove it
and configure in your main ejabberd.yml
```

In order to download modules dependencies, first of all install `git` in the container:

``` sh
podman exec --user root ejabberd apk add git
```

Now remove the module `deps/` folder and install again:

``` sh
podman exec ejabberd rm -rf /opt/ejabberd/.ejabberd-modules/sources/ejabberd-contrib/mod_ecaptcha/deps/

podman exec ejabberd ejabberdctl module_upgrade mod_ecaptcha
```

This time dependencies will be downloaded, compiled and installed:

``` sh
Fetching dependency ecaptcha: Cloning into 'ecaptcha'...
Module mod_ecaptcha has been installed and started.
It's configured in the file:
  /opt/ejabberd/.ejabberd-modules/mod_ecaptcha/conf/mod_ecaptcha.yml
Configure the module in that file, or remove it
and configure in your main ejabberd.yml
```

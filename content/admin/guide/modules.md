# Get More Modules

## `ejabberd-modules`

ejabberd starts automatically modules installed in `.ejabberd-modules`,
in addition to all the [modules included](../configuration/modules.md) with ejabberd.
There are [API commands](../../developer/ejabberd-api/admin-tags.md#modules)
to compile, install, upgrade and uninstall those additional modules.

!!! info "`ejabberd-modules` path in your system"

    By default it is `$HOME/.ejabberd-modules`,
    being that the home path of the system account running ejabberd.
    The exact path in your ejabberd installation may be:

    - `/home/youraccount/.ejabberd-modules` when [compiling source code](../install/source.md) or using [binary installers](../install/binary-installer.md)
    - `/opt/ejabberd/.ejabberd-modules` in the [`ejabberd`](../../CONTAINER.md) and the [`ecs`](../../CONTAINER.md) container images
    - `/home/ejabberd/.ejabberd-modules` in the [`ecs`](../../CONTAINER.md) container image
    - `/var/lib/ejabberd/.ejabberd-modules` when installed from [Debian package](../install/os-package.md)

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

!!! failure "git not found?"

    Installing a module with dependencies requires `git` or `mix` installed in the system,
    otherwise compilation fails with errors like:
    ``` bash
    /bin/sh: mix: not found
    /bin/sh: git: not found
    ```
    If you are using an ejabberd container image, see the solution in
    [Install git for dependencies](../../CONTAINER.md#install-git-for-dependencies).

The command `module_install` performs several tasks:

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

# ejabberd Rest API

## Introduction

ejabberd comes with a powerful API serving two goals:

1. Manage the XMPP service and integrate the platform with back-end platforms and script tools.
2. Allow users to perform tasks via the client, allowing simple basic clients that do not need to use XMPP protocol.
   This can be handy, for example to send a message from your smartwatch, or show the number of offline messages.

The system is powerful and very versatile and you can configure it very finely, but it can be quite daunting to set up.

This section is written to demystify ejabberd API configuration and help you integrate ejabberd with your other back-ends or script through an HTTP / HTTPS ReST API.

## Understanding ejabberd "commands"

ejabberd operations are organised around the concept of commands. ejabberd standard modules already provide many commands, but the mechanism is generic and any module can provide its own set of commands. This exposition of commands for third-party modules make it very powerful.

All commands can be exposed through interfaces. Available interfaces are:

- [ejabberdctl](../../admin/guide/managing.md#ejabberdctl) command-line tool,
- [mod_http_api](../../admin/configuration/modules.md#mod_http_api) for ReST calls using JSON data,
- [ejabberd_xmlrpc](../../admin/configuration/listen.md#ejabberd_xmlrpc) for XML-RPC calls,
- [WebAdmin](../../admin/guide/managing.md#web-admin) uses most commands to build the web pages,
- [mod_configure](../../admin/configuration/modules.md#mod_configure) includes support for a few administrative tasks (using XMPP protocol itself through discovery and adhoc commands)

The [ejabberd-contrib Github repository](../../admin/guide/modules.md#ejabberd-contrib) provides other interfaces that can be installed to execute ejabberd commands in different ways: `mod_rest` (HTTP POST service), `mod_shcommands` (ejabberd WebAdmin page).

Any module in ejabberd can add its own command(s) through ejabberd Erlang/Elixir API, making the whole system totally extensible. A third-party module can expose its own command(s) and feel like a real part of the system. A module that exposes commands allows server admins to expose it the way they want.

ejabberd commands are universal, extensible and widely available through various configurable entrypoints.

_Note: The XML-RPC API still works but is deprecated in favor of the ReST API. You should migrate to ReST if you are using it._

<!-- TODO A diagram would be nice to have -->

## The role of ejabberd API

As we have seen, ejabberd API role is to provide and control access to ejabberd commands over HTTP/HTTPS.

Thus, ejabberd API primary goal is to grant access to some or all ejabberd "commands".

An admin ejabberd ReST API requires:

- At least one admin user, if you plan to check credentials for command access (You can alternatively rely on originating IP addresses).
- HTTP/HTTPS handlers configured to expose the desired commands.
- The selection of authentication mechanisms that can be used to access the API.
  Two mechanisms are available to access the HTTP API:
  - Basic authentication. This mechanism is enabled by default.
  - OAuth 2.0 token based authentication. It has to be explicitly added to the config file.

## Learning the basics

The first resources to read to learn about ejabberd ReST API configuration are
the following:

- [Simple API configuration](simple-configuration.md)
- Using ejabberd client API libraries and tools

<!-- TODO: Using API with ejabberd command-line tool and Go based library -->

The list of available commands is available in the [API Reference](admin-api.md) section.
Additionally, you can check at runtime what commands are available in your installed server using [ejabberdctl](../../admin/guide/managing.md#ejabberdctl):

``` sh
❯ ejabberdctl
Usage: ejabberdctl [--no-timeout] [--node nodename] [--version api_version] command [arguments]

Available commands in this ejabberd node:
  backup file
             Store internal Mnesia database to binary backup file
  ban_account user host reason
             Ban an account: kick sessions and set random password
  ...

❯ ejabberdctl help
  ...

❯ ejabberdctl help ban_account
  ...
```

## Next steps

You can dig deeper into ejabberd ReST API configuration on the following pages:

- [API Permissions](permissions.md)
- [OAuth Support](oauth.md)
- [Administration API Commands](admin-api.md)

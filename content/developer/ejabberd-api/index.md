# ejabberd ReST API

## Introduction

ejabberd comes with a extensive API that allows to perform administrative tasks from outside ejabberd:

1. Manage the XMPP server:
   restart the server, reload configuration, ...
2. Integrate the XMPP server with your existing platforms:
   create a MUC room when a new party starts in your platform, ...
3. Allow users to perform tasks using simple basic programs with no XMPP support:
   send a message from the smartwatch, show the number of offline messages...

The system is powerful, versatile, and you can configure access permissions very finely.
In the next sections you will learn the basic concepts,
how to start using ejabberd's API,
how to adjust it to your needs,
and integrate ejabberd with your existing systems.

def:api command
: Command defined in some ejabberd module API backend, that can be executed using some API frontend.

ejabberd's API currently includes over 200 API Commands,
see [API Reference](admin-api.md) for a detailed list of all the existing commands.
Alternatively you can view the list of commands grouped by their [API Tags](admin-api.md).

## API Backends

API commands are defined and implemented in Erlang or Elixir modules
that can be considered "API backends".
Some modules included in ejabberd define their commands,
while the majority of the existing commands are defined and implemented in:

- `ejabberd_admin`
- [mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)
- [mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

When developing a module in Erlang or Elixir, it can define new commands,
see [Commands](commands.md) page for details.

## API Frontends

The API commands are exposed through interfaces,
implemented in modules that can be considered "API frontends".
Available interfaces are:

- [ejabberdctl](../../admin/guide/managing.md#ejabberdctl) command-line tool
- [mod_http_api](../../admin/configuration/modules.md#mod_http_api) for HTTP ReST calls using JSON data
- [mod_adhoc_api](../../admin/configuration/modules.md#mod_adhoc_api) for calls using a XMPP client
- [WebAdmin](../../admin/guide/managing.md#web-admin) uses most commands to build the web pages
- [ejabberd_xmlrpc](../../admin/configuration/listen.md#ejabberd_xmlrpc) for XML-RPC calls (deprecated in favor of mod_http_api)

There are other interfaces available in the [ejabberd-contrib](../../admin/guide/modules.md#ejabberd-contrib) Github repository:

- [mod_rest](https://github.com/processone/ejabberd-contrib/tree/master/mod_rest) for HTTP ReST calls using plaintext data
- [mod_shcommands](https://github.com/processone/ejabberd-contrib/tree/master/mod_shcommands) for a WebAdmin page

## Process Flow

Let's review the process flow with one example:

1. API Client: Your web client (in this case Curl) sends an HTTP query
1. API Frontend: [mod_http_api](../../admin/configuration/modules.md#mod_http_api)
  checks the client authentication,
  permissions to execute the command
  and processes command arguments, then calls
1. API Backend: [mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)
  actually executes the command.
  In this case, it will query to proper internal ejabberd code.

``` mermaid
sequenceDiagram
  autonumber
  participant C as API Client<br/>curl
  box ejabberd
  participant F as API Frontend<br/>mod_http_api
  participant B as API Backend<br/>mod_admin_extra
  participant M as Module<br/>mod_last
  end
  Note right of C: HTTP Query
  C-->>F: POST /api/get_last<br/>{"user": "tom",<br/>"host": "localhost"}
  Note right of F: API Command Call
  F-->>B: get_last<br/>tom localhost
  Note right of B: Erlang Function Call
  B-->>M: mod_last:get_last_info<br/>(tom, localhost)
  activate M
  M-->>B: {ok,<br/>1743517196,<br/>"Disconnected"}
  deactivate M
  B-->>F: {"2025-04-01T14:19:56Z",<br/>"Disconnected"}
  F-->>C: 200 OK<br/>{"timestamp":<br/> "2025-04-01T14:19:56Z",<br/>"status":"Disconnected"}
```

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

<!-- TODO:: Using API with ejabberd command-line tool and Go based library -->

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

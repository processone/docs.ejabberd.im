---
title: API Versioning
menu: Versioning
order: 22
toc: true
---

# Introduction

It is possible to support different versions of the ejabberd API.
Versioning is used to ensure compatibility with third party
backend that uses the API.
This feature is available since ejabberd 24.XX.

If the API client does not specify the API version,
ejabberd uses by default the most recent available API version.

Alternatively, the API client can specify an API version,
and ejabberd will use that one to process the query,
or the most recent to the one specified.
For example: if a command is defined in API versions 0, 2, 3, 7, and 9,
and a client declares to support up to API version 5,
then ejabberd uses the command API version 3, which is the most recent available
for the one supported by the client.

API versioning is supported by
[`mod_http_api`](https://docs.ejabberd.im/admin/configuration/modules/#mod-http-api)
ReST interface and
[`ejabberdctl`](https://docs.ejabberd.im/admin/guide/managing/#ejabberdctl)
command line script.
However [`ejabberd_xmlrpc`](https://docs.ejabberd.im/admin/configuration/listen/#ejabberd-xmlrpc)
doesn't support API versioning, and consequently it can only use the latest API version.


# Command Definition

When a command is modified (either its
declaration or its definition, breaking compatibility), those
modifications should be done in a new version of the API, keeping the old
command still available in the previous API version.
An API version is an integer (sub-versions are not supported).

In practice, when you want to modify a command in a breaking way,
copy the `#ejabberd_commands` definition of that command, and in the new
definition set a `version` attribute with the API version (an integer)
where this command version is available.

There is no need to
add a new `#ejabberd_commands` record for commands that are not modified
in a given API version, immediate inferior version is used.

By default, all commands are in API version 0, and latest API is used
if no version is specified when calling `ejabberd_commands` directly
without specifying a version.


# API Documentation

The command documentation indicates the api version as a tag: `v1`, `v2`, ...
Commands not versioned do not have such a tag: they are version 0.

The [ejabberd Docs: API Tags](https://docs.ejabberd.im/developer/ejabberd-api/admin-tags/)
page lists the most recent API versions, and what commands are included.

To know exactly what is the format expected for a command in a specific API version,
use `ejabberdctl` specifying what API version you want to consult
and the command name, for example:
```
ejabberdctl --version 0 help get_roster
```


# `mod_http_api`

The server administrator can set the default version when configuring `request_handlers`,
by including a `vN` in its path, where `N` is an integer corresponding to the version.

In any case, the API client can specify a version when sending the request,
by appending `vN` to the request path.

For example, when configured like:
```yaml
listen:
  -
    request_handlers:
      /api/v0: mod_http_api
      /v1/api: mod_http_api
      /api: mod_http_api
```

see what API version will be used depending on the URL:

- `api/command` use the latest available version
- `api/command/v0` use version 0
- `api/command/v1` use version 1
- `v1/api/command` use version 1
- `v1/api/command/v0` use version 0
- `api/v0/command` use version 0
- `api/v0/command/v1` use version 1

In this example, the server administrator configured the default API version to 0:
```yaml
listen:
  -
    request_handlers:
      /api/v0: mod_http_api
```

the client doesn't specify any version, so 0 is used:
```
$ curl -k -X POST -H "Content-type: application/json" \
  -d '{}' "http://localhost:5280/api/v0/get_loglevel"
{"levelatom":"info"}
```

This time the client requests the API version 2:
```
$ curl -k -X POST -H "Content-type: application/json" \
  -d '{}' "http://localhost:5280/api/v0/get_loglevel/v2"
"info"
```


# `ejabberdctl`

By default the latest version of a given command is used.
To use a command in a specific API version, use the `--version` switch,
followed by the version number, and then the command name.

Example:
``` bash
ejabberdctl --version 2 set_loglevel 4
```

Use the most recent API version:
```
$ ejabberdctl get_roster admin localhost
jan@localhost jan   none    subscribe       group1,group2
tom@localhost tom   none    subscribe       group3
```

Use version 0:
```
$ ejabberdctl --version 0 get_roster admin localhost
jan@localhost jan   none    subscribe       group1;group2
tom@localhost tom   none    subscribe       group3
```

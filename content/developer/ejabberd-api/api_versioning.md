---
title: API Versioning
menu: Versioning
order: 22
toc: true
---

# Introduction

It is possible to support different versions of the ejabberd API (see
the *Global commands interface* documentation for details on the
ejabberd API).  Only REST and command line frontends support versions,
XML-RPC is not supported and can only use the latest API
version. Versioning is used to ensure compatibility with third party
backend using the API. When some commands are modified (either its
declaration or its definition, breaking compatibility), those
modifications can be done in a new version of the API, keeping the old
commands still available in the previous API version.

An API version is an integer (sub-versions are not supported).

# Command definition

If a command is modified, a new `#ejabberd_commands` record should be
defined with a `version` attribute set to the API version (an integer)
where this command version is available. There is no need to
add a new `#ejabberd_commands` record for commands that are not modified
in a given API version, immediate inferior version is used.

By default, all commands are in API version 0, and latest API is used
if no version is specified when calling `ejabberd_commands` directly
without specifying a version.

# API usage

## HTTP REST

To support a given HTTP REST API version, the `ejabberd_http` handler
defined in the `ejabberd.yml` configuration file associated to
`mod_http_api` must include a `vN` in its path, where `N` is an
integer corresponding to the version.

Example:

``` yaml
    request_handlers:
      "/api/v1": mod_http_api  # API version 1 handler
      "/v2/api": mod_http_api  # API version 2 handler
```
	  
- If there is no `vN` in the handler path, the API version 0 is used.
- In case there is several `vN` in the path the last one is used.

## Command line

By default, when using `ejabberdctl`, the latest version a given
command is used.  To use a command in a specific API version, use the
`--version` switch, followed by the version number, before the command
name.

Example:

``` bash
ejabberdctl --version 2 set_loglevel 4
```

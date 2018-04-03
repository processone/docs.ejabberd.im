---
title: ejabberd Rest API
menu: ejabberd ReST API
order: 20
toc: true
---

# Introduction

ejabberd comes with a powerful API that serve two goals:

1. It is primarily used to manage the XMPP service and integrate the platform with back-end platforms and script tools.
2. It also can be used from user clients to perform tasks on behalf of the users. This allows write simple basic clients
   that do not have to use XMPP protocol. This can be handy, for example to send a message from your smartwatch, or show
   the number of offline messages.

The system is powerful and very versatile and you can configure it very finely, but it can be quite daunting to set up.

This section is write to demystify ejabberd API configuration and help you
integrate ejabberd with your other back-ends or script through an HTTP / HTTPS ReST API.

# Understanding ejabberd "commands" 

ejabberd operations are organised around the concept of commands. ejabberd standard modules already provide many commands, but the mechanism is generic and any module can provide its own set of commands. This exposition of commands for third-party modules make it very powerful.

All commands can be exposed through interfaces. Available interfaces are: 

- ejabberdctl command-line tool,
- ejabberd ReST API and ejabberd XML-RPC API,
- to some extend, XMPP protocol itself through discovery and adhoc commands.

Any module in ejabberd can adds its own command through ejabberd Erlang/Elixir API, making the whole
system totally extensible. A third-party module can expose its own command and feel like a real part
of the system. A module that exposes commands makes it possible for server admin to expose it the way he wants.

ejabberd commands are universal, extensible and widely available through various configurable entrypoints.

_Note: The XML-RPC API still works but is deprecated in favor of the ReST API. You should migrate to ReST if you are using it._

<!-- TODO A diagram would be nice to have -->

# The role of ejabberd API

As we have seen, ejabberd API role is to provide and control access to ejabberd commands over HTTP/HTTPS.

Thus, ejabberd API primary goal is to grant access to some or all ejabberd "commands". 

An admin ejabberd ReST API requires:

- At least one admin user, if you plan to check credentials for command access (You can alternatively rely on originating IP addresses).
- HTTP/HTTPS handlers configured to expose the desired commands.
- The selection of authentication mechanisms that can be used to access the API.
  Two mechanisms are available to access the HTTP API:

    - Basic authentication. This mechanism is enabled by default.
    - OAuth 2.0 token based authentication. It has to be explicitely added to the config file.

# Learning the basics

The first resources to read to learn about ejabberd ReST API configuration are
the following:

* [Simple API configuration](/developer/ejabberd-api/simple-configuration/)
* Using ejabberd client API libraries and tools

<!--
TODO: Using API with ejabberd command-line tool and Go based library
-->

# Next steps

You can dig deeper into ejabberd ReST API configuration on the following pages:

* [API Permissions](/developer/ejabberd-api/permissions/)

* [OAuth Support](/developer/ejabberd-api/oauth/)

* [Administration API Commands](/developer/ejabberd-api/admin-api/)

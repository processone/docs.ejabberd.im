---
title: ejabberd Admin API
menu: ejabberd ReST API
order: 20
---

ejabberd comes with a powerful API to manage the service. The system is powerful
and very versatile and you can configure it very finely, but it can be quite
daunting to set up.

This section is write to demystify ejabberd API configuration and help you
integrate ejabberd with your other backends through an HTTP / HTTPS ReST API.


# Introduction

ejabberd API goal is to grant access to ejabberd "commands". Any module in
ejabberd can adds its own command through ejabberd Erlang API, making the whole
system totally extensible. A third-party module can exposes its own command and
feel like a real part of the system.

Every ejabberd command can be used in different ways:

- an ejabberd command can be used from `ejabberdctl` command-line tool.
- an ejabberd command can be exposed through ReST API.

Admin ejabberd ReST API requires:

- At least one admin user.
- HTTP/HTTPS handlers configured to expose the desired command.
- Selection of an authentication mechanism that can be used to access the API.
    Two mechanisms are available to access the HTTP API:

    - Basic authentication.
    - OAuth 2.0 token based authentication.

<!--
# Learning the basics

The first resources to read to learn about ejabberd ReST API configuration are
the following:

*

TODO: Using API with ejabberd command-line tool and Go based library
-->

# Next steps

You can dig deeper into ejabberd ReST API configuration on the following pages:

* [API Permissions](/developer/ejabberd-api/permissions/)

* [OAuth Support](/developer/ejabberd-api/oauth/)

* [Administration API Commands](/developer/ejabberd-api/admin-api/)

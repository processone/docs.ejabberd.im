---
title: Supporting and configuring specific XMPP Extensions in ejabberd | ejabberd documentation 
---

{:toc}

# Supporting and configuring specific XMPP Extensions in ejabberd

XMPP extensions support can be implemented in different ways in ejabberd. We have several cases:

1. The extension must be implemented as a core feature of ejabberd. In
   that case, it may be supported as default and / or have
   configuration options in ejabberd config files.
2. The extension can be optional and implemented as a module.
3. The extension may additionally need to be enabled on the client
   stream to use it (For example MAM or stream management).

This section is here to help you understand how they are implemented
XEP by XEP in ejabberd. We intend to explain what you can do in case
you want to support a given XEP or if you want to make sure a specific
Extension is disabled.

## XEP-0004: Data Forms

#### Specification

[XEP-0004: Data Forms](http://xmpp.org/extensions/xep-0004.html)

#### Implementation

ejabberd core

#### Comment

This extension is a general design principle for forms in XMPP. The
principles are applied by all services, components and modules inside
ejabberd.

#### Enabling / Disabling

As it is a general specification, it is used as default and cannot be
disabled.

## XEP-0012: Last Activity

#### Specification

[XEP-0012: Last Activity](http://xmpp.org/extensions/xep-0012.html)

#### Implementation

Main ejabberd module: [mod_last.erl](https://github.com/processone/ejabberd/blob/master/src/mod_last.erl)

#### Comment

This extension is optional. It allows the server to send back
information about when the user disconnected his last session.

It also allows to query the uptime of an ejabberd server.

#### Enabling

1. Add `mod_last` configuration in `modules` section of the
   configuration file.

It is enabled by default in ejabberd configuration template.

#### Disabling

2. Make sure `mod_last` is not defined or commented in ejabberd config
   `modules` section.
   
No side effect.

#### Module documentation

* [mod_last](/admin/guide/configuration/#modlast)

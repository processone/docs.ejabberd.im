---
title: Supporting and configuring specific XMPP Extensions in ejabberd
toc: true
menu: XMPP Extensions
order: 50
---

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

# XEP-0004: Data Forms

## Specification

[XEP-0004: Data Forms](http://xmpp.org/extensions/xep-0004.html)

## Implementation

ejabberd core

## Comment

This extension is a general design principle for forms in XMPP. The
principles are applied by all services, components and modules inside
ejabberd.

## Enabling / Disabling

As it is a general specification, it is used as default and cannot be
disabled.

# XEP-0012: Last Activity

## Specification

[XEP-0012: Last Activity](http://xmpp.org/extensions/xep-0012.html)

## Implementation

Main ejabberd module: [mod_last.erl](https://github.com/processone/ejabberd/blob/master/src/mod_last.erl)

## Comment

This extension is optional. It allows the server to send back
information about when the user disconnected their last session.

It also allows to query the uptime of an ejabberd server.

## Enabling

Add `mod_last` configuration in `modules` section of the
   configuration file.

It is enabled by default in ejabberd configuration template.

## Disabling

Make sure `mod_last` is not defined or is commented out in ejabberd
   config `modules` section.
   
No side effect.

## Module documentation

* [mod_last](/admin/guide/configuration/#mod-last)

# XEP-0013: Flexible Offline Message Retrieval

## Specification

[XEP-0013: Flexible Offline Message Retrieval](http://xmpp.org/extensions/xep-0013.html)

## Implementation

Main ejabberd module: [mod_offline.erl](https://github.com/processone/ejabberd/blob/master/src/mod_offline.erl)

## Comment

This extension is active on server if `mod_offline` module is enabled on ejabberd.

However, it is not used by client automatically. Flexible offline
message retrieval is enabled in the following cases:

* client send request to retrieve number of messages prior to sending
  its initial presence:
  [Requesting Number of Messages](http://xmpp.org/extensions/xep-0013.html#request-number)
* client send request to retrieve messages headers prior to sending
  its initial presence:
  [Requesting Message Headers](http://xmpp.org/extensions/xep-0013.html#request-headers)
* client send all messages retrieval request prior to sending its
  initial presence:
  [Retrieving All Messages](http://xmpp.org/extensions/xep-0013.html#retrieve-all)

## Enabling

Add `mod_offline` configuration in `modules` section of the
   configuration file.

It is enabled and can be used by client if mod_offline is
enabled. This is a module enabled by default in default ejabberd
configuration file template.

## Disabling

Make sure `mod_offline` is not defined or is commented out in
   ejabberd config `modules` section.
   
Side effect: It will disable all offline messages storage.

## Module documentation

* [mod_offline](/admin/guide/configuration/#mod-offline)

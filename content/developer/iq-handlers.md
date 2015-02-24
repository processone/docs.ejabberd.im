---
title: IQ Handlers | ejabberd documentation
---

# IQ Handlers

## Introduction

ejabberd internal modules can register themselves to handle XMPP IQ (Info-Query) using a
specific namespace, in a way similar to the hooks mechanism.

IQ are allowing clients to interact with XMPP services in a
synchronous way, using request and response mechanism.

Depending on the XML namespace used on the XMPP IQ, different services
will be leveraged. Having different modules handle different IQ
namespaces is our way to make service handling extremely modular.

## Example

The module
[mod_last.erl](https://github.com/processone/ejabberd/blob/master/src/mod_last.erl)
is using the IQ handling mechanism. It also uses the
[events/hooks](/developer/hooks/) mechanism.


---
title: IQ Handlers | ejabberd documentation
---

# IQ Handlers

## Introduction

ejabberd internal modules can register themselves to handle XMPP IQ
(Info-Query) using a specific namespace, in a way similar to the hooks
mechanism.

IQ are allowing clients to interact with XMPP services in a
synchronous way, using request and response mechanism.

Depending on the XML namespace used on the XMPP IQ, different services
will be leveraged. Having different modules handle different IQ
namespaces is our way to make service handling extremely modular.

## Type of IQ handlers

When you add an IQ handler, you can use one of the two types:

1. `ejabberd_local`: It means that the server will process IQ addressed
   to the server domain itself, with a specific namespace:
  
   For example:
   
   ~~~ erlang
   gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_LAST, ?MODULE, process_local_iq, IQDisc).
   ~~~ 
   
   Parameters are:
   
1. `ejabberd_sm`: This are IQ handlers that can be processed on the
   behalf of a domain user, when offline.
   
   For example:
   
   ~~~ erlang
   gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
	   		  ?NS_LAST, ?MODULE, process_sm_iq, IQDisc).
   ~~~

Modules are expected to handle specific namespaces, so namespace
matching is a mandatory part of the IQ handler registration and IQ
stanza dispatching.

## Example

The module
[mod_last.erl](https://github.com/processone/ejabberd/blob/master/src/mod_last.erl)
is using the IQ handling mechanism. It also uses the
[events/hooks](/developer/hooks/) mechanism.


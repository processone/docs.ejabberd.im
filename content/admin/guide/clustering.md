---
title: ejabberd cluster | ejabberd Installation and Operation Guide
bodyclass: nocomment
---

# Clustering

* This line is a placeholder to generate the table of contents
{:toc}
---

## Purpose

The purpose of ejabberd clustering is to be able to use several
servers for a single or small group of large domains, for
fault-tolerance and scalability.

Note that you do not necessarily need clustering if you want to run
two large domains independantly. You may simply want to run two
different independant servers.

However, to build reliable service and support large user base,
clustering is a must have feature.

## How it Works

A XMPP domain is served by one or more `ejabberd` nodes. These nodes can
be run on different machines that are connected via a network. They all
must have the ability to connect to port 4369 of all another nodes, and
must have the same magic cookie (see Erlang/OTP documentation, in other
words the file `~ejabberd/.erlang.cookie` must be the same on all
nodes). This is needed because all nodes exchange information about
connected users, s2s connections, registered services, etc…

Each `ejabberd` node has the following modules:

-   router,

-   local router,

-   session manager,

-   s2s manager.

### Router

This module is the main router of XMPP packets on each node. It routes
them based on their destination’s domains. It uses a global routing
table. The domain of the packet’s destination is searched in the routing
table, and if it is found, the packet is routed to the appropriate
process. If not, it is sent to the s2s manager.

### Local Router

This module routes packets which have a destination domain equal to one
of this server’s host names. If the destination JID has a non-empty user
part, it is routed to the session manager, otherwise it is processed
depending on its content.

### Session Manager

This module routes packets to local users. It looks up to which user
resource a packet must be sent via a presence table. Then the packet is
either routed to the appropriate c2s process, or stored in offline
storage, or bounced back.

### s2s Manager

This module routes packets to other XMPP servers. First, it checks if an
opened s2s connection from the domain of the packet’s source to the
domain of the packet’s destination exists. If that is the case, the s2s
manager routes the packet to the process serving this connection,
otherwise a new connection is opened.

## Before to get started

Before you start implementing clustering, there if a few things you
need to take into account:

- Cluster should be set up in a single data center: The clustering in
  ejabberd Community Edition rely on low latency network. While it may
  work across region, it is recommended that you run an ejabberd
  cluster in a single Amazon region.
- Clustering rely on Erlang feature and Mnesia shared schema. Before
  getting started, it is best to get familiar with Erlang environment
  as the wording will heavily reference Erlang terms.

## Clustering Setup

### Adding a node in a cluster

Suppose you already configured `ejabberd` on one machine named
(`first`), and you need to setup another one to make an `ejabberd`
cluster. Then do following steps:

1.  Copy `~ejabberd/.erlang.cookie` file from `first` to `second`.

	(alt) You can also add ‘`-setcookie content_of_.erlang.cookie`’
	option to all ‘`erl`’ commands below.

2.  Adding a node into the cluster is done by starting a new ejabberd
	node within the same network, and running a command from a cluster
	node. On `second` node for example, as ejabberd is already
	started, run the following command as the `ejabberd` daemon user,
	using the ejabberdctl script:

		#!console
		$ ejabberdctl join_cluster 'ejabberd@first'

	This enable ejabberd internals replications to be launched across
	all nodes so new node can start receiving messages from other
	nodes and be registered in the routing tables.

### Removing a node from the cluster

To remove a node from the cluster, it just has to be shut down. There
is no specific delay for the cluster to figure out that the node is
gone, the node is immediately removed from other routers entries. All
clients directly connected to the stopped node are disconnected, and
should reconnect to other nodes.

If the cluster is used behind a load balancer and the node has been
removed from the load balancer, no new clients should be connecting to
that node but established connections should be kept, thus allowing to
remove a node smoothly, by stopping it after most clients disconnected
by themselves.  If the node is started again, it's immediately
attached back to the cluster until it has been explicitly removed
permanently from the cluster.

To remove permanently a node from the cluster, a command must be run from the node to be removed:

	    #!console
	    $ ejabberdctl leave_cluster

# Service Load-Balancing

### Domain Load-Balancing Algorithm

`ejabberd` includes an algorithm to load balance the components that are
plugged on an `ejabberd` cluster. It means that you can plug one or
several instances of the same component on each `ejabberd` cluster and
that the traffic will be automatically distributed.

The default distribution algorithm try to deliver to a local instance of
a component. If several local instances are available, one instance is
chosen randomly. If no instance is available locally, one instance is
chosen randomly among the remote component instances.

If you need a different behaviour, you can change the load balancing
behaviour with the option `domain_balancing`. The syntax of the option
is the following:

`domain_balancing: BalancingCriteria`

:  

Several balancing criteria are available:

-   `destination`: the full JID of the packet `to` attribute is used.

-   `source`: the full JID of the packet `from` attribute is used.

-   `bare_destination`: the bare JID (without resource) of the packet
	`to` attribute is used.

-   `bare_source`: the bare JID (without resource) of the packet `from`
	attribute is used.

If the value corresponding to the criteria is the same, the same
component instance in the cluster will be used.

### Load-Balancing Buckets

When there is a risk of failure for a given component, domain balancing
can cause service trouble. If one component is failing the service will
not work correctly unless the sessions are rebalanced.

In this case, it is best to limit the problem to the sessions handled by
the failing component. This is what the
`domain_balancing_component_number` option does, making the load
balancing algorithm not dynamic, but sticky on a fix number of component
instances.

The syntax is:

`domain_balancing_component_number: Number`

:  

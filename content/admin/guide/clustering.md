---
title: ejabberd Clustering
toc: true
menu: Clustering
---

# Purpose

The purpose of ejabberd clustering is to be able to use several
servers for a single or small group of large domains, for
fault-tolerance and scalability.

Note that you do not necessarily need clustering if you want to run
two large domains independently. You may simply want to run two
different independent servers.

However, to build reliable service and support large user base,
clustering is a must have feature.

# How it Works

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

## Router

This module is the main router of XMPP packets on each node. It routes
them based on their destination’s domains. It uses a global routing
table. The domain of the packet’s destination is searched in the routing
table, and if it is found, the packet is routed to the appropriate
process. If not, it is sent to the s2s manager.

## Local Router

This module routes packets which have a destination domain equal to one
of this server’s host names. If the destination JID has a non-empty user
part, it is routed to the session manager, otherwise it is processed
depending on its content.

## Session Manager

This module routes packets to local users. It looks up to which user
resource a packet must be sent via a presence table. Then the packet is
either routed to the appropriate c2s process, or stored in offline
storage, or bounced back.

## s2s Manager

This module routes packets to other XMPP servers. First, it checks if an
opened s2s connection from the domain of the packet’s source to the
domain of the packet’s destination exists. If that is the case, the s2s
manager routes the packet to the process serving this connection,
otherwise a new connection is opened.

# Before you get started

Before you start implementing clustering, there are a few things you
need to take into account:

- Cluster should be set up in a single data center: The clustering in
  ejabberd Community Edition relies on low latency networking. While it may
  work across regions, it is recommended that you run an ejabberd
  cluster in a single Amazon region.
- Clustering relies on Erlang features and Mnesia shared schemas. Before
  getting started, it is best to get familiar with the Erlang environment
  as this guide will heavily reference Erlang terms.

# Clustering Setup

## Adding a node to a cluster

Suppose you have already configured `ejabberd` on one node named
`ejabberd01`. Let's create an additional node (`ejabberd02`) and connect them
together.

1. Copy the `/home/ejabberd/.erlang.cookie` file from `ejabberd01` to
   `ejabberd02`.

   Alternatively you could pass the `-setcookie <value>`
   option to all `erl` commands below.

2. Make sure your new ejabberd node is properly configured. Usually,
   you want to have the same `ejabberd.yml` config file on the new node that on the
   other cluster nodes.

3.  Adding a node to the cluster is done by starting a new `ejabberd`
	node within the same network, and running a command from a cluster
	node. On the `ejabberd02` node for example, as ejabberd is already
	started, run the following command as the `ejabberd` daemon user,
	using the ejabberdctl script:

    ~~~ bash
	$ ejabberdctl --no-timeout join_cluster 'ejabberd@ejabberd01'
    ~~~

	This enables ejabberd's internal replications to be launched
	across all nodes so new nodes can start receiving messages from
	other nodes and be registered in the routing tables.

## Removing a node from the cluster

To remove a node from the cluster, it just needs to be shut down. There
is no specific delay for the cluster to figure out that the node is
gone, the node is immediately removed from other router entries. All
clients directly connected to the stopped node are disconnected, and
should reconnect to other nodes.

If the cluster is used behind a load balancer and the node has been
removed from the load balancer, no new clients should be connecting to
that node but established connections should be kept, thus allowing to
remove a node smoothly, by stopping it after most clients disconnected
by themselves. If the node is started again, it's immediately
attached back to the cluster until it has been explicitly removed
permanently from the cluster.

To permanently remove a running node from the cluster, the following
command must be run as the `ejabberd` daemon user, from one node of the
cluster:

~~~ bash
$ ejabberdctl leave_cluster 'ejabberd@ejabberd02'
~~~

The removed node must be running while calling leave_cluster to make
it permanently removed. It's then immediately stopped.

## Restarting cluster nodes

Ejabberd Community Server uses mnesia internal database to manage cluster
and internode synchronisation. As a result, you may restart ejabberd nodes
as long as there is at least one running node. If you stop the last running
node of a cluster, you MUST restart that node first in order to get a running
service back.

# Service Load-Balancing

## Domain Load-Balancing Algorithm

`ejabberd` includes an algorithm to load balance the components that are
plugged on an `ejabberd` cluster. It means that you can plug one or
several instances of the same component on each `ejabberd` cluster and
that the traffic will be automatically distributed.

The default distribution algorithm attempts to deliver to a local instance of
a component. If several local instances are available, one instance is
chosen at random. If no instance is available locally, one instance is
randomly chosen among the remote component instances.

If you need a different behaviour, you can change the load balancing
behaviour with the
[domain_balancing](/admin/configuration/toplevel/#domain-balancing)
option.

## Load-Balancing Buckets

When there is a risk of failure for a given component, domain balancing
can cause service trouble. If one component is failing the service will
not work correctly unless the sessions are rebalanced.

In this case, it is best to limit the problem to the sessions handled by
the failing component. This is what the
`component_number` option does, making the load
balancing algorithm not dynamic, but sticky on a fix number of component
instances.
Check [domain_balancing](/admin/configuration/toplevel/#domain-balancing)
top-level option documentation for details.

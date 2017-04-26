---
title: Advanced Configuration
menu: Advanced Configuration
order: 100
toc: true
---

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
behaviour with the option `domain_balancing`. The syntax of the option
is the following:

**`domain_balancing: BalancingCriteria`**: Several balancing criterias are available:

-   `destination`: the full JID of the packet `to` attribute is used.

-   `source`: the full JID of the packet `from` attribute is used.

-   `bare_destination`: the bare JID (without resource) of the packet
	`to` attribute is used.

-   `bare_source`: the bare JID (without resource) of the packet `from`
	attribute is used.

If the value corresponding to the criteria is the same, the same
component instance in the cluster will be used.

## Load-Balancing Buckets

When there is a risk of failure for a given component, domain balancing
can cause service trouble. If one component is failing the service will
not work correctly unless the sessions are rebalanced.

In this case, it is best to limit the problem to the sessions handled by
the failing component. This is what the
`domain_balancing_component_number` option does, making the load
balancing algorithm not dynamic, but sticky on a fix number of component
instances.

The syntax is:

**`domain_balancing_component_number: Number`**
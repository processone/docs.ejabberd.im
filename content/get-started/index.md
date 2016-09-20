---
title: Getting started with ejabberd
---

# Getting started with ejabberd

## Overview

ejabberd is the de facto XMPP server in the world. The fact that it is
used to power the largest deployments in the world should not
intimidate you. ejabberd is equally suitable for small instances.

ejabberd has been designed from the ground-up, since 2002 for robust,
entreprise deployment. The goal has always been to shot for the moon
and that what made it a long-lasting success.

ejabberd is specifically designed for enterprise purposes: it is
fault-tolerant can utilise the resources of multiple clustered
machines, and easily scale when more capacity is required (by just
adding a box/VM).

Designed at a moment where clients were mostly for desktop and when
was supported only by a kind of HTTP polling call BOSH (), the project
managed to adapt to the recent changes, introducing Websockets
support, Bosh improvements and a solid mobile stack.

It was developed at a time where XMPP protocol was still known as
"Jabber", but fastly adopted the protocol evolution to support the
various version of the XMPP RFCs. It also encourages innovation and
experimentation by supporting most, if not all, extensions produced by
the XSF.

It relies on a dynamic community all over the world. To get an idea of
existing contributions, you can check
[ejabberd main repository](https://www.github.com/processone/ejabberd)
or the repository containing a great amount of
[contributed extensions](https://github.com/processone/ejabberd-contrib).

This is possible thanks to a very modular architecture based on a core
router and an extremely powerful plugin mechanism that is getting
richer every day.

Welcome in your journey for ejabberd mastery !

## Options to use ejabberd

ejabberd can be used in different ways. The most common one is to use
ejabberd Community Edition. This is the standard Open Source version
that everyone loves: highly scalable and flexible.

Fortunately, if you need more than just the ejabberd platform
software, ProcessOne can help you with a commercial
offering. Commercial offering come in two type of packaging:

- ejabberd Business Edition, including features for large companies
  (enhanced geodistributed companies and mobile support to develop
  own, rich clients) and world-clas support, that can please even the
  most demanding businesses, with 24/7 options.
- ejabberd SaaS being a way to access and benefit of all the features
  of ejabberd Business Edition at an attractive and scalable
  price. ejabberd SaaS allows you to keep control of your data thanks
  to integration API you can implement on your backend to become a
  data provider for ejabberd SaaS.

Whatever approach you choose, you can hardly make the wrong choice
with ejabberd.

## Architecture of an ejabberd service

ejabberd brings configurability, scalability and fault-tolerance to
the core feature of XMPP – routing messages.

Its architecture is based on a set of pluggable modules that enable
different features, including:

* One-to-one messaging
* Store-and-forward (offline messages)
* Contact list (roster) and presence
* Groupchat: MUC (Multi-User Chat)
* Messaging archiving with Message Archive Management (MAM)
* User presence extension: Personal Event Protocol (PEP) and typing indicator
* Privacy settings, through privacy list and simple blocking extensions
* User profile with vCards
* Full feature web support, with BOSH and websockets
* Stream management for message reliability on mobile (aka XEP-0198)
* Message Delivery Receipts (aka XEP-184)
* Last activity
* Metrics and full command-line administration
* and many many more.

The full list of supported protocol and extensions is available on
[Protocols Supported by ejabberd](http://www.process-one.net/en/ejabberd/protocols/)
page

This modular architecture allows high customisability and easy access
to the required features.

ejabberd enables authenticating users using external or internal
databases (Mnesia, SQL), LDAP or external scripts. It also allows
connecting anonymous users, when required.

For storing persistent data, ejabberd uses Mnesia (the distributed
internal Erlang database), but you can opt for other storage:

* SQL databases like MySQL or Postgres
* NoSQL databases like Riak (also written in Erlang)

And of course, thanks to its API, ejabberd can be customised to work
with a database chosen by the customer.

## Deploying and managing an ejabberd service

ejabberd can be deployed for a number of scenarios fitting end-user /
developer / customer needs. The default installation setup consists of
a single ejabberd node using Mnesia, so it does not require any
additional configuration. This primary system is sufficient for fast
deployment and connecting XMPP clients. It should be good enough for
most of the small deployments (and even medium ones).

A more scalable solution would be deploying ejabberd with an external
database for persistent data. As Mnesia is caching part of its data in
ejabberd memory (actually in Erlang VM node), this kind of setup make
your system more scalable and typically easier to integrate with your
usual database. As a sysadmin, yes, you can use your standard backup
process.

Those larger setup can run as a cluster of ejabberd nodes. This is a
clustering mode where all nodes are active, so it can be use for
fault-tolerance, but also to increase the capacity of your ejabberd
deployment.

With such a deployment you can load balance the traffic to your
cluster node using one of the following solution:

* traditional TCP/IP load balancer (beware of the cost of your
solution, typical XMPP connections are persistent).
* DNS load balancing.
* Custom approach that requires client cooperation.

If deployed on a 16 GB RAM machine with at least 4 cores, a single
ejabberd node can typically handle 200-300 K online users. This setup
is suitable for systems with up to 10 nodes.

Note that your mileage may vary depending on your use case, the
feature your are using and how clean the architecture design and the
client is developed. That's why, if you plan to reach huge volume, it
is recommended to start asking advices from day 1 to an
[ejabberd expert](http://www.process-one.net). Initial mistakes in the
solution design are harder to fix once the project is in production.

If the service requires a cluster of more than 10 nodes, we recommend
not relying on Mnesia clusting mode. Many solutions are available, the
easiest and more inexpensive being to rely on
[ejabberd Software-as-a-Service](http://www.process-one.net/en/ejabberd/saas/)
approach.

ejabberd also allows connecting different clusters as parts of larger
systems. This is a standard XMPP feature call server-to-server (aka
s2s in XMPP lingo). It is used in geo-localised services handling
massive traffic from all over the world. Special extension are also
available from ProcessOne to handle geodistribution in an even more
robust way.

To manage the users, rosters, messages and general settings, we
provide a command-line tool, ejabberdctl. That command-line allows you
to gather metrics from ejabberd to be able to monitor what is
happening in your system, understand and even anticipate issues.

The main benefit of ejabberd is the ability to reach a command-line to
type Erlang commands. This allows you to fix and troubleshoot most of
the tricky situation and even update and reload code without stopping
the service. This is a life saver for your uptime.

Welcome to the benefit of Erlang hot-code swapping!

## ejabberd is more than XMPP

Thanks to the modular architecture of ejabberd, the platform is
becoming a core component for messaging applications.

Messaging applications require to transfer more than text
messages. ejabberd has grow a full set of media related features that
makes ejabberd a great choice to support voice and video applications,
but also to proxy various kind of media transfer (images, audio and
video files for example).

As such, ejabberd support:

* Jingle, XMPP based voice protocol
* SIP (Session Initiation Protocol): Yes, you can pass SIP calls using ejabberd :)
* ICE (Interactive Connectivity Establishment: A Protocol for Network
Address Translator (NAT) Traversal)
* STUN
* TURN
* Proxy65 media relay

This makes ejabberd the best XMPP server to support SIP and WebRTC
based communication tools.

## Helping us in the development process

With thousands of more or less official forks, the core ejabberd team,
supported by ProcessOne, is constantly monitoring and reviewing
improvements.  We use our 15 years of experience to filter the best
ideas or improvements to make sure ejabberd is always your most solid
choice in term of scalability, robustness and manageability.

The best way to start developing for ejabberd is to clone, watch and
star the [project](https://www.github.com/processone/ejabberd) to get
in touch on our developer chatroom (ejabberd@conference.jabber.ru) or
to join the [forums](http://www.ejabberd.im).

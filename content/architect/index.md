---
title: ejabberd for architects | ejabberd documentation
---

# ejabberd for software architects

This section contains information to help your understand ejabberd
architecture and will explain how to integrate ejabberd properly into
your overall infrastructure.

## Overview

ejabberd is a configurable system where modules can be enabled or
disabled based on customer requirements. Users can connect not only
from a regular PC but also from mobile devices and from the web. User
data can be stored internally in Mnesia or in one of the support SQL
or NoSQL backend. Users can be totally managed by your own backend
through a ReST interface.

ejabberd support a core concept of XMPP: Federation. Federation is a
mechanism allowing different independing XMPP servers and clusters to
communicate with each other.

Here is a high level diagram of ejabberd internal architecture:

TODO: insert ejabberd architecture diagram.

## Virtual hosting

If you need to manage several small XMPP domains, ejabberd supports
virtual hosting. It means you can host as many domain as you want on a
single ejabberd deployment.

Instances can be made to be totally independant and invisible for each
other if needed (or they can communicate as they would through
federation).

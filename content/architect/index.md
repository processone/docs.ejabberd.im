---
title: ejabberd for architects
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

ejabberd internal architecture is organised around its router. Most of
the other elements are plugins that can be adapted, enhanced or
replaced to build a custom solution tailored to your needs.

ejabberd support a core concept of XMPP: Federation. Federation is a
mechanism allowing different independing XMPP servers and clusters to
communicate with each other.

Here is a high level diagram of ejabberd internal architecture:

[![][image-1]](/images/architect/ejabberd_internals.pdf)

[*Click for large size PDF*](/images/architect/ejabberd_internals.pdf)

## Typical large scale deployments

Here is a diagram for a typical ejabberd large scale deployment. It
can scale massively and rely on several back-ends.

[![][image-2]](/images/architect/ejabberd_large_scale.pdf)

[*Click for large size PDF*](/images/architect/ejabberd_large_scale.pdf)

Note that ejabberd ejabberd support a core concept of XMPP:
Federation. Federation is a mechanism allowing different independing
XMPP servers and clusters to communicate with each other. This is a
purely optional layer, but it can help integrate with the rest of the
world. It is also sometimes internally by companies to group users in
subsidiaries or regions.

## Virtual hosting

If you need to manage several small XMPP domains, ejabberd supports
virtual hosting. It means you can host as many domain as you want on a
single ejabberd deployment.

Instances can be made to be totally independant and invisible for each
other if needed (or they can communicate as they would through
federation).

[image-1]:	/images/architect/ejabberd_internals.png
[image-2]:	/images/architect/ejabberd_large_scale.png

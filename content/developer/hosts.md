---
title: Understanding ejabberd hosts
menu: Hosts
---

The host parameter is very commonly used through ejabberd code
base. It is very important to understand what it means and how it is
used.

# Component subdomains

Most XMPP service are reference as subdomain of the main XMPP
domain. For example, Multi User Chat or Publish-Subscribe are
available as subdomains of the main XMPP domain served by an
installation.

You can also plug external components to an ejabberd server. External
components will have their own subdomain as well and will be
exchanging data with ejabberd using a simplified XML stream.

Components can be written in any programming language. ejabberd
supports
[XEP-0114: Jabber Component Protocol](https://xmpp.org/extensions/xep-0114.html).

Note the external component have limited rights on the XMPP
server. As such, it is less powerful than an ejabberd module written
in Erlang or Elixir. Some proposed XMPP extensions, like
[Priviledge Entities](https://xmpp.org/extensions/xep-0356.html),
may grant more privileges in the future to external components.

# Virtual hosts

ejabberd fully support virtual hosting. It means that the same
ejabberd cluster can provide the service for multiple user bases using
a single deployment. Each virtual hosts is totally independent from
the other domain. They can have a different configurations.

# Glossary

<glossary::def>

---

def:bosh
: [Bidirectional-streams Over Synchronous HTTP](https://xmpp.org/about/technology-overview/#bosh): an extension for [XMPP](def:XMPP) defined in [XEP-0124](https://xmpp.org/extensions/xep-0124.html) that allows long lived connections for XMPP over HTTP.
  See [mod_bosh](admin/configuration/modules.md#mod_bosh).

def:command
: May refer to:

    - [](def:ad-hoc command)
    - [](def:API command)
    - shell command

def:eBE
: ejabberd Business Edition.
  See [Options to use ejabberd](get-started/index.md#options-to-use-ejabberd)

def:eCS
: ejabberd Community Server, see [Options to use ejabberd](get-started/index.md#options-to-use-ejabberd)

def:erlang
: [Erlang](https://www.erlang.org/) is a programming language used to build massively scalable soft real-time systems with requirements on high availability. Some of its uses are in telecoms, banking, e-commerce, computer telephony and instant messaging. Erlang's runtime system has built-in support for concurrency, distribution and fault tolerance.

def:iq
: [Stanza](def:Stanza) that provides a structured request-response mechanism.

def:jid
: [Jabber ID](https://xmpp.org/rfcs/rfc6122.html) is a unique identifier for an entity in the XMPP network (like a server, a service, an account, a chat room, ...). It consists of three parts: a local part, a domain part, and an optional resource part, formatted as `localpart@domain/resource`.

def:mnesia
: [Mnesia](https://www.erlang.org/doc/apps/mnesia/mnesia_chap1.html) is a distributed database specifically designed for industrial-grade telecommunications applications written in Erlang. It is used by default in ejabberd as it requires no initial configuration. However, for large volumes of data, it is recommended to [setup a SQL database](admin/configuration/database.md).

def:muc
: [Multi-User Chat](https://xmpp.org/about/technology-overview/#muc), an extension for [XMPP](def:XMPP) defined in [XEP-0045](https://xmpp.org/extensions/xep-0045.html) for multi-party information exchange whereby multiple users can exchange messages in the context of a chat room.
  See [mod_muc](admin/configuration/modules.md#mod_muc).

def:node
: May refer to:

    - disco node, see [mod_disco](admin/configuration/modules.md#mod_disco)
    - [](def:erlang node)
    - pubsub node, see [mod_pubsub](admin/configuration/modules.md#mod_pubsub)

def:omemo
: OMEMO is an extension for [XMPP](def:XMPP) defined in [XEP-0384](https://xmpp.org/extensions/xep-0384.html) that allows end-to-end encryption between clients.

def:otp
: [Open Telecom Platform](https://www.erlang.org/) is a set of Erlang libraries and design principles providing middle-ware to develop these systems. It includes its own distributed database, applications to interface towards other languages, debugging and release handling tools.

def:presence
: [Stanza](def:Stanza) that provides information about network availability of an account (online, away, offline, ...).

def:pubsub
: [Publish-Subscribe](https://xmpp.org/about/technology-overview/#pubsub), an extension for [XMPP](def:XMPP) defined in [XEP-0060](https://xmpp.org/extensions/xep-0060.html) for  for generic publish-subscribe functionality.
  See [mod_pubsub](admin/configuration/modules.md#mod_pubsub).

def:roster
: A list of contacts of an account, which is stored on the server, used to manage [presence](def:presence).

def:stanza
: [XMPP](def:XMPP) packet of type `message`, `presence` or [IQ](def:IQ).

def:vCard
: Virtual Card, an extension for [XMPP](def:XMPP) defined in [XEP-0054](https://xmpp.org/extensions/xep-0054.html) that allows to publish contact information like name, email, phone number, address, etc.
  See [mod_vcard](admin/configuration/modules.md#mod_vcard).

def:vhost
: Virtual Host, a domain name served by ejabberd that corresponds to a [XMPP Domain](admin/configuration/basic.md#xmpp-domains).

def:xmpp
: [Extensible Messaging and Presence Protocol](https://xmpp.org/about/technology-overview/): an open protocol based in XML for real-time communications.

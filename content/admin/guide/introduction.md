---
title: Introduction
menu: Introduction
order: 10
---

`ejabberd` is a *free and open source* instant messaging server written in [`Erlang/OTP`][1].

`ejabberd` is *cross-platform*, distributed, fault-tolerant, and based on open standards to achieve real-time communication.

`ejabberd` is designed to be a *rock-solid and feature rich* XMPP server.

`ejabberd` is suitable for small deployments, whether they need to be *scalable* or not, as well as extremely big deployments.

# Key Features

`ejabberd` is:

-   *Cross-platform:* `ejabberd` runs under Microsoft Windows and Unix derived systems such as Linux, FreeBSD and NetBSD.

-   *Distributed:* You can run `ejabberd` on a cluster of machines and all of them will serve the same Jabber domain(s). When you need more capacity you can simply add a new cheap node to your cluster. Accordingly, you do not need to buy an expensive high-end machine to support tens of thousands concurrent users.

-   *Fault-tolerant:* You can deploy an `ejabberd` cluster so that all the information required for a properly working service will be replicated permanently on all nodes. This means that if one of the nodes crashes, the others will continue working without disruption. In addition, nodes also can be added or replaced ‘on the fly’.

-   *Administrator Friendly:* `ejabberd` is built on top of the Erlang programming language. As a result, if you wish, you can perform self-contained deployments. You are not required to install an external database, an external web server, amongst others because everything is already included, and ready to run out of the box. Other administrator benefits include:

	-   Comprehensive documentation.
	-   Straightforward installers for Linux, Mac OS X, and Windows.
	-   Web Administration.
	-   Shared Roster Groups.
	-   Command line administration tool.
	-   Can integrate with existing authentication mechanisms.
	-   Capability to send announce messages.

-   *Internationalized:* `ejabberd` leads in internationalization. Hence it is very well suited to build service available across the world. Related features are:

	-   Translated to 25 languages.
	-   Support for [`IDNA`][2].

-   *Open Standards:* `ejabberd` is the first Open Source Jabber server claiming to fully comply to the XMPP standard.

	-   Fully XMPP compliant.
	-   XML-based protocol.
	-   [Many protocols supported][3].

# Additional Features

Moreover, `ejabberd` comes with a wide range of other state-of-the-art features:

-   Modular

	-   Load only the modules you want.
	-   Extend `ejabberd` with your own custom modules.


-   Security

	-   SASL and STARTTLS for c2s and s2s connections.
	-   STARTTLS and Dialback s2s connections.
	-   Web Admin accessible via HTTPS secure access.


-   Databases

	-   Internal database for fast deployment (Mnesia).
	-   Native MySQL support.
	-   Native PostgreSQL support.
	-   ODBC data storage support.
	-   Microsoft SQL Server support.
	-   SQLite support.
	-   Riak NoSQL database support.


-   Authentication

	-   Internal Authentication.
	-   PAM, LDAP, SQL and Riak.
	-   External Authentication script.


-   Others

	-   Support for virtual hosting.
	-   Compressing XML streams with Stream Compression
		([`XEP-0138`][4]).
	-   Statistics via Statistics Gathering
		([`XEP-0039`][5]).
	-   IPv6 support both for c2s and s2s connections.
	-   [`Multi-User Chat`][6] module with support for clustering and HTML logging.
	-   Users Directory based on users vCards.
	-   [`Publish-Subscribe`][7] component with support for
		[`Personal Eventing via Pubsub`][8].
	- Support for web clients: Support for [XMPP subprotocol for Websocket][9] and [`HTTP Binding (BOSH)`][10] services.
	-   IRC transport.
	-   SIP support.
	-   Component support: interface with networks such as AIM, ICQ and MSN installing special tranports.

[1]:	http://www.erlang.org/
[2]:	http://tools.ietf.org/html/rfc3490
[3]:	http://www.ejabberd.im/protocols
[4]:	http://xmpp.org/extensions/xep-0138.html
[5]:	http://xmpp.org/extensions/xep-0039.html
[6]:	http://xmpp.org/extensions/xep-0045.html
[7]:	http://xmpp.org/extensions/xep-0060.html
[8]:	http://xmpp.org/extensions/xep-0163.html
[9]:	https://tools.ietf.org/html/rfc7395
[10]:	http://xmpp.org/extensions/xep-0206.html

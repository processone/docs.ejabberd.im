---
hide:
  - navigation
---

# ejabberd Roadmap

## In the Works


## Planned

- **Remove support for Rebar2**

    ejabberd includes many tweaks to support rebar3 and rebar2. By removing support for rebar2, we could simplify rebar.config and other files a lot.
    But more importantly, dependencies would not need to be updated just because other dependencies are updated: Rebar2 requires exact version numbers to be provided, [Rebar3 doesn't require that](https://rebar3.org/docs/configuration/dependencies/#dependency-version-handling), and [neither does Mix](https://hexdocs.pm/elixir/Version.html#module-requirements).

## Released

This is a brief summary for each released version. For details, please consult ejabberd's [ChangeLog](../CHANGELOG.md).

### 2025

- [25.10](https://www.process-one.net/blog/ejabberd-25-10/)
    - [mod_configure](../admin/configuration/modules.md/#mod_configure): Added more Ad-Hoc Commands from [XEP-0133: Service Administration](https://xmpp.org/extensions/xep-0133.html)
    - [mod_muc](../admin/configuration/modules.md/#mod_muc): Updated support for [XEP-0317: Hats](https://xmpp.org/extensions/xep-0317.html)
    - Removed support for Erlang/OTP older than 25.0
    - Rename `New` SQL schema to `Multihost`, and `Default` to `Singlehost`

- [25.08](https://www.process-one.net/blog/ejabberd-25-08/)
    - New module [mod_providers](../admin/configuration/modules.md/#mod_providers)
    - [mod_matrix_gw](../admin/configuration/modules.md/#mod_matrix_gw): Support for Hydra rooms (room version 12, and other improvements
    - Fixed ACME in Erlang/OTP 28.0.2

- [25.07](https://www.process-one.net/blog/ejabberd-25-07/)
    - New module [mod_antispam](../admin/configuration/modules.md/#mod_antispam)
    - New module [mod_pubsub_serverinfo](../admin/configuration/modules.md/#mod_pubsub_serverinfo)
    - [mod_matrix_gw](../admin/configuration/modules.md/#mod_matrix_gw): Support for joining old versions of Matrix rooms too
    - New options [rest_proxy](../admin/configuration/toplevel.md#rest_proxy), [hosts_alias](../admin/configuration/toplevel.md#hosts_alias), [auth_password_types_hidden_in_sasl1](../admin/configuration/toplevel.md#auth_password_types_hidden_in_sasl1)
    - Erlang/OTP 25 required, OTP 28 supported

- [25.04](https://www.process-one.net/blog/ejabberd-25-04/)
    - Fix handling multiple occupant-id

- [25.03](https://www.process-one.net/blog/ejabberd-25-03/)
    - [mod_matrix_gw](../admin/configuration/modules.md/#mod_matrix_gw): Support for joining Matrix rooms as MUC rooms
    - [auth_stored_password_types](../admin/configuration/toplevel.md#auth_stored_password_types): Multiple Simultaneous Password Types
    - [mod_adhoc_api](../admin/configuration/modules.md/#mod_adhoc_api): New module to execute API commands using XMPP client
    - [Macros and Keywords](../admin/configuration/file-format.md#macros-and-keywords) Improvements
    - [CTL_OVER_HTTP](../admin/guide/managing.md#ctl_over_http): New option in `ejabberdctl` script
    - [Container images](../CONTAINER.md#images-comparison): reduce friction, use macros, webadmin port

### 2024

- [24.12](https://www.process-one.net/blog/ejabberd-24-12/)
    - XEP-0484: Fast Authentication Streamlining Tokens
    - Deprecation schedule for Erlang/OTP older than 25.0
    - Commands API v3

- [24.10](https://www.process-one.net/blog/ejabberd-24-10/)
    - New module [mod_s2s_bidi](../admin/configuration/modules.md/#mod_s2s_bidi)
    - New module [mod_scram_upgrade](../admin/configuration/modules.md/#mod_scram_upgrade)
    - IQ permission in [privileged](../admin/configuration/modules.md/#mod_privilege) entities
    - PubSub varied fixes
    - WebAdmin improvements

- [24.07](https://www.process-one.net/blog/ejabberd-24-07/)
    - Bugfixes and minor improvements

- [24.06](https://www.process-one.net/blog/ejabberd-24-06/)
    - Reworked the [ejabberd Docs](https://docs.ejabberd.im/) and moved to MkDocs+Material
    - [Automatic SQL schema](https://www.process-one.net/blog/automatic-schema-update-in-ejabberd/) is now enabled by default
    - Improved the ejabberd WebAdmin with support to use API commands
    - Support for UNIX Socket Domain in MySQL and PostgreSQL
    - Support Elixir 1.17 and Erlang/OTP 27.0

- [24.02](https://www.process-one.net/blog/ejabberd-24-02/)
    - [Matrix gateway](../admin/configuration/modules.md/#mod_matrix_gw)
    - [RFC 9266 Channel Bindings for TLS 1.3](https://www.rfc-editor.org/rfc/rfc9266)
    - [XEP-0386: Bind 2](https://xmpp.org/extensions/xep-0386.html)
    - [XEP-0388: Extensible SASL Profile (SASL2)](https://xmpp.org/extensions/xep-0388.html)
    - [XEP-0424: Message Retraction](https://xmpp.org/extensions/xep-0424.html)
    - [XEP-0440: SASL Channel-Binding Type Capability](https://xmpp.org/extensions/xep-0440.html)
    - [XEP-0474: SASL SCRAM Downgrade Protection](https://xmpp.org/extensions/xep-0474.html)
    - [XEP-0480: SASL Upgrade Tasks](https://xmpp.org/extensions/xep-0480.html)
    - [Automatic SQL schema creation and update](https://www.process-one.net/blog/automatic-schema-update-in-ejabberd/)
    - Commands [API versioning](https://github.com/processone/ejabberd/pull/4118)
    - Support Elixir 1.16 and Erlang/OTP 27.0-rc1

### 2023

- [23.10](https://www.process-one.net/blog/ejabberd-23-10/)
    - Support for [XEP-0402: PEP Native Bookmarks](https://xmpp.org/extensions/xep-0402.html)
    - Support for [XEP-0421: Occupant Id](https://xmpp.org/extensions/xep-0421.html)
    - MySQL Performance enhancements

- [23.04](https://www.process-one.net/blog/ejabberd-23-04/)
    - [`mod_mam`](../admin/configuration/modules.md#mod_mam) support for [XEP-0425: Message Moderation](https://xmpp.org/extensions/xep-0425.html)
    - New [`mod_muc_rtbl`](../admin/configuration/modules.md#mod_muc_rtbl): [Real-Time Block List](https://xmppbl.org/) for MUC rooms
    - Binaries use [Erlang/OTP](https://www.erlang.org/) 25.3, and changes in containers

- [23.01](https://www.process-one.net/blog/ejabberd-23-01/)
    - New [`mod_mqtt_bridge`](../admin/configuration/modules.md#mod_mqtt_bridge): MQTT bridge

### 2022

- [22.10](https://www.process-one.net/blog/ejabberd-22-10/)
    - Improved [MIX](https://xmpp.org/extensions/xep-0369.html) support
    -  Improved SQL reconnection Mechanism
    - Better burst traffix handling

- [22.05](https://www.process-one.net/blog/ejabberd-22-05/)
    - Improved MQTT, MUC and [ConverseJS](https://conversejs.org/) integration
    - New installers and container image
    - Support for [Erlang/OTP](https://www.erlang.org/) 25

### 2021

- [21.12](https://www.process-one.net/blog/ejabberd-21-12/)
    - New [`mod_conversejs`](../admin/configuration/modules.md#mod_conversejs): built-in [ConverseJS](https://conversejs.org/) web client
    - Support for [MUC Hats](https://xmpp.org/extensions/xep-0317.html) extension
    - PubSub, [MucSub](../developer/xmpp-clients-bots/extensions/muc-sub.md) and [Multicast](https://xmpp.org/extensions/xep-0033.html) improvements

* [21.07](https://www.process-one.net/blog/ejabberd-21-07/)
    * Improved database and caching for shared rosters
    * Broader multicast support for MUC
    * Improved rebar3 and Elixir support

* [21.04](https://www.process-one.net/blog/ejabberd-21-04/)
    * Full support for [Erlang/OTP 24](https://www.erlang.org/) and rebar3
    * New API commands
    * New CAPTCHA script

* [21.01](https://www.process-one.net/blog/ejabberd-21-01/)
    * Systemd watchdog support
    * STUN improvements

### 2020

* [20.12](https://www.process-one.net/blog/ejabberd-20-12/)
    * Extended SCRAM-SHA support
    * Microsoft ODBC Driver support

* [20.07](https://www.process-one.net/blog/ejabberd-20-07/)
    * Support for Unix Domain Sockets
    * [Erlang/OTP 23](https://www.erlang.org/) compatibility

* [20.04](https://www.process-one.net/blog/ejabberd-20-04/)
    * New [`mod_stun_disco`](../admin/configuration/modules.md#mod_stun_disco): support [XEP-0215](https://xmpp.org/extensions/xep-0215.html) including Audio/Video calls
    * Improved MS SQL support

* [20.03](https://www.process-one.net/blog/ejabberd-20-03/)
    * [SSL connection](../admin/configuration/database.md#sql-with-ssl-connection) to MySQL
    * Improved performance of `mod_shared_roster`

* [20.02](https://www.process-one.net/blog/ejabberd-20-02/)
    * Improved compatibility with CockroachDB
    * Emoji storage in MSSQL

* [20.01](https://www.process-one.net/blog/ejabberd-20-01/)
    * [OAuth support](../developer/ejabberd-api/oauth.md) for [ejabberd's MQTT](../admin/guide/mqtt/index.md)
    * New OTP 22 event logger
    * New [config parser](../admin/configuration/file-format.md) & validator

### 2019

* [19.09](https://www.process-one.net/blog/ejabberd-19-09/)
    * Significant improvements in ACME support: ACME v2
    * Erlang/OTP 19.3 is required

* [19.08](https://www.process-one.net/blog/ejabberd-19-08/)
    * New JWT (JSON Web Token) authentication
    * New configuration validator, yconf
    * Improved MUC scalability
    * Removed Riak support

* [19.05](https://www.process-one.net/blog/ejabberd-19-05/)
    * MQTT over WebSocket
    * Improved MucSub
    * Erlang/OTP 19.1 is required

* [19.02](https://www.process-one.net/blog/ejabberd-19-02-the-mqtt-edition/)
    * MQTT Support
    * MIX improvements

### 2018

* [18.12](https://www.process-one.net/blog/ejabberd-18-12/)
    * XML Compression in message archive storage
    * PROXY protocol support versions 1 and 2
    * MUC Self-Ping server optimisation (XEP-0410)
    * Bookmarks Conversion (XEP-0411)

* [18.09](https://www.process-one.net/blog/ejabberd-18-09/)
    * Default configuration file simplification
    * Improved logging
    * OpenSSL 1.1.1 support
    * Modular ejabberd core

* [18.06](https://www.process-one.net/blog/ejabberd-18-06/)
    * Stop ejabberd initialization on invalid/unknown options
    * Support SASL PLAIN
    * Drop support of mod_irc

* [18.04](https://www.process-one.net/blog/ejabberd-18-04/)

* [18.03](https://www.process-one.net/blog/ejabberd-18-03/)
    * New SQL schemas with server_host

* [18.01](https://www.process-one.net/blog/ejabberd-18-01/)

### 2017

* [17.12](https://www.process-one.net/blog/ejabberd-17-12/)
    * SNI (Server Name Indication) for inbound connections
    * Rewrite ejabberd system monitor
    * Support PubSub v1.14 and OMEMO

* [17.11](https://www.process-one.net/blog/ejabberd-17-11-happy-birthday-ejabberd/)
    * ACME Support
    * Introduce ‘certfiles’ global option
    * PubSub improved, and SQL storage

* [17.09](https://www.process-one.net/blog/ejabberd-17-09/)
    * New mod_avatar
    * SRV for XMPP over TLS

* [17.08](https://www.process-one.net/blog/ejabberd-17-08/)
    * XEP-0357: Push Notifications
    * Modular cluster with cluster_backend

* [17.07](https://www.process-one.net/blog/ejabberd-17-07/)

* [17.06](https://www.process-one.net/blog/ejabberd-17-06/)
    * New Caching system
    * Extended Riak support
    * Certificate manager

* [17.04](https://www.process-one.net/blog/ejabberd-17-04/)

* [17.03](https://www.process-one.net/blog/ejabberd-17-03/)
    * Modular code
    * Dynamic configuration reload
    * mod_blockstrangers for spam protection
    * S2S dialback

* [17.01](https://www.process-one.net/blog/ejabberd-17-01/)
    * PostgreSQL SSL support

### 2016

* [16.12](https://www.process-one.net/blog/ejabberd-16-12/)
    * New BOSH module
    * New Commands API permissions framework
    * XMPP packet handling using dedicated `xmpp` erlang library
    * New [ejaberd/mix Docker container](https://www.process-one.net/blog/ejabberd-development-with-docker/)

* [16.09](https://www.process-one.net/blog/ejabberd-16-09/)
    * Support for Elixir configuration file
    * XEP-0355 Namespace Delegation
    * XEP-0356 Privileged Entity

* [16.08](https://www.process-one.net/blog/ejabberd-16-08/)
    * New [MUC/Sub](https://www.process-one.net/blog/xmpp-mobile-groupchat-introducing-muc-subscription/)
    * Improved Elixir support
    * Major clean-up and improvement on OAuth ReST API

* [16.06](https://www.process-one.net/blog/ejabberd-16-06/)
    * New ACL (Access Control List) infrastructure

* [16.04](https://www.process-one.net/blog/ejabberd-16-04/)

* [16.03](https://www.process-one.net/blog/ejabberd-16-03-experimental-mix-support-ldap-sql-and-riak-improvements/)
    * Experimental support for [MIX (Mediated Information eXchange)](https://www.process-one.net/blog/experimental-mix-support-for-group-conversations-added-to-ejabberd/)
    * Erlang/OTP 17.5 required

* [16.02](https://www.process-one.net/blog/ejabberd-16-02-happy-leap-day/)
    * XEP-0013 Flexible Offline Message Retrieval
    * Improved Message Archive Management (MAM)
    * Published [ejabberd on hex.pm](https://hex.pm/packages/ejabberd)
    * Faster and more memory efficient XML parsing and TLS encryption.
    * Stream compression after SASL
    * Migration script from Prosody

* [16.01](https://www.process-one.net/blog/ejabberd-16-01/)

### 2015

* [15.11](https://www.process-one.net/blog/ejabberd-15-11-simpler-cluster-setup/)
    * Improved `join_cluster` and `leave_cluster`

* [15.10](https://www.process-one.net/blog/ejabberd-15-10-http-upload-metrics-and-performance/)
    * New mod_http_upload with support for [XEP-0363 HTTP File Upload](https://xmpp.org/extensions/xep-0363.html)
    * Added support for [Grapherl](https://www.process-one.net/blog/grapherl-google-summer-of-code-metrics-for-ejabberd/)

* [15.09](https://www.process-one.net/blog/ejabberd-15-09-oauth/)
    * OAuth 2.0 delegation framework
    * Preliminary OAuth and HTTP based ejabberd API
    * X-AUTH2 authentication mechanism

* [15.07](https://www.process-one.net/blog/ejabberd-15-07-released-summer-progress/)

* [15.06](https://www.process-one.net/blog/ejabberd-15-06/)
    * New mod_mam with [XEP-0313 Message Archive Management](https://xmpp.org/extensions/xep-0313.html)
    * Configuration checking on launch
    * Added Windows 7/8 installers, RPM and DEB packages
    * Document protocol support and version inside each module

* [15.04](https://www.process-one.net/blog/ejabberd-15-04/)
    * Added mod_admin_extra and mod_muc_admin
    * Added XEP-0033 Extended Stanza Addressing
    * Support to [embed ejabberd in an Elixir app](https://www.process-one.net/blog/embedding-ejabberd-into-an-elixir-phoenix-web-application/)
    * Erlang/OTP R16B03-1 is required

* [15.03](https://www.process-one.net/blog/ejabberd-15-03/)
    * Added support for WebSocket
    * Customizable session backends
    * SCRAM support for SQL authentication backend
    * Documentation was [converted from LaTeX to Markdown](https://www.process-one.net/blog/ejabberd-new-documentation-site-a-community-effort/) and published in [docs.ejabberd.im/](https://docs.ejabberd.im/)

* [15.02](https://www.process-one.net/blog/ejabberd-community-15-02/)
    * Added [Elixir support](https://www.process-one.net/blog/ejabberd-joins-the-elixir-revolution/)
    * New command to reload configuration withour restart
    * Bug tracker [moves from JIRA to GitHub Issues](https://www.process-one.net/blog/revamped-ejabberd-im-website-logo/)

* [Revamped ejabberd website, new logo, new development process](https://www.process-one.net/blog/revamped-ejabberd-im-website-logo/) and bugtracking migrated from JIRA to GitHub

### 2014

* [14.12](https://www.process-one.net/blog/ejabberd-community-14-12/)
    * New mod_client_state with XEP-0352: Client State Indication
    * New mod_fail2ban

* [14.07](https://www.process-one.net/blog/ejabberd-community-14-07/)
    * SIP Outbound (RFC 5626)

* [14.05](https://www.process-one.net/blog/ejabberd-community-14-05/)
    * RFC-3261 SIP proxy/registrar
    * RFC-5766 TURN: Traversal Using Relays around NAT
    * XEP-0198 Stream Management
    * XEP-0321 Remote Roster Management
    * Several improvements regarding encryption
    * New Bash completion script for ejabberdctl

### 2013

* [13.12](https://www.process-one.net/blog/ejabberd-community-13-12/)
    * New OpenSSL ciphers option in c2s, s2s and s2s_out
    * ejabberd_xmlrpc included

* [13.10](https://www.process-one.net/blog/ejabberd-community-13-10/)
    * [ejabberd configuration file in YAML format](https://www.process-one.net/blog/switch-ejabberd-configuration-to-yaml/)
    * Log files are created using Lager
    * Rebar2 is used to manage dependencies
    * Erlang/OTP R15 is required

* [13.03-beta1](https://www.process-one.net/blog/ejabberd-community-server-13-03-is-launched-in-beta/) ([announcement](https://web.archive.org/web/20220128081349/http://lists.jabber.ru/pipermail/ejabberd/2013-March/007974.html))
    * Binarize and indent code
    * New versioning scheme

### 2012

* [2.1.11](https://www.process-one.net/blog/ann-bugfix-release-ejabberd-2-1-11/)
    * Added ODBC support for several modules

### 2011

* [2.1.10](https://www.process-one.net/blog/new-releases-ejabberd-2110-and-exmpp-099/)

* [2.1.9](https://www.process-one.net/blog/new-releases-ejabberd-219-300-alpha-4-and-exmpp-098/)
    * New SASL SCRAM-SHA-1 authentication mechanism

### 2010

* [2.1.6](https://www.process-one.net/blog/ejabberd-216-release/)
    * mod_register: New captcha_protected option to require CAPTCHA
    * Support PostgreSQL 9.0

* October: the source code repository and the bug tracker were finally moved to GitHub

* [2.1.5](https://www.process-one.net/blog/ejabberd-215-and-exmpp-095-bugfix-releases/)

* [2.1.4](https://www.process-one.net/blog/ejabberd-214-and-exmpp-094-bugfix-releases/)
    * Full support for XEP-0115 Entity Capabilities v1.5

* [2.1.2](https://www.process-one.net/blog/ann-ejabberd-212-bugfix-release/)

### 2009

* [2.1.1](https://www.process-one.net/blog/ann-ejabberd-211-bugfix-release/)

* [2.1.0](https://www.process-one.net/blog/ejabberd-210-finally-released/)
    * LDAPS support
    * STUN server
    * New XEPs supported: XMPP Ping, Roster Versioning, [Import/Export Format](https://www.process-one.net/blog/ejabberd-migration-kit/)
    * Erlang/OTP R13 is supported

* [2.0.5](https://web.archive.org/web/20120122013929/https://www.process-one.net/en/ejabberd/release_notes/release_note_ejabberd_2.0.5/) ([announcement](https://www.process-one.net/blog/ejabberd-205-has-been-released/))

* [2.0.4](https://web.archive.org/web/20120122014454/https://www.process-one.net/en/ejabberd/release_notes/release_note_ejabberd_2.0.4/) ([announcement](https://www.process-one.net/blog/ejabberd-204-has-been-released/))

* [2.0.3](https://web.archive.org/web/20120122014449/https://www.process-one.net/en/ejabberd/release_notes/release_note_ejabberd_2.0.3/) ([announcement](https://www.process-one.net/blog/ejabberd-203-has-been-released/))

### 2008

* [2.0.2](https://web.archive.org/web/20120122014509/https://www.process-one.net/en/ejabberd/release_notes/release_note_ejabberd_202/) ([announcement](https://www.process-one.net/blog/ejabberd-202/))

* [2.0.1](https://web.archive.org/web/20120122013959/https://www.process-one.net/en/ejabberd/release_notes/release_note_ejabberd_201/) ([announcement](https://www.process-one.net/blog/ejabberd-201/))

* [2.0.0](https://web.archive.org/web/20120122013537/https://www.process-one.net/en/ejabberd/release_notes/release_note_ejabberd_200/) ([announcement](https://www.process-one.net/blog/ejabberd-200/))
    * New front-end and back-end cluster architecture
    * Complete rewrite of the PubSub module
    * New Proxy65 file transfer proxy
    * BOSH support
    * Many more improvements

### 2007

* [1.1.4](https://www.process-one.net/blog/ejabberd-114-released/)

* [1.1.3](https://web.archive.org/web/20220124230040/http://lists.jabber.ru/pipermail/ejabberd/2007-February/002440.html)

### 2006

* [1.1.2](https://web.archive.org/web/20061206003138/http://www.process-one.net/en/projects/ejabberd/releases/release_1.1.2.html) ([announcement](https://web.archive.org/web/20220124221010/http://lists.jabber.ru/pipermail/ejabberd/2006-September/002209.html))
    * LDAP improvements
    * Microsoft SQL supported
    * New Windows installer

* [1.1.1](https://web.archive.org/web/20060617072935/http://ejabberd.jabber.ru/ejabberd-1.1.1) ([announcement](https://web.archive.org/web/20220127211621/http://lists.jabber.ru/pipermail/ejabberd/2006-April/001751.html))
    * Erlang/OTP R9C-2 required

* [1.1.0](https://web.archive.org/web/20060624222148/http://www.process-one.net/en/projects/ejabberd/releases/release_1.1.0.html) ([announcement](https://web.archive.org/web/20220127203655/http://lists.jabber.ru/pipermail/ejabberd/2006-April/001726.html))
    * JEP-0050: Ad-Hoc Commands
    * JEP-0138: Stream Compression
    * JEP-0175: SASL anonymous
    * Native MySQL support
    * MUC improvement: Chatroom logging

### 2005

* [1.0.0](https://web.archive.org/web/20060613001514/http://www.process-one.net/en/projects/ejabberd/releases/release_1.0.0.html) ([announcement](https://web.archive.org/web/20220120011006/http://lists.jabber.ru/pipermail/ejabberd/2005-December/001481.html))
    * S2S encryption: STARTTLS + SASL_EXTERNAL and STARTTLS + Dialback
    * Different certificates can be defined for each virtual host.
    * Support for vCard storage in ODBC
    * New tool to convert Mnesia to ODBC
    * Native PostgreSQL support

* [0.9.8](https://web.archive.org/web/20060706014203/http://www.process-one.net/en/projects/ejabberd/releases/release_0.9.8.html) ([announcement](https://web.archive.org/web/20220118153712/http://lists.jabber.ru/pipermail/ejabberd/2005-August/001278.html))
    * Enhanced virtual hosting
    * Enhanced PubSub

* [0.9.1](https://web.archive.org/web/20060706014255/http://www.process-one.net/en/projects/ejabberd/releases/release_0.9.1.html) ([announcement](https://web.archive.org/web/20220116215413/http://lists.jabber.ru/pipermail/ejabberd/2005-May/001101.html))

* [0.9](https://web.archive.org/web/20060613001412/http://www.process-one.net/en/projects/ejabberd/releases/release_0.9.html) ([announcement](https://web.archive.org/web/20220125230349/http://lists.jabber.ru/pipermail/ejabberd/2005-April/000987.html))
    * Added support for virtual hosts
    * New mod_shared_roster
    * Added PostgreSQL support

* February: source code moved from SVN to Git, and the [bug tracker from Bugzilla to JIRA](https://www.process-one.net/blog/ejabberd-bug-tracker-open-for-registration/)

* Beginning of 2005, source code moved from JabberStudio CVS to ProcessOne SVN

### 2004

* October: website moved from JabberStudio to [ejabberd.jabber.ru](https://web.archive.org/web/20050730000817/http://ejabberd.jabber.ru/), and the bug tracker to Jabber.ru’s Bugzilla

* [0.7.5](https://web.archive.org/web/20220128023940/http://lists.jabber.ru/pipermail/ejabberd/2004-October/000337.html)
    * Support for STARTTLS with C2S connections
    * Support for authentification via external script
    * Added module which implement JUD and vCard services using LDAP
    * Improvements in web-based administration interface (user creation/removal, roster and offline queue management)
    * Support for message expiration (JEP-0023)
    * Support for HTTPS in web interface

* [0.7](https://web.archive.org/web/20220122152308/http://lists.jabber.ru/pipermail/ejabberd/2004-July/000129.html)
    * Support for LDAP authentification
    * Support for HTTP Polling
    * Support for web-based administration interface
    * Added command-line administration utility "ejabberdctl"
    * Support for history management in MUC rooms

### 2003

* 16th November, [0.5](https://web.archive.org/web/20211208160408/http://lists.jabber.ru/pipermail/ejabberd/2003-November/000052.html)
    * First release

* January, initial documentation in LaTeX: [Ejabberd Installation and Operation Guide](https://web.archive.org/web/20030409163941/http://ejabberd.jabberstudio.org/guide.html)

### 2002

* 18th November, [first commit](https://github.com/processone/ejabberd/commit/e0b348319ad6902ffcbb663e81c29b229c551b61) to CVS

* 16th November, first erlang modules written

---
search:
  boost: 1
---

# Modules Options

!!! info "Please note"

    This section describes modules options of ejabberd [24.12](../../archive/24.12/index.md).  If you are using an old ejabberd release, please refer to the corresponding archived version of this page in the [Archive](../../archive/index.md).

    The modules that changed in this version are marked with 🟤.

mod\_adhoc
----------

This module implements [XEP-0050: Ad-Hoc
Commands](https://xmpp.org/extensions/xep-0050.html). It’s an auxiliary
module and is only needed by some of the other modules.

__Available options:__

- **report\_commands\_node**: `true | false`  
Provide the Commands item in the Service Discovery. Default value:
`false`.

mod\_admin\_extra
-----------------

This module provides additional administrative commands.

Details for some commands:

[ban_account](../../developer/ejabberd-api/admin-api.md#ban_account) API: This command kicks all the connected sessions of
the account from the server. It also changes their password to a
randomly generated one, so they can’t login anymore unless a server
administrator changes their password again. It is possible to define the
reason of the ban. The new password also includes the reason and the
date and time of the ban. See an example below.

[push_roster](../../developer/ejabberd-api/admin-api.md#push_roster) API (and [push_roster_all](../../developer/ejabberd-api/admin-api.md#push_roster_all) API): The roster file must
be placed, if using Windows, on the directory where you installed
ejabberd: `C:/Program Files/ejabberd` or similar. If you use other
Operating System, place the file on the same directory where the .beam
files are installed. See below an example roster file.

[srg_create](../../developer/ejabberd-api/admin-api.md#srg_create) API: If you want to put a group Name with blank spaces,
use the characters `"`' and `'"` to define when the Name starts and
ends. See an example below.

The module has no options.

__Examples:__

With this configuration, vCards can only be modified with
mod\_admin\_extra commands:

~~~ yaml
acl:
  adminextraresource:
    - resource: "modadminextraf8x,31ad"
access_rules:
  vcard_set:
    - allow: adminextraresource
modules:
  mod_admin_extra: {}
  mod_vcard:
    access_set: vcard_set
~~~

Content of roster file for [push_roster](../../developer/ejabberd-api/admin-api.md#push_roster) API:

~~~ yaml
[{<<"bob">>, <<"example.org">>, <<"workers">>, <<"Bob">>},
{<<"mart">>, <<"example.org">>, <<"workers">>, <<"Mart">>},
{<<"Rich">>, <<"example.org">>, <<"bosses">>, <<"Rich">>}].
~~~

With this call, the sessions of the local account which JID is
`boby@example.org` will be kicked, and its password will be set to
something like
`BANNED_ACCOUNT—20080425T21:45:07—2176635—Spammed_rooms`

~~~ yaml
ejabberdctl vhost example.org ban_account boby "Spammed rooms"
~~~

Call to [srg_create](../../developer/ejabberd-api/admin-api.md#srg_create) API using double-quotes and single-quotes:

~~~ yaml
ejabberdctl srg_create g1 example.org "'Group number 1'" this_is_g1 g1
~~~

mod\_admin\_update\_sql
-----------------------

This module can be used to update existing SQL database from the default
to the new schema. Check the section
[Default and New Schemas](database.md#default-and-new-schemas) for
details. Please note that only MS SQL, MySQL, and PostgreSQL are
supported. When the module is loaded use [update_sql](../../developer/ejabberd-api/admin-api.md#update_sql) API.

The module has no options.

mod\_announce
-------------

This module enables configured users to broadcast announcements and to
set the message of the day (MOTD). Configured users can perform these
actions with an XMPP client either using Ad-hoc Commands or sending
messages to specific JIDs.

Note that this module can be resource intensive on large deployments as
it may broadcast a lot of messages. This module should be disabled for
instances of ejabberd with hundreds of thousands users.

The Ad-hoc Commands are listed in the Server Discovery. For this feature
to work, [mod_adhoc](#mod_adhoc) must be enabled.

The specific JIDs where messages can be sent are listed below. The first
JID in each entry will apply only to the specified virtual host
example.org, while the JID between brackets will apply to all virtual
hosts in ejabberd:

-   example.org/announce/all (example.org/announce/all-hosts/all):: The
    message is sent to all registered users. If the user is online and
    connected to several resources, only the resource with the highest
    priority will receive the message. If the registered user is not
    connected, the message will be stored offline in assumption that
    offline storage (see [mod_offline](#mod_offline)) is enabled.

-   example.org/announce/online
    (example.org/announce/all-hosts/online):: The message is sent to all
    connected users. If the user is online and connected to several
    resources, all resources will receive the message.

-   example.org/announce/motd (example.org/announce/all-hosts/motd)::
    The message is set as the message of the day (MOTD) and is sent to
    users when they login. In addition the message is sent to all
    connected users (similar to announce/online).

-   example.org/announce/motd/update
    (example.org/announce/all-hosts/motd/update):: The message is set as
    message of the day (MOTD) and is sent to users when they login. The
    message is not sent to any currently connected user.

-   example.org/announce/motd/delete
    (example.org/announce/all-hosts/motd/delete):: Any message sent to
    this JID removes the existing message of the day (MOTD).

__Available options:__

- **access**: `AccessName`  
This option specifies who is allowed to send announcements and to set
the message of the day. The default value is `none` (i.e. nobody is able
to send such messages).

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**: `true | false`  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

mod\_auth\_fast 🟤
------------------

<!-- md:version added in [24.12](../../archive/24.12/index.md) -->


The module adds support for [XEP-0484: Fast Authentication Streamlining
Tokens](https://xmpp.org/extensions/xep-0484.html) that allows users to
authenticate using self-managed tokens.

__Available options:__

- **db\_type**: `mnesia`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **token\_lifetime**: `timeout()`  
Time that tokens will be kept, measured from it’s creation time.
Default value set to 30 days

- **token\_refresh\_age**: `timeout()`  
This time determines age of token, that qualifies for automatic refresh.
Default value set to 1 day

__**Example**:__

~~~ yaml
modules:
  mod_auth_fast:
    token_lifetime: 14days
~~~

mod\_avatar
-----------

The purpose of the module is to cope with legacy and modern XMPP clients
posting avatars. The process is described in [XEP-0398: User Avatar to
vCard-Based Avatars
Conversion](https://xmpp.org/extensions/xep-0398.html).

Also, the module supports conversion between avatar image formats on the
fly.

The module depends on [mod_vcard](#mod_vcard), [mod_vcard_xupdate](#mod_vcard_xupdate) and
[mod_pubsub](#mod_pubsub).

__Available options:__

- **convert**: `{From: To}`  
Defines image conversion rules: the format in `From` will be converted
to format in `To`. The value of `From` can also be `default`, which is
match-all rule. NOTE: the list of supported formats is detected at
compile time depending on the image libraries installed in the system.

    **Example**:

    ~~~ yaml
    convert:
      webp: jpg
      default: png
    ~~~

- **rate\_limit**: `Number`  
Limit any given JID by the number of avatars it is able to convert per
minute. This is to protect the server from image conversion DoS. The
default value is `10`.

mod\_block\_strangers
---------------------

This module blocks and logs any messages coming from an unknown entity.
If a writing entity is not in your roster, you can let this module drop
and/or log the message. By default you’ll just not receive message from
that entity. Enable this module if you want to drop SPAM messages.

__Available options:__

- **access**: `AccessName`  
The option is supposed to be used when `allow_local_users` and
`allow_transports` are not enough. It’s an ACL where `deny` means the
message will be rejected (or a CAPTCHA would be generated for a
presence, if configured), and `allow` means the sender is whitelisted
and the stanza will pass through. The default value is `none`, which
means nothing is whitelisted.

- **allow\_local\_users**: `true | false`  
This option specifies if strangers from the same local host should be
accepted or not. The default value is `true`.

- **allow\_transports**: `true | false`  
If set to `true` and some server’s JID is in user’s roster, then
messages from any user of this server are accepted even if no
subscription present. The default value is `true`.

- **captcha**: `true | false`  
Whether to generate CAPTCHA or not in response to messages from
strangers. See also section [CAPTCHA](basic.md#captcha) of the
Configuration Guide. The default value is `false`.

- **drop**: `true | false`  
This option specifies if strangers messages should be dropped or not.
The default value is `true`.

- **log**: `true | false`  
This option specifies if strangers' messages should be logged (as info
message) in ejabberd.log. The default value is `false`.

mod\_blocking
-------------

The module implements [XEP-0191: Blocking
Command](https://xmpp.org/extensions/xep-0191.html).

This module depends on [mod_privacy](#mod_privacy) where all the configuration is
performed.

The module has no options.

mod\_bosh
---------

This module implements XMPP over BOSH as defined in
[XEP-0124](https://xmpp.org/extensions/xep-0124.html) and
[XEP-0206](https://xmpp.org/extensions/xep-0206.html). BOSH stands for
Bidirectional-streams Over Synchronous HTTP. It makes it possible to
simulate long lived connections required by XMPP over the HTTP protocol.
In practice, this module makes it possible to use XMPP in a browser
without WebSocket support and more generally to have a way to use XMPP
while having to get through an HTTP proxy.

__Available options:__

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**: `true | false`  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **json**: `true | false`  
This option has no effect.

- **max\_concat**: `pos_integer() | infinity`  
This option limits the number of stanzas that the server will send in a
single bosh request. The default value is `unlimited`.

- **max\_inactivity**: `timeout()`  
The option defines the maximum inactivity period. The default value is
`30` seconds.

- **max\_pause**: `pos_integer()`  
Indicate the maximum length of a temporary session pause (in seconds)
that a client can request. The default value is `120`.

- **prebind**: `true | false`  
If enabled, the client can create the session without going through
authentication. Basically, it creates a new session with anonymous
authentication. The default value is `false`.

- **queue\_type**: `ram | file`  
Same as top-level [queue_type](toplevel.md#queue_type) option, but applied to this module
only.

- **ram\_db\_type**: `mnesia | sql | redis`  
Same as top-level [default_ram_db](toplevel.md#default_ram_db) option, but applied to this module
only.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

__**Example**:__

~~~ yaml
listen:
  -
    port: 5222
    module: ejabberd_c2s
  -
    port: 5443
    module: ejabberd_http
    request_handlers:
      /bosh: mod_bosh

modules:
  mod_bosh: {}
~~~

mod\_caps
---------

This module implements [XEP-0115: Entity
Capabilities](https://xmpp.org/extensions/xep-0115.html). The main
purpose of the module is to provide PEP functionality (see
[mod_pubsub](#mod_pubsub)).

__Available options:__

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**: `true | false`  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

mod\_carboncopy
---------------

The module implements [XEP-0280: Message
Carbons](https://xmpp.org/extensions/xep-0280.html). The module
broadcasts messages on all connected user resources (devices).

The module has no options.

mod\_client\_state
------------------

This module allows for queueing certain types of stanzas when a client
indicates that the user is not actively using the client right now (see
[XEP-0352: Client State
Indication](https://xmpp.org/extensions/xep-0352.html)). This can save
bandwidth and resources.

A stanza is dropped from the queue if it’s effectively obsoleted by a
new one (e.g., a new presence stanza would replace an old one from the
same client). The queue is flushed if a stanza arrives that won’t be
queued, or if the queue size reaches a certain limit (currently 100
stanzas), or if the client becomes active again.

__Available options:__

- **queue\_chat\_states**: `true | false`  
Queue "standalone" chat state notifications (as defined in [XEP-0085:
Chat State Notifications](https://xmpp.org/extensions/xep-0085.html))
while a client indicates inactivity. The default value is `true`.

- **queue\_pep**: `true | false`  
Queue PEP notifications while a client is inactive. When the queue is
flushed, only the most recent notification of a given PEP node is
delivered. The default value is `true`.

- **queue\_presence**: `true | false`  
While a client is inactive, queue presence stanzas that indicate
(un)availability. The default value is `true`.

mod\_configure
--------------

The module provides server configuration functionality via [XEP-0050:
Ad-Hoc Commands](https://xmpp.org/extensions/xep-0050.html). Implements
many commands as defined in [XEP-0133: Service
Administration](https://xmpp.org/extensions/xep-0133.html). This module
requires [mod_adhoc](#mod_adhoc) to be loaded.

The module has no options.

mod\_conversejs
---------------

<!-- md:version added in [21.12](../../archive/21.12/index.md) and improved in [22.05](../../archive/22.05/index.md) -->


This module serves a simple page for the
[Converse](https://conversejs.org/) XMPP web browser client.

To use this module, in addition to adding it to the `modules` section,
you must also enable it in `listen` → `ejabberd_http` →
[request_handlers](listen-options.md#request_handlers).

Make sure either [mod_bosh](#mod_bosh) or
[ejabberd_http_ws](listen.md#ejabberd_http_ws) are enabled in at least
one `request_handlers`.

When `conversejs_css` and `conversejs_script` are `auto`, by default
they point to the public Converse client.

__Available options:__

- **bosh\_service\_url**: `auto | BoshURL`  
BOSH service URL to which Converse can connect to. The keyword *@HOST@*
is replaced with the real virtual host name. If set to `auto`, it will
build the URL of the first configured BOSH request handler. The default
value is `auto`.

- **conversejs\_css**: `auto | URL`  
Converse CSS URL. The keyword `@HOST@` is replaced with the hostname.
The default value is `auto`.

- **conversejs\_options**: `{Name: Value}`  
<!-- md:version added in [22.05](../../archive/22.05/index.md) -->
 Specify additional options to
be passed to Converse. See [Converse
configuration](https://conversejs.org/docs/html/configuration.html).
Only boolean, integer and string values are supported; lists are not
supported.

- **conversejs\_resources**: `Path`  
<!-- md:version added in [22.05](../../archive/22.05/index.md) -->
 Local path to the Converse
files. If not set, the public Converse client will be used instead.

- **conversejs\_script**: `auto | URL`  
Converse main script URL. The keyword `@HOST@` is replaced with the
hostname. The default value is `auto`.

- **default\_domain**: `Domain`  
Specify a domain to act as the default for user JIDs. The keyword
`@HOST@` is replaced with the hostname. The default value is `@HOST@`.

- **websocket\_url**: `auto | WebSocketURL`  
A WebSocket URL to which Converse can connect to. The `@HOST@` keyword
is replaced with the real virtual host name. If set to `auto`, it will
build the URL of the first configured WebSocket request handler. The
default value is `auto`.

__Examples:__

Manually setup WebSocket url, and use the public Converse client:

~~~ yaml
listen:
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      /bosh: mod_bosh
      /websocket: ejabberd_http_ws
      /conversejs: mod_conversejs

modules:
  mod_bosh: {}
  mod_conversejs:
    websocket_url: "ws://@HOST@:5280/websocket"
~~~

Host Converse locally and let auto detection of WebSocket and Converse
URLs:

~~~ yaml
listen:
  -
    port: 443
    module: ejabberd_http
    tls: true
    request_handlers:
      /websocket: ejabberd_http_ws
      /conversejs: mod_conversejs

modules:
  mod_conversejs:
    conversejs_resources: "/home/ejabberd/conversejs-9.0.0/package/dist"
~~~

Configure some additional options for Converse

~~~ yaml
modules:
  mod_conversejs:
    websocket_url: auto
    conversejs_options:
      auto_away: 30
      clear_cache_on_logout: true
      i18n: "pt"
      locked_domain: "@HOST@"
      message_archiving: always
      theme: dracula
~~~

mod\_delegation
---------------

This module is an implementation of [XEP-0355: Namespace
Delegation](https://xmpp.org/extensions/xep-0355.html). Only admin mode
has been implemented by now. Namespace delegation allows external
services to handle IQ using specific namespace. This may be applied for
external PEP service.

!!! warning

    Security issue: Namespace delegation gives components access to
    sensitive data, so permission should be granted carefully, only if you
    trust the component.

!!! note

    This module is complementary to [mod_privilege](#mod_privilege) but can also be used
    separately.

__Available options:__

- **namespaces**: `{Namespace: Options}`  
If you want to delegate namespaces to a component, specify them in this
option, and associate them to an access rule. The `Options` are:

    - **access**: `AccessName`  
   The option defines which components are
    allowed for namespace delegation. The default value is `none`.

    - **filtering**: `Attributes`  
   The list of attributes. Currently not
    used.

__Examples:__

Make sure you do not delegate the same namespace to several services at
the same time. As in the example provided later, to have the
`sat-pubsub.example.org` component perform correctly disable the
[mod_pubsub](#mod_pubsub) module.

~~~ yaml
access_rules:
  external_pubsub:
    allow: external_component
  external_mam:
    allow: external_component

acl:
  external_component:
    server: sat-pubsub.example.org

modules:
  mod_delegation:
    namespaces:
      urn:xmpp:mam:1:
        access: external_mam
      http://jabber.org/protocol/pubsub:
        access: external_pubsub
~~~

mod\_disco
----------

This module adds support for [XEP-0030: Service
Discovery](https://xmpp.org/extensions/xep-0030.html). With this module
enabled, services on your server can be discovered by XMPP clients.

__Available options:__

- **extra\_domains**: `[Domain, ...]`  
With this option, you can specify a list of extra domains that are added
to the Service Discovery item list. The default value is an empty list.

- **name**: `Name`  
A name of the server in the Service Discovery. This will only be
displayed by special XMPP clients. The default value is `ejabberd`.

- **server\_info**: `[Info, ...]`  
Specify additional information about the server, as described in
[XEP-0157: Contact Addresses for XMPP
Services](https://xmpp.org/extensions/xep-0157.html). Every `Info`
element in the list is constructed from the following options:

    - **modules**: `all | [Module, ...]`  
   The value can be the keyword
    `all`, in which case the information is reported in all the
    services, or a list of ejabberd modules, in which case the
    information is only specified for the services provided by those
    modules.

    - **name**: `Name`  
   The field `var` name that will be defined. See
    XEP-0157 for some standardized names.

    - **urls**: `[URI, ...]`  
   A list of contact URIs, such as HTTP URLs,
    XMPP URIs and so on.

    **Example**:

    ~~~ yaml
    server_info:
      -
        modules: all
        name: abuse-addresses
        urls: ["mailto:abuse@shakespeare.lit"]
      -
        modules: [mod_muc]
        name: "Web chatroom logs"
        urls: ["http://www.example.org/muc-logs"]
      -
        modules: [mod_disco]
        name: feedback-addresses
        urls:
          - http://shakespeare.lit/feedback.php
          - mailto:feedback@shakespeare.lit
          - xmpp:feedback@shakespeare.lit
      -
        modules:
          - mod_disco
          - mod_vcard
        name: admin-addresses
        urls:
          - mailto:xmpp@shakespeare.lit
          - xmpp:admins@shakespeare.lit
    ~~~

mod\_fail2ban
-------------

The module bans IPs that show the malicious signs. Currently only C2S
authentication failures are detected.

Unlike the standalone program, `mod_fail2ban` clears the record of
authentication failures after some time since the first failure or on a
successful authentication. It also does not simply block network
traffic, but provides the client with a descriptive error message.

!!! warning

    You should not use this module behind a proxy or load balancer.
    ejabberd will see the failures as coming from the load balancer and,
    when the threshold of auth failures is reached, will reject all
    connections coming from the load balancer. You can lock all your user
    base out of ejabberd when using this module behind a proxy.

__Available options:__

- **access**: `AccessName`  
Specify an access rule for whitelisting IP addresses or networks. If the
rule returns `allow` for a given IP address, that address will never be
banned. The `AccessName` should be of type `ip`. The default value is
`none`.

- **c2s\_auth\_ban\_lifetime**: `timeout()`  
The lifetime of the IP ban caused by too many C2S authentication
failures. The default value is `1` hour.

- **c2s\_max\_auth\_failures**: `Number`  
The number of C2S authentication failures to trigger the IP ban. The
default value is `20`.

mod\_host\_meta
---------------

<!-- md:version added in [22.05](../../archive/22.05/index.md) -->


This module serves small `host-meta` files as described in [XEP-0156:
Discovering Alternative XMPP Connection
Methods](https://xmpp.org/extensions/xep-0156.html).

To use this module, in addition to adding it to the `modules` section,
you must also enable it in `listen` → `ejabberd_http` →
[request_handlers](listen-options.md#request_handlers).

Notice it only works if [ejabberd_http](listen.md#ejabberd_http) has
[tls](listen-options.md#tls) enabled.

__Available options:__

- **bosh\_service\_url**: `undefined | auto | BoshURL`  
BOSH service URL to announce. The keyword `@HOST@` is replaced with the
real virtual host name. If set to `auto`, it will build the URL of the
first configured BOSH request handler. The default value is `auto`.

- **websocket\_url**: `undefined | auto | WebSocketURL`  
WebSocket URL to announce. The keyword `@HOST@` is replaced with the
real virtual host name. If set to `auto`, it will build the URL of the
first configured WebSocket request handler. The default value is `auto`.

__**Example**:__

~~~ yaml
listen:
  -
    port: 443
    module: ejabberd_http
    tls: true
    request_handlers:
      /bosh: mod_bosh
      /ws: ejabberd_http_ws
      /.well-known/host-meta: mod_host_meta
      /.well-known/host-meta.json: mod_host_meta

modules:
  mod_bosh: {}
  mod_host_meta:
    bosh_service_url: "https://@HOST@:5443/bosh"
    websocket_url: "wss://@HOST@:5443/ws"
~~~

mod\_http\_api
--------------

This module provides a ReST interface to call
[ejabberd API](../../developer/ejabberd-api/index.md) commands using
JSON data.

To use this module, in addition to adding it to the `modules` section,
you must also enable it in `listen` → `ejabberd_http` →
[request_handlers](listen-options.md#request_handlers).

To use a specific API version N, when defining the URL path in the
request\_handlers, add a vN. For example: `/api/v2: mod_http_api`.

To run a command, send a POST request to the corresponding URL:
`http://localhost:5280/api/COMMAND-NAME`

__Available options:__

- **default\_version 🟤`*: `integer() | string()*  
<!-- md:version added in [24.12](../../archive/24.12/index.md) -->
 What API version to use when
none is specified in the URL path. If setting an ejabberd version, it
will use the latest API version that was available in that ejabberd
version. For example, setting `"24.06"` in this option implies `2`. The
default value is the latest version.

__**Example**:__

~~~ yaml
listen:
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      /api: mod_http_api

modules:
  mod_http_api:
    default_version: 2
~~~

mod\_http\_fileserver
---------------------

This simple module serves files from the local disk over HTTP.

__Available options:__

- **accesslog**: `Path`  
File to log accesses using an Apache-like format. No log will be
recorded if this option is not specified.

- **content\_types**: `{Extension: Type}`  
Specify mappings of extension to content type. There are several content
types already defined. With this option you can add new definitions or
modify existing ones. The default values are:

    **Example**:

    ~~~ yaml
    content_types:
      .css: text/css
      .gif: image/gif
      .html: text/html
      .jar: application/java-archive
      .jpeg: image/jpeg
      .jpg: image/jpeg
      .js: text/javascript
      .png: image/png
      .svg: image/svg+xml
      .txt: text/plain
      .xml: application/xml
      .xpi: application/x-xpinstall
      .xul: application/vnd.mozilla.xul+xml
    ~~~

- **custom\_headers**: `{Name: Value}`  
Indicate custom HTTP headers to be included in all responses. There are
no custom headers by default.

- **default\_content\_type**: `Type`  
Specify the content type to use for unknown extensions. The default
value is `application/octet-stream`.

- **directory\_indices**: `[Index, ...]`  
Indicate one or more directory index files, similarly to Apache’s
`DirectoryIndex` variable. When an HTTP request hits a directory instead
of a regular file, those directory indices are looked in order, and the
first one found is returned. The default value is an empty list.

- **docroot**: `Path`  
Directory to serve the files from. This is a mandatory option.

- **must\_authenticate\_with**: `[{Username, Hostname}, ...]`  
List of accounts that are allowed to use this service. Default value:
`[]`.

__Examples:__

This example configuration will serve the files from the local directory
`/var/www` in the address `http://example.org:5280/pub/content/`. In
this example a new content type `ogg` is defined, `png` is redefined,
and `jpg` definition is deleted:

~~~ yaml
listen:
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      /pub/content: mod_http_fileserver

modules:
  mod_http_fileserver:
    docroot: /var/www
    accesslog: /var/log/ejabberd/access.log
    directory_indices:
      - index.html
      - main.htm
    custom_headers:
      X-Powered-By: Erlang/OTP
      X-Fry: "It's a widely-believed fact!"
    content_types:
      .ogg: audio/ogg
      .png: image/png
    default_content_type: text/html
~~~

mod\_http\_upload
-----------------

This module allows for requesting permissions to upload a file via HTTP
as described in [XEP-0363: HTTP File
Upload](https://xmpp.org/extensions/xep-0363.html). If the request is
accepted, the client receives a URL for uploading the file and another
URL from which that file can later be downloaded.

In order to use this module, it must be enabled in `listen` →
`ejabberd_http` →
[request_handlers](listen-options.md#request_handlers).

__Available options:__

- **access**: `AccessName`  
This option defines the access rule to limit who is permitted to use the
HTTP upload service. The default value is `local`. If no access rule of
that name exists, no user will be allowed to use the service.

- **custom\_headers**: `{Name: Value}`  
This option specifies additional header fields to be included in all
HTTP responses. By default no custom headers are included.

- **dir\_mode**: `Permission`  
This option defines the permission bits of the `docroot` directory and
any directories created during file uploads. The bits are specified as
an octal number (see the `chmod(1)` manual page) within double quotes.
For example: `"0755"`. The default is undefined, which means no explicit
permissions will be set.

- **docroot**: `Path`  
Uploaded files are stored below the directory specified (as an absolute
path) with this option. The keyword `@HOME@` is replaced with the home
directory of the user running ejabberd, and the keyword `@HOST@` with
the virtual host name. The default value is `"@HOME@/upload"`.

- **external\_secret**: `Text`  
This option makes it possible to offload all HTTP Upload processing to a
separate HTTP server. Both ejabberd and the HTTP server should share
this secret and behave exactly as described at [Prosody’s
mod\_http\_upload\_external:
Implementation](https://modules.prosody.im/mod_http_upload_external.html#implementation).
There is no default value.

- **file\_mode**: `Permission`  
This option defines the permission bits of uploaded files. The bits are
specified as an octal number (see the `chmod(1)` manual page) within
double quotes. For example: `"0644"`. The default is undefined, which
means no explicit permissions will be set.

- **get\_url**: `URL`  
This option specifies the initial part of the GET URLs used for
downloading the files. The default value is `undefined`. When this
option is `undefined`, this option is set to the same value as
`put_url`. The keyword `@HOST@` is replaced with the virtual host name.
NOTE: if GET requests are handled by this module, the `get_url` must
match the `put_url`. Setting it to a different value only makes sense
if an external web server or [mod_http_fileserver](#mod_http_fileserver) is used to serve
the uploaded files.

- **host**  
Deprecated. Use `hosts` instead.

- **hosts**: `[Host, ...]`  
This option defines the Jabber IDs of the service. If the `hosts` option
is not specified, the only Jabber ID will be the hostname of the virtual
host with the prefix `"upload."`. The keyword `@HOST@` is replaced with
the real virtual host name.

- **jid\_in\_url**: `node | sha1`  
When this option is set to `node`, the node identifier of the user’s JID
(i.e., the user name) is included in the GET and PUT URLs generated by
`mod_http_upload`. Otherwise, a SHA-1 hash of the user’s bare JID is
included instead. The default value is `sha1`.

- **max\_size**: `Size`  
This option limits the acceptable file size. Either a number of bytes
(larger than zero) or `infinity` must be specified. The default value is
`104857600`.

- **name**: `Name`  
A name of the service in the Service Discovery. The default value is
`"HTTP File Upload"`. Please note this will only be displayed by some
XMPP clients.

- **put\_url**: `URL`  
This option specifies the initial part of the PUT URLs used for file
uploads. The keyword `@HOST@` is replaced with the virtual host name.
NOTE: different virtual hosts cannot use the same PUT URL. The default
value is `"https://@HOST@:5443/upload"`.

- **rm\_on\_unregister**: `true | false`  
This option specifies whether files uploaded by a user should be removed
when that user is unregistered. The default value is `true`.

- **secret\_length**: `Length`  
This option defines the length of the random string included in the GET
and PUT URLs generated by `mod_http_upload`. The minimum length is `8`
characters, but it is recommended to choose a larger value. The default
value is `40`.

- **service\_url**  
Deprecated.

- **thumbnail**: `true | false`  
This option specifies whether ejabberd should create thumbnails of
uploaded images. If a thumbnail is created, a &lt;thumbnail/&gt; element
that contains the download &lt;uri/&gt; and some metadata is returned
with the PUT response. The default value is `false`.

- **vcard**: `vCard`  
A custom vCard of the service that will be displayed by some XMPP
clients in Service Discovery. The value of `vCard` is a YAML map
constructed from an XML representation of vCard. Since the
representation has no attributes, the mapping is straightforward.

    **Example**:

    ~~~ yaml
    # This XML representation of vCard:
    #   <vCard xmlns='vcard-temp'>
    #     <FN>Conferences</FN>
    #     <ADR>
    #       <WORK/>
    #       <STREET>Elm Street</STREET>
    #     </ADR>
    #   </vCard>
    #
    # is translated to:
    vcard:
      fn: Conferences
      adr:
        -
          work: true
          street: Elm Street
    ~~~

__**Example**:__

~~~ yaml
listen:
  -
    port: 5443
    module: ejabberd_http
    tls: true
    request_handlers:
      /upload: mod_http_upload

modules:
  mod_http_upload:
    docroot: /ejabberd/upload
    put_url: "https://@HOST@:5443/upload"
~~~

mod\_http\_upload\_quota
------------------------

This module adds quota support for mod\_http\_upload.

This module depends on [mod_http_upload](#mod_http_upload).

__Available options:__

- **access\_hard\_quota**: `AccessName`  
This option defines which access rule is used to specify the "hard
quota" for the matching JIDs. That rule must yield a positive number for
any JID that is supposed to have a quota limit. This is the number of
megabytes a corresponding user may upload. When this threshold is
exceeded, ejabberd deletes the oldest files uploaded by that user until
their disk usage equals or falls below the specified soft quota (see
also option `access_soft_quota`). The default value is
`hard_upload_quota`.

- **access\_soft\_quota**: `AccessName`  
This option defines which access rule is used to specify the "soft
quota" for the matching JIDs. That rule must yield a positive number of
megabytes for any JID that is supposed to have a quota limit. See the
description of the `access_hard_quota` option for details. The default
value is `soft_upload_quota`.

- **max\_days**: `Days`  
If a number larger than zero is specified, any files (and directories)
older than this number of days are removed from the subdirectories of
the `docroot` directory, once per day. The default value is `infinity`.

__Examples:__

Notice it’s not necessary to specify the `access_hard_quota` and
`access_soft_quota` options in order to use the quota feature. You can
stick to the default names and just specify access rules such as those
in this example:

~~~ yaml
shaper_rules:
  soft_upload_quota:
    1000: all # MiB
  hard_upload_quota:
    1100: all # MiB

modules:
  mod_http_upload: {}
  mod_http_upload_quota:
    max_days: 100
~~~

mod\_jidprep
------------

This module allows XMPP clients to ask the server to normalize a JID as
per the rules specified in [RFC 6122: XMPP Address
Format](https://tools.ietf.org/html/rfc6122). This might be useful for
clients in certain constrained environments, or for testing purposes.

__Available options:__

- **access**: `AccessName`  
This option defines which access rule will be used to control who is
allowed to use this service. The default value is `local`.

mod\_last
---------

This module adds support for [XEP-0012: Last
Activity](https://xmpp.org/extensions/xep-0012.html). It can be used to
discover when a disconnected user last accessed the server, to know when
a connected user was last active on the server, or to query the uptime
of the ejabberd server.

__Available options:__

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**: `true | false`  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

mod\_legacy\_auth
-----------------

The module implements [XEP-0078: Non-SASL
Authentication](https://xmpp.org/extensions/xep-0078.html).

!!! note

    This type of authentication was obsoleted in 2008 and you unlikely
    need this module unless you have something like outdated Jabber bots.

The module has no options.

mod\_mam
--------

This module implements [XEP-0313: Message Archive
Management](https://xmpp.org/extensions/xep-0313.html) and [XEP-0441:
Message Archive Management
Preferences](https://xmpp.org/extensions/xep-0441.html). Compatible XMPP
clients can use it to store their chat history on the server.

__Available options:__

- **access\_preferences**: `AccessName`  
This access rule defines who is allowed to modify the MAM preferences.
The default value is `all`.

- **assume\_mam\_usage**: `true | false`  
This option determines how ejabberd’s stream management code (see
[mod_stream_mgmt](#mod_stream_mgmt)) handles unacknowledged messages when the connection
is lost. Usually, such messages are either bounced or resent. However,
neither is done for messages that were stored in the user’s MAM archive
if this option is set to `true`. In this case, ejabberd assumes those
messages will be retrieved from the archive. The default value is
`false`.

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**: `true | false`  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **clear\_archive\_on\_room\_destroy**: `true | false`  
Whether to destroy message archive of a room (see [mod_muc](#mod_muc)) when it
gets destroyed. The default value is `true`.

- **compress\_xml**: `true | false`  
When enabled, new messages added to archives are compressed using a
custom compression algorithm. This feature works only with SQL backends.
The default value is `false`.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **default**: `always | never | roster`  
The option defines default policy for chat history. When `always` is set
every chat message is stored. With `roster` only chat history with
contacts from user’s roster is stored. And `never` fully disables chat
history. Note that a client can change its policy via protocol commands.
The default value is `never`.

- **request\_activates\_archiving**: `true | false`  
If the value is `true`, no messages are stored for a user until their
client issue a MAM request, regardless of the value of the `default`
option. Once the server received a request, that user’s messages are
archived as usual. The default value is `false`.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

- **user\_mucsub\_from\_muc\_archive**: `true | false`  
When this option is disabled, for each individual subscriber a separate
mucsub message is stored. With this option enabled, when a user fetches
archive virtual mucsub, messages are generated from muc archives. The
default value is `false`.

mod\_matrix\_gw
---------------

<!-- md:version added in [24.02](../../archive/24.02/index.md) -->


[Matrix](https://matrix.org/) gateway. Erlang/OTP 25 or higher is
required to use this module.

__Available options:__

- **host**: `Host`  
This option defines the Jabber IDs of the service. If the `host` option
is not specified, the Jabber ID will be the hostname of the virtual host
with the prefix `"matrix."`. The keyword `@HOST@` is replaced with the
real virtual host name.

- **key**: `string()`  
Value of the matrix signing key, in base64.

- **key\_name**: `string()`  
Name of the matrix signing key.

- **matrix\_domain**: `Domain`  
Specify a domain in the Matrix federation. The keyword `@HOST@` is
replaced with the hostname. The default value is `@HOST@`.

- **matrix\_id\_as\_jid**: `true | false`  
If set to `true`, all packets failing to be delivered via an XMPP
server-to-server connection will then be routed to the Matrix gateway by
translating a Jabber ID `user@matrixdomain.tld` to a Matrix user
identifier `@user:matrixdomain.tld`. When set to `false`, messages must
be explicitly sent to the matrix gateway service Jabber ID to be routed
to a remote Matrix server. In this case, to send a message to Matrix
user `@user:matrixdomain.tld`, the client must send a message to the JID
`user%<matrixdomain.tld@matrix.myxmppdomain>.tld`, where
`matrix.myxmppdomain.tld` is the JID of the gateway service as set by
the `host` option. The default is `false`.

__**Example**:__

~~~ yaml
listen:
  -
    port: 8448
    module: ejabberd_http
    tls: true
    request_handlers:
      "/_matrix": mod_matrix_gw

modules:
  mod_matrix_gw:
    key_name: "key1"
    key: "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    matrix_id_as_jid: true
~~~

mod\_metrics
------------

This module sends events to external backend (by now only
[grapherl](https://github.com/processone/grapherl) is supported).
Supported events are:

-   sm\_register\_connection

-   sm\_remove\_connection

-   user\_send\_packet

-   user\_receive\_packet

-   s2s\_send\_packet

-   s2s\_receive\_packet

-   register\_user

-   remove\_user

-   offline\_message

When enabled, every call to these hooks triggers a counter event to be
sent to the external backend.

__Available options:__

- **ip**: `IPv4Address`  
IPv4 address where the backend is located. The default value is
`127.0.0.1`.

- **port**: `Port`  
An internet port number at which the backend is listening for incoming
connections/packets. The default value is `11111`.

mod\_mix
--------

<!-- md:version added in 16.03 and improved in 19.02 -->


This module is an experimental implementation of [XEP-0369: Mediated
Information eXchange (MIX)](https://xmpp.org/extensions/xep-0369.html).
It’s asserted that the MIX protocol is going to replace the MUC protocol
in the future (see [mod_muc](#mod_muc)).

To learn more about how to use that feature, you can refer to our
tutorial: [Getting started with MIX](../../tutorials/mix-010.md)

The module depends on [mod_mam](#mod_mam).

__Available options:__

- **access\_create**: `AccessName`  
An access rule to control MIX channels creations. The default value is
`all`.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **host**  
Deprecated. Use `hosts` instead.

- **hosts**: `[Host, ...]`  
This option defines the Jabber IDs of the service. If the `hosts` option
is not specified, the only Jabber ID will be the hostname of the virtual
host with the prefix `"mix."`. The keyword `@HOST@` is replaced with the
real virtual host name.

- **name**: `Name`  
A name of the service in the Service Discovery. This will only be
displayed by special XMPP clients. The default value is `Channels`.

mod\_mix\_pam
-------------

This module implements [XEP-0405: Mediated Information eXchange (MIX):
Participant Server
Requirements](https://xmpp.org/extensions/xep-0405.html). The module is
needed if MIX compatible clients on your server are going to join MIX
channels (either on your server or on any remote servers).

!!! note

    [mod_mix](#mod_mix) is not required for this module to work, however, without
    `mod_mix_pam` the MIX functionality of your local XMPP clients will
    be impaired.

__Available options:__

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**: `true | false`  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

mod\_mqtt
---------

This module adds [support for the MQTT](../guide/mqtt/index.md)
protocol version `3.1.1` and `5.0`. Remember to configure `mod_mqtt` in
`modules` and `listen` sections.

__Available options:__

- **access\_publish**: `{TopicFilter: AccessName}`  
Access rules to restrict access to topics for publishers. By default
there are no restrictions.

- **access\_subscribe**: `{TopicFilter: AccessName}`  
Access rules to restrict access to topics for subscribers. By default
there are no restrictions.

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**: `true | false`  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **match\_retained\_limit**: `pos_integer() | infinity`  
The option limits the number of retained messages returned to a client
when it subscribes to some topic filter. The default value is `1000`.

- **max\_queue**: `Size`  
Maximum queue size for outgoing packets. The default value is `5000`.

- **max\_topic\_aliases**: `0..65535`  
The maximum number of aliases a client is able to associate with the
topics. The default value is `100`.

- **max\_topic\_depth**: `Depth`  
The maximum topic depth, i.e. the number of slashes (`/`) in the topic.
The default value is `8`.

- **queue\_type**: `ram | file`  
Same as top-level [queue_type](toplevel.md#queue_type) option, but applied to this module
only.

- **ram\_db\_type**: `mnesia`  
Same as top-level [default_ram_db](toplevel.md#default_ram_db) option, but applied to this module
only.

- **session\_expiry**: `timeout()`  
The option specifies how long to wait for an MQTT session resumption.
When `0` is set, the session gets destroyed when the underlying client
connection is closed. The default value is `5` minutes.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

mod\_mqtt\_bridge
-----------------

This module adds ability to synchronize local MQTT topics with data on
remote servers It can update topics on remote servers when local user
updates local topic, or can subscribe for changes on remote server, and
update local copy when remote data is updated. It is available since
ejabberd [23.01](../../archive/23.01/index.md).

__Available options:__

- **replication\_user**: `JID`  
Identifier of a user that will be assigned as owner of local changes.

- **servers**: `{ServerUrl: {Key: Value}}`  
Declaration of data to share for each ServerUrl. Server URLs can use
schemas: `mqtt`, `mqtts` (mqtt with tls), `mqtt5`, `mqtt5s` (both to
trigger v5 protocol), `ws`, `wss`, `ws5`, `wss5`. Keys must be:

    - **authentication**: `{AuthKey: AuthValue}`  
   List of authentication
    information, where AuthKey can be: `username` and `password` fields,
    or `certfile` pointing to client certificate. Certificate
    authentication can be used only with mqtts, mqtt5s, wss, wss5.

    - **publish**: `{LocalTopic: RemoteTopic}`  
   Either publish or subscribe
    must be set, or both.

    - **subscribe**: `{RemoteTopic: LocalTopic}`  
   Either publish or
    subscribe must be set, or both.

__**Example**:__

~~~ yaml
modules:
  mod_mqtt_bridge:
    replication_user: "mqtt@xmpp.server.com"
    servers:
      "mqtt://server.com":
        authentication:
          certfile: "/etc/ejabberd/mqtt_server.pem"
        publish:
          "localA": "remoteA" # local changes to 'localA' will be replicated on remote server as 'remoteA'
          "topicB": "topicB"
        subscribe:
          "remoteB": "localB" # changes to 'remoteB' on remote server will be stored as 'localB' on local server
~~~

mod\_muc
--------

This module provides support for [XEP-0045: Multi-User
Chat](https://xmpp.org/extensions/xep-0045.html). Users can discover
existing rooms, join or create them. Occupants of a room can chat in
public or have private chats.

The MUC service allows any Jabber ID to register a nickname, so nobody
else can use that nickname in any room in the MUC service. To register a
nickname, open the Service Discovery in your XMPP client and register in
the MUC service.

It is also possible to register a nickname in a room, so nobody else can
use that nickname in that room. If a nick is registered in the MUC
service, that nick cannot be registered in any room, and vice versa: a
nick that is registered in a room cannot be registered at the MUC
service.

This module supports clustering and load balancing. One module can be
started per cluster node. Rooms are distributed at creation time on all
available MUC module instances. The multi-user chat module is clustered
but the rooms themselves are not clustered nor fault-tolerant: if the
node managing a set of rooms goes down, the rooms disappear and they
will be recreated on an available node on first connection attempt.

__Available options:__

- **access**: `AccessName`  
You can specify who is allowed to use the Multi-User Chat service. By
default everyone is allowed to use it.

- **access\_admin**: `AccessName`  
This option specifies who is allowed to administrate the Multi-User Chat
service. The default value is `none`, which means that only the room
creator can administer their room. The administrators can send a normal
message to the service JID, and it will be shown in all active rooms as
a service message. The administrators can send a groupchat message to
the JID of an active room, and the message will be shown in the room as
a service message.

- **access\_create**: `AccessName`  
To configure who is allowed to create new rooms at the Multi-User Chat
service, this option can be used. The default value is `all`, which
means everyone is allowed to create rooms.

- **access\_mam**: `AccessName`  
To configure who is allowed to modify the `mam` room option. The default
value is `all`, which means everyone is allowed to modify that option.

- **access\_persistent**: `AccessName`  
To configure who is allowed to modify the `persistent` room option. The
default value is `all`, which means everyone is allowed to modify that
option.

- **access\_register**: `AccessName`  
<!-- md:version improved in [23.10](../../archive/23.10/index.md) -->
 This option specifies who
is allowed to register nickname within the Multi-User Chat service and
rooms. The default is `all` for backward compatibility, which means that
any user is allowed to register any free nick in the MUC service and in
the rooms.

- **cleanup\_affiliations\_on\_start**: `true | false`  
<!-- md:version added in [22.05](../../archive/22.05/index.md) -->
 Remove affiliations for
non-existing local users on startup. The default value is `false`.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **default\_room\_options**: `Options`  
Define the default room options. Note that the creator of a room can
modify the options of his room at any time using an XMPP client with MUC
capability. The `Options` are:

    - **allow\_change\_subj**: `true | false`  
   Allow occupants to change
    the subject. The default value is `true`.

    - **allow\_private\_messages\_from\_visitors**: *anyone | moderators |
    nobody* Visitors can send private messages to other occupants. The
    default value is `anyone` which means visitors can send private
    messages to any occupant.

    - **allow\_query\_users**: `true | false`  
   Occupants can send IQ
    queries to other occupants. The default value is `true`.

    - **allow\_subscription**: `true | false`  
   Allow users to subscribe to
    room events as described in
    [Multi-User Chat Subscriptions](../../developer/xmpp-clients-bots/extensions/muc-sub.md).
    The default value is `false`.

    - **allow\_user\_invites**: `true | false`  
   Allow occupants to send
    invitations. The default value is `false`.

    - **allow\_visitor\_nickchange**: `true | false`  
   Allow visitors to
    change nickname. The default value is `true`.

    - **allow\_visitor\_status**: `true | false`  
   Allow visitors to send
    status text in presence updates. If disallowed, the status text is
    stripped before broadcasting the presence update to all the room
    occupants. The default value is `true`.

    - **allow\_voice\_requests**: `true | false`  
   Allow visitors in a
    moderated room to request voice. The default value is `true`.

    - **allowpm**: `anyone | participants | moderators | none`  
   Who can
    send private messages. The default value is `anyone`.

    - **anonymous**: `true | false`  
   The room is anonymous: occupants don’t
    see the real JIDs of other occupants. Note that the room moderators
    can always see the real JIDs of the occupants. The default value is
    `true`.

    - **captcha\_protected**: `true | false`  
   When a user tries to join a
    room where they have no affiliation (not owner, admin or member),
    the room requires them to fill a CAPTCHA challenge (see section
    [CAPTCHA](basic.md#captcha) in order to accept their join in the
    room. The default value is `false`.

    - **description**: `Room Description`  
   Short description of the room.
    The default value is an empty string.

    - **enable\_hats**: `true | false`  
   Allow extended roles as defined in
    XEP-0317 Hats. The default value is `false`.

    - **lang**: `Language`  
   Preferred language for the discussions in the
    room. The language format should conform to RFC 5646. There is no
    value by default.

    - **logging**: `true | false`  
   The public messages are logged using
    [mod_muc_log](#mod_muc_log). The default value is `false`.

    - **mam**: `true | false`  
   Enable message archiving. Implies mod\_mam
    is enabled. The default value is `false`.

    - **max\_users**: `Number`  
   Maximum number of occupants in the room.
    The default value is `200`.

    - **members\_by\_default**: `true | false`  
   The occupants that enter
    the room are participants by default, so they have "voice". The
    default value is `true`.

    - **members\_only**: `true | false`  
   Only members of the room can
    enter. The default value is `false`.

    - **moderated**: `true | false`  
   Only occupants with "voice" can send
    public messages. The default value is `true`.

    - **password**: `Password`  
   Password of the room. Implies option
    `password_protected` set to `true`. There is no default value.

    - **password\_protected**: `true | false`  
   The password is required to
    enter the room. The default value is `false`.

    - **persistent**: `true | false`  
   The room persists even if the last
    participant leaves. The default value is `false`.

    - **presence\_broadcast**: `[Role]`  
   List of roles for which presence
    is broadcasted. The list can contain one or several of: `moderator`,
    `participant`, `visitor`. The default value is shown in the example
    below:

        **Example**:

        ~~~ yaml
        presence_broadcast:
          - moderator
          - participant
          - visitor
        ~~~

    - **public**: `true | false`  
   The room is public in the list of the MUC
    service, so it can be discovered. MUC admins and room participants
    will see private rooms in Service Discovery if their XMPP client
    supports this feature. The default value is `true`.

    - **public\_list**: `true | false`  
   The list of participants is public,
    without requiring to enter the room. The default value is `true`.

    - **pubsub**: `PubSub Node`  
   XMPP URI of associated Publish/Subscribe
    node. The default value is an empty string.

    - **title**: `Room Title`  
   A human-readable title of the room. There is
    no default value

    - **vcard**: `vCard`  
   A custom vCard for the room. See the equivalent
    mod\_muc option.The default value is an empty string.

    - **voice\_request\_min\_interval**: `Number`  
   Minimum interval between
    voice requests, in seconds. The default value is `1800`.

- **hibernation\_timeout**: `infinity | Seconds`  
Timeout before hibernating the room process, expressed in seconds. The
default value is `infinity`.

- **history\_size**: `Size`  
A small history of the current discussion is sent to users when they
enter the room. With this option you can define the number of history
messages to keep and send to users joining the room. The value is a
non-negative integer. Setting the value to `0` disables the history
feature and, as a result, nothing is kept in memory. The default value
is `20`. This value affects all rooms on the service. NOTE: modern XMPP
clients rely on Message Archives (XEP-0313), so feel free to disable the
history feature if you’re only using modern clients and have [mod_mam](#mod_mam)
module loaded.

- **host**  
Deprecated. Use `hosts` instead.

- **hosts**: `[Host, ...]`  
This option defines the Jabber IDs of the service. If the `hosts` option
is not specified, the only Jabber ID will be the hostname of the virtual
host with the prefix "conference.". The keyword `@HOST@` is replaced
with the real virtual host name.

- **max\_captcha\_whitelist**: `Number`  
<!-- md:version added in [21.01](../../archive/21.01/index.md) -->
 This option defines the
maximum number of characters that Captcha Whitelist can have when
configuring the room. The default value is `infinity`.

- **max\_password**: `Number`  
<!-- md:version added in [21.01](../../archive/21.01/index.md) -->
 This option defines the
maximum number of characters that Password can have when configuring the
room. The default value is `infinity`.

- **max\_room\_desc**: `Number`  
This option defines the maximum number of characters that Room
Description can have when configuring the room. The default value is
`infinity`.

- **max\_room\_id**: `Number`  
This option defines the maximum number of characters that Room ID can
have when creating a new room. The default value is `infinity`.

- **max\_room\_name**: `Number`  
This option defines the maximum number of characters that Room Name can
have when configuring the room. The default value is `infinity`.

- **max\_rooms\_discoitems**: `Number`  
When there are more rooms than this `Number`, only the non-empty ones
are returned in a Service Discovery query. The default value is `100`.

- **max\_user\_conferences**: `Number`  
This option defines the maximum number of rooms that any given user can
join. The default value is `100`. This option is used to prevent
possible abuses. Note that this is a soft limit: some users can
sometimes join more conferences in cluster configurations.

- **max\_users**: `Number`  
This option defines at the service level, the maximum number of users
allowed per room. It can be lowered in each room configuration but
cannot be increased in individual room configuration. The default value
is `200`.

- **max\_users\_admin\_threshold**: `Number`  
This option defines the number of service admins or room owners allowed
to enter the room when the maximum number of allowed occupants was
reached. The default limit is `5`.

- **max\_users\_presence**: `Number`  
This option defines after how many users in the room, it is considered
overcrowded. When a MUC room is considered overcrowded, presence
broadcasts are limited to reduce load, traffic and excessive presence
"storm" received by participants. The default value is `1000`.

- **min\_message\_interval**: `Number`  
This option defines the minimum interval between two messages send by an
occupant in seconds. This option is global and valid for all rooms. A
decimal value can be used. When this option is not defined, message rate
is not limited. This feature can be used to protect a MUC service from
occupant abuses and limit number of messages that will be broadcasted by
the service. A good value for this minimum message interval is `0.4`
second. If an occupant tries to send messages faster, an error is send
back explaining that the message has been discarded and describing the
reason why the message is not acceptable.

- **min\_presence\_interval**: `Number`  
This option defines the minimum of time between presence changes coming
from a given occupant in seconds. This option is global and valid for
all rooms. A decimal value can be used. When this option is not defined,
no restriction is applied. This option can be used to protect a MUC
service for occupants abuses. If an occupant tries to change its
presence more often than the specified interval, the presence is cached
by ejabberd and only the last presence is broadcasted to all occupants
in the room after expiration of the interval delay. Intermediate
presence packets are silently discarded. A good value for this option is
`4` seconds.

- **name**: `string()`  
The value of the service name. This name is only visible in some clients
that support [XEP-0030: Service
Discovery](https://xmpp.org/extensions/xep-0030.html). The default is
`Chatrooms`.

- **preload\_rooms**: `true | false`  
Whether to load all persistent rooms in memory on startup. If disabled,
the room is only loaded on first participant join. The default is
`true`. It makes sense to disable room preloading when the number of
rooms is high: this will improve server startup time and memory
consumption.

- **queue\_type**: `ram | file`  
Same as top-level [queue_type](toplevel.md#queue_type) option, but applied to this module
only.

- **ram\_db\_type**: `mnesia | sql`  
Same as top-level [default_ram_db](toplevel.md#default_ram_db) option, but applied to this module
only.

- **regexp\_room\_id**: `string()`  
This option defines the regular expression that a Room ID must satisfy
to allow the room creation. The default value is the empty string.

- **room\_shaper**: `none | ShaperName`  
This option defines shaper for the MUC rooms. The default value is
`none`.

- **user\_message\_shaper**: `none | ShaperName`  
This option defines shaper for the users messages. The default value is
`none`.

- **user\_presence\_shaper**: `none | ShaperName`  
This option defines shaper for the users presences. The default value is
`none`.

- **vcard**: `vCard`  
A custom vCard of the service that will be displayed by some XMPP
clients in Service Discovery. The value of `vCard` is a YAML map
constructed from an XML representation of vCard. Since the
representation has no attributes, the mapping is straightforward.

    **Example**:

    ~~~ yaml
    # This XML representation of vCard:
    #   <vCard xmlns='vcard-temp'>
    #     <FN>Conferences</FN>
    #     <ADR>
    #       <WORK/>
    #       <STREET>Elm Street</STREET>
    #     </ADR>
    #   </vCard>
    #
    # is translated to:
    vcard:
      fn: Conferences
      adr:
        -
          work: true
          street: Elm Street
    ~~~

mod\_muc\_admin
---------------

This module provides commands to administer local MUC services and their
MUC rooms. It also provides simple WebAdmin pages to view the existing
rooms.

This module depends on [mod_muc](#mod_muc).

__Available options:__

- **subscribe\_room\_many\_max\_users**: `Number`  
<!-- md:version added in [22.05](../../archive/22.05/index.md) -->
 How many users can be
subscribed to a room at once using the [subscribe_room_many](../../developer/ejabberd-api/admin-api.md#subscribe_room_many) API. The
default value is `50`.

mod\_muc\_log
-------------

This module enables optional logging of Multi-User Chat (MUC) public
conversations to HTML. Once you enable this module, users can join a
room using a MUC capable XMPP client, and if they have enough
privileges, they can request the configuration form in which they can
set the option to enable room logging.

Features:

-   Room details are added on top of each page: room title, JID, author,
    subject and configuration.

-   The room JID in the generated HTML is a link to join the room (using
    XMPP URI).

-   Subject and room configuration changes are tracked and displayed.

-   Joins, leaves, nick changes, kicks, bans and `/me` are tracked and
    displayed, including the reason if available.

-   Generated HTML files are XHTML 1.0 Transitional and CSS compliant.

-   Timestamps are self-referencing links.

-   Links on top for quicker navigation: Previous day, Next day, Up.

-   CSS is used for style definition, and a custom CSS file can be used.

-   URLs on messages and subjects are converted to hyperlinks.

-   Timezone used on timestamps is shown on the log files.

-   A custom link can be added on top of each page.

The module depends on [mod_muc](#mod_muc).

__Available options:__

- **access\_log**: `AccessName`  
This option restricts which occupants are allowed to enable or disable
room logging. The default value is `muc_admin`. NOTE: for this default
setting you need to have an access rule for `muc_admin` in order to
take effect.

- **cssfile**: `Path | URL`  
With this option you can set whether the HTML files should have a custom
CSS file or if they need to use the embedded CSS. Allowed values are
either `Path` to local file or an `URL` to a remote file. By default a
predefined CSS will be embedded into the HTML page.

- **dirname**: `room_jid | room_name`  
Configure the name of the room directory. If set to `room_jid`, the
room directory name will be the full room JID. Otherwise, the room
directory name will be only the room name, not including the MUC service
name. The default value is `room_jid`.

- **dirtype**: `subdirs | plain`  
The type of the created directories can be specified with this option.
If set to `subdirs`, subdirectories are created for each year and month.
Otherwise, the names of the log files contain the full date, and there
are no subdirectories. The default value is `subdirs`.

- **file\_format**: `html | plaintext`  
Define the format of the log files: `html` stores in HTML format,
`plaintext` stores in plain text. The default value is `html`.

- **file\_permissions**: `{mode: Mode, group: Group}`  
Define the permissions that must be used when creating the log files:
the number of the mode, and the numeric id of the group that will own
the files. The default value is shown in the example below:

    **Example**:

    ~~~ yaml
    file_permissions:
      mode: 644
      group: 33
    ~~~

- **outdir**: `Path`  
This option sets the full path to the directory in which the HTML files
should be stored. Make sure the ejabberd daemon user has write access on
that directory. The default value is `www/muc`.

- **spam\_prevention**: `true | false`  
If set to `true`, a special attribute is added to links that prevent
their indexation by search engines. The default value is `true`, which
mean that `nofollow` attributes will be added to user submitted links.

- **timezone**: `local | universal`  
The time zone for the logs is configurable with this option. If set to
`local`, the local time, as reported to Erlang emulator by the operating
system, will be used. Otherwise, UTC time will be used. The default
value is `local`.

- **top\_link**: `{URL: Text}`  
With this option you can customize the link on the top right corner of
each log file. The default value is shown in the example below:

    **Example**:

    ~~~ yaml
    top_link:
      /: Home
    ~~~

- **url**: `URL`  
A top level `URL` where a client can access logs of a particular
conference. The conference name is appended to the URL if `dirname`
option is set to `room_name` or a conference JID is appended to the
`URL` otherwise. There is no default value.

mod\_muc\_occupantid
--------------------

<!-- md:version added in [23.10](../../archive/23.10/index.md) -->


This module implements [XEP-0421: Anonymous unique occupant identifiers
for MUCs](https://xmpp.org/extensions/xep-0421.html).

When the module is enabled, the feature is enabled in all semi-anonymous
rooms.

The module has no options.

mod\_muc\_rtbl
--------------

<!-- md:version added in [23.04](../../archive/23.04/index.md) -->


This module implement Real-time blocklists for MUC rooms.

It works by observing remote pubsub node conforming with specification
described in <https://xmppbl.org/>.

__Available options:__

- **rtbl\_node**: `PubsubNodeName`  
Name of pubsub node that should be used to track blocked users. The
default value is `muc_bans_sha256`.

- **rtbl\_server**: `Domain`  
Domain of xmpp server that serves block list. The default value is
`xmppbl.org`

mod\_multicast
--------------

This module implements a service for [XEP-0033: Extended Stanza
Addressing](https://xmpp.org/extensions/xep-0033.html).

__Available options:__

- **access**: `Access`  
The access rule to restrict who can send packets to the multicast
service. Default value: `all`.

- **host**  
Deprecated. Use `hosts` instead.

- **hosts**: `[Host, ...]`  
This option defines the Jabber IDs of the service. If the `hosts` option
is not specified, the only Jabber ID will be the hostname of the virtual
host with the prefix "multicast.". The keyword `@HOST@` is replaced with
the real virtual host name. The default value is `multicast.@HOST@`.

- **limits**: `Sender: Stanza: Number`  
Specify a list of custom limits which override the default ones defined
in XEP-0033. Limits are defined per sender type and stanza type, where:

    - `sender` can be: `local` or `remote`.

    - `stanza` can be: `message` or `presence`.

    - `number` can be a positive integer or `infinite`.

        **Example**:

        ~~~ yaml
        # Default values:
        local:
          message: 100
          presence: 100
        remote:
          message: 20
          presence: 20
        ~~~

- **name**  
Service name to provide in the Info query to the Service Discovery.
Default is `"Multicast"`.

- **vcard**  
vCard element to return when queried. Default value is `undefined`.

__**Example**:__

~~~ yaml
# Only admins can send packets to multicast service
access_rules:
  multicast:
    - allow: admin

# If you want to allow all your users:
access_rules:
  multicast:
    - allow

# This allows both admins and remote users to send packets,
# but does not allow local users
acl:
  allservers:
    server_glob: "*"
access_rules:
  multicast:
    - allow: admin
    - deny: local
    - allow: allservers

modules:
  mod_multicast:
     host: multicast.example.org
     access: multicast
     limits:
       local:
         message: 40
         presence: infinite
       remote:
         message: 150
~~~

mod\_offline
------------

This module implements [XEP-0160: Best Practices for Handling Offline
Messages](https://xmpp.org/extensions/xep-0160.html) and [XEP-0013:
Flexible Offline Message
Retrieval](https://xmpp.org/extensions/xep-0013.html). This means that
all messages sent to an offline user will be stored on the server until
that user comes online again. Thus it is very similar to how email
works. A user is considered offline if no session presence priority &gt;
0 are currently open.

The [delete_expired_messages](../../developer/ejabberd-api/admin-api.md#delete_expired_messages) API allows to delete expired messages,
and [delete_old_messages](../../developer/ejabberd-api/admin-api.md#delete_old_messages) API deletes older ones.

__Available options:__

- **access\_max\_user\_messages**: `AccessName`  
This option defines which access rule will be enforced to limit the
maximum number of offline messages that a user can have (quota). When a
user has too many offline messages, any new messages that they receive
are discarded, and a *&lt;resource-constraint/&gt;* error is returned to
the sender. The default value is `max_user_offline_messages`.

- **bounce\_groupchat**: `true | false`  
This option is use the disable an optimization that avoids bouncing
error messages when groupchat messages could not be stored as offline.
It will reduce chat room load, without any drawback in standard use
cases. You may change default value only if you have a custom module
which uses offline hook after `mod_offline`. This option can be useful
for both standard MUC and MucSub, but the bounce is much more likely to
happen in the context of MucSub, so it is even more important to have it
on large MucSub services. The default value is `false`, meaning the
optimization is enabled.

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **store\_empty\_body**: `true | false | unless_chat_state`  
Whether or not to store messages that lack a *&lt;body/&gt;* element.
The default value is `unless_chat_state`, which tells ejabberd to
store messages even if they lack the *&lt;body/&gt;* element, unless
they only contain a chat state notification (as defined in [XEP-0085:
Chat State Notifications](https://xmpp.org/extensions/xep-0085.html).

- **store\_groupchat**: `true | false`  
Whether or not to store groupchat messages. The default value is
`false`.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

- **use\_mam\_for\_storage**: `true | false`  
This is an experimental option. By enabling the option, this module uses
the `archive` table from [mod_mam](#mod_mam) instead of its own spool table to
retrieve the messages received when the user was offline. This allows
client developers to slowly drop XEP-0160 and rely on XEP-0313 instead.
It also further reduces the storage required when you enable MucSub.
Enabling this option has a known drawback for the moment: most of
flexible message retrieval queries don’t work (those that allow
retrieval/deletion of messages by id), but this specification is not
widely used. The default value is `false` to keep former behaviour as
default.

__Examples:__

This example allows power users to have as much as 5000 offline
messages, administrators up to 2000, and all the other users up to 100:

~~~ yaml
acl:
  admin:
    user:
      - admin1@localhost
      - admin2@example.org
  poweruser:
    user:
      - bob@example.org
      - jane@example.org

shaper_rules:
  max_user_offline_messages:
    - 5000: poweruser
    - 2000: admin
    - 100

modules:
  ...
  mod_offline:
    access_max_user_messages: max_user_offline_messages
  ...
~~~

mod\_ping
---------

This module implements support for [XEP-0199: XMPP
Ping](https://xmpp.org/extensions/xep-0199.html) and periodic
keepalives. When this module is enabled ejabberd responds correctly to
ping requests, as defined by the protocol.

__Available options:__

- **ping\_ack\_timeout**: `timeout()`  
How long to wait before deeming that a client has not answered a given
server ping request. NOTE: when [mod_stream_mgmt](#mod_stream_mgmt) is loaded and stream
management is enabled by a client, this value is ignored, and the
`ack_timeout` applies instead. The default value is `undefined`.

- **ping\_interval**: `timeout()`  
How often to send pings to connected clients, if option `send_pings` is
set to `true`. If a client connection does not send or receive any
stanza within this interval, a ping request is sent to the client. The
default value is `1` minute.

- **send\_pings**: `true | false`  
If this option is set to `true`, the server sends pings to connected
clients that are not active in a given interval defined in
`ping_interval` option. This is useful to keep client connections alive
or checking availability. The default value is `false`.

- **timeout\_action**: `none | kill`  
What to do when a client does not answer to a server ping request in
less than period defined in `ping_ack_timeout` option: `kill` means
destroying the underlying connection, `none` means to do nothing. NOTE:
when [mod_stream_mgmt](#mod_stream_mgmt) is loaded and stream management is enabled by a
client, killing the client connection doesn’t mean killing the client
session - the session will be kept alive in order to give the client a
chance to resume it. The default value is `none`.

__**Example**:__

~~~ yaml
modules:
  mod_ping:
    send_pings: true
    ping_interval: 4 min
    timeout_action: kill
~~~

mod\_pres\_counter
------------------

This module detects flood/spam in presence subscriptions traffic. If a
user sends or receives more of those stanzas in a given time interval,
the exceeding stanzas are silently dropped, and a warning is logged.

__Available options:__

- **count**: `Number`  
The number of subscription presence stanzas (subscribe, unsubscribe,
subscribed, unsubscribed) allowed for any direction (input or output)
per time defined in `interval` option. Please note that two users
subscribing to each other usually generate 4 stanzas, so the recommended
value is `4` or more. The default value is `5`.

- **interval**: `timeout()`  
The time interval. The default value is `1` minute.

__**Example**:__

~~~ yaml
modules:
  mod_pres_counter:
    count: 5
    interval: 30 secs
~~~

mod\_privacy
------------

This module implements [XEP-0016: Privacy
Lists](https://xmpp.org/extensions/xep-0016.html).

!!! note

    Nowadays modern XMPP clients rely on [XEP-0191: Blocking
    Command](https://xmpp.org/extensions/xep-0191.html) which is
    implemented by [mod_blocking](#mod_blocking). However, you still need
    `mod_privacy` loaded in order for `mod_blocking` to work.

__Available options:__

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**: `true | false`  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

mod\_private
------------

This module adds support for [XEP-0049: Private XML
Storage](https://xmpp.org/extensions/xep-0049.html).

Using this method, XMPP entities can store private data on the server,
retrieve it whenever necessary and share it between multiple connected
clients of the same user. The data stored might be anything, as long as
it is a valid XML. One typical usage is storing a bookmark of all user’s
conferences ([XEP-0048:
Bookmarks](https://xmpp.org/extensions/xep-0048.html)).

It also implements the bookmark conversion described in [XEP-0402: PEP
Native Bookmarks](https://xmpp.org/extensions/xep-0402.html), see
[bookmarks_to_pep](../../developer/ejabberd-api/admin-api.md#bookmarks_to_pep) API.

__Available options:__

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**: `true | false`  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

mod\_privilege
--------------

<!-- md:version improved in [24.10](../../archive/24.10/index.md) -->


This module is an implementation of [XEP-0356: Privileged
Entity](https://xmpp.org/extensions/xep-0356.html). This extension
allows components to have privileged access to other entity data (send
messages on behalf of the server or on behalf of a user, get/set user
roster, access presence information, etc.). This may be used to write
powerful external components, for example implementing an external
[PEP](https://xmpp.org/extensions/xep-0163.html) or
[MAM](https://xmpp.org/extensions/xep-0313.html) service.

By default a component does not have any privileged access. It is worth
noting that the permissions grant access to the component to a specific
data type for all users of the virtual host on which `mod_privilege` is
loaded.

Make sure you have a listener configured to connect your component.
Check the section about listening ports for more information.

!!! warning

    Security issue: Privileged access gives components access to sensitive
    data, so permission should be granted carefully, only if you trust a
    component.

!!! note

    This module is complementary to [mod_delegation](#mod_delegation), but can also be
    used separately.

__Available options:__

- **iq**: `{Namespace: Options}`  
This option defines namespaces and their IQ permissions. By default no
permissions are given. The `Options` are:

    - **both**: `AccessName`  
   Allows sending IQ stanzas of type `get` and
    `set`. The default value is `none`.

    - **get**: `AccessName`  
   Allows sending IQ stanzas of type `get`. The
    default value is `none`.

    - **set**: `AccessName`  
   Allows sending IQ stanzas of type `set`. The
    default value is `none`.

- **message**: `Options`  
This option defines permissions for messages. By default no permissions
are given. The `Options` are:

    - **outgoing**: `AccessName`  
   The option defines an access rule for
    sending outgoing messages by the component. The default value is
    `none`.

- **presence**: `Options`  
This option defines permissions for presences. By default no permissions
are given. The `Options` are:

    - **managed\_entity**: `AccessName`  
   An access rule that gives
    permissions to the component to receive server presences. The
    default value is `none`.

    - **roster**: `AccessName`  
   An access rule that gives permissions to
    the component to receive the presence of both the users and the
    contacts in their roster. The default value is `none`.

- **roster**: `Options`  
This option defines roster permissions. By default no permissions are
given. The `Options` are:

    - **both**: `AccessName`  
   Sets read/write access to a user’s roster.
    The default value is `none`.

    - **get**: `AccessName`  
   Sets read access to a user’s roster. The
    default value is `none`.

    - **set**: `AccessName`  
   Sets write access to a user’s roster. The
    default value is `none`.

__**Example**:__

~~~ yaml
modules:
  mod_privilege:
    iq:
      http://jabber.org/protocol/pubsub:
        get: all
    roster:
      get: all
    presence:
      managed_entity: all
    message:
      outgoing: all
~~~

mod\_proxy65
------------

This module implements [XEP-0065: SOCKS5
Bytestreams](https://xmpp.org/extensions/xep-0065.html). It allows
ejabberd to act as a file transfer proxy between two XMPP clients.

__Available options:__

- **access**: `AccessName`  
Defines an access rule for file transfer initiators. The default value
is `all`. You may want to restrict access to the users of your server
only, in order to avoid abusing your proxy by the users of remote
servers.

- **auth\_type**: `anonymous | plain`  
SOCKS5 authentication type. The default value is `anonymous`. If set to
`plain`, ejabberd will use authentication backend as it would for SASL
PLAIN.

- **host**  
Deprecated. Use `hosts` instead.

- **hostname**: `Host`  
Defines a hostname offered by the proxy when establishing a session with
clients. This is useful when you run the proxy behind a NAT. The keyword
`@HOST@` is replaced with the virtual host name. The default is to use
the value of `ip` option. Examples: `proxy.mydomain.org`,
`200.150.100.50`.

- **hosts**: `[Host, ...]`  
This option defines the Jabber IDs of the service. If the `hosts` option
is not specified, the only Jabber ID will be the hostname of the virtual
host with the prefix "proxy.". The keyword `@HOST@` is replaced with the
real virtual host name.

- **ip**: `IPAddress`  
This option specifies which network interface to listen for. The default
value is an IP address of the service’s DNS name, or, if fails,
`127.0.0.1`.

- **max\_connections**: `pos_integer() | infinity`  
Maximum number of active connections per file transfer initiator. The
default value is `infinity`.

- **name**: `Name`  
The value of the service name. This name is only visible in some clients
that support [XEP-0030: Service
Discovery](https://xmpp.org/extensions/xep-0030.html). The default is
"SOCKS5 Bytestreams".

- **port**: `1..65535`  
A port number to listen for incoming connections. The default value is
`7777`.

- **ram\_db\_type**: `mnesia | redis | sql`  
Same as top-level [default_ram_db](toplevel.md#default_ram_db) option, but applied to this module
only.

- **recbuf**: `Size`  
A size of the buffer for incoming packets. If you define a shaper, set
the value of this option to the size of the shaper in order to avoid
traffic spikes in file transfers. The default value is `65536` bytes.

- **shaper**: `Shaper`  
This option defines a shaper for the file transfer peers. A shaper with
the maximum bandwidth will be selected. The default is `none`, i.e. no
shaper.

- **sndbuf**: `Size`  
A size of the buffer for outgoing packets. If you define a shaper, set
the value of this option to the size of the shaper in order to avoid
traffic spikes in file transfers. The default value is `65536` bytes.

- **vcard**: `vCard`  
A custom vCard of the service that will be displayed by some XMPP
clients in Service Discovery. The value of `vCard` is a YAML map
constructed from an XML representation of vCard. Since the
representation has no attributes, the mapping is straightforward.

__**Example**:__

~~~ yaml
acl:
  admin:
    user: admin@example.org
  proxy_users:
    server: example.org

access_rules:
  proxy65_access:
    allow: proxy_users

shaper_rules:
  proxy65_shaper:
    none: admin
  proxyrate: proxy_users

shaper:
  proxyrate: 10240

modules:
  mod_proxy65:
    host: proxy1.example.org
    name: "File Transfer Proxy"
    ip: 200.150.100.1
    port: 7778
    max_connections: 5
    access: proxy65_access
    shaper: proxy65_shaper
    recbuf: 10240
    sndbuf: 10240
~~~

mod\_pubsub
-----------

This module offers a service for [XEP-0060:
Publish-Subscribe](https://xmpp.org/extensions/xep-0060.html). The
functionality in `mod_pubsub` can be extended using plugins. The plugin
that implements PEP ([XEP-0163: Personal Eventing via
Pubsub](https://xmpp.org/extensions/xep-0163.html)) is enabled in the
default ejabberd configuration file, and it requires [mod_caps](#mod_caps).

__Available options:__

- **access\_createnode**: `AccessName`  
This option restricts which users are allowed to create pubsub nodes
using `acl` and `access`. By default any account in the local ejabberd
server is allowed to create pubsub nodes. The default value is: `all`.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **default\_node\_config**: `List of Key:Value`  
To override default node configuration, regardless of node plugin. Value
is a list of key-value definition. Node configuration still uses default
configuration defined by node plugin, and overrides any items by value
defined in this configurable list.

- **force\_node\_config**: `List of Node and the list of its Key:Value`  
Define the configuration for given nodes. The default value is: `[]`.

    **Example**:

    ~~~ yaml
    force_node_config:
      ## Avoid buggy clients to make their bookmarks public
      storage:bookmarks:
        access_model: whitelist
    ~~~

- **host**  
Deprecated. Use `hosts` instead.

- **hosts**: `[Host, ...]`  
This option defines the Jabber IDs of the service. If the `hosts` option
is not specified, the only Jabber ID will be the hostname of the virtual
host with the prefix "pubsub.". The keyword `@HOST@` is replaced with
the real virtual host name.

- **ignore\_pep\_from\_offline**: `false | true`  
To specify whether or not we should get last published PEP items from
users in our roster which are offline when we connect. Value is `true`
or `false`. If not defined, pubsub assumes true so we only get last
items of online contacts.

- **last\_item\_cache**: `false | true`  
To specify whether or not pubsub should cache last items. Value is
`true` or `false`. If not defined, pubsub does not cache last items. On
systems with not so many nodes, caching last items speeds up pubsub and
allows you to raise the user connection rate. The cost is memory usage,
as every item is stored in memory.

- **max\_item\_expire\_node**: `timeout() | infinity`  
<!-- md:version added in [21.12](../../archive/21.12/index.md) -->
 Specify the maximum item epiry
time. Default value is: `infinity`.

- **max\_items\_node**: `non_neg_integer() | infinity`  
Define the maximum number of items that can be stored in a node. Default
value is: `1000`.

- **max\_nodes\_discoitems**: `pos_integer() | infinity`  
The maximum number of nodes to return in a discoitem response. The
default value is: `100`.

- **max\_subscriptions\_node**: `MaxSubs`  
Define the maximum number of subscriptions managed by a node. Default
value is no limitation: `undefined`.

- **name**: `Name`  
The value of the service name. This name is only visible in some clients
that support [XEP-0030: Service
Discovery](https://xmpp.org/extensions/xep-0030.html). The default is
`vCard User Search`.

- **nodetree**: `Nodetree`  
To specify which nodetree to use. If not defined, the default pubsub
nodetree is used: `tree`. Only one nodetree can be used per host, and is
shared by all node plugins.

    - `tree` nodetree store node configuration and relations on the
    database. `flat` nodes are stored without any relationship, and
    `hometree` nodes can have child nodes.

    - `virtual` nodetree does not store nodes on database. This saves
    resources on systems with tons of nodes. If using the `virtual`
    nodetree, you can only enable those node plugins: `[flat, pep]` or
    `[flat]`; any other plugins configuration will not work. Also, all
    nodes will have the default configuration, and this can not be
    changed. Using `virtual` nodetree requires to start from a clean
    database, it will not work if you used the default `tree` nodetree
    before.

- **pep\_mapping**: `List of Key:Value`  
In this option you can provide a list of key-value to choose defined
node plugins on given PEP namespace. The following example will use
`node_tune` instead of `node_pep` for every PEP node with the tune
namespace:

    **Example**:

    ~~~ yaml
    modules:
      ...
      mod_pubsub:
        pep_mapping:
          http://jabber.org/protocol/tune: tune
      ...
    ~~~

- **plugins**: `[Plugin, ...]`  
To specify which pubsub node plugins to use. The first one in the list
is used by default. If this option is not defined, the default plugins
list is: `[flat]`. PubSub clients can define which plugin to use when
creating a node: add *type='plugin-name`' attribute to the `create*
stanza element.

    - `flat` plugin handles the default behaviour and follows standard
    XEP-0060 implementation.

    - `pep` plugin adds extension to handle Personal Eventing Protocol
    (XEP-0163) to the PubSub engine. When enabled, PEP is handled
    automatically.

- **vcard**: `vCard`  
A custom vCard of the server that will be displayed by some XMPP clients
in Service Discovery. The value of `vCard` is a YAML map constructed
from an XML representation of vCard. Since the representation has no
attributes, the mapping is straightforward.

    **Example**:

    ~~~ yaml
    # This XML representation of vCard:
    #   <vCard xmlns='vcard-temp'>
    #     <FN>Conferences</FN>
    #     <ADR>
    #       <WORK/>
    #       <STREET>Elm Street</STREET>
    #     </ADR>
    #   </vCard>
    #
    # is translated to:
    vcard:
      fn: Conferences
      adr:
        -
          work: true
          street: Elm Street
    ~~~

__Examples:__

Example of configuration that uses flat nodes as default, and allows use
of flat, hometree and pep nodes:

~~~ yaml
modules:
  mod_pubsub:
    access_createnode: pubsub_createnode
    max_subscriptions_node: 100
    default_node_config:
      notification_type: normal
      notify_retract: false
      max_items: 4
    plugins:
      - flat
      - pep
~~~

Using relational database requires using mod\_pubsub with db\_type
`sql`. Only flat, hometree and pep plugins supports SQL. The following
example shows previous configuration with SQL usage:

~~~ yaml
modules:
  mod_pubsub:
    db_type: sql
    access_createnode: pubsub_createnode
    ignore_pep_from_offline: true
    last_item_cache: false
    plugins:
      - flat
      - pep
~~~

mod\_push
---------

This module implements the XMPP server’s part of the push notification
solution specified in [XEP-0357: Push
Notifications](https://xmpp.org/extensions/xep-0357.html). It does not
generate, for example, APNS or FCM notifications directly. Instead, it’s
designed to work with so-called "app servers" operated by third-party
vendors of mobile apps. Those app servers will usually trigger
notification delivery to the user’s mobile device using
platform-dependent backend services such as FCM or APNS.

__Available options:__

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**: `true | false`  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **include\_body**: `true | false | Text`  
If this option is set to `true`, the message text is included with push
notifications generated for incoming messages with a body. The option
can instead be set to a static `Text`, in which case the specified text
will be included in place of the actual message body. This can be useful
to signal the app server whether the notification was triggered by a
message with body (as opposed to other types of traffic) without leaking
actual message contents. The default value is "New message".

- **include\_sender**: `true | false`  
If this option is set to `true`, the sender’s JID is included with push
notifications generated for incoming messages with a body. The default
value is `false`.

- **notify\_on**: `messages | all`  
<!-- md:version added in [23.10](../../archive/23.10/index.md) -->
 If this option is set to
`messages`, notifications are generated only for actual chat messages
with a body text (or some encrypted payload). If it’s set to `all`, any
kind of XMPP stanza will trigger a notification. If unsure, it’s
strongly recommended to stick to `all`, which is the default value.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

mod\_push\_keepalive
--------------------

This module tries to keep the stream management session (see
[mod_stream_mgmt](#mod_stream_mgmt)) of a disconnected mobile client alive if the client
enabled push notifications for that session. However, the normal session
resumption timeout is restored once a push notification is issued, so
the session will be closed if the client doesn’t respond to push
notifications.

The module depends on [mod_push](#mod_push).

__Available options:__

- **resume\_timeout**: `timeout()`  
This option specifies the period of time until the session of a
disconnected push client times out. This timeout is only in effect as
long as no push notification is issued. Once that happened, the
resumption timeout configured for [mod_stream_mgmt](#mod_stream_mgmt) is restored. The
default value is `72` hours.

- **wake\_on\_start**: `true | false`  
If this option is set to `true`, notifications are generated for **all**
registered push clients during server startup. This option should not be
enabled on servers with many push clients as it can generate significant
load on the involved push services and the server itself. The default
value is `false`.

- **wake\_on\_timeout**: `true | false`  
If this option is set to `true`, a notification is generated shortly
before the session would time out as per the `resume_timeout` option.
The default value is `true`.

mod\_register
-------------

This module adds support for [XEP-0077: In-Band
Registration](https://xmpp.org/extensions/xep-0077.html). This protocol
enables end users to use an XMPP client to:

-   Register a new account on the server.

-   Change the password from an existing account on the server.

-   Delete an existing account on the server.

This module reads also the top-level [registration_timeout](toplevel.md#registration_timeout) option
defined globally for the server, so please check that option
documentation too.

__Available options:__

- **access**: `AccessName`  
Specify rules to restrict what usernames can be registered. If a rule
returns `deny` on the requested username, registration of that user name
is denied. There are no restrictions by default. If `AccessName` is
`none`, then registering new accounts using In-Band Registration is
disabled and the corresponding stream feature is not announced to
clients.

- **access\_from**: `AccessName`  
By default, `ejabberd` doesn’t allow the client to register new accounts
from s2s or existing c2s sessions. You can change it by defining access
rule in this option. Use with care: allowing registration from s2s leads
to uncontrolled massive accounts creation by rogue users.

- **access\_remove**: `AccessName`  
Specify rules to restrict access for user unregistration. By default any
user is able to unregister their account.

- **allow\_modules**: `all | [Module, ...]`  
<!-- md:version added in [21.12](../../archive/21.12/index.md) -->
 List of modules that can
register accounts, or `all`. The default value is `all`, which is
equivalent to something like `\[mod\_register, mod_register_web]`.

- **captcha\_protected**: `true | false`  
Protect registrations with [CAPTCHA](basic.md#captcha). The default is
`false`.

- **ip\_access**: `AccessName`  
Define rules to allow or deny account registration depending on the IP
address of the XMPP client. The `AccessName` should be of type `ip`. The
default value is `all`.

- **password\_strength**: `Entropy`  
This option sets the minimum [Shannon
entropy](https://en.wikipedia.org/wiki/Entropy_(information_theory)) for
passwords. The value `Entropy` is a number of bits of entropy. The
recommended minimum is 32 bits. The default is `0`, i.e. no checks are
performed.

- **redirect\_url**: `URL`  
This option enables registration redirection as described in [XEP-0077:
In-Band Registration:
Redirection](https://xmpp.org/extensions/xep-0077.html#redirect).

- **registration\_watchers**: `[JID, ...]`  
This option defines a list of JIDs which will be notified each time a
new account is registered.

- **welcome\_message**: `{subject: Subject, body: Body}`  
Set a welcome message that is sent to each newly registered account. The
message will have subject `Subject` and text `Body`.

    **Example**:

    ~~~ yaml
    modules:
      mod_register:
        welcome_message:
          subject: "Welcome!"
          body: |-
            Hi!
            Welcome to this XMPP server
    ~~~

mod\_register\_web
------------------

This module provides a web page where users can:

-   Register a new account on the server.

-   Change the password from an existing account on the server.

-   Unregister an existing account on the server.

This module supports [CAPTCHA](basic.md#captcha) to register a new
account. To enable this feature, configure the top-level [captcha_cmd](toplevel.md#captcha_cmd)
and top-level [captcha_url](toplevel.md#captcha_url) options.

As an example usage, the users of the host `localhost` can visit the
page: `https://localhost:5280/register/` It is important to include the
last / character in the URL, otherwise the subpages URL will be
incorrect.

This module is enabled in `listen` → `ejabberd_http` →
[request_handlers](listen-options.md#request_handlers), no need to
enable in `modules`. The module depends on [mod_register](#mod_register) where all
the configuration is performed.

The module has no options.

__**Example**:__

~~~ yaml
listen:
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      /register: mod_register_web

modules:
  mod_register: {}
~~~

mod\_roster
-----------

This module implements roster management as defined in [RFC6121 Section
2](https://tools.ietf.org/html/rfc6121#section-2). The module also adds
support for [XEP-0237: Roster
Versioning](https://xmpp.org/extensions/xep-0237.html).

__Available options:__

- **access**: `AccessName`  
This option can be configured to specify rules to restrict roster
management. If the rule returns `deny` on the requested user name, that
user cannot modify their personal roster, i.e. they cannot
add/remove/modify contacts or send presence subscriptions. The default
value is `all`, i.e. no restrictions.

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**: `true | false`  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **store\_current\_id**: `true | false`  
If this option is set to `true`, the current roster version number is
stored on the database. If set to `false`, the roster version number is
calculated on the fly each time. Enabling this option reduces the load
for both ejabberd and the database. This option does not affect the
client in any way. This option is only useful if option `versioning` is
set to `true`. The default value is `false`. IMPORTANT: if you use
[mod_shared_roster](#mod_shared_roster) or [mod_shared_roster_ldap](#mod_shared_roster_ldap), you must set the
value of the option to `false`.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

- **versioning**: `true | false`  
Enables/disables Roster Versioning. The default value is `false`.

__**Example**:__

~~~ yaml
modules:
  mod_roster:
    versioning: true
    store_current_id: false
~~~

mod\_s2s\_bidi
--------------

<!-- md:version added in [24.10](../../archive/24.10/index.md) -->


The module adds support for [XEP-0288: Bidirectional Server-to-Server
Connections](https://xmpp.org/extensions/xep-0288.html) that allows
using single s2s connection to communicate in both directions.

The module has no options.

__**Example**:__

~~~ yaml
modules:
  mod_s2s_bidi: {}
~~~

mod\_s2s\_dialback
------------------

The module adds support for [XEP-0220: Server
Dialback](https://xmpp.org/extensions/xep-0220.html) to provide server
identity verification based on DNS.

!!! warning

    DNS-based verification is vulnerable to [DNS cache
    poisoning](https://en.wikipedia.org/wiki/DNS_spoofing), so modern
    servers rely on verification based on PKIX certificates. Thus this
    module is only recommended for backward compatibility with servers
    running outdated software or non-TLS servers, or those with invalid
    certificates (as long as you accept the risks, e.g. you assume that
    the remote server has an invalid certificate due to poor
    administration and not because it’s compromised).

__Available options:__

- **access**: `AccessName`  
An access rule that can be used to restrict dialback for some servers.
The default value is `all`.

__**Example**:__

~~~ yaml
modules:
  mod_s2s_dialback:
    access:
      allow:
        server: legacy.domain.tld
        server: invalid-cert.example.org
      deny: all
~~~

mod\_scram\_upgrade
-------------------

<!-- md:version added in [24.10](../../archive/24.10/index.md) -->


The module adds support for [XEP-0480: SASL Upgrade
Tasks](https://xmpp.org/extensions/xep-0480.html) that allows users to
upgrade passwords to more secure representation.

__Available options:__

- **offered\_upgrades**: `list(sha256, sha512)`  
List with upgrade types that should be offered

__**Example**:__

~~~ yaml
modules:
  mod_scram_upgrade:
    offered_upgrades:
      - sha256
      - sha512
~~~

mod\_service\_log
-----------------

This module forwards copies of all stanzas to remote XMPP servers or
components. Every stanza is encapsulated into &lt;forwarded/&gt; element
as described in [XEP-0297: Stanza
Forwarding](https://xmpp.org/extensions/xep-0297.html).

__Available options:__

- **loggers**: `[Domain, ...]`  
A list of servers or connected components to which stanzas will be
forwarded.

__**Example**:__

~~~ yaml
modules:
  mod_service_log:
    loggers:
      - xmpp-server.tld
      - component.domain.tld
~~~

mod\_shared\_roster
-------------------

This module enables you to create shared roster groups: groups of
accounts that can see members from (other) groups in their rosters.

The big advantages of this feature are that end users do not need to
manually add all users to their rosters, and that they cannot
permanently delete users from the shared roster groups. A shared roster
group can have members from any XMPP server, but the presence will only
be available from and to members of the same virtual host where the
group is created. It still allows the users to have / add their own
contacts, as it does not replace the standard roster. Instead, the
shared roster contacts are merged to the relevant users at retrieval
time. The standard user rosters thus stay unmodified.

Shared roster groups can be edited via the Web Admin, and some API
commands called `srg_`, for example [srg_add](../../developer/ejabberd-api/admin-api.md#srg_add) API. Each group has a
unique name and those parameters:

-   Label: Used in the rosters where this group is displayed.

-   Description: of the group, which has no effect.

-   Members: A list of JIDs of group members, entered one per line in
    the Web Admin. The special member directive `@all@` represents all
    the registered users in the virtual host; which is only recommended
    for a small server with just a few hundred users. The special member
    directive `@online@` represents the online users in the virtual
    host. With those two directives, the actual list of members in those
    shared rosters is generated dynamically at retrieval time.

-   Displayed: A list of groups that will be in the rosters of this
    group’s members. A group of other vhost can be identified with
    `groupid@vhost`.

This module depends on [mod_roster](#mod_roster). If not enabled, roster queries
will return 503 errors.

__Available options:__

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**: `true | false`  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **db\_type**: `mnesia | sql`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

__Examples:__

Take the case of a computer club that wants all its members seeing each
other in their rosters. To achieve this, they need to create a shared
roster group similar to this one:

~~~ yaml
Name: club_members
Label: Club Members
Description: Members from the computer club
Members: member1@example.org, member2@example.org, member3@example.org
Displayed Groups: club_members
~~~

In another case we have a company which has three divisions: Management,
Marketing and Sales. All group members should see all other members in
their rosters. Additionally, all managers should have all marketing and
sales people in their roster. Simultaneously, all marketeers and the
whole sales team should see all managers. This scenario can be achieved
by creating shared roster groups as shown in the following lists:

~~~ yaml
First list:
Name: management
Label: Management
Description: Management
Members: manager1@example.org, manager2@example.org
Displayed: management, marketing, sales

Second list:
Name: marketing
Label: Marketing
Description: Marketing
Members: marketeer1@example.org, marketeer2@example.org, marketeer3@example.org
Displayed: management, marketing

Third list:
Name: sales
Label: Sales
Description: Sales
Members: salesman1@example.org, salesman2@example.org, salesman3@example.org
Displayed: management, sales
~~~

mod\_shared\_roster\_ldap
-------------------------

This module lets the server administrator automatically populate users'
rosters (contact lists) with entries based on users and groups defined
in an LDAP-based directory.

!!! note

    `mod\_shared_roster_ldap` depends on `mod_roster` being enabled.
    Roster queries will return `503` errors if `mod_roster` is not
    enabled.

The module accepts many configuration options. Some of them, if
unspecified, default to the values specified for the top level of
configuration. This lets you avoid specifying, for example, the bind
password in multiple places.

-   Filters: `ldap_rfilter`, `ldap_ufilter`, `ldap_gfilter`,
    `ldap_filter`. These options specify LDAP filters used to query for
    shared roster information. All of them are run against the
    ldap\_base.

-   Attributes: `ldap_groupattr`, `ldap_groupdesc`,
    `ldap_memberattr`, `ldap_userdesc`, `ldap_useruid`. These options
    specify the names of the attributes which hold interesting data in
    the entries returned by running filters specified with the filter
    options.

-   Control parameters: `ldap_auth_check`,
    `ldap\_group\_cache_validity`, `ldap_memberattr_format`,
    `ldap\_memberattr\_format\_re`, `ldap_user_cache_validity`. These
    parameters control the behaviour of the module.

-   Connection parameters: The module also accepts the connection
    parameters, all of which default to the top-level parameter of the
    same name, if unspecified. See
    [LDAP Connection](ldap.md#ldap-connection) section for more
    information about them.

Check also the [Configuration examples](ldap.md#ldap-examples) section
to get details about retrieving the roster, and configuration examples
including Flat DIT and Deep DIT.

__Available options:__

- **cache\_life\_time**  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **ldap\_auth\_check**: `true | false`  
Whether the module should check (via the ejabberd authentication
subsystem) for existence of each user in the shared LDAP roster. Set to
`false` if you want to disable the check. Default value is `true`.

- **ldap\_backups**  
Same as top-level [ldap_backups](toplevel.md#ldap_backups) option, but applied to this module
only.

- **ldap\_base**  
Same as top-level [ldap_base](toplevel.md#ldap_base) option, but applied to this module only.

- **ldap\_deref\_aliases**  
Same as top-level [ldap_deref_aliases](toplevel.md#ldap_deref_aliases) option, but applied to this
module only.

- **ldap\_encrypt**  
Same as top-level [ldap_encrypt](toplevel.md#ldap_encrypt) option, but applied to this module
only.

- **ldap\_filter**  
Additional filter which is AND-ed together with "User Filter" and "Group
Filter". For more information check the LDAP [Filters](ldap.md#filters)
section.

- **ldap\_gfilter**  
"Group Filter", used when retrieving human-readable name (a.k.a.
"Display Name") and the members of a group. See also the parameters
`ldap_groupattr`, `ldap_groupdesc` and `ldap_memberattr`. If
unspecified, defaults to the top-level parameter of the same name. If
that one also is unspecified, then the filter is constructed exactly
like "User Filter".

- **ldap\_groupattr**  
The name of the attribute that holds the group name, and that is used to
differentiate between them. Retrieved from results of the "Roster
Filter" and "Group Filter". Defaults to `cn`.

- **ldap\_groupdesc**  
The name of the attribute which holds the human-readable group name in
the objects you use to represent groups. Retrieved from results of the
"Group Filter". Defaults to whatever `ldap_groupattr` is set.

- **ldap\_memberattr**  
The name of the attribute which holds the IDs of the members of a group.
Retrieved from results of the "Group Filter". Defaults to `memberUid`.
The name of the attribute differs depending on the objectClass you use
for your group objects, for example: `posixGroup` → `memberUid`;
`groupOfNames` → `member`; `groupOfUniqueNames` → `uniqueMember`.

- **ldap\_memberattr\_format**  
A globbing format for extracting user ID from the value of the attribute
named by `ldap_memberattr`. Defaults to `%u`, which means that the
whole value is the member ID. If you change it to something different,
you may also need to specify the User and Group Filters manually; see
section Filters.

- **ldap\_memberattr\_format\_re**  
A regex for extracting user ID from the value of the attribute named by
`ldap_memberattr`. Check the LDAP
[Control Parameters](ldap.md#control-parameters) section.

- **ldap\_password**  
Same as top-level [ldap_password](toplevel.md#ldap_password) option, but applied to this module
only.

- **ldap\_port**  
Same as top-level [ldap_port](toplevel.md#ldap_port) option, but applied to this module only.

- **ldap\_rfilter**  
So called "Roster Filter". Used to find names of all "shared roster"
groups. See also the `ldap_groupattr` parameter. If unspecified,
defaults to the top-level parameter of the same name. You must specify
it in some place in the configuration, there is no default.

- **ldap\_rootdn**  
Same as top-level [ldap_rootdn](toplevel.md#ldap_rootdn) option, but applied to this module
only.

- **ldap\_servers**  
Same as top-level [ldap_servers](toplevel.md#ldap_servers) option, but applied to this module
only.

- **ldap\_tls\_cacertfile**  
Same as top-level [ldap_tls_cacertfile](toplevel.md#ldap_tls_cacertfile) option, but applied to this
module only.

- **ldap\_tls\_certfile**  
Same as top-level [ldap_tls_certfile](toplevel.md#ldap_tls_certfile) option, but applied to this
module only.

- **ldap\_tls\_depth**  
Same as top-level [ldap_tls_depth](toplevel.md#ldap_tls_depth) option, but applied to this module
only.

- **ldap\_tls\_verify**  
Same as top-level [ldap_tls_verify](toplevel.md#ldap_tls_verify) option, but applied to this module
only.

- **ldap\_ufilter**  
"User Filter", used for retrieving the human-readable name of roster
entries (usually full names of people in the roster). See also the
parameters `ldap_userdesc` and `ldap_useruid`. For more information
check the LDAP [Filters](ldap.md#filters) section.

- **ldap\_uids**  
Same as top-level [ldap_uids](toplevel.md#ldap_uids) option, but applied to this module only.

- **ldap\_userdesc**  
The name of the attribute which holds the human-readable user name.
Retrieved from results of the "User Filter". Defaults to `cn`.

- **ldap\_userjidattr**  
The name of the attribute which is used to map user id to XMPP jid. If
not specified (and that is default value of this option), user jid will
be created from user id and this module host.

- **ldap\_useruid**  
The name of the attribute which holds the ID of a roster item. Value of
this attribute in the roster item objects needs to match the ID
retrieved from the `ldap_memberattr` attribute of a group object.
Retrieved from results of the "User Filter". Defaults to `cn`.

- **use\_cache**  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

mod\_sic
--------

This module adds support for [XEP-0279: Server IP
Check](https://xmpp.org/extensions/xep-0279.html). This protocol enables
a client to discover its external IP address.

!!! warning

    The protocol extension is deferred and seems like there are no clients
    supporting it, so using this module is not recommended and,
    furthermore, the module might be removed in the future.

The module has no options.

mod\_sip
--------

This module adds SIP proxy/registrar support for the corresponding
virtual host.

!!! note

    It is not enough to just load this module. You should also configure
    listeners and DNS records properly. For details see the section about
    the [ejabberd_sip](listen.md#ejabberd_sip) listen module in the
    ejabberd Documentation.

__Available options:__

- **always\_record\_route**: `true | false`  
Always insert "Record-Route" header into SIP messages. With this
approach it is possible to bypass NATs/firewalls a bit more easily. The
default value is `true`.

- **flow\_timeout\_tcp**: `timeout()`  
The option sets a keep-alive timer for [SIP
outbound](https://tools.ietf.org/html/rfc5626) TCP connections. The
default value is `2` minutes.

- **flow\_timeout\_udp**: `timeout()`  
The options sets a keep-alive timer for [SIP
outbound](https://tools.ietf.org/html/rfc5626) UDP connections. The
default value is `29` seconds.

- **record\_route**: `URI`  
When the option `always_record_route` is set to `true` or when [SIP
outbound](https://tools.ietf.org/html/rfc5626) is utilized, ejabberd
inserts "Record-Route" header field with this `URI` into a SIP message.
The default is a SIP URI constructed from the virtual host on which the
module is loaded.

- **routes**: `[URI, ...]`  
You can set a list of SIP URIs of routes pointing to this SIP proxy
server. The default is a list containing a single SIP URI constructed
from the virtual host on which the module is loaded.

- **via**: `[URI, ...]`  
A list to construct "Via" headers for inserting them into outgoing SIP
messages. This is useful if you’re running your SIP proxy in a
non-standard network topology. Every `URI` element in the list must be
in the form of "scheme://host:port", where "transport" must be `tls`,
`tcp`, or `udp`, "host" must be a domain name or an IP address and
"port" must be an internet port number. Note that all parts of the `URI`
are mandatory (e.g. you cannot omit "port" or "scheme").

__**Example**:__

~~~ yaml
modules:
  mod_sip:
    always_record_route: false
    record_route: "sip:example.com;lr"
    routes:
      - "sip:example.com;lr"
      - "sip:sip.example.com;lr"
    flow_timeout_udp: 30 sec
    flow_timeout_tcp: 1 min
    via:
      - tls://sip-tls.example.com:5061
      - tcp://sip-tcp.example.com:5060
      - udp://sip-udp.example.com:5060
~~~

mod\_stats
----------

This module adds support for [XEP-0039: Statistics
Gathering](https://xmpp.org/extensions/xep-0039.html). This protocol
allows you to retrieve the following statistics from your ejabberd
server:

-   Total number of registered users on the current virtual host
    (users/total).

-   Total number of registered users on all virtual hosts
    (users/all-hosts/total).

-   Total number of online users on the current virtual host
    (users/online).

-   Total number of online users on all virtual hosts
    (users/all-hosts/online).

!!! note

    The protocol extension is deferred and seems like even a few clients
    that were supporting it are now abandoned. So using this module makes
    very little sense.

The module has no options.

mod\_stream\_mgmt
-----------------

This module adds support for [XEP-0198: Stream
Management](https://xmpp.org/extensions/xep-0198.html). This protocol
allows active management of an XML stream between two XMPP entities,
including features for stanza acknowledgments and stream resumption.

__Available options:__

- **ack\_timeout**: `timeout()`  
A time to wait for stanza acknowledgments. Setting it to `infinity`
effectively disables the timeout. The default value is `1` minute.

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only. The default value is `48 hours`.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **max\_ack\_queue**: `Size`  
This option specifies the maximum number of unacknowledged stanzas
queued for possible retransmission. When the limit is exceeded, the
client session is terminated. The allowed values are positive integers
and `infinity`. You should be careful when setting this value as it
should not be set too low, otherwise, you could kill sessions in a loop,
before they get the chance to finish proper session initiation. It
should definitely be set higher that the size of the offline queue (for
example at least 3 times the value of the max offline queue and never
lower than `1000`). The default value is `5000`.

- **max\_resume\_timeout**: `timeout()`  
A client may specify the period of time until a session times out if the
connection is lost. During this period of time, the client may resume
its session. This option limits the period of time a client is permitted
to request. It must be set to a timeout equal to or larger than the
default `resume_timeout`. By default, it is set to the same value as
the `resume_timeout` option.

- **queue\_type**: `ram | file`  
Same as top-level [queue_type](toplevel.md#queue_type) option, but applied to this module
only.

- **resend\_on\_timeout**: `true | false | if_offline`  
If this option is set to `true`, any message stanzas that weren’t
acknowledged by the client will be resent on session timeout. This
behavior might often be desired, but could have unexpected results under
certain circumstances. For example, a message that was sent to two
resources might get resent to one of them if the other one timed out.
Therefore, the default value for this option is `false`, which tells
ejabberd to generate an error message instead. As an alternative, the
option may be set to `if_offline`. In this case, unacknowledged
messages are resent only if no other resource is online when the session
times out. Otherwise, error messages are generated.

- **resume\_timeout**: `timeout()`  
This option configures the (default) period of time until a session
times out if the connection is lost. During this period of time, a
client may resume its session. Note that the client may request a
different timeout value, see the `max_resume_timeout` option. Setting
it to `0` effectively disables session resumption. The default value is
`5` minutes.

mod\_stun\_disco
----------------

<!-- md:version added in [20.04](../../archive/20.04/index.md) -->


This module allows XMPP clients to discover STUN/TURN services and to
obtain temporary credentials for using them as per [XEP-0215: External
Service Discovery](https://xmpp.org/extensions/xep-0215.html).

__Available options:__

- **access**: `AccessName`  
This option defines which access rule will be used to control who is
allowed to discover STUN/TURN services and to request temporary
credentials. The default value is `local`.

- **credentials\_lifetime**: `timeout()`  
The lifetime of temporary credentials offered to clients. If ejabberd’s
built-in TURN service is used, TURN relays allocated using temporary
credentials will be terminated shortly after the credentials expired.
The default value is `12 hours`. Note that restarting the ejabberd node
invalidates any temporary credentials offered before the restart unless
a `secret` is specified (see below).

- **offer\_local\_services**: `true | false`  
This option specifies whether local STUN/TURN services configured as
ejabberd listeners should be announced automatically. Note that this
will not include TLS-enabled services, which must be configured manually
using the `services` option (see below). For non-anonymous TURN
services, temporary credentials will be offered to the client. The
default value is `true`.

- **secret**: `Text`  
The secret used for generating temporary credentials. If this option
isn’t specified, a secret will be auto-generated. However, a secret must
be specified explicitly if non-anonymous TURN services running on other
ejabberd nodes and/or external TURN `services` are configured. Also note
that auto-generated secrets are lost when the node is restarted, which
invalidates any credentials offered before the restart. Therefore, it’s
recommended to explicitly specify a secret if clients cache retrieved
credentials (for later use) across service restarts.

- **services**: `[Service, ...]`  
The list of services offered to clients. This list can include STUN/TURN
services running on any ejabberd node and/or external services. However,
if any listed TURN service not running on the local ejabberd node
requires authentication, a `secret` must be specified explicitly, and
must be shared with that service. This will only work with ejabberd’s
built-in STUN/TURN server and with external servers that support the
same [REST API For Access To TURN
Services](https://tools.ietf.org/html/draft-uberti-behave-turn-rest-00).
Unless the `offer_local_services` is set to `false`, the explicitly
listed services will be offered in addition to those announced
automatically.

    - **host**: `Host`  
   The hostname or IP address the STUN/TURN service is
    listening on. For non-TLS services, it’s recommended to specify an
    IP address (to avoid additional DNS lookup latency on the client
    side). For TLS services, the hostname (or IP address) should match
    the certificate. Specifying the `host` option is mandatory.

    - **port**: `1..65535`  
   The port number the STUN/TURN service is
    listening on. The default port number is 3478 for non-TLS services
    and 5349 for TLS services.

    - **restricted**: `true | false`  
   This option determines whether
    temporary credentials for accessing the service are offered. The
    default is `false` for STUN/STUNS services and `true` for TURN/TURNS
    services.

    - **transport**: `tcp | udp`  
   The transport protocol supported by the
    service. The default is `udp` for non-TLS services and `tcp` for TLS
    services.

    - **type**: `stun | turn | stuns | turns`  
   The type of service. Must be
    `stun` or `turn` for non-TLS services, `stuns` or `turns` for TLS
    services. The default type is `stun`.

    **Example**:

    ~~~ yaml
    services:
      -
        host: 203.0.113.3
        port: 3478
        type: stun
        transport: udp
        restricted: false
      -
        host: 203.0.113.3
        port: 3478
        type: turn
        transport: udp
        restricted: true
      -
        host: 2001:db8::3
        port: 3478
        type: stun
        transport: udp
        restricted: false
      -
        host: 2001:db8::3
        port: 3478
        type: turn
        transport: udp
        restricted: true
      -
        host: server.example.com
        port: 5349
        type: turns
        transport: tcp
        restricted: true
    ~~~

mod\_time
---------

This module adds support for [XEP-0202: Entity
Time](https://xmpp.org/extensions/xep-0202.html). In other words, the
module reports server’s system time.

The module has no options.

mod\_vcard
----------

This module allows end users to store and retrieve their vCard, and to
retrieve other users vCards, as defined in [XEP-0054:
vcard-temp](https://xmpp.org/extensions/xep-0054.html). The module also
implements an uncomplicated Jabber User Directory based on the vCards of
these users. Moreover, it enables the server to send its vCard when
queried.

__Available options:__

- **allow\_return\_all**: `true | false`  
This option enables you to specify if search operations with empty input
fields should return all users who added some information to their
vCard. The default value is `false`.

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**: `true | false`  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **db\_type**: `mnesia | sql | ldap`  
Same as top-level [default_db](toplevel.md#default_db) option, but applied to this module
only.

- **host**  
Deprecated. Use `hosts` instead.

- **hosts**: `[Host, ...]`  
This option defines the Jabber IDs of the service. If the `hosts` option
is not specified, the only Jabber ID will be the hostname of the virtual
host with the prefix "vjud.". The keyword `@HOST@` is replaced with the
real virtual host name.

- **matches**: `pos_integer() | infinity`  
With this option, the number of reported search results can be limited.
If the option’s value is set to `infinity`, all search results are
reported. The default value is `30`.

- **name**: `Name`  
The value of the service name. This name is only visible in some clients
that support [XEP-0030: Service
Discovery](https://xmpp.org/extensions/xep-0030.html). The default is
`vCard User Search`.

- **search**: `true | false`  
This option specifies whether the search functionality is enabled or
not. If disabled, the options `hosts`, `name` and `vcard` will be
ignored and the Jabber User Directory service will not appear in the
Service Discovery item list. The default value is `false`.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

- **vcard**: `vCard`  
A custom vCard of the server that will be displayed by some XMPP clients
in Service Discovery. The value of `vCard` is a YAML map constructed
from an XML representation of vCard. Since the representation has no
attributes, the mapping is straightforward.

    **Example**:

    ~~~ yaml
    # This XML representation of vCard:
    #
    #   <vCard xmlns='vcard-temp'>
    #     <FN>Conferences</FN>
    #     <ADR>
    #       <WORK/>
    #       <STREET>Elm Street</STREET>
    #     </ADR>
    #   </vCard>
    #
    # is translated to:
    #
    vcard:
      fn: Conferences
      adr:
        -
          work: true
          street: Elm Street
    ~~~

__Available options for `ldap` backend:__

- **ldap\_backups**  
Same as top-level [ldap_backups](toplevel.md#ldap_backups) option, but applied to this module
only.

- **ldap\_base**  
Same as top-level [ldap_base](toplevel.md#ldap_base) option, but applied to this module only.

- **ldap\_deref\_aliases**  
Same as top-level [ldap_deref_aliases](toplevel.md#ldap_deref_aliases) option, but applied to this
module only.

- **ldap\_encrypt**  
Same as top-level [ldap_encrypt](toplevel.md#ldap_encrypt) option, but applied to this module
only.

- **ldap\_filter**  
Same as top-level [ldap_filter](toplevel.md#ldap_filter) option, but applied to this module
only.

- **ldap\_password**  
Same as top-level [ldap_password](toplevel.md#ldap_password) option, but applied to this module
only.

- **ldap\_port**  
Same as top-level [ldap_port](toplevel.md#ldap_port) option, but applied to this module only.

- **ldap\_rootdn**  
Same as top-level [ldap_rootdn](toplevel.md#ldap_rootdn) option, but applied to this module
only.

- **ldap\_search\_fields**: `{Name: Attribute, ...}`  
This option defines the search form and the LDAP attributes to search
within. `Name` is the name of a search form field which will be
automatically translated by using the translation files (see
`msgs/*.msg` for available words). `Attribute` is the LDAP attribute or
the pattern `%u`.

    **Examples**:

    The default is:

    ~~~ yaml
    User: "%u"
    "Full Name": displayName
    "Given Name": givenName
    "Middle Name": initials
    "Family Name": sn
    Nickname: "%u"
    Birthday: birthDay
    Country: c
    City: l
    Email: mail
    "Organization Name": o
    "Organization Unit": ou
    ~~~

- **ldap\_search\_reported**: `{SearchField: VcardField}, ...}`  
This option defines which search fields should be reported.
`SearchField` is the name of a search form field which will be
automatically translated by using the translation files (see
`msgs/*.msg` for available words). `VcardField` is the vCard field name
defined in the `ldap_vcard_map` option.

    **Examples**:

    The default is:

    ~~~ yaml
    "Full Name": FN
    "Given Name": FIRST
    "Middle Name": MIDDLE
    "Family Name": LAST
    "Nickname": NICKNAME
    "Birthday": BDAY
    "Country": CTRY
    "City": LOCALITY
    "Email": EMAIL
    "Organization Name": ORGNAME
    "Organization Unit": ORGUNIT
    ~~~

- **ldap\_servers**  
Same as top-level [ldap_servers](toplevel.md#ldap_servers) option, but applied to this module
only.

- **ldap\_tls\_cacertfile**  
Same as top-level [ldap_tls_cacertfile](toplevel.md#ldap_tls_cacertfile) option, but applied to this
module only.

- **ldap\_tls\_certfile**  
Same as top-level [ldap_tls_certfile](toplevel.md#ldap_tls_certfile) option, but applied to this
module only.

- **ldap\_tls\_depth**  
Same as top-level [ldap_tls_depth](toplevel.md#ldap_tls_depth) option, but applied to this module
only.

- **ldap\_tls\_verify**  
Same as top-level [ldap_tls_verify](toplevel.md#ldap_tls_verify) option, but applied to this module
only.

- **ldap\_uids**  
Same as top-level [ldap_uids](toplevel.md#ldap_uids) option, but applied to this module only.

- **ldap\_vcard\_map**: `{Name: {Pattern, LDAPattributes}, ...}`  
With this option you can set the table that maps LDAP attributes to
vCard fields. `Name` is the type name of the vCard as defined in [RFC
2426](https://tools.ietf.org/html/rfc2426). `Pattern` is a string which
contains pattern variables `%u`, `%d` or `%s`. `LDAPattributes` is the
list containing LDAP attributes. The pattern variables `%s` will be
sequentially replaced with the values of LDAP attributes from
`List_of_LDAP_attributes`, `%u` will be replaced with the user part
of a JID, and `%d` will be replaced with the domain part of a JID.

    **Examples**:

    The default is:

    ~~~ yaml
    NICKNAME: {"%u": []}
    FN: {"%s": [displayName]}
    LAST: {"%s": [sn]}
    FIRST: {"%s": [givenName]}
    MIDDLE: {"%s": [initials]}
    ORGNAME: {"%s": [o]}
    ORGUNIT: {"%s": [ou]}
    CTRY: {"%s": [c]}
    LOCALITY: {"%s": [l]}
    STREET: {"%s": [street]}
    REGION: {"%s": [st]}
    PCODE: {"%s": [postalCode]}
    TITLE: {"%s": [title]}
    URL: {"%s": [labeleduri]}
    DESC: {"%s": [description]}
    TEL: {"%s": [telephoneNumber]}
    EMAIL: {"%s": [mail]}
    BDAY: {"%s": [birthDay]}
    ROLE: {"%s": [employeeType]}
    PHOTO: {"%s": [jpegPhoto]}
    ~~~

__Available options for `mnesia` backend:__

- **search\_all\_hosts**: `true | false`  
Whether to perform search on all virtual hosts or not. The default value
is `true`.

mod\_vcard\_xupdate
-------------------

The user’s client can store an avatar in the user vCard. The vCard-Based
Avatars protocol ([XEP-0153](https://xmpp.org/extensions/xep-0153.html))
provides a method for clients to inform the contacts what is the avatar
hash value. However, simple or small clients may not implement that
protocol.

If this module is enabled, all the outgoing client presence stanzas get
automatically the avatar hash on behalf of the client. So, the contacts
receive the presence stanzas with the `Update Data` described in
[XEP-0153](https://xmpp.org/extensions/xep-0153.html) as if the client
would had inserted it itself. If the client had already included such
element in the presence stanza, it is replaced with the element
generated by ejabberd.

By enabling this module, each vCard modification produces a hash
recalculation, and each presence sent by a client produces hash
retrieval and a presence stanza rewrite. For this reason, enabling this
module will introduce a computational overhead in servers with clients
that change frequently their presence. However, the overhead is
significantly reduced by the use of caching, so you probably don’t want
to set `use_cache` to `false`.

The module depends on [mod_vcard](#mod_vcard).

!!! note

    Nowadays [XEP-0153](https://xmpp.org/extensions/xep-0153.html) is used
    mostly as "read-only", i.e. modern clients don’t publish their avatars
    inside vCards. Thus in the majority of cases the module is only used
    along with [mod_avatar](#mod_avatar) for providing backward compatibility.

__Available options:__

- **cache\_life\_time**: `timeout()`  
Same as top-level [cache_life_time](toplevel.md#cache_life_time) option, but applied to this module
only.

- **cache\_missed**: `true | false`  
Same as top-level [cache_missed](toplevel.md#cache_missed) option, but applied to this module
only.

- **cache\_size**: `pos_integer() | infinity`  
Same as top-level [cache_size](toplevel.md#cache_size) option, but applied to this module
only.

- **use\_cache**: `true | false`  
Same as top-level [use_cache](toplevel.md#use_cache) option, but applied to this module only.

mod\_version
------------

This module implements [XEP-0092: Software
Version](https://xmpp.org/extensions/xep-0092.html). Consequently, it
answers ejabberd’s version when queried.

__Available options:__

- **show\_os**: `true | false`  
Should the operating system be revealed or not. The default value is
`true`.


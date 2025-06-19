---
search:
  exclude: true
---

# Listen Modules

!!! info "Please note"

    This section describes the most recent ejabberd version. If you are using an old ejabberd release, please refer to the corresponding archived version of this page in the [Archive](../../archive/index.md).

## Listen Options

The `listen` option defines for which ports, addresses and network
protocols `ejabberd` will listen and what services will be run on them.

Each element of the list is an associative array with the following
elements:

- **port**: *Number*

    Defines which port number to listen for incoming connections:
    it can be a Jabber/XMPP standard port or any other valid port number.

    Alternatively, set the option to a string in form `"unix:/path/to/socket"`
    to create and listen on a unix domain socket `/path/to/socket`.

- **ip**: *IpAddress*

    The socket will listen only in that network interface.
    Depending on the type of the IP address, IPv4 or IPv6 will be used.

    It is possible to specify a generic address
    (`"0.0.0.0"` for IPv4 or `"::"` for IPv6),
    so `ejabberd` will listen in all addresses.
    Note that on some operating systems and/or OS configurations, listening
    on `"::"` will mean listening for IPv4 traffic as well as IPv6 traffic.

    Some example values for IP address:

    - `"0.0.0.0"` to listen in all IPv4 network interfaces. This is the
     default value when the option is not specified.

    - `"::"` to listen in all IPv6 network interfaces

    - `"10.11.12.13"` is the IPv4 address `10.11.12.13`

    - `"::FFFF:127.0.0.1"` is the IPv6 address `::FFFF:127.0.0.1/128`

- **transport**: *tcp|udp*

    Defines the transport protocol. Default is `tcp`.

- **module**: *ModuleName*

    Listening module that serves this port

- Any other options for the socket and for the listening module, described later.

For example:

``` yaml
listen:
  -
    port: 5222
    ip: 127.0.0.1
    module: ejabberd_c2s
    starttls: true
  -
    port: 5269
    transport: tcp
    module: ejabberd_s2s_in
```

## ejabberd_c2s

Handles c2s connections.

General listen options supported:
[access](listen-options.md#access),
[allow_unencrypted_sasl2](listen-options.md#allow_unencrypted_sasl2),
[cafile](listen-options.md#cafile),
[ciphers](listen-options.md#ciphers),
[dhfile](listen-options.md#dhfile),
[max_fsm_queue](listen-options.md#max_fsm_queue),
[max_stanza_size](listen-options.md#max_stanza_size),
[protocol_options](listen-options.md#protocol_options),
[send_timeout](listen-options.md#send_timeout),
[shaper](listen-options.md#shaper),
[starttls](listen-options.md#starttls),
[starttls_required](listen-options.md#starttls_required),
[tls](listen-options.md#tls),
[tls_compression](listen-options.md#tls-compression),
[tls_verify](listen-options.md#tls_verify),
[zlib](listen-options.md#zlib).

## ejabberd_s2s_in

Handles incoming s2s connections.

General listen options supported:
[cafile](listen-options.md#cafile),
[ciphers](listen-options.md#ciphers),
[dhfile](listen-options.md#dhfile),
[max_fsm_queue](listen-options.md#max_fsm_queue),
[max_stanza_size](listen-options.md#max_stanza_size),
[protocol_options](listen-options.md#protocol_options),
[send_timeout](listen-options.md#send_timeout),
[shaper](listen-options.md#shaper),
[tls](listen-options.md#tls),
[tls_compression](listen-options.md#tls-compression).

## ejabberd_service

Interacts with an [`external component`](https://ejabberd.im/tutorials-transports)
as defined in [XEP-0114: Jabber Component Protocol](https://xmpp.org/extensions/xep-0114.html).

General listen options supported:
[access](listen-options.md#access),
[cafile](listen-options.md#cafile),
[certfile](listen-options.md#certfile),
[check_from](listen-options.md#check_from),
[ciphers](listen-options.md#ciphers),
[dhfile](listen-options.md#dhfile),
[global_routes](listen-options.md#global_routes),
[hosts](listen-options.md#hosts),
[max_fsm_queue](listen-options.md#max_fsm_queue),
[max_stanza_size](listen-options.md#max_stanza_size),
[password](listen-options.md#password),
[protocol_options](listen-options.md#protocol_options),
[send_timeout](listen-options.md#send_timeout),
[shaper](listen-options.md#shaper),
[shaper_rule](listen-options.md#shaper-rule),
[tls](listen-options.md#tls),
[tls_compression](listen-options.md#tls-compression).

## mod_mqtt

Support for MQTT requires configuring `mod_mqtt` both in the
[listen](toplevel.md#listen) and the
[modules](toplevel.md#modules) sections.
Check the [mod_mqtt module](modules.md#mod_mqtt) options,
and the [MQTT Support](../../admin/guide/mqtt/index.md) section.

General listen options supported:
[backlog](listen-options.md#backlog),
[max_fsm_queue](listen-options.md#max_fsm_queue),
[max_payload_size](listen-options.md#max_payload_size),
[send_timeout](listen-options.md#send_timeout),
[tls](listen-options.md#tls),
[tls_verify](listen-options.md#tls_verify).

<!-- other potions supposedly supported by mqtt, but undocumented
     accept_interval, supervisor, transport, use_proxy_protocol -->

## ejabberd_stun

`ejabberd` can act as a stand-alone STUN/TURN server,
and this module handles STUN/TURN requests as defined in
([`RFC 5389`](https://tools.ietf.org/html/rfc5389)/[`RFC 5766`](https://tools.ietf.org/html/rfc5766).
In that role `ejabberd` helps clients with ICE
([`RFC 5245`](https://tools.ietf.org/html/rfc5245) or Jingle ICE
([`XEP-0176`](https://xmpp.org/extensions/xep-0176.html) support to
discover their external addresses and ports and to relay media traffic
when it is impossible to establish direct peer-to-peer connection.

General listen options supported:
[certfile](listen-options.md#certfile),
[send_timeout](listen-options.md#send_timeout),
[shaper](listen-options.md#shaper),
[tls](listen-options.md#tls),

The specific `ejabberd_stun` configurable options are:

- **auth_realm**: *String*

    When `auth_type` is set to `user` and you have several virtual hosts
    configured you should set this option explicitly to the virtual host
    you want to serve on this particular listening port.
    Implies `use_turn`.

- **auth_type**: *user|anonymous*

    Which authentication type to use for TURN allocation requests.
    When type `user` is set, ejabberd authentication backend is used.
    For `anonymous` type no authentication is performed (not recommended for public services).
    The default is `user`.
    Implies `use_turn`.

- **shaper**: *Atom*

    For `tcp` transports defines shaper to use.
    The default is `none`.

- **server_name**: *String*

    Defines software version to return with every response.
    The default is the STUN library version.

- **turn_blacklist**: *String | [String,...]*

    Specify one or more IP addresses and/or subnet addresses/masks.
    The TURN server will refuse to relay traffic from/to blacklisted IP addresses.
    By default, loopback addresses (`127.0.0.0/8` and `::1/128`) are blacklisted.

- **turn_ipv4_address**: *String*

    The IPv4 address advertised by your TURN server.
    The address should not be NAT’ed or firewalled.
    There is not default, so you should set this option explicitly.
    Implies `use_turn`.

- **turn_ipv6_address**: *String*

    The IPv6 address advertised by your TURN server.
    The address should not be NAT’ed or firewalled.
    There is not default, so you should set this option explicitly.
    Implies `use_turn`.

- **turn_max_allocations**: *Integer|infinity*

    Maximum number of TURN allocations available from the particular IP address.
    The default value is 10. Implies `use_turn`.

- **turn_max_permissions**: *Integer|infinity*

    Maximum number of TURN permissions available from the particular IP address.
    The default value is 10.
    Implies `use_turn`.

- **turn_max_port**: *Integer*

    Together with `turn_min_port` forms port range to allocate from.
    The default is 65535.
    Implies `use_turn`.

- **turn_min_port**: *Integer*

    Together with `turn_max_port` forms port range to allocate from.
    The default is 49152.
    Implies `use_turn`.

- **use_turn**: *true|false*

    Enables/disables TURN (media relay) functionality.
    The default is `false`.

Example configuration with disabled TURN functionality (STUN only):

``` yaml
listen:
  -
    port: 5478
    transport: udp
    module: ejabberd_stun
  -
    port: 5478
    module: ejabberd_stun
  -
    port: 5349
    module: ejabberd_stun
    tls: true
    certfile: /etc/ejabberd/server.pem
```

Example configuration with TURN functionality. Note that STUN is always
enabled if TURN is enabled. Here, only UDP section is shown:

``` yaml
listen:
  -
    port: 5478
    transport: udp
    use_turn: true
    turn_ipv4_address: 10.20.30.1
    module: ejabberd_stun
```

## ejabberd_sip


`ejabberd` has built-in support to handle SIP requests
as defined in [`RFC 3261`](https://tools.ietf.org/html/rfc3261).

To activate this feature,
add the [`ejabberd_sip`](#ejabberd_sip) listen module, enable
[`mod_sip`](modules.md#mod_sip) module
for the desired virtual host, and configure DNS properly.

To add a listener you should configure `ejabberd_sip` listening module
as described in [Listen](#listen-options) section.
If option [`tls`](listen-options.md#tls) is specified,
option [`certfile`](listen-options.md#certfile)
must be specified as well, otherwise incoming TLS connections
would fail.

General listen options supported:
[certfile](listen-options.md#certfile),
[send_timeout](listen-options.md#send_timeout),
[tls](listen-options.md#tls).

Example configuration with standard ports (as per
[`RFC 3261`](https://tools.ietf.org/html/rfc3261)):

``` yaml
listen:
  -
    port: 5060
    transport: udp
    module: ejabberd_sip
  -
    port: 5060
    module: ejabberd_sip
  -
    port: 5061
    module: ejabberd_sip
    tls: true
    certfile: /etc/ejabberd/server.pem
```

Note that there is no StartTLS support in SIP and
[`SNI`](https://en.wikipedia.org/wiki/Server_Name_Indication) support is
somewhat tricky, so for TLS you have to configure different virtual
hosts on different ports if you have different certificate files for
them.

Next you need to configure DNS SIP records for your virtual domains.
Refer to [`RFC 3263`](https://tools.ietf.org/html/rfc3263) for the
detailed explanation. Simply put, you should add NAPTR and SRV records
for your domains. Skip NAPTR configuration if your DNS provider doesn't
support this type of records. It’s not fatal, however, highly
recommended.

Example configuration of NAPTR records:

``` sh
example.com IN NAPTR 10  0 "s" "SIPS+D2T" "" _sips._tcp.example.com.
example.com IN NAPTR 20  0 "s" "SIP+D2T" "" _sip._tcp.example.com.
example.com IN NAPTR 30  0 "s" "SIP+D2U" "" _sip._udp.example.com.
```

Example configuration of SRV records with standard ports (as per
[`RFC 3261`](https://tools.ietf.org/html/rfc3261):

``` sh
_sip._udp   IN SRV  0 0 5060 sip.example.com.
_sip._tcp   IN SRV  0 0 5060 sip.example.com.
_sips._tcp  IN SRV  0 0 5061 sip.example.com.
```

!!! warning

    SIP authentication does not support SCRAM. As such, it is not possible
    to use `mod_sip` to authenticate when ejabberd has been set to encrypt
    password with SCRAM.

## ejabberd_http

Handles incoming HTTP connections.

With the proper request handlers configured, this serves HTTP services like
[ACME](../../admin/configuration/basic.md#acme),
[API](modules.md#mod_http_api),
[BOSH](modules.md#mod_bosh),
[CAPTCHA](../../admin/configuration/basic.md#captcha),
[Fileserver](modules.md#mod_http_fileserver),
[OAuth](../../developer/ejabberd-api/oauth.md),
[RegisterWeb](modules.md#mod_register_web),
[Upload](modules.md#mod_http_upload),
[WebAdmin](../../admin/guide/managing.md#web-admin),
[WebSocket](#ejabberd_http_ws),
[XML-RPC](#ejabberd_xmlrpc).

Options:
[cafile](listen-options.md#cafile),
[ciphers](listen-options.md#ciphers),
[custom_headers](listen-options.md#custom_headers),
[dhfile](listen-options.md#dhfile),
[protocol_options](listen-options.md#protocol_options),
[request_handlers](listen-options.md#request_handlers),
[send_timeout](listen-options.md#send_timeout),
[tag](listen-options.md#tag),
[tls](listen-options.md#tls),
[tls_compression](listen-options.md#tls-compression),
and the [trusted_proxies](toplevel.md#trusted_proxies) top-level option.

### ejabberd_http_ws

This module enables XMPP communication over WebSocket connection as
described in [`RFC 7395`](https://tools.ietf.org/html/rfc7395).

#### WebSocket Config

To enable WebSocket, simply add a handler to the `request_handlers`
section of an `ejabberd_http` listener:

``` yaml
listen:
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      /xmpp: ejabberd_http_ws
```

This module can be configured using those top-level options:

- [websocket\_origin](toplevel.md#websocket_origin)
- [websocket\_ping\_interval](toplevel.md#websocket_ping_interval)
- [websocket\_timeout](toplevel.md#websocket_timeout)

#### WebSocket Discovery

With the example configuration previously mentioned,
the WebSocket URL would be: `ws://localhost:5280/xmpp`

You may want to provide a `host-meta` file so clients can
easily discover WebSocket service for your XMPP domain
(see [XEP-0156](https://xmpp.org/extensions/xep-0156.html#http)).
One easy way to provide that file is using
[`mod_host_meta`](modules.md#mod_host_meta).

#### Testing WebSocket

A test client can be found on Github: [WebSocket test client](https://github.com/processone/xmpp-websocket-client)

There is an example configuration for WebSocket and Converse.js in the
ejabberd [21.12](../../archive/21.12/index.md) release notes.

<!-- TODO We should probably embed a test WebSocket client on the WebSocket info get page. -->

### ejabberd_xmlrpc

Handles XML-RPC requests to execute
[ejabberd commands](../../admin/guide/managing.md#ejabberd_commands).
It is configured as a request handler in
[ejabberd_http](#ejabberd_http).

This is the minimum configuration required to enable the feature:

``` yaml
listen:
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      /xmlrpc: ejabberd_xmlrpc

api_permissions:
  "public commands":
    who:
      ip: 127.0.0.1/8
    what:
      - connected_users_number
```

Example Python3 script:

``` python
import xmlrpc.client
server = xmlrpc.client.ServerProxy("http://127.0.0.1:5280/xmlrpc/");
print(server.connected_users_number())
```

By default there is no restriction to who can execute what commands,
so it is strongly recommended that you configure restrictions using
[API Permissions](../../developer/ejabberd-api/permissions.md).

This example configuration adds some restrictions (only requests from localhost are accepted, the XML-RPC query must include authentication credentials of a specific account registered in ejabberd, and only two commands are accepted):

``` yaml
listen:
  -
    port: 5280
    ip: "::"
    module: ejabberd_http
    request_handlers:
      /xmlrpc: ejabberd_xmlrpc

api_permissions:
  "some XMLRPC commands":
    from: ejabberd_xmlrpc
    who:
      - ip: 127.0.0.1
      - user: user1@localhost
    what:
      - registered_users
      - connected_users_number
```

Example Python3 script for that restricted configuration:

``` python
import xmlrpc.client
server = xmlrpc.client.ServerProxy("http://127.0.0.1:5280/xmlrpc/");

params = {}
params['host'] = 'localhost'

auth = {'user': 'user1',
        'server': 'localhost',
        'password': 'mypass11',
        'admin': True}

def calling(command, data):
    fn = getattr(server, command)
    return fn(auth, data)

print(calling('registered_users', params))
```

Please notice, when using the old Python2, replace the two first lines with:

``` python
import xmlrpclib
server = xmlrpclib.Server("http://127.0.0.1:5280/xmlrpc/");
```

It's possible to use OAuth for authentication instead of plain password, see
[OAuth Support](../../developer/ejabberd-api/oauth.md).

In ejabberd [20.03](../../archive/20.03/index.md) and older,
it was possible to configure `ejabberd_xmlrpc` as a listener.

Just for reference, there's also the old
[`ejabberd_xmlrpc documentation`](https://ejabberd.im/ejabberd_xmlrpc)
with example clients in other languages.

## Examples

For example, the following simple configuration defines:

- There are three domains. The default certificate file is `server.pem`. However, the c2s and s2s connections to the domain `example.com` use the file `example_com.pem`.

- Port 5222 listens for c2s connections with STARTTLS, and also allows plain connections for old clients.

- Port 5223 listens for c2s connections with the old SSL.

- Port 5269 listens for s2s connections with STARTTLS. The socket is set for IPv6 instead of IPv4.

- Port 5478 listens for STUN requests over UDP.

- Port 5280 listens for HTTP requests, and serves the HTTP-Bind (BOSH) service.

- Port 5281 listens for HTTP requests, using HTTPS to serve HTTP-Bind (BOSH) and the Web Admin as explained in [Managing: Web Admin](../../admin/guide/managing.md#web-admin). The socket only listens connections to the IP address 127.0.0.1.

``` yaml
hosts:
  - example.com
  - example.org
  - example.net

certfiles:
  - /etc/ejabberd/server.pem
  - /etc/ejabberd/example_com.pem

listen:
  -
    port: 5222
    module: ejabberd_c2s
    access: c2s
    shaper: c2s_shaper
    starttls: true
    max_stanza_size: 65536
  -
    port: 5223
    module: ejabberd_c2s
    access: c2s
    shaper: c2s_shaper
    tls: true
    max_stanza_size: 65536
  -
    port: 5269
    ip: "::"
    module: ejabberd_s2s_in
    shaper: s2s_shaper
    max_stanza_size: 131072
  -
    port: 5478
    transport: udp
    module: ejabberd_stun
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      /bosh: mod_bosh
  -
    port: 5281
    ip: 127.0.0.1
    module: ejabberd_http
    tls: true
    request_handlers:
      /admin: ejabberd_web_admin
      /bosh: mod_bosh

s2s_use_starttls: optional
outgoing_s2s_families:
  - ipv4
  - ipv6
outgoing_s2s_timeout: 10000
trusted_proxies: [127.0.0.1, 192.168.1.11]
```

In this example, the following configuration defines that:

- c2s connections are listened for on port 5222 (all IPv4 addresses)
 and on port 5223 (SSL, IP 192.168.0.1 and fdca:8ab6:a243:75ef::1)
 and denied for the user called ‘`bad`’.

- s2s connections are listened for on port 5269 (all IPv4 addresses)
 with STARTTLS for secured traffic strictly required, and the
 certificates are verified. Incoming and outgoing connections of
 remote XMPP servers are denied, only two servers can connect:
 “jabber.example.org” and “example.com”.

- Port 5280 is serving the Web Admin and the HTTP-Bind (BOSH) service in
 all the IPv4 addresses. Note that it is also possible to serve them
 on different ports. The second example in section [Managing: Web Admin](../../admin/guide/managing.md#web-admin) shows
 how exactly this can be done. A request handler to serve MQTT over WebSocket is also defined.

- All users except for the administrators have a traffic of limit
 1,000Bytes/second

- The [`AIM transport`](https://ejabberd.im/pyaimt)
 `aim.example.org` is connected to port 5233 on localhost IP
 addresses (127.0.0.1 and ::1) with password ‘`aimsecret`’.

- The ICQ transport JIT (`icq.example.org` and `sms.example.org`) is
 connected to port 5234 with password ‘`jitsecret`’.

- The [`MSN transport`](https://ejabberd.im/pymsnt)
 `msn.example.org` is connected to port 5235 with password
 ‘`msnsecret`’.

- The [`Yahoo! transport`](https://ejabberd.im/yahoo-transport-2)
 `yahoo.example.org` is connected to port 5236 with password
 ‘`yahoosecret`’.

- The
 [`Gadu-Gadu transport`](https://ejabberd.im/jabber-gg-transport)
 `gg.example.org` is connected to port 5237 with password
 ‘`ggsecret`’.

- The [`Jabber Mail Component`](https://ejabberd.im/jmc)
 `jmc.example.org` is connected to port 5238 with password
 ‘`jmcsecret`’.

- The service custom has enabled the special option to avoiding
 checking the `from` attribute in the packets send by this component.
 The component can send packets in behalf of any users from the
 server, or even on behalf of any server.

``` yaml
acl:
  blocked:
    user: bad
  trusted_servers:
    server:
      - example.com
      - jabber.example.org
  xmlrpc_bot:
    user:
      - xmlrpc-robot@example.org
shaper:
  normal: 1000
shaper_rules:
  c2s_shaper:
    - none: admin
    - normal
access_rules:
  c2s:
    - deny: blocked
    - allow
  xmlrpc_access:
    - allow: xmlrpc_bot
  s2s:
    - allow: trusted_servers
certfiles:
  - /path/to/ssl.pem
s2s_access: s2s
s2s_use_starttls: required_trusted
listen:
  -
    port: 5222
    module: ejabberd_c2s
    shaper: c2s_shaper
    access: c2s
  -
    ip: 192.168.0.1
    port: 5223
    module: ejabberd_c2s
    tls: true
    access: c2s
  -
    ip: "FDCA:8AB6:A243:75EF::1"
    port: 5223
    module: ejabberd_c2s
    tls: true
    access: c2s
  -
    port: 5269
    module: ejabberd_s2s_in
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      /admin: ejabberd_web_admin
      /bosh: mod_bosh
      /mqtt: mod_mqtt
  -
    port: 4560
    module: ejabberd_xmlrpc
    access_commands: {}
  -
    ip: 127.0.0.1
    port: 5233
    module: ejabberd_service
    hosts:
      aim.example.org:
        password: aimsecret
  -
    ip: "::1"
    port: 5233
    module: ejabberd_service
    hosts:
      aim.example.org:
        password: aimsecret
  -
    port: 5234
    module: ejabberd_service
    hosts:
      icq.example.org:
        password: jitsecret
      sms.example.org:
        password: jitsecret
  -
    port: 5235
    module: ejabberd_service
    hosts:
      msn.example.org:
        password: msnsecret
  -
    port: 5236
    module: ejabberd_service
    password: yahoosecret
  -
    port: 5237
    module: ejabberd_service
    hosts:
      gg.example.org:
        password: ggsecret
  -
    port: 5238
    module: ejabberd_service
    hosts:
      jmc.example.org:
        password: jmcsecret
  -
    port: 5239
    module: ejabberd_service
    check_from: false
    hosts:
      custom.example.org:
        password: customsecret
```

Note, that for services based in jabberd14 or WPJabber you have to make
the transports log and do XDB by themselves:

``` xml
<!--
   You have to add elogger and rlogger entries here when using ejabberd.
   In this case the transport will do the logging.
-->

<log id='logger'>
  <host/>
  <logtype/>
  <format>%d: [%t] (%h): %s</format>
  <file>/var/log/jabber/service.log</file>
</log>

<!--
   Some XMPP server implementations do not provide
   XDB services (for example, jabberd2 and ejabberd).
   xdb_file.so is loaded in to handle all XDB requests.
-->

<xdb id="xdb">
  <host/>
  <load>
    <!-- this is a lib of wpjabber or jabberd14 -->
    <xdb_file>/usr/lib/jabber/xdb_file.so</xdb_file>
    </load>
  <xdb_file xmlns="jabber:config:xdb_file">
    <spool><jabberd:cmdline flag='s'>/var/spool/jabber</jabberd:cmdline></spool>
  </xdb_file>
</xdb>
```

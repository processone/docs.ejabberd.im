---
search:
  exclude: true
---

# Listen Option

The **listen option** defines for which ports, addresses and network
protocols `ejabberd` will listen and what services will be run on them.
Each element of the list is an associative array with the following
elements:

-   Port, optionally also the IP address and/or a transport
	protocol.

-   Listening module that serves this port.

-   Options for the TCP socket and for the listening module.

For example:


	listen:
	  -
	    port: 5222
	    module: ejabberd_c2s
	    ip: 127.0.0.1
	    starttls: true
	  -
	    port: 5269
	    module: ejabberd_s2s_in
	    transport: tcp


The **port** defines which port number to listen for incoming connections:
it can be a Jabber/XMPP standard port or any other valid port number.
Alternatively, set the option to a string in form `"unix:/path/to/socket"`
to create and listen on a unix domain socket `/path/to/socket`.

The **IP address** can be represented as a string. The socket will listen
only in that network interface. It is possible to specify a generic
address ("0.0.0.0" for IPv4 or "::" for IPv6), so `ejabberd` will listen
in all addresses. Depending on the type of the IP address, IPv4 or IPv6
will be used. When the IP address is not specified, it will listen on
all IPv4 network addresses.

Note that on some operating systems and/or OS configurations, listening
on "::" will mean listening for IPv4 traffic as well as IPv6 traffic.

Some example values for IP address:

-   `"0.0.0.0"` to listen in all IPv4 network interfaces. This is the
	default value when no IP is specified.

-   `"::"` to listen in all IPv6 network interfaces

-   `"10.11.12.13"` is the IPv4 address `10.11.12.13`

-   `"::FFFF:127.0.0.1"` is the IPv6 address `::FFFF:127.0.0.1/128`

The **transport protocol** can be `tcp` or `udp`. Default is `tcp`.

# Summary of Listen Modules

The available modules, their purpose and the options allowed by each one
are:

## ejabberd_c2s

Handles c2s connections.

Options:
[access](listen-options.md#access),
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
[tls_compression](listen-options.md#tls_compression),
[tls_verify](listen-options.md#tls_verify),
[zlib](listen-options.md#zlib).

## ejabberd_s2s_in

Handles incoming s2s connections.

Options:
[cafile](listen-options.md#cafile),
[ciphers](listen-options.md#ciphers),
[dhfile](listen-options.md#dhfile),
[max_fsm_queue](listen-options.md#max_fsm_queue),
[max_stanza_size](listen-options.md#max_stanza_size),
[protocol_options](listen-options.md#protocol_options),
[send_timeout](listen-options.md#send_timeout),
[shaper](listen-options.md#shaper),
[tls](listen-options.md#tls),
[tls_compression](listen-options.md#tls_compression).

## ejabberd_service

Interacts with an
	[`external component`](https://ejabberd.im/tutorials-transports)
	(as defined in the Jabber Component Protocol
	([`XEP-0114`](https://xmpp.org/extensions/xep-0114.html)).

Options:
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
[shaper_rule](listen-options.md#shaper_rule),
[tls](listen-options.md#tls),
[tls_compression](listen-options.md#tls_compression).

## ejabberd_sip

Handles SIP requests as defined in
	[`RFC 3261`](https://tools.ietf.org/html/rfc3261).

For details please check the
[ejabberd_sip](listen.md#ejabberd_sip_1)
and [mod_sip](modules.md#mod_sip) sections.

General listener options:
[certfile](listen-options.md#certfile),
[send_timeout](listen-options.md#send_timeout),
[tls](listen-options.md#tls).

## ejabberd_stun

Handles STUN/TURN requests as defined in
	[`RFC 5389`](https://tools.ietf.org/html/rfc5389) and
	[`RFC 5766`](https://tools.ietf.org/html/rfc5766).

For the specific module options, please check the
[ejabberd_stun](listen.md#ejabberd_stun_1) section:
`auth_realm`,
`auth_type`,
`server_name`,
`turn_blacklist`,
`turn_ipv4_address`,
`turn_ipv6_address`,
`turn_max_allocations`,
`turn_max_permissions`,
`turn_max_port`,
`turn_min_port`,
`use_turn`.

General listener options:
[certfile](listen-options.md#certfile),
[send_timeout](listen-options.md#send_timeout),
[shaper](listen-options.md#shaper),
[tls](listen-options.md#tls),

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
[WebAdmin](../../admin/guide/managing.md#web_admin),
[WebSocket](listen.md#ejabberd_http_ws),
[XMP-RPC](listen.md#ejabberd_xmlrpc).

Options:
[cafile](listen-options.md#cafile),
[ciphers](listen-options.md#ciphers),
[custom_headers](listen-options.md#custom_headers),
[default_host](listen-options.md#default_host),
[dhfile](listen-options.md#dhfile),
[protocol_options](listen-options.md#protocol_options),
[request_handlers](listen-options.md#request_handlers),
[send_timeout](listen-options.md#send_timeout),
[tag](listen-options.md#tag),
[tls](listen-options.md#tls),
[tls_compression](listen-options.md#tls_compression),
and the [trusted_proxies](toplevel.md#trusted_proxies) top-level option.

## mod_mqtt

Support for MQTT requires configuring `mod_mqtt` both in the
[listen](toplevel.md#listen) and the
[modules](toplevel.md#modules) sections.
Check the [mod_mqtt modules options](modules.md#mod_mqtt).
Check the advanded documentation in [MQTT Support](../../admin/guide/mqtt/index.md).

Listen options:
[max_fsm_queue](listen-options.md#max_fsm_queue),
[max_payload_size](listen-options.md#max_payload_size),
[send_timeout](listen-options.md#send_timeout),
[tls](listen-options.md#tls),
[tls_verify](listen-options.md#tls_verify).


# ejabberd_stun

`ejabberd` is able to act as a stand-alone STUN/TURN server
([`RFC 5389`](https://tools.ietf.org/html/rfc5389)/[`RFC 5766`](https://tools.ietf.org/html/rfc5766).
In that role `ejabberd` helps clients with ICE
([`RFC 5245`](https://tools.ietf.org/html/rfc5245) or Jingle ICE
([`XEP-0176`](https://xmpp.org/extensions/xep-0176.html) support to
discover their external addresses and ports and to relay media traffic
when it is impossible to establish direct peer-to-peer connection.

The specific configurable options are:

**`tls: true|false`**:   If enabled, `certfile` option must be set, otherwise `ejabberd` will
	not be able to accept TLS connections. Obviously, this option makes
	sense for `tcp` transport only. The default is `false`.

**`certfile: Path`**:   Path to the certificate file. Only makes sense when `tls` is set.

**`use_turn: true|false`**:   Enables/disables TURN (media relay) functionality. The default is
	`false`.

**`turn_blacklist: String | [String,...]`**:   Specify one or more IP addresses and/or subnet
	addresses/masks. The TURN server will refuse to relay traffic from/to
	blacklisted IP addresses. By default, loopback addresses (`127.0.0.0/8`
	and `::1/128`) are blacklisted.

**`turn_ipv4_address: String`**:   The IPv4 address advertised by your TURN server.
	The address should not be NAT’ed or firewalled. There is not default,
	so you should set this option explicitly. Implies `use_turn`.

**`turn_ipv6_address: String`**:   The IPv6 address advertised by your TURN server.
	The address should not be NAT’ed or firewalled. There is not default,
	so you should set this option explicitly. Implies `use_turn`.

**`turn_min_port: Integer`**:   Together with `turn_max_port` forms port range to allocate from. The
	default is 49152. Implies `use_turn`.

**`turn_max_port: Integer`**:   Together with `turn_min_port` forms port range to allocate from. The
	default is 65535. Implies `use_turn`.

**`turn_max_allocations: Integer|infinity`**:   Maximum number of TURN allocations available from the particular IP
	address. The default value is 10. Implies `use_turn`.

**`turn_max_permissions: Integer|infinity`**:   Maximum number of TURN permissions available from the particular IP
	address. The default value is 10. Implies `use_turn`.

**`auth_type: user|anonymous`**:   Which authentication type to use for TURN allocation requests. When
	type `user` is set, ejabberd authentication backend is used. For
	`anonymous` type no authentication is performed (not recommended for
	public services). The default is `user`. Implies `use_turn`.

**`auth_realm: String`**:   When `auth_type` is set to `user` and you have several virtual hosts
	configured you should set this option explicitly to the virtual host
	you want to serve on this particular listening port. Implies
	`use_turn`.

**`shaper: Atom`**:   For `tcp` transports defines shaper to use. The default is `none`.

**`server_name: String`**:   Defines software version to return with every response. The default
	is the STUN library version.

Example configuration with disabled TURN functionality (STUN only):


	listen:
	  ...
	  -
	    port: 3478
	    transport: udp
	    module: ejabberd_stun
	  -
	    port: 3478
	    module: ejabberd_stun
	  -
	    port: 5349
	    module: ejabberd_stun
	    certfile: /etc/ejabberd/server.pem
	  ...

Example configuration with TURN functionality. Note that STUN is always
enabled if TURN is enabled. Here, only UDP section is shown:


	listen:
	  ...
	  -
	    port: 3478
	    transport: udp
	    use_turn: true
	    turn_ipv4_address: 10.20.30.1
	    module: ejabberd_stun
	  ...

You also need to configure DNS SRV records properly so clients can
easily discover a STUN/TURN server serving your XMPP domain. Refer to
section
[`DNS Discovery of a Server`](https://tools.ietf.org/html/rfc5389#section-9)
of [`RFC 5389`](https://tools.ietf.org/html/rfc5389) and section
[`Creating an Allocation`](https://tools.ietf.org/html/rfc5766#section-6)
of [`RFC 5766`](https://tools.ietf.org/html/rfc5766) for details.

Example DNS SRV configuration for STUN only:

	_stun._udp   IN SRV  0 0 3478 stun.example.com.
	_stun._tcp   IN SRV  0 0 3478 stun.example.com.
	_stuns._tcp  IN SRV  0 0 5349 stun.example.com.

And you should also add these in the case if TURN is enabled:

	_turn._udp   IN SRV  0 0 3478 turn.example.com.
	_turn._tcp   IN SRV  0 0 3478 turn.example.com.
	_turns._tcp  IN SRV  0 0 5349 turn.example.com.

# ejabberd_sip

## SIP Configuration

`ejabberd` has built-in SIP support. To activate this feature,
add the [`ejabberd_sip`](#ejabberd_sip) listen module, enable
[`mod_sip`](modules.md#mod_sip) module
for the desired virtual host, and configure DNS properly.

To add a listener you should configure `ejabberd_sip` listening module
as described in [Listen](#listen-option) section.
If option [`tls`](listen-options.md#tls) is specified,
option [`certfile`](listen-options.md#certfile)
must be specified as well, otherwise incoming TLS connections
would fail.

Example configuration with standard ports (as per
[`RFC 3261`](https://tools.ietf.org/html/rfc3261)):


	listen:
	  ...
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
	  ...

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

	example.com IN NAPTR 10  0 "s" "SIPS+D2T" "" _sips._tcp.example.com.
	example.com IN NAPTR 20  0 "s" "SIP+D2T" "" _sip._tcp.example.com.
	example.com IN NAPTR 30  0 "s" "SIP+D2U" "" _sip._udp.example.com.

Example configuration of SRV records with standard ports (as per
[`RFC 3261`](https://tools.ietf.org/html/rfc3261):

	_sip._udp   IN SRV  0 0 5060 sip.example.com.
	_sip._tcp   IN SRV  0 0 5060 sip.example.com.
	_sips._tcp  IN SRV  0 0 5061 sip.example.com.

## Note on SIP usage

SIP authentication does not support SCRAM. As such, it is not possible
to use `mod_sip` to authenticate when ejabberd has been set to encrypt
password with SCRAM.

# ejabberd_http_ws

This module enables XMPP communication over Websocket connection as
described in [`RFC 7395`](https://tools.ietf.org/html/rfc7395).

## Enabling Websocket support

To enable this module it must have handler added to `request_handlers`
section of `ejabberd_http` listener:

	listen:
	  ...
	  -
	    port: 5280
	    module: ejabberd_http
	    request_handlers:
	      ...
	      /xmpp: ejabberd_http_ws
	      ...
	  ...

This module can be configured by using those options that should be
placed in general section of config file: [websocket\_origin](toplevel.md#websocket_origin), [websocket\_ping\_interval](toplevel.md#websocket_ping_interval), [websocket\_timeout](toplevel.md#websocket_timeout).

## Discovery

You also need to configure DNS SRV records properly so clients can
easily discover Websocket service for your XMPP domain. Refer to
[XEP-0156](https://xmpp.org/extensions/xep-0156.html).

Example DNS TXT configuration for Websocket:

    _xmppconnect IN TXT "[ _xmpp-client-websocket=wss://web.example.com:443/ws ]"

## Testing Websocket

A test client can be found on Github: [Websocket test client](https://github.com/processone/xmpp-websocket-client)

<!-- TODO We should probably embed a test Websocket client on the Websocket info get page. -->

# ejabberd_xmlrpc

Handles XML-RPC requests to execute
[ejabberd commands](../../admin/guide/managing.md#ejabberd_commands).
It is configured as a request handler in
[ejabberd_http](listen.md#ejabberd_http).

By default there is no restriction to who can execute what commands,
so it is strongly recommended that you configure restrictions using
[API Permissions](../../developer/ejabberd-api/permissions.md).

This is the minimum configuration required to enable the feature:

    listen:
      -
        port: 4560
        module: ejabberd_http
        request_handlers:
          /: ejabberd_xmlrpc

Example Python script:

    import xmlrpclib
    server = xmlrpclib.Server('http://127.0.0.1:4560/');
    params = {}
    params["host"] = "localhost"
    print server.registered_users(params)

This example configuration adds some restrictions:

    listen:
      -
        port: 5281
        ip: "::"
        module: ejabberd_http
        request_handlers:
          /api: mod_http_api
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

With that configuration, it is possible to execute two specific commands using
`ejabberd_xmlrpc`, with two access restrictions. Example Python script:

    import xmlrpclib
    server = xmlrpclib.Server('http://127.0.0.1:5281/xmlrpc')
    LOGIN = {'user':'user1',
             'server':'localhost',
             'password':'mypass11',
             'admin':True}
    def calling(command, data):
        fn = getattr(server, command)
        return fn(LOGIN, data)
    print calling('registered_users', {'host':'localhost'})

It's possible to use OAuth for authentication instead of plain password, see
[OAuth Support](../../developer/ejabberd-api/oauth.md).

In ejabberd 20.03 and older, it was possible to configure `ejabberd_xmlrpc` as a
listener, see the old document for reference and example configuration:
[Listening Module](../old.md#listening_module).

Just for reference, there's also the old
[`ejabberd_xmlrpc documentation`](https://ejabberd.im/ejabberd_xmlrpc).

# Examples

For example, the following simple configuration defines:

-   There are three domains. The default certificate file is
	`server.pem`. However, the c2s and s2s connections to the domain
	`example.com` use the file `example_com.pem`.

-   Port 5222 listens for c2s connections with STARTTLS, and also allows
	plain connections for old clients.

-   Port 5223 listens for c2s connections with the old SSL.

-   Port 5269 listens for s2s connections with STARTTLS. The socket is
	set for IPv6 instead of IPv4.

-   Port 3478 listens for STUN requests over UDP.

-   Port 5280 listens for HTTP requests, and serves the HTTP-Bind (BOSH)
	service.

-   Port 5281 listens for HTTP requests, using HTTPS to serve HTTP-Bind
	(BOSH) and the Web Admin as explained in [Managing: Web Admin](../../admin/guide/managing.md#web_admin). The
	socket only listens connections to the IP address 127.0.0.1.


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
			    port: 3478
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


In this example, the following configuration defines that:

-   c2s connections are listened for on port 5222 (all IPv4 addresses)
	and on port 5223 (SSL, IP 192.168.0.1 and fdca:8ab6:a243:75ef::1)
	and denied for the user called ‘`bad`’.

-   s2s connections are listened for on port 5269 (all IPv4 addresses)
	with STARTTLS for secured traffic strictly required, and the
	certificates are verified. Incoming and outgoing connections of
	remote XMPP servers are denied, only two servers can connect:
	“jabber.example.org” and “example.com”.

-   Port 5280 is serving the Web Admin and the HTTP-Bind (BOSH) service in
	all the IPv4 addresses. Note that it is also possible to serve them
	on different ports. The second example in section [Managing: Web Admin](../../admin/guide/managing.md#web_admin) shows
	how exactly this can be done. A request handler to serve MQTT over Websocket is also defined.

-   All users except for the administrators have a traffic of limit
	1,000Bytes/second

-   The [`AIM transport`](https://ejabberd.im/pyaimt)
	`aim.example.org` is connected to port 5233 on localhost IP
	addresses (127.0.0.1 and ::1) with password ‘`aimsecret`’.

-   The ICQ transport JIT (`icq.example.org` and `sms.example.org`) is
	connected to port 5234 with password ‘`jitsecret`’.

-   The [`MSN transport`](https://ejabberd.im/pymsnt)
	`msn.example.org` is connected to port 5235 with password
	‘`msnsecret`’.

-   The [`Yahoo! transport`](https://ejabberd.im/yahoo-transport-2)
	`yahoo.example.org` is connected to port 5236 with password
	‘`yahoosecret`’.

-   The
	[`Gadu-Gadu transport`](https://ejabberd.im/jabber-gg-transport)
	`gg.example.org` is connected to port 5237 with password
	‘`ggsecret`’.

-   The [`Jabber Mail Component`](https://ejabberd.im/jmc)
	`jmc.example.org` is connected to port 5238 with password
	‘`jmcsecret`’.

-   The service custom has enabled the special option to avoiding
	checking the `from` attribute in the packets send by this component.
	The component can send packets in behalf of any users from the
	server, or even on behalf of any server.


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


Note, that for services based in jabberd14 or WPJabber you have to make
the transports log and do XDB by themselves:


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

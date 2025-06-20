---
search:
  exclude: true
---

# Configuring ejabberd 20.03 or older

Here are the main entry points to learn more about ejabberd configuration.
ejabberd is extremely powerful and can be configured in many ways with many options.

Do not let this complexity scare you. Most of you will be fine with default config file (or light changes).

## Config File Formatting

### Yaml Configuration File Format

The configuration file will be loaded the first time you start
`ejabberd`. The configuration file name MUST have “.yml” or “.yaml”
extension. This helps ejabberd to differentiate between the new and
legacy file formats (see
section [Legacy Configuration File](#legacy-configuration-file)).

The configuration file is written in [`YAML`][1].

Note that `ejabberd` never edits the configuration file. If you are
changing parameter from web admin interface, you will need to apply
them to configuration file manually. This is to prevent messing up
with your config file comments, syntax, etc.

Please, consult `ejabberd.log` for configuration errors. `ejabberd` will
report syntax related errors, as well as complains about unknown options
and invalid values. Make sure you respect indentation (YAML is
sensitive to this) or you will get pretty cryptic errors.

### Legacy Configuration File

In previous `ejabberd` version the configuration file should be
written in Erlang terms. The format is still supported, but it is
highly recommended to convert it to the new YAML format using
`convert_to_yaml` command from `ejabberdctl` (see
[ejabberdctl](../admin/guide/managing.md#ejabberdctl)
and [List of ejabberd Commands](../admin/guide/managing.md#list_of_ejabberd_commands)
for details).

If you want to specify some options using the old Erlang format, you
can set them in an additional cfg file, and include it using the
`include_config_file` option, see
[Include Additional Configuration Files](#include-additional-configuration-files)
for the option description and a related example in
[Restrict Execution with AccessCommands](../admin/guide/managing.md#restrict_execution_with_accesscommands).

## Configuring One or Several XMPP Domains

### Host Names

`ejabberd` supports managing several independent XMPP domains on a
single ejabberd instance, using a feature called virtual hosting.

The option `hosts` defines a list containing one or more domains that
`ejabberd` will serve.

Of course, the `hosts` list can contain just one domain if you do not
want to host multiple XMPP domains on the same instance.

The syntax is: `[HostName1, Hostname2]`

Examples:

- Serving one domain:

``` yaml
hosts: [example.org]
```

- Serving three domains:

``` yaml
  hosts:
    - example.net
    - example.com
    - jabber.somesite.org
```

### Virtual Hosting

When managing several XMPP domains in a single instance, those domains are truly independent.
It means they can even have different configuration parameters.

Options can be defined separately for every virtual host using the `host_config` option.

The syntax is: `{HostName: [Option, ...]}`

Examples:

- Domain `example.net` is using the internal authentication method
 while domain `example.com` is using the LDAP server running on the
 domain `localhost` to perform authentication:

``` yaml
  host_config:
    example.net:
      auth_method: internal
    example.com:
      auth_method: ldap
      ldap_servers:
        - localhost
      ldap_uids:
        - uid
      ldap_rootdn: "dc=localdomain"
      ldap_rootdn: "dc=example,dc=com"
      ldap_password: ""
```

- Domain `example.net` is using SQL to perform authentication while
 domain `example.com` is using the LDAP servers running on the
 domains `localhost` and `otherhost`:

  host_config:
    example.net:
      auth_method: sql
      sql_type: odbc
      sql_server: "DSN=ejabberd;UID=ejabberd;PWD=ejabberd"
    example.com:
      auth_method: ldap
      ldap_servers:
        - localhost
        - otherhost
      ldap_uids:
        - uid
      ldap_rootdn: "dc=localdomain"
      ldap_rootdn: "dc=example,dc=com"
      ldap_password: ""

To define specific ejabberd modules in a virtual host, you can define
the global `modules` option with the common modules, and later add
specific modules to certain virtual hosts. To accomplish that, instead
of defining each option in `host_config` use `append_host_config` with
the same syntax.

In this example three virtual hosts have some similar modules, but there
are also other different modules for some specific virtual hosts:

``` yaml
 ## This ejabberd server has three vhosts:
 hosts:
   - one.example.org
   - two.example.org
   - three.example.org

 ## Configuration of modules that are common to all vhosts
 modules:
   mod_roster:    {}
   mod_configure: {}
   mod_disco:     {}
   mod_private:   {}
   mod_time:      {}
   mod_last:      {}
   mod_version:   {}

 ## Add some modules to vhost one:
 append_host_config:
   one.example.org:
     modules:
       mod_muc:
         host: conference.one.example.org
       mod_ping: {}

 ## Add a module just to vhost two:
 append_host_config:
   two.example.org:
     modules:
       mod_muc:
         host: conference.two.example.org
```

## Basic Configuration

### Logging

`ejabberd` configuration can help a lot by having the right amount of logging set up.

Here is a few general option to configure logging:

**`loglevel: Number`**: Verbosity of log files generated by ejabberd. Available levels are.

- 0: No ejabberd log at all (not recommended)
- 1: Critical
- 2: Error
- 3: Warning
- 4: Info
- 5: Debug

**`log_rotate: size, date and count`**: The *rotation* parameters describe how to rotate logs. Either size and/or date can trigger
    log rotation. Setting count to N keeps N rotated logs. Setting count to 0
    does not disable rotation, it instead rotates the file and keeps no previous
    versions around. Setting size to X rotate log when it reaches X bytes.
    To disable rotation set the size to 0 and the date to ""
    Date syntax is taken from the syntax newsyslog uses in newsyslog.conf.

Here are some examples:

- $D0     rotate every night at midnight
- $D23    rotate every day at 23:00 hr
- $W0D23  rotate every week on Sunday at 23:00 hr
- $W5D16  rotate every week on Friday at 16:00 hr
- $M1D0   rotate on the first day of every month at midnight
- $M5D6   rotate on every 5th day of the month at 6:00 hr

The values in default configuration file are:

``` yaml
    log_rotate_size: 0
    log_rotate_date: "$D0"
    log_rotate_count: 1
```

**`log_rate_limit: Number`**: This option is used for overload protection: If you want to limit
    the number of messages per second allowed from error_logger, which is
    a good idea if you want to avoid a flood of messages when system is
    overloaded, you can set a limit. Default value is 100.

For example:

``` yaml
    log_rate_limit: 100
```

**`hide_sensitive_log_data: Boolean`**: We have a privacy option to ensure we do not log IP address or
    sensitive data. Default value is false for backward compatibility.

For example:

``` yaml
    hide_sensitive_log_data: false
```

### Listening Ports

The option `listen` defines for which ports, addresses and network protocols `ejabberd` will listen and what services will be run on them.
Each element of the list is an associative array with the following elements:

- Port number. Optionally also the IP address and/or a transport
 protocol.

- Listening module that serves this port.

- Options for the TCP socket and for the listening module.

The option syntax is:

**`[Listener, ...]`**:

Example:

``` yaml
 listen:
   -
     port: 5222
     module: ejabberd_c2s
     starttls: true
   -
     port: 5269
     module: ejabberd_s2s_in
     transport: tcp
```

#### Port Number, IP Address and Transport Protocol

The port number defines which port to listen for incoming connections.
It can be a Jabber/XMPP standard port or any other valid port number.

The IP address can be represented as a string. The socket will listen only in that network interface.
It is possible to specify a generic address ("0.0.0.0" for IPv4 or "::" for IPv6), so `ejabberd` will listen in all addresses.
Depending on the type of the IP address, IPv4 or IPv6 will be used.
When the IP address is not specified, it will listen on all IPv4 network addresses.

Note that on some operating systems and/or OS configurations, listening on "::" will mean listening for IPv4 traffic as well as IPv6 traffic.

Some example values for IP address:

- `"0.0.0.0"` to listen in all IPv4 network interfaces. This is the
 default value when no IP is specified.

- `"::"` to listen in all IPv6 network interfaces

- `"10.11.12.13"` is the IPv4 address `10.11.12.13`

- `"::FFFF:127.0.0.1"` is the IPv6 address `::FFFF:127.0.0.1/128`

The transport protocol can be `tcp` or `udp`. Default is `tcp`.

#### Listening Module

The available modules, their purpose and the options allowed by each one
are:

**`ejabberd_c2s`**:   Handles c2s connections.
    Options: `access`, `ciphers`, `dhfile`, `protocol_options`,
 `max_fsm_queue`, `max_stanza_size`, `shaper`,
 `starttls`, `starttls_required`, `tls`, `zlib`,
 `tls_compression`

**`ejabberd_s2s_in`**:   Handles incoming s2s connections.
    Options: `max_stanza_size`, `shaper`, `tls_compression`

**`ejabberd_service`**:   Interacts with an
 [`external component`](https://ejabberd.im/tutorials-transports)
 (as defined in the Jabber Component Protocol
 ([`XEP-0114`](https://xmpp.org/extensions/xep-0114.html)).
 Options: `access`, `hosts`, `max_fsm_queue`, `password`, `check_from`,
 `shaper_rule`

**`ejabberd_sip`**:   Handles SIP requests as defined in
 [`RFC 3261`](https://tools.ietf.org/html/rfc3261).
 Options: `certfile`, `tls`

**`ejabberd_stun`**:   Handles STUN/TURN requests as defined in
 [`RFC 5389`](https://tools.ietf.org/html/rfc5389) and
 [`RFC 5766`](https://tools.ietf.org/html/rfc5766).
 Options: `certfile`, `tls`, `use_turn`, `turn_ip`,
 `turn_port_range`, `turn_max_allocations`, `turn_max_permissions`,
 `shaper`, `server_name`, `auth_realm`, `auth_type`

**`ejabberd_http`**:   Handles incoming HTTP connections. This module is responsible for serving Web Admin, but also XMPP BOSH and Websocket with proper request handler configured.
 Options: `default_host`, `dhfile`,
 `request_handlers`, `tls`, `tls_compression`,
 `trusted_proxies` (global option)

**`ejabberd_xmlrpc`**:   Handles XML-RPC requests to execute
 [ejabberd commands](../admin/guide/managing.md#ejabberd_commands).
 Options: `access_commands`, `maxsessions`, `timeout`.
 For explanations about `access_commands` see
 [Restrict Execution with AccessCommands](../admin/guide/managing.md#restrict_execution_with_accesscommands).
 Check some [XML-RPC examples](../developer/ejabberd-api/oauth.md#xml_rpc_examples).
 You can find more information in the old
 [`ejabberd_xmlrpc documentation`](https://ejabberd.im/ejabberd_xmlrpc).

#### Options

This is a detailed description of each option allowed by the listening
modules:

**`access: AccessName`**:   This option defines access to the port. The default value is `all`.

**`backlog: Value`**:   The backlog value defines the maximum length that the queue of
 pending connections may grow to. This should be increased if the
 server is going to handle lots of new incoming connections as they
 may be dropped if there is no space in the queue (and ejabberd was
 not able to accept them immediately). Default value is 5.

**`ciphers: Ciphers`**:   OpenSSL ciphers list in the same format accepted by
 ‘`openssl ciphers`’ command.

**`protocol_options: ProtocolOpts`**:   List of general options relating to SSL/TLS. These map to
 `OpenSSL’s set_options()`.
 The default entry is: `"no_sslv3|cipher_server_preference|no_compression"`

**`default_host: undefined|HostName`**:   If the HTTP request received by ejabberd contains the HTTP header
 `Host` with an ambiguous virtual host that doesn’t match any one
 defined in ejabberd (see [Host Names](#host-names)), then this configured HostName
 is set as the request Host. The default value of this option is:
 `undefined`.

**`dhfile: Path`**:   Full path to a file containing custom parameters for Diffie-Hellman key
 exchange. Such a file could be created with the command
 `openssl dhparam -out dh.pem 2048`. If this option is not specified,
 default parameters will be used, which might not provide the same level
 of security as using custom parameters.

**`hosts: {Hostname: [HostOption, ...]}`**:   The external Jabber component that connects to this
 `ejabberd_service` can serve one or more hostnames. As `HostOption`
 you can define options for the component; currently the only allowed
 option is the password required to the component when attempt to
 connect to ejabberd: `password: Secret`. Note that you
 cannot define in a single `ejabberd_service` components of different
 services: add an `ejabberd_service` for each service, as seen in an
 example below. This option may not be necessary if the component
 already provides the host in its packets; in that case, you can simply
 provide the password option that will be used for all the hosts
 (see port 5236 definition in the example below).

Remember that you must also install and enable the module `mod\_bosh`, and the `ejabberd_c2s` listener must be available too.

If HTTP Bind (BOSH) is enabled, it will be available at `http://server:port/bosh/`.
Be aware that support for HTTP Bind is also needed in the XMPP client.
Remark also that HTTP Bind can be interesting to host a web-based XMPP client such as [`JWChat`](https://jwchat.org) (check the tutorials to install JWChat with ejabberd and an [`embedded local web server`](https://ejabberd.im/jwchat-localserver) or [`Apache`](https://ejabberd.im/jwchat-apache)).

**`max_fsm_queue: Size`**:   This option specifies the maximum number of elements in the queue of
 the FSM (Finite State Machine). Roughly speaking, each message in
 such queues represents one XML stanza queued to be sent into its
 relevant outgoing stream. If queue size reaches the limit (because,
 for example, the receiver of stanzas is too slow), the FSM and the
 corresponding connection (if any) will be terminated and error
 message will be logged. The reasonable value for this option depends
 on your hardware configuration. This option can be specified for
 `ejabberd_service` and `ejabberd_c2s` listeners, or also globally
 for `ejabberd_s2s_out`. If the option is not specified for
 `ejabberd_service` or `ejabberd_c2s` listeners, the globally
 configured value is used. The allowed values are integers and
 ’undefined’. Default value: ’10000’.

**`max_stanza_size: Size`**:   This option specifies an approximate maximum size in bytes of XML
 stanzas. Approximate, because it is calculated with the precision of
 one block of read data. For example `{max_stanza_size, 65536}`. The
 default value is `infinity`. Recommended values are 65536 for c2s
 connections and 131072 for s2s connections. s2s max stanza size must
 always much higher than c2s limit. Change this value with extreme
 care as it can cause unwanted disconnect if set too low.

**`password: Secret`**:   Specify the password to verify an external component that connects to the port.

**`request_handlers: {Path: Module}`**:   To define one or several handlers that will serve HTTP requests. The
 Path is a string; so the URIs that start with that Path will be
 served by Module. For example, if you want `mod_foo` to serve the
 URIs that start with `/a/b/`, and you also want `mod_bosh` to
 serve the URIs `/bosh/`, use this option:
 ``` yaml
   request_handlers:
   /a/b: mod_foo
   /bosh: mod_bosh
   /mqtt: mod_mqtt
 ```

**`check_from: true|false`**:   This option can be used with `ejabberd_service` only.
 [`XEP-0114`](https://xmpp.org/extensions/xep-0114.html) requires that
 the domain must match the hostname of the component. If this option
 is set to `false`, `ejabberd` will allow the component to send
 stanzas with any arbitrary domain in the ’from’ attribute. Only use
 this option if you are completely sure about it. The default value
 is `true`, to be compliant with
 [`XEP-0114`](https://xmpp.org/extensions/xep-0114.html).

**`shaper: none|ShaperName`**:   This option defines a shaper for the port (see section [Shapers](#shapers)).
 The default value is `none`.

**`shaper_rule: none|ShaperRule`**:   This option defines a shaper rule for the `ejabberd_service` (see
 section [Shapers](#shapers)). The recommended value is `fast`.

**`starttls: true|false`**:   This option specifies that STARTTLS encryption is available on
 connections to the port. You should also set the `certfiles` option or configure [ACME](#acme).

**`starttls_required: true|false`**:   This option specifies that STARTTLS encryption is required on
 connections to the port. No unencrypted connections will be allowed.
 You should also set the `certfiles` option or configure [ACME](#acme).

**`timeout: Integer`**:   Timeout of the connections, expressed in milliseconds. Default: 5000

**`tls: true|false`**:   This option specifies that traffic on the port will be encrypted
 using SSL immediately after connecting. This was the traditional
 encryption method in the early Jabber software, commonly on port
 5223 for client-to-server communications. But this method is
 nowadays deprecated and not recommended. The preferable encryption
 method is STARTTLS on port 5222, as defined
 [`RFC 6120: XMPP Core`](https://xmpp.org/rfcs/rfc6120.html#tls),
 which can be enabled in `ejabberd` with the option `starttls`. If
 this option is set, you should also set the `certfiles` option or configure [ACME](#acme).
 The option `tls` can also be used in `ejabberd_http` to support HTTPS.

**`tls_compression: true|false`**:   Whether to enable or disable TLS compression. The default value is
 `false`.

**`use_proxy_protocol: true|false`**:   Is this listener accessed by proxy service that is using
    proxy protocol for supplying real IP addresses to ejabberd server. You can read about this protocol
    in [Proxy protocol specification](http://www.haproxy.org/download/1.8/doc/proxy-protocol.txt).
    The default value of this option is`false`.

**`zlib: true|false`**:   This option specifies that Zlib stream compression (as defined in
 [`XEP-0138`](https://xmpp.org/extensions/xep-0138.html)) is available
 on connections to the port.

#### Global Options

There are some additional global options that can be specified in the ejabberd configuration file (outside `listen`):

**`acme`**: Automated SSL certificate management. See section [ACME](#acme).

**`certfiles: List of paths`**: The option accepts a list of file
  paths (optionally with wildcards) containing either PEM certificates
  or PEM private keys. At startup, ejabberd sorts the certificates,
  finds matching private keys and rebuilds full certificates
  chains. Use this option when enabling options like `starttls` or
  `tls` in listeners `ejabberd_c2s`, `ejabberd_s2s` or
  `ejabberd_http`.

**`c2s_cafile: Path`**: Full path to a file containing one or more CA
  certificates in PEM format.  All client certificates should be
  signed by one of these root CA certificates and should contain the
  corresponding JID(s) in `subjectAltName` field.

**`c2s_ciphers: Ciphers`**: OpenSSL ciphers list in the same format
  accepted by ‘`openssl ciphers`’ command.

**`c2s_dhfile: Path`**: Full path to a file containing custom DH
  parameters. Such a file could be created with the command `openssl
  dhparam -out dh.pem 2048`. If this option is not specified, default
  parameters will be used, which might not provide the same level of
  security as using custom parameters.

**`c2s_protocol_options: ProtocolOpts`**: List of general options relating to SSL/TLS. These map to `OpenSSL’s set_options()`.
  The default entry is: `"no_sslv3|cipher_server_preference|no_compression"`

**`c2s_tls_compression: true|false`**: Whether to enable or disable
  TLS compression for c2s connections.  The default value is `false`.

**`s2s_use_starttls: false|optional|required|required_trusted`**:   This option defines if s2s connections don’t use STARTTLS
 encryption; if STARTTLS can be used optionally; if STARTTLS is
 required to establish the connection; or if STARTTLS is required and
 the remote certificate must be valid and trusted. Do note that
 `required_trusted` is deprecated and will be unsupported in future
 releases. Instead, set it to `required` and make sure `mod_s2s_dialback`
 is *NOT* loaded. The default value is to not use STARTTLS: `false`.

**`s2s_dhfile: Path`**:   Full path to a file containing custom DH parameters. Such a file could be
 created with the command `openssl dhparam -out dh.pem 2048`. If this
 option is not specified, default parameters will be used, which might
 not provide the same level of security as using custom parameters.

**`s2s_ciphers: Ciphers`**:   OpenSSL ciphers list in the same format accepted by
 ‘`openssl ciphers`’ command.

**`s2s_protocol_options: ProtocolOpts`**:   List of general options relating to SSL/TLS. These map to
 `OpenSSL’s set_options()`.
 The default entry is: `"no_sslv3|cipher_server_preference|no_compression"`

**`outgoing_s2s_families: [Family, ...]`**:   Specify which address families to try, in what order. By default it
 first tries connecting with IPv4, if that fails it tries using IPv6.

**`outgoing_s2s_timeout: Timeout`**:   The timeout in seconds for outgoing S2S connection attempts.

**`s2s_access: Access`**:   The policy for incoming and outgoing s2s connections to other XMPP
 servers. The default value is `all`. You can disable connections to other servers (federation)
 here by setting it to `none`, but this will not block other locally hosted servers as, for performance
 reasons, they are connected internally and not via the s2s module. To block even those you might
 need to use an additional module, for example [`mod_isolation`](https://github.com/processone/ejabberd-contrib/tree/master/mod_isolation).

**`s2s_dns_timeout: Timeout`**:   The timeout in seconds for DNS resolving. The default value is `10`.

**`s2s_dns_retries: Number`**:   DNS resolving retries in seconds. The default value is `2`.

**`s2s_max_retry_delay: Seconds`**:   The maximum allowed delay for retry to connect after a failed
 connection attempt. Specified in seconds. The default value is 300
 seconds (5 minutes).

**`s2s_tls_compression: true|false`**:   Whether to enable or disable TLS compression for s2s connections.
 The default value is `false`.

**`trusted_proxies: all | [IpString]`**:   Specify what proxies are trusted when an HTTP request contains the
 header `X-Forwarded-For` You can specify `all` to allow all proxies,
 or specify a list of IPs in string format. The default value is:
 `["127.0.0.1"]`. This allows, if enabled, to be able to know the real IP of the request, for admin purpose,
 or security configuration (for example using [mod_fail2ban](#mod_fail2ban))).
 **Important:** The proxy MUST be configured to set the `X-Forwarded-For` header if you enable this option as,
 otherwise, the client can set it itself and as a result the IP value cannot be trusted for security rules in
 ejabberd.

### Examples

For example, the following simple configuration defines:

- There are three domains. The default certificate file is `server.pem`. However, the c2s and s2s connections to the domain
 `example.com` use the file `example_com.pem`.

- Port 5222 listens for c2s connections with STARTTLS, and also allows plain connections for old clients.

- Port 5223 listens for c2s connections with the old SSL.

- Port 5269 listens for s2s connections with STARTTLS. The socket is set for IPv6 instead of IPv4.

- Port 3478 listens for STUN requests over UDP.

- Port 5280 listens for HTTP requests, and serves the HTTP-Bind (BOSH)  service.

- Port 5281 listens for HTTP requests, using HTTPS to serve HTTP-Bind (BOSH) and the Web Admin as explained in [Managing: Web Admin](../admin/guide/managing.md#web_admin).
The socket only listens connections to the IP address 127.0.0.1.

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
 on different ports. The second example in section [Managing: Web Admin](../admin/guide/managing.md#web_admin) shows
 how exactly this can be done. A request handler to serve MQTT over Websocket is also defined.

- All users except for the administrators have a traffic of limit
 1,000Bytes/second

- The [`AIM transport`][2]
 `aim.example.org` is connected to port 5233 on localhost IP
 addresses (127.0.0.1 and ::1) with password ‘`aimsecret`’.

- The ICQ transport JIT (`icq.example.org` and `sms.example.org`) is
 connected to port 5234 with password ‘`jitsecret`’.

- The [`MSN transport`][3]
 `msn.example.org` is connected to port 5235 with password
 ‘`msnsecret`’.

- The [`Yahoo! transport`][4]
 `yahoo.example.org` is connected to port 5236 with password
 ‘`yahoosecret`’.

- The
 [`Gadu-Gadu transport`][5]
 `gg.example.org` is connected to port 5237 with password
 ‘`ggsecret`’.

- The [`Jabber Mail Component`][6]
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

### ACME

[ACME](https://tools.ietf.org/html/rfc8555) is used to automatically obtain SSL certificates for the domains served by ejabberd, which means that certificate requests and renewals are performed to some CA server (aka "ACME server")
in a fully automated mode. The automated mode is enabled by default.
However, since ACME requires HTTP challenges (i.e. an ACME server will connect to ejabberd server on HTTP port 80 during certificate issuance), some configuration of ejabberd is still required. Namely, an HTTP listener for `ejabberd_http` module should be configured on non-TLS port with so called "ACME well known" request handler:

``` yaml
    listen:
      ...
      -
        module: ejabberd_http
        port: 5280
        request_handlers:
          /.well-known/acme-challenge: ejabberd_acme
          ...
      ...
```

Note that the ACME protocol **requires** challenges to be sent on port 80. Since this is a privileged
port, ejabberd cannot listen on it directly without root privileges. Thus you need some mechanism
to forward port 80 to the port defined by the listener (port 5280 in the example above). There are
several ways to do this: using NAT, setcap (Linux only), or HTTP front-ends (e.g. `sslh`, `nginx`,
`haproxy` and so on). Pick one that fits your installation the best, but **DON'T** run ejabberd as root.

If you see errors in the logs with ACME server problem reports, it's **highly** recommended to change `ca_url`
option of section `acme` to the URL pointing to some staging ACME environment, fix the problems until you obtain
a certificate, and then change the URL back and retry using `request-certificate` ejabberdctl command
(see below). This is needed because ACME servers typically have rate limits, preventing you from requesting
certificates too rapidly and you can get stuck for several hours or even days.
By default, ejabberd uses [Let's Encrypt](https://letsencrypt.org) authority.
Thus, the default value of `ca_url` option is `https://acme-v02.api.letsencrypt.org/directory`
and the staging URL will be `https://acme-staging-v02.api.letsencrypt.org/directory`:

``` yaml
    acme:
      ## Staging environment
      ca_url: https://acme-staging-v02.api.letsencrypt.org/directory
      ## Production environment (the default):
      # ca_url: https://acme-v02.api.letsencrypt.org/directory
```

The automated mode can be disabled by setting `auto` option of section `acme` to `false`:

``` yaml
    acme:
      auto: false
      ...
```

In this case automated renewals are still enabled, however, in order to request a new certificate,
you need to run `request-certificate` ejabberdctl command:

    `$ ejabberdctl request-certificate all`

If you only want to request certificates for a subset of the domains, run:

    `$ ejabberdctl request-certificate domain.tld,pubsub.domain.tld,server.com,conference.server.com,...`

You can view the certificates obtained using ACME:

``` sh
    $ ejabberdctl list-certificates
    domain.tld /path/to/cert/file1 true
    server.com /path/to/cert/file2 false
    ...
```

The output is mostly self-explained: every line contains the domain, the corresponding certificate file,
and whether this certificate file is used or not. A certificate might not be used for several reasons:
mostly because ejabberd detects a better certificate (i.e. not expired, or having a longer lifetime).
It's recommended to revoke unused certificates if they are not yet expired (see below).

At any point you can revoke a certificate: pick the certificate file from the listing above and run:

`$ ejabberdctl revoke-certificate /path/to/cert/file`

If the commands return errors, consult the log files for details.

### Available ACME options

**`ca_url`**: The ACME directory URL used as an entry point for the ACME server.
   The default value is `https://acme-v02.api.letsencrypt.org/directory` - the
   directory URL of [Let's Encrypt](https://letsencrypt.org) authority.

**`contact`**: A list of contact addresses (typically emails) where an ACME server
   will send notifications when problems occur. The default is an empty list which
   means an ACME server will send no notices.

**`auto`**: Whether to automatically request certificates for all configured domains
   (that yet have no a certificate) on server start or configuration reload.
   The default is `true`.

**`cert_type`**: A type of a certificate key. Available values are `ec` and `rsa`
   for EC and RSA certificates respectively. It's better to have RSA certificates
   for the purpose of backward compatibility with legacy clients and servers,
   thus the default is `rsa`.

Example:

``` yaml
    acme:
      ca_url: https://acme-v02.api.letsencrypt.org/directory
      contact:
        - mailto:admin@domain.tld
        - mailto:bot@domain.tld
      auto: true
      cert_type: rsa

```

#### ACME implementation details

In nutshell, certification requests are performed in two phases. Firstly, ejabberd
creates an account at the ACME server. That is an EC private key. Secondly,
a certificate is requested. In the case of a revocation, no account is
used - only a cerificate in question is needed. All information is stored under
`acme` directory inside spool directory of ejabberd (typically `/var/lib/ejabberd`).
An example content of the directory is the following:

``` sh
$ tree /var/lib/ejabberd
/var/lib/ejabberd
├── acme
│   ├── account.key
│   └── live
│       ├── 251ce180d964e98a2f18b65504df2ab7c55943e2
│       └── 93816a8429ebbaa75574eb3f59d4a806b67d6917
...
```

Here, `account.key` is the EC private key used to identify the ACME account.
You can inspect its content using `openssl` command:

``` sh
openssl ec -text -noout -in /var/lib/ejabberd/acme/account.key
```

Obtained certificates are stored under `acme/live` directory.
You can inspect any of the certificates using `openssl` command as well:

``` sh
openssl x509 -text -noout -in /var/lib/ejabberd/acme/live/251ce180d964e98a2f18b65504df2ab7c55943e2
```

In the case of errors, you can delete the whole `acme` directory - ejabberd
will recreate its content on next certification request. However, don't delete it
too frequently - usually there is a rate limit on the number of accounts and
certificates an ACME server creates. In particular, for [Let's Encrypt](https://letsencrypt.org)
the limits are described [here](https://letsencrypt.org/docs/rate-limits).

## Authentication

The option `auth_method` defines the authentication methods that are
used for user authentication. The syntax is:

`[Method, ...]` :

The following authentication methods are supported by `ejabberd`:

- internal — See section [Internal](#internal).

- external — See section [External Script](#external-script).

- ldap — See section  [LDAP](#ldap).

- sql — See section [Relational Databases](#relational-databases).

- anonymous — See section [Anonymous Login and SASL Anonymous](#anonymous-login-and-sasl-anonymous).

- pam — See section [Pam Authentication](#pam-authentication).

When the option is omitted, ejabberd will rely upon the default database which is configured in `default_db` option. If this option is not set neither the default authentication method will be `internal`.

Account creation is only supported by internal, external and sql methods.

The option `resource_conflict` defines the action when a client attempts
to login to an account with a resource that is already connected. The
option syntax is:

**`resource_conflict: setresource|closenew|closeold`**:  The possible values match exactly the three possibilities described in
[`XMPP Core: section 7.7.2.2`][7].
The default value is `closeold`. If the client uses old Jabber Non-SASL
authentication ([`XEP-0078`][8]),
then this option is not respected, and the action performed is
`closeold`.

**`fqdn: undefined|FqdnString|[FqdnString]`**:
Allows you to define the Fully Qualified Domain Name
of the machine, in case it isn't detected automatically. The FQDN is
used to authenticate some clients that use the DIGEST-MD5 SASL
mechanism.

**`disable_sasl_mechanisms: Mechanism|[Mechanism, ...]`**:
Specify a list of SASL mechanisms
(such as "DIGEST-MD5", "SCRAM-SHA1" or "X-OAUTH2") that should *not* be offered to
the client. The mechanisms can be listed as lowercase or uppercase
strings.

### Internal

`ejabberd` uses its internal Mnesia database as the default
authentication method. The value `internal` will enable the internal
authentication method.

The option `auth_password_format: plain|scram` defines in what format
the users passwords are stored:

**`plain`**:   The password is stored as plain text in the database. This is risky
 because the passwords can be read if your database gets compromised.
 This is the default value. This format allows clients to
 authenticate using: the old Jabber Non-SASL
 ([`XEP-0078`](https://xmpp.org/extensions/xep-0078.html)),
 `SASL PLAIN`, `SASL DIGEST-MD5`, and `SASL SCRAM-SHA-1`.

**`scram`**:   The password is not stored, only some information that allows to
 verify the hash provided by the client. It is impossible to obtain
 the original plain password from the stored information; for this
 reason, when this value is configured it cannot be changed to
 `plain` anymore. This format allows clients to authenticate using:
 `SASL PLAIN` and `SASL SCRAM-SHA-1`.

For details about the client-server communication when using SCRAM-SHA-1,
refer to [SASL and SCRAM-SHA-1](https://wiki.xmpp.org/web/SASLandSCRAM-SHA-1).

When you enable SCRAM password format for internal storage, if you try
to authenticate a user that had their password already stored in plain
text, their password will be automatically converted to SCRAM format. It
means database is converted as you use it.

If you want to convert your Mnesia database all at once, you can look
into the Erlang function:
`ejabberd_auth_internal:maybe_scram_passwords/0`.

Examples:

- To use internal authentication on `example.org` and LDAP
 authentication on `example.net`:

``` yaml
  host_config:
    example.org:
      auth_method: [internal]
    example.net:
      auth_method: [ldap]
```

- To use internal authentication with hashed passwords on all virtual
 hosts:

``` yaml
  auth_method: internal
  auth_password_format: scram
```

*Note on SCRAM using and foreign authentication limitations*: when using the SCRAM password format, it is not possible to use foreign authentication method in ejabberd, as the real password is not known,
Foreign authentication are use to authenticate through various bridges ejabberd provide.
Foreign authentication includes at the moment SIP and TURN auth support and they will not be working with SCRAM.

#### External Script

In this authentication method, when `ejabberd` starts, it start a script, and calls it to perform authentication tasks.

The server administrator can write the external authentication script in any language. The details on the interface between ejabberd and the
script are described in the `ejabberd Developers Guide`. There are also [`several example authentication scripts`][9].

These are the specific options:

**`extauth_program: PathToScript`**:   Indicate in this option the full path to the external authentication
 script. The script must be executable by ejabberd.

**`extauth_instances: Integer`**:   Indicate how many instances of the script to run simultaneously to
 serve authentication in the virtual host. The default value is the
 minimum number: 1.

**`auth_use_cache: false|true`**:   Starting in *ejabberd 17.06*, caching has received a complete overhaul.
 Instead of `extauth_cache`, a set of new variables describes cache
 behaviour, and the default value is now `true`. Note that caching
 interferes with the ability to maintain multiple passwords per
 account. So if your authentication mechanism supports
 application-specific passwords, caching must be disabled.
 The options are called `auth_use_cache`, `auth_cache_missed`,
 `auth_cache_size`, `auth_cache_life_time`,
 and you can see details in the section [Caching](#caching).

This example sets external authentication, the extauth script, enables
caching for 10 minutes, and starts three instances of the script for
each virtual host defined in ejabberd:

``` yaml
 auth_method: [external]
 extauth_program: /etc/ejabberd/JabberAuth.class.php
 extauth_instances: 3
 auth_use_cache: false
```

### Anonymous Login and SASL Anonymous

The `anonymous` authentication method enables two modes for anonymous
authentication:

**`Anonymous login`**:   This is a standard login, that use the classical login and password
 mechanisms, but where password is accepted or preconfigured for all
 anonymous users. This login is compliant with SASL authentication,
 password and digest non-SASL authentication, so this option will
 work with almost all XMPP clients

**`SASL Anonymous`**:   This is a special SASL authentication mechanism that allows to login
 without providing username or password (see
 [`XEP-0175`](https://xmpp.org/extensions/xep-0175.html)). The main
 advantage of SASL Anonymous is that the protocol was designed to
 give the user a login. This is useful to avoid in some case, where
 the server has many users already logged or registered and when it
 is hard to find a free username. The main disavantage is that you
 need a client that specifically supports the SASL Anonymous
 protocol.

The anonymous authentication method can be configured with the following
options. Remember that you can use the `host_config` option to set
virtual host specific options (see section [Virtual Hosting](#virtual-hosting)).

**`allow_multiple_connections: false|true`**:   This option is only used when the anonymous mode is enabled. Setting
 it to `true` means that the same username can be taken multiple
 times in anonymous login mode if different resource are used to
 connect. This option is only useful in very special occasions. The
 default value is `false`.

**`anonymous_protocol: login_anon | sasl_anon | both`**:   `login_anon` means that the anonymous login method will be used.
 `sasl_anon` means that the SASL Anonymous method will be used.
 `both` means that SASL Anonymous and login anonymous are both
 enabled.

Those options are defined for each virtual host with the `host_config`
parameter (see section  [Virtual Hosting](#virtual-hosting)).

Examples:

- To enable anonymous login on all virtual hosts:

``` yaml
  auth_method: [anonymous]
  anonymous_protocol: login_anon
```

- Similar as previous example, but limited to `public.example.org`:

``` yaml
  host_config:
    public.example.org:
      auth_method: [anonymous]
      anonymous_protoco: login_anon
```

- To enable anonymous login and internal authentication on a virtual  host:

``` yaml
  host_config:
    public.example.org:
      auth_method:
        - internal
        - anonymous
      anonymous_protocol: login_anon
```

- To enable SASL Anonymous on a virtual host:

``` yaml
  host_config:
    public.example.org:
      auth_method: [anonymous]
      anonymous_protocol: sasl_anon
```

- To enable SASL Anonymous and anonymous login on a virtual host:

``` yaml
  host_config:
    public.example.org:
      auth_method: [anonymous]
      anonymous_protocol: both
```

- To enable SASL Anonymous, anonymous login, and internal authentication on a virtual host:

``` yaml
  host_config:
    public.example.org:
      auth_method:
        - internal
        - anonymous
      anonymous_protocol: both
```

There are more configuration examples and XMPP client example stanzas in [`Anonymous users support`][10].

#### PAM Authentication

`ejabberd` supports authentication via Pluggable Authentication Modules
(PAM). PAM is currently supported in AIX, FreeBSD, HP-UX, Linux, Mac OS
X, NetBSD and Solaris. PAM authentication is disabled by default, so you
have to configure and compile `ejabberd` with PAM support enabled:

 `./configure --enable-pam && make install`

Options:

**`pam_service: Name`**:   This option defines the PAM service name. Default is `ejabberd`.
 Refer to the PAM documentation of your operation system for more
 information.

**`pam_userinfotype: username|jid`**:   This option defines what type of information about the user ejabberd
 provides to the PAM service: only the username, or the user JID.
 Default is `username`.

Example:

``` yaml
 auth_method: [pam]
 pam_service: ejabberd
```

Though it is quite easy to set up PAM support in `ejabberd`, PAM itself
introduces some security issues:

- To perform PAM authentication `ejabberd` uses external C-program
 called `epam`. By default, it is located in
 `/var/lib/ejabberd/priv/bin/` directory. You have to set it root on
 execution in the case when your PAM module requires root privileges
 (`pam_unix.so` for example). Also you have to grant access for
 `ejabberd` to this file and remove all other permissions from it.
 Execute with root privileges:

``` sh
  chown root:ejabberd /var/lib/ejabberd/priv/bin/epam
  chmod 4750 /var/lib/ejabberd/priv/bin/epam
```

- Make sure you have the latest version of PAM installed on your
 system. Some old versions of PAM modules cause memory leaks. If you
 are not able to use the latest version, you can `kill(1)` `epam`
 process periodically to reduce its memory consumption: `ejabberd`
 will restart this process immediately.

- `epam` program tries to turn off delays on authentication failures.
 However, some PAM modules ignore this behavior and rely on their own
 configuration options. You can create a configuration file
 `ejabberd.pam`. This example shows how to turn off delays in
 `pam_unix.so` module:

``` sh
  #%PAM-1.0
  auth        sufficient  pam_unix.so likeauth nullok nodelay
  account     sufficient  pam_unix.so
```

	That is not a ready to use configuration file: you must use it as a
 hint when building your own PAM configuration instead. Note that if
 you want to disable delays on authentication failures in the PAM
 configuration file, you have to restrict access to this file, so a
 malicious user can’t use your configuration to perform brute-force
 attacks.

- You may want to allow login access only for certain users.
 `pam_listfile.so` module provides such functionality.

- If you use `pam_winbind` to authorise against a Windows Active
 Directory, then `/etc/nsswitch.conf` must be configured to use
 `winbind` as well.

### JWT Authentication

`ejabberd` supports authentication using JSON Web Token (JWT).  When enabled,
clients send signed tokens instead of passwords, which are checked using a
private key specified in the `jwt_key` option.  JWT payload must look like
this:

``` json
    {
      "jid": "test@example.org",
      "exp": 1564436511
    }
 ```

Options:

**`jwt_key: Path`**:   Full path to a file containing the JWT private key.

**`jwt_auth_only_rule: AccessName`**:   The accounts that match this access rule can authenticate only using JWT, even if ejabberd is configured to support other auth methods too.

Example:
``` yaml
 auth_method: jwt
 jwt_key: /path/to/jwt/key
```

In this example, admins can use both JWT and plain passwords, while the rest of users can use only JWT.

``` yaml
 # the order is important here, don't use [sql, jwt]
 auth_method: [jwt, sql]

 access_rules:
   ...
   jwt_only:
     deny: admin
     allow: all

 jwt_auth_only_rule: jwt_only
```

## Access Rules

*This section describes new ACL syntax introduced in ejabberd 16.06.
For old access rule and ACL syntax documentation, please refer to [configuration document archive](https://github.com/processone/docs.ejabberd.im/blob/7391ac375fd8253f74214cbffa2bafb140501981/content/admin/guide/configuration.md)*

### ACL Definition

Access control in `ejabberd` is performed via Access Control Lists
(ACLs). The declarations of ACLs in the configuration file have the
following syntax:

**`acl: { ACLName: { ACLType: ACLValue } }`**

`ACLType: ACLValue` can be one of the following:

**`all`**:   Matches all JIDs. Example:

``` yaml
     acl:
       world: all
```

**`user: Username`**:   Matches the user with the name `Username` on any of the local virtual host.
 Example:

``` yaml
     acl:
       admin:
         user: yozhik
```

**`user: {Username: Server} | Jid`**:   Matches the user with the JID `Username@Server` and any resource.
 Example:

``` yaml
     acl:
       admin:
         - user:
             yozhik@example.org
         - user: peter@example.org
```

**`server: Server`**:   Matches any JID from server `Server`. Example:

``` yaml
      acl:
       exampleorg:
         server: example.org
```

**`resource: Resource`**:   Matches any JID with a resource `Resource`. Example:

``` yaml
     acl:
       mucklres:
         resource: muckl
```

**`shared_group: Groupname`**:   Matches any member of a Shared Roster Group with name `Groupname` in
 the virtual host. Example:

``` yaml
     acl:
       techgroupmembers:
         shared_group: techteam
```

**`shared_group: {Groupname: Server}`**:   Matches any member of a Shared Roster Group with name `Groupname` in
 the virtual host `Server`. Example:

``` yaml
     acl:
       techgroupmembers:
         shared_group:
           techteam: example.org
```

**`ip: Network`**:   Matches any IP address from the `Network`. Example:

``` yaml
     acl:
       loopback:
         ip:
           - 127.0.0.0/8
           - "::1"
```

**`user_regexp: Regexp`**:   Matches any local user with a name that matches `Regexp` on local
 virtual hosts. Example:

``` yaml
     acl:
       tests:
         user_regexp: "^test[0-9]*$"
```

**`user_regexp: {Regexp: Server} | JidRegexp`**:   Matches any user with a name that matches `Regexp` at server
 `Server`. Example:

``` yaml
     acl:
       tests:
         user_regexp:
           - "^test1": example.org
           - "^test2@example.org"
```

**`server_regexp: Regexp`**:   Matches any JID from the server that matches `Regexp`. Example:

``` yaml
     acl:
       icq:
         server_regexp: "^icq\\."
```

**`resource_regexp: Regexp`**:   Matches any JID with a resource that matches `Regexp`. Example:

``` yaml
     acl:
       icq:
         resource_regexp: "^laptop\\."
```

**`node_regexp: {UserRegexp: ServerRegexp}`**:   Matches any user with a name that matches `UserRegexp` at any server
 that matches `ServerRegexp`. Example:

``` yaml
     acl:
       yozhik:
         node_regexp:
           "^yozhik$": "^example.(com|org)$"
```
<!-- TODO:: explain remaining parameters -->

**`user_glob: Glob`**:

**`user_glob: {Glob: Server}`**:

**`server_glob: Glob`**:

**`resource_glob: Glob`**:

**`node_glob: {UserGlob: ServerGlob}`**:   This is the same as above. However, it uses shell glob patterns
instead of regexp. These patterns can have the following special
characters:

- **`*`**:   matches any string including the null string.

- **`?`**:   matches any single character.

- **`[...]`**:   matches any of the enclosed characters. Character ranges are
     specified by a pair of characters separated by a `-`. If the
     first character after `[` is a `!`, any character not enclosed
     is matched.

The following `ACLName` are pre-defined:

**`all`**:   Matches any JID.

**`none`**:   Matches no JID.

### Access Rights

An entry allowing or denying access to different services. The syntax
is:

**`access_rules: { AccessName: { - allow|deny: ACLRule|ACLDefinition } }`**

Each definition may contain arbitrary number of `- allow` or `- deny`
sections, and each section can contain any number of acl rules
(as defined in [previous section](#acl-definition), it recognizes
one additional rule `acl: RuleName` that matches when acl rule
named `RuleName` matches). If no rule or definition is defined, the
rule `all` is applied.

Definition's `- allow` and `- deny` sections are processed in top
to bottom order, and first one for which all listed acl rules matches
is returned as result of access rule. If no rule matches `deny` is returned.

To simplify configuration two shortcut version are available:
`- allow: acl` and `- allow`, example below shows equivalent
definitions where short or long version are used:

``` yaml
    access_rules:
      a_short: admin
      a_long:
        - acl: admin
      b_short:
        - deny: banned
        - allow
      b_long:
        - deny:
          - acl: banned
        - allow:
          - all
```

If you define specific Access rights in a virtual host, remember that
the globally defined Access rights have precedence over those. This
means that, in case of conflict, the Access granted or denied in the
global server is used and the Access of a virtual host doesn't have
effect.

Example:

``` yaml
      access_rules:
        configure:
          - allow: admin
        something:
          - deny: someone
          - allow
        s2s_banned:
          - deny: problematic_hosts
          - deny:
            - acl: banned_forever
          - deny:
            - ip: 222.111.222.111/32
          - deny:
            - ip: 111.222.111.222/32
          - allow
        xmlrpc_access:
          - allow:
            - user: peter@example.com
          - allow:
            - user: ivone@example.com
          - allow:
            - user: bot@example.com
            - ip: 10.0.0.0/24
```

The following `AccessName` are pre-defined:

**`all`**:   Always returns the value ‘`allow`’.

**`none`**:   Always returns the value ‘`deny`’.

### Shaper Rules

An entry allowing to declaring shaper to use for matching user/hosts. The syntax
is:

**`shaper_rules: { ShaperRuleName: { - Number|ShaperName: ACLRule|ACLDefinition } }`**

Semantic is similar to that described in [Access Rights](#access-rights) section,
only difference is that instead using `- allow` or `- deny`, name of shaper or number
should be used.

Examples:

    shaper_rules:
      connections_limit:
        - 10:
          - user: peter@example.com
        - 100: admin
        - 5
      download_speed:
        - fast: admin
        - slow: anonymous_users
        - normal
      log_days: 30

### Limiting Opened Sessions with ACL

The special access `max_user_sessions` specifies the maximum number of
sessions (authenticated connections) per user. If a user tries to open
more sessions by using different resources, the first opened session
will be disconnected. The error `session replaced` will be sent to the
disconnected session. The value for this option can be either a number,
or `infinity`. The default value is `infinity`.

The syntax is:

**`{ max_user_sessions: { - Number: ACLRule|ACLDefinition } }`**

This example limits the number of sessions per user to 5 for all users,
and to 10 for admins:

 shaper_rules:
   max_user_sessions:
     - 10: admin
     - 5

### Several connections to a remote XMPP server with ACL

The special access `max_s2s_connections` specifies how many simultaneous
S2S connections can be established to a specific remote XMPP server. The
default value is `1`. There’s also available the access
`max_s2s_connections_per_node`.

The syntax is:

**`{ max_s2s_connections: { ACLName: MaxNumber } }`**

Examples:

- Allow up to 3 connections with each remote server:

  shaper_rules:
    max_s2s_connections: 3

## Shapers

Shapers enable you to limit connection traffic. The syntax is:

**`shaper: { ShaperName: Rate }`**:  where `Rate` stands for the maximum allowed incoming rate in bytes per
second. When a connection exceeds this limit, `ejabberd` stops reading
from the socket until the average rate is again below the allowed
maximum.

Examples:

- To define a shaper named ‘`normal`’ with traffic speed limited to
 1,000bytes/second:

  shaper:
    normal: 1000

- To define a shaper named ‘`fast`’ with traffic speed limited to
 50,000bytes/second:

  shaper:
    fast: 50000

## Default Language

The option `language` defines the default language of server strings
that can be seen by XMPP clients. If a XMPP client does not support
`xml:lang`, the specified language is used.

The option syntax is:

**`language: Language`**: The default value is `en`. In order to take effect there must be a
translation file `Language.msg` in `ejabberd`’s `msgs` directory.

For example, to set Russian as default language:

 language: ru

The page [Internationalization and Localization](../developer/extending-ejabberd/localization.md)
provides more details.

## CAPTCHA

Some `ejabberd` modules can be configured to require a CAPTCHA challenge
on certain actions. If the client does not support CAPTCHA Forms
([`XEP-0158`][11]), a web link is
provided so the user can fill the challenge in a web browser.

An example script is provided that generates the image using
ImageMagick’s Convert program.

!!! info
    We do not provide example scripts to support image generation on Microsoft Windows. Captcha will not work with ejabberd Windows installer.

The configurable options are:

**`captcha_cmd: Path`**:   Full path to a script that generates the image. The default value
 disables the feature: `undefined`

**`captcha_url: URL`**:  An URL where CAPTCHA requests should be sent.
        You need to configure request_handlers for ejabberd_http listener as well.

Example configuration:

 hosts: [example.org]

 captcha_cmd: /lib/ejabberd/priv/bin/captcha.sh
 captcha_url: http://example.org:5280/captcha
 ## captcha_url: https://example.org:443/captcha
 ## captcha_url: http://example.com/captcha

 listen:
   ...
   -
     port: 5280
     module: ejabberd_http
     request_handlers:
       /captcha: ejabberd_captcha
   ...

## Node settings

`net_ticktime: 60`

This option can be used to tune tick time parameter of
net_kernel. It tells `erlang` VM how often nodes should check if
intra-node communication was not interrupted. This option must have
identical value on all nodes, or it will lead to subtle bugs. Usually
leaving default value of this is option is best, tweak it only if you
know what you are doing.

## STUN and TURN

`ejabberd` is able to act as a stand-alone STUN/TURN server
([`RFC 5389`][12]/[`RFC 5766`][13]).
In that role `ejabberd` helps clients with ICE
([`RFC 5245`][14]) or Jingle ICE
([`XEP-0176`][15]) support to
discover their external addresses and ports and to relay media traffic
when it is impossible to establish direct peer-to-peer connection.

You should configure `ejabberd_stun` listening module as described in
[Listening Module](#listening-module) section. The specific configurable options are:

**`tls: true|false`**:   If enabled, `certfile` option must be set, otherwise `ejabberd` will
 not be able to accept TLS connections. Obviously, this option makes
 sense for `tcp` transport only. The default is `false`.

**`certfile: Path`**:   Path to the certificate file. Only makes sense when `tls` is set.

**`use_turn: true|false`**:   Enables/disables TURN (media relay) functionality. The default is
 `false`.

**`turn_ip: String`**:   The IPv4 address advertised by your TURN server. The address should
 not be NAT’ed or firewalled. There is not default, so you should set
 this option explicitly. Implies `use_turn`.

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
     turn_ip: 10.20.30.1
     module: ejabberd_stun
   ...

You also need to configure DNS SRV records properly so clients can
easily discover a STUN/TURN server serving your XMPP domain. Refer to
section
[`DNS Discovery of a Server`][16]
of [`RFC 5389`][17] and section
[`Creating an Allocation`][18]
of [`RFC 5766`][19] for details.

Example DNS SRV configuration for STUN only:

 _stun._udp   IN SRV  0 0 3478 stun.example.com.
 _stun._tcp   IN SRV  0 0 3478 stun.example.com.
 _stuns._tcp  IN SRV  0 0 5349 stun.example.com.

And you should also add these in the case if TURN is enabled:

 _turn._udp   IN SRV  0 0 3478 turn.example.com.
 _turn._tcp   IN SRV  0 0 3478 turn.example.com.
 _turns._tcp  IN SRV  0 0 5349 turn.example.com.

## SIP

### SIP Configuration

`ejabberd` has built-in SIP support. In order to activate it you need to
add listeners for it, configure DNS properly and enable `mod_sip` for
the desired virtual host.

To add a listener you should configure `ejabberd_sip` listening module
as described in [Listening Module](#listening-module) section. If option `tls` is specified, option
`certfile` must be specified as well, otherwise incoming TLS connections
would fail.

Example configuration with standard ports (as per
[`RFC 3261`][20]):

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
[`SNI`][21] support is
somewhat tricky, so for TLS you have to configure different virtual
hosts on different ports if you have different certificate files for
them.

Next you need to configure DNS SIP records for your virtual domains.
Refer to [`RFC 3263`][22] for the
detailed explanation. Simply put, you should add NAPTR and SRV records
for your domains. Skip NAPTR configuration if your DNS provider doesn't
support this type of records. It’s not fatal, however, highly
recommended.

Example configuration of NAPTR records:

 example.com IN NAPTR 10  0 "s" "SIPS+D2T" "" _sips._tcp.example.com.
 example.com IN NAPTR 20  0 "s" "SIP+D2T" "" _sip._tcp.example.com.
 example.com IN NAPTR 30  0 "s" "SIP+D2U" "" _sip._udp.example.com.

Example configuration of SRV records with standard ports (as per
[`RFC 3261`][23]):

 _sip._udp   IN SRV  0 0 5060 sip.example.com.
 _sip._tcp   IN SRV  0 0 5060 sip.example.com.
 _sips._tcp  IN SRV  0 0 5061 sip.example.com.

### Note on SIP usage

SIP authentication does not support SCRAM. As such, it is not possible
to use `mod_sip` to authenticate when ejabberd has been set to encrypt
password with SCRAM.

## Include Additional Configuration Files

The option `include_config_file` in a configuration file instructs
`ejabberd` to include other configuration files immediately.

The basic syntax is:

**`include_config_file: [Filename]`**

It is possible to specify suboptions using the full syntax:

**`include_config_file: { Filename: [Suboption, ...] }`**:  The filename can be indicated either as an absolute path, or relative to
the main `ejabberd` configuration file. It isn't possible to use
wildcards. The file must exist and be readable.

The allowed suboptions are:

**`disallow: [Optionname, ...]`**:   Disallows the usage of those options in the included configuration
 file. The options that match this criteria are not accepted. The
 default value is an empty list: `[]`

**`allow_only: [Optionname, ...]`**:   Allows only the usage of those options in the included configuration
 file. The options that do not match this criteria are not accepted.
 The default value is: `all`

This is a basic example:

 include_config_file: /etc/ejabberd/additional.yml

In this example, the included file is not allowed to contain a `listen`
option. If such an option is present, the option will not be accepted.
The file is in a subdirectory from where the main configuration file is.

 include_config_file:
   ./example.org/additional_not_listen.yml:
     disallow: [listen]

Please notice that options already defined in the main configuration file cannot be redefined in the included configuration files. But you can use `host_config` and `append_host_config` as usual (see [Virtual Hosting](#virtual-hosting)).

In this example, `ejabberd.yml` defines some ACL for the whole ejabberd server, and later includes another file:

 acl:
   admin:
     user:
       - admin@localhost
 include_config_file:
   /etc/ejabberd/acl.yml

The file `acl.yml` can add additional administrators to one of the virtual hosts:

 append_host_config:
   localhost:
     acl:
       admin:
         user:
           - bob@localhost
           - jan@localhost

## Option Macros in Configuration File

In the `ejabberd` configuration file, it is possible to define a macro
for a value and later use this macro when defining an option.

A macro is defined with this syntax:

**`define_macro: { ’MACRO’: Value }`**:  Defines a macro. The `value` can be any
valid arbitrary YAML value. For convenience, it's recommended to define
a macro name in capital letters.

Duplicated macros are not allowed.

Macros are processed after additional configuration files have been
included, so it is possible to use macros that are defined in
configuration files included before the usage.

It is possible to use a macro in the definition of another macro.

This example shows the basic usage of a macro:

 define_macro:
   LOG_LEVEL_NUMBER: 5
 loglevel: LOG_LEVEL_NUMBER

The resulting option interpreted by `ejabberd` is: `loglevel: 5`.

This example shows that values can be any arbitrary YAML value:

 define_macro:
   USERBOB:
     user:
       - bob@localhost
 acl:
   admin: USERBOB

The resulting option interpreted by `ejabberd` is:

 acl:
   admin:
     user:
       - bob@localhost

This complex example:

 define_macro:
   NUMBER_PORT_C2S: 5222
   NUMBER_PORT_HTTP: 5280
 listen:
   -
     port: NUMBER_PORT_C2S
     module: ejabberd_c2s
   -
     port: NUMBER_PORT_HTTP
     module: ejabberd_http

produces this result after being interpreted:

 listen:
   -
     port: 5222
     module: ejabberd_c2s
   -
     port: 5280
     module: ejabberd_http

# Database and LDAP Configuration

`ejabberd` uses its internal Mnesia database by default. However, it is
possible to use a relational database, key-value storage or an LDAP
server to store persistent, long-living data. `ejabberd` is very
flexible: you can configure different authentication methods for
different virtual hosts, you can configure different authentication
mechanisms for the same virtual host (fallback), you can set different
storage systems for modules, and so forth.

The following databases are supported by `ejabberd`:

- [`Mnesia`][24]

- [`MySQL`][25]

- [`Any ODBC compatible database`][26]

- [`PostgreSQL`][27]

- [`MS SQL Server/SQL Azure`][115]

- [`SQLite`][119]

- [`Redis`][29] (only for transient data)

The following LDAP servers are tested with `ejabberd`:

- `Active Directory` (see
 section [Active Directory](#active-directory))

- [`OpenLDAP`][31]

- [`CommuniGate Pro`][32]

- Normally any LDAP compatible server should work; inform us about
 your success with a not-listed server so that we can list it here.

Important note about virtual hosting: if you define several domains in
ejabberd.yml (see section [Host Names](#host-names)), you probably want that each
virtual host uses a different configuration of database, authentication
and storage, so that usernames do not conflict and mix between different
virtual hosts. For that purpose, the options described in the next
sections must be set inside a `host_config` for each vhost (see section
[Virtual Hosting](#virtual-hosting)). For example:

 host_config:
   public.example.org:
     sql_type: pgsql
     sql_server: localhost
     sql_database: database-public-example-org
     sql_username: ejabberd
     sql_password: password
     auth_method: [sql]

## Relational Databases

There are two schemas usable for ejabberd. The default lecacy schema allows to
store one XMPP domain into one ejabberd database. The 'new' schema allows to
handle several XMPP domains in a single ejabberd database. Using this new schema
is best when serving several XMPP domains and/or changing domains from time to
time. This avoid need to manage several databases and handle complex
configuration changes.

You need to upload SQL schema to your SQL server. Choose the one from [`this`][118] list.
If you are using MySQL and choose the default schema, use `mysql.sql`. If you
are using PostgreSQL and need the new schema, use `pg.new.sql`.

If you choose the new schema, you MUST add an extra line into `ejabberd.yml`
configuration file to enable the feature:

 new_sql_schema: true

The actual database access is defined in the options with `sql_`
prefix. The values are used to define if we want to use ODBC, or one of
the two native interface available, PostgreSQL or MySQL.

The following parameters are available:

**`sql_type: mysql | pgsql | odbc | mssql | sqlite`**:   The type of an SQL connection. The default is `odbc`.

**`sql_server: String`**:   A hostname of the SQL server. The default is `localhost`.

**`sql_port: Port`**:   The port where the SQL server is accepting connections. The option
 is valid for `mysql`, `pgsql` and `mssql`. The default is `3306` and
 `5432` respectively.

**`sql_database: String`**:   The database name. The default is `ejabberd`. The option is valid
 for `mysql`, `pgsql` and `mssql`.

**`sql_username: String`**:   The username. The default is `ejabberd`. The option is valid
 for `mysql`, `pgsql` and `mssql`.

**`sql_password: String`**:   The password. The default is empty string. The option is valid
 for `mysql`, `pgsql` and `mssql`.

**`sql_ssl: Boolean`**:   use ssl if not set plain tcp.
 The default value is `false`. The option is valid for `pgsql` and `mysql`.

**`sql_ssl_verify: Boolean`**:   if set verify ssl connect.
 The default value is `false`. The option is valid for `pgsql`.

**`sql_ssl_cafile: String`**:  The path to a CA certificate file.
 By default the option is not set. The option is valid for `pgsql`.

**`sql_ssl_certfile: String`**:  The path to a client/authentication certificate file.
 By default the option is not set. The option is valid for `pgsql`.

**`sql_pool_size: N`**:   By default `ejabberd` opens 10 connections to the database for each
 virtual host. You can change this number by using this option.

**`sql_keepalive_interval: N`**:   You can configure an interval to make a dummy SQL request to keep
 alive the connections to the database. The default value is
 ’undefined’, so no keepalive requests are made. Specify in seconds:
 for example 28800 means 8 hours.

**`sql_start_interval: N`**:   If the connection to the database fails, `ejabberd` waits 30 seconds
 before retrying. You can modify this interval with this option.

**`sql_prepared_statements: Boolean`**:  If you need to disable prepared
 statements.  The default value is `true`.  The option is valid for
 `pgsql` and `mysql`.

Example of plain ODBC connection:

 sql_server: "DSN=database;UID=ejabberd;PWD=password"

Example of MySQL connection:

 sql_type: mysql
 sql_server: server.company.com
 sql_port: 3306 # the default
 sql_database: mydb
 sql_username: user1
 sql_password: "**********"
 sql_pool_size: 5

## Microsoft SQL Notes

For now, MS SQL is only supported in Unix-like OS'es. You need to have
[`FreeTDS`][116] and [`unixODBC`][117] installed on your machine.
Also, in some cases you need to add machine name to `sql_username`, especially
when you have `sql_server` defined as an IP address, e.g.:

 sql_type: mssql
 sql_server: 1.2.3.4
 ...
 sql_username: user1@host

### SQL Authentication

You can authenticate users against an SQL database, see the option
`auth_method` in section [Authentication](#authentication).

The option `auth_password_format` is supported,
for details see section [Internal](#internal).
Please note that if you use SQL auth method and set SCRAM format,
old plain passwords that may be stored in the database are not
automatically scrammed. For that, you can execute the command:

 ejabberdctl convert_to_scram example.org

### SQL Storage

An ODBC compatible database also can be used to store information into
from several `ejabberd` modules. See section [Modules Overview](#modules-overview) to see which
modules can be used with relational databases like MySQL. To enable
storage to your database, just make sure that your database is running
well (see previous sections), and add the module option `db_type: sql`
or set `default_db: sql` globally if you want to use SQL for all modules.

## LDAP

`ejabberd` has built-in LDAP support. You can authenticate users against
LDAP server and use LDAP directory as vCard storage.

Usually `ejabberd` treats LDAP as a read-only storage: it is possible to
consult data, but not possible to create accounts or edit vCard that is
stored in LDAP. However, it is possible to change passwords if
`mod_register` module is enabled and LDAP server supports
[`RFC 3062`][33].

### LDAP Connection

Two connections are established to the LDAP server per vhost, one for
authentication and other for regular calls.

Parameters:

**`ldap_servers: [Servers, ...]`**:   List of IP addresses or DNS names of your LDAP servers. This option
 is required.

**`ldap_encrypt: none|tls`**:   Type of connection encryption to the LDAP server. Allowed values
 are: `none`, `tls`. The value `tls` enables encryption by using LDAP
 over SSL. Note that STARTTLS encryption is not supported. The
 default value is: `none`.

**`ldap_tls_verify: false|soft|hard`**:   This option specifies whether to verify LDAP server certificate or
 not when TLS is enabled. When `hard` is enabled `ejabberd` doesn’t
 proceed if a certificate is invalid. When `soft` is enabled
 `ejabberd` proceeds even if check fails. The default is `false`
 which means no checks are performed.

**`ldap_tls_cacertfile: Path`**:   Path to file containing PEM encoded CA certificates. This option is
 needed (and required) when TLS verification is enabled.

**`ldap_tls_depth: Number`**:   Specifies the maximum verification depth when TLS verification is
 enabled, i.e. how far in a chain of certificates the verification
 process can proceed before the verification is considered to fail.
 Peer certificate = 0, CA certificate = 1, higher level CA
 certificate = 2, etc. The value 2 thus means that a chain can at
 most contain peer cert, CA cert, next CA cert, and an additional CA
 cert. The default value is 1.

**`ldap_port: Number`**:   Port to connect to your LDAP server. The default port is 389 if
 encryption is disabled; and 636 if encryption is enabled. If you
 configure a value, it is stored in `ejabberd`’s database. Then, if
 you remove that value from the configuration file, the value
 previously stored in the database will be used instead of the
 default port.

**`ldap_rootdn: RootDN`**:   Bind DN. The default value is empty string "" which means ‘anonymous connection’.

**`ldap_password: Password`**:   Bind password. The default value is is empty string.

**`ldap_deref_aliases: never|always|finding|searching`**:   Whether or not to dereference aliases. The default is `never`.

Example:

 auth_method: [ldap]
 ldap_servers:
   - ldap1.example.org
 ldap_port: 389
 ldap_rootdn: "cn=Manager,dc=domain,dc=org"
 ldap_password: "**********"

### LDAP Authentication

You can authenticate users against an LDAP directory. Note that current
LDAP implementation does not support SASL authentication.

Available options are:

**`ldap_base: Base`**:   LDAP base directory which stores users accounts. This option is
 required.

**`ldap_uids: [ ldap_uidattr | {ldap_uidattr: ldap_uidattr_format} ]`**:   LDAP attribute which holds a list of attributes to use as
 alternatives for getting the JID. The default attributes are
 `[{uid, %u}]`. The attributes are of the form: `[{ldap_uidattr}]` or
 `[{ldap_uidattr, ldap_uidattr_format}]`. You can use as many comma
 separated attributes as needed. The values for `ldap_uidattr` and
 `ldap_uidattr_format` are described as follow:

- **`ldap_uidattr`**:   LDAP attribute which holds the user’s part of a JID. The default
     value is `uid`.

- **`ldap_uidattr_format`**:   Format of the `ldap_uidattr` variable. The format *must* contain
     one and only one pattern variable `%u` which will be replaced by
     the user’s part of a JID. For example, `%u@example.org`. The
     default value is `%u`.

**`ldap_filter: Filter`**:   [`RFC 4515`][34] LDAP filter. The
 default Filter value is: `undefined`. Example:
 `(&(objectClass=shadowAccount)(memberOf=Jabber Users))`. Please, do
 not forget to close brackets and do not use superfluous whitespaces.
 Also you *must not* use `ldap_uidattr` attribute in filter because
 this attribute will be substituted in LDAP filter automatically.

**`ldap_dn_filter: { Filter: FilterAttrs }`**:   This filter is applied on the results returned by the main filter.
 This filter performs additional LDAP lookup to make the complete
 result. This is useful when you are unable to define all filter
 rules in `ldap_filter`. You can define `%u`, `%d`, `%s` and `%D`
 pattern variables in Filter: `%u` is replaced by a user’s part of a
 JID, `%d` is replaced by the corresponding domain (virtual host),
 all `%s` variables are consecutively replaced by values of
 FilterAttrs attributes and `%D` is replaced by Distinguished Name.
 By default `ldap_dn_filter` is undefined. Example:

     ldap_dn_filter:
       "(&(name=%s)(owner=%D)(user=%u@%d))": [sn]

Since this filter makes additional LDAP lookups, use it only in the
last resort: try to define all filter rules in `ldap_filter` if
possible.

### LDAP Examples

#### Common example

Let’s say `ldap.example.org` is the name of our LDAP server. We have
users with their passwords in `ou=Users,dc=example,dc=org` directory.
Also we have addressbook, which contains users emails and their
additional infos in `ou=AddressBook,dc=example,dc=org` directory. The
connection to the LDAP server is encrypted using TLS, and using the
custom port 6123. Corresponding authentication section should looks like
this:

 ## Authentication method
 auth_method: [ldap]
 ## DNS name of our LDAP server
 ldap_servers: [ldap.example.org]
 ## Bind to LDAP server as "cn=Manager,dc=example,dc=org" with password "secret"
 ldap_rootdn: "cn=Manager,dc=example,dc=org"
 ldap_password: secret
 ldap_encrypt: tls
 ldap_port: 6123
 ## Define the user's base
 ldap_base: "ou=Users,dc=example,dc=org"
 ## We want to authorize users from 'shadowAccount' object class only
 ldap_filter: "(objectClass=shadowAccount)"

Now we want to use users LDAP-info as their vCards. We have four
attributes defined in our LDAP schema: `mail` — email address,
`givenName` — first name, `sn` — second name, `birthDay` — birthday.
Also we want users to search each other. Let’s see how we can set it up:

 modules:
   ...
   mod_vcard:
     db_type: ldap
     ## We use the same server and port, but want to bind anonymously because
     ## our LDAP server accepts anonymous requests to
     ## "ou=AddressBook,dc=example,dc=org" subtree.
     ldap_rootdn: ""
     ldap_password: ""
     ## define the addressbook's base
     ldap_base: "ou=AddressBook,dc=example,dc=org"
     ## uidattr: user's part of JID is located in the "mail" attribute
     ## uidattr_format: common format for our emails
     ldap_uids:
       mail: "%u@mail.example.org"
     ## We have to define empty filter here, because entries in addressbook does not
     ## belong to shadowAccount object class
     ldap_filter: ""
     ## Now we want to define vCard pattern
     ldap_vcard_map:
      NICKNAME: {"%u": []} # just use user's part of JID as their nickname
      GIVEN: {"%s": [givenName]}
      FAMILY: {"%s": [sn]}
      FN: {"%s, %s": [sn, givenName]} # example: "Smith, John"
      EMAIL: {"%s": [mail]}
      BDAY: {"%s": [birthDay]}]}
     ## Search form
     ldap_search_fields:
       User: "%u"
       Name: givenName
       "Family Name": sn
       Email: mail
       Birthday: birthDay
     ## vCard fields to be reported
     ## Note that JID is always returned with search results
     ldap_search_reported:
       "Full Name": FN
       Nickname: NICKNAME
       Birthday: BDAY
   ...

Note that `mod_vcard` with LDAP backend checks for the existence of the user
before searching their information in LDAP.

#### Active Directory

Active Directory is just an LDAP-server with predefined attributes. A
sample configuration is shown below:

 auth_method: [ldap]
 ldap_servers: [office.org]  # List of LDAP servers
 ldap_base: "DC=office,DC=org" # Search base of LDAP directory
 ldap_rootdn: "CN=Administrator,CN=Users,DC=office,DC=org" # LDAP manager
 ldap_password: "*******" # Password to LDAP manager
 ldap_uids: [sAMAccountName]
 ldap_filter: "(memberOf=*)"

 modules:
   ...
   mod_vcard:
     db_type: ldap
     ldap_vcard_map:
       NICKNAME: {"%u": []}
       GIVEN: {"%s": [givenName]}
       MIDDLE: {"%s": [initials]}
       FAMILY: {"%s": [sn]}
       FN: {"%s": [displayName]}
       EMAIL: {"%s": [mail]}
       ORGNAME: {"%s": [company]}
       ORGUNIT: {"%s": [department]}
       CTRY: {"%s": [c]}
       LOCALITY: {"%s": [l]}
       STREET: {"%s": [streetAddress]}
       REGION: {"%s": [st]}
       PCODE: {"%s": [postalCode]}
       TITLE: {"%s": [title]}
       URL: {"%s": [wWWHomePage]}
       DESC: {"%s": [description]}
       TEL: {"%s": [telephoneNumber]}]}
     ldap_search_fields:
       User: "%u"
       Name: givenName
       "Family Name": sn
       Email: mail
       Company: company
       Department: department
       Role: title
       Description: description
       Phone: telephoneNumber
     ldap_search_reported:
       "Full Name": FN
       Nickname: NICKNAME
       Email: EMAIL
   ...

## Redis

[`Redis`][37] is an advanced key-value cache and store. You can
use it to store transient data, such as records for C2S (client) sessions.
There are several options available:

**`redis_server: String`**:   A hostname of the Redis server. The default is `localhost`.

**`redis_port: Port`**:   The port where the Redis server is accepting connections. The default
 is 6379.

**`redis_password: String`**:   The password to the Redis server. The default is an empty string,
 i.e. no password.

**`redis_db: N`**:   Redis database number. The default is 0.

**`redis_connect_timeout: N`**:   A number of seconds to wait for the connection to be established to the Redis
 server. The default is 1 second.

Example configuration:

 redis_server: redis.server.com
 redis_db: 1

## Default database configuration

You can simplify the configuration by setting the default database. This can be done with `default_db` option:

**`default_db: mnesia|sql`**:  This will define the default database for a module lacking `db_type` option or if `auth_method` option is not set.

# Session Management

By default pointers to C2S sessions are kept in Mnesia. You may want to
use another database backend for this. The option is:

**`sm_db_type: mnesia|sql|redis`**:   Note that for `sql` or `redis` you should have them configured. See sections
 [Relational Databases](#relational-databases) or [Redis](#redis).

# Modules Configuration

The option `modules` defines the list of modules that will be loaded
after `ejabberd`’s startup. Each entry in the list is a tuple in which
the first element is the name of a module and the second is a list of
options for that module.

The syntax is:

**`modules: { ModuleName: ModuleOptions }`**

Examples:

- In this example only the module `mod_muc` is loaded and no module
 options are specified between the square brackets:

  modules:
    mod_muc: {}

- In the second example the modules `mod_muc`, `mod_time`, and
 `mod_version` are loaded without options.

  modules:
    mod_muc:       {}
    mod_time:      {}
    mod_version:   {}

## Modules Overview

The following table lists all modules included in `ejabberd`.

| **Module**                                     | **Feature**                                          | **Dependencies**                 |
|:-----------------------------------------------|:-----------------------------------------------------|:---------------------------------|
| mod_adhoc                                      | Ad-Hoc Commands ([`XEP-0050`][38])                   |                                  |
| [mod_announce](#mod_announce)                  | Manage announcements                                 | recommends `mod_adhoc`           |
| [mod_block_strangers](#mod_block_strangers)    | Block packets from non-subscribers                   |                                  |
| mod_blocking                                   | Simple Communications Blocking ([`XEP-0191`][39])    | `mod_privacy`                    |
| [mod_bosh](#mod_bosh)                          | BOSH ([`XEP-0124`][69]) and XMPP over BOSH ([`XEP-0206`][70]) | `ejabberd_c2s` listener |
| mod_caps                                       | Entity Capabilities ([`XEP-0115`][40])               |                                  |
| mod_carboncopy                                 | Message Carbons ([`XEP-0280`][41])                   |                                  |
| [mod_client_state](#mod_client_state)          | Filter stanzas for inactive clients                  |                                  |
| mod_configure                                  | Server configuration using Ad-Hoc                    | `mod_adhoc`                      |
| [mod_delegation](#mod_delegation)              | Namespace Delegation ([`XEP-0355`][123])             |                                  |
| [mod_disco](#mod_disco)                        | Service Discovery ([`XEP-0030`][42])                 |                                  |
| [mod_fail2ban](#mod_fail2ban)                  | Bans IPs that show the malicious signs               |                                  |
| [mod_http_api](#mod_http_api)                  | REST API for ejabberd using JSON data                |                                  |
| [mod_http_fileserver](#mod_http_fileserver)    | Small HTTP file server                               |                                  |
| [mod_http_upload](#mod_http_upload)            | HTTP File Upload ([`XEP-0363`][120])                 |                                  |
| [mod_http_upload_quota](#mod_http_upload_quota)| HTTP File Upload Quotas                              | `mod_http_upload`                |
| [mod_jidprep](#mod_jidprep)                    | JID Prep ([`XEP-0328`][128])                         |                                  |
| [mod_last](#mod_last)                          | Last Activity ([`XEP-0012`][43])                     |                                  |
| [mod_mam](#mod_mam)                            | Message Archive Management ([`XEP-0313`][114])       | `mod_mam`                        |
| [mod_mix](#mod_mix)                            | Mediated Information eXchange ([`XEP-0369`][122])    | `mod_pubsub`                     |
| [mod_metrics](#mod_metrics)                    | Simple metrics handler for runtime statistics        |                                  |
| [mod_muc](#mod_muc)                            | Multi-User Chat ([`XEP-0045`][44])                   |                                  |
| mod_muc_admin                                  | Administrative commands for Multi-User Chat          | `mod_muc`                        |
| [mod_muc_log](#mod_muc_log)                    | Multi-User Chat room logging                         | `mod_muc`                        |
| [mod_multicast](#mod_multicast)                | Extended Stanza Addressing ([`XEP-0033`][109])       |                                  |
| [mod_offline](#mod_offline)                    | Offline message storage ([`XEP-0160`][45])           |                                  |
| [mod_ping](#mod_ping)                          | XMPP Ping and periodic keepalives ([`XEP-0199`][46]) |                                  |
| [mod_pres_counter](#mod_pres_counter)          | Detect presence subscription flood                   |                                  |
| [mod_privacy](#mod_privacy)                    | Blocking Communication ([`XEP-0016`][47])            |                                  |
| [mod_private](#mod_private)                    | Private XML Storage ([`XEP-0049`][48])               |                                  |
| [mod_privilege](#mod_privilege)                | Privileged Entity ([`XEP-0356`][124])                |                                  |
| [mod_proxy65](#mod_proxy65)                    | SOCKS5 Bytestreams ([`XEP-0065`][49])                |                                  |
| [mod_pubsub](#mod_pubsub)                      | Pub-Sub ([`XEP-0060`][50]), PEP ([`XEP-0163`][51])   | `mod_caps`                       |
| [mod_push](#mod_push)                          | Push Notifications ([`XEP-0357`][126])               |                                  |
| [mod_push_keepalive](#mod_push_keepalive)      | Keep sessions of push clients alive                  | `mod_push`                       |
| [mod_register](#mod_register)                  | In-Band Registration ([`XEP-0077`][54])              |                                  |
| [mod_register_web](#mod_register_web)          | Web for Account Registrations                        |                                  |
| [mod_roster](#mod_roster)                      | Roster management (XMPP IM)                          |                                  |
| [mod_s2s_dialback](#mod_s2s_dialback)          | Server Dialback ([`XEP-0220`][127])                  |                                  |
| [mod_service_log](#mod_service_log)            | Copy user messages to logger service                 |                                  |
| [mod_shared_roster](#mod_shared_roster)        | Shared roster management                             | `mod_roster`                     |
| [mod_shared_roster_ldap](#mod_shared_roster_ldap) | LDAP Shared roster management                        | `mod_roster`                     |
| [mod_sic](#mod_sic)                            | Server IP Check ([`XEP-0279`][55])                   |                                  |
| [mod_sip](#mod_sip)                            | SIP Registrar/Proxy ([`RFC 3261`][56])               | `ejabberd_sip`                   |
| [mod_stats](#mod_stats)                        | Statistics Gathering ([`XEP-0039`][57])              |                                  |
| [mod_stream_mgmt](#mod_stream_mgmt)            | Stream Management ([`XEP-0198`][125])                |                                  |
| [mod_time](#mod_time)                          | Entity Time ([`XEP-0202`][58])                       |                                  |
| [mod_vcard](#mod_vcard)                        | vcard-temp ([`XEP-0054`][59])                        |                                  |
| [mod_vcard_xupdate](#mod_vcard_xupdate)        | vCard-Based Avatars ([`XEP-0153`][61])               | `mod_vcard`                      |
| [mod_version](#mod_version)                    | Software Version ([`XEP-0092`][62])                  |                                  |

- (\*) This module requires a supported database. For a list of
 supported databases, see section [Database and LDAP Configuration](#database-and-ldap-configuration).

You can see which database backend each module needs by looking at the
suffix:

- No suffix, this means that the module uses Erlang’s built-in
 database Mnesia as backend or SQL database
 (see [Database and LDAP Configuration](#database-and-ldap-configuration)).

- ‘\_ldap’, this means that the module needs an LDAP server as
 backend.

You can find more
[`contributed modules`][63] on the
`ejabberd` website. Please remember that these contributions might not
work or that they can contain severe bugs and security leaks. Therefore,
use them at your own risk!

## Common Options

The following options are used by many modules. Therefore, they are
described in this separate section.

### host

This option defines the Jabber ID of a service provided by an `ejabberd`
module.

The syntax is:

**`host: HostName`**:

If you include the keyword “@HOST@” in the HostName, it is replaced at
start time with the real virtual host string.

This example configures the MUC module to provide its service
in the Jabber ID `conference.example.org`:

 modules:
   ...
   mod_muc:
     host: conference.example.org
   ...

However, if there are several virtual hosts and this module is enabled
in all of them, the “@HOST@” keyword must be used:

 modules:
   ...
   mod_muc:
     host: "conference.@HOST@"
   ...

### Caching

There are several global options to enable caching and configure its behaviour:

**`use_cache: false|true`**:   Enable or disable cache. The default is `true`.

**`cache_missed: false|true`**:   Whether or not to cache missed lookups.
    When there is an attempt to lookup for a value in a database and this
    value is not found and the option is set to `true`, this attempt will
    be cached and no attempts will be performed until the cache expires.
    Usually you don't want to change it. Default is `true`.

**`cache_size: Items|infinity`**:    A maximum number of items (not memory!) in cache.
    The rule of thumb, for all tables except rosters, you should set it to
    the number of maximum online users you expect.
    For roster multiply this number by 20 or so.
    If the cache size reaches this threshold, it's fully cleared, i.e.
    all items are deleted, and the corresponding warning is logged.
    You should avoid frequent cache clearance, because this degrades performance.
    The default value is 1000.

**`cache_life_time: Seconds|infinity`**:    The time (in seconds) of a cached item to keep in cache.
    Once it's expired, the corresponding item is erased from cache.
    The default value is 3600 seconds, that is one hour.

These options can also be defined specifically in several modules.
Additionally, some core ejabberd parts support those options,
like `auth`, `oauth`, `router` and `sm`.
In those cases, please prepend the option names with the part name,
for example `router_use_cache`, or `sm_cache_size`.

## mod_admin_extra

Available option:

**`module_resource: Resource`**:   Indicate the resource that the XMPP stanzas must use in the FROM or TO JIDs.
    This is only useful in the vcard set and get commands.
    The default value is "mod_admin_extra".

In this example configuration, the users vCards can only be modified
by executing `mod_admin_extra` commands:

    acl:
      adminextraresource:
        - resource: "modadminextraf8x,31ad"
    access_rules:
      vcard_set:
        - allow: adminextraresource
    modules:
      mod_admin_extra:
        module_resource: "modadminextraf8x,31ad"
      mod_vcard:
        access_set: vcard_set

Description of some commands:

**`pushroster`**:  The file used by `pushroster` and `pushroster-all` must be placed:
     - Windows: on the directory were you installed ejabberd:
       `C:/Program Files/ejabberd`
     - Other OS: on the same directory where the .beam files are.
   Example content for the roster file:

  [{<<"bob">>, <<"example.org">>, <<"workers">>, <<"Bob">>},
   {<<"mart">>, <<"example.org">>, <<"workers">>, <<"Mart">>},
   {<<"Rich">>, <<"example.org">>, <<"bosses">>, <<"Rich">>}].

**`srg-create`**:  If you want to put a group Name with blankspaces, use the characters
   "' and '" to define when the Name starts and ends. For example:

  ejabberdctl srg-create g1 example.org "'Group number 1'" this_is_g1 g1

**`ban-account`**:  This command kicks all the connected sessions of the account from the
   server.  It also changes their password to a randomly
   generated one, so they can't login anymore unless a server administrator
   changes their password again.

It is possible to define the reason of the ban.  The new password
also includes the reason and the date and time of the ban.

For example, if this command is called:

  ejabberdctl vhost example.org ban-account boby Spammed several MUC rooms

then the sessions of the local account which JID is <boby@example.org>
will be kicked, and its password will be set to something like this:

  BANNED_ACCOUNT--20080425T21:45:07--2176635--Spammed_several_MUC_rooms

## mod_announce

This module enables configured users to broadcast announcements and to
set the message of the day (MOTD). Configured users can perform these
actions with a XMPP client either using Ad-hoc commands or sending
messages to specific JIDs.

The Ad-hoc commands are listed in the Server Discovery. For this feature
to work, `mod_adhoc` must be enabled.

The specific JIDs where messages can be sent are listed below. The
first JID in each entry will apply only to the specified virtual host
`example.org`, while the JID between brackets will apply to all virtual
hosts in ejabberd.

**`example.org/announce/all (example.org/announce/all-hosts/all)`**:   The message is sent to all registered users. If the user is online
 and connected to several resources, only the resource with the
 highest priority will receive the message. If the registered user is
 not connected, the message will be stored offline in assumption that
 offline storage (see section  [mod_offline](#mod_offline)) is enabled.

**`example.org/announce/online (example.org/announce/all-hosts/online)`**:   The message is sent to all connected users. If the user is online
 and connected to several resources, all resources will receive the
 message.

**`example.org/announce/motd (example.org/announce/all-hosts/motd)`**:   The message is set as the message of the day (MOTD) and is sent to
 users when they login. In addition the message is sent to all
 connected users (similar to `announce/online`).

**`example.org/announce/motd/update (example.org/announce/all-hosts/motd/update)`**:   The message is set as message of the day (MOTD) and is sent to users
 when they login. The message is *not sent* to any currently
 connected user.

**`example.org/announce/motd/delete (example.org/announce/all-hosts/motd/delete)`**:   Any message sent to this JID removes the existing message of the day
 (MOTD).

Options:

**`db_type: mnesia|sql`**:   Define the type of storage where the module will create the tables and store user information. The default is the storage defined by the global option `default_db`, or `mnesia` if omitted. If `sql` value is defined, make sure you have defined the database, see [database](#database-and-ldap-configuration).

**`use_cache: false|true`**:   Use this option and related ones as explained in section [Caching](#caching).

**`access: AccessName`**:   This option specifies who is allowed to send announcements and to
 set the message of the day (by default, nobody is able to send such
 messages).

Examples:

- Only administrators can send announcements:

  access_rules:
    announce:
      - allow: admin

  modules:
    ...
    mod_adhoc: {}
    mod_announce:
      access: announce
    ...

- Administrators as well as the direction can send announcements:

  acl:
    direction:
      user:
        big_boss@example.org
        assistant@example.org
    admin:
      user:
        admin@example.org
  access_rules:
    announce:
      - allow: admin
      - allow: direction

  modules:
    ...
    mod_adhoc: {}
    mod_announce:
      access: announce
    ...

Note that `mod_announce` can be resource intensive on large deployments
as it can broadcast lot of messages. This module should be disabled for
instances of `ejabberd` with hundreds of thousands users.

## mod_block_strangers

This module allows to block/log messages coming from an unknown entity.
If a writing entity is not in your roster, you can let this module drop and/or
log the message. By default you'll just not receive message from that entity.
Enable this module if you want to drop SPAM messages.

Options:

**`drop: true|false`**:   This option specifies if strangers messages should
be dropped or not. The default value is `true`.

**`log: true|false`**:   This option specifies if strangers messages should
be logged (as info message) in ejabberd logs. The default value is `false`.

**`allow_local_users: true|false`**:   This option specifies if strangers from
the same local host should be accepted or not. The default value is `true`.

**`allow_transports: true|false`**:   If set to `true` and some server's JID
is in user's roster, then messages from any user of this server are
accepted even if no subscription present. The default value is `true`.

**`captcha: true|false`**: Whether to generate CAPTCHA or not in response
to messages from strangers. See also section [CAPTCHA](#captcha). The
default value is `false`.

## mod_bosh

This module implements XMPP over BOSH as defined in [`XEP-0124`][69] and [`XEP-0206`][70]. BOSH stands for
Bidirectional-streams Over Synchronous HTTP. It makes it possible to simulate long lived connections required by XMPP
over HTTP protocol. In practice, this module makes it possible to use XMPP in a browser without Websocket support and
more generally to have a way to use XMPP while having to get through an HTTP proxy.

### Enabling BOSH support

`mod_bosh` extends ejabberd’s built in HTTP service with a configurable resource at which this service will be hosted.

To use XMPP over BOSH, enable the module:

 modules:
   ...
   mod_bosh: {}
   ...

and add `mod_bosh` in the HTTP listener service, together with `ejabberd_c2s`. For example:

 listen:
   -
     port: 5222
     module: ejabberd_c2s
     max_stanza_size: 65536
     shaper: c2s_shaper
     access: c2s
   ...
   -
     port: 5280
     module: ejabberd_http
     request_handlers:
        /bosh: mod_bosh
        /admin: ejabberd_web_admin
   ...

With this configuration, the module will serve the requests sent to
`http://example.org:5280/bosh/`

Please, remember that this page is not designed to be used by web browsers, it is used by XMPP clients that support XMPP
over BOSH.

### Options

- **`{max_inactivity, Seconds}`**:   Define the maximum inactivity period in seconds. Default value is 30
 seconds. For example, to set 50 seconds:

     modules:
       ...
       mod_bosh:
         max_inactivity: 50
       ...

- **`use_cache: false|true`**:   Use this option and related ones as explained in section [Caching](#caching).

<!-- TODO:: Document options
json, max_concat, max_inactivity, max_pause, prebind, ram_db_type,     queue_type, use_cache, cache_size, cache_missed, cache_life_time
-->

### Discovery

You also need to configure DNS SRV records properly so clients can
easily discover a BOSH server serving your XMPP domain. Refer to
[XEP-0156](https://xmpp.org/extensions/xep-0156.html).

Example DNS TXT configuration for BOSH:

    _xmppconnect IN TXT "[ _xmpp-client-xbosh=https://web.example.org:5280/bosh ]"

## mod_client_state

This module allows for queueing certain types of stanzas when a client
indicates that the user is not actively using the client right now (see
[`XEP-0352`][65]). This can save bandwidth and resources.

A stanza is dropped from the queue if it's effectively obsoleted by a new
one (e.g., a new presence stanza would replace an old one from the same
client). The queue is flushed if a stanza arrives that won't be queued, or
if the queue size reaches a certain limit (currently 100 stanzas), or if
the client becomes active again.

Options:

**`queue_chat_states: true|false`**:   Queue “standalone” chat state notifications (as defined in
 [`XEP-0085`](https://xmpp.org/extensions/xep-0085.html)) while a
 client indicates inactivity. The default value is `true`.

**`queue_pep: true|false`**:   Queue PEP notifications while a client is inactive. When the queue is
 flushed, only the most recent notification of a given PEP node is
 delivered. The default value is `true`.

**`queue_presence: true|false`**:   While a client is inactive, queue presence stanzas that indicate
 (un)availability. The default value is `true`.

Example:

 modules:
   ...
   mod_client_state:
     queue_chat_states: true
     queue_pep: false
     queue_presence: true
   ...

## mod_delegation

This module is an implementation of ([`XEP-0355`][123]). Only admin mode has been implemented by now. Namespace delegation allows external services to handle IQ using specific namespace. This may be applied for external PEP service.

Example:

To use the module add `mod_delegation` to `modules` section. If you want to delegate namespaces to a component, specify them in the namespaces option, and associate them to a access rule, e.g.:

  module:
  ...
    mod_delegation:
      namespaces:
        "urn:xmpp:mam:1":
           filtering: [node]
           access: external_mam
        "http://jabber.org/protocol/pubsub":
           access: external_pubsub
    ...

You'll also have to define the access rules to allow the component to handle the namespaces:

   access_rules:
   ...
     external_pubsub:
            - allow: external_component
     external_mam:
            - allow: external_component

   acl:
   ...
     external_component:
       server:
         - sat-pubsub.example.org

In the example above `sat-pubsub.example.org` will receive all `pubsub` requests and all `MAM` requests with the `node` filtering attribute presented in `<query/>`.

Make sure you do not delegate the same namespace to several services at the same time. As in the example above to have the `sat-pubsub.example.org` component perform correctly disable the `mod_pubsub` module.

### Security issue

Namespace delegation gives components access to sensitive data, so permission should be granted carefully, only if you trust the component.

### Note

This module is complementary to `mod_privilege` ([`XEP-0356`][124]) but can also be used separately.

## mod_disco

This module adds support for Service Discovery
([`XEP-0030`][66]). With this
module enabled, services on your server can be discovered by XMPP
clients. Note that `ejabberd` has no modules with support for the
superseded Jabber Browsing
([`XEP-0011`][67]) and Agent
Information ([`XEP-0094`][68]).
Accordingly, XMPP clients need to have support for the newer Service
Discovery protocol if you want them be able to discover the services you
offer.

Options:

**`extra_domains: [Domain, ...]`**:   With this option, you can specify a list of extra domains that are
 added to the Service Discovery item list.

**`server_info: [ { modules: Modules, name: Name, urls: [URL, ...] } ]`**:   Specify additional information about the server, as described in
 Contact Addresses for XMPP Services
 ([`XEP-0157`](https://xmpp.org/extensions/xep-0157.html)). `Modules`
 can be the keyword ‘all’, in which case the information is reported
 in all the services; or a list of `ejabberd` modules, in which case
 the information is only specified for the services provided by those
 modules. Any arbitrary `Name` and `URL` can be specified, not only
 contact addresses.

Examples:

- To serve a link to the Jabber User Directory on `jabber.org`:

  modules:
    ...
    mod_disco:
      extra_domains: [users.jabber.org]
    ...

- To serve a link to the transports on another server:

  modules:
    ...
    mod_disco:
      extra_domains:
        - icq.example.com
        - msn.example.com
    ...

- To serve a link to a few friendly servers:

  modules:
    ...
    mod_disco:
      extra_domains:
        - example.org
        - example.com
    ...

- With this configuration, all services show abuse addresses, feedback
 address on the main server, and admin addresses for both the main
 server and the vJUD service:

  modules:
    ...
    mod_disco:
      server_info:
        -
          modules: all
          name: abuse-addresses
          urls: [mailto:abuse@shakespeare.lit]
        -
          modules: [mod_muc]
          name: "Web chatroom logs"
          urls: [http://www.example.org/muc-logs]
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
    ...

## mod_fail2ban

The module bans IPs that show the malicious signs. Currently only C2S
authentication failures are detected.

Unlike the [fail2ban standalone program](https://github.com/fail2ban/fail2ban), `mod_fail2ban`
clears the record of authentication failures `c2s_auth_ban_lifetime`
seconds after the first failure or on a successful authentication. It
also does not simply block network traffic, but provides the client with
a descriptive error message.

Available options:

**`c2s_auth_ban_lifetime: Seconds`**:   The lifetime of the IP ban caused by too many C2S authentication
 failures. The default is 3600, i.e. one hour.

**`c2s_max_auth_failures: Integer`**:   The number of C2S authentication failures to trigger the IP ban. The
 default is 20.

**`access: AccessName`**:   Specify an access rule for whitelisting IP addresses or networks. If
 the rule returns ‘`allow`’ for a given IP address, that
 address will never be banned. The `AccessName` should be of type
 `ip`. The default value is `none`.

Example:

 modules:
   ...
   mod_fail2ban:
     c2s_auth_ban_lifetime: 7200
     c2s_max_auth_failures: 50
   ...

*Warning*: You should not use `mod_fail2ban` behind a proxy or load balancer. ejabberd will see the failures as coming
from the load balancer and, when the threshold of auth failures is reached, will reject all connections coming from the
load balancer. You can lock all your user base out of ejabberd when using `mod_fail2ban` behind a proxy.

## mod_http_api

This module provides a ReST API to call ejabberd commands using JSON data.

To start using it, simply add it as a request handler, for example:

 listen:
   -
     port: 5280
     module: ejabberd_http
     request_handlers:
       /api: mod_http_api

To use a specific API version N, add a vN element in the URL path, like:

       /api/v2: mod_http_api

Access rights are defined with those global options:

 commands_admin_access: configure
 commands:
   - add_commands: user

`add_commands` allows exporting a class of commands, from

- `open`: methods that are not risky and can be called without any access check

- `restricted`: the same, but will appear only in ejabberdctl list. This is the default.

- `admin`: auth is required with XMLRPC and HTTP API and checked for admin privileges, works as usual in ejabberdctl.

- `user`: can be used through XMLRPC and HTTP API, even by user. Only admin can use the commands for other users.

Then, to run a command, send a POST request to the following URL:
`http://localhost:5280/api/<command_name>`

It's also possible to enable unrestricted access to some commands from group
of IP addresses by using option `admin_ip_access` by having fragment like
this in configuration file:

 modules:
   mod_http_api:
     admin_ip_access: admin_ip_access_rule

 access:
   admin_ip_access_rule:
     admin_ip_acl:
       - command1
       - command2
       ## use `all` to give access to all commands

 acl:
   admin_ip_acl:
     ip:
       - 127.0.0.1/8

## mod_http_fileserver

This simple module serves files from the local disk over HTTP.

Options:

**`docroot: Path`**:   Directory to serve the files.

**`accesslog: Path`**:   File to log accesses using an Apache-like format. No log will be
 recorded if this option is not specified.

**`directory_indices: [Index, ...]`**:   Indicate one or more directory index files, similarly to Apache’s
 DirectoryIndex variable. When a web request hits a directory instead
 of a regular file, those directory indices are looked in order, and
 the first one found is returned.

**`custom_headers: {Name: Value}`**:   Indicate custom HTTP headers to be included in all responses.
 Default value is: `[]`

**`content_types: {Name: Type}`**:   Specify mappings of extension to content type. There are several
 content types already defined, with this option you can add new
 definitions, modify or delete existing ones. To delete an existing
 definition, simply define it with a value: ‘undefined’.

**`default_content_type: Type`**:   Specify the content type to use for unknown extensions. Default
 value is ‘application/octet-stream’.

This example configuration will serve the files from the local directory
`/var/www` in the address `http://example.org:5280/pub/archive/`. In
this example a new content type `ogg` is defined, `png` is redefined,
and `jpg` definition is deleted. To use this module you must enable it:

 modules:
   ...
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
       .jpg: undefined
     default_content_type: text/html
   ...

And define it as a handler in the HTTP service:

 listen:
   ...
   -
     port: 5280
     module: ejabberd_http
     request_handlers:
       ...
       /pub/archive: mod_http_fileserver
       ...
   ...

## mod_http_upload

This module allows for requesting permissions to upload a file via HTTP
as described in [`XEP-0363`][120]. If the request is accepted, the
client receives a URL for uploading the file and another URL from which
that file can later be downloaded.

In order to use this module, it must be configured as a
`request_handler` for an `ejabberd_http` listener.

Options:

**`host: HostName`**: This option defines the JID for the HTTP upload service. The keyword
“@HOST@” is replaced with the virtual host name. Default:
`upload.@HOST@`.

**`name: Text`**: This option defines the Service Discovery name for the HTTP upload
service. Default: `HTTP File Upload`.

**`access: AccessName`**: This option defines the access rule to limit who is permitted to use
the HTTP upload service. The default value is `local`. If no access rule
of that name exists, no user will be allowed to use the service.

**`max_size: Size`**: This option limits the acceptable file size. Either a number of bytes
(larger than zero) or `infinity` must be specified. Default:
`104857600`.

**`secret_length: Length`**: This option defines the length of the random string included in the
GET and PUT URLs generated by `mod_http_upload`. The minimum length is
`8` characters, but it is recommended to choose a larger value. Default:
`40`.

**`jid_in_url: node|sha1`**: When this option is set to `node`, the node identifier of the user's
JID (i.e., the user name) is included in the GET and PUT URLs generated
by `mod_http_upload`. Otherwise, a SHA-1 hash of the user's bare JID is
included instead. Default: `sha1`.

**`thumbnail: true|false`**: This option specifies whether ejabberd should create thumbnails of
uploaded images. If a thumbnail is created, a `<thumbnail/>` element
that contains the download `<uri/>` and some metadata is returned with
the PUT response. Default: `false`.

**`file_mode: Mode`**: This option defines the permission bits of uploaded files. The bits
are specified as an octal number (see the `chmod(1)` manual page) within
double quotes. For example: `"0644"`. Default: `undefined`.

**`dir_mode: Mode`**: This option defines the permission bits of the `docroot` directory and
any directories created during file uploads. The bits are specified as
an octal number (see the `chmod(1)` manual page) within double quotes.
For example: `"0755"`. Default: `undefined`.

**`docroot: Path`**: Uploaded files are stored below the directory specified (as an
absolute path) with this option. The keyword `@HOME@` is replaced with
the home directory of the user running ejabberd, and the keyword
`@HOST@` with the virtual host name. Default: `"@HOME@/upload"`.

**`put_url: URL`**: This option specifies the initial part of the PUT URLs used for file
uploads. The keyword `@HOST@` is replaced with the virtual host name.
Default: `"https://@HOST@:5443"`. *Note: Different virtual hosts cannot
use the same PUT URL.*

**`get_url: URL`**: This option specifies the initial part of the GET URLs used for
downloading the files. By default, it is set to the same value as the
`put_url`. The keyword `@HOST@` is replaced with the virtual host name.
*Note: If GET requests are handled by `mod_http_upload`, the `get_url`
must match the `put_url`.* Setting it to a different value only makes
sense if an external web server or `mod_http_fileserver` is used to
serve the uploaded files.

**`service_url: URL`**:
WARNING: **The `service_url` option is deprecated** and will be unsupported
in future releases, better use the `external_secret` option instead.
If a `service_url` is specified, upload slot requests are forwarded to
this external service instead of being handled by `mod_http_upload`
itself. Whenever an upload slot request is accepted as per the `access`
rule, a HTTP GET query with `jid`, `name`, `size`, and `content_type`
fields is issued. In order to accept the request, the service must
return an HTTP status code of 200 or 201 and two lines of text/plain
output. The first line is forwarded to the XMPP client as the PUT URL,
the second line as the GET URL. In order to reject the request, the
service should return a status code of 402 (`resource-constraint`), 403
(`not-allowed`), or 413 (`not-acceptable`). Default: `undefined`.

**`custom_headers: {Name: Value}`**: This option specifies additional header fields to be included in all
HTTP responses. Default: `[]`.

**`rm_on_unregister: true|false`**: This option specifies whether files uploaded by a user should be
removed when that user is unregistered. Default: `true`.

**`external_secret: "text"`**: This option makes it possible to offload
all HTTP Upload processing to a separate HTTP server. Both ejabberd and
the HTTP server should share this secret and behave exactly as described
at [Prosody's mod_http_upload_external](https://modules.prosody.im/mod_http_upload_external.html)
in the 'Implementation' section. Default: `undefined`.

Example:

 certfiles:
   - /etc/ejabberd/certificate.pem

 listen:
   ...
   -
     port: 5443
     module: ejabberd_http
     tls: true
     request_handlers:
       ...
       upload: mod_http_upload
       ...
   ...

 modules:
   ...
   mod_http_upload:
     docroot: /ejabberd/upload
     put_url: "https://@HOST@:5443/upload"
   ...

And using a separate HTTP server to host the files:

 modules:
   ...
   mod_http_upload:
     put_url: https://separate.http.server/upload
     external_secret: "foo bar baz"
   ...

## mod_http_upload_quota

This module adds quota support for `mod_http_upload`.

Options:

**`max_days: Days`**: If a number larger than zero is specified, any files (and directories)
older than this number of days are removed from the subdirectories of the
`docroot` directory, once per day. Default: `infinity`.

**`access_hard_quota: AccessName`**: This option defines which access rule is used to specify the "hard
quota" for the matching JIDs. That rule must yield a positive number for
any JID that is supposed to have a quota limit. This is the number of
megabytes a corresponding user may upload. When this threshold is
exceeded, ejabberd deletes the oldest files uploaded by that user until
their disk usage equals or falls below the specified soft quota (see
below).

**`access_soft_quota: AccessName`**: This option defines which access rule is used to specify the "soft
quota" for the matching JIDs. That rule must yield a positive number of
megabytes for any JID that is supposed to have a quota limit. See the
description of the `access_hard_quota` option for details.

Note: It's not necessary to specify the `access_hard_quota` and
`access_soft_quota` options in order to use the quota feature. You can
stick to the default names and just specify access rules such as those
in the following example.

 shaper_rules:
   ...
   soft_upload_quota:
     - 1000: all # MiB
   hard_upload_quota:
     - 1100: all # MiB
   ...

 modules:
   ...
   mod_http_upload: {}
   mod_http_upload_quota:
     max_days: 100
   ...

## ejabberd_http_ws

This module enables XMPP communication over Websocket connection as
described in [`RFC 7395`][71].

### Enabling Websocket support

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
placed in general section of config file:

- **`websocket_origin: ignore|URL`**: This option enables validation for `"Origin"` header
to protect against connections from other domains than given in config.
In this way, the lower layer load balancer can get chosen for a specific
ejabberd implementation while still providing a secure websocket connection.
Default value of this option is `ignore`.
Example value of URL: `"https://test.example.org:8081"`

- **`websocket_ping_interval: Seconds`**: Defines time between pings send by server to client (Websocket level
protocol pings are used for this) to keep connection active. If client
won't respond to two corresponding pings connection will be assumed as
closed. Value of `0` can be used to disable it feature. This options
do make server send pings only for connections using rfc compliant
protocol, for older style connections server expects that whitespace
pings would be used for this purpose. Default value of this option
is set to 60.

- **`websocket_timeout: Seconds`**: Amount of time without any communication after which connection
would be closed. The specified number of seconds must be larger than 0.
This option is set to 300 by default.

### Discovery

You also need to configure DNS SRV records properly so clients can
easily discover Websocket service for your XMPP domain. Refer to
[XEP-0156](https://xmpp.org/extensions/xep-0156.html).

Example DNS TXT configuration for Websocket:

    _xmppconnect IN TXT "[ _xmpp-client-websocket=wss://web.example.com:443/ws ]"

### Testing Websocket

A test client can be found on Github: [Websocket test client](https://github.com/processone/xmpp-websocket-client)

<!-- TODO:: We should probably embed a test Websocket client on the Websocket info get page. -->

## mod_jidprep

This module allows XMPP clients to ask the server to normalize a JID as
per the rules specified in [`RFC 6122: XMPP Address Format`][129]. This
might be useful for clients in certain constrained environments, or for
testing purposes.

Options:

**`access: AccessName`**:   This option defines which access rule will be
 used to control who is allowed to use this service. The default
 is `local`.

## mod_last

### Description

This module adds support for Last Activity
([`XEP-0012`][75]). It can be used
to discover when a disconnected user last accessed the server, to know
when a connected user was last active on the server, or to query the
uptime of the `ejabberd` server.

### Options

**`db_type: mnesia|sql`**:   Define the type of storage where the module will create the tables and store user information. The default is the storage defined by the global option `default_db`, or `mnesia` if omitted. If `sql` value is defined, make sure you have defined the database, see [database](#database-and-ldap-configuration).

**`use_cache: false|true`**:   Use this option and related ones as explained in section [Caching](#caching).

### Example Configuration

``` yaml
modules:
  mod_last: {}
```

### Implementation notes

You may want to disable that module depending on several parameters:

- **Privacy:** You may not want to store the last presence of your users
  in database.
- **Performance:** If you have usage pattern with large spike of
  disconnect, you may want to disable that module to limit traffic
  spike on your `mod_last` database backend.

Note that you may want also to purge last activity that is too old to
limit the storage size of those data.

## mod_mam

This module implements Message Archive Management as described in [`XEP-0313`][114]. Versions 0.2 and 0.3 are supported at the moment. Compatible XMPP clients can use it to store their chat history on the server.

Options:

**`assume_mam_usage: true|false`**: This option determines how
  ejabberd's stream management code handles unacknowledged messages
  when the connection is lost. Usually, such messages are either
  bounced or resent. However, neither is done for messages that were
  stored in the user's MAM archive if this option is set to `true`. In
  this case, ejabberd assumes those messages will be retrieved from
  the archive. The default is `false`.

**`compress_xml: true|false`**: When enabled, new messages added to
  archives are compressed using a custom compression algorithm (for
  details see the [`ejabberd 18.12 release
  notes`](https://www.process-one.net/blog/ejabberd-18-12/)). This feature
  works only with `db_type: sql`. The default value is `false`.

**`db_type: mnesia|sql`**: Define the type of storage where the module
  will create the tables and store user information. The default is
  the storage defined by the global option `default_db`, or `mnesia`
  if omitted. If `sql` value is defined, make sure you have defined
  the database, see [database](#database-and-ldap-configuration).
  Note: If `mnesia` is used, the total size of all MAM archives cannot
  exceed 2 GB. The `delete_old_mam_messages` command could be run
  periodically to make sure the `mnesia` data won't grow beyond that
  limit. To support larger archives, `sql` storage must be used.

**`default: always|never|roster`**: The option defines default policy
  for chat history. When `always` is set every chat message is
  stored. With `roster` only chat history with contacts from user's
  roster is stored. `never` fully disables chat history. Note that a
  client can change its policy via protocol commands. The default is
  `never`.

**`request_activates_archiving: true|false`**: If this option is
  enabled, no messages are stored for a user until their client issue
  a MAM request, regardless of the value of the `default` option. Once
  the server received a request, that user's messages are archived as
  usual. The default is `false`.

**`use_cache: false|true`**: Use this option and related ones as
  explained in section [Caching](#caching).

**`user_mucsub_from_muc_archive: false|true`**: this option is used
to enable storage optimization when using MucSub on large chat rooms along
with MAM. The default is `false` (disabled), to keep former behaviour as
default.

## mod_mix

This module is an experimental implementation Mediated Information
eXchange (MIX) as described in [`XEP-0369`][122]. Our implementation is
based on version 0.13.0.

This feature was added in ejabberd 16.03 as an experimental feature,
updated in 19.02, and is not yet ready to use in production.

To learn more about how to use that feature, you can refer to our
tutorial:
[Getting started with XEP-0369: Mediated Information eXchange (MIX) v0.1](../tutorials/mix-010.md).

## mod_metrics

This module sends events to external backend (by now it only
supports grapherl). Supported events are: sm_register_connection,
sm_remove_connection, user_send_packet, user_receive_packet,
s2s_send_packet, s2s_receive_packet, register_user, remove_user,
offline_message.

When enabled, every call to these hooks triggers a counter event
to the configured backend.

Backend is not configurable yet, by now it requires to be grapherl
running in localhost and listening to port 11111.

## mod_muc

This module provides a Multi-User Chat
([`XEP-0045`][77]) service. Users
can discover existing rooms, join or create them. Occupants of a room
can chat in public or have private chats.

Some of the features of Multi-User Chat:

- Sending public and private messages to room occupants.

- Inviting other users to a room.

- Setting a room subject.

- Creating password protected rooms.

- Kicking and banning occupants.

The MUC service allows any Jabber ID to register a nickname, so nobody
else can use that nickname in any room in the MUC service. To register a
nickname, open the Service Discovery in your XMPP client and register in
the MUC service.

This module supports clustering and load balancing. One module can be
started per cluster node. Rooms are distributed at creation time on all
available MUC module instances. The multi-user chat module is clustered
but the rooms themselves are not clustered nor fault-tolerant: if the
node managing a set of rooms goes down, the rooms disappear and they
will be recreated on an available node on first connection attempt.

Module options:

**`host: HostName`**:   This option defines the Jabber ID of the service. If the `host`
 option is not specified, the Jabber ID will be the hostname of the
 virtual host with the prefix ‘`conference.`’. The keyword “@HOST@”
 is replaced at start time with the real virtual host name.

**`db_type: mnesia|sql`**:   Define the type of storage where the module will create the tables and store user information. The default is the storage defined by the global option `default_db`, or `mnesia` if omitted. If `sql` value is defined, make sure you have defined the database, see [database](#database-and-ldap-configuration).

**`access: AccessName`**:   You can specify who is allowed to use the Multi-User Chat service.
 By default everyone is allowed to use it.

**`access_create: AccessName`**:   To configure who is allowed to create new rooms at the Multi-User
 Chat service, this option can be used. By default any account in the
 local ejabberd server is allowed to create rooms.

**`access_persistent: AccessName`**:   To configure who is allowed to modify the ’persistent’ room option.
 By default any account in the local ejabberd server is allowed to
 modify that option.

**`access_mam: AccessName`**:   To configure who is allowed to modify the ’mam’ room option.
 By default any account in the local ejabberd server is allowed to
 modify that option.

**`access_admin: AccessName`**:   This option specifies who is allowed to administrate the Multi-User
 Chat service. The default value is `none`, which means that only the
 room creator can administer their room. The administrators can send a
 normal message to the service JID, and it will be shown in all
 active rooms as a service message. The administrators can send a
 groupchat message to the JID of an active room, and the message will
 be shown in the room as a service message.

**`access_register: AccessName`**:   This option specifies who is allowed to
 register nickname within the Multi-User Chat service. The default is `all`
 for backward compatibility, which means that any user is allowed to
 register any free nick.

**`history_size: Size`**:   A small history of the current discussion is sent to users when they
 enter the room. With this option you can define the number of
 history messages to keep and send to users joining the room. The
 value is an integer. Setting the value to `0` disables the history
 feature and, as a result, nothing is kept in memory. The default
 value is `20`. This value is global and thus affects all rooms on
 the service.

**`max_users: Number`**:   This option defines at the service level, the maximum number of
 users allowed per room. It can be lowered in each room configuration
 but cannot be increased in individual room configuration. The
 default value is 200.

**`max_users_admin_threshold: Number`**:   This option defines the number of service admins or room owners
 allowed to enter the room when the maximum number of allowed
 occupants was reached. The default limit is 5.

**`max_user_conferences: Number`**:   This option defines the maximum number of rooms that any given user
 can join. The default value is 100. This option is used to prevent
 possible abuses. Note that this is a soft limit: some users can
 sometimes join more conferences in cluster configurations.

**`max_room_id: Number`**:   This option defines the maximum number of characters that Room ID
 can have when creating a new room. The default value is to not
 limit: `infinity`.

**`regexp_room_id: String`**:   This option defines the regular expression that a Room ID must satisfy
 to allow the room creation. The default value is the empty string.

**`max_room_name: Number`**:   This option defines the maximum number of characters that Room Name
 can have when configuring the room. The default value is to not
 limit: `infinity`.

**`max_room_desc: Number`**:   This option defines the maximum number of characters that Room
 Description can have when configuring the room. The default value is
 to not limit: `infinity`.

**`max_rooms_discoitems: Number`**:   When there are more rooms than this Number,
 only the non-empty ones are returned in a Service Discovery query.
 The default value is: 100.

**`min_message_interval: Number`**:   This option defines the minimum interval between two messages send
 by an occupant in seconds. This option is global and valid for all
 rooms. A decimal value can be used. When this option is not defined,
 message rate is not limited. This feature can be used to protect a
 MUC service from occupant abuses and limit number of messages that
 will be broadcasted by the service. A good value for this minimum
 message interval is 0.4 second. If an occupant tries to send
 messages faster, an error is send back explaining that the message
 has been discarded and describing the reason why the message is not
 acceptable.

**`min_presence_interval: Number`**:   This option defines the minimum of time between presence changes
 coming from a given occupant in seconds. This option is global and
 valid for all rooms. A decimal value can be used. When this option
 is not defined, no restriction is applied. This option can be used
 to protect a MUC service for occupants abuses. If an occupant tries
 to change its presence more often than the specified interval, the
 presence is cached by `ejabberd` and only the last presence is
 broadcasted to all occupants in the room after expiration of the
 interval delay. Intermediate presence packets are silently
 discarded. A good value for this option is 4 seconds.

**`max_users_presence: Number`**:   This option defines after how many users in the room, it is
    considered overcrowded. When a MUC room is considered overcrowed,
    presence broadcasts are limited to reduce load, traffic and excessive
    presence 'storm' received by participants.

**`default_room_options: {OptionName: OptionValue}`**:   This module option allows to define the desired default room
 options. Note that the creator of a room can modify the options of
 his room at any time using an XMPP client with MUC capability.
 All of those room options can be set to `true` or `false`, except
 `password` and `title` which are strings, and `max_users` that is
 integer. The
 available room options and the default values are:

- **`allow_change_subj: true|false`**:   Allow occupants to change the subject.

- **`allow_private_messages: true|false`**:   Occupants can send private messages to other occupants.

- **`allow_private_messages_from_visitors: anyone|moderators|nobody`**:   Visitors can send private messages to other occupants.

- **`allow_query_users: true|false`**:   Occupants can send IQ queries to other occupants.

- **`allow_subscription: true|false`**:   Allow users to subscribe to room events as described in
 [`Multi-User Chat Subscriptions`](../developer/xmpp-clients-bots/extensions/muc-sub.md).

- **`allow_user_invites: false|true`**:   Allow occupants to send invitations.

- **`allow_visitor_nickchange: true|false`**:   Allow visitors to change nickname.

- **`allow_visitor_status: true|false`**:   Allow visitors to send status text in presence updates. If
     disallowed, the `status` text is stripped before broadcasting
     the presence update to all the room occupants.

- **`anonymous: true|false`**:   The room is anonymous: occupants don’t see the real JIDs of
     other occupants. Note that the room moderators can always see
     the real JIDs of the occupants.

- **`captcha_protected: false|true`**:   When a user tries to join a room where they have no affiliation
     (not owner, admin or member), the room requires them to fill a
     CAPTCHA challenge (see section [CAPTCHA](#captcha)) in order to accept their
     join in the room.

- **`lang: Language`**:   Preferred language for the discussions in the room.
     The language format should conform to RFC 5646.

- **`logging: false|true`**:   The public messages are logged using `mod_muc_log`.

- **`mam: false|true`**:   Enable message archiving. Implies `mod_mam` is enabled.

- **`max_users: Number`**:   Maximum number of occupants in the room. The default value is 200.

- **`members_by_default: true|false`**:   The occupants that enter the room are participants by default,
     so they have ’voice’.

- **`members_only: false|true`**:   Only members of the room can enter.

- **`moderated: true|false`**:   Only occupants with ’voice’ can send public messages.

- **`password: roompass123`**:   Password of the room. You may want to enable the next option
     too.

- **`password_protected: false|true`**:   The password is required to enter the room.

- **`persistent: false|true`**:   The room persists even if the last participant leaves.

- **`presence_broadcast: [moderator, participant, visitor]`**:   List of roles for which presence is broadcasted.
 The list can contain one or several of: moderator, participant, visitor.

- **`public: true|false`**:   The room is public in the list of the MUC service, so it can be
     discovered. MUC admins and room participants will see private rooms in Service Discovery,
     and the room name will have the word: "private".

- **`public_list: true|false`**:   The list of participants is public, without requiring to enter
     the room.

- **`title: Room Title`**:   A human-readable title of the room.

Examples:

- In the first example everyone is allowed to use the Multi-User Chat
 service. Everyone will also be able to create new rooms but only the
 user `admin@example.org` is allowed to administrate any room. In
 this example they are also a global administrator. When
 `admin@example.org` sends a message such as ‘Tomorrow, the XMPP
 server will be moved to new hardware. This will involve service
 breakdowns around 23:00 UMT. We apologise for this inconvenience.’
 to `conference.example.org`, it will be displayed in all active
 rooms. In this example the history feature is disabled.

  acl:
    admin:
      user:
        - admin@example.org

  access_rules:
    muc_admin:
      - allow: admin

  modules:
    ...
    mod_muc:
      access: all
      access_create: all
      access_admin: muc_admin
      history_size: 0
    ...

- In the second example the Multi-User Chat service is only accessible
 by paying customers registered on our domains and on other servers.
 Of course the administrator is also allowed to access rooms. In
 addition, he is the only authority able to create and administer
 rooms. When `admin@example.org` sends a message such as ‘Tomorrow,
 the Jabber server will be moved to new hardware. This will involve
 service breakdowns around 23:00 UMT. We apologise for this
 inconvenience.’ to `conference.example.org`, it will be displayed in
 all active rooms. No `history_size` option is used, this means that
 the feature is enabled and the default value of 20 history messages
 will be send to the users.

  acl:
    paying_customers:
      user:
        - customer1@example.net
        - customer2@example.com
        - customer3@example.org
    admin:
      user:
        - admin@example.org

  access_rules:
    muc_admin
      - allow: admin
    muc_access:
      - allow: paying_customers
      - allow: admin

  modules:
    ...
    mod_muc:
      access: muc_access
      access_create: muc_admin
      access_admin: muc_admin
    ...

- In the following example, MUC anti abuse options are used. An
 occupant cannot send more than one message every 0.4 seconds and
 cannot change its presence more than once every 4 seconds. The
 Room IDs can contain only letters and numbers. The
 length of Room IDs and Room Names are limited to 20 characters, and
 Room Description to 300 characters. No ACLs are defined, but some
 user restriction could be added as well:

  modules:
    ...
    mod_muc:
      min_message_interval: 0.4
      min_presence_interval: 4
      regexp_room_id: "^[a-z0-9]+$"
      max_room_id: 20
      max_room_name: 20
      max_room_desc: 300
    ...

- This example shows how to use `default_room_options` to make sure
 the newly created rooms have by default those options.

  modules:
    ...
    mod_muc:
      access: muc_access
      access_create: muc_admin
      default_room_options:
        allow_change_subj: false
        allow_query_users: true
        allow_private_messages: true
        members_by_default: false
        presence_broadcast: [visitor, moderator]
        title: "New chatroom"
        anonymous: false
      access_admin: muc_admin
    ...

## mod_muc_log

This module enables optional logging of Multi-User Chat (MUC) public
conversations to HTML. Once you enable this module, users can join a
room using a MUC capable XMPP client, and if they have enough
privileges, they can request the configuration form in which they can
set the option to enable room logging.

Features:

- Room details are added on top of each page: room title, JID, author,
 subject and configuration.

- The room JID in the generated HTML is a link to join the room (using
 [`XMPP URI`][79]).

- Subject and room configuration changes are tracked and displayed.

- Joins, leaves, nick changes, kicks, bans and ‘/me’ are tracked and
 displayed, including the reason if available.

- Generated HTML files are XHTML 1.0 Transitional and CSS compliant.

- Timestamps are self-referencing links.

- Links on top for quicker navigation: Previous day, Next day, Up.

- CSS is used for style definition, and a custom CSS file can be used.

- URLs on messages and subjects are converted to hyperlinks.

- Timezone used on timestamps is shown on the log files.

- A custom link can be added on top of each page.

Options:

**`access_log: AccessName`**:   This option restricts which occupants are allowed to enable or
 disable room logging. The default value is `muc_admin`. Note for
 this default setting you need to have an access rule for `muc_admin`
 in order to take effect.

**`cssfile: false|URL`**:   With this option you can set whether the HTML files should have a
 custom CSS file or if they need to use the embedded CSS file.
 Allowed values are `false` and an URL to a CSS file. With the first
 value, HTML files will include the embedded CSS code. With the
 latter, you can specify the URL of the custom CSS file (for example:
 `http://example.com/my.css`). The default value is `false`.

**`dirname: room_jid|room_name`**:   Allows to configure the name of the room directory. Allowed values
 are `room_jid` and `room_name`. With the first value, the room
 directory name will be the full room JID. With the latter, the room
 directory name will be only the room name, not including the MUC
 service name. The default value is `room_jid`.

**`dirtype: subdirs|plain`**:   The type of the created directories can be specified with this
 option. Allowed values are `subdirs` and `plain`. With the first
 value, subdirectories are created for each year and month. With the
 latter, the names of the log files contain the full date, and there
 are no subdirectories. The default value is `subdirs`.

**`file_format: html|plaintext`**:   Define the format of the log files: `html` stores in HTML format,
 `plaintext` stores in plain text. The default value is `html`.

**`file_permissions: {mode: Mode, group: Group}`**:   Define the permissions that must be used when creating the log
 files: the number of the mode, and the numeric id of the group that
 will own the files. The default value is `{644, 33}`.

**`outdir: Path`**:   This option sets the full path to the directory in which the HTML
 files should be stored. Make sure the `ejabberd` daemon user has
 write access on that directory. The default value is `www/muc`.

**`spam_prevention: true|false`**:   To prevent spam, the `spam_prevention` option adds a special
 attribute to links that prevent their indexation by search engines.
 The default value is `true`, which mean that nofollow attributes
 will be added to user submitted links.

**`timezone: local|universal`**:   The time zone for the logs is configurable with this option. Allowed
 values are `local` and `universal`. With the first value, the local
 time, as reported to Erlang by the operating system, will be used.
 With the latter, GMT/UTC time will be used. The default value is
 `local`.

**`top_link: {URL: Text}`**:   With this option you can customize the link on the top right corner
 of each log file. The default value is `{/, Home}`.

**`url: URL`**: A top level URL where a client is able to access logs of a particular conference.
        The conference name is appended to the URL if `dirname` is `room_name` or
 a conference JID is appended to the URL if `dirname` is `room_jid`.

Examples:

- In the first example any room owner can enable logging, and a custom
 CSS file will be used, `http://example.com/my.css`. The names of the
 log files will contain the full date, and there will be no
 subdirectories. The log files will be stored in /var/www/muclogs,
 and the time zone will be GMT/UTC. Finally, the top link will be
 `<a href="http://www.jabber.ru/">Jabber.ru</a>`.

  access_rules:
    - muc: allow

  modules:
    ...
    mod_muc_log:
      access_log: muc
      cssfile: http://example.com/my.css
      dirtype: plain
      dirname: room_jid
      outdir: /var/www/muclogs
      timezone: universal
      spam_prevention: true
      top_link:
        http://www.jabber.ru/: Jabber.ru
    ...

- In the second example only `admin1@example.org` and
 `admin2@example.net` can enable logging, and the embedded CSS file
 will be used. The names of the log files will only contain the day
 (number), and there will be subdirectories for each year and month.
 The log files will be stored in /var/www/muclogs, and the local time
 will be used. Finally, the top link will be the default
 `<a href="/">Home</a>`.

  acl:
    admin:
      user:
        - admin1@example.org
        - admin2@example.net
  access_rules:
    muc_log:
      - allow: admin

  modules:
    ...
    mod_muc_log:
      access_log: muc_log
      cssfile: false
      dirtype: subdirs
      file_permissions:
        mode: 644
        group: 33
      outdir: /var/www/muclogs
      timezone: local
    ...

- In the third example log files are served by `mod_http_fileserver` module and
    are available at `https://domain.tld:5443/chatlogs`:

          listen:
            ...
            -
              module: ejabberd_http
              port: 5443
              tls: true
              request_hanlers:
                /chatlogs: mod_http_fileserver
                ...

          modules:
            ...
            mod_http_fileserver:
              docroot: /var/www/muclogs
              ...
            mod_muc_log:
              url: https://domain.tld:5443/chatlogs
              outdir: /var/www/muclogs
              ...

## mod_multicast

This module implements a service for Extended Stanza Addressing ([`XEP-0033`][109])

### Enabling multicast

Edit ejabberd.yml and add the module to the list of modules:

    mod_multicast: []

### Configurable options

- **`host`**: Define the hostname of the service. Default value: "multicast.SERVER"

- **`access`**: Specify who can send packets to the multicast service. Default value: all

- **`limits`**: Specify a list of custom limits which override the default ones defined in XEP-0033.
    Limits are defined per sender type and stanza type, where:
  - The sender type can be: local or remote.
  - The stanza type can be: message or presence.
  - The number can be a positive integer or the key word infinite.
  - Default values:

 local:
   message: 100
   presence: 100
 remote:
   message: 20
   presence: 20

### Example configuration

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

### Service check

You have to restart ejabberd after adding or modifying `mod_multicast` configuration.

To verify the service is running, login to ejabberd with a XMPP client, open the service discovery and check that
there's a service called "Multicast".

### How to simulate group chat with multicast

#### For stanza sender

Instead of sending two stanzas, like this:

``` xml
<message to='user2@localhost' type='chat'>
   <body>Hello, world!</body>
</message>
<message to='user3@localhost' type='chat'>
   <body>Hello, world!</body>
</message>
```

now you can send a stanza like this:

``` xml
<message to='multicast.localhost' type='chat'>
   <addresses xmlns='http://jabber.org/protocol/address'>
       <address type='to' jid='user2@localhost'/>
       <address type='cc' jid='user3@localhost'/>
   </addresses>
   <body>Hello, world!</body>
</message>
```

#### For the stanza receivers

They will receive something like this:

``` xml
<message from='user1@localhost/tka1'
        to='user3@localhost'
        type='chat'>
  <addresses xmlns='http://jabber.org/protocol/address'>
    <address type='cc'
        jid='user3@localhost'/>
  </addresses>
  <body>Hello, world!</body>
</message>
```

The addresses tag let the receiver knows that there were multiple
recipients and can:

- Display it properly in client interface.
- Reply to all recipients.

## mod_offline

### Description

This module implements offline message storage ([`XEP-0160`][80]) and
flexible offline message retrieval
([`XEP-0013`][121]). This means
that all messages sent to an offline user will be stored on the server
until that user comes online again. Thus it is very similar to how
email works. A user is considered offline if no session presence
priority > 0 are currently open.

Note that `ejabberdctl` has a command to delete expired messages (see
section [Managing: ejabberdctl](../admin/guide/managing.md#ejabberdctl)).

### Options

**`db_type: mnesia|sql`**: Define the type of storage where the module will create the tables
and store user information. The default is the storage defined by the
global option `default_db`, or `mnesia` if omitted. If `sql` value is defined, make sure you have defined the database, see
[database](#database-and-ldap-configuration).

**`access_max_user_messages: AccessName`**: This option defines which access rule (atom) will be enforced to
 limit the maximum number of offline messages that a user can have
 (quota).  When a user has too many offline messages, any new
 messages that they receive are discarded, and a resource-constraint
 error is returned to the sender. The default value is
 `max_user_offline_messages`.  Then you can define an access rule
 with a syntax similar to `max_user_sessions` (see
 [Limiting Opened Sessions with ACL](#limiting-opened-sessions-with-acl)).

**`store_empty_body: true|false|unless_chat_state`**: Whether or not to store messages that lack a `<body/>` element. The default
 value is `unless_chat_state`, which tells ejabberd to store messages
 even if they lack the `<body/>` element, *unless* they only contain a
 chat state notification (as defined in
 [`XEP-0085`](https://xmpp.org/extensions/xep-0085.html)).

**`pool_size: Size`**: This option specifies the size of the worker pool for storing
 offline messages. The allowed values are positive integers.
 Default value: `16`.

**`bounce_groupchat: false|true`**: This option is use the disable an optimisation that avoids bouncing error messages
when groupchat messages could not be stored as offline. It will reduce chat room load, without any drawback in standard
use cases. You may change default value only if you have a custom module which uses offline hook after mod_offline.
This option  can be useful for both standard MUC and MucSub, but the bounce is much more likely to happen in the
context of MucSub, so it is even more important to have it on large MucSub services.The default is `false`, meaning
the optimisation is enabled.

**`use_mam_for_storage: false|true`**: This is an experimental option. Enabling this option will make mod_offline not
use the former spool table for storing MucSub offline messages, but will use the archive table instead. This use of
the archive table is cleaner and it makes it possible for clients to slowly drop the former offline use case and rely
on message archive instead. It also further reduce the storage required when you enabled MucSub. Enabling this option
has a known drawback for the moment: most of flexible message retrieval queries don’t work (those that allow
retrieval/deletion of messages by id), but this specification is not widely used. The default value is `false` to
keep former behaviour as default and ensure this option is disabled.

### Example Configuration

This example allows power users to have as much as 5000 offline
messages, administrators up to 2000, and all the other users up to
100.

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

### Implementation notes

You have several approach for a client to learn what happened when it was offline:

- offline storage: This is adequate especially when a single client is
  typically used at a time.
- message archive (MAM): This is a new archiving feature. A client
  (for example a mobile client) can use it to resync its own history
  with what happened when it was offline. In that case, the client is
  responsible for marking messages it considers new as "unread". See
  [mod_mam](#mod_mam).

Some clients may rely on offline and/or message archives to catch up
on messages on reconnect, but client developers should always consider
interactions with those two modules together.

## mod_ping

This module implements support for XMPP Ping
([`XEP-0199`][82]) and periodic
keepalives. When this module is enabled ejabberd responds correctly to
ping requests, as defined in the protocol.

Configuration options:

**`send_pings: true|false`**:   If this option is set to `true`, the server sends pings to connected
 clients that are not active in a given interval `ping_interval`.
 This is useful to keep client connections alive or checking
 availability. By default this option is disabled.

**`ping_interval: Seconds`**:   How often to send pings to connected clients, if the previous option
 is enabled. If a client connection does not send or receive any
 stanza in this interval, a ping request is sent to the client. The
 default value is 60 seconds.

**`ping_ack_timeout: Seconds`**:   How long to wait before deeming that a client has not answered a given
    server ping request.
 default value is 32 seconds.

**`timeout_action: none|kill`**:   What to do when a client does not answer to a server ping request in
 less than `ping_ack_timeout`. The default is to do nothing.

This example enables Ping responses, configures the module to send pings
to client connections that are inactive for 4 minutes, and if a client
does not answer to the ping in less than 32 seconds, its connection is
closed:

 modules:
   ...
   mod_ping:
     send_pings: true
     ping_interval: 240
     timeout_action: kill
   ...

## mod_pres_counter

This module detects flood/spam in presence subscription stanza traffic.
If a user sends or receives more of those stanzas in a time interval,
the exceeding stanzas are silently dropped, and warning is logged.

Configuration options:

**`count: StanzaNumber`**:   The number of subscription presence stanzas (subscribe, unsubscribe,
 subscribed, unsubscribed) allowed for any direction (input or
 output) per time interval. Please note that two users subscribing to
 each other usually generate 4 stanzas, so the recommended value is 4
 or more. The default value is: 5.

**`interval: Seconds`**:   The time interval defined in seconds. The default value is 60.

This example enables the module, and allows up to 5 presence
subscription stanzas to be sent or received by the users in 60 seconds:

 modules:
   ...
   mod_pres_counter:
     count: 5
     interval: 60
   ...

## mod_privacy

This module implements
[XEP-0016: Privacy Lists\`][83]. If
end users have support for it in their XMPP client, they will be able
to:

- Retrieving one’s privacy lists.
- Adding, removing, and editing one’s privacy lists.
- Setting, changing, or declining active lists.
- Setting, changing, or declining the default list (i.e., the list that is active by default).
- Allowing or blocking messages based on JID, group, or subscription type (or globally).
- Allowing or blocking inbound presence notifications based on JID, group, or subscription type (or globally).
- Allowing or blocking outbound presence notifications based on JID, group, or subscription type (or globally).
- Allowing or blocking IQ stanzas based on JID, group, or subscription type (or globally).
- Allowing or blocking all communications based on JID, group, or subscription type (or globally).

Options:

**`db_type: mnesia|sql`**:   Define the type of storage where the module will create the tables and store user information. The default is the storage defined by the global option `default_db`, or `mnesia` if omitted. If `sql` value is defined, make sure you have defined the database, see [database](#database-and-ldap-configuration).

**`use_cache: false|true`**:   Use this option and related ones as explained in section [Caching](#caching).

## mod_private

This module adds support for Private XML Storage
([`XEP-0049`][85]):

> Using this method, XMPP entities can store private data on the server
> and retrieve it whenever necessary. The data stored might be anything,
> as long as it is valid XML. One typical usage for this namespace is
> the server-side storage of client-specific preferences; another is
> Bookmark Storage
> ([`XEP-0048`][86]).

Options:

**`db_type: mnesia|sql`**:   Define the type of storage where the module will create the tables and store user information. The default is the storage defined by the global option `default_db`, or `mnesia` if omitted. If `sql` value is defined, make sure you have defined the database, see [database](#database-and-ldap-configuration).

**`use_cache: false|true`**:   Use this option and related ones as explained in section [Caching](#caching).

## mod_privilege

This module is an implementation of ([`XEP-0356`][124]). This extension allows components to have privileged access to
other entity data (send messages on behalf of the server or on behalf of a user, get/set user roster, access presence
information, etc.). This may be used to write powerful external components, for example implementing an external PEP
([XEP-0163: Personal Eventing Protocol](https://xmpp.org/extensions/xep-0163.html)) or MAM
([XEP-0313: Message Archive Management](https://xmpp.org/extensions/xep-0313.html)) service.

### Permission list

By default a component does not have any privileged access. It is worth noting that the permissions listed below
grant access to the component to a specific data type for all users of the domain.

When configuring `mod_privilege` module, here are the available permissions:

- presence
  - managed_entity: receive server user presence.
  - roster: the component is allowed to receive the presence of both the users and the contacts in their roster.
- message
  - outgoing: the component is allowed to send messages on behalf of either the server or a bare JID of server users.
- roster
  - get: read access to a user's roster.
  - set: write access to a user's roster.
  - both: read/write access to a user's roster.

You grant access to the component by providing an ACL to apply to that permission or `all` to give that access level to all
authenticated components.

### Example

If you want to grant privileged access to a component, specify it in the module configurations, e.g.:

``` yaml
module:
...
  mod_privilege:
    roster:
      get: all
    presence:
      managed_entity: all
    message:
      outgoing: all
```

In the example above, the `sat-pubsub.example.org` component can get the roster of every user of the server and send
messages on behalf of the server.

Note: You need to make sure you have a listener configured to connect your component. Check the
[listener](#listening-ports) section for more information.

Here is a more complete example, with ACL and support for namespace delegation
([XEP-0355](https://xmpp.org/extensions/xep-0355.html)):

``` yaml
listen:
...
  - port: 8888
    module: ejabberd_service
    hosts:
      sat-pubsub.example.org:
        password: mypass

acl:
...
  external_component:
    server:
      - sat-pubsub.example.org

access_rules:
...
  external_pubsub:
    - allow: external_component

module:
...
  mod_delegation:
    namespaces:
      "http://jabber.org/protocol/pubsub":
         access: external_pubsub
  mod_privilege:
    roster:
      get: external_pubsub
    presence:
      managed_entity: external_pubsub
    message:
      outgoing: external_pubsub
```

### Security issue

Privileged access gives components access to sensitive data, so permission should be granted carefully, only if you trust a component.

### Note

This module is complementary to `mod_delegation` ([`XEP-0355`][123]) but can also be used separately.

## mod_proxy65

This module implements SOCKS5 Bytestreams
([`XEP-0065`][88]). It allows
`ejabberd` to act as a file transfer proxy between two XMPP clients.

Options:

**`host: HostName`**:   This option defines the Jabber ID of the service. If the `host`
 option is not specified, the Jabber ID will be the hostname of the
 virtual host with the prefix ‘`proxy.`’. The keyword “@HOST@” is
 replaced at start time with the real virtual host name.

**`name: Text`**:   Defines Service Discovery name of the service. Default is
 `SOCKS5 Bytestreams`.

**`ip: IP`**:   This option specifies which network interface to listen for. Default
 is an IP address of the service’s DNS name, or, if fails,
 `"127.0.0.1"`.

**`port: Number`**:   This option defines port to listen for incoming connections. Default
 is 7777.

**`hostname: HostName`**:   Defines a hostname advertised by the service when establishing a
 session with clients. This is useful when you run the service behind
 a NAT. The keyword “@HOST@” is replaced with the virtual host name.
 The default is the value of `ip` option. Examples:
 `proxy.mydomain.org`, `200.150.100.50`. Note that not all clients
 understand domain names in stream negotiation, so you should think
 twice before setting domain name in this option.

**`auth_type: anonymous|plain`**:   SOCKS5 authentication type. Possible values are `anonymous` and
 `plain`. Default is `anonymous`.

**`access: AccessName`**:   Defines ACL for file transfer initiators. Default is `all`.

**`max_connections: Number`**:   Maximum number of active connections per file transfer initiator. No
 limit by default.

**`shaper: none|ShaperName`**:   This option defines shaper for the file transfer peers. Shaper with
 the maximum bandwidth will be selected. Default is `none`.

Examples:

- The simplest configuration of the module:

  modules:
    ...
    mod_proxy65: {}
    ...

- More complicated configuration.

  acl:
    admin:
      user:
        - admin@example.org
    proxy_users:
      server:
        - example.org

  access_rules:
    proxy65_access:
      - allow: proxy_users
  shaper_rules:
    proxy65_shaper:
      - none: admin
      - proxyrate: proxy_users

  shaper:
    proxyrate: 10240

  modules:
    ...
    mod_proxy65:
      host: proxy1.example.org
      name: "File Transfer Proxy"
      ip: 200.150.100.1
      port: 7778
      max_connections: 5
      access: proxy65_access
      shaper: proxy65_shaper
    ...

## mod_pubsub

This module offers a Publish-Subscribe Service
([`XEP-0060`][89]). The
functionality in `mod_pubsub` can be extended using plugins. The plugin
that implements PEP (Personal Eventing via Pubsub)
([`XEP-0163`][90]) is enabled in
the default ejabberd configuration file, and it requires `mod_caps`.

Options:

**`host: HostName`**:   This option defines the Jabber ID of the service. If the `host`
 option is not specified, the Jabber ID will be the hostname of the
 virtual host with the prefix ‘`pubsub.`’. The keyword “@HOST@” is
 replaced at start time with the real virtual host name.

If you use `mod_pubsub` with `sql` `db_type`, please ensure the
prefix contains only one dot, for example ‘`pubsub.`’, or
‘`publish.`’,.

**`access_createnode: AccessName`**:   This option restricts which users are allowed to create pubsub nodes
 using ACL and ACCESS. By default any account in the local ejabberd
 server is allowed to create pubsub nodes.

**`max_items_node: MaxItems`**:   Define the maximum number of items that can be stored in a node.
 Default value is 10.

**`max_subscriptions_node: MaxSubs`**:   Define the maximum number of subscriptions managed by a node.
 Default value is undefined; no limitation.

**`plugins: [ Plugin, ...]`**:   To specify which pubsub node plugins to use. The first one in the
 list is used by default. If this option is not defined, the default
 plugins list is: `[flat]`. PubSub clients can define which plugin to
 use when creating a node: add `type=’plugin-name’` attribute to the
 `create` stanza element.

- `flat` plugin handles the default behaviour and follows standard
      XEP-0060 implementation.

- `pep` plugin adds extension to handle Personal Eventing Protocol
     ([`XEP-0163`](https://xmpp.org/extensions/xep-0163.html)) to the PubSub
     engine. Adding pep plugin to PubSub make it handle PEP automatically.

**`nodetree: Nodetree`**:   To specify which nodetree to use. If not defined, the default pubsub
 nodetree is used: `tree`. Only one nodetree can be used per host,
 and is shared by all node plugins.

- `tree` nodetree store node configuration and relations on the
     database. `flat` nodes are stored without any relationship, and
     `hometree` nodes can have child nodes.

- `virtual` nodetree does not store nodes on database. This saves
     resources on systems with tons of nodes. If using the `virtual`
     nodetree, you can only enable those node plugins: [`flat`, `pep`] or
     [`flat`]; any other plugins configuration will not work. Also, all
     nodes will have the default configuration, and this can not be
     changed. Using “virtual” nodetree requires to start from a clean
     database, it will not work if you used the default “tree” nodetree
     before.

- `dag` nodetree provides experimental support for PubSub
     Collection Nodes
     ([`XEP-0248`](https://xmpp.org/extensions/xep-0248.html)). In that
     case you should also add “dag” node plugin as default, for example:

        plugins: [flat,pep]

**`ignore_pep_from_offline: false|true`**:   To specify whether or not we should get last published PEP items
 from users in our roster which are offline when we connect. Value is
 true or false. If not defined, pubsub assumes true so we only get
 last items of online contacts.

**`last_item_cache: false|true`**:   To specify whether or not pubsub should cache last items. Value is
 true or false. If not defined, pubsub do not cache last items. On
 systems with not so many nodes, caching last items speeds up pubsub
 and allows to raise user connection rate. The cost is memory usage,
 as every item is stored in memory.

**`default_node_config: Config`**:   To override default node configuration, regardless of node plugin. Value
 is a list of key-value definition. Node configuration still uses
 default configuration defined by node plugin, and overrides any items
 by value defined in this configurable list.

**`pep_mapping: {Key, Value}`**:   This allow to define a Key-Value list to choose defined node plugins
 on given PEP namespace. The following example will use node\_tune
 instead of node\_pep for every PEP node with tune namespace:

     modules:
       ...
       mod_pubsub:
         pep_mapping:
           http://jabber.org/protocol/tune: tune
       ...

Example of configuration that uses flat nodes as default, and allows use
of flat, hometree and pep nodes:

 modules:
   ...
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
   ...

Using relational database requires using mod\_pubsub with db_type
`sql`. Only flat, hometree and pep plugins supports SQL. The
following example shows previous configuration with SQL usage:

 modules:
   ...
   mod_pubsub:
     db_type: sql
     access_createnode: pubsub_createnode
     ignore_pep_from_offline: true
     last_item_cache: false
     plugins:
       - flat
       - pep
   ...

## mod_push

This module implements the XMPP server's part of the push notification
solution specified in [`XEP-0357`][126]. It does *not* generate, for
example, APNS or FCM notifications directly. Instead, it's designed to
work with so-called "app servers" operated by third-party vendors of
mobile apps. Those app servers will usually trigger notification
delivery to the user's mobile device using platform-dependant backend
services such as FCM or APNS.

Options:

**`include_sender: true|false`**:   If this option is set to `true`, the
 sender's JID is included with push notifications generated for
 incoming messages with a body. Default: `false`.

**`include_body: true|false|String`**:   If this option is set to
 `true`, the message text is included with push notifications
 generated for incoming messages with a body. The option can
 instead be set to a static text, in which case the specified
 text will be included in place of the actual message body. This
 can be useful to signal the app server whether the notification
 was triggered by a message with body (as opposed to other types
 of traffic) without leaking actual message contents. Default:
 `"New message"`.

**`db_type: mnesia|sql`**:   The type of storage where the module will
 store push session information. The default is the storage
 defined by the global option `default_db`, or `mnesia` if
 omitted. If `sql` is specified, make sure you have configured
 the database, see the
 [database](#database-and-ldap-configuration) section.

**`use_cache: false|true`**:   Use this option and related ones as
 explained in the section on [caching](#caching).

## mod_push_keepalive

This module tries to keep the [stream management](#mod_stream_mgmt)
session of a disconnected mobile client alive if the client enabled
[push notifications](#mod_push) for that session. However, the normal
session resumption timeout is restored once a push notification is
issued, so the session will be closed if the client doesn't respond to
push notifications.

Options:

**`resume_timeout: Seconds|undefined`**:   This option specifies the
 number of seconds until the session of a disconnected push
 client times out. This timeout is only in effect as long as no
 push notification is issued. Once that happened, the resumption
 timeout configured for the [stream management
 module](#mod_stream_mgmt) is restored. If this option is set to
 `undefined`, the resumption timeout won't be modified by
 `mod_push_keepalive`. Default: `259200`.

**`wake_on_start: true|false`**:   If this option is set to `true`,
 notifications are generated for *all* registered push clients
 during server startup. This option should *not* be enabled on
 servers with many push clients as it can generate significant
 load on the involved push services. Default: `false`.

**`wake_on_timeout: true|false`**:   If this option is set to `true`, a
 notification is generated shortly before the session would time
 out as per the `resume_timeout` described above. Default:
 `true`.

## mod_register

This module adds support for In-Band Registration
([`XEP-0077`][91]). This protocol
enables end users to use a XMPP client to:

- Register a new account on the server.

- Change the password from an existing account on the server.

- Delete an existing account on the server.

Options:

**`access: AccessName`**:   Specify rules to restrict what usernames can be registered and
 unregistered. If a rule returns ‘deny’ on the requested username,
 registration and unregistration of that user name is denied. There
 are no restrictions by default.

**`access_from: AccessName`**:   By default, `ejabberd` doesn't allow to register new accounts from
 s2s or existing c2s sessions. You can change it by defining access
 rule in this option. Use with care: allowing registration from s2s
 leads to uncontrolled massive accounts creation by rogue users.

**`captcha_protected: false|true`**:   Protect registrations with CAPTCHA (see section [CAPTCHA](#captcha)). The
 default is `false`.

**`ip_access: AccessName`**:   Define rules to allow or deny account registration depending on the
 IP address of the XMPP client. The `AccessName` should be of type
 `ip`. The default value is `all`.

**`password_strength: Entropy`**:   This option sets the minimum Shannon entropy for passwords.
 The value `Entropy` is a number of bits of entropy. The recommended
 minimum is 32 bits. The default is 0, i.e. no checks are performed.

**`redirect_url: URL`**:   This option enables registration redirection as described in
 [`XEP-0077: In-Band Registration: Redirection`](https://xmpp.org/extensions/xep-0077.html#redirect).

**`registration_watchers: [ JID, ...]`**:   This option defines a list of JIDs which will be notified each time
 a new account is registered.

**`welcome_message: {subject: Subject, body: Body}`**:   Set a welcome message that is sent to each newly registered account.
 The first string is the subject, and the second string is the
 message body.

This module reads also another option defined globally for the server:
`registration_timeout: Timeout`. This option limits the frequency of
registration from a given IP or username. So, a user that tries to
register a new account from the same IP address or JID during this
number of seconds after their previous registration will receive an error
`resource-constraint` with the explanation: “Users are not allowed to
register accounts so quickly”. The timeout is expressed in seconds, and
it must be an integer. To disable this limitation, instead of an integer
put a word like: `infinity`. Default value: 600 seconds.

Examples:

- Next example prohibits the registration of too short account names,
 and allows to create accounts only to clients of the local network:

  acl:
    loopback:
      ip:
        - 127.0.0.0/8
        - "::1"
    shortname:
      user_glob:
        - "?"
        - "??"
      ## The same using regexp:
      ##user_regexp: "^..?$"

  access_rules:
    mynetworks:
      - allow: loopback
    register:
      - deny: shortname
      - allow: all

  modules:
    mod_register:
      ip_access: mynetworks
      access: register

- This configuration prohibits usage of In-Band Registration to create
 or delete accounts, but allows existing accounts to change the
 password:

  modules:
    ...
    mod_register:
      access: none
    ...

- This configuration disables all In-Band Registration functionality:
 create, delete accounts and change password:

  modules:
    ...
    ## mod_register:
    ##   access: register
    ...

- Define the welcome message and two registration watchers. Also
 define a registration timeout of one hour:

  registration_timeout: 3600
  modules:
    ...
    mod_register:
      welcome_message:
        subject: "Welcome!"
        body: |-
          Hi.
          Welcome to this XMPP server.
          Check https://xmpp.org/

          Bye
      registration_watchers:
        - admin1@example.org
        - boss@example.net
      redirect_url: http://my.site.org/register
    ...

## mod_register_web

This module provides a web page where people can:

- Register a new account on the server.

- Change the password from an existing account on the server.

- Delete an existing account on the server.

This module supports CAPTCHA image to register a new account. To enable
this feature, configure the options captcha\_cmd and captcha\_host.

Options:

**`registration_watchers: [ JID, ...]`**:   This option defines a list of JIDs which will be notified each time
 a new account is registered.

This example configuration shows how to enable the module and the web
handler:

 hosts:
   - localhost
   - example.org
   - example.com

 certfiles:
   - /etc/ejabberd/certificate.pem

 listen:
   ...
   -
     port: 5281
     module: ejabberd_http
     tls: true
     request_handlers:
       /register: mod_register_web
   ...

 modules:
   ...
   mod_register_web: {}
   ...

For example, the users of the host `example.org` can visit the page:
`https://example.org:5281/register/` It is important to include the last
/ character in the URL, otherwise the subpages URL will be incorrect.

## mod_roster

This module implements roster management as defined in
[`RFC 6121: XMPP IM`][92]. It
also supports Roster Versioning
([`XEP-0237`][93]).

Options:

**`db_type: mnesia|sql`**:   Define the type of storage where the module will create the tables and store user information. The default is the storage defined by the global option `default_db`, or `mnesia` if omitted. If `sql` value is defined, make sure you have defined the database, see [database](#database-and-ldap-configuration).

**`use_cache: false|true`**:   Use this option and related ones as explained in section [Caching](#caching).

**`versioning: false|true`**:   Enables Roster Versioning. This option is disabled by default. We use a [simplified version of roster versioning](../developer/xmpp-clients-bots/extensions/roster-versioning.md).

**`store_current_id: false|true`**:   If this option is enabled, the current version number is stored on
 the database. If disabled, the version number is calculated on the
 fly each time. Enabling this option reduces the load for both
 ejabberd and the database. This option does not affect the client in
 any way. This option is only useful if Roster Versioning is enabled.
 This option is disabled by default. Important: if you use
 `mod_shared_roster` or `mod_shared_roster_ldap`, you must disable
 this option.

**`access`**:   This option can be configured to specify rules to restrict roster
 management. If a rule returns ‘deny’ on the requested user name,
 that user cannot modify their personal roster: not add/remove/modify
 contacts, or subscribe/unsubscribe presence. By default there aren’t
 restrictions.

This example configuration enables Roster Versioning with storage of
current id. The ICQ and MSN transports can get ICQ and MSN contacts, add
them, or remove them for any local account:

 modules:
   ...
   mod_roster:
     versioning: true
     store_current_id: true
   ...

With this example configuration, only admins can manage their rosters;
everybody else cannot modify the roster:

 acl:
   admin:
     user:
       - sarah@example.org
 access_rules:
   roster:
     - allow: admin

 modules:
   ...
   mod_roster:
     access: roster
   ...

## mod_s2s_dialback

This module adds support for the Server Dialback protocol ([`XEP-0220`][127])
to provide identity verification based on DNS.

## mod_service_log

This module adds support for logging end user packets via a XMPP message
auditing service such as
[`Bandersnatch`][95].
All user packets are encapsulated in a `<route/>` element and sent to
the specified service(s).

Options:

**`loggers: [Names, ...]`**:   With this option a (list of) service(s) that will receive the
 packets can be specified.

Examples:

- To log all end user packets to the Bandersnatch service running on
 `bandersnatch.example.com`:

  modules:
    ...
    mod_service_log:
      loggers: [bandersnatch.example.com]
    ...

- To log all end user packets to the Bandersnatch service running on
 `bandersnatch.example.com` and the backup service on
 `bandersnatch.example.org`:

  modules:
    ...
    mod_service_log:
      loggers:
        - bandersnatch.example.com
        - bandersnatch.example.org
    ...

## mod_shared_roster

This module enables you to create shared roster groups. This means that
you can create groups of people that can see members from (other) groups
in their rosters. The big advantages of this feature are that end users
do not need to manually add all users to their rosters, and that they
cannot permanently delete users from the shared roster groups. A shared
roster group can have members from any XMPP server, but the presence
will only be available from and to members of the same virtual host
where the group is created.

It still allows the users to have / add their own contacts, as it does
not replace the standard roster. Instead, the shared roster contacts
are merged to the relevant users at retrieval time. The standard user
rosters thus stay unmodified.

Special values like @all@ and @online@ will help populating the shared
roster dynamically. It means that the actual list of members in those
shared rosters is generated dynamically at retrieval time.

**Note:** `mod_shared_roster` depends on `mod_roster` being
enabled. Roster queries will return 503 errors if `mod_roster` is not
enabled.

Options:

**`db_type: mnesia|sql`**:   Define the type of storage where the module will create the tables and store user information. The default is the storage defined by the global option `default_db`, or `mnesia` if omitted. If `sql` value is defined, make sure you have defined the database, see [database](#database-and-ldap-configuration).

Shared roster groups can be edited *only* via the Web Admin. Each group
has a unique identification and the following parameters:

- **Name**:   The name of the group, which will be displayed in the roster.

- **Description**:   The description of the group. This parameter does not affect
 anything.

- **Members**:   A list of JIDs of group members, entered one per line in the Web
 Admin. The special member directive `@all@` represents all the
 registered users in the virtual host; which is only recommended for
 a small server with just a few hundred users. The special member
 directive `@online@` represents the online users in the virtual
 host.

- **Displayed groups**:   A list of groups that will be in the rosters of this group’s
 members. A group of other vhost can be identified with
 `groupid@vhost`

Examples:

- Take the case of a computer club that wants all its members seeing
 each other in their rosters. To achieve this, they need to create a
 shared roster group similar to this one:

 **Identification**:  Group ‘`club_members`’

 **Name**:  Club Members

 **Description**:  Members from the computer club

 **Members**:  `member1@example.org`, `member2@example.org`, `member3@example.org`

 **Displayed groups**:  `club_members`

- In another case we have a company which has three divisions:
 Management, Marketing and Sales. All group members should see all
 other members in their rosters. Additionally, all managers should
 have all marketing and sales people in their roster. Simultaneously,
 all marketeers and the whole sales team should see all managers.
 This scenario can be achieved by creating shared roster groups as
 shown in the following lists:

 First list:

 **Identification**:  Group ‘`management`’

 **Name**:  Management

 **Description**:  Management

 **Members**:  `manager1@example.org`, `manager2@example.org`

 **Displayed groups**:  `management`, `marketing`, `sales`

 Second list:

 **Identification**:  Group ‘`marketing`’

 **Name**:  Marketing

 **Description**:  Marketing

 **Members**:  `marketeer1@example.org`, `marketeer2@example.org`, `marketeer3@example.org`

 **Displayed groups**:  `management`, `marketing`

 Third list:

 **Identification**:  Group ‘`sales`’

 **Name**:  Sales

 **Description**:  Sales

 **Members**:  `salesman1@example.org`, `salesman2@example.org`, `salesman3@example.org`

 **Displayed groups**:  `management`, `sales`

## mod_shared_roster_ldap

This module lets the server administrator automatically populate users’
rosters (contact lists) with entries based on users and groups defined
in an LDAP-based directory.

**Note:** `mod_shared_roster_ldap` depends on `mod_roster` being
enabled. Roster queries will return 503 errors if `mod_roster` is not
enabled.

### Configuration parameters

The module accepts the following configuration parameters. Some of them,
if unspecified, default to the values specified for the top level of
configuration. This lets you avoid specifying, for example, the bind
password, in multiple places.

**`use_cache: false|true`**:   Use this option and related ones as explained in section [Caching](#caching).

#### Filters

These parameters specify LDAP filters used to query for shared roster
information. All of them are run against the `ldap_base`.

**`ldap_rfilter`**:   So called “Roster Filter”. Used to find names of all “shared roster”
 groups. See also the `ldap_groupattr` parameter. If unspecified,
 defaults to the top-level parameter of the same name. You
 *must* specify it in some place in the configuration,
 there is no default.

**`ldap_ufilter`**:   “User Filter” – used for retrieving the human-readable name of
 roster entries (usually full names of people in the roster). See
 also the parameters `ldap_userdesc` and `ldap_useruid`. If
 unspecified, defaults to the top-level parameter of the same name.
 If that one also is unspecified, then the filter is assembled from
 values of other parameters as follows (`[ldap_SOMETHING]` is used to
 mean “the value of the configuration parameter
 `ldap_SOMETHING`”):

   (&(&([ldap_memberattr]=[ldap_memberattr_format])([ldap_groupattr]=%g))[ldap_filter])

Subsequently `%u` and `%g` are replaced with a \*.
This means that given the defaults, the filter sent to the LDAP
server would be `(&(memberUid=*)(cn=*))`. If however the
`ldap_memberattr_format` is something like
`uid=%u,ou=People,o=org`, then the filter will be
`(&(memberUid=uid=*,ou=People,o=org)(cn=*))`.

**`ldap_gfilter`**:   “Group Filter” – used when retrieving human-readable name (a.k.a.
 “Display Name”) and the members of a group. See also the parameters
 `ldap_groupattr`, `ldap_groupdesc` and `ldap_memberattr`. If
 unspecified, defaults to the top-level parameter of the same name.
 If that one also is unspecified, then the filter is constructed
 exactly in the same way as *User Filter*.

**`ldap_filter`**:   Additional filter which is AND-ed together with *User
 Filter* and *Group Filter*. If unspecified,
 defaults to the top-level parameter of the same name. If that one is
 also unspecified, then no additional filter is merged with the other
 filters.

Note that you will probably need to manually define the
*User* and *Group Filter* (since the
auto-assembled ones will not work) if:

- your `ldap_memberattr_format` is anything other than a
 simple `%u`,

- **and** the attribute specified with
 `ldap_memberattr` does not support substring matches.

An example where it is the case is OpenLDAP and
*(unique)MemberName* attribute from the
*groupOf(Unique)Names* objectClass. A symptom of this problem
is that you will see messages such as the following in your
`slapd.log`:

 get_filter: unknown filter type=130
 filter="(&(?=undefined)(?=undefined)(something=else))"

### Attributes

These parameters specify the names of the attributes which hold
interesting data in the entries returned by running filters specified in
section [Filters](#filters).

**`ldap_groupattr`**: The name of the attribute that holds the group name, and that is used to
  differentiate between them. Retrieved from results of the “Roster
  Filter” and “Group Filter”. Defaults to `cn`.

**`ldap_groupdesc`**: The name of the attribute which holds the human-readable group name in
  the objects you use to represent groups. Retrieved from results of the
  “Group Filter”. Defaults to whatever `ldap_groupattr` is
  set.

**`ldap_memberattr`**: The name of the attribute which holds the IDs of the members of a group.
 Retrieved from results of the “Group Filter”. Defaults to
 `memberUid`.

The name of the attribute differs depending on the
`objectClass` you use for your group objects, for example:

  `posixGroup` -> `memberUid`
  `groupOfNames` -> `member`
  `groupOfUniqueNames` -> `uniqueMember`

**`ldap_userdesc`**: The name of the attribute which holds the human-readable user name.
  Retrieved from results of the “User Filter”. Defaults to
  `cn`.

**`ldap_useruid`**: The name of the attribute which holds the ID of a roster item. Value of
  this attribute in the roster item objects needs to match the ID
  retrieved from the `ldap_memberattr` attribute of a group
  object. Retrieved from results of the “User Filter”. Defaults to
  `cn`.

### Control parameters

These parameters control the behaviour of the module.

**`ldap_memberattr_format`**:   A globbing format for extracting user ID from the value of the
 attribute named by `ldap_memberattr`. Defaults to `%u`,
 which means that the whole value is the member ID. If you change it
 to something different, you may also need to specify the User and
 Group Filters manually — see section [Filters](#filters).

**`ldap_memberattr_format_re`**:   A regex for extracting user ID from the value of the attribute named
 by `ldap_memberattr`.

An example value
`“CN=(\\w*),(OU=.*,)*DC=company,DC=com”`
works for user IDs such as the following:

- `CN=Romeo,OU=Montague,DC=company,DC=com`
- `CN=Abram,OU=Servants,OU=Montague,DC=company,DC=com`
- `CN=Juliet,OU=Capulet,DC=company,DC=com`
- `CN=Peter,OU=Servants,OU=Capulet,DC=company,DC=com`

In case:

- the option is unset,
- or the `re` module in unavailable in the current
    Erlang environment,
- or the regular expression does not compile,

then instead of a regular expression, a simple format specified by
`ldap_memberattr_format` is used. Also, in the last two
cases an error message is logged during the module initialization.

Also, note that in all cases `ldap_memberattr_format`
(and `*not*` the regex version) is used for constructing
the default “User/Group Filter” — see section [Filters](#filters).

**`ldap_auth_check`**:   Whether the module should check (via the ejabberd authentication
 subsystem) for existence of each user in the shared LDAP roster. See
 section [mod_shared_roster_ldap](#mod_shared_roster_ldap) form more information. Set to
 `off` if you want to disable the check. Defaults to
 `on`.

**`ldap_user_cache_validity`**:   Number of seconds for which the cache for roster item full names is
 considered fresh after retrieval. 300 by default. See
 section [mod_shared_roster_ldap](#mod_shared_roster_ldap) on how it is used during roster
 retrieval.

**`ldap_group_cache_validity`**:   Number of seconds for which the cache for group membership is
 considered fresh after retrieval. 300 by default. See
 section [mod_shared_roster_ldap](#mod_shared_roster_ldap) on how it is used during roster
 retrieval.

### Connection parameters

The module also accepts the connection parameters, all of which default
to the top-level parameter of the same name, if unspecified.
See [LDAP Connection](#ldap-connection)] for more information about them.

### Retrieving the roster

When the module is called to retrieve the shared roster for a user, the
following algorithm is used:

1. [step:rfilter] A list of names of groups to display is created: the
 *Roster Filter* is run against the base DN, retrieving
 the values of the attribute named by `ldap_groupattr`.

2. Unless the group cache is fresh (see the
 `ldap_group_cache_validity` option), it is refreshed:

 1. Information for all groups is retrieved using a single query:
  the *Group Filter* is run against the Base DN,
  retrieving the values of attributes named by
  `ldap_groupattr` (group ID),
  `ldap_groupdesc` (group “Display Name”) and
  `ldap_memberattr` (IDs of group members).

 2. group “Display Name”, read from the attribute named by
  `ldap_groupdesc`, is stored in the cache for the
  given group

 3. the following processing takes place for each retrieved value of
  attribute named by `ldap_memberattr`:

  1. the user ID part of it is extracted using
   `ldap_memberattr_format(_re)`,

  2. then (unless `ldap_auth_check` is set to
   `off`) for each found user ID, the module checks
   (using the `ejabberd` authentication subsystem) whether such
   user exists in the given virtual host. It is skipped if the
   check is enabled and fails.

   This step is here for historical reasons. If you have a tidy
   DIT and properly defined “Roster Filter” and “Group Filter”,
   it is safe to disable it by setting
   `ldap_auth_check` to `off` — it will
   speed up the roster retrieval.

  3. the user ID is stored in the list of members in the cache
   for the given group

3. For each item (group name) in the list of groups retrieved in
 step [step:rfilter]:

 1. the display name of a shared roster group is retrieved from the
  group cache

 2. for each IDs of users which belong to the group, retrieved from
  the group cache:

  1. the ID is skipped if it’s the same as the one for which we
   are retrieving the roster. This is so that the user does not
   have himself in the roster.

  2. the display name of a shared roster user is retrieved:

   1. first, unless the user name cache is fresh (see the
    `ldap_user_cache_validity` option), it is
    refreshed by running the *User Filter*,
    against the Base DN, retrieving the values of attributes
    named by `ldap_useruid` and
    `ldap_userdesc`.

   2. then, the display name for the given user ID is
    retrieved from the user name cache.

### Configuration examples

Since there are many possible
[`DIT`][97]
layouts, it will probably be easiest to understand how to configure the
module by looking at an example for a given DIT (or one resembling it).

#### Flat DIT

This seems to be the kind of DIT for which this module was initially
designed. Basically there are just user objects, and group membership is
stored in an attribute individually for each user. For example in a
layout shown in figure [fig:msrl-dit-flat], the group of each user is
stored in its `ou` attribute.

Such layout has a few downsides, including:

- information duplication – the group name is repeated in every member
 object

- difficult group management – information about group members is not
 centralized, but distributed between member objects

- inefficiency – the list of unique group names has to be computed by
 iterating over all users

This however seems to be a common DIT layout, so the module keeps
supporting it. You can use the following configuration…

 modules:
   ...
   mod_shared_roster_ldap:
     ldap_base: "ou=flat,dc=nodomain"
     ldap_rfilter: "(objectClass=inetOrgPerson)"
     ldap_groupattr: ou
     ldap_memberattr: cn
     ldap_filter: "(objectClass=inetOrgPerson)"
     ldap_userdesc: displayName
   ...

…to be provided with a roster as shown in figure [fig:msrl-roster-flat]
upon connecting as user `czesio`.

#### Deep DIT

This type of DIT contains distinctly typed objects for users and groups
– see figure [fig:msrl-dit-deep]. They are shown separated into
different subtrees, but it’s not a requirement.

If you use the following example module configuration with it:

 modules:
   ...
   mod_shared_roster_ldap:
     ldap_base: "ou=deep,dc=nodomain"
     ldap_rfilter: "(objectClass=groupOfUniqueNames)"
     ldap_filter: ""
     ldap_gfilter: "(&(objectClass=groupOfUniqueNames)(cn=%g))"
     ldap_groupdesc: description
     ldap_memberattr: uniqueMember
     ldap_memberattr_format: "cn=%u,ou=people,ou=deep,dc=nodomain"
     ldap_ufilter: "(&(objectClass=inetOrgPerson)(cn=%u))"
     ldap_userdesc: displayName
   ...

…and connect as user `czesio`, then `ejabberd` will provide
you with the roster shown in figure [fig:msrl-roster-deep].

## mod_sic

This module adds support for Server IP Check
([`XEP-0279`][98]). This protocol
enables a client to discover its external IP address.

## mod_sip

This module adds SIP proxy/registrar support for the corresponding
virtual host. Note that it is not enough to just load this module only.
You should also configure listeners and DNS records properly. See
section [SIP](#sip) for the full explanation.

Example configuration:

 modules:
   ...
   mod_sip: {}
   ...

Options:

**`record_route: SIP_URI`**:   When the option `always_record_route` is set or when SIP outbound is
 utilized [`RFC 5626`](https://tools.ietf.org/html/rfc5626),
 `ejabberd` inserts `Record-Route` header field with this `SIP_URI`
 into a SIP message. The default is SIP URI constructed from the
 virtual host.

**`always_record_route: true|false`**:   Always insert `Record-Route` header into SIP messages. This approach
 allows to bypass NATs/firewalls a bit more easily. The default is
 `true`.

**`routes: [SIP_URI]`**:   You can set a list of SIP URIs of routes pointing to this proxy
 server. The default is a list of a SIP URI constructed from the
 virtual host.

**`flow_timeout_udp: Seconds`**:   For SIP outbound UDP connections set a keep-alive timer to
 `Seconds`. The default is 29.

**`flow_timeout_tcp: Seconds`**:   For SIP outbound TCP connections set a keep-alive timer to
 `Seconds`. The default is 120.

**`via: [{type: Type, host: Host, port: Port}]`**:   With this option for every `Type` you can specify `Host` and `Port`
 to set in `Via` header of outgoing SIP messages, where `Type` can be
 `udp`, `tcp` or `tls`. `Host` is a string and `Port` is a non
 negative integer. This is useful if you’re running your server in a
 non-standard network topology.

Example complex configuration:

 modules:
   ...
   mod_sip:
     always_record_route: false
     record_route: "sip:example.com;lr"
     routes:
       - "sip:example.com;lr"
       - "sip:sip.example.com;lr"
     flow_timeout_udp: 30
     flow_timeout_tcp: 130
     via:
       -
         type: tls
         host: sip-tls.example.com
         port: 5061
       -
         type: tcp
         host: sip-tcp.example.com
         port: 5060
       -
         type: udp
         host: sip-udp.example.com
         port: 5060
   ...

## mod_stats

This module adds support for Statistics Gathering
([`XEP-0039`][99]). This protocol
allows you to retrieve next statistics from your `ejabberd` deployment:

- Total number of registered users on the current virtual host
 (users/total).

- Total number of registered users on all virtual hosts
 (users/all-hosts/total).

- Total number of online users on the current virtual host
 (users/online).

- Total number of online users on all virtual hosts
 (users/all-hosts/online).

As there are only a small amount of clients (for example
[`Tkabber`][100]) and software libraries with
support for this XEP, a few examples are given of the XML you need to
send in order to get the statistics. Here they are:

- You can request the number of online users on the current virtual
 host (`example.org`) by sending:

  <iq to='example.org' type='get'>
    <query xmlns='http://jabber.org/protocol/stats'>
      <stat name='users/online'/>
    </query>
  </iq>

- You can request the total number of registered users on all virtual
 hosts by sending:

  <iq to='example.org' type='get'>
    <query xmlns='http://jabber.org/protocol/stats'>
      <stat name='users/all-hosts/total'/>
    </query>
  </iq>

## mod_stream_mgmt

This module adds support for Stream Management ([`XEP-0198`][125]).
This protocol allows active management of an XML stream between two XMPP entities,
including features for stanza acknowledgements and stream resumption.

Options:

**`max_ack_queue: Size`**:   This option specifies the maximum number of unacknowledged stanzas
 queued for possible retransmission.
 When the limit is exceeded, the client session is
 terminated. The allowed values are positive integers and `infinity`.
 You should be careful when setting this value as it should not be set too low,
 otherwise, you could killed sessions in loop, before they get the chance to finish
 proper session initiation. It should definitely be set higher that the size of the
 offline queue (for example at least 3 times the value of the max offline queue and never
 lower than 1000).
 Default value: `5000`.

**`max_resume_timeout: Seconds`**:   A client may specify the number of
 seconds until a session times out if the connection is lost. During
 this period of time, the client may resume the session. This option
 limits the number of seconds a client is permitted to request.
 It must be set to
 a number equal to or larger than the default `resume_timeout` (see
 below). By default, it is set to the same value as the
 `resume_timeout` option.

**`resend_on_timeout: true|false|if_offline`**:   If this option is set to `true`,
 any message stanzas that weren’t acknowledged by the client will
 be resent on session timeout. This behavior might often be desired,
 but could have unexpected results under certain circumstances. For
 example, a message that was sent to two resources might get resent
 to one of them if the other one timed out. Therefore, the default
 value for this option is `false`, which tells ejabberd to generate
 an error message instead. As an alternative, the option may be set
 to `if_offline`. In this case, unacknowledged messages are resent
 only if no other resource is online when the session times out.
 Otherwise, error messages are generated.

**`resume_timeout: Seconds`**:   This option configures the (default) number of seconds until a session
 times out if the connection is lost. During this period of time, a
 client may resume the session.
 (Note that the client may request a different timeout value, see the
 `max_resume_timeout` option above.) Setting it to `0` effectively disables
 session resumption. The default value is `300`.

**`ack_timeout: Seconds`**: The default value is `60`.
 Setting it to `infinity` effectively disables the timeout.

**`queue_type: Discipline`**: The default value is `ram`.
 The accepted values are `ram` and `file`.

## mod_time

This module features support for Entity Time
([`XEP-0202`][101]). By using this
XEP, you are able to discover the time at another entity’s location.

## mod_vcard

This module allows end users to store and retrieve their vCard, and to
retrieve other users vCards, as defined in vcard-temp
([`XEP-0054`][102]). The module
also implements an uncomplicated Jabber User Directory based on the
vCards of these users. Moreover, it enables the server to send its vCard
when queried.

Options:

**`host: HostName`**:   This option defines the Jabber ID of the service. If the `host`
 option is not specified, the Jabber ID will be the hostname of the
 virtual host with the prefix ‘`vjud.`’. The keyword “@HOST@” is
 replaced at start time with the real virtual host name.

**`db_type: mnesia|sql|ldap`**: Define the type of storage where
  the module will create the tables and store user information. The
  default is the storage defined by the global option `default_db`, or
  `mnesia` if omitted. If `sql` value is defined, make sure
  you have defined the database, see
  [database](#database-and-ldap-configuration). If `ldap` value is
  defined, check its specific options, see [vCards in
  LDAP](#vcards-in-ldap).

**`use_cache: false|true`**:   Use this option and related ones as explained in section [Caching](#caching).

**`search: true|false`**:   This option specifies whether the search functionality is enabled or
 not. If disabled, the option `host` will be ignored and the Jabber
 User Directory service will not appear in the Service Discovery item
 list. The default value is `false`.

**`matches: infinity|Number`**:   With this option, the number of reported search results can be
 limited. If the option’s value is set to `infinity`, all search
 results are reported. The default value is `30`.

**`allow_return_all: false|true`**:   This option enables you to specify if search operations with empty
 input fields should return all users who added some information to
 their vCard. The default value is `false`.

**`search_all_hosts, true|false`**:   If this option is set to `true`, search operations will apply to all
 virtual hosts. Otherwise only the current host will be searched. The
 default value is `true`. This option is available in `mod_vcard`
 when using Mnesia, but not when using SQL storage.

Examples:

- In this first situation, search results are limited to twenty items,
 every user who added information to their vCard will be listed when
 people do an empty search, and only users from the current host will
 be returned:

  modules:
    ...
    mod_vcard:
      search: true
      matches: 20
      allow_return_all: true
      search_all_hosts: false
    ...

- The second situation differs in a way that search results are not
 limited, and that all virtual hosts will be searched instead of only
 the current one:

  modules:
    ...
    mod_vcard:
      search: true
      matches: infinity
      allow_return_all: true
    ...

### vCards in LDAP

`ejabberd` can map LDAP attributes to vCard fields. This feature is
enabled when the `mod_vcard` module is configured with `db_type:
ldap`. Notice that it does not depend on the authentication method
(see [LDAP Authentication](#ldap-authentication)).

Usually `ejabberd` treats LDAP as a read-only storage: it is possible to
consult data, but not possible to create accounts or edit vCard that is
stored in LDAP. However, it is possible to change passwords if
`mod_register` module is enabled and LDAP server supports
[`RFC 3062`][104].

This feature has its own optional parameters. The first
group of parameters has the same meaning as the top-level LDAP
parameters to set the authentication method: `ldap_servers`,
`ldap_port`, `ldap_rootdn`, `ldap_password`, `ldap_base`, `ldap_uids`,
`ldap_deref_aliases` and `ldap_filter`. See section
[LDAP Authentication](#ldap-authentication) for
detailed information about these options. If one of these options is not
set, `ejabberd` will look for the top-level option with the same name.

The second group of parameters consists of the following `mod_vcard`-specific options:

**`host: HostName`**:   This option defines the Jabber ID of the service. If the `host`
 option is not specified, the Jabber ID will be the hostname of the
 virtual host with the prefix ‘`vjud.`’. The keyword “@HOST@” is
 replaced at start time with the real virtual host name.

**`search: true|false`**:   This option specifies whether the search functionality is enabled
 (value: `true`) or disabled (value: `false`). If disabled, the
 option `host` will be ignored and the Jabber User Directory service
 will not appear in the Service Discovery item list. The default
 value is `false`.

**`matches: infinity|Number`**:   With this option, the number of reported search results can be
 limited. If the option’s value is set to `infinity`, all search
 results are reported. The default value is `30`.

**`ldap_vcard_map: {Name: {Pattern, LDAPattributes}, ...}`**:   With this option you can set the table that maps LDAP attributes to
 vCard fields. `Name` is the type name of the vCard as defined in
 [`RFC 2426`](https://tools.ietf.org/html/rfc2426). `Pattern` is a
 string which contains pattern variables `%u`, `%d` or `%s`.
 `LDAPattributes` is the list containing LDAP attributes. The pattern
 variables `%s` will be sequentially replaced with the values of LDAP
 attributes from `List_of_LDAP_attributes`, `%u` will be replaced
 with the user part of a JID, and `%d` will be replaced with the
 domain part of a JID. The default is:

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

**`ldap_search_fields: {Name: Attribute, ...}`**:   This option defines the search form and the LDAP attributes to
 search within. `Name` is the name of a search form field which will
 be automatically translated by using the translation files (see
 `msgs/*.msg` for available words). `Attribute` is the LDAP attribute
 or the pattern `%u`. The default is:

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

**`ldap_search_reported: {SearchField: VcardField}, ...}`**:   This option defines which search fields should be reported.
 `SearchField` is the name of a search form field which will be
 automatically translated by using the translation files (see
 `msgs/*.msg` for available words). `VcardField` is the vCard field
 name defined in the `ldap_vcard_map` option. The default is:

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

Examples:

- Let’s say `ldap.example.org` is the name of our LDAP server. We have
 users with their passwords in `ou=Users,dc=example,dc=org`
 directory. Also we have addressbook, which contains users emails and
 their additional infos in `ou=AddressBook,dc=example,dc=org`
 directory. Corresponding authentication section should looks like
 this:

  ## authentication method
  auth_method: ldap
  ## DNS name of our LDAP server
  ldap_servers:
    - ldap.example.org
  ## We want to authorize users from 'shadowAccount' object class only
  ldap_filter: "(objectClass=shadowAccount)"

Now we want to use users LDAP-info as their vCards. We have four
attributes defined in our LDAP schema: `mail` — email address,
`givenName` — first name, `sn` — second name, `birthDay` — birthday.
Also we want users to search each other. Let’s see how we can set it
up:

  modules:
    mod_vcard:
      db_type: ldap
      ## We use the same server and port, but want to bind anonymously because
      ## our LDAP server accepts anonymous requests to
      ## "ou=AddressBook,dc=example,dc=org" subtree.
      ldap_rootdn: ""
      ldap_password: ""
      ## define the addressbook's base
      ldap_base: "ou=AddressBook,dc=example,dc=org"
      ## uidattr: user's part of JID is located in the "mail" attribute
      ## uidattr_format: common format for our emails
      ldap_uids: {"mail": "%u@mail.example.org"}
      ## Now we want to define vCard pattern
      ldap_vcard_map:
        NICKNAME: {"%u": []} # just use user's part of JID as their nickname
        FIRST: {"%s": [givenName]}
        LAST: {"%s": [sn]}
        FN: {"%s, %s": [sn, givenName]} # example: "Smith, John"
        EMAIL: {"%s": [mail]}
        BDAY: {"%s": [birthDay]}
      ## Search form
      ldap_search_fields:
        User: "%u"
        Name: givenName
        "Family Name": sn
        Email: mail
        Birthday: birthDay
      ## vCard fields to be reported
      ## Note that JID is always returned with search results
      ldap_search_reported:
        "Full Name": FN
        Nickname: NICKNAME
        Birthday: BDAY

Note that `mod_vcard` with LDAP backend checks an existence of the user
before searching their info in LDAP.

- `ldap_vcard_map` example:

  ldap_vcard_map:
    NICKNAME: {"%u": []} # just use user's part of JID as their nickname
    FN: {"%s": [displayName]}
    CTRY: {Russia: []}
    EMAIL: {"%u@%d": []}
    DESC: {"%s\n%s": [title, description]}

- `ldap_search_fields` example:

  ldap_search_fields:
    User: uid
    "Full Name": displayName
    Email: mail

- `ldap_search_reported` example:

  ldap_search_reported:
    "Full Name": FN
    Email: EMAIL
    Birthday: BDAY
    Nickname: NICKNAME

## mod_vcard_xupdate

The user’s client can store an avatar in the user vCard. The vCard-Based
Avatars protocol
([`XEP-0153`][105]) provides a
method for clients to inform the contacts what is the avatar hash value.
However, simple or small clients may not implement that protocol.

If this module is enabled, all the outgoing client presence stanzas get
automatically the avatar hash on behalf of the client. So, the contacts
receive the presence stanzas with the Update Data described in
[`XEP-0153`][106] as if the client
would had inserted it itself. If the client had already included such
element in the presence stanza, it is replaced with the element
generated by ejabberd.

By enabling this module, each vCard modification produces a hash
recalculation, and each presence sent by a client produces hash
retrieval and a presence stanza rewrite. For this reason, enabling this
module will introduce a computational overhead in servers with clients
that change frequently their presence.

Supported options:

**`use_cache: false|true`**:   Use this option and related ones as explained in section [Caching](#caching).

## mod_version

This module implements Software Version
([`XEP-0092`][108]). Consequently,
it answers `ejabberd`’s version when queried.

Options:

**`show_os: true|false`**:   Should the operating system be revealed or not. The default value is
 `true`.

[1]: https://en.wikipedia.org/wiki/YAML
[2]: https://ejabberd.im/pyaimt
[3]: https://ejabberd.im/pymsnt
[4]: https://ejabberd.im/yahoo-transport-2
[5]: https://ejabberd.im/jabber-gg-transport
[6]: https://ejabberd.im/jmc
[7]: https://tools.ietf.org/html/rfc6120#section-7.7.2.2
[8]: https://xmpp.org/extensions/xep-0078.html
[9]: https://ejabberd.im/extauth
[10]: https://ejabberd.im/Anonymous-users-support
[11]: https://xmpp.org/extensions/xep-0158.html
[12]: https://tools.ietf.org/html/rfc5389
[13]: https://tools.ietf.org/html/rfc5766
[14]: https://tools.ietf.org/html/rfc5245
[15]: https://xmpp.org/extensions/xep-0176.html
[16]: https://tools.ietf.org/html/rfc5389#section-9
[17]: https://tools.ietf.org/html/rfc5389
[18]: https://tools.ietf.org/html/rfc5766#section-6
[19]: https://tools.ietf.org/html/rfc5766
[20]: https://tools.ietf.org/html/rfc3261
[21]: https://en.wikipedia.org/wiki/Server_Name_Indication
[22]: https://tools.ietf.org/html/rfc3263
[23]: https://tools.ietf.org/html/rfc3261
[24]: https://erlang.org/doc/apps/mnesia/
[25]: https://www.mysql.com/
[26]: https://en.wikipedia.org/wiki/Open_Database_Connectivity
[27]: https://www.postgresql.org/
[29]: https://redis.io/
[31]: https://openldap.org/
[32]: https://communigate.com/
[33]: https://tools.ietf.org/html/rfc3062
[34]: https://tools.ietf.org/html/rfc4515
[37]: https://redis.io/
[38]: https://xmpp.org/extensions/xep-0050.html
[39]: https://xmpp.org/extensions/xep-0191.html
[40]: https://xmpp.org/extensions/xep-0115.html
[41]: https://xmpp.org/extensions/xep-0280.html
[42]: https://xmpp.org/extensions/xep-0030.html
[43]: https://xmpp.org/extensions/xep-0012.html
[44]: https://xmpp.org/extensions/xep-0045.html
[45]: https://xmpp.org/extensions/xep-0160.html
[46]: https://xmpp.org/extensions/xep-0199.html
[47]: https://xmpp.org/extensions/xep-0016.html
[48]: https://xmpp.org/extensions/xep-0049.html
[49]: https://xmpp.org/extensions/xep-0065.html
[50]: https://xmpp.org/extensions/xep-0060.html
[51]: https://xmpp.org/extensions/xep-0163.html
[54]: https://xmpp.org/extensions/xep-0077.html
[55]: https://xmpp.org/extensions/xep-0279.html
[56]: https://tools.ietf.org/html/rfc3261
[57]: https://xmpp.org/extensions/xep-0039.html
[58]: https://xmpp.org/extensions/xep-0202.html
[59]: https://xmpp.org/extensions/xep-0054.html
[61]: https://xmpp.org/extensions/xep-0153.html
[62]: https://xmpp.org/extensions/xep-0092.html
[63]: https://ejabberd.im/contributions
[65]: https://xmpp.org/extensions/xep-0352.html
[66]: https://xmpp.org/extensions/xep-0030.html
[67]: https://xmpp.org/extensions/xep-0011.html
[68]: https://xmpp.org/extensions/xep-0094.html
[69]: https://xmpp.org/extensions/xep-0124.html
[70]: https://xmpp.org/extensions/xep-0206.html
[71]: https://tools.ietf.org/html/rfc7395
[75]: https://xmpp.org/extensions/xep-0012.html
[77]: https://xmpp.org/extensions/xep-0045.html
[79]: https://xmpp.org/rfcs/rfc5122.html
[80]: https://xmpp.org/extensions/xep-0160.html
[82]: https://xmpp.org/extensions/xep-0199.html
[83]: https://xmpp.org/extensions/xep-0016.html
[85]: https://xmpp.org/extensions/xep-0049.html
[86]: https://xmpp.org/extensions/xep-0048.html
[88]: https://xmpp.org/extensions/xep-0065.html
[89]: https://xmpp.org/extensions/xep-0060.html
[90]: https://xmpp.org/extensions/xep-0163.html
[91]: https://xmpp.org/extensions/xep-0077.html
[92]: https://tools.ietf.org/html/rfc6121#section-2
[93]: https://xmpp.org/extensions/xep-0237.html
[95]: https://www.funkypenguin.co.nz/project/bandersnatch/
[97]: https://en.wikipedia.org/wiki/Directory_Information_Tree
[98]: https://xmpp.org/extensions/xep-0279.html
[99]: https://xmpp.org/extensions/xep-0039.html
[100]: http://tkabber.jabber.ru/
[101]: https://xmpp.org/extensions/xep-0202.html
[102]: https://xmpp.org/extensions/xep-0054.html
[104]: https://tools.ietf.org/html/rfc3062
[105]: https://xmpp.org/extensions/xep-0153.html
[106]: https://xmpp.org/extensions/xep-0153.html
[108]: https://xmpp.org/extensions/xep-0092.html
[109]: https://xmpp.org/extensions/xep-0033.html
[114]:  https://xmpp.org/extensions/xep-0313.html
[115]:  https://en.wikipedia.org/wiki/Microsoft_SQL_Server
[116]:  https://www.freetds.org/
[117]:  http://www.unixodbc.org/
[118]:  https://github.com/processone/ejabberd/tree/master/sql
[119]:  https://sqlite.org/
[120]:  https://xmpp.org/extensions/xep-0363.html
[121]:  https://xmpp.org/extensions/xep-0013.html
[122]:  https://xmpp.org/extensions/xep-0369.html
[123]:  https://xmpp.org/extensions/xep-0355.html
[124]:  https://xmpp.org/extensions/xep-0356.html
[125]:  https://xmpp.org/extensions/xep-0198.html
[126]:  https://xmpp.org/extensions/xep-0357.html
[127]:  https://xmpp.org/extensions/xep-0220.html
[128]:  https://xmpp.org/extensions/xep-0328.html
[129]:  https://tools.ietf.org/html/rfc6122
[130]:  https://xmpp.org/extensions/xep-0398.html

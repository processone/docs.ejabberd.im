# Listen Options

> This section describes the most recent ejabberd version. If you are using an old ejabberd release, please refer to the corresponding archived version of this page in the [Archive](../../archive/index.md).

This is a detailed description of each option allowed by the listening
modules:

## access

*AccessName*

This option defines access to the port. The default value is `all`.

## allow_unencrypted_sasl2

*true | false*

As per [`XEP-0388`](https://xmpp.org/extensions/xep-0388.html#security),
ejabberd rejects SASL2 negotiations over non-TLS connections by default. Setting
this option to `true` allows SASL2 over plaintext connections, which may be
useful in case TLS is terminated by some proxy in front of ejabberd.

## backlog

*Value*

The backlog value defines the maximum length that the queue of
 pending connections may grow to. This should be increased if the
 server is going to handle lots of new incoming connections as they
 may be dropped if there is no space in the queue (and ejabberd was
 not able to accept them immediately). Default value is 5.

## cafile

*Path*

Path to a file of CA root certificates.
The default is to use system defined file if possible.

This option is useful to define the file for a specific port listener.
To set a file for all client listeners or for specific vhosts, you can use the
[`c2s_cafile`](toplevel.md#c2s_cafile) top-level option.
To set a file for all server connections, you can use the
[`s2s_cafile`](toplevel.md#s2s_cafile) top-level option
or the
[`ca_file`](toplevel.md#ca_file) top-level option.

Please note: if this option is set in
[`ejabberd_c2s`](listen.md#ejabberd_c2s)
or [`ejabberd_s2s_in`](listen.md#ejabberd_s2s_in)
and the corresponding top-level option is also set
([`c2s_cafile`](toplevel.md#c2s_cafile),
[`s2s_cafile`](toplevel.md#s2s_cafile)),
then the top-level option is used, not this one.

## certfile

*Path*

Path to the certificate file.
Only makes sense when the [`tls`](#tls)
options is set.
If this option is not set, you should set the
[`certfiles`](toplevel.md#certfiles) top-level option
or configure [ACME](basic.md#acme).

## check_from

*true | false*

This option can be used with
[`ejabberd_service`](listen.md#ejabberd_service) only.
 [`XEP-0114`](https://xmpp.org/extensions/xep-0114.html) requires that
 the domain must match the hostname of the component. If this option
 is set to `false`, `ejabberd` will allow the component to send
 stanzas with any arbitrary domain in the ’from’ attribute. Only use
 this option if you are completely sure about it. The default value
 is `true`, to be compliant with
 [`XEP-0114`](https://xmpp.org/extensions/xep-0114.html).

## ciphers

*Ciphers*

OpenSSL ciphers list in the same format accepted by
 ‘`openssl ciphers`’ command.

Please note: if this option is set in
[`ejabberd_c2s`](listen.md#ejabberd_c2s)
or [`ejabberd_s2s_in`](listen.md#ejabberd_s2s_in)
and the corresponding top-level option is also set
([`c2s_ciphers`](toplevel.md#c2s_ciphers),
[`s2s_ciphers`](toplevel.md#s2s_ciphers)),
then the top-level option is used, not this one.

## custom_headers

*{Name: Value}*

Specify additional HTTP headers to be included in all HTTP responses.
Default value is: `[]`

## default_host

*undefined | HostName*

If the HTTP request received by ejabberd contains the HTTP header
 `Host` with an ambiguous virtual host that doesn’t match any one
 defined in ejabberd (see
        [Host Names](basic.md#host_names)),
        then this configured HostName
 is set as the request Host. The default value of this option is:
 `undefined`.

## dhfile

*Path*

Full path to a file containing custom parameters for Diffie-Hellman key
 exchange. Such a file could be created with the command
 `openssl dhparam -out dh.pem 2048`. If this option is not specified,
 default parameters will be used, which might not provide the same level
 of security as using custom parameters.

Please note: if this option is set in
[`ejabberd_c2s`](listen.md#ejabberd_c2s)
or [`ejabberd_s2s_in`](listen.md#ejabberd_s2s_in)
and the corresponding top-level option is also set
([`c2s_dhfile`](toplevel.md#c2s_dhfile),
[`s2s_dhfile`](toplevel.md#s2s_dhfile)),
then the top-level option is used, not this one.

## global_routes

*true | false*

This option emulates legacy behaviour which registers all routes
defined in [`hosts`](toplevel.md#hosts)
on a component connected. This behaviour
is considered harmful in the case when it's desired to multiplex
different components on the same port, so, to disable it,
set `global_routes` to `false`.

The default value is `true`,
e.g. legacy behaviour is emulated: the only reason for this is
to maintain backward compatibility with existing deployments.

## hosts

*{Hostname: [HostOption, ...]}*

The external Jabber component that connects to this
[`ejabberd_service`](listen.md#ejabberd_service)
can serve one or more hostnames. As `HostOption`
 you can define options for the component; currently the only allowed
 option is the password required to the component when attempt to
 connect to ejabberd: `password: Secret`. Note that you
 cannot define in a single `ejabberd_service` components of different
 services: add an `ejabberd_service` for each service, as seen in an
 example below. This option may not be necessary if the component
 already provides the host in its packets; in that case, you can simply
 provide the password option that will be used for all the hosts
 (see port 5236 definition in the example below).

## max_fsm_queue

*Size*

This option specifies the maximum number of elements in the queue of
 the FSM (Finite State Machine). Roughly speaking, each message in
 such queues represents one XML stanza queued to be sent into its
 relevant outgoing stream. If queue size reaches the limit (because,
 for example, the receiver of stanzas is too slow), the FSM and the
 corresponding connection (if any) will be terminated and error
 message will be logged. The reasonable value for this option depends
 on your hardware configuration. This option can be specified for
        [`ejabberd_service`](listen.md#ejabberd_service)
        and [`ejabberd_c2s`](listen.md#ejabberd_c2s)
        listeners, or also globally for
        [`ejabberd_s2s_out`](listen.md#ejabberd_s2s_out).
        If the option is not specified for
 `ejabberd_service` or `ejabberd_c2s` listeners, the globally
 configured value is used. The allowed values are integers and
 ’undefined’. Default value: ’10000’.

## max_payload_size

*Size*

Specify the maximum payload size in bytes.
It can be either an integer or the word `infinity`.
The default value is `infinity`.

## max_stanza_size

*Size*

This option specifies an approximate maximum size in bytes of XML
 stanzas. Approximate, because it is calculated with the precision of
 one block of read data. For example `{max_stanza_size, 65536}`. The
 default value is `infinity`. Recommended values are 65536 for c2s
 connections and 131072 for s2s connections. s2s max stanza size must
 always much higher than c2s limit. Change this value with extreme
 care as it can cause unwanted disconnect if set too low.

## password

*Secret*

Specify the password to verify an external component that connects to the port.

## port

*Port number, or unix domain socket path*

<!-- md:version improved in [20.07](../../archive/20.07/index.md) -->

Declares at which port/unix domain socket should be listening.

Can be set to number between `1` and `65535` to listen on TCP or UDP socket,
or can be set to string in form `"unix:/path/to/socket"` to create and listen
on unix domain socket `/path/to/socket`.

## protocol_options

*ProtocolOpts*

List of general options relating to SSL/TLS. These map to
 [`OpenSSL’s set_options()`](https://www.openssl.org/docs/manmaster/man3/SSL_CTX_set_options.html).
 The default entry is: `"no_sslv3|cipher_server_preference|no_compression"`

Please note: if this option is set in
[`ejabberd_c2s`](listen.md#ejabberd_c2s)
or [`ejabberd_s2s_in`](listen.md#ejabberd_s2s_in)
and the corresponding top-level option is also set
([`c2s_protocol_options`](toplevel.md#c2s_protocol_options),
[`s2s_protocol_options`](toplevel.md#s2s_protocol_options)),
then the top-level option is used, not this one.

## request_handlers

*{Path: Module}*

To define one or several handlers that will serve HTTP requests in
[`ejabberd_http`](listen.md#ejabberd_http). The
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

## send_timeout

*Integer | infinity*

<!-- md:version new in [21.07](../../archive/21.07/index.md) -->

Sets the longest time that data can wait to be accepted to sent by OS socket. Triggering this timeout will cause the server to close it. By default it's set to 15 seconds, expressed in milliseconds: 15000

## shaper

*none | ShaperName*

This option defines a shaper for the port (see section
[Shapers](basic.md#shapers)).
 The default value is `none`.

## shaper_rule

*none | ShaperRule*

This option defines a shaper rule for
[`ejabberd_service`](listen.md#ejabberd_service) (see
section [Shapers](basic.md#shapers)).
The recommended value is `fast`.

## starttls

*true | false*

This option specifies that STARTTLS encryption is available on
connections to the port. You should also set the
[`certfiles`](toplevel.md#certfiles) top-level option
or configure [ACME](basic.md#acme).

This option gets implicitly enabled when enabling
[`starttls_required`](#starttls_required) or [`tls_verify`](#tls_verify).

## starttls_required

*true | false*

This option specifies that STARTTLS encryption is required on connections to the port. No unencrypted connections will be allowed.
You should also set the [`certfiles`](toplevel.md#certfiles) top-level option or configure [ACME](basic.md#acme).

Enabling this option implicitly enables also the [`starttls`](#starttls) option.

## tag

*String*

Allow specifying a tag in a `listen` section and later use it to have a special [`api_permissions`](toplevel.md#api_permissions) just for it.

For example:

``` yaml
listen:
  -
    port: 4000
    module: ejabberd_http
    tag: "magic_listener"

api_permissions:
  "magic_access":
    from:
      - tag: "magic_listener"
    who: all
    what: "*"
```

The default value is the empty string: `""`.

## timeout

*Integer*

Timeout of the connections, expressed in milliseconds. Default: 5000

## tls

*true | false*

This option specifies that traffic on the port will be encrypted
 using SSL immediately after connecting. This was the traditional
 encryption method in the early Jabber software, commonly on port
 5223 for client-to-server communications. But this method is
 nowadays deprecated and not recommended. The preferable encryption
 method is STARTTLS on port 5222, as defined
 [`RFC 6120: XMPP Core`](https://xmpp.org/rfcs/rfc6120.html#tls),
 which can be enabled in `ejabberd` with the option
        [`starttls`](#starttls).

If this option is set, you should also set the
        [`certfiles`](toplevel.md#certfiles) top-level
        option or configure [ACME](basic.md#acme).

The option `tls` can also be used in
        [`ejabberd_http`](listen.md#ejabberd_http)
        to support HTTPS.

Enabling this option implicitly disables the [`starttls`](#starttls) option.

## tls_compression

*true | false*

Whether to enable or disable TLS compression. The default value is
 `false`.

Please note: if this option is set in
[`ejabberd_c2s`](listen.md#ejabberd_c2s)
or [`ejabberd_s2s_in`](listen.md#ejabberd_s2s_in)
and the corresponding top-level option is also set
([`c2s_tls_compression`](toplevel.md#c2s_tls_compression),
[`s2s_tls_compression`](toplevel.md#s2s_tls_compression)),
then the top-level option is used, not this one.

## tls_verify

*false | true*

This option specifies whether to verify the certificate or not when TLS is enabled.

The default value is `false`, which means no checks are performed.

The certificate will be checked against trusted CA roots, either defined at the operation system level or defined in the
 listener [`cafile`](#cafile). If trusted, it will accept the jid that is embedded in the certificate in the
 `subjectAltName` field of that certificate.

Enabling this option implicitly enables also the [`starttls`](#starttls) option.

## use_proxy_protocol

*true | false*

Is this listener accessed by proxy service that is using
    proxy protocol for supplying real IP addresses to ejabberd server. You can read about this protocol
    in [Proxy protocol specification](https://www.haproxy.org/download/1.8/doc/proxy-protocol.txt).
    The default value of this option is`false`.

## zlib

*true | false*

This option specifies that Zlib stream compression (as defined in
 [`XEP-0138`](https://xmpp.org/extensions/xep-0138.html)) is available
 on connections to the port.

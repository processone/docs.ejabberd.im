---
search:
  boost: 1
---

# Top-Level Options

This section describes top level options of ejabberd [25.04](../../archive/25.04/index.md).71. The
options that changed in this version are marked with 🟤.

## access\_rules

`{AccessName: {allow|deny: ACLName|ACLDefinition}}`  

This option defines [Access Rules](basic.md#access-rules). Each access
rule is assigned a name that can be referenced from other parts of the
configuration file (mostly from `access` options of ejabberd modules).
Each rule definition may contain arbitrary number of `allow` or `deny`
sections, and each section may contain any number of ACL rules (see
[acl](#acl) option). There are no access rules defined by default.

**Example**:

~~~ yaml
access_rules:
  configure:
    allow: admin
  something:
    deny: someone
    allow: all
  s2s_banned:
    deny: problematic_hosts
    deny: banned_forever
    deny:
      ip: 222.111.222.111/32
    deny:
      ip: 111.222.111.222/32
    allow: all
  xmlrpc_access:
    allow:
      user: peter@example.com
    allow:
      user: ivone@example.com
    allow:
      user: bot@example.com
      ip: 10.0.0.0/24
~~~

## acl

`{ACLName: {ACLType: ACLValue}}`  

This option defines
[access control lists](../configuration/basic.md#acl): named sets of
rules which are used to match against different targets (such as a JID
or an IP address). Every set of rules has name `ACLName`: it can be any
string except `all` or `none` (those are predefined names for the rules
that match all or nothing respectively). The name `ACLName` can be
referenced from other parts of the configuration file, for example in
[access_rules](#access_rules) option. The rules of `ACLName` are represented by
mapping `{ACLType: ACLValue}`. These can be one of the following:

-   **ip**: `Network`  
    The rule matches any IP address from the
    `Network`.

-   **node\_glob**: `Pattern`  
    Same as `node_regexp`, but matching is
    performed on a specified `Pattern` according to the rules used by
    the Unix shell.

-   **node\_regexp**: `user_regexp@server_regexp`  
    The rule matches any
    JID with node part matching regular expression `user_regexp` and
    server part matching regular expression `server_regexp`.

-   **resource**: `Resource`  
    The rule matches any JID with a resource
    `Resource`.

-   **resource\_glob**: `Pattern`  
    Same as `resource_regexp`, but
    matching is performed on a specified `Pattern` according to the
    rules used by the Unix shell.

-   **resource\_regexp**: `Regexp`  
    The rule matches any JID with a
    resource that matches regular expression `Regexp`.

-   **server**: `Server`  
    The rule matches any JID from server `Server`.
    The value of `Server` must be a valid hostname or an IP address.

-   **server\_glob**: `Pattern`  
    Same as `server_regexp`, but matching
    is performed on a specified `Pattern` according to the rules used by
    the Unix shell.

-   **server\_regexp**: `Regexp`  
    The rule matches any JID from the
    server that matches regular expression `Regexp`.

-   **user**: `Username`  
    If `Username` is in the form of "user@server",
    the rule matches a JID against this value. Otherwise, if `Username`
    is in the form of "user", the rule matches any JID that has
    `Username` in the node part as long as the server part of this JID
    is any virtual host served by ejabberd.

-   **user\_glob**: `Pattern`  
    Same as `user_regexp`, but matching is
    performed on a specified `Pattern` according to the rules used by
    the Unix shell.

-   **user\_regexp**: `Regexp`  
    If `Regexp` is in the form of
    "regexp@server", the rule matches any JID with node part matching
    regular expression "regexp" as long as the server part of this JID
    is equal to "server". If `Regexp` is in the form of "regexp", the
    rule matches any JID with node part matching regular expression
    "regexp" as long as the server part of this JID is any virtual host
    served by ejabberd.

## acme

`Options`  

[ACME](basic.md#acme) configuration, to automatically obtain SSL
certificates for the domains served by ejabberd, which means that
certificate requests and renewals are performed to some CA server (aka
"ACME server") in a fully automated mode. The `Options` are:

-   **auto**: `true | false`  
    Whether to automatically request
    certificates for all configured domains (that yet have no a
    certificate) on server start or configuration reload. The default is
    `true`.

-   **ca\_url**: `URL`  
    The ACME directory URL used as an entry point for
    the ACME server. The default value is
    <https://acme-v02.api.letsencrypt.org/directory> - the directory URL
    of Let’s Encrypt authority.

-   **cert\_type**: `rsa | ec`  
    A type of a certificate key. Available
    values are `ec` and `rsa` for EC and RSA certificates respectively.
    It’s better to have RSA certificates for the purpose of backward
    compatibility with legacy clients and servers, thus the default is
    `rsa`.

-   **contact**: `[Contact, ...]`  
    A list of contact addresses
    (typically emails) where an ACME server will send notifications when
    problems occur. The value of `Contact` must be in the form of
    "scheme:address" (e.g. "mailto:user@domain.tld"). The default is an
    empty list which means an ACME server will send no notices.

**Example**:

~~~ yaml
acme:
  ca_url: https://acme-v02.api.letsencrypt.org/directory
  contact:
    - mailto:admin@domain.tld
    - mailto:bot@domain.tld
  auto: true
  cert_type: rsa
~~~

## allow\_contrib\_modules

`true | false`  

Whether to allow installation of third-party modules or not. See
[ejabberd-contrib](../../admin/guide/modules.md#ejabberd-contrib)
documentation section. The default value is `true`.

## allow\_multiple\_connections

`true | false`  

This option is only used when the anonymous mode is enabled. Setting it
to `true` means that the same username can be taken multiple times in
anonymous login mode if different resource are used to connect. This
option is only useful in very special occasions. The default value is
`false`.

## anonymous\_protocol

`login_anon | sasl_anon | both`  

Define what
[anonymous](authentication.md#anonymous-login-and-sasl-anonymous)
protocol will be used:

-   `login_anon` means that the anonymous login method will be used.

-   `sasl_anon` means that the SASL Anonymous method will be used.

-   `both` means that SASL Anonymous and login anonymous are both
    enabled.

The default value is `sasl_anon`.

## api\_permissions

`[Permission, ...]`  

Define the permissions for API access. Please consult the ejabberd Docs
web → For Developers → ejabberd ReST API →
[API Permissions](../../developer/ejabberd-api/permissions.md).

## append\_host\_config

`{Host: Options}`  

Add a few specific options to a certain
[virtual host](../configuration/basic.md#virtual-hosting).

## auth\_cache\_life\_time

`timeout()`  

Same as [cache_life_time](#cache_life_time), but applied to authentication cache only.
If not set, the value from [cache_life_time](#cache_life_time) will be used.

## auth\_cache\_missed

`true | false`  

Same as [cache_missed](#cache_missed), but applied to authentication cache only. If
not set, the value from [cache_missed](#cache_missed) will be used.

## auth\_cache\_size

`pos_integer() | infinity`  

Same as [cache_size](#cache_size), but applied to authentication cache only. If not
set, the value from [cache_size](#cache_size) will be used.

## auth\_external\_user\_exists\_check

`true | false`  

<!-- md:version added in [23.10](../../archive/23.10/index.md) -->

Supplement check for user
existence based on [mod_last](modules.md#mod_last) data, for authentication methods that
don’t have a way to reliably tell if a user exists (like is the case for
`jwt` and certificate based authentication). This helps with processing
offline message for those users. The default value is `true`.

## auth\_method

`[mnesia | sql | anonymous | external | jwt | ldap | pam, ...]`  

A list of [authentication](authentication.md) methods to use. If
several methods are defined, authentication is considered successful as
long as authentication of at least one of the methods succeeds. The
default value is `[mnesia]`.

## auth\_opts

`[Option, ...]`  

This is used by the contributed module `ejabberd_auth_http` that can
be installed from the
[ejabberd-contrib](../../admin/guide/modules.md#ejabberd-contrib) Git
repository. Please refer to that module’s README file for details.

## auth\_password\_format

`plain | scram`  

<!-- md:version improved in [20.01](../../archive/20.01/index.md) -->

The option defines in what
format the users passwords are stored, plain text or in
[SCRAM](authentication.md#scram) format:

-   `plain`: The password is stored as plain text in the database. This
    is risky because the passwords can be read if your database gets
    compromised. This is the default value. This format allows clients
    to authenticate using: the old Jabber Non-SASL (XEP-0078), SASL
    PLAIN, SASL DIGEST-MD5, and SASL SCRAM-SHA-1/256/512(-PLUS).

-   `scram`: The password is not stored, only some information required
    to verify the hash provided by the client. It is impossible to
    obtain the original plain password from the stored information; for
    this reason, when this value is configured it cannot be changed to
    plain anymore. This format allows clients to authenticate using:
    SASL PLAIN and SASL SCRAM-SHA-1/256/512(-PLUS). The SCRAM variant
    depends on the [auth_scram_hash](#auth_scram_hash) option.

The default value is `plain`.

## auth\_scram\_hash

`sha | sha256 | sha512`  

Hash algorithm that should be used to store password in
[SCRAM](authentication.md#scram) format. You shouldn’t change this if
you already have passwords generated with a different algorithm - users
that have such passwords will not be able to authenticate. The default
value is `sha`.

## auth\_stored\_password\_types

`[plain | scram_sha1 | scram_sha256 | scram_sha512]`  

<!-- md:version added in [25.03](../../archive/25.03/index.md) -->

List of password types that
should be stored simultaneously for each user in database. When the user
sets the account password, database will be updated to store the
password in formats compatible with each type listed here. This can be
used to migrate user passwords to a more secure format. If this option
if set, it will override values set in [auth_scram_hash](#auth_scram_hash) and
[auth_password_format](#auth_password_format) options. The default value is `[]`.

## auth\_use\_cache

`true | false`  

Same as [use_cache](#use_cache), but applied to authentication cache only. If not
set, the value from [use_cache](#use_cache) will be used.

## c2s\_cafile

`Path`  

Full path to a file containing one or more CA certificates in PEM
format. All client certificates should be signed by one of these root CA
certificates and should contain the corresponding JID(s) in
`subjectAltName` field. There is no default value.

You can use [host_config](#host_config) to specify this option per-vhost.

To set a specific file per listener, use the listener’s
[cafile](listen-options.md#cafile) option. Please notice that
`c2s_cafile` overrides the listener’s `cafile` option.

## c2s\_ciphers

`[Cipher, ...]`  

A list of OpenSSL ciphers to use for c2s connections. The default value
is shown in the example below:

**Example**:

~~~ yaml
c2s_ciphers:
  - HIGH
  - "!aNULL"
  - "!eNULL"
  - "!3DES"
  - "@STRENGTH"
~~~

## c2s\_dhfile

`Path`  

Full path to a file containing custom DH parameters to use for c2s
connections. Such a file could be created with the command *"openssl
dhparam -out dh.pem 2048"*. If this option is not specified, 2048-bit
MODP Group with 256-bit Prime Order Subgroup will be used as defined in
RFC5114 Section 2.3.

## c2s\_protocol\_options

`[Option, ...]`  

List of general SSL options to use for c2s connections. These map to
OpenSSL’s `set_options()`. The default value is shown in the example
below:

**Example**:

~~~ yaml
c2s_protocol_options:
  - no_sslv3
  - cipher_server_preference
  - no_compression
~~~

## c2s\_tls\_compression

`true | false`  

Whether to enable or disable TLS compression for c2s connections. The
default value is `false`.

## ca\_file

`Path`  

Path to a file of CA root certificates. The default is to use system
defined file if possible.

For server connections, this `ca_file` option is overridden by the
[s2s_cafile](#s2s_cafile) option.

## cache\_life\_time

`timeout()`  

The time of a cached item to keep in cache. Once it’s expired, the
corresponding item is erased from cache. The default value is `1 hour`.
Several modules have a similar option; and some core ejabberd parts
support similar options too, see [auth_cache_life_time](#auth_cache_life_time),
[oauth_cache_life_time](#oauth_cache_life_time), [router_cache_life_time](#router_cache_life_time), and
[sm_cache_life_time](#sm_cache_life_time).

## cache\_missed

`true | false`  

Whether or not to cache missed lookups. When there is an attempt to
lookup for a value in a database and this value is not found and the
option is set to `true`, this attempt will be cached and no attempts
will be performed until the cache expires (see [cache_life_time](#cache_life_time)).
Usually you don’t want to change it. Default is `true`. Several modules
have a similar option; and some core ejabberd parts support similar
options too, see [auth_cache_missed](#auth_cache_missed), [oauth_cache_missed](#oauth_cache_missed),
[router_cache_missed](#router_cache_missed), and [sm_cache_missed](#sm_cache_missed).

## cache\_size

`pos_integer() | infinity`  

A maximum number of items (not memory!) in cache. The rule of thumb, for
all tables except rosters, you should set it to the number of maximum
online users you expect. For roster multiply this number by 20 or so. If
the cache size reaches this threshold, it’s fully cleared, i.e. all
items are deleted, and the corresponding warning is logged. You should
avoid frequent cache clearance, because this degrades performance. The
default value is `1000`. Several modules have a similar option; and some
core ejabberd parts support similar options too, see
[auth_cache_size](#auth_cache_size), [oauth_cache_size](#oauth_cache_size), [router_cache_size](#router_cache_size), and
[sm_cache_size](#sm_cache_size).

## captcha\_cmd

`Path | ModuleName`  

<!-- md:version improved in [23.01](../../archive/23.01/index.md) -->

Full path to a script that
generates [CAPTCHA](basic.md#captcha) images. The keyword *@VERSION@*
is replaced with ejabberd version number in `XX.YY` format. The keyword
`@SEMVER@` is replaced with ejabberd version number in semver format
when compiled with Elixir’s mix, or XX.YY format otherwise.
Alternatively, it can be the name of a module that implements ejabberd
CAPTCHA support. There is no default value: when this option is not set,
CAPTCHA functionality is completely disabled.

**Examples**:

When using the ejabberd installers or container image, the example
captcha scripts can be used like this:

~~~ yaml
captcha_cmd: /opt/ejabberd-@VERSION@/lib/ejabberd-@SEMVER@/priv/bin/captcha.sh
~~~

## captcha\_host

`String`  

Deprecated. Use [captcha_url](#captcha_url) instead.

## captcha\_limit

`pos_integer() | infinity`  

Maximum number of [CAPTCHA](basic.md#captcha) generated images per
minute for any given JID. The option is intended to protect the server
from CAPTCHA DoS. The default value is `infinity`.

## captcha\_url

`URL | auto | undefined`  

<!-- md:version improved in [23.04](../../archive/23.04/index.md) -->

An URL where
[CAPTCHA](basic.md#captcha) requests should be sent. NOTE: you need to
configure `request_handlers` for `ejabberd_http` listener as well. If
set to `auto`, it builds the URL using a `request_handler` already
enabled, with encryption if available. If set to `undefined`, it builds
the URL using the deprecated [captcha_host](#captcha_host) `+ /captcha`. The default
value is `auto`.

## certfiles

`[Path, ...]`  

The option accepts a list of file paths (optionally with wildcards)
containing either PEM certificates or PEM private keys. At startup or
configuration reload, ejabberd reads all certificates from these files,
sorts them, removes duplicates, finds matching private keys and then
rebuilds full certificate chains for the use in TLS connections. Use
this option when TLS is enabled in either of ejabberd listeners:
`ejabberd_c2s`, `ejabberd_http` and so on. NOTE: if you modify the
certificate files or change the value of the option, run *ejabberdctl
reload-config* in order to rebuild and reload the certificate chains.

**Examples**:

If you use [Let’s Encrypt](https://letsencrypt.org) certificates for
your domain "domain.tld", the configuration will look like this:

~~~ yaml
certfiles:
  - /etc/letsencrypt/live/domain.tld/fullchain.pem
  - /etc/letsencrypt/live/domain.tld/privkey.pem
~~~

## cluster\_backend

`Backend`  

A database backend to use for storing information about cluster. The
only available value so far is `mnesia`.

## cluster\_nodes

`[Node, ...]`  

A list of Erlang nodes to connect on ejabberd startup. This option is
mostly intended for ejabberd customization and sophisticated setups. The
default value is an empty list.

## default\_db

`mnesia | sql`  

[Default database](database.md#default-database) to store persistent
data in ejabberd. Modules and other components (e.g. authentication) may
have its own value. The default value is `mnesia`.

## default\_ram\_db

`mnesia | redis | sql`  

Default volatile (in-memory) storage for ejabberd. Modules and other
components (e.g. session management) may have its own value. The default
value is `mnesia`.

## define\_keyword

`{NAME: Value}`  

<!-- md:version added in [25.03](../../archive/25.03/index.md) -->

Allows to define configuration
[keywords](../configuration/file-format.md#macros-and-keywords).

**Example**:

~~~ yaml
define_keyword:
  SQL_USERNAME: "eja.global"

host_config:
  localhost:
    define_keyword:
      SQL_USERNAME: "eja.localhost"

sql_username: "prefix.@SQL_USERNAME@"
~~~

## define\_macro

`{NAME: Value}`  

<!-- md:version improved in [25.03](../../archive/25.03/index.md) -->

Allows to define
configuration
[macros](../configuration/file-format.md#macros-and-keywords).

**Example**:

~~~ yaml
define_macro:
  DEBUG: debug
  LOG_LEVEL: DEBUG
  USERBOB:
    user: bob@localhost

loglevel: LOG_LEVEL

acl:
  admin: USERBOB
~~~

## disable\_sasl\_mechanisms

`[Mechanism, ...]`  

Specify a list of SASL mechanisms (such as `DIGEST-MD5` or `SCRAM-SHA1`)
that should not be offered to the client. For convenience, the value of
`Mechanism` is case-insensitive. The default value is an empty list,
i.e. no mechanisms are disabled by default.

## disable\_sasl\_scram\_downgrade\_protection

`true | false`  

Allows to disable sending data required by *XEP-0474: SASL SCRAM
Downgrade Protection*. There are known buggy clients (like those that
use strophejs 1.6.2) which will not be able to authenticatate when
servers sends data from that specification. This options allows server
to disable it to allow even buggy clients connects, but in exchange
decrease MITM protection. The default value of this option is `false`
which enables this extension.

## domain\_balancing

`{Domain: Options}`  

An algorithm to
[load-balance](../guide/clustering.md#service-load-balancing) the
components that are plugged on an ejabberd cluster. It means that you
can plug one or several instances of the same component on each ejabberd
node and that the traffic will be automatically distributed. The
algorithm to deliver messages to the component(s) can be specified by
this option. For any component connected as `Domain`, available
`Options` are:

-   **component\_number**: `2..1000`  
    The number of components to
    balance.

-   **type**: `Value`  
    How to deliver stanzas to connected components.
    The default value is `random`. Possible values:

    **- bare\_destination**  
    by the bare JID (without resource) of the packet’s `to` attribute

    **- bare\_source**  
    by the bare JID (without resource) of the packet’s `from` attribute
    is used

    **- destination**  
    an instance is chosen by the full JID of the packet’s `to` attribute

    **- random**  
    an instance is chosen at random

    **- source**  
    by the full JID of the packet’s `from` attribute

    **Example**:

    ~~~ yaml
    domain_balancing:
      component.domain.tld:
        type: destination
        component_number: 5
      transport.example.org:
        type: bare_source
    ~~~

## ext\_api\_headers

`Headers`  

String of headers (separated with commas `,`) that will be provided by
ejabberd when sending ReST requests. The default value is an empty
string of headers: `""`.

## ext\_api\_http\_pool\_size

`pos_integer()`  

Define the size of the HTTP pool, that is, the maximum number of
sessions that the ejabberd ReST service will handle simultaneously. The
default value is: `100`.

## ext\_api\_path\_oauth

`Path`  

Define the base URI path when performing OAUTH ReST requests. The
default value is: `"/oauth"`.

## ext\_api\_url

`URL`  

Define the base URI when performing ReST requests. The default value is:
`"http://localhost/api"`.

## extauth\_pool\_name

`Name`  

Define the pool name appendix in
[external auth](authentication.md#external-script), so the full pool
name will be `extauth_pool_Name`. The default value is the hostname.

## extauth\_pool\_size

`Size`  

The option defines the number of instances of the same
[external auth](authentication.md#external-script) program to start for
better load balancing. The default is the number of available CPU cores.

## extauth\_program

`Path`  

Indicate in this option the full path to the
[external authentication script](authentication.md#external-script).
The script must be executable by ejabberd.

## fqdn

`Domain`  

A fully qualified domain name that will be used in SASL DIGEST-MD5
authentication. The default is detected automatically.

## hide\_sensitive\_log\_data

`true | false`  

A privacy option to not log sensitive data (mostly IP addresses). The
default value is `false` for backward compatibility.

## host\_config

`{Host: Options}`  

The option is used to redefine `Options` for
[virtual host](../configuration/basic.md#virtual-hosting) `Host`. In
the example below LDAP authentication method will be used on virtual
host `domain.tld` and SQL method will be used on virtual host
`example.org`.

**Example**:

~~~ yaml
hosts:
  - domain.tld
  - example.org

auth_method:
  - sql

host_config:
  domain.tld:
    auth_method:
      - ldap
~~~

## hosts

`[Domain1, Domain2, ...]`  

List of one or more [host names](../configuration/basic.md#host-names)
(or domains) that ejabberd will serve. This is a **mandatory** option.

## hosts\_alias

`{Alias: Host}`  

<!-- md:version added in 25.xx -->

Define aliases for existing
vhosts managed by ejabberd. An alias may be a regexp expression. This
option is only consulted by the `ejabberd_http` listener.

**Example**:

~~~ yaml
hosts:
  - domain.tld
  - example.org

hosts_alias:
  xmpp.domain.tld: domain.tld
  jabber.domain.tld: domain.tld
  mytest.net: example.org
  "exa*": example.org
~~~

## include\_config\_file

*\[Filename, ...\] | {Filename: Options}*  

Read and
[include additional file](../configuration/file-format.md#include-additional-files)
from `Filename`. If the value is provided in *{Filename: Options}*
format, the `Options` must be one of the following:

-   **allow\_only**: `[OptionName, ...]`  
    Allows only the usage of
    those options in the included file `Filename`. The options that do
    not match this criteria are not accepted. The default value is to
    include all options.

-   **disallow**: `[OptionName, ...]`  
    Disallows the usage of those
    options in the included file `Filename`. The options that match this
    criteria are not accepted. The default value is an empty list.

## install\_contrib\_modules

`[Module, ...]`  

<!-- md:version added in [23.10](../../archive/23.10/index.md) -->

Modules to install from
[ejabberd-contrib](../../admin/guide/modules.md#ejabberd-contrib) at
start time. The default value is an empty list of modules: `[]`.

## jwt\_auth\_only\_rule

`AccessName`  

This ACL rule defines accounts that can use only the
[JWT](authentication.md#jwt-authentication) auth method, even if others
are also defined in the ejabberd configuration file. In other words: if
there are several auth methods enabled for this host (JWT, SQL, …),
users that match this rule can only use JWT. The default value is
`none`.

## jwt\_jid\_field

`FieldName`  

By default, the JID is defined in the `"jid"` JWT field. In this option
you can specify other [JWT](authentication.md#jwt-authentication) field
name where the JID is defined.

## jwt\_key

`FilePath`  

Path to the file that contains the
[JWT](authentication.md#jwt-authentication) key. The default value is
`undefined`.

## language

`Language`  

Define the
[default language](../configuration/basic.md#default-language) of
server strings that can be seen by XMPP clients. If an XMPP client does
not possess `xml:lang` attribute, the specified language is used. The
default value is `"en"`.

## ldap\_backups

`[Host, ...]`  

A list of IP addresses or DNS names of LDAP backup servers (see
[LDAP connection](../configuration/ldap.md#ldap-connection)). When no
servers listed in [ldap_servers](#ldap_servers) option are reachable, ejabberd
connects to these backup servers. The default is an empty list, i.e. no
backup servers specified. Please notice that ejabberd only connects to
the next server when the existing connection is lost; it doesn’t detect
when a previously-attempted server becomes available again.

## ldap\_base

`Base`  

LDAP base directory which stores users accounts. There is no default
value: you must set the option in order for LDAP connections to work
properly.

## ldap\_deref\_aliases

`never | always | finding | searching`  

Whether to dereference aliases or not. The default value is `never`.

## ldap\_dn\_filter

`{Filter: FilterAttrs}`  

This filter is applied on the results returned by the main filter. The
filter performs an additional LDAP lookup to make the complete result.
This is useful when you are unable to define all filter rules in
`ldap_filter`. You can define `"%u"`, `"%d"`, `"%s"* and *"%D"` pattern
variables in *Filter: "%u"* is replaced by a user’s part of the JID,
`"%d"` is replaced by the corresponding domain (virtual host), all
`"%s"` variables are consecutively replaced by values from the
attributes in `FilterAttrs` and *"%D"* is replaced by Distinguished Name
from the result set. There is no default value, which means the result
is not filtered. WARNING: Since this filter makes additional LDAP
lookups, use it only as the last resort: try to define all filter rules
in [ldap_filter](#ldap_filter) option if possible.

**Example**:

~~~ yaml
ldap_dn_filter:
  "(&(name=%s)(owner=%D)(user=%u@%d))": [sn]
~~~

## ldap\_encrypt

`tls | none`  

Whether to encrypt LDAP connection using TLS or not. The default value
is `none`. NOTE: STARTTLS encryption is not supported.

## ldap\_filter

`Filter`  

An LDAP filter as defined in
[RFC4515](https://tools.ietf.org/html/rfc4515). There is no default
value. Example: *"(&(objectClass=shadowAccount)(memberOf=XMPP Users))"*.
NOTE: don’t forget to close brackets and don’t use superfluous
whitespaces. Also you must not use `"uid"` attribute in the filter
because this attribute will be appended to the filter automatically.

## ldap\_password

`Password`  

Bind password. The default value is an empty string.

## ldap\_port

`1..65535`  

Port to connect to your LDAP server. The default port is `389` if
encryption is disabled and `636` if encryption is enabled.

## ldap\_rootdn

`RootDN`  

Bind Distinguished Name. The default value is an empty string, which
means "anonymous connection".

## ldap\_servers

`[Host, ...]`  

A list of IP addresses or DNS names of your LDAP servers (see
[LDAP connection](../configuration/ldap.md#ldap-connection)). ejabberd
connects immediately to all of them, and reconnects infinitely if
connection is lost. The default value is `[localhost]`.

## ldap\_tls\_cacertfile

`Path`  

A path to a file containing PEM encoded CA certificates. This option is
required when TLS verification is enabled.

## ldap\_tls\_certfile

`Path`  

A path to a file containing PEM encoded certificate along with PEM
encoded private key. This certificate will be provided by ejabberd when
TLS enabled for LDAP connections. There is no default value, which means
no client certificate will be sent.

## ldap\_tls\_depth

`Number`  

Specifies the maximum verification depth when TLS verification is
enabled, i.e. how far in a chain of certificates the verification
process can proceed before the verification is considered to be failed.
Peer certificate = 0, CA certificate = 1, higher level CA certificate =
2, etc. The value `2` thus means that a chain can at most contain peer
cert, CA cert, next CA cert, and an additional CA cert. The default
value is `1`.

## ldap\_tls\_verify

`false | soft | hard`  

This option specifies whether to verify LDAP server certificate or not
when TLS is enabled. When `hard` is set, ejabberd doesn’t proceed if the
certificate is invalid. When `soft` is set, ejabberd proceeds even if
the check has failed. The default is `false`, which means no checks are
performed.

## ldap\_uids

*\[Attr\] | {Attr: AttrFormat}*  

LDAP attributes which hold a list of attributes to use as alternatives
for getting the JID, where `Attr` is an LDAP attribute which holds the
user’s part of the JID and `AttrFormat` must contain one and only one
pattern variable *"%u"* which will be replaced by the user’s part of the
JID. For example, *"%<u@example>.org"*. If the value is in the form of
`[Attr]` then `AttrFormat` is assumed to be *"%u"*.

## listen

`[Options, ...]`  

The option for listeners configuration. See the
[Listen Modules](listen.md) section for details.

## log\_burst\_limit\_count

`Number`  

<!-- md:version added in [22.10](../../archive/22.10/index.md) -->

The number of messages to
accept in `log_burst_limit_window_time` period before starting to drop
them. Default `500`

## log\_burst\_limit\_window\_time

`Number`  

<!-- md:version added in [22.10](../../archive/22.10/index.md) -->

The time period to rate-limit
log messages by. Defaults to `1` second.

## log\_modules\_fully

`[Module, ...]`  

<!-- md:version added in [23.01](../../archive/23.01/index.md) -->

List of modules that will log
everything independently from the general loglevel option.

## log\_rotate\_count

`Number`  

The number of rotated log files to keep. The default value is `1`, which
means that only keeps `ejabberd.log.0`, `error.log.0` and `crash.log.0`.

## log\_rotate\_size

`pos_integer() | infinity`  

The size (in bytes) of a log file to trigger rotation. If set to
`infinity`, log rotation is disabled. The default value is 10 Mb
expressed in bytes: `10485760`.

## loglevel

`none | emergency | alert | critical | error | warning | notice | info | debug`  

Verbosity of ejabberd [logging](../configuration/basic.md#logging). The
default value is `info`. NOTE: previous versions of ejabberd had log
levels defined in numeric format (`0..5`). The numeric values are still
accepted for backward compatibility, but are not recommended.

## max\_fsm\_queue

`Size`  

This option specifies the maximum number of elements in the queue of the
FSM (Finite State Machine). Roughly speaking, each message in such
queues represents one XML stanza queued to be sent into its relevant
outgoing stream. If queue size reaches the limit (because, for example,
the receiver of stanzas is too slow), the FSM and the corresponding
connection (if any) will be terminated and error message will be logged.
The reasonable value for this option depends on your hardware
configuration. The allowed values are positive integers. The default
value is `10000`.

## modules

`{Module: Options}`  

Set all the [modules](modules.md) configuration options.

## negotiation\_timeout

`timeout()`  

Time to wait for an XMPP stream negotiation to complete. When timeout
occurs, the corresponding XMPP stream is closed. The default value is
`120` seconds.

## net\_ticktime

`timeout()`  

This option can be used to tune tick time parameter of `net_kernel`. It
tells Erlang VM how often nodes should check if intra-node communication
was not interrupted. This option must have identical value on all nodes,
or it will lead to subtle bugs. Usually leaving default value of this is
option is best, tweak it only if you know what you are doing. The
default value is `1 minute`.

## new\_sql\_schema

`true | false`  

Whether to use the
[new SQL schema](database.md#default-and-new-schemas). All schemas are
located at <https://github.com/processone/ejabberd/tree/25.04/sql>.
There are two schemas available. The default legacy schema stores one
XMPP domain into one ejabberd database. The `new` schema can handle
several XMPP domains in a single ejabberd database. Using this `new`
schema is best when serving several XMPP domains and/or changing domains
from time to time. This avoid need to manage several databases and
handle complex configuration changes. The default depends on
configuration flag `--enable-new-sql-schema` which is set at compile
time.

## oauth\_access

`AccessName`  

By default creating OAuth tokens is not allowed. To define which users
can create OAuth tokens, you can refer to an ejabberd access rule in the
`oauth_access` option. Use `all` to allow everyone to create tokens.

## oauth\_cache\_life\_time

`timeout()`  

Same as [cache_life_time](#cache_life_time), but applied to OAuth cache only. If not
set, the value from [cache_life_time](#cache_life_time) will be used.

## oauth\_cache\_missed

`true | false`  

Same as [cache_missed](#cache_missed), but applied to OAuth cache only. If not set,
the value from [cache_missed](#cache_missed) will be used.

## oauth\_cache\_rest\_failure\_life\_time

`timeout()`  

<!-- md:version added in [21.01](../../archive/21.01/index.md) -->

The time that a failure in
OAuth ReST is cached. The default value is `infinity`.

## oauth\_cache\_size

`pos_integer() | infinity`  

Same as [cache_size](#cache_size), but applied to OAuth cache only. If not set, the
value from [cache_size](#cache_size) will be used.

## oauth\_client\_id\_check

`allow | db | deny`  

Define whether the client authentication is always allowed, denied, or
it will depend if the client ID is present in the database. The default
value is `allow`.

## oauth\_db\_type

`mnesia | sql`  

Database backend to use for OAuth authentication. The default value is
picked from [default_db](#default_db) option, or if it’s not set, `mnesia` will be
used.

## oauth\_expire

`timeout()`  

Time during which the OAuth token is valid, in seconds. After that
amount of time, the token expires and the delegated credential cannot be
used and is removed from the database. The default is `4294967` seconds.

## oauth\_use\_cache

`true | false`  

Same as [use_cache](#use_cache), but applied to OAuth cache only. If not set, the
value from [use_cache](#use_cache) will be used.

## oom\_killer

`true | false`  

Enable or disable OOM (out-of-memory) killer. When system memory raises
above the limit defined in [oom_watermark](#oom_watermark) option, ejabberd triggers
OOM killer to terminate most memory consuming Erlang processes. Note
that in order to maintain functionality, ejabberd only attempts to kill
transient processes, such as those managing client sessions, s2s or
database connections. The default value is `true`.

## oom\_queue

`Size`  

Trigger OOM killer when some of the running Erlang processes have
messages queue above this `Size`. Note that such processes won’t be
killed if [oom_killer](#oom_killer) option is set to `false` or if
[oom_watermark](#oom_watermark) is not reached yet.

## oom\_watermark

`Percent`  

A percent of total system memory consumed at which OOM killer should be
activated with some of the processes possibly be killed (see
[oom_killer](#oom_killer) option). Later, when memory drops below this `Percent`,
OOM killer is deactivated. The default value is `80` percents.

## outgoing\_s2s\_families

`[ipv6 | ipv4, ...]`  

<!-- md:version changed in [23.01](../../archive/23.01/index.md) -->

Specify which address
families to try, in what order. The default is `[ipv6, ipv4]` which
means it first tries connecting with IPv6, if that fails it tries using
IPv4. This option is obsolete and irrelevant when using ejabberd [23.01](../../archive/23.01/index.md)
and Erlang/OTP 22, or newer versions of them.

## outgoing\_s2s\_ipv4\_address

`Address`  

<!-- md:version added in [20.12](../../archive/20.12/index.md) -->

Specify the IPv4 address that
will be used when establishing an outgoing S2S IPv4 connection, for
example `"127.0.0.1"`. The default value is `undefined`.

## outgoing\_s2s\_ipv6\_address

`Address`  

<!-- md:version added in [20.12](../../archive/20.12/index.md) -->

Specify the IPv6 address that
will be used when establishing an outgoing S2S IPv6 connection, for
example `"::FFFF:127.0.0.1"`. The default value is `undefined`.

## outgoing\_s2s\_port

`1..65535`  

A port number to use for outgoing s2s connections when the target server
doesn’t have an SRV record. The default value is `5269`.

## outgoing\_s2s\_timeout

`timeout()`  

The timeout in seconds for outgoing S2S connection attempts. The default
value is `10` seconds.

## pam\_service

`Name`  

This option defines the [PAM](authentication.md#pam-authentication)
service name. Refer to the PAM documentation of your operation system
for more information. The default value is `ejabberd`.

## pam\_userinfotype

`username | jid`  

This option defines what type of information about the user ejabberd
provides to the [PAM](authentication.md#pam-authentication) service:
only the username, or the user’s JID. Default is `username`.

## pgsql\_users\_number\_estimate

`true | false`  

Whether to use PostgreSQL estimation when counting registered users. The
default value is `false`.

## queue\_dir

`Directory`  

If [queue_type](#queue_type) option is set to `file`, use this `Directory` to store
file queues. The default is to keep queues inside Mnesia directory.

## queue\_type

`ram | file`  

Default type of queues in ejabberd. Modules may have its own value of
the option. The value of `ram` means that queues will be kept in memory.
If value `file` is set, you may also specify directory in [queue_dir](#queue_dir)
option where file queues will be placed. The default value is `ram`.

## redis\_connect\_timeout

`timeout()`  

A timeout to wait for the connection to be re-established to the
[Redis](database.md#redis) server. The default is `1 second`.

## redis\_db

`Number`  

[Redis](database.md#redis) database number. The default is `0`.

## redis\_password

`Password`  

The password to the [Redis](database.md#redis) server. The default is
an empty string, i.e. no password.

## redis\_pool\_size

`Number`  

The number of simultaneous connections to the
[Redis](database.md#redis) server. The default value is `10`.

## redis\_port

`1..65535`  

The port where the [Redis](database.md#redis) server is accepting
connections. The default is `6379`.

## redis\_queue\_type

`ram | file`  

The type of request queue for the [Redis](database.md#redis) server.
See description of [queue_type](#queue_type) option for the explanation. The
default value is the value defined in [queue_type](#queue_type) or `ram` if the
latter is not set.

## redis\_server

`Host | IP Address | Unix Socket Path`  

<!-- md:version improved in [24.12](../../archive/24.12/index.md) -->

A hostname, IP address or
unix domain socket file of the [Redis](database.md#redis) server. Setup
the path to unix domain socket like: `"unix:/path/to/socket"`. The
default value is `localhost`.

## registration\_timeout

`timeout()`  

This is a global option for module [mod_register](modules.md#mod_register). It limits the
frequency of registrations from a given IP or username. So, a user that
tries to register a new account from the same IP address or JID during
this time after their previous registration will receive an error with
the corresponding explanation. To disable this limitation, set the value
to `infinity`. The default value is `600 seconds`.

## resource\_conflict

`setresource | closeold | closenew`  

NOTE: this option is deprecated and may be removed anytime in the future
versions. The possible values match exactly the three possibilities
described in [XMPP Core: section
7.7.2.2](https://tools.ietf.org/html/rfc6120#section-7.7.2.2). The
default value is `closeold`. If the client uses old Jabber Non-SASL
authentication (XEP-0078), then this option is not respected, and the
action performed is `closeold`.

## rest\_proxy

`Host`  

Address of a HTTP Connect proxy used by modules issuing rest calls (like
ejabberd\_oauth\_rest)

## rest\_proxy\_password

`string()`  

Password used to authenticate to HTTP Connect proxy used by modules
issuing rest calls (like ejabberd\_oauth\_rest)

## rest\_proxy\_port

`1..65535`  

Port of a HTTP Connect proxy used by modules issuing rest calls (like
ejabberd\_oauth\_rest)

## rest\_proxy\_username

`string()`  

Username used to authenticate to HTTP Connect proxy used by modules
issuing rest calls (like ejabberd\_oauth\_rest)

## router\_cache\_life\_time

`timeout()`  

Same as [cache_life_time](#cache_life_time), but applied to routing table cache only. If
not set, the value from [cache_life_time](#cache_life_time) will be used.

## router\_cache\_missed

`true | false`  

Same as [cache_missed](#cache_missed), but applied to routing table cache only. If
not set, the value from [cache_missed](#cache_missed) will be used.

## router\_cache\_size

`pos_integer() | infinity`  

Same as [cache_size](#cache_size), but applied to routing table cache only. If not
set, the value from [cache_size](#cache_size) will be used.

## router\_db\_type

`mnesia | redis | sql`  

Database backend to use for routing information. The default value is
picked from [default_ram_db](#default_ram_db) option, or if it’s not set, `mnesia` will
be used.

## router\_use\_cache

`true | false`  

Same as [use_cache](#use_cache), but applied to routing table cache only. If not
set, the value from [use_cache](#use_cache) will be used.

## rpc\_timeout

`timeout()`  

A timeout for remote function calls between nodes in an ejabberd
cluster. You should probably never change this value since those calls
are used for internal needs only. The default value is `5` seconds.

## s2s\_access

`Access`  

This [Access Rule](basic.md#access-rules) defines to what remote
servers can s2s connections be established. The default value is `all`;
no restrictions are applied, it is allowed to connect s2s to/from all
remote servers.

## s2s\_cafile

`Path`  

A path to a file with CA root certificates that will be used to
authenticate s2s connections. If not set, the value of [ca_file](#ca_file) will
be used.

You can use [host_config](#host_config) to specify this option per-vhost.

## s2s\_ciphers

`[Cipher, ...]`  

A list of OpenSSL ciphers to use for s2s connections. The default value
is shown in the example below:

**Example**:

~~~ yaml
s2s_ciphers:
  - HIGH
  - "!aNULL"
  - "!eNULL"
  - "!3DES"
  - "@STRENGTH"
~~~

## s2s\_dhfile

`Path`  

Full path to a file containing custom DH parameters to use for s2s
connections. Such a file could be created with the command *"openssl
dhparam -out dh.pem 2048"*. If this option is not specified, 2048-bit
MODP Group with 256-bit Prime Order Subgroup will be used as defined in
RFC5114 Section 2.3.

## s2s\_dns\_retries

`Number`  

DNS resolving retries. The default value is `2`.

## s2s\_dns\_timeout

`timeout()`  

The timeout for DNS resolving. The default value is `10` seconds.

## s2s\_max\_retry\_delay

`timeout()`  

The maximum allowed delay for s2s connection retry to connect after a
failed connection attempt. The default value is `300` seconds (5
minutes).

## s2s\_protocol\_options

`[Option, ...]`  

List of general SSL options to use for s2s connections. These map to
OpenSSL’s `set_options()`. The default value is shown in the example
below:

**Example**:

~~~ yaml
s2s_protocol_options:
  - no_sslv3
  - cipher_server_preference
  - no_compression
~~~

## s2s\_queue\_type

`ram | file`  

The type of a queue for s2s packets. See description of [queue_type](#queue_type)
option for the explanation. The default value is the value defined in
[queue_type](#queue_type) or `ram` if the latter is not set.

## s2s\_timeout

`timeout()`  

A time to wait before closing an idle s2s connection. The default value
is `1` hour.

## s2s\_tls\_compression

`true | false`  

Whether to enable or disable TLS compression for s2s connections. The
default value is `false`.

## s2s\_use\_starttls

`true | false | optional | required`  

Whether to use STARTTLS for s2s connections. The value of `false` means
STARTTLS is prohibited. The value of `true` or `optional` means STARTTLS
is enabled but plain connections are still allowed. And the value of
`required` means that only STARTTLS connections are allowed. The default
value is `false` (for historical reasons).

## s2s\_zlib

`true | false`  

Whether to use `zlib` compression (as defined in
[XEP-0138](https://xmpp.org/extensions/xep-0138.html)) or not. The
default value is `false`. WARNING: this type of compression is nowadays
considered insecure.

## shaper

`{ShaperName: Rate}`  

The option defines a set of
[shapers](../configuration/basic.md#shapers). Every shaper is assigned
a name `ShaperName` that can be used in other parts of the configuration
file, such as [shaper_rules](#shaper_rules) option. The shaper itself is defined by
its `Rate`, where `Rate` stands for the maximum allowed incoming rate in
**bytes** per second. When a connection exceeds this limit, ejabberd
stops reading from the socket until the average rate is again below the
allowed maximum. In the example below shaper `normal` limits the traffic
speed to 1,000 bytes/sec and shaper `fast` limits the traffic speed to
50,000 bytes/sec:

**Example**:

~~~ yaml
shaper:
  normal: 1000
  fast: 50000
~~~

## shaper\_rules

`{ShaperRuleName: {Number|ShaperName: ACLName|ACLDefinition}}`  

This option defines
[shaper rules](../configuration/basic.md#shaper-rules) to use for
matching user/hosts. Semantics is similar to [access_rules](#access_rules) option,
the only difference is that instead using `allow` or `deny`, a name of a
shaper (defined in [shaper](#shaper) option) or a positive number should be
used.

**Example**:

~~~ yaml
shaper_rules:
  connections_limit:
    10:
      user: peter@example.com
    100: admin
    5: all
  download_speed:
    fast: admin
    slow: anonymous_users
    normal: all
  log_days: 30
~~~

## sm\_cache\_life\_time

`timeout()`  

Same as [cache_life_time](#cache_life_time), but applied to client sessions table cache
only. If not set, the value from [cache_life_time](#cache_life_time) will be used.

## sm\_cache\_missed

`true | false`  

Same as [cache_missed](#cache_missed), but applied to client sessions table cache
only. If not set, the value from [cache_missed](#cache_missed) will be used.

## sm\_cache\_size

`pos_integer() | infinity`  

Same as [cache_size](#cache_size), but applied to client sessions table cache only.
If not set, the value from [cache_size](#cache_size) will be used.

## sm\_db\_type

`mnesia | redis | sql`  

Database backend to use for client sessions information. The default
value is picked from [default_ram_db](#default_ram_db) option, or if it’s not set,
`mnesia` will be used.

## sm\_use\_cache

`true | false`  

Same as [use_cache](#use_cache), but applied to client sessions table cache only.
If not set, the value from [use_cache](#use_cache) will be used.

## sql\_connect\_timeout

`timeout()`  

A time to wait for connection to an SQL server to be established. The
default value is `5` seconds.

## sql\_database

`Database`  

An SQL database name. For SQLite this must be a full path to a database
file. The default value is `ejabberd`.

## sql\_flags

`[mysql_alternative_upsert]`  

<!-- md:version added in [24.02](../../archive/24.02/index.md) -->

This option accepts a list of
SQL flags, and is empty by default. `mysql_alternative_upsert` forces
the alternative upsert implementation in MySQL.

## sql\_keepalive\_interval

`timeout()`  

An interval to make a dummy SQL request to keep alive the connections to
the database. There is no default value, so no keepalive requests are
made.

## sql\_odbc\_driver

`Path`  

<!-- md:version added in [20.12](../../archive/20.12/index.md) -->

Path to the ODBC driver to use
to connect to a Microsoft SQL Server database. This option only applies
if the [sql_type](#sql_type) option is set to `mssql` and [sql_server](#sql_server) is not
an ODBC connection string. The default value is: `libtdsodbc.so`

## sql\_password

`Password`  

The password for SQL authentication. The default is empty string.

## sql\_pool\_size

`Size`  

Number of connections to the SQL server that ejabberd will open for each
virtual host. The default value is `10`. WARNING: for SQLite this value
is `1` by default and it’s not recommended to change it due to potential
race conditions.

## sql\_port

`1..65535`  

The port where the SQL server is accepting connections. The default is
`3306` for MySQL, `5432` for PostgreSQL and `1433` for MS SQL. The
option has no effect for SQLite.

## sql\_prepared\_statements

`true | false`  

<!-- md:version added in [20.01](../../archive/20.01/index.md) -->

This option is `true` by
default, and is useful to disable prepared statements. The option is
valid for PostgreSQL and MySQL.

## sql\_query\_timeout

`timeout()`  

A time to wait for an SQL query response. The default value is `60`
seconds.

## sql\_queue\_type

`ram | file`  

The type of a request queue for the SQL server. See description of
[queue_type](#queue_type) option for the explanation. The default value is the
value defined in [queue_type](#queue_type) or `ram` if the latter is not set.

## sql\_server

`Host | IP Address | ODBC Connection String | Unix Socket Path`  

<!-- md:version improved in [24.06](../../archive/24.06/index.md) -->

The hostname or IP address
of the SQL server. For [sql_type](#sql_type) `mssql` or `odbc` this can also be
an ODBC connection string. When [sql_type](#sql_type) is `mysql` or `pgsql`, this
can be the path to a unix domain socket expressed like:
`"unix:/path/to/socket"`.The default value is `localhost`.

## sql\_ssl

`true | false`  

<!-- md:version improved in [20.03](../../archive/20.03/index.md) -->

Whether to use SSL
encrypted connections to the SQL server. The option is only available
for MySQL, MS SQL and PostgreSQL. The default value is `false`.

## sql\_ssl\_cafile

`Path`  

A path to a file with CA root certificates that will be used to verify
SQL connections. Implies [sql_ssl](#sql_ssl) and [sql_ssl_verify](#sql_ssl_verify) options are
set to `true`. There is no default which means certificate verification
is disabled. This option has no effect for MS SQL.

## sql\_ssl\_certfile

`Path`  

A path to a certificate file that will be used for SSL connections to
the SQL server. Implies [sql_ssl](#sql_ssl) option is set to `true`. There is no
default which means ejabberd won’t provide a client certificate to the
SQL server. This option has no effect for MS SQL.

## sql\_ssl\_verify

`true | false`  

Whether to verify SSL connection to the SQL server against CA root
certificates defined in [sql_ssl_cafile](#sql_ssl_cafile) option. Implies [sql_ssl](#sql_ssl)
option is set to `true`. This option has no effect for MS SQL. The
default value is `false`.

## sql\_start\_interval

`timeout()`  

A time to wait before retrying to restore failed SQL connection. The
default value is `30` seconds.

## sql\_type

`mssql | mysql | odbc | pgsql | sqlite`  

The type of an SQL connection. The default is `odbc`.

## sql\_username

`Username`  

A user name for SQL authentication. The default value is `ejabberd`.

## trusted\_proxies

`all | [Network1, Network2, ...]`  

Specify what proxies are trusted when an HTTP request contains the
header `X-Forwarded-For`. You can specify `all` to allow all proxies, or
specify a list of IPs, possibly with masks. The default value is an
empty list. Using this option you can know the real IP of the request,
for admin purpose, or security configuration (for example using
[mod_fail2ban](modules.md#mod_fail2ban)). IMPORTANT: The proxy MUST be configured to set the
`X-Forwarded-For` header if you enable this option as, otherwise, the
client can set it itself and as a result the IP value cannot be trusted
for security rules in ejabberd.

## update\_sql\_schema

`true | false`  

<!-- md:version updated in [24.06](../../archive/24.06/index.md) -->

Allow ejabberd to update SQL
schema in MySQL, PostgreSQL and SQLite databases. This option was added
in ejabberd [23.10](../../archive/23.10/index.md), and enabled by default since [24.06](../../archive/24.06/index.md). The default value
is `true`.

## update\_sql\_schema\_timeout

`timeout()`  

<!-- md:version added in [24.07](../../archive/24.07/index.md) -->

Time allocated to SQL schema
update queries. The default value is set to 5 minutes.

## use\_cache

`true | false`  

Enable or disable cache. The default is `true`. Several modules have a
similar option; and some core ejabberd parts support similar options
too, see [auth_use_cache](#auth_use_cache), [oauth_use_cache](#oauth_use_cache), [router_use_cache](#router_use_cache),
and [sm_use_cache](#sm_use_cache).

## validate\_stream

`true | false`  

Whether to validate any incoming XML packet according to the schemas of
[supported XMPP
extensions](https://github.com/processone/xmpp#supported-xmpp-elements).
WARNING: the validation is only intended for the use by client
developers - don’t enable it in production environment. The default
value is `false`.

## version

`string()`  

The option can be used to set custom ejabberd version, that will be used
by different parts of ejabberd, for example by [mod_version](modules.md#mod_version) module.
The default value is obtained at compile time from the underlying
version control system.

## websocket\_origin

`ignore | URL`  

This option enables validation for `Origin` header to protect against
connections from other domains than given in the configuration file. In
this way, the lower layer load balancer can be chosen for a specific
ejabberd implementation while still providing a secure WebSocket
connection. The default value is `ignore`. An example value of the `URL`
is `"https://test.example.org:8081"`.

## websocket\_ping\_interval

`timeout()`  

Defines time between pings sent by the server to a client (WebSocket
level protocol pings are used for this) to keep a connection active. If
the client doesn’t respond to two consecutive pings, the connection will
be assumed as closed. The value of `0` can be used to disable the
feature. This option makes the server sending pings only for connections
using the RFC compliant protocol. For older style connections the server
expects that whitespace pings would be used for this purpose. The
default value is `60` seconds.

## websocket\_timeout

`timeout()`  

Amount of time without any communication after which the connection
would be closed. The default value is `300` seconds.


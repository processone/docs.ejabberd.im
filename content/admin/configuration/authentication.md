# Authentication

## Supported Methods

The authentication methods supported by `ejabberd` are:

- `internal` — See section [Internal](#internal).

- `external` — See section [External Script](#external-script).

- `ldap` — See section  [LDAP](ldap.md#ldap-authentication).

- `sql` — See section [Relational Databases](database.md#sql-authentication).

- `anonymous` — See section [Anonymous Login and SASL Anonymous](#anonymous-login-and-sasl-anonymous).

- `pam` — See section [PAM Authentication](#pam-authentication).

- `jwt` — See section [JWT Authentication](#jwt-authentication).

The top-level option [auth_method](toplevel.md#auth_method)
defines the authentication methods that are
used for user authentication.
The option syntax is:

``` yaml
auth_method: [Method1, Method2, ...]
```

When the `auth_method` option is omitted, `ejabberd` relies on the default database which is configured in [`default_db`](toplevel.md#default_db) option. If this option is not set neither, then the default authentication method will be `internal`.

Account creation is only supported by `internal`, `external` and `sql` auth methods.

## General Options

The top-level option
[auth_password_format](toplevel.md#auth_password_format)
allows to store the passwords in SCRAM format,
see the [SCRAM](#scram) section.

Other top-level options that are relevant to the authentication configuration:
[disable_sasl_mechanisms](toplevel.md#disable_sasl_mechanisms),
[fqdn](toplevel.md#fqdn).

Authentication caching is enabled by default, and can be
disabled in a specific vhost with the option
[auth_use_cache](toplevel.md#auth_use_cache).
The global authentication cache can be configured for all the authentication
methods with the global top-level options:
[auth_cache_missed](toplevel.md#auth_cache_missed),
[auth_cache_size](toplevel.md#auth_cache_size),
[auth_cache_life_time](toplevel.md#auth_cache_life_time).
For example:

``` yaml
auth_cache_size: 1500
auth_cache_life_time: 10 minutes

host_config:
  example.org:
    auth_method: [internal]
  example.net:
    auth_method: [ldap]
    auth_use_cache: false
```

## Internal

`ejabberd` uses its internal Mnesia database as the default
authentication method. The value `internal` will enable the internal
authentication method.

To store the passwords in SCRAM format instead of plaintext,
see the [SCRAM](#scram) section.

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

## External Script

In the `external` authentication method, ejabberd uses a custom
script to perform authentication tasks.
The server administrator can write that external authentication script in
any programming language.

Please check some example scripts,
and the details on the interface between ejabberd and the script in the
[Developers > Internals > External Authentication](../../developer/guide.md#external-authentication) section.

Options:

- [extauth_pool_name](toplevel.md#extauth_pool_name)
- [extauth_pool_size](toplevel.md#extauth_pool_size)
- [extauth_program](toplevel.md#extauth_program)

Please note that caching interferes with the ability
to maintain multiple passwords per account.
So if your authentication mechanism supports application-specific passwords,
caching must be disabled in the host that uses this authentication method with the
option [auth_use_cache](toplevel.md#auth_use_cache).

This example sets external authentication, specifies the extauth script,
disables caching, and starts three instances of the script for
each virtual host defined in ejabberd:

``` yaml
auth_method: [external]
extauth_program: /etc/ejabberd/JabberAuth.class.php
extauth_pool_size: 3
auth_use_cache: false
```

## Anonymous Login and SASL Anonymous

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
 is hard to find a free username. The main disadvantage is that you
 need a client that specifically supports the SASL Anonymous
 protocol.

The anonymous authentication method can be configured with the following
options. Remember that you can use the [host_config](toplevel.md#host_config) option to set
virtual host specific options (see section [Virtual Hosting](basic.md#virtual-hosting)):

- [allow_multiple_connections](toplevel.md#allow_multiple_connections)
- [anonymous_protocol](toplevel.md#anonymous_protocol)

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

- To enable anonymous login and internal authentication on a virtual
 host:

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

- To enable SASL Anonymous, anonymous login, and internal
 authentication on a virtual host:

    ``` yaml
    host_config:
      public.example.org:
        auth_method:
          - internal
          - anonymous
        anonymous_protocol: both
    ```

There are more configuration examples and XMPP client example stanzas in
[`Anonymous users support`](https://ejabberd.im/Anonymous-users-support).

## PAM Authentication

`ejabberd` supports authentication via Pluggable Authentication Modules
(PAM). PAM is currently supported in AIX, FreeBSD, HP-UX, Linux, Mac OS
X, NetBSD and Solaris.

If compiling ejabberd from source code, PAM support is disabled by default,
so you have to enable PAM support when
[configuring](../install/source.md#configure)
the `ejabberd` compilation: `./configure --enable-pam`

Options:

- [pam_service](toplevel.md#pam_service)
- [pam_userinfotype](toplevel.md#pam_userinfotype)

Example:

``` yaml
auth_method: [pam]
pam_service: ejabberd
```

Though it is quite easy to set up PAM support in `ejabberd`, there are several
problems that you may need to solve:

- To perform PAM authentication `ejabberd` uses external C-program
 called `epam`. By default, it is located in
 `/var/lib/ejabberd/priv/bin/` directory. You have to set it root on
 execution in the case when your PAM module requires root privileges
 (`pam_unix.so` for example). Also you have to grant access for
 `ejabberd` to this file and remove all other permissions from it.
 Execute with root privileges:

    ```sh
    chown root:ejabberd /var/lib/ejabberd/priv/bin/epam
    chmod 4750 /var/lib/ejabberd/priv/bin/epam
    ```

- Make sure you have the latest version of PAM installed on your
 system. Some old versions of PAM modules cause memory leaks. If you
 are not able to use the latest version, you can `kill(1)` `epam`
 process periodically to reduce its memory consumption: `ejabberd`
 will restart this process immediately.

- ejabberd [binary installers](../install/binary-installer.md) include `epam`
 pointing to module paths that may not work in your system.
 If authentication doesn't work correctly, check if syslog
 (example: `journalctl -t epam -f`)
 reports errors like `PAM unable to dlopen(/home/runner/... No such file or directory`.
 In that case, create a PAM configuration file
 (example: `/etc/pam.d/ejabberd`)
 and provide the real path to that file in your machine:

    ```sh
    #%PAM-1.0
    auth        sufficient  /usr/lib/x86_64-linux-gnu/security/pam_unix.so audit
    account     sufficient  /usr/lib/x86_64-linux-gnu/security/pam_unix.so audit
    ```

- `epam` program tries to turn off delays on authentication failures.
 However, some PAM modules ignore this behavior and rely on their own
 configuration options. You can create a configuration file
 (in Debian it would be `/etc/pam.d/ejabberd`).
 This example shows how to turn off delays in
 `pam_unix.so` module:

    ```sh
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

- If you use `pam_winbind` to authorize against a Windows Active
 Directory, then `/etc/nsswitch.conf` must be configured to use
 `winbind` as well.

## JWT Authentication

`ejabberd` supports authentication using JSON Web Token (JWT).  When enabled,
clients send signed tokens instead of passwords, which are checked using a
private key specified in the [jwt_key](toplevel.md#jwt_key) option.
JWT payload must look like this:

``` json
{
  "jid": "test@example.org",
  "exp": 1564436511
}
```

Options:

- [jwt_key](toplevel.md#jwt_key)
- [jwt_auth_only_rule](toplevel.md#jwt_auth_only_rule)
- [jwt_jid_field](toplevel.md#jwt_jid_field)

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
  jwt_only:
    deny: admin
    allow: all

jwt_auth_only_rule: jwt_only
```

Please notice that, when using JWT authentication,
[mod_offline](modules.md#mod_offline) will not work.
With JWT authentication the accounts do not exist in the database,
and there is no way to know if a given account exists or not.

For more information about JWT authentication, you can check a brief tutorial in the
[ejabberd 19.08 release notes](https://www.process-one.net/blog/ejabberd-19-08/).

## SCRAM

The top-level option
[`auth_password_format`](toplevel.md#auth_password_format)
defines in what format the users passwords are stored:
SCRAM format or plaintext format.

The top-level option
[`auth_scram_hash`](toplevel.md#auth_scram_hash)
defines the hash algorithm that will be used to scram the password.

ejabberd supports channel binding to the external channel,
allowing the clients to use `-PLUS` authentication mechanisms.

In summary, depending on the configured options, ejabberd supports:

- `SCRAM_SHA-1(-PLUS)`
- `SCRAM_SHA-256(-PLUS)`
- `SCRAM_SHA-512(-PLUS)`

For details about the client-server communication when using SCRAM,
refer to [SASL Authentication and SCRAM](https://wiki.xmpp.org/web/SASL_Authentication_and_SCRAM).

### Internal storage

When ejabberd starts with internal auth method and SCRAM password format configured:

``` yaml
auth_method: internal
auth_password_format: scram
```

and detects that there are plaintext passwords stored,
they are automatically converted to SCRAM format:

``` log
[info] Passwords in Mnesia table 'passwd' will be SCRAM'ed
[info] Transforming table 'passwd', this may take a while
```

### SQL Database

Please note that if you use SQL auth method and SCRAM password format,
the plaintext passwords already stored in the database are not automatically
converted to SCRAM format.

To convert plaintext passwords to SCRAM format in your database,
use the [convert_to_scram](../../developer/ejabberd-api/admin-api.md#convert_to_scram) command:

``` sh
ejabberdctl convert_to_scram example.org
```

### Foreign authentication

*Note on SCRAM using and foreign authentication limitations*:
when using
the SCRAM password format, it is not possible to use foreign
authentication method in ejabberd, as the real password is not known.

Foreign authentication are use to authenticate through various bridges
ejabberd provide. Foreign authentication includes at the moment SIP
and TURN auth support and they will not be working with SCRAM.

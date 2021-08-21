---
title: Authentication
toc: true
menu: Authentication
order: 50
---

The toplevel option [auth_method](/admin/configuration/toplevel/#auth-method)
defines the authentication methods that are
used for user authentication. The option syntax is:

`auth_method: [Method1, Method2, ...]`

The following authentication methods are supported by `ejabberd`:

-   `internal` — See section [Internal](#internal).

-   `external` — See section [External Script](#external-script).

-   `ldap` — See section  [LDAP](/admin/configuration/database-ldap/#ldap-autentication).

-   `sql` — See section [Relational Databases](/admin/configuration/database-ldap/#relational-databases).

-   `anonymous` — See section [Anonymous Login and SASL Anonymous](#anonymous-login-and-sasl-anonymous).

-   `pam` — See section [Pam Authentication](#pam-authentication).

-   `jwt` — See section [JWT Authentication](#jwt-authentication).

When the option is omitted, ejabberd will rely upon the default database which is configured in `default_db` option. If this option is not set neither the default authentication method will be `internal`.

Account creation is only supported by `internal`, `external` and `sql` auth methods.

Other toplevel options that are relevant to the authentication configuration:
[disable_sasl_mechanisms](/admin/configuration/toplevel/#disable-sasl-mechanisms),
[fqdn](/admin/configuration/toplevel/#fqdn).

# Internal

`ejabberd` uses its internal Mnesia database as the default
authentication method. The value `internal` will enable the internal
authentication method.

To store the passwords in SCRAM format instead of plaintext,
see the [SCRAM](#scram) section.

Examples:

-   To use internal authentication on `example.org` and LDAP
	authentication on `example.net`:


		host_config:
		  example.org:
		    auth_method: [internal]
		  example.net:
		    auth_method: [ldap]

-   To use internal authentication with hashed passwords on all virtual
	hosts:


		auth_method: internal
		auth_password_format: scram

# External Script

In this authentication method, when `ejabberd` starts, it start a
script, and calls it to perform authentication tasks.

The server administrator can write the external authentication script in
any language. The details on the interface between ejabberd and the
script are described in the `ejabberd Developers Guide`. There are also
[`several example authentication scripts`](https://ejabberd.im/extauth).

Options:

- [extauth_pool_name](/admin/configuration/toplevel/#extauth-pool-name)
- [extauth_pool_size](/admin/configuration/toplevel/#extauth-pool-size)
- [extauth_program](/admin/configuration/toplevel/#extauth-program)

Starting in *ejabberd 17.06*, caching has received a complete overhaul.
	Instead of `extauth_cache`, a set of new variables describes cache
	behaviour, and the default value is now `true`. Note that caching
	interferes with the ability to maintain multiple passwords per
	account. So if your authentication mechanism supports
	application-specific passwords, caching must be disabled.
	Those options are:
[auth_use_cache](/admin/configuration/toplevel/#auth-use-cache),
[auth_cache_missed](/admin/configuration/toplevel/#auth-cache-missed),
[auth_cache_size](/admin/configuration/toplevel/#auth-cache-size), and
[auth_cache_life_time](/admin/configuration/toplevel/#auth-cache-life-time).

This example sets external authentication, the extauth script, enables
caching for 10 minutes, and starts three instances of the script for
each virtual host defined in ejabberd:


	auth_method: [external]
	extauth_program: /etc/ejabberd/JabberAuth.class.php
	extauth_instances: 3
	auth_use_cache: false

# Anonymous Login and SASL Anonymous

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
options. Remember that you can use the [host_config](/admin/configuration/toplevel/#host-config) option to set
virtual host specific options (see section [Virtual Hosting](/admin/configuration/basic/#virtual-hosting)):

- [allow_multiple_connections](/admin/configuration/toplevel/#allow-multiple-connections)
- [anonymous_protocol](/admin/configuration/toplevel/#anonymous-protocol)

Examples:

-   To enable anonymous login on all virtual hosts:


		auth_method: [anonymous]
		anonymous_protocol: login_anon

-   Similar as previous example, but limited to `public.example.org`:


		host_config:
		  public.example.org:
		    auth_method: [anonymous]
		    anonymous_protoco: login_anon

-   To enable anonymous login and internal authentication on a virtual
	host:


		host_config:
		  public.example.org:
		    auth_method:
		      - internal
		      - anonymous
		    anonymous_protocol: login_anon

-   To enable SASL Anonymous on a virtual host:


		host_config:
		  public.example.org:
		    auth_method: [anonymous]
		    anonymous_protocol: sasl_anon

-   To enable SASL Anonymous and anonymous login on a virtual host:


		host_config:
		  public.example.org:
		    auth_method: [anonymous]
		    anonymous_protocol: both

-   To enable SASL Anonymous, anonymous login, and internal
	authentication on a virtual host:


		host_config:
		  public.example.org:
		    auth_method:
		      - internal
		      - anonymous
		    anonymous_protocol: both

There are more configuration examples and XMPP client example stanzas in
[`Anonymous users support`](https://ejabberd.im/Anonymous-users-support).

# PAM Authentication

`ejabberd` supports authentication via Pluggable Authentication Modules
(PAM). PAM is currently supported in AIX, FreeBSD, HP-UX, Linux, Mac OS
X, NetBSD and Solaris. PAM authentication is disabled by default, so you
have to configure and compile `ejabberd` with PAM support enabled:


	./configure --enable-pam && make install

Options:

- [pam_service](/admin/configuration/toplevel/#pam-service)
- [pam_userinfotype](/admin/configuration/toplevel/#pam-userinfotype)

Example:

	auth_method: [pam]
	pam_service: ejabberd

Though it is quite easy to set up PAM support in `ejabberd`, PAM itself
introduces some security issues:

-   To perform PAM authentication `ejabberd` uses external C-program
	called `epam`. By default, it is located in
	`/var/lib/ejabberd/priv/bin/` directory. You have to set it root on
	execution in the case when your PAM module requires root privileges
	(`pam_unix.so` for example). Also you have to grant access for
	`ejabberd` to this file and remove all other permissions from it.
	Execute with root privileges:


		chown root:ejabberd /var/lib/ejabberd/priv/bin/epam
		chmod 4750 /var/lib/ejabberd/priv/bin/epam

-   Make sure you have the latest version of PAM installed on your
	system. Some old versions of PAM modules cause memory leaks. If you
	are not able to use the latest version, you can `kill(1)` `epam`
	process periodically to reduce its memory consumption: `ejabberd`
	will restart this process immediately.

-   `epam` program tries to turn off delays on authentication failures.
	However, some PAM modules ignore this behavior and rely on their own
	configuration options. You can create a configuration file
	`ejabberd.pam`. This example shows how to turn off delays in
	`pam_unix.so` module:

		#%PAM-1.0
		auth        sufficient  pam_unix.so likeauth nullok nodelay
		account     sufficient  pam_unix.so

	That is not a ready to use configuration file: you must use it as a
	hint when building your own PAM configuration instead. Note that if
	you want to disable delays on authentication failures in the PAM
	configuration file, you have to restrict access to this file, so a
	malicious user can’t use your configuration to perform brute-force
	attacks.

-   You may want to allow login access only for certain users.
	`pam_listfile.so` module provides such functionality.

-   If you use `pam_winbind` to authorise against a Windows Active
	Directory, then `/etc/nsswitch.conf` must be configured to use
	`winbind` as well.

# JWT Authentication

`ejabberd` supports authentication using JSON Web Token (JWT).  When enabled,
clients send signed tokens instead of passwords, which are checked using a
private key specified in the [jwt_key](/admin/configuration/toplevel/#jwt-key) option.
JWT payload must look like this:

    {
      "jid": "test@example.org",
      "exp": 1564436511
    }

Options:

- [jwt_key](/admin/configuration/toplevel/#jwt-key)
- [jwt_auth_only_rule](/admin/configuration/toplevel/#jwt-auth-only-rule)
- [jwt_jid_field](/admin/configuration/toplevel/#jwt-jid-field)

Example:

	auth_method: jwt
	jwt_key: /path/to/jwt/key

In this example, admins can use both JWT and plain passwords, while the rest of users can use only JWT.

	# the order is important here, don't use [sql, jwt]
	auth_method: [jwt, sql]

	access_rules:
	  ...
	  jwt_only:
	    deny: admin
	    allow: all

	jwt_auth_only_rule: jwt_only

Please notice that, when using JWT authentication,
[mod_offline](/admin/configuration/modules/#mod-offline) will not work.
With JWT authentication the accounts do not exist in the database,
and there is no way to know if a given account exists or not.

For more information about JWT authentication, you can check a brief tutorial in the
[ejabberd 19.08 release notes](https://www.process-one.net/blog/ejabberd-19-08/).

# SCRAM

The top-level option
[auth_password_format](/admin/configuration/toplevel/#auth-password-format)
allows to store the passwords in SCRAM format instead of plaintext format.

For details about the client-server communication when using SCRAM-SHA-1,
refer to [SASL and SCRAM-SHA-1](https://wiki.xmpp.org/web/SASLandSCRAM-SHA-1).

## Internal storage

When ejabberd starts with internal auth method and SCRAM password format configured:

    auth_method: internal
    auth_password_format: scram

and detects that there are plaintext passwords stored,
they are automatically converted to SCRAM format:

    [info] Passwords in Mnesia table 'passwd' will be SCRAM'ed
    [info] Transforming table 'passwd', this may take a while

## SQL Database

Please note that if you use SQL auth method and SCRAM password format,
the plaintext passwords already stored in the database are not automatically
converted to SCRAM format.

To convert plaintext passwords to SCRAM format in your database,
use the [convert_to_scram](/developer/ejabberd-api/admin-api/#convert-to-scram) command:


	ejabberdctl convert_to_scram example.org

## Foreign authentication

*Note on SCRAM using and foreign authentication limitations*:
when using
the SCRAM password format, it is not possible to use foreign
authentication method in ejabberd, as the real password is not known.

Foreign authentication are use to authenticate through various bridges
ejabberd provide. Foreign authentication includes at the moment SIP
and TURN auth support and they will not be working with SCRAM.

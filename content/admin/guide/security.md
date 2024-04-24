# Securing ejabberd

## Firewall Settings

You need to take the following ports in mind when configuring your firewall.
The ports may change depending on your ejabberd configuration.
Most of them are TCP ports, except the explicitely mentioned ones:

| **Port**              | **Description**                                                                             |
|:----------------------|:--------------------------------------------------------------------------------------------|
| 5222                  | Jabber/XMPP client connections, plain or STARTTLS                                           |
| 5223                  | Jabber client connections using the old SSL method                                          |
| 5269                  | Jabber/XMPP incoming server connections                                                     |
| 5280/5443             | HTTP/HTTPS for Web Admin and many more ([ejabberd_http](../configuration/listen.md#ejabberd_http)) |
| 1883/8883             | MQTT/MQTTS service ([mod_mqtt](../configuration/listen.md#mod_mqtt))                        |
| 3478/5349             | STUN+TURN/STUNS+TURNS service ([ejabberd_stun](../configuration/listen.md#ejabberd_stun))   |
| 3478 UDP              | ' '                                                                                         |
| 49152-65535 range UDP | STUN+TURN service ([ejabberd_stun](../configuration/listen.md#ejabberd_stun)), configure with `turn_min_port` and `turn_max_port` |
| 5060/5061             | SIP service ([ejabberd_sip](../configuration/listen.md#ejabberd_sip))                       |
| 7777                  | SOCKS5 file transfer proxy ([mod_proxy65](../configuration/modules.md#mod_proxy65))         |
| 4369                  | EPMD (see [epmd](#epmd)) listens for Erlang node name requests                              |
| random port range     | Used by [epmd](#epmd) for connections between Erlang nodes, configure with `inet_dist_listen_min` and `inet_dist_listen_max` |
| 5210                  | Erlang connectivity when `ERL_DIST_PORT` is set, alternative to EPMD                        |

## epmd

[`epmd (Erlang Port Mapper Daemon)`](https://erlang.org/doc/man/epmd.html)
is a small name server included in Erlang/OTP and used by Erlang
programs when establishing distributed Erlang communications. `ejabberd`
needs `epmd` to use `ejabberdctl` and also when clustering `ejabberd`
nodes. This small program is automatically started by Erlang, and is
never stopped. If `ejabberd` is stopped, and there aren't any other
Erlang programs running in the system, you can safely stop `epmd` if you
want.

`ejabberd` runs inside an Erlang node. To communicate with `ejabberd`,
the script `ejabberdctl` starts a new Erlang node and connects to the
Erlang node that holds `ejabberd`. In order for this communication to
work, `epmd` must be running and listening for name requests in the port
4369. You should block the port 4369 in the firewall in such a way that
only the programs in your machine can access it, or configure the option
`ERL_EPMD_ADDRESS` in the file `ejabberdctl.cfg`.

If you build a cluster of several `ejabberd` instances, each `ejabberd`
instance is called an `ejabberd` node. Those `ejabberd` nodes use a
special Erlang communication method to build the cluster, and EPMD is
again needed listening in the port 4369. So, if you plan to build a
cluster of `ejabberd` nodes you must open the port 4369 for the machines
involved in the cluster. Remember to block the port so Internet doesn't
have access to it.

Once an Erlang node solved the node name of another Erlang node using
EPMD and port 4369, the nodes communicate directly. The ports used in
this case by default are random, but can be configured in the file
`ejabberdctl.cfg`. The Erlang command-line parameter used internally is,
for example:

``` sh
erl ... -kernel inet_dist_listen_min 4370 inet_dist_listen_max 4375
```

It is also possible to configure in `ejabberdctl.cfg` the network
interface where the Erlang node will listen and accept connections. The
Erlang command-line parameter used internally is, for example:

``` sh
erl ... -kernel inet_dist_use_interface "{127,0,0,1}"
```

## Erlang Cookie

The Erlang cookie is a string with numbers and letters. An Erlang node
reads the cookie at startup from the command-line parameter
`-setcookie`. If not indicated, the cookie is read from the file
`$HOME/.erlang.cookie`.

If this file does not exist, it is created
immediately with a random cookie in the user `$HOME` path.
This means the user running ejabberd must have a `$HOME`,
and have write access to that path.
So, when you create a new account in your system for running ejabberd,
either allow it to have a `$HOME`,
or set as `$HOME` a path where ejabberd will have write access.
Depending on your setup, examples could be:

``` sh
adduser --home /usr/local/var/lib/ejabberd ejabberd
```

or

``` sh
adduser --home /var/lib/ejabberd ejabberd
```

Two Erlang nodes communicate only if
they have the same cookie. Setting a cookie on the Erlang node allows
you to structure your Erlang network and define which nodes are allowed
to connect to which.

Thanks to Erlang cookies, you can prevent access to the Erlang node by
mistake, for example when there are several Erlang nodes running
different programs in the same machine.

Setting a secret cookie is a simple method to difficult unauthorized
access to your Erlang node. However, the cookie system is not ultimately
effective to prevent unauthorized access or intrusion to an Erlang node.
The communication between Erlang nodes are not encrypted, so the cookie
could be read sniffing the traffic on the network. The recommended way
to secure the Erlang node is to block the port 4369.

## Erlang Node Name

An Erlang node may have a node name. The name can be short (if indicated
with the command-line parameter `-sname`) or long (if indicated with the
parameter `-name`). Starting an Erlang node with -sname limits the
communication between Erlang nodes to the LAN.

Using the option `-sname` instead of `-name` is a simple method to
difficult unauthorized access to your Erlang node. However, it is not
ultimately effective to prevent access to the Erlang node, because it
may be possible to fake the fact that you are on another network using a
modified version of Erlang `epmd`. The recommended way to secure the
Erlang node is to block the port 4369.

## Securing Sensitive Files

`ejabberd` stores sensitive data in the file system either in plain text
or binary files. The file system permissions should be set to only allow
the proper user to read, write and execute those files and directories.

**ejabberd configuration file: `/etc/ejabberd/ejabberd.yml`**:   Contains the JID of administrators and passwords of external
 components. The backup files probably contain also this information,
 so it is preferable to secure the whole `/etc/ejabberd/` directory.

**ejabberd service log: `/var/log/ejabberd/ejabberd.log`**:   Contains IP addresses of clients. If the loglevel is set to 5, it
 contains whole conversations and passwords. If a logrotate system is
 used, there may be several log files with similar information, so it
 is preferable to secure the whole `/var/log/ejabberd/` directory.

**Mnesia database spool files in `/var/lib/ejabberd/`**:   The files store binary data, but some parts are still readable. The
 files are generated by Mnesia and their permissions cannot be set
 directly, so it is preferable to secure the whole
 `/var/lib/ejabberd/` directory.

**Erlang cookie file: `/var/lib/ejabberd/.erlang.cookie`**:   See section [Erlang Cookie](#erlang-cookie).

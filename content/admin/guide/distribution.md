# Erlang Distribution

## Overview

ejabberd uses the
[Distributed Erlang](https://www.erlang.org/doc/system/distributed.html)
feature for two purposes:

- [`ejabberdctl`](managing.md#ejabberdctl) shell script
  that connects to your running ejabberd node to allow you executing
  [API](../../developer/ejabberd-api/index.md) commands in it,
  opening an interactive erlang shell, ...

- [clustering](clustering.md) several ejabberd nodes that
  you deploy in separate machines to serve a large XMPP domain
  with fault-tolerance and scalability.

There are three topics involved in erlang distribution:

- [Cookie](#cookie), automatically generated, shared by your ejabberd nodes, and secret to the world
- [Node Name](#node-name) that you pick for each ejabberd node to identify them in a cluster
- **Port** number to use, determined using either:
    - [ERL_DIST_PORT](#erl_dist_port) environment variable that lets you assign your desired port number
    - [epmd](#epmd) program that assigns a random port number and maps names with ports

## Cookie

def:cookie
: Random alphanumeric string assigned to each erlang [](def:node) used to secure connections between erlang nodes.
  See [Security in distributed erlang](https://www.erlang.org/doc/system/distributed#security).

An Erlang node
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
The communication between Erlang nodes are not encrypted, so the [](def:cookie)
could be read sniffing the traffic on the network. The recommended way
to secure the Erlang node is to block the port 4369.

## Node Name

def:erlang node
: A node in erlang connected to a cluster.
  See [Erlang node name](distribution.md#node-name)

| Variable in ejabberdctl.cfg | Corresponding argument in Erlang/OTP |
|---------------------------|--------------------------------------|
| `ERLANG_NODE=ejabberd@localhost` | -[name](https://www.erlang.org/doc/apps/erts/erl_cmd#name) `ejabberd@localhost` <br>-[sname](https://www.erlang.org/doc/apps/erts/erl_cmd#sname) `ejabberd` |

An Erlang node may have a node name. The name can be short (if indicated
with the command-line parameter `-sname`) or long (if indicated with the
parameter `-name`). Starting an Erlang node with `-sname` limits the
communication between Erlang nodes to the LAN.

Using the argument `-sname` instead of `-name` is a simple method to
difficult unauthorized access to your Erlang node. However, it is not
ultimately effective to prevent access to the Erlang node, because it
may be possible to fake the fact that you are on another network using a
modified version of Erlang [](def:epmd). The recommended way to secure the
Erlang node is to block the port 4369.

## `ERL_DIST_PORT`

| Variable in ejabberdctl.cfg | Corresponding argument in Erlang/OTP |
|---------------------------|--------------------------------------|
| `INET_DIST_INTERFACE`     | -kernel [inet_dist_use_interface](https://www.erlang.org/doc/apps/kernel/kernel_app.html#inet_dist_use_interface) |
| `ERL_DIST_PORT=5210`         | -[erl_epmd_port](https://www.erlang.org/docs/28/apps/erts/erl_cmd#erl_epmd_port) `5210` -[start_epmd](https://www.erlang.org/docs/28/apps/erts/erl_cmd.html#start_epmd) `false` |

The TCP port where your Erlang node listens for Erlang distribution connections
can be set with the environment variable `ERL_DIST_PORT` in the file `ejabberdctl.cfg`.
When attempting to connect to other remote nodes,
it will also try to connect to that port number.

By default this variable is set to `5210`,
but you can set any number you prefer as long as it isn't used in the machine,
and it is the same for all the other ejabberd nodes that will build the cluster.

When building a cluster of several Erlang nodes,
all of them must have the same `ERL_DIST_PORT`,
because this is used for listening and also for connecting.
Consequently, if you want to build a cluster of several nodes in the same machine,
each node must have a different IP address in `INET_DIST_INTERFACE`.

Just as a small note: since Erlang/OTP 27.2, the argument `-erl_epmd_port` is obsoleted by
[-kernel erl_epmd_node_listen_port](https://www.erlang.org/docs/28/apps/kernel/kernel_app#erl_epmd_node_listen_port).
However, ejabberd does not yet use the new argument because ejabberd supports Erlang/OTP 25+.


## epmd

| Variable in ejabberdctl.cfg | Corresponding argument in Erlang/OTP |
|---------------------------|--------------------------------------|
| `INET_DIST_INTERFACE`     | -kernel [inet_dist_use_interface](https://www.erlang.org/doc/apps/kernel/kernel_app.html#inet_dist_use_interface) |
| `FIREWALL_WINDOW=XX-YY`   | -kernel [inet_dist_listen_min](https://www.erlang.org/doc/apps/kernel/kernel_app.html#inet_dist_listen) `XX` inet_dist_listen_max `YY` |
| `ERL_EPMD_ADDRESS`        | [ERL_EPMD_ADDRESS](https://www.erlang.org/doc/apps/erts/epmd_cmd.html#environment-variables) |

def:epmd
: Small name server included in Erlang/OTP and used by Erlang
  programs when establishing distributed Erlang communications.
  See [`epmd (Erlang Port Mapper Daemon)`](https://www.erlang.org/doc/apps/erts/epmd_cmd.html)

When [ERL_DIST_PORT](#erl_dist_port) is not set in `ejabberdctl.cfg`,
Erlang starts the `epmd` program (which listens in port `4369`),
and connects to that program to ask for a random port number to use.

You should block the port `4369` in the firewall in such a way that
only the programs in your machine can access it,
or configure the environment variable `ERL_EPMD_ADDRESS`
in the file `ejabberdctl.cfg`.

When building a cluster of several ejabberd nodes,
if you don't set [ERL_DIST_PORT](#erl_dist_port)
with the same port number in `ejabberdctl.cfg` in all the nodes,
then you must open the port `4369` for all the machines
involved in the cluster so their epmd programs can be accessed.
Remember to block the port so Internet doesn't have access to it.

Once an Erlang node solved the node name of another Erlang node using
EPMD and port `4369`, the nodes communicate directly. The ports used in
this case by default are random,
and can be restricted with the environment variable `FIREWALL_WINDOW`
in the file `ejabberdctl.cfg`.

The network interface where the Erlang node will listen and accept connections
can be configured with the environment variable `INET_DIST_INTERFACE`
in the file `ejabberdctl.cfg`.

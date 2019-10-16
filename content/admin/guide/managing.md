---
title: Managing an ejabberd server
toc: true
menu: Managing
order: 60
---

# ejabberdctl

With the `ejabberdctl` command line administration script you can
execute `ejabberdctl commands` (described in the next section,
[ejabberdctl Commands](#ejabberdctl-commands)) and also many general
`ejabberd commands` (described in section
[ejabberd Commands](#ejabberd-commands)).
This means you can start, stop and perform many
other administrative tasks in a local or remote `ejabberd` server (by
providing the argument `–node NODENAME`).

The `ejabberdctl` script can be configured in the file
`ejabberdctl.cfg`. This file includes detailed information about each
configurable option. See section [Erlang Runtime System](#erlang-runtime-system).

The `ejabberdctl` script returns a numerical status code. Success is
represented by `0`, error is represented by `1`, and other codes may be
used for specific results. This can be used by other scripts to
determine automatically if a command succeeded or failed, for example
using: `echo $?`

To restrict what commands can be executed;
see [API Permissions](/developer/ejabberd-api/permissions/).

If you use Bash, you can get Bash completion by copying the file
`tools/ejabberdctl.bc` to the directory `/etc/bash_completion.d/` (in
Debian, Ubuntu, Fedora and maybe others).

## ejabberdctl Commands

When `ejabberdctl` is executed without any parameter, it displays the
available options. If there isn't an `ejabberd` server running, the
available parameters are:

**`start`**:   Start `ejabberd` in background mode. This is the default method.

**`debug`**:   Attach an Erlang shell to an already existing `ejabberd` server.
	This allows to execute commands interactively in the `ejabberd`
	server.

**`live`**:   Start `ejabberd` in live mode: the shell keeps attached to the
	started server, showing log messages and allowing to execute
	interactive commands.

If there is an `ejabberd` server running in the system, `ejabberdctl`
shows the `ejabberdctl commands` described bellow and all the
`ejabberd commands` available in that server (see
[List of ejabberd Commands](#list-of-ejabberd-commands)).

The `ejabberdctl commands` are:

**`help`**:   Get help about ejabberdctl or any available command. Try
	`ejabberdctl help help`.

**`status`**:   Check the status of the `ejabberd` server.

**`stop`**:   Stop the `ejabberd` server.

**`restart`**:   Restart the `ejabberd` server.

**`mnesia`**:   Get information about the Mnesia database.


# Erlang Runtime System

`ejabberd` is an Erlang/OTP application that runs inside an Erlang
runtime system. This system is configured using environment variables
and command line parameters. The `ejabberdctl` administration script
uses many of those possibilities. You can configure some of them with
the file `ejabberdctl.cfg`, which includes detailed description about
them. This section describes for reference purposes all the environment
variables and command line parameters.

The environment variables:

**`EJABBERD_CONFIG_PATH`**:   Path to the ejabberd configuration file.

**`EJABBERD_MSGS_PATH`**:   Path to the directory with translated strings.

**`EJABBERD_LOG_PATH`**:   Path to the ejabberd service log file.

**`EJABBERD_SO_PATH`**:   Path to the directory with binary system libraries.

**`EJABBERD_DOC_PATH`**:   Path to the directory with ejabberd documentation.

**`EJABBERD_PID_PATH`**:   Path to the PID file that ejabberd can create when started.

**`HOME`**:   Path to the directory that is considered `ejabberd`’s home. This
	path is used to read the file `.erlang.cookie`.

**`ERL_CRASH_DUMP`**:   Path to the file where crash reports will be dumped.

**`ERL_EPMD_ADDRESS`**:   IP address where epmd listens for connections (see [epmd](../security/#epmd)).

**`ERL_INETRC`**:   Indicates which IP name resolution to use. If using `-sname`,
	specify either this option or `-kernel inetrc filepath`.

**`ERL_MAX_PORTS`**:   Maximum number of simultaneously open Erlang ports.

**`ERL_MAX_ETS_TABLES`**:   Maximum number of ETS and Mnesia tables.

The command line parameters:

**`-sname ejabberd`**:   The Erlang node will be identified using only the first part of the
	host name, i.e. other Erlang nodes outside this domain cannot
	contact this node. This is the preferable option in most cases.

**`-name ejabberd`**:   The Erlang node will be fully identified. This is only useful if you
	plan to setup an `ejabberd` cluster with nodes in different
	networks.

**`-kernel inetrc ’/etc/ejabberd/inetrc’`**:   Indicates which IP name resolution to use. If using `-sname`,
	specify either this option or `ERL_INETRC`.

**`-kernel inet_dist_listen_min 4200 inet_dist_listen_min 4210`**:   Define the first and last ports that `epmd` can listen to
	(see [epmd](../security/#epmd)).

**`-kernel inet_dist_use_interface { 127,0,0,1 }`**:   Define the IP address where this Erlang node listens for other nodes
	connections (see [epmd](../security/#epmd)).

**`-detached`**:   Starts the Erlang system detached from the system console. Useful
	for running daemons and background processes.

**`-noinput`**:   Ensures that the Erlang system never tries to read any input. Useful
	for running daemons and background processes.

**`-pa /var/lib/ejabberd/ebin`**:   Specify the directory where Erlang binary files (\*.beam) are
	located.

**`-s ejabberd`**:   Tell Erlang runtime system to start the `ejabberd` application.

**`-mnesia dir ’/var/lib/ejabberd/’`**:   Specify the Mnesia database directory.

**`-sasl sasl_error_logger {file, /var/log/ejabberd/erlang.log}`**:   Path to the Erlang/OTP system log file. SASL here means “System
	Architecture Support Libraries” not “Simple Authentication and
	Security Layer”.

**`+K [true|false]`**:   Kernel polling.

**`-smp [auto|enable|disable]`**:   SMP support.

**`+P 250000`**:   Maximum number of Erlang processes.

**`-remsh ejabberd@localhost`**:   Open an Erlang shell in a remote Erlang node.

**`-hidden`**:   The connections to other nodes are hidden (not published). The
	result is that this node is not considered part of the cluster. This
	is important when starting a temporary `ctl` or `debug` node.

Note that some characters need to be escaped when used in shell scripts,
for instance `"` and `{}`. You can find other options in the Erlang
manual page (`erl -man erl`).

# ejabberd Commands

An `ejabberd command` is an abstract function identified by a name, with
a defined number and type of calling arguments and type of result that
is registered in the `ejabberd_commands` service. Those commands can be
defined in any Erlang module and executed using any valid frontend.

`ejabberd` includes three frontends to execute `ejabberd commands`:
the script `ejabberdctl` (see [ejabberdctl](#ejabberdctl)),
the `ejabberd_xmlrpc` listener (see [Listening Ports](../configuration/#listening-ports))
and `mod_http_api` (see [mod_http_api](../configuration/#mod-http-api)).
Other known frontends that can be installed to
execute ejabberd commands in different ways are: `mod_rest` (HTTP POST
service), `mod_shcommands` (ejabberd WebAdmin page).

You probably want to restrict what commands can be executed, how and who;
for that matter see [API Permissions](/developer/ejabberd-api/permissions/).

## List of ejabberd Commands

`ejabberd` includes a few ejabberd Commands by default as listed below.
When more modules are installed, new commands may be available in the
frontends, see
[Administration API Commands](/developer/ejabberd-api/admin-api/)
for a more detailed list of commands.

The easiest way to get a list of the available commands, and get help
for them is to use the ejabberdctl script:

``` bash
$ ejabberdctl help
Usage: ejabberdctl [--node nodename] [--auth user host password] command [options]

Available commands in this ejabberd node:
  backup file                  Store the database to backup file
  connected_users              List all established sessions
  connected_users_number       Get the number of established sessions
  ...
```

The commands included in ejabberd by default are:

**`stop_kindly delay announcement`**:   Inform users and rooms, wait, and stop the server. Provide the delay
	in seconds, and the announcement quoted.

**`registered_vhosts`**:   List all registered vhosts in SERVER

**`reopen_log`**:   Reopen the log files after they were renamed. This can be useful when an
	external tool is used for log rotation. See section
	[Log Files](../troubleshooting/#log-files).

**`rotate_log`**:   Rotate the log files.

**`convert_to_yaml /etc/ejabberd/ejabberd.cfg /etc/ejabberd/ejabberd-converted.yml`**:   Convert an old ejabberd.cfg file to the YAML syntax in a new file.

**`backup ejabberd.backup`**:   Store internal Mnesia database to a binary backup file.

**`restore ejabberd.backup`**:   Restore immediately from a binary backup file the internal Mnesia
	database. This will consume a lot of memory if you have a large
	database, so better use `install_fallback`.

**`install_fallback ejabberd.backup`**:   The binary backup file is installed as fallback: it will be used to
	restore the database at the next ejabberd start. This means that,
	after running this command, you have to restart ejabberd. This
	command requires less memory than `restore`.

**`dump ejabberd.dump`**:   Dump internal Mnesia database to a text file dump.

**`load ejabberd.dump`**:   Restore immediately from a text file dump. This is not recommended
	for big databases, as it will consume much time, memory and
	processor. In that case it’s preferable to use `backup` and
	`install_fallback`.

**`import_piefxis, export_piefxis, export_piefxis_host`**:   These options can be used to migrate accounts using
	[`XEP-0227`](http://xmpp.org/extensions/xep-0227.html) formatted XML
	files from/to other Jabber/XMPP servers or move users of a vhost to
	another ejabberd installation.

**`import_file, import_dir`**:   These options can be used to migrate accounts using jabberd1.4
	formatted XML files. from other Jabber/XMPP servers There exist
	tutorials to
	[`migrate from other software to ejabberd`](http://www.ejabberd.im/migrate-to-ejabberd).

**`import_prosody import_dir`**:  Import data from Prosody server. `import_dir` is typically `/var/lib/prosody/`.
   Currently the following data is imported: vcards, accounts, rosters, private data
   (e.g. conference bookmarks), conferences, offline messages and privacy lists.
   Note: ejabberd must be compiled with optional tools support and package must
   provide optional luerl dependency.

**`set_master nodename`**:   Set master node of the clustered Mnesia tables. If you provide as
	nodename “self”, this node will be set as its own master.

**`mnesia_change_nodename oldnodename newnodename oldbackup newbackup`**:   Change the erlang node name in a backup file

**`export2sql virtualhost filepath`**:   Export virtual host information from Mnesia tables to a SQL file.

**`update_list`**:   List modified modules that can be updated

**`update module`**:   Update the given module, or use the keyword: all

**`reload_config`**:   Reload ejabberd configuration file into memory

**`delete_expired_messages`**:   This option can be used to delete old messages in offline storage.
	This might be useful when the number of offline messages is very
	high.

**`delete_old_messages days`**:   Delete offline messages older than the given days.

**`incoming_s2s_number`**:   Number of incoming s2s connections on the node

**`outgoing_s2s_number`**:   Number of outgoing s2s connections on the node

**`register user host password`**:   Register an account in that domain with the given password.

**`unregister user host`**:   Unregister the given account.

**`registered_users host`**:   List all registered users in HOST

**`connected_users`**:   List all established sessions

**`connected_users_number`**:   Get the number of established sessions

**`user_resources user host`**:   List user’s connected resources

**`kick_user user host`**:   Disconnect user’s active sessions

# Web Admin

The `ejabberd` Web Admin allows to administer most of `ejabberd` using a
web browser.

This feature is enabled by default: a `ejabberd_http` listener with the
option `web_admin` (see [Listening Ports](../configuration/#listening-ports))
is included in the listening
ports. Then you can open `http://server:port/admin/` in your favourite
web browser. You will be asked to enter the username (the *full* Jabber
ID) and password of an `ejabberd` user with administrator rights. After
authentication you will see a page similar to the following screen.

![Web Admin](/static/images/admin/webadmin.png)

Here you can edit access restrictions, manage users, create backups,
manage the database, enable/disable ports listened for, view server
statistics,…

The access rule `configure` determines what accounts can access the Web
Admin and modify it. The access rule `webadmin_view` is to grant only
view access: those accounts can browse the Web Admin with read-only
access.

Example configurations:

- You can serve the Web Admin on the same port as the HTTP Polling
  interface. In this example you should point your web browser to
  `http://example.org:5280/admin/` to administer all virtual hosts or
  to `http://example.org:5280/admin/server/example.com/` to administer
  only the virtual host `example.com`. Before you get access to the
  Web Admin you need to enter as username, the JID and password from a
  registered user that is allowed to configure `ejabberd`. In this
  example you can enter as username ‘`admin@example.net`’ to
  administer all virtual hosts (first URL). If you log in with
  ‘`admin@example.com`’ on
  `http://example.org:5280/admin/server/example.com/` you can only
  administer the virtual host `example.com`. The account
  ‘`reviewer@example.com`’ can browse that vhost in read-only mode.

  ``` yaml
  acl:
    admin:
      user:
        - "admin": "example.net"
   
  host_config:
    "example.com":
      acl:
        admin:
  	user:
  	  - "admin": "example.com"
        viewers:
  	user:
  	  - "reviewer": "example.com"
  
  access:
    configure:
      admin: allow
    webadmin_view:
      viewers: allow
  
  hosts:
    - "example.org"
  
  listen:
    ...
    -
      port: 5280
      module: ejabberd_http
      web_admin: true
    ...
  ```

-   For security reasons, you can serve the Web Admin on a secured
	connection, on a port differing from the HTTP Polling interface, and
	bind it to the internal LAN IP. The Web Admin will be accessible by
	pointing your web browser to `https://192.168.1.1:5282/admin/`:

  ``` yaml
  hosts:
    - "example.org"
  listen:
    ...
    - 
      port: 5280
      module: ejabberd_http
    - 
      ip: "192.168.1.1"
      port: 5282
      module: ejabberd_http
      certfile: "/usr/local/etc/server.pem"
      tls: true
      web_admin: true
    ...
  ```

Certain pages in the ejabberd Web Admin contain a link to a related
section in the ejabberd Installation and Operation Guide. In order to
view such links, a copy in HTML format of the Guide must be installed in
the system. The file is searched by default in
`/share/doc/ejabberd/guide.html`. The directory of the documentation can
be specified in the environment variable `EJABBERD_DOC_PATH`. See
section [Erlang Runtime System](#erlang-runtime-system).

# Ad-hoc Commands

If you enable `mod_configure` and `mod_adhoc`, you can perform several
administrative tasks in `ejabberd` with an XMPP client. The client must
support Ad-Hoc Commands
([`XEP-0050`][1]), and you must
login in the XMPP server with an account with proper privileges.

# Change Computer Hostname

`ejabberd` uses the distributed Mnesia database. Being distributed,
Mnesia enforces consistency of its file, so it stores the name of the
Erlang node in it (see section [Erlang Node Name](../security/#erlang-node-name)). The name of an Erlang node
includes the hostname of the computer. So, the name of the Erlang node
changes if you change the name of the machine in which `ejabberd` runs,
or when you move `ejabberd` to a different machine.

You have two ways to use the old Mnesia database in an ejabberd with new
node name: put the old node name in `ejabberdctl.cfg`, or convert the
database to the new node name.

Those example steps will backup, convert and load the Mnesia database.
You need to have either the old Mnesia spool dir or a backup of Mnesia.
If you already have a backup file of the old database, you can go
directly to step 5. You also need to know the old node name and the new
node name. If you don’t know them, look for them by executing
`ejabberdctl` or in the ejabberd log files.

Before starting, setup some variables:

``` bash
OLDNODE=ejabberd@oldmachine
NEWNODE=ejabberd@newmachine
OLDFILE=/tmp/old.backup
NEWFILE=/tmp/new.backup
```

1. Start ejabberd enforcing the old node name:

    ``` bash
    ejabberdctl --node $OLDNODE start
    ```

2. Generate a backup file:

    ``` bash
    ejabberdctl --node $OLDNODE backup $OLDFILE
    ```

3.  Stop the old node:

    ``` bash
    ejabberdctl --node $OLDNODE stop
    ```

4.  Make sure there aren't files in the Mnesia spool dir. For example:

    ``` bash
    mkdir /var/lib/ejabberd/oldfiles
    mv /var/lib/ejabberd/*.* /var/lib/ejabberd/oldfiles/
    ```

5.  Start ejabberd. There isn't any need to specify the node name
	anymore:

    ``` bash
    ejabberdctl start
    ```

6.  Convert the backup to new node name:

    ``` bash
    ejabberdctl mnesia_change_nodename $OLDNODE $NEWNODE $OLDFILE $NEWFILE
    ```

7.  Install the backup file as a fallback:

    ``` bash
    ejabberdctl install_fallback $NEWFILE
    ```

8.  Stop ejabberd:

    ``` bash
    ejabberdctl stop
    ```

    You may see an error message in the log files, it’s normal, so don’t worry:

    ``` bash
    Mnesia(ejabberd@newmachine):
    ** ERROR ** (ignoring core)
    ** FATAL ** A fallback is installed and Mnesia must be restarted.
      Forcing shutdown after mnesia_down from ejabberd@newmachine...
    ```

9.  Now you can finally start ejabberd:

    ``` bash
    ejabberdctl start
    ```

10. Check that the information of the old database is available:
	accounts, rosters... After you finish, remember to delete the
	temporary backup files from public directories.

[1]:	http://xmpp.org/extensions/xep-0050.html

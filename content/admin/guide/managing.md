# Managing an ejabberd server

## ejabberdctl

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

To restrict what commands can be executed; see [API Permissions](../../developer/ejabberd-api/permissions.md).

### Bash Completion

If you use Bash, you can get Bash completion for ejabberdctl commands names.

Some methods to enable that feature:

- Copy the file `tools/ejabberdctl.bc` to the directory `/etc/bash_completion.d/` (in
Debian, Ubuntu, Fedora and maybe others)

- Or add to your `$HOME/.bashrc` a line similar to:

    ``` sh
    source /path/to/ejabberd/tools/ejabberdctl.bc
    ```

When ejabberd is running in the machine, type `ejabberdctl` in a console
and press the `TAB` key.

The first time this is used,
the list of commands is extracted from ejabberd and stored in a file in `/tmp/`.
The next time, that file is reused for faster responses.

### ejabberdctl Commands

When `ejabberdctl` is executed without any parameter, it displays the
available options. If there isn't an `ejabberd` server running, the
available parameters are:

- **`start`**:   Start `ejabberd` in background mode. This is the default method.

- **`debug`**:   Attach an Erlang shell to an already existing `ejabberd` server.
 This allows to execute commands interactively in the `ejabberd`
 server.

- **`live`**:   Start `ejabberd` in live mode: the shell keeps attached to the
 started server, showing log messages and allowing to execute
 interactive commands.

If there is an `ejabberd` server running in the system, `ejabberdctl`
shows the `ejabberdctl commands` described below and all the
`ejabberd commands` available in that server (see
[List of ejabberd Commands](./managing.md#list-of-ejabberd-commands)).

The `ejabberdctl commands` are:

- **`help`**:   Get help about ejabberdctl or any available command. Try
 `ejabberdctl help help`.

- **`status`**:   Check the status of the `ejabberd` server.

- **`stop`**:   Stop the `ejabberd` server.

- **`restart`**:   Restart the `ejabberd` server.

- **`mnesia`**:   Get information about the Mnesia database.

## ejabberd Commands

Please go to the [API](../../developer/ejabberd-api/index.md) section.

## Erlang Runtime System

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

**`ERL_EPMD_ADDRESS`**:   IP address where epmd listens for connections (see [epmd](security.md#epmd)).

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
 (see [epmd](security.md#epmd)).

**`-kernel inet_dist_use_interface { 127,0,0,1 }`**:   Define the IP address where this Erlang node listens for other nodes
 connections (see [epmd](security.md#epmd)).

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

## Web Admin

The `ejabberd` Web Admin allows to administer some parts of `ejabberd` using a
web browser: accounts, Shared Roster Groups, manage the Mnesia database,
create and restore backups, view server statistics, …

### Basic Setup

1. If not done already, register an account and grant administration rights to it
   (see [Administration Account](../install/index.md/#administration_account)):

    ``` yaml
    acl:
      admin:
        user: admin1@example.org
    access_rules:
      configure:
        allow: admin
    ```

2. Make sure `ejabberd_web_admin` is available in
   [request_handlers](../configuration/listen-options.md#request_handlers)
   of a [ejabberd_http](../configuration/listen.md#ejabberd_http) listener.
   If you want to use HTTPS, enable [tls](../configuration/listen-options.md#tls).
   For example:

    ``` yaml
    listen:
       -
         port: 5443
         ip: "::"
         module: ejabberd_http
         tls: true
         request_handlers:
           /admin: ejabberd_web_admin
    ```

3. Open the Web Admin page in your favourite web browser.
The exact address depends on your configuration;
in this example the address is: `https://example.net:5443/admin/`

4. In the login window provide the **full Jabber ID: `admin1@example.org`** and password.
   If the web address hostname is the same that the account JID,
   you can provide simply the username instead of the full JID: `admin1`.

5. You're good! You can now use the Web Admin.

### Advanced Configuration

There are two [access rules](../configuration/basic.md#access_rules) supported:

- `configure` determines what accounts can access the Web Admin and make changes.
- `webadmin_view` grants only view access:
   those accounts can browse the Web Admin with read-only access.

Example configurations:

- You can serve the Web Admin on the same port as the HTTP Polling
  interface. In this example you should point your web browser to
  `http://example.org:5280/admin/` to administer all virtual hosts or
  to `http://example.org:5280/admin/server/example.com/` to administer
  only the virtual host `example.com`. Before you get access to the
  Web Admin you need to enter as username, the JID and password from a
  registered user that is allowed to configure `ejabberd`. In this
  example you can enter as username `admin@example.net` to
  administer all virtual hosts (first URL). If you log in with
  `admin@example.com` on
  `http://example.org:5280/admin/server/example.com/` you can only
  administer the virtual host `example.com`. The account
  `reviewer@example.com` can browse that vhost in read-only mode.

    ``` yaml
    acl:
      admin:
        user:
          - admin: example.net

    host_config:
      example.com:
        acl:
          admin:
            user:
              - admin: example.com
          viewers:
            user:
              - reviewer: example.com

    access:
      configure:
        admin: allow
      webadmin_view:
        viewers: allow

    hosts:
      - example.org

    listen:
      -
        port: 5280
        module: ejabberd_http
        request_handlers:
          /admin: ejabberd_web_admin
    ```

- For security reasons, you can serve the Web Admin on a secured connection, on a port differing from the HTTP Polling interface, and bind it to the internal LAN IP. The Web Admin will be accessible by pointing your web browser to `https://192.168.1.1:5282/admin/`:

    ``` yaml
    hosts:
      - example.org
    listen:
      -
        port: 5280
        module: ejabberd_http
      -
        ip: "192.168.1.1"
        port: 5282
        module: ejabberd_http
        certfile: "/usr/local/etc/server.pem"
        tls: true
        request_handlers:
          /admin: ejabberd_web_admin
    ```

Certain pages in the ejabberd Web Admin contain a link to a related
section in the ejabberd Installation and Operation Guide. In order to
view such links, a copy in HTML format of the Guide must be installed in
the system. The file is searched by default in
`/share/doc/ejabberd/guide.html`. The directory of the documentation can
be specified in the environment variable `EJABBERD_DOC_PATH`. See
section [Erlang Runtime System](#erlang-runtime-system).

## Ad-hoc Commands

If you enable [mod_configure](../configuration/modules.md#mod_configure) and [mod_adhoc](../configuration/modules.md#mod_adhoc), you can perform several administrative tasks in `ejabberd` with an XMPP client. The client must support Ad-Hoc Commands ([`XEP-0050`](https://xmpp.org/extensions/xep-0050.html)), and you must login in the XMPP server with an account with proper privileges.

## Change Computer Hostname

`ejabberd` uses the distributed Mnesia database. Being distributed,
Mnesia enforces consistency of its file, so it stores the name of the
Erlang node in it (see section [Erlang Node Name](security.md#erlang_node_name)). The name of an Erlang node
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

``` sh
OLDNODE=ejabberd@oldmachine
NEWNODE=ejabberd@newmachine
OLDFILE=/tmp/old.backup
NEWFILE=/tmp/new.backup
```

1. Start ejabberd enforcing the old node name:

    ``` sh
    ejabberdctl --node $OLDNODE start
    ```

2. Generate a backup file:

    ``` sh
    ejabberdctl --node $OLDNODE backup $OLDFILE
    ```

3. Stop the old node:

    ``` sh
    ejabberdctl --node $OLDNODE stop
    ```

4. Make sure there aren't files in the Mnesia spool dir. For example:

    ``` sh
    mkdir /var/lib/ejabberd/oldfiles
    mv /var/lib/ejabberd/*.* /var/lib/ejabberd/oldfiles/
    ```

5. Start ejabberd. There isn't any need to specify the node name
 anymore:

    ``` sh
    ejabberdctl start
    ```

6. Convert the backup to new node name using
    [mnesia_change_nodename](../../developer/ejabberd-api/admin-api.md/#mnesia-change-nodename):

    ``` sh
    ejabberdctl mnesia_change_nodename $OLDNODE $NEWNODE $OLDFILE $NEWFILE
    ```

7. Install the backup file as a fallback using
    [install_fallback](../../developer/ejabberd-api/admin-api.md#install-fallback):

    ``` sh
    ejabberdctl install_fallback $NEWFILE
    ```

8. Stop ejabberd:

    ``` sh
    ejabberdctl stop
    ```

    You may see an error message in the log files, it’s normal, so don’t worry:

    ``` erl
    Mnesia(ejabberd@newmachine):
    ** ERROR ** (ignoring core)
    ** FATAL ** A fallback is installed and Mnesia must be restarted.
      Forcing shutdown after mnesia_down from ejabberd@newmachine...
    ```

9. Now you can finally start ejabberd:

    ``` sh
    ejabberdctl start
    ```

10. Check that the information of the old database is available:
 accounts, rosters... After you finish, remember to delete the
 temporary backup files from public directories.

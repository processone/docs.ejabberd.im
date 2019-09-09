---
title: Installing ejabberd
menu: Installation
toc: true
order: 2
---

You have several options to install ejabberd:

- [Quick Start with Binary Installers](#quick-start) – recommended when starting development on localhost
  - [Install on Windows](#install-on-windows)
  - [Install on Linux](#install-on-linux)
  - [Install on macOS](#install-on-macos)
- [Install from Source Code](#install-from-source-code) – recommended for advanced users
- [Install with Operating System specific packages](#install-with-os-specific-packages) – recommended for sysops
- [Post-install operations](#post-install-operations) – for example, register users and create admin accounts

# Quick Start

The Binary Installer will deploy and configure a full featured
ejabberd server and does not require any extra dependencies. It
includes a stripped down version of Erlang. As such, when using
ejabberd installer, you do not need to install Erlang separately.

These tutorials assume installation on `localhost` for development purposes.
In this document, when mentioning `ejabberd-YY.MM`, we assume `YY.MM` 
is the release number, for example 18.01. Also note that the installer 
scripts support many options useful for production or unattended, scripted installation.
You can read more on installer options on [unattended installation](/admin/guide/unattended/).

## Install on Windows

1. Go to [ejabberd official download page](https://www.process-one.net/en/ejabberd/downloads/) on ProcessOne website.
2. Download the "Windows 64-bits Installer".
3. Double-click the `ejabberd-YY.MM-windows-installer.exe` to start the installer.
4. On the Windows Security dialog, Allow this application to install.
5. Select the installer language you prefer, then click "Next" to go through necessary installation steps:
    - accepting the license agreement,
    - selecting the installation directory,
    - defining the XMPP domain: by default, it's set to the name of your computer on the local network. **if unsure, type `localhost` here**,
    - setting the administrator username,
    - setting the administrator password,
    - selecting if this ejabberd instance will be part of a cluster: for simple local install, just select "No",
    - start the installation,
    - when asked by the Windows Firewall prompt, you can both times click "Cancel",
6. After successful install, you should see on your Desktop two new shortcuts: "Start ejabberd" and "Stop ejabberd". **To start or stop ejabberd, righ-click on each shortcut and select "Run as Administrator", then confirm the Windows dialog by clicking "Yes".**
7. After starting ejabberd, a welcome screen should open in your default browser. You can go to the web dashboard at `http://localhost:5280/admin/` and fill the username field with the full account JID, for example `admin@domain` (or `admin@localhost` as above). Then fill the password field with that account's `password`. The next step is to get to know [how to configure ejabberd](https://docs.ejabberd.im/admin/configuration/).
8. If something goes wrong during the installation, and you would like to start from scratch, you will find the ejabberd `uninstall.exe` in the directory where it was installed. By default, that's `\Program Files\ejabberd-YY.MM\uninstall.app`. The uninstaller will stop your ejabberd server and remove all its files. Log files may be left behind, so to completely remove ejabberd, just delete its main folder.

## Install on Linux

1. Go to [ejabberd official download page](https://www.process-one.net/en/ejabberd/downloads/) on ProcessOne website.
2. Download the "Linux x86 64-bits Intel Installer".
3. **Right-click on the downloaded file and select "Properties". Click on the "Permissions" tab and tick the box that says "Allow executing file as program".**
4. Now you are able to double-click the file to execute it and start the installer.
   You can also set the installer as executable and start it using the command line:
   ``` bash
   chmod +x ejabberd-YY.MM-linux-x86_64-installer.run
   ./ejabberd-YY.MM-linux-x86_64-installer.run
   ```

5. Select the installer language you prefer, then click "Forward" to go through necessary installation steps:
    - accepting the license agreement,
    - selecting the installation directory,
    - defining the XMPP domain: by default, it's set to the name of your computer on the local network. **if unsure, type `localhost` here**,
    - setting the administrator username,
    - setting the administrator password,
    - selecting if this ejabberd instance will be part of a cluster: for simple local install, just select "No",
    - start the installation,
6. After successful installation, let's launch ejabberd using the Terminal. In the command line, go to the installation folder and execute `./bin/ejabberdctl live`. This will start ejabberd in an interactive live mode with some useful messages printed in the Terminal.
7. Now you can go to the web dashboard at `http://localhost:5280/admin/` and fill the username field with the full account JID, for example `admin@domain` (or `admin@localhost` as above). Then fill the password field with that account's `password`. The next step is to get to know [how to configure ejabberd](https://docs.ejabberd.im/admin/configuration/).
8. If something goes wrong during the installation, and you would like to start from scratch, you will find the ejabberd `uninstall` in the directory where it was installed.

## Install on macOS

### Using Binary Installer

Before you begin installing ejabberd, make sure your Mac allows apps from identified developers. To do this, **go to your Mac Preferences, Security & Privacy and select "Allow apps downloaded from: App Store and identified developers"**. Then you can download ejabberd and proceed with installation:

1. Go to [ejabberd official download page](https://www.process-one.net/en/ejabberd/downloads/) on ProcessOne website.
2. Download the "Mac OS X Intel Installer". If your system is older than the minimal requirements specified, search the [ejabberd Download Archive](https://www.process-one.net/en/ejabberd/archive/) for an appropriate version.
3. Double-click the `ejabberd-YY.MM-osx-installer.app.zip` to unpack the archive.
4. Double-click the `ejabberd-YY.MM-osx-installer.app` to start the installer.
5. Confirm the security dialog by clicking "Open".
6. If you see a dialog titled **"App is not optimized for your Mac" you can safely discard it** – it only applies to the installer, not ejabberd itself.
7. Select the installer language you prefer, then click "Next" to go through necessary installation steps:
    - accepting the license agreement,
    - selecting the installation directory,
    - defining the XMPP domain: by default, it's set to the name of your computer on the local network. **if unsure, type `localhost` here**,
    - setting the administrator username,
    - setting the administrator password,
    - selecting if this ejabberd instance will be part of a cluster: for simple local install, just select "No",
    - start the installation,
8. Once the installation script finishes, it attempts to start ejabberd. You may see a prompt asking to allow incoming connections to `beam.smp`. **Unless you allow, the installation cannot finish successfully.**
9. If something goes wrong during the installation, and you would like to start from scratch, you will find the ejabberd `uninstall.app` in the directory where it was installed. By default, that's `/Apllications/ejabberd-YY.MM/uninstall.app`. The uninstaller will stop your ejabberd server and remove all its files. Log files are left behind, so to completely remove ejabberd, just delete its main folder.


Binary Installers of ejabberd prior to version 18.01 did not have an Apple Developer signature.
If macOS complains when you try to install ejabberd older than 18.01 with binary installer with
message *"ejabberd-installer is damaged and can’t be opened"* – then you need to temporarily 
disable gatekeeper to be able to install ejabberd:

``` bash
$ sudo spctl --master-disable
<install ejabberd>
$ sudo spctl --master-enable
```

### Using Homebrew

[Homebrew](https://brew.sh) is a package manager for macOS that aims to port the many Unix & Linux software that is not easily available or compatible. Homebrew installation is simple and the instruction is available on its website.

1. Once you have Homebrew installed, open Terminal. Run `brew install ejabberd`. This should install the latest or at most the one-before-latest version of ejabberd. The installation directory should be reported at the end of this process, but usually the main executable is stored at `/usr/local/sbin/ejabberdctl`.
2. Start your ejabberd by running `/usr/local/sbin/ejabberdctl live`. This interactive mode prints useful messages in the Terminal. The default domain used by Homebrew's ejabberd is `localhost`.
3. Create an admin account by running `/usr/local/sbin/ejabberdctl register admin localhost password`. This creates an account `admin@localhost` with the specified `password`.
4. Now you can go to the web dashboard at `http://localhost:5280/admin/` and fill the username field with the full account JID, for example `admin@localhost`, then fill the password field with that account's `password`. Without configuration there's not much to see here, therefore the next step is to get to know [how to configure ejabberd](https://docs.ejabberd.im/admin/configuration/).

# Install from Source Code

The canonical form for distribution of ejabberd stable releases is
the source code package. Compiling ejabberd from source code is
quite easy in \*nix systems, as long as your system have all the
dependencies.

## Requirements

To compile ejabberd on a ‘Unix-like’ operating system, you need:

-   GNU Make
-   GCC
-   Libexpat 1.95 or higher
-   Libyaml 0.1.4 or higher
-   Erlang/OTP 19.1 or higher. We recommend using Erlang OTP 21.2.
-   OpenSSL 1.0.0 or higher, for STARTTLS, SASL and SSL encryption.
-   Zlib 1.2.3 or higher, for Stream Compression support ([`XEP-0138`][4]). Optional.
-   PAM library. Optional. For Pluggable Authentication Modules (PAM). See section [pam][8].
-   ImageMagick’s Convert program. Optional. For CAPTCHA challenges. See section [captcha][16].

## Downloading

Released versions of ejabberd are available on ProcessOne
[ejabberd official download page][1].

Alternatively, the latest development source code can be retrieved
from the Git repository using the commands:

``` bash
git clone git://github.com/processone/ejabberd.git ejabberd
cd ejabberd
./autogen.sh
```

## Compilation

To compile ejabberd execute the commands:

``` bash
./configure --enable-user=ejabberd --enable-mysql
```

This tells the configuration to prepare the installed program
to run with a user called ejabberd, so please create that user
or tell to use another local user.
It isn't recommended to run ejabberd with `root` user.

*Note*: To build ejabberd, you will need Internet access, as
 dependencies will be downloaded depending on the selected options.

The build configuration script allows several options. To get the full
list run the command:

``` bash
./configure --help
```

## Options

Some options that you may be interested in modifying:

- **`-–bindir=/`**: Specify the path to the user executables
  (where `epmd` and `iex` are available).

- **`-–prefix=/`**: Specify the path prefix where the files will be
  copied when running the `make install` command.

- **`-–enable-user[=USER]`**: Allow this normal system user to execute
  the ejabberdctl script (see section [ejabberdctl][7]), read the
  configuration files, read and write in the spool directory, read and
  write in the log directory. The account user and group must exist in
  the machine before running `make install`. This account doesn't need
  an explicit HOME directory, because `/var/lib/ejabberd/` will be
  used by default.

- **`-–enable-pam`**: Enable the PAM authentication method (see section
  [pam][8]).

- **`-–enable-tools`**: Enable the use of development tools.

- **`-–enable-mysql`**: Enable MySQL support (see section [databases][18]).

- **`-–enable-pgsql`**: Enable PostgreSQL support (see section [databases][18]).

- **`-–enable-sqlite`**: Enable SQLite support (see section [databases][18]).

- **`–-enable-riak`**: Enable Riak database support (see section [databases][18]).

- **`-–enable-redis`**: Enable Redis support to use for external session storage.

- **`-–enable-zlib`**: Enable Stream Compression (XEP-0138) using zlib.

- **`-–enable-lager`**: Use lager Erlang logging tool instead of
  standard error logger.

- **`–-enable-debug`**: Compile with `+debug_info` enabled.

- **`–-enable-elixir`**: Build ejabberd with Elixir extension support.

- **`–-enable-all`**: Enable all previous options.

- **`--enable-latest-deps`**: Makes rebar use latest versions of dependences developed
  alongside ejabberd instead of version specified in rebar.config. Should be only used
  when developing ejabberd.

Here are other available options, that are experimental and not recommended:

- **`–-enable-hipe`**: Compile natively with HiPE, not recommended.

## Installation

To install ejabberd in the destination directories, run the command `make install`.

Note that you probably need administrative privileges in the system to
install ejabberd.

The files and directories created are, by default:

<!-- TODO: Check we are up to date -->

- `/etc/ejabberd/`:  Configuration directory:

    - `ejabberd.yml`:  ejabberd configuration file
    - `ejabberdctl.cfg`: Configuration file of the administration script
    - `inetrc`: Network DNS configuration file for Erlang

- `/lib/ejabberd/`:

    - `ebin/`: Erlang binary files (\*.beam)
    - `include/`: Erlang header files (\*.hrl)
    - `priv/`: Additional files required at runtime
    - `bin/`: Executable programs
    - `lib/`: Binary system libraries (\*.so)
    - `msgs/`:   Translation files (\*.msgs)

- `/sbin/ejabberdctl`: Administration script (see section [ejabberdctl][7])

- `/share/doc/ejabberd/`: Documentation of ejabberd

- `/var/lib/ejabberd/`: Spool directory:

   - `.erlang.cookie`: Erlang cookie file (see section [cookie][19])
   - `acl.DCD, ...`: Mnesia database spool files (\*.DCD, \*.DCL, \*.DAT)

- `/var/log/ejabberd/`: Log directory (see section [logfiles]):

    - `ejabberd.log`:   ejabberd service log
    - `erlang.log`:   Erlang/OTP system log

## Specific notes

### BSD

The command to compile ejabberd in BSD systems is `gmake`.

### macOS

If compiling from sources on macOS, you must configure ejabberd to use custom OpenSSL, Yaml, iconv.
The best approach is to use [Homebrew](http://brew.sh) to install your dependencies, then
exports your custom path to let configure and make be aware of them.

``` bash
brew install git erlang elixir openssl expat libyaml libiconv libgd sqlite rebar rebar3 automake autoconf 
export LDFLAGS="-L/usr/local/opt/openssl/lib -L/usr/local/lib -L/usr/local/opt/expat/lib"
export CFLAGS="-I/usr/local/opt/openssl/include -I/usr/local/include -I/usr/local/opt/expat/include"
export CPPFLAGS="-I/usr/local/opt/openssl/include/ -I/usr/local/include -I/usr/local/opt/expat/include"
./configure
make
```

# Install with OS specific packages

<!-- TODO: Update with mention to our links and RPMs -->

Some Operating Systems provide a specific ejabberd package adapted
to the system architecture and libraries. It usually also checks
dependencies and performs basic configuration tasks like creating the
initial administrator account. Some examples are Debian and
Gentoo. Consult the resources provided by your Operating System for
more information.

ProcessOne now provides RPM and DEB all in one packages as well, since
ejabberd version 15.06. This is self-sufficient packages also
containing a minimal Erlang distribution. It ensures that it does not
interfere with your existing Erlang version. This is also a good way
to make sure ejabberd will run with the latest Erlang version. You can
download the packages from [ejabberd official download page][1].


# Starting ejabberd

ejabberd can be started manually at any time, or automatically by
the operating system at system boot time.

To start and stop ejabberd manually, use the desktop shortcuts
created by the installer. If the machine doesn't have a graphical
system, use the scripts ’start’ and ’stop’ in the ’bin’ directory
where ejabberd is installed.

You can also use the `ejabberdctl` command line administration script to
start and stop ejabberd. If you provided the configure option
`–enable-user=USER` (see [compile](#compile)), you can execute `ejabberdctl`
with either that system account or root.

Usage example:

``` bash
prompt> ejabberdctl start

prompt> ejabberdctl status
The node ejabberd@localhost is started with status: started
ejabberd is running in that node

prompt> ejabberdctl stop
```

If ejabberd doesn't start correctly and a crash dump file is
generated, there was a severe problem. You can try starting ejabberd
with the script `bin/live.bat` in Windows, or with the command
`bin/ejabberdctl live` in other Operating Systems. This way you see
the error message provided by Erlang and can identify what is exactly
the problem.

The `ejabberdctl` administration script is included in the `bin`
directory. Please refer to the section [ejabberdctl][7] for details
about `ejabberdctl`, and configurable options to fine tune the Erlang
runtime system.

## Autostart on Linux

On a \*nix system, create a system user called 'ejabberd', give it write access
to the directories `database/` and `logs/`, and set that as home. If you want
ejabberd to be started as daemon at boot time with that user, copy
`ejabberd.init` from the ’bin’ directory to something like
`/etc/init.d/ejabberd` or (on a systemd distribution) copy `ejabberd.service`
to `/etc/systemd/system/`, run as root `systemctl daemon-reload` and
`systemctl enable ejabberd.service`. Then you can call
`/etc/inid.d/ejabberd start` or (with systemd) `systemctl start ejabberd` as
root to start the server.

When ejabberd is started, the processes that are started in the system
are `beam` or `beam.smp`, and also `epmd`. For more information
regarding `epmd` consult the section relating to [epmd][6].

## Autostart on Windows

The Windows installer also adds ejabberd as a system service, and a
shortcut to a debug console for experienced administrators. If you
want ejabberd to be started automatically at boot time, go to the
Windows service settings and set ejabberd to be automatically
started. Note that the Windows service is a feature still in
development, and for example it doesn't read the file
`ejabberdctl.cfg`.

On Microsoft Windows, the Erlang processes for ejabberd are named
`erl.exe` and `epmd.exe`.


# Post-install operations

## Administration account

ejabberd binary installer prompts you for an admin account, so in that case,
you can probably skip this step.

However, if you use another way of installing ejabberd you may need to create an
admin XMPP account.

You need an XMPP account with administrative privileges to
enter the ejabberd Web Admin. Here are the steps to create it:

1.  Register an XMPP account on your ejabberd server, for example
    `admin1@example.org`. There are two ways to register an XMPP
    account:

  1.  Using `ejabberdctl` (see section [ejabberdctl][7]):

      ``` bash
      ejabberdctl register admin1 example.org password
      ```

  2.  Using an XMPP client and In-Band Registration (see section [mod_register][20]).

2.  Edit the ejabberd configuration file to give administration
    rights to the XMPP account you created:

    ``` bash
    acl:
      admin:
        user:
          - "admin1": "example.org"
    access:
      configure:
        admin: allow
    ```

    You can grant administrative privileges to many XMPP accounts, and
    also to accounts in other XMPP servers.

3.  Restart ejabberd to load the new configuration.

4.  Open the Web Admin (usually `http://localhost:5280/admin/`) in your favourite
    browser. Make sure to enter the *full* JID as username (in this
    example: `admin1@example.org`). The reason that you also need to
    enter the suffix is due to ejabberd’s virtual hosting support. You can
    manage several XMPP domains on a single instance.

## Backend database

By default, ejabberd uses its own database to store runtime data. In many cases
you may need to let ejabberd use an external SQL database.
Supported SQL backends are MySQL, PostgreSQL, Sqlite, MSSQL.

When using external database backend, ejabberd does not create schema and tables
by itself. You must create the schema before you run ejabberd.

- If installing ejabberd from sources, you will find sql script for your backend
  in the installation directory. By default: `/usr/local/lib/ejabberd/priv/sql`

- If installing ejabberd from Process-One installer, the init scripts are located
  in the ejabberd's installation path under `<base>/lib/ejabberd*/priv/sql`

See [ejabberd SQL Database Schema](/developer/sql-schema/)
for details on database schemas.
See [Step-by-step Databases Configuration Guides](/admin/guide/databases/)
for detailed setup instructions.


[1]:  http://www.process-one.net/en/ejabberd/downloads
[2]:  http://www.microsoft.com/
[3]:  http://cean.process-one.net/
[4]:  http://xmpp.org/extensions/xep-0138.html
[5]:  http://www.process-one.net/
[6]:    /admin/guide/security/#epmd
[7]:    /admin/guide/managing/#ejabberdctl
[8]:    /admin/guide/configuration/#pam-authentication
[10]: https://www.opencsw.org/
[11]: http://www.erlang.org/download.html
[12]: http://sourceforge.net/project/showfiles.php?group_id=10127&package_id=11277
[13]: http://www.gnu.org/software/libiconv/
[14]: http://www.slproweb.com/products/Win32OpenSSL.html
[15]: http://www.zlib.net/
[16]:   /admin/guide/configuration/#captcha
[17]:   /admin/guide/configuration/#database
[18]:   /admin/guide/configuration/#relational-databases
[19]:   /admin/guide/security/#erlang-cookie
[20]:   /admin/guide/configuration/#mod-register

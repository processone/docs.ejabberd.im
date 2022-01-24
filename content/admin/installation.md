---
title: Installing ejabberd
menu: Installation
toc: true
order: 2
---

There are several ways to install ejabberd Community Server:

- [Linux RUN Installer](#linux-run-installer) – for any Linux
- [Linux DEB and RPM Installers](#linux-deb-and-rpm-installers) – for DEB and RPM based Linux
- [Operating System packages](#operating-system-packages) – for System Operators
- [Docker image](#docker-image) – for Windows, macOS, Linux, ...
- [Homebrew](#homebrew) – for macOS
- [Source Code](#source-code) – for developers and advanced administrators

Once installed, you can head to the next steps:

- [Starting ejabberd](#starting-ejabberd)
- [Autostart on Linux](#autostart-on-linux)
- [Administration Account](#administration-account)
- [Configuring ejabberd](#configuring-ejabberd)

# Linux RUN Installer

The `*.run` binary installer will deploy and configure a full featured
ejabberd server and does not require any extra dependencies. It
includes a stripped down version of Erlang. As such, when using
ejabberd installer, you do not need to install Erlang separately.

Those instructions assume installation on `localhost` for development purposes.
In this document, when mentioning `ejabberd-YY.MM`, we assume `YY.MM`
is the release number, for example 18.01. Also note that the installer
scripts support many options useful for production or unattended, scripted installation.
You can read more on installer options on [unattended installation](/admin/guide/unattended/).

Installation using the `*.run` binary installer:

1. Go to [ejabberd official download page](https://www.process-one.net/en/ejabberd/downloads/) on ProcessOne website.
2. Download the "Linux x86 64-bits Installer".
3. **Right-click on the downloaded file and select "Properties". Click on the "Permissions" tab and tick the box that says "Allow executing file as program".**
4. Now you are able to double-click the file to execute it and start the installer.
   You can also set the installer as executable and start it using the command line:
   ``` bash
   chmod +x ejabberd-YY.MM-linux-x64.run
   ./ejabberd-YY.MM-linux-x64.run
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
7. Now you can go to the web dashboard at `http://localhost:5280/admin/` and fill the username field with the full account JID, for example `admin@domain` (or `admin@localhost` as above). Then fill the password field with that account's `password`. The next step is to get to know [how to configure ejabberd](/admin/configuration/).
8. If something goes wrong during the installation, and you would like to start from scratch, you will find the ejabberd `uninstall` in the directory where it was installed.

To start and stop ejabberd manually, use the desktop shortcuts
created by the installer. If the machine doesn't have a graphical
system, use the scripts ’start’ and ’stop’ in the ’bin’ directory
where ejabberd is installed.

# Linux DEB and RPM Installers

ProcessOne provides DEB and RPM all-in-one binary installers with the same content
that the `*.run` Binary Installer mentioned in the previous section.

Those are self-sufficient packages that contain a minimal Erlang distribution,
this ensures that it does not interfere with your existing Erlang version
and is also a good way to make sure ejabberd will run with the latest Erlang version.

Those packages install ejabberd in `/opt/ejabberd-XX.YY/`.
Your configuration and Mnesia database are available in `/opt/ejabberd/`.

You can download the DEB and RPM packages from the
[ejabberd official download page](https://www.process-one.net/en/ejabberd/downloads/).

# Operating System Packages

Many operating systems provide specific ejabberd packages adapted
to the system architecture and libraries. They usually also check
dependencies and perform basic configuration tasks like creating the
initial administrator account.

List of known ejabberd packages:

- [Alpine Linux](https://pkgs.alpinelinux.org/packages?name=ejabberd&branch=edge)
- [Arch Linux](https://archlinux.org/packages/community/x86_64/ejabberd/)
- [Debian](https://tracker.debian.org/pkg/ejabberd)
- [Fedora](https://apps.fedoraproject.org/packages/ejabberd)
- [FreeBSD](https://www.freshports.org/net-im/ejabberd/)
- [Gentoo](https://packages.gentoo.org/packages/net-im/ejabberd)
- [OpenSUSE](https://software.opensuse.org/package/ejabberd)
- [NetBSD](https://pkgsrc.se/chat/ejabberd/)
- [Ubuntu](https://packages.ubuntu.com/search?keywords=ejabberd)

Consult the resources provided by your Operating System for more information.

There's also an [ejabberd snap](https://snapcraft.io/ejabberd)
to install ejabberd on serveral operating systems using Snap package manager.

# Docker Image

If you already have Docker installed,
the [ejabberd/ecs Docker image](https://hub.docker.com/r/ejabberd/ecs)
allows to install ejabberd in one single command:

``` bash
docker pull ejabberd/ecs
```

If you use Microsoft Windows 7, 10, or similar operating systems, check those tutorials:

- [Install ejabberd on Windows 10 using Docker Desktop](https://www.process-one.net/blog/install-ejabberd-on-windows-10-using-docker-desktop/)
- [Install ejabberd on Windows 7 using Docker Toolbox](https://www.process-one.net/blog/install-ejabberd-on-windows-7-using-docker-toolbox/)

For bug reports and improvement suggestions, please go to the
[docker-ejabberd Git Repository](https://github.com/processone/docker-ejabberd/tree/master/ecs).

# Homebrew

[Homebrew](https://brew.sh/) is a package manager for macOS that aims to port the many Unix & Linux software that is not easily available or compatible. Homebrew installation is simple and the instruction is available on its website.

Check also the guide for
[Installing ejabberd development environment on OSX](/developer/install-osx/)

The ejabberd configuration included in Homebrew's ejabberd 
has as default domain `localhost`,
and has already granted administrative privileges to the account `admin@localhost`.

1. Once you have Homebrew installed, open Terminal. Run

    ``` bash
    brew install ejabberd
    ```

    This should install the latest or at most the one-before-latest version of ejabberd. The installation directory should be reported at the end of this process, but usually the main executable is stored at `/usr/local/sbin/ejabberdctl`.

2. Start ejabberd in interactive mode, which prints useful messages in the Terminal.

    ``` bash
    /usr/local/sbin/ejabberdctl live
    ```

3. Create the account `admin@localhost` with password set as `password`:

    ``` bash
    /usr/local/sbin/ejabberdctl register admin localhost password
    ```

4. Now you can go to the web dashboard at `http://localhost:5280/admin/` and fill the username field with the full account JID, for example `admin@localhost`, then fill the password field with that account's `password`.

5. Without configuration there's not much to see here, therefore the next step is to get to know [how to configure ejabberd](/admin/configuration/).

# Source Code

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
-   [Erlang/OTP](https://www.erlang.org/) 19.3 or higher.
    We recommend using Erlang OTP 21.2.
-   OpenSSL 1.0.0 or higher, for STARTTLS, SASL and SSL encryption.
-   Zlib 1.2.3 or higher. Optional.
    For [Zlib Stream Compression](/admin/configuration/listen-options/#zlib)
-   PAM library. Optional.
    For [PAM Authentication](/admin/configuration/authentication/#pam-authentication)
-   ImageMagick’s Convert program and Ghostscript fonts. Optional.
    For [CAPTCHA challenges](/admin/configuration/basic/#captcha).
-   [Elixir](https://elixir-lang.org/) 1.10.3 or higher. Optional.
    For [Elixir Development](/developer/extending-ejabberd/elixir/)

## Downloading

Released versions of ejabberd are available on ProcessOne
[ejabberd official download page](https://www.process-one.net/en/ejabberd/downloads).

Alternatively, the latest development source code can be retrieved
from the Git repository using the commands:

``` bash
git clone git://github.com/processone/ejabberd.git ejabberd
cd ejabberd
```

## Compilation

To compile ejabberd execute the commands:

``` bash
./autogen.sh
./configure --enable-user=ejabberd --enable-mysql
make
```

`./autogen.sh` is required only when download from git,
as that doesn't include the configure script.

In this example, [`./configure`](#configure) prepares the installed program
to run with a user called ejabberd, so please create that user
or tell to use another local user.
It isn't recommended to run ejabberd with `root` user.

[`make`](#make) gets the dependencies and compiles everything.

*Note*: To build ejabberd, you will need Internet access, as
 dependencies will be downloaded depending on the selected options.

### `./configure`

The build configuration script supports many options.
Get the full list:

``` bash
./configure --help
```

Options details:

- **`-–bindir=/`**: Specify the path to the user executables
  (where `epmd` and `iex` are available).

- **`-–prefix=/`**: Specify the path prefix where the files will be
  copied when running the `make install` command.

<div class="note-down">added in <a href="/archive/20_12/">20.12</a>
and improved in <a href="/archive/21_07/">21.07</a></div>

<br />

- **`--with-rebar=/`**: Specify the path to rebar, rebar3 or
  [mix](/developer/extending-ejabberd/elixir/)

- **`-–enable-user[=USER]`**: Allow this normal system user to execute
  the ejabberdctl script (see section [ejabberdctl](/admin/guide/managing/#ejabberdctl)), read the
  configuration files, read and write in the spool directory, read and
  write in the log directory. The account user and group must exist in
  the machine before running `make install`.
  This account needs a HOME directory, because the
  [Erlang cookie file](/admin/guide/security/#erlang-cookie)
  will be created and read there.

- **`-–enable-group[=GROUP]`**: Similar to the previous option, but for
  system groups.

- **`–-enable-all`**: Enable many of the database and dependencies
    options described here, this is useful for Dialyzer checks:
    --enable-debug --enable-elixir --enable-mysql --enable-odbc
    --enable-pam --enable-pgsql --enable-redis --enable-sip
    --enable-sqlite --enable-stun --enable-tools --enable-zlib

- **`–-disable-debug`**: Compile without `+debug_info`.

<div class="note-down">improved in <a href="/archive/21_07/">21.07</a></div>

<br />

- **`–-enable-elixir`**: Build ejabberd with Elixir extension support.
    Works only with rebar2. If interested in Elixir development, you may
    prefer to install Elixir yourself and use `--with-rebar=mix`

- **`–-disable-erlang-version-check`**: Don't check Erlang/OTP version.

- **`-–enable-full-xml`**: Use XML features in XMPP stream (ex:
    CDATA). This requires XML compliant clients).

- **`–-enable-hipe`**: Compile natively with HiPE. This is an
    experimental feature, and not recommended.

- **`-–enable-lager`**: Use lager Erlang logging tool instead of
  standard error logger.

- **`--enable-latest-deps`**: Makes rebar use latest versions of
  dependencies developed alongside ejabberd instead of version
  specified in rebar.config. Should be only used when developing
  ejabberd.

<div class="note-left">added in <a href="/archive/21_04/">21.04</a></div>

- **`-–enable-lua`**: Enable Lua support, to import from Prosody.

- **`-–enable-mssql`**: Enable Microsoft SQL Server support, this
    option requires --enable-odbc (see [Supported storages][18]).

- **`-–enable-mysql`**: Enable MySQL support (see [Supported storages][18]).

- **`-–enable-new-sql-schema`**: Use new SQL schema.

- **`-–enable-odbc`**: Enable pure ODBC support.

- **`-–enable-pam`**: Enable the PAM authentication method (see [PAM Authentication](/admin/configuration/authentication/#pam-authentication) section).

- **`-–enable-pgsql`**: Enable PostgreSQL support (see [Supported storages][18]).

- **`-–enable-redis`**: Enable Redis support to use for external
    session storage.

- **`-–enable-roster-gateway-workaround`**: Turn on workaround for
    processing gateway subscriptions.

- **`-–enable-sip`**: Enable SIP support.

- **`-–enable-sqlite`**: Enable SQLite support (see [Supported storages][18]).

- **`-–disable-stun`**: Disable STUN/TURN support.

- **`--enable-system-deps`**: Makes rebar use locally installed
    dependencies instead of downloading them.

<div class="note-left">changed in <a href="/archive/21_04/">21.04</a></div>

- **`-–enable-tools`**: Enable the use of development tools.

- **`-–disable-zlib`**: Disable Stream Compression (XEP-0138) using zlib.

### `make`

This tool is used to compile ejabberd and perform other tasks:

- Get, update, compile dependencies; clean files
- [System install](#system-install), uninstall
- Build OTP [production](#production-release)
/ [development](#production-release) releases
- Development: edoc, [options](/developer/guide/#configuration),
 [translations](/developer/extending-ejabberd/localization/), tags
- Testing: dialyzer, [hooks](/developer/guide/#hooks),
 [test](/developer/extending-ejabberd/testing/), xref

Get the full list and details:

``` bash
make help
```

## Installation

There are several ways to install and run the ejabberd compiled from source code:
[system install](#system-install),
building a [production](#production-release) release,
or building a [development](#production-release) release.

### System Install

To install ejabberd in the destination directories, run the command `make install`.

Note that you probably need administrative privileges in the system to
install ejabberd.

The created files and directories depend on the options
provided to [`./configure`](#configure), by default they are:

<!-- TODO: Check we are up to date -->

- `/etc/ejabberd/`:  Configuration directory:

    - `ejabberd.yml`:  ejabberd configuration file (see [File Format](/admin/configuration/file-format/))
    - `ejabberdctl.cfg`: Configuration file of the administration script (see [Erlang Runtime System](/admin/guide/managing/#erlang-runtime-system))
    - `inetrc`: Network DNS configuration file for Erlang

- `/lib/ejabberd/`:

    - `ebin/`: Erlang binary files (\*.beam)
    - `include/`: Erlang header files (\*.hrl)
    - `priv/`: Additional files required at runtime
    - `bin/`: Executable programs
    - `lib/`: Binary system libraries (\*.so)
    - `msgs/`: Translation files (\*.msgs) (see [Default Language](/admin/configuration/basic/#default-language))

- `/sbin/ejabberdctl`: Administration script (see [ejabberdctl](/admin/guide/managing/#ejabberdctl))

- `/share/doc/ejabberd/`: Documentation of ejabberd

- `/var/lib/ejabberd/`: Spool directory:

   - `.erlang.cookie`: The [Erlang cookie file](/admin/guide/security/#erlang-cookie)
   - `acl.DCD, ...`: Mnesia database spool files (\*.DCD, \*.DCL, \*.DAT)

- `/var/log/ejabberd/`: Log directory (see [Logging](/admin/configuration/basic/#logging)):

    - `ejabberd.log`:   ejabberd service log
    - `erlang.log`:   Erlang/OTP system log

<div class="note-down">improved in <a href="/archive/21_07/">21.07</a></div>
### Production Release

You can build an OTP release that includes ejabberd, Erlang/OTP
and all the required erlang dependencies in a single tar.gz file.
Then you can copy that file to another machine that has the same machine
architecture, and run ejabberd without installing anything else.

To build that release, run:
``` bash
make rel
```

If you provided to [`./configure`](#configure) the option
 `--with-rebar` to use rebar3 or mix, this will directly
produce a tar.gz that you can copy.

This example uses rebar3 to manage the compilation,
builds an OTP production release,
copies the resulting package to a temporary path,
and starts ejabberd there:

``` bash
./configure --with-rebar=rebar3
make rel
cp _build/prod/rel/ejabberd/ejabberd-21.04.123.tar.gz /tmp
cd /tmp
mkdir ejatest
tar -xzf ejabberd-21.04.123.tar.gz -C ejatest
ejatest/bin/ejabberd console
```

<div class="note-down">new in <a href="/archive/21_07/">21.07</a></div>
### Development Release

If you provided to [`./configure`](#configure) the option
 `--with-rebar` to use rebar3 or mix,
you can build an OTP development release.

This is designed to run ejabberd in the local machine for development,
manual testing... without installing in the system.

This development release has some customizations: uses a dummy certificate file,
if you register the account admin@localhost it has admin rights...

This example [uses Elixir's mix](/developer/extending-ejabberd/elixir/)
to manage the compilation,
builds an OTP development release,
and starts ejabberd there:

``` bash
./configure --with-rebar=mix
make dev
_build/dev/rel/ejabberd/bin/ejabberdctl live
```

## Specific notes

### BSD

The command to compile ejabberd in BSD systems is `gmake`.

### macOS

If compiling from sources on macOS, you must configure ejabberd to use custom OpenSSL, Yaml, iconv.
The best approach is to use [Homebrew](https://brew.sh/) to install your dependencies, then
exports your custom path to let configure and make be aware of them.

``` bash
brew install git erlang elixir openssl expat libyaml libiconv libgd sqlite rebar rebar3 automake autoconf
export LDFLAGS="-L/usr/local/opt/openssl/lib -L/usr/local/lib -L/usr/local/opt/expat/lib"
export CFLAGS="-I/usr/local/opt/openssl/include -I/usr/local/include -I/usr/local/opt/expat/include"
export CPPFLAGS="-I/usr/local/opt/openssl/include/ -I/usr/local/include -I/usr/local/opt/expat/include"
./configure
make
```


# Next steps

## Starting ejabberd

Depending on how you installed ejabberd, it may be started automatically by
the operating system at system boot time.

You can use the `ejabberdctl` command line administration script to
start and stop ejabberd, check its status and many other administrative tasks.

If you provided the configure option
`–enable-user=USER` (see compilation [options](#options)), you can execute `ejabberdctl`
with either that system account or root.

Usage example:

``` bash
prompt> ejabberdctl start

prompt> ejabberdctl status
The node ejabberd@localhost is started with status: started
ejabberd is running in that node

prompt> ejabberdctl stop
```

If ejabberd doesn't start correctly and a crash dump file is generated,
there was a severe problem.
You can try to start ejabberd in interactive mode with the command
`bin/ejabberdctl live`
to see the error messages provided by Erlang and identify the exact the problem.

The `ejabberdctl` administration script is included in the `bin`
directory in the Linux Installers and Docker image.

Please refer to the section
[ejabberdctl](/admin/guide/managing/#ejabberdctl)
for details
about `ejabberdctl`, and configurable options to fine tune the Erlang
runtime system.

## Autostart on Linux

On a \*nix system, create a system user called 'ejabberd', give it write access
to the directories `database/` and `logs/`, and set that as home.

If you want ejabberd to be started as daemon at boot time with that user,
copy `ejabberd.init` from the `bin` directory to something like `/etc/init.d/ejabberd`.
Then you can call `/etc/inid.d/ejabberd start` to start the server.

Or if you have a `systemd` distribution:

1. copy `ejabberd.service` to `/etc/systemd/system/`
2. run `systemctl daemon-reload`
3. run `systemctl enable ejabberd.service`
4. To start the server, you can run `systemctl start ejabberd`

When ejabberd is started, the processes that are started in the system
are `beam` or `beam.smp`, and also `epmd`. For more information
regarding `epmd` consult the section relating to
[epmd](/admin/guide/security/#epmd).


## Administration Account

ejabberd binary installer prompts you for an admin account, so in that case,
you can probably skip this step.

However, if you use another way of installing ejabberd you may need to create an
admin XMPP account.

You need an XMPP account with administrative privileges to
enter the ejabberd Web Admin. Here are the steps to create it:

1.  Register an XMPP account on your ejabberd server, for example
    `admin1@example.org`. There are two ways to register an XMPP
    account:

  - Using an XMPP client and [In-Band Registration](/admin/configuration/modules/#mod-register).

  - Using [ejabberdctl](/admin/guide/managing/#ejabberdctl):
      ``` bash
      ejabberdctl register admin1 example.org password
      ```

2.  Edit the ejabberd configuration file to give administration
    rights to the XMPP account you created:

    ``` yaml
    acl:
      admin:
        user: admin1@example.org
    access_rules:
      configure:
        allow: admin
    ```

    You can grant administrative privileges to many XMPP accounts, and
    also to accounts in other XMPP servers.

3.  Restart ejabberd to load the new configuration.

4.  Open the Web Admin page (usually `http://localhost:5280/admin/`) in your favourite
    browser. Make sure to enter the **full JID* (in this
    example: `admin1@example.org`).
See [Web Admin](/admin/guide/managing/#web-admin) for details.


## Configuring ejabberd

Now that you got ejabberd installed and running, it's time to configure it
to your needs. You can follow on the
[Configuration](/admin/configuration/) section
and take also a look at the
[Tutorials](/tutorials/).



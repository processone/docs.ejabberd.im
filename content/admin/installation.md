---
title: ejabberd Installation and Setup
menu: Installation
order: 2
---

You have several options to install ejabberd:

- [Installing `ejabberd` with Binary Installer](#installing-ejabberd-with-binary-installer)
- [Installing `ejabberd` with Operating System Specific Packages](#installing-ejabberd-with-operating-system-specific-packages)
- [Installing `ejabberd` from Source Code](#installing-ejabberd-from-source-code)

Once installed, you may want to register users and create admin accounts, if you
did not use binary installers:

- [Post-install Operations](#post-install-operations)

___

# Installing ejabberd with Binary Installer {#installing-ejabberd-with-binary-installer}

## Downloading and running the installer

Probably the easiest way to install an `ejabberd` instant messaging
server is using the binary installer published by ProcessOne. The
binary installers of released `ejabberd` versions are available in the
[ProcessOne][5] `ejabberd` downloads page: [ejabberd Downloads][1].

The binary installer will deploy and configure a full featured
`ejabberd` server and does not require any extra dependencies. It
includes a stripped down version of Erlang. As such, when using
ejabberd installer, you do not need to install Erlang separately.

In \*nix systems, remember to set executable the binary installer
before starting it. For example:

``` bash
chmod +x ejabberd-16.09-linux-x86_64-installer.run
./ejabberd-16.09-linux-x86_64-installer.run
```

The installer script support many options, especially for unattended, scripted
installation. You can read more on installer options on [unattended
installation](/admin/guide/unattended/).

### Starting ejabberd

`ejabberd` can be started manually at any time, or automatically by
the operating system at system boot time.

To start and stop `ejabberd` manually, use the desktop shortcuts
created by the installer. If the machine doesn’t have a graphical
system, use the scripts ’start’ and ’stop’ in the ’bin’ directory
where `ejabberd` is installed.

<!-- TODO: What about systemd in newer Linux distributions ? -->

On a \*nix system, if you want ejabberd to be started as daemon at
boot time, copy `ejabberd.init` from the ’bin’ directory to something
like `/etc/init.d/ejabberd` (depending on your distribution). Create a
system user called `ejabberd`, give it write access to the directories
`database/` and `logs/`, and set that as home; the script will start
the server with that user. Then you can call `/etc/inid.d/ejabberd
start` as root to start the server.

When ejabberd is started, the processes that are started in the system
are `beam` or `beam.smp`, and also `epmd`. For more information
regarding `epmd` consult the section relating to [epmd][6].

If `ejabberd` doesn’t start correctly and a crash dump file is
generated, there was a severe problem. You can try starting `ejabberd`
with the script `bin/live.bat` in Windows, or with the command
`bin/ejabberdctl live` in other Operating Systems. This way you see
the error message provided by Erlang and can identify what is exactly
the problem.

The `ejabberdctl` administration script is included in the `bin`
directory. Please refer to the section [ejabberdctl][7] for details
about `ejabberdctl`, and configurable options to fine tune the Erlang
runtime system.

## Using ejabberd on Microsoft Windows {#using-ejabberd-on-microsoft-windows}

The Windows installer also adds ejabberd as a system service, and a
shortcut to a debug console for experienced administrators. If you
want ejabberd to be started automatically at boot time, go to the
Windows service settings and set ejabberd to be automatically
started. Note that the Windows service is a feature still in
development, and for example it doesn’t read the file
`ejabberdctl.cfg`.

On Microsoft Windows, the Erlang processes for ejabberd are named
`erl.exe` and `epmd.exe`.

# Installing ejabberd with Operating System Specific Packages {#installing-ejabberd-with-operating-system-specific-packages}

<!-- TODO: Update with mention to our links and RPMs -->

Some Operating Systems provide a specific `ejabberd` package adapted
to the system architecture and libraries. It usually also checks
dependencies and performs basic configuration tasks like creating the
initial administrator account. Some examples are Debian and
Gentoo. Consult the resources provided by your Operating System for
more information.

Usually those packages create a script like `/etc/init.d/ejabberd` to
start and stop `ejabberd` as a service at boot time.

ProcessOne now provides RPM and DEB all in one packages as well, since
ejabberd version 15.06. This is self-sufficient packages also
containing a minimal Erlang distribution. It ensures that it does not
interfere with your existing Erlang version. This is also a good way
to make sure ejabberd will run with the latest Erlang version. You can
download the packages from [ejabberd Downloads][1].

# Installing ejabberd from Source Code {#installing-ejabberd-from-source-code}

The canonical form for distribution of `ejabberd` stable releases is
the source code package. Compiling `ejabberd` from source code is
quite easy in \*nix systems, as long as your system have all the
dependencies.

## Requirements

To compile `ejabberd` on a ‘Unix-like’ operating system, you need:

-   GNU Make
-   GCC
-   Libexpat 1.95 or higher
-   Libyaml 0.1.4 or higher
-   Erlang/OTP 17.1 or higher
-   OpenSSL 1.0.0 or higher, for STARTTLS, SASL and SSL encryption.
-   Zlib 1.2.3 or higher, for Stream Compression support ([`XEP-0138`][4]). Optional.
-   PAM library. Optional. For Pluggable Authentication Modules (PAM). See section [pam][8].
-   GNU Iconv 1.8 or higher, for the IRC Transport (mod\_irc). Optional. Not needed on systems with GNU Libc. See section [mod_irc][9].
-   ImageMagick’s Convert program. Optional. For CAPTCHA challenges. See section [captcha][16].

## Downloading ejabberd Source Code

Released versions of `ejabberd` are available on ProcessOne
[`ejabberd` downloads page][1].

Alternatively, the latest development source code can be retrieved
from the Git repository using the commands:

``` bash
git clone git://github.com/processone/ejabberd.git ejabberd
cd ejabberd
./autogen.sh
```

## Compile

To compile `ejabberd` execute the commands:

``` bash
./configure --enable-mysql -–enable-user=ejabberd
```

*Note*: To build ejabberd, you will need Internet access, as
 dependencies will by downloaded depending on the selected options in

The build configuration script allows several options. To get the full
list run the command:

``` bash
./configure --help
```

### Configure options

Some options that you may be interested in modifying:

- **`-–bindir=/`**: Specify the path to the user executables
  (where `epmd` and `iex` are available).

- **`-–prefix=/`**: Specify the path prefix where the files will be
  copied when running the `make install` command.

- **`-–enable-user[=USER]`**: Allow this normal system user to execute
  the ejabberdctl script (see section [ejabberdctl][7]), read the
  configuration files, read and write in the spool directory, read and
  write in the log directory. The account user and group must exist in
  the machine before running `make install`. This account doesn’t need
  an explicit HOME directory, because `/var/lib/ejabberd/` will be
  used by default.
  **NOTE:** If obmit this command ejabberd may be run as an unespected
  user. If you e.g. later install it with root privilegies it may even
  be executed as root!

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

- **`-–enable-iconv`**: Enable iconv support. This is needed for
  `mod_irc` (see section [mod_irc][9]).

- **`–-enable-debug`**: Compile with `+debug_info` enabled.

- **`–-enable-nif`**: Replaces some critical Erlang functions with
  equivalents written in C to improve performance.

- **`–-enable-elixir`**: Build ejabberd with Elixir extension support.

- **`–-enable-all`**: Enable all previous options.

- **`--enable-latest-deps`**: Makes rebar use latest versions of dependences developed
  alongside `ejabberd` instead of version specified in rebar.config. Should be only used
  when developing `ejabberd`.

Here are other available options, that are experimental and not recommended:

- **`–-disable-transient-supervisors`**: Disable the use of Erlang/OTP
  supervision for transient processes.

- **`–-enable-hipe`**: Compile natively with HiPE, not recommended.

## Install

To install `ejabberd` in the destination directories, run the command:

``` bash
make install
```

Note that you probably need administrative privileges in the system to
install `ejabberd`.

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

- `/sbin/ejabberdctl`: Administration script (see section [ejabberdctl][7])

- `/share/doc/ejabberd/`: Documentation of ejabberd

- `/var/lib/ejabberd/`: Spool directory:

   - `.erlang.cookie`: Erlang cookie file (see section [cookie][19])
   - `acl.DCD, ...`: Mnesia database spool files (\*.DCD, \*.DCL, \*.DAT)

- `/var/log/ejabberd/`: Log directory (see section [logfiles]):

    - `ejabberd.log`:   ejabberd service log
    - `erlang.log`:   Erlang/OTP system log

## Start

You can use the `ejabberdctl` command line administration script to
start and stop `ejabberd`. If you provided the configure option
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

If `ejabberd` doesn’t start correctly and a crash dump is generated,
there was a severe problem. You can try starting `ejabberd` with the
command `ejabberdctl live` to see the error message provided by Erlang
and can identify what is exactly the problem.

Please refer to the section [ejabberdctl][7] for details about
`ejabberdctl`, and configurable options to fine tune the Erlang
runtime system.

<!-- TODO: What about systemd -->

If you want ejabberd to be started as daemon at boot time, copy
`ejabberd.init` to something like `/etc/init.d/ejabberd` (depending on
your distribution). Create a system user called `ejabberd`; it will be
used by the script to start the server. Then you can call
`/etc/inid.d/ejabberd start` as root to start the server.

## Specific Notes

### Specific Notes for OSX (Yosemite, El Capitan, Sierra)

On OS X, you need to tell ejabberd to use custom OpenSSL, Yaml, iconv
for the build. The best approach is to use [Homebrew](http://brew.sh) to
install your dependencies:

``` bash
brew install git erlang autoconf automake expat openssl libyaml libiconv sqlite
```

Here is an example command to build ejabberd with brew-installed
dependencies:

``` bash
export LDFLAGS="-L/usr/local/opt/openssl/lib -L/usr/local/lib -L/usr/local/opt/expat/lib"
export CFLAGS="-I/usr/local/opt/openssl/include/ -I/usr/local/include -I/usr/local/opt/expat/include"
export CPPFLAGS="-I/usr/local/opt/openssl/include/ -I/usr/local/include -I/usr/local/opt/expat/include"
./configure --enable-mysql
make
```

Note: Reference to custom OpenSSL needs at the moment to be passed to
`make` command. This is because make command download, configure and
build dependencies. The reference is also needed in that context.

Please, make sure that for OSX El Capitan you are aware of rootless
feature and have read Homebrew documentation no that topic:
[El Capitan & Homebrew](https://github.com/Homebrew/homebrew/blob/master/share/doc/homebrew/El_Capitan_and_Homebrew.md)


### Specific Notes for BSD

The command to compile `ejabberd` in BSD systems is:

``` bash
gmake
```

### Specific Notes for Sun Solaris

<!-- TODO: Is this still valid ? -->

You need to have `GNU install`, but it isn’t included in Solaris. It
can be easily installed if your Solaris system is set up for
[`blastwave.org`][10] package repository. Make sure `/opt/csw/bin` is
in your `PATH` and run:

``` bash
pkg-get -i fileutils
```

If that program is called `ginstall`, modify the `ejabberd` `Makefile`
script to suit your system, for example:

``` bash
cat Makefile | sed s/install/ginstall/ > Makefile.gi
```

And finally install `ejabberd` with:

``` bash
gmake -f Makefile.gi ginstall
```

# Post-install Operations {#post-install-operations}

## Creating an XMPP Account for Administration

`ejabberd` binary installer prompts you for an admin account, so in that case,
you can probably skip this step.

However, if you use another way of installing ejabberd you may need to create an
admin XMPP account.

You need an XMPP account and grant him administrative privileges to
enter the `ejabberd` Web Admin. Here are the steps to create it:

1.  Register an XMPP account on your `ejabberd` server, for example
    `admin1@example.org`. There are two ways to register an XMPP
    account:

	1.  Using `ejabberdctl` (see section [ejabberdctl][7]):

	    ``` bash
	    ejabberdctl register admin1 example.org FgT5bk3
	    ```

	2.  Using an XMPP client and In-Band Registration (see section [mod_register][20]).

2.  Edit the `ejabberd` configuration file to give administration
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

3.  Restart `ejabberd` to load the new configuration.

4.  Open the Web Admin (usually `http://localhost:5280/admin/`) in your favourite
    browser. Make sure to enter the *full* JID as username (in this
    example: `admin1@example.org`). The reason that you also need to
    enter the suffix is due to `ejabberd`’s virtual hosting support. You can
    manage several XMPP domain on a single instance.

[1]:	http://www.process-one.net/en/ejabberd/downloads
[2]:	http://www.microsoft.com/
[3]:	http://cean.process-one.net/
[4]:	http://xmpp.org/extensions/xep-0138.html
[5]:	http://www.process-one.net/
[6]:    /admin/guide/security/#epmd
[7]:    /admin/guide/managing/#ejabberdctl
[8]:    /admin/guide/configuration/#pam-authentication
[9]:    /admin/guide/configuration/#modirc
[10]:	http://www.blastwave.org/
[11]:	http://www.erlang.org/download.html
[12]:	http://sourceforge.net/project/showfiles.php?group_id=10127&package_id=11277
[13]:	http://www.gnu.org/software/libiconv/
[14]:	http://www.slproweb.com/products/Win32OpenSSL.html
[15]:	http://www.zlib.net/
[16]:   /admin/guide/configuration/#captcha
[17]:   /admin/guide/configuration/#database
[18]:   /admin/guide/configuration/#relational-databases
[19]:   /admin/guide/security/#erlang-cookie
[20]:   /admin/guide/configuration/#mod_register

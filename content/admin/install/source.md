# Install ejabberd from Source Code

The canonical distribution form of ejabberd stable releases is the source code package.
Compiling ejabberd from source code is quite easy in \*nix systems, as long as your system have all the dependencies.

## Requirements

To compile ejabberd you need:

- GNU Make
- GCC
- Libexpat ≥ 1.95
- Libyaml ≥ 0.1.4
- [Erlang/OTP](https://www.erlang.org/) ≥ 20.0. We recommend using Erlang OTP 26.2, which is the version used in the binary installers and container images.
- OpenSSL ≥ 1.0.0
- Curl. Optional, for ejabberdctl feature [CTL_OVER_HTTP](../guide/managing.md#ctl_over_http)

Other optional libraries are:

- Zlib ≥ 1.2.3, For [Zlib Stream Compression](../configuration/listen-options.md#zlib)
- PAM library, for [PAM Authentication](../configuration/authentication.md#pam-authentication)
- ImageMagick’s Convert program and Ghostscript fonts, for [CAPTCHA challenges](../configuration/basic.md/#captcha).
- [Elixir](https://elixir-lang.org/) ≥ 1.10.3, for [Elixir Development](../../developer/extending-ejabberd/elixir.md). It is recommended Elixir 1.13.4 or higher and Erlang/OTP 23.0 or higher.

If your system splits packages in libraries and development headers,
install the development packages too.

For example, in Debian:

``` sh
apt-get install libexpat1-dev libgd-dev libpam0g-dev \
                libsqlite3-dev libwebp-dev libyaml-dev \
                autoconf automake erlang elixir rebar3
```

## Download

There are several ways to obtain the ejabberd source code:

- Source code package from [ProcessOne Downloads][p1dl] or [GitHub Releases][ghr]

- Latest development code from [ejabberd Git repository][gitrepo] using the commands:
    ``` sh
    git clone https://github.com/processone/ejabberd.git
    cd ejabberd
    ```

[p1dl]: https://www.process-one.net/download/
[ghr]: https://github.com/processone/ejabberd/releases
[gitrepo]: https://github.com/processone/ejabberd

## Compile

The generic instructions to compile ejabberd are:

``` sh
./autogen.sh
./configure
make
```

Let's view them in detail.

### `./configure`

The build configuration script supports many options.
Get the full list:

``` sh
./configure --help
```

In this example, [`./configure`](#configure) prepares the installed program to run with a user called ejabberd that should exist in the system (it isn't recommended to run ejabberd with `root` user):

``` sh
./configure --enable-user=ejabberd --enable-mysql
```

If you get `Error loading module rebar3`,
please consult how to use [rebar with old Erlang](#rebar-with-old-erlang).


Options details:

- **`--bindir=/`**: Specify the path to the user executables (where `epmd` and `iex` are available).

- **`--prefix=/`**: Specify the path prefix where the files will be copied when running the `make install` command.

- **`--with-rebar=/`**: Specify the path to rebar, rebar3 or
  [mix](../../developer/extending-ejabberd/elixir.md)

    <!-- md:version added in [20.12](../../archive/20.12/index.md) and improved in [24.02](../../archive/24.02/index.md) -->

- **`--enable-user[=USER]`**: Allow this normal system user to execute the ejabberdctl script (see section [ejabberdctl](../guide/managing.md#ejabberdctl)), read the configuration files, read and write in the spool directory, read and  write in the log directory.
The account user and group must exist in  the machine before running `make install`.
This account needs a HOME directory, because the [Erlang cookie file](../guide/security.md#erlang-cookie) will be created and read there.

- **`--enable-group[=GROUP]`**: Use this option additionally to `--enable-user`
  when that account is in a group that doesn't coincide with its username.

- **`--enable-all`**: Enable many of the database and dependencies
    options described here, this is useful for Dialyzer checks:
    --enable-debug --enable-elixir --enable-mysql --enable-odbc
    --enable-pam --enable-pgsql --enable-redis --enable-sip
    --enable-sqlite --enable-stun --enable-tools --enable-zlib

- **`--disable-debug`**: Compile without `+debug_info`.

- **`--enable-elixir`**: Build ejabberd with Elixir extension support.
    Works only with rebar3, not rebar2. Requires to have Elixir installed.
    If interested in Elixir development, you may prefer to use `--with-rebar=mix`

    <!-- md:version improved in [24.02](../../archive/24.02/index.md) -->

- **`--disable-erlang-version-check`**: Don't check Erlang/OTP version.

- **`--enable-full-xml`**: Use XML features in XMPP stream (ex: CDATA). This requires XML compliant clients).

- **`--enable-hipe`**: Compile natively with HiPE. This is an experimental feature, and not recommended.

- **`--enable-lager`**: Use lager Erlang logging tool instead of standard error logger.

- **`--enable-latest-deps`**: Makes rebar use latest versions of
  dependencies developed alongside ejabberd instead of version
  specified in rebar.config. Should be only used when developing
  ejabberd.

- **`--enable-lua`**: Enable Lua support, to import from Prosody.

    <!-- md:version added in [21.04](../../archive/21.04/index.md) -->

- **`--enable-mssql`**: Enable Microsoft SQL Server support, this
    option requires --enable-odbc (see [Supported storages][18]).

- **`--enable-mysql`**: Enable MySQL support (see [Supported storages][18]).

- **`--enable-new-sql-schema`**: Use new SQL schema.

- **`--enable-odbc`**: Enable pure ODBC support.

- **`--enable-pam`**: Enable the PAM authentication method (see [PAM Authentication](../configuration/authentication.md#pam-authentication) section).

- **`--enable-pgsql`**: Enable PostgreSQL support (see [Supported storages][18]).

- **`--enable-redis`**: Enable Redis support to use for external session storage.

- **`--enable-roster-gateway-workaround`**: Turn on workaround for processing gateway subscriptions.

- **`--enable-sip`**: Enable SIP support.

- **`--enable-sqlite`**: Enable SQLite support (see [Supported storages][18]).

- **`--disable-stun`**: Disable STUN/TURN support.

- **`--enable-system-deps`**: Makes rebar use locally installed
    dependencies instead of downloading them.

- **`--enable-tools`**: Enable the use of development tools.

    <!-- md:version changed in [21.04](../../archive/21.04/index.md) -->

- **`--disable-zlib`**: Disable Stream Compression (XEP-0138) using zlib.

### `make`

This gets the erlang depencies and compiles everything, among other tasks:

- Get, update, compile dependencies; clean files
- [System install](#system-install), uninstall
- Build OTP [production](#production-release) / [development](#production-release) releases
- Development: edoc, [options](../../developer/guide.md#configuration), [translations](../../developer/extending-ejabberd/localization.md), tags
- Testing: dialyzer, [hooks](../../developer/guide.md#hooks), [test](../../developer/extending-ejabberd/testing.md), xref

Get the full task list:

``` sh
make help
```

*Note*: The required erlang dependencies are downloaded from Internet. Or you can copy `$HOME/.hex/` package cache from another machine.

## Install

There are several ways to install and run ejabberd after it's compiled from source code:

- [system install](#system-install)
- [system install a release](#system-install-release)
- building a [production](#production-release) release
- building a [development](#production-release) release
- don't install at all, just [start](#start) with `make relive`

### System Install

To install ejabberd in the destination directories, run:

``` sh
make install
```

Note that you probably need administrative privileges in the system to install ejabberd.

The created files and directories depend on the options provided to [`./configure`](#configure), by default they are:

<!-- TODO: Check we are up to date -->

- `/etc/ejabberd/`:  Configuration directory:

    + `ejabberd.yml`:  ejabberd configuration file (see [File Format](../configuration/file-format.md))
    + `ejabberdctl.cfg`: Configuration file of the administration script (see [Erlang Runtime System](../guide/managing.md#erlang-runtime-system))
    + `inetrc`: Network DNS configuration file for Erlang

- `/lib/ejabberd/`:

    * `ebin/`: Erlang binary files (\*.beam)
    * `include/`: Erlang header files (\*.hrl)
    * `priv/`: Additional files required at runtime
    * `bin/`: Executable programs
    * `lib/`: Binary system libraries (\*.so)
    * `msgs/`: Translation files (\*.msgs) (see [Default Language](../configuration/basic.md#default-language))

- `/sbin/ejabberdctl`: Administration script (see [ejabberdctl](../guide/managing.md#ejabberdctl))

- `/share/doc/ejabberd/`: Documentation of ejabberd

- `/var/lib/ejabberd/`: Spool directory:

    - `.erlang.cookie`: The [Erlang cookie file](../guide/security.md#erlang-cookie)
    - `acl.DCD, ...`: Mnesia database spool files (\*.DCD, \*.DCL, \*.DAT)

- `/var/log/ejabberd/`: Log directory (see [Logging](../configuration/basic.md#logging)):

    - `ejabberd.log`:   ejabberd service log
    - `erlang.log`:   Erlang/OTP system log

### System Install Release

<!-- md:version added in [24.02](../../archive/24.02/index.md) -->

This builds a [production release](#production-release),
and then performs a [system install](#system-install) of that release,
obtaining a result similar to the one mentioned in the previous section.

Simply run:

``` sh
make install-rel
```

The benefits of `install-rel` over `install`:

- this uses OTP release code from rebar/rebar3/mix, and consequently requires less code in our `Makefile.in` file
- `uninstall-rel` correctly deletes all the library files
- the `*.beam` files are smaller as debug information is stripped

### Production Release

<!-- md:version improved in [21.07](../../archive/21.07/index.md) -->

You can build an OTP release that includes ejabberd, Erlang/OTP
and all the required erlang dependencies in a single tar.gz file.
Then you can copy that file to another machine that has the same machine
architecture, and run ejabberd without installing anything else.

To build that production release, run:

``` sh
make prod
```

If you provided to [`./configure`](#configure) the option
 `--with-rebar` to use rebar3 or mix, this will directly
produce a tar.gz that you can copy.

This example uses rebar3 to manage the compilation,
builds an OTP production release,
copies the resulting package to a temporary path,
and starts ejabberd there:

``` sh
./autogen.sh
./configure --with-rebar=rebar3
make
make prod
mkdir $HOME/eja-release
tar -xzvf _build/prod/ejabberd-*.tar.gz -C $HOME/eja-release
$HOME/eja-release/bin/ejabberdctl live
```

### Development Release

<!-- md:version new in [21.07](../../archive/21.07/index.md) -->

If you provided to [`./configure`](#configure) the option
 `--with-rebar` to use rebar3 or mix,
you can build an OTP development release.

This is designed to run ejabberd in the local machine for development,
manual testing... without installing in the system.

This development release has some customizations: uses a dummy certificate file,
if you register the account admin@localhost it has admin rights...

This example [uses Elixir's mix](../../developer/extending-ejabberd/elixir.md)
to manage the compilation,
builds an OTP development release,
and starts ejabberd there:

``` sh
./autogen.sh
./configure --with-rebar=mix
make
make dev
_build/dev/rel/ejabberd/bin/ejabberdctl live
```

## Specific notes

### asdf

When Erlang/OTP (and/or Elixir) is installed using [asdf](https://asdf-vm.com/) (multiple runtime version manager),
it is available only for your account, in `$HOME/.asdf/shims/erl`.
In that case, you cannot install ejabberd globally in the system, and you cannot use the `root` account to start it, because that account doesn't have access to erlang.

In that scenario, there are several ways to run/install ejabberd:

- Run a [development release](#development-release) locally without installing

- Copy a [production release](#production-release) locally

- Use [system install](#system-install), but install it locally:

``` sh
./autogen.sh
./configure --prefix=$HOME/eja-install --enable-user
make
make install
$HOME/eja-install/sbin/ejabberdctl live
```

### BSD

The command to compile ejabberd in BSD systems is `gmake`.

You may want to check [pkgsrc.se for ejabberd](https://pkgsrc.se/chat/ejabberd/).

Up to ejabberd [23.04](../../archive/23.04/index.md),
some old scripts where included in ejabberd source for NetBSD compilation, and you can take a look to those files for reference in ejabberd [`23.04/examples/mtr/`](https://github.com/processone/ejabberd/tree/23.04/examples/mtr) path.

### Erlang Configuration

Usually ejabberd is started using the `ejabberdctl` script,
which takes care to setup the
[Erlang runtime system options](https://docs.ejabberd.im/admin/guide/managing/#erlang-runtime-system)
following your `ejabberdctl.cfg` configuration file.

However, there are other methods to start ejabberd, and they use specific methods to configure those options:

- When using rebar3 `make relive`:  
system is set in `rebar.config`, apps in `rel/relive.config`

- When using mix `make relive`:  
system is set in `Makefile`, apps in `config/runtime.exs`

- When using rebar3 `make dev && ejabberd`:  
system is set in `rel/vm.args`, apps in `rel/sys.config`

- When using mix `make dev|prod && ejabberd`:  
system is set in `rel/vm.args.eex` and `rel/env.sh.eex`, apps in `config/runtime.exs`


### macOS

If compiling from sources on Mac OS X, you must configure ejabberd to use custom OpenSSL, Yaml, iconv.
The best approach is to use [Homebrew](https://brew.sh/) to install your dependencies, then exports your custom path to let configure and make be aware of them.

``` sh
brew install git erlang elixir openssl expat libyaml libiconv libgd sqlite rebar rebar3 automake autoconf
export LDFLAGS="-L/usr/local/opt/openssl/lib -L/usr/local/lib -L/usr/local/opt/expat/lib"
export CFLAGS="-I/usr/local/opt/openssl/include -I/usr/local/include -I/usr/local/opt/expat/include"
export CPPFLAGS="-I/usr/local/opt/openssl/include/ -I/usr/local/include -I/usr/local/opt/expat/include"
./configure
make
```

Check also the guide for [Installing ejabberd development environment on OSX](../../developer/install-osx.md)

### man

ejabberd includes a man page which documents the toplevel and modules options,
the same information that is published in the
[Top-Level Options](../configuration/toplevel.md) and
[Modules Options](../configuration/modules.md) sections.

The man file can be read locally with:

``` sh
man -l man/ejabberd.yml.5
```

### rebar with old Erlang

The ejabberd source code package includes `rebar` and `rebar3` binaries
that work with Erlang/OTP 24.0 up to 27.

To compile ejabberd using rebar/rebar3 and Erlang 20.0 up to 23.3,
you can install it from your operating system,
or compile yourself from the rebar source code,
or download the old binary from ejabberd 21.12:

```sh
wget https://github.com/processone/ejabberd/raw/21.12/rebar
wget https://github.com/processone/ejabberd/raw/21.12/rebar3
```

## Start

You can use the `ejabberdctl` command line administration script to
start and stop ejabberd. Some examples, depending on your installation method:

- When [installed in the system](#system-install):
``` sh
ejabberdctl start
/sbin/ejabberdctl start
```

- When [built an OTP production release](#production-release):
``` sh
_build/prod/rel/ejabberd/bin/ejabberdctl start
_build/prod/rel/ejabberd/bin/ejabberdctl live
```

- Start interactively without installing or building OTP release:
``` sh
make relive
```

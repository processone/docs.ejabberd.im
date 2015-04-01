---
title: Installing ejabberd | ejabberd Installation and Operation Guide
bodyclass: nocomment
---

# Installing `ejabberd`

## Installing `ejabberd` with Binary Installer

Probably the easiest way to install an `ejabberd` instant messaging server is using the binary installer published by ProcessOne. The binary installers of released `ejabberd` versions are available in the ProcessOne `ejabberd` downloads page: [`http://www.process-one.net/en/ejabberd/downloads`][1]

The installer will deploy and configure a full featured `ejabberd` server and does not require any extra dependencies.

In \*nix systems, remember to set executable the binary installer before starting it. For example:

	#!console
	chmod +x ejabberd-2.0.0_1-linux-x86-installer.bin
	./ejabberd-2.0.0_1-linux-x86-installer.bin

`ejabberd` can be started manually at any time, or automatically by the operating system at system boot time.

To start and stop `ejabberd` manually, use the desktop shortcuts created by the installer. If the machine doesn’t have a graphical system, use the scripts ’start’ and ’stop’ in the ’bin’ directory where `ejabberd` is installed.

The Windows installer also adds ejabberd as a system service, and a shortcut to a debug console for experienced administrators. If you want ejabberd to be started automatically at boot time, go to the Windows service settings and set ejabberd to be automatically started. Note that the Windows service is a feature still in development, and for example it doesn’t read the file `ejabberdctl.cfg`.

On a \*nix system, if you want ejabberd to be started as daemon at boot time, copy `ejabberd.init` from the ’bin’ directory to something like `/etc/init.d/ejabberd` (depending on your distribution). Create a system user called `ejabberd`, give it write access to the directories `database/` and `logs/`, and set that as home; the script will start the server with that user. Then you can call `/etc/inid.d/ejabberd start` as root to start the server.

When ejabberd is started, the processes that are started in the system are `beam` or `beam.smp`, and also `epmd`. In Microsoft Windows, the processes are `erl.exe` and `epmd.exe`. For more information regarding `epmd` consult the section relating to epmd.

If `ejabberd` doesn’t start correctly in Windows, try to start it using the shortcut in desktop or start menu. If the window shows error 14001, the solution is to install: “Microsoft Visual C++ 2005 SP1 Redistributable Package”. You can download it from [`www.microsoft.com`][2]. Then uninstall `ejabberd` and install it again.

If `ejabberd` doesn’t start correctly and a crash dump is generated, there was a severe problem. You can try starting `ejabberd` with the script `bin/live.bat` in Windows, or with the command `bin/ejabberdctl live` in other Operating Systems. This way you see the error message provided by Erlang and can identify what is exactly the problem.

The `ejabberdctl` administration script is included in the `bin` directory. Please refer to the section [ejabberdctl] for details about `ejabberdctl`, and configurable options to fine tune the Erlang runtime system.

## Installing `ejabberd` with Operating System Specific Packages

Some Operating Systems provide a specific `ejabberd` package adapted to the system architecture and libraries. It usually also checks dependencies and performs basic configuration tasks like creating the initial administrator account. Some examples are Debian and Gentoo. Consult the resources provided by your Operating System for more information.

Usually those packages create a script like `/etc/init.d/ejabberd` to start and stop `ejabberd` as a service at boot time.

## Installing `ejabberd` with CEAN

[`CEAN`][3] (Comprehensive Erlang Archive
Network) is a repository that hosts binary packages from many Erlang programs, including `ejabberd` and all its dependencies. The binaries are available for many different system architectures, so this is an alternative to the binary installer and Operating System’s `ejabberd` packages.

You will have to create your own `ejabberd` start script depending of how you handle your CEAN installation. The default `ejabberdctl` script is located into `ejabberd`’s priv directory and can be used as an example.

## Installing `ejabberd` from Source Code

The canonical form for distribution of `ejabberd` stable releases is the source code package. Compiling `ejabberd` from source code is quite easy in \*nix systems, as long as your system have all the dependencies.

### Requirements

To compile `ejabberd` on a ‘Unix-like’ operating system, you need:

-   GNU Make
-   GCC
-   Libexpat 1.95 or higher
-   Erlang/OTP R15B or higher.
-   Libyaml 0.1.4 or higher
-   OpenSSL 0.9.8 or higher, for STARTTLS, SASL and SSL encryption.
-   Zlib 1.2.3 or higher, for Stream Compression support
	([`XEP-0138`][4]). Optional.
-   PAM library. Optional. For Pluggable Authentication Modules (PAM). See section [pam].
-   GNU Iconv 1.8 or higher, for the IRC Transport (mod\_irc). Optional. Not needed on systems with GNU Libc. See section [modirc].
-   ImageMagick’s Convert program. Optional. For CAPTCHA challenges. See section [captcha].

### Download Source Code

Released versions of `ejabberd` are available in the ProcessOne
`ejabberd` downloads page: [`http://www.process-one.net/en/ejabberd/downloads`][5]

Alternatively, the latest development source code can be retrieved from the Git repository using the commands:

	#!console
	git clone git://github.com/processone/ejabberd.git ejabberd
	cd ejabberd
	./autogen.sh

### Compile

To compile `ejabberd` execute the commands:

	#!console
	./configure
	make

The build configuration script allows several options. To get the full list run the command:

	#!console
	./configure --help

Some options that you may be interested in modifying:

`–prefix=/`: Specify the path prefix where the files will be copied when running the `make install` command.

`–enable-user[=USER]`: Allow this normal system user to execute the ejabberdctl script (see section [ejabberdctl]()), read the configuration files, read and write in the spool directory, read and write in the log directory. The account user and group must exist in the machine before running `make install`. This account doesn’t need an explicit HOME directory, because `/var/lib/ejabberd/` will be used by default.

`–enable-pam`: Enable the PAM authentication method (see section [pam]).

`–enable-mssql`: Required if you want to use an external database. See section [database]() for more information.

`–enable-tools`: Enable the use of development tools.

`–enable-mysql`: Enable MySQL support (see section [odbc]).

`–enable-pgsql`: Enable PostgreSQL support (see section [odbc]).

`–enable-zlib`: Enable Stream Compression (XEP-0138) using zlib.

`–enable-iconv`: Enable iconv support. This is needed for `mod_irc` (see section [modirc]()).

`–enable-debug`: Compile with `+debug_info` enabled.

`–enable-full-xml`: Enable the use of XML based optimisations. It will for example use CDATA to escape characters in the XMPP stream. Use this option only if you are sure your XMPP clients include a fully compliant XML parser.

`–disable-transient-supervisors`: Disable the use of Erlang/OTP supervision for transient processes.

`–enable-nif`: Replaces some critical Erlang functions with equivalents written in C to improve performance.

`–enable-elixir`: Build ejabberd with Elixir extension support.

### Install

To install `ejabberd` in the destination directories, run the command:

	#!console
	make install

Note that you probably need administrative privileges in the system to install `ejabberd`.

The files and directories created are, by default:

`/etc/ejabberd/`:  Configuration directory:

- `ejabberd.yml`:  ejabberd configuration file
- `ejabberdctl.cfg`: Configuration file of the administration script
- `inetrc`: Network DNS configuration file for Erlang

`/lib/ejabberd/`:

- `ebin/`: Erlang binary files (\*.beam)
- `include/`: Erlang header files (\*.hrl)
- `priv/`: Additional files required at runtime
- `bin/`: Executable programs
- `lib/`: Binary system libraries (\*.so)
- `msgs/`:   Translation files (\*.msgs)

`/sbin/ejabberdctl`: Administration script (see section [ejabberdctl])

`/share/doc/ejabberd/`: Documentation of ejabberd

`/var/lib/ejabberd/`: Spool directory:

- `.erlang.cookie`: Erlang cookie file (see section [cookie]())
- `acl.DCD, ...`: Mnesia database spool files (\*.DCD, \*.DCL, \*.DAT)

`/var/log/ejabberd/`: Log directory (see section [logfiles]):

- `ejabberd.log`:   ejabberd service log
- `erlang.log`:   Erlang/OTP system log

### Start

You can use the `ejabberdctl` command line administration script to start and stop `ejabberd`. If you provided the configure option `–enable-user=USER` (see [compile]), you can execute `ejabberdctl` with either that system account or root.

Usage example:

	#!console
	prompt> ejabberdctl start
	
	prompt> ejabberdctl status
	The node ejabberd@localhost is started with status: started
	ejabberd is running in that node
	
	prompt> ejabberdctl stop

If `ejabberd` doesn’t start correctly and a crash dump is generated, there was a severe problem. You can try starting `ejabberd` with the command `ejabberdctl live` to see the error message provided by Erlang and can identify what is exactly the problem.

Please refer to the section [ejabberdctl] for details about
`ejabberdctl`, and configurable options to fine tune the Erlang runtime system.

If you want ejabberd to be started as daemon at boot time, copy `ejabberd.init` to something like `/etc/init.d/ejabberd` (depending on your distribution). Create a system user called `ejabberd`; it will be used by the script to start the server. Then you can call `/etc/inid.d/ejabberd start` as root to start the server.

### Specific Notes for BSD

The command to compile `ejabberd` in BSD systems is:

	#!console
	gmake

### Specific Notes for Sun Solaris

You need to have `GNU install`, but it isn’t included in Solaris. It can be easily installed if your Solaris system is set up for [`blastwave.org`][10] package repository. Make sure `/opt/csw/bin` is in your `PATH` and run:

	#!console
	pkg-get -i fileutils

If that program is called `ginstall`, modify the `ejabberd` `Makefile` script to suit your system, for example:

	#!console
	cat Makefile | sed s/install/ginstall/ > Makefile.gi

And finally install `ejabberd` with:

	#!console
	gmake -f Makefile.gi ginstall

### Specific Notes for Microsoft Windows

#### Requirements

To compile `ejabberd` on a Microsoft Windows system, you need:

-   MS Visual C++ 6.0 Compiler
-   [`Erlang/OTP R11B-5`][11]
-   [`Expat 2.0.0 or higher`][12]
-   [`GNU Iconv 1.9.2`][13] (optional)
-   [`Shining Light OpenSSL 0.9.8d or higher`][14] (to enable SSL connections)
-   [`Zlib 1.2.3 or higher`][15]

#### Compilation

We assume that we will try to put as much library as possible into `C:\sdk\` to make it easier to track what is install for `ejabberd`.

1.  Install Erlang emulator (for example, into `C:\sdk\erl5.5.5`).

2.  Install Expat library into `C:\sdk\Expat-2.0.0` directory.

	Copy file `C:\sdk\Expat-2.0.0\Libs\libexpat.dll` to your Windows system directory (for example, `C:\WINNT` or `C:\WINNT\System32`)

3.  Build and install the Iconv library into the directory
	`C:\sdk\GnuWin32`.

	Copy file `C:\sdk\GnuWin32\bin\lib*.dll` to your Windows system	directory (more installation instructions can be found in the file README.woe32 in the iconv distribution).

	Note: instead of copying libexpat.dll and iconv.dll to the Windows directory, you can add the directories `C:\sdk\Expat-2.0.0\Libs` and `C:\sdk\GnuWin32\bin` to the `PATH` environment variable.

4.  Install OpenSSL in `C:\sdk\OpenSSL` and add `C:\sdk\OpenSSL\lib\VC` to your path or copy the binaries to your system directory.

5.  Install ZLib in `C:\sdk\gnuWin32`. Copy
	`C:\sdk\GnuWin32\bin\zlib1.dll` to your system directory. If you change your path it should already be set after libiconv install.

6.  Make sure the you can access Erlang binaries from your path. For example: `set PATH=%PATH%;"C:\sdk\erl5.6.5\bin"`

7.  Depending on how you end up actually installing the library you might need to check and tweak the paths in the file configure.erl.

8.  While in the directory `ejabberd\src` run:

		#!console
		configure.bat
		nmake -f Makefile.win32

9.  Edit the file `ejabberd\src\ejabberd.yml` and run

		#!console
		werl -s ejabberd -name ejabberd

## Create an XMPP Account for Administration

You need an XMPP account and grant him administrative privileges to enter the `ejabberd` Web Admin:

1.  Register an XMPP account on your `ejabberd` server, for example `admin1@example.org`. There are two ways to register an XMPP account:

	1.  Using `ejabberdctl` (see section [ejabberdctl]):

			#!console
			ejabberdctl register admin1 example.org FgT5bk3

	2.  Using an XMPP client and In-Band Registration (see section [modregister]).

2.  Edit the `ejabberd` configuration file to give administration rights to the XMPP account you created:

		#!yaml
		acl:
		  admin:
		    user:
		      - "admin1": "example.org"
		access:
		  configure:
		    admin: allow

	You can grant administrative privileges to many XMPP accounts, and also to accounts in other XMPP servers.

3.  Restart `ejabberd` to load the new configuration.

4.  Open the Web Admin (`http://server:port/admin/`) in your favourite browser. Make sure to enter the *full* JID as username (in this example: `admin1@example.org`. The reason that you also need to enter the suffix, is because `ejabberd`’s virtual hosting support.

## Upgrading `ejabberd`

To upgrade an ejabberd installation to a new version, simply uninstall the old version, and then install the new one. Of course, it is important that the configuration file and Mnesia database spool directory are not removed.

`ejabberd` automatically updates the Mnesia table definitions at startup when needed. If you also use an external database for storage of some modules, check if the release notes of the new ejabberd version indicates you need to also update those tables.

[1]:	http://www.process-one.net/en/ejabberd/downloads
[2]:	http://www.microsoft.com/
[3]:	http://cean.process-one.net/
[4]:	http://xmpp.org/extensions/xep-0138.html
[5]:	http://www.process-one.net/en/ejabberd/downloads
[10]:	http://www.blastwave.org/
[11]:	http://www.erlang.org/download.html
[12]:	http://sourceforge.net/project/showfiles.php?group_id=10127&package_id=11277
[13]:	http://www.gnu.org/software/libiconv/
[14]:	http://www.slproweb.com/products/Win32OpenSSL.html
[15]:	http://www.zlib.net/
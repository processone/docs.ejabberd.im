---
title: Installing ejabberd development environment on OSX
---

# Installing ejabberd development environment on OSX

This short guide will show you how to compile ejabberd from source
code on Mac OS X, and get users chatting right away.

## Before you start

ejabberd is supported on Mac OS X 10.6.8 and later. Before you can
compile and run ejabberd, you also need the following to be installed
on your system:

* Gnu Make and GCC (the GNU Compiler Collection). To ensure that these
  are installed, you can install the Command Line Tools for Xcode,
  available via Xcode or from the Apple Developer website.
* Git
* Erlang /OTP R16B or higher. We recommand using Erlang R17B, latest
  version.
* Autotools

## Homebrew

An easy way to install some of the dependencies is by using a package
manager, such as [Homebrew](http://brew.sh) – the Homebrew commands
are provided here:

* Git: `brew install git`
* Erlang /OTP: `brew install erlang`
* Autoconf: `brew install autoconf`
* Automake: `brew install automake`
* Openssl: `brew install openssl`
* Expat: `brew install expat`
* Libyaml: `brew install libyaml`
* Libiconv: `brew install libiconv`
* Sqlite: `brew install sqlite`

You can install everything with a single command:

    #!console
    brew install git erlang autoconf automake expat openssl libyaml libiconv sqlite 

Please, make sure that for OSX El Capitan you are aware of rootless
feature and have read Homebrew documentation no that topic:
[El Capitan & Homebrew](https://github.com/Homebrew/homebrew/blob/master/share/doc/homebrew/El_Capitan_and_Homebrew.md)

## Installation

To build and install ejabberd from source code, do the following:

1. Clone the Git repository:  `git clone git@github.com:processone/ejabberd.git`
2. Go to your ejabberd build directory: `cd ejabberd`
3. Run the following commands, assuming you want to install your
   ejabberd deployment into your home directory:

        chmod +x autogen.sh
        ./autogen.sh
        export LDFLAGS="-L/usr/local/opt/openssl/lib -L/usr/local/lib -L/usr/local/opt/expat/lib"
        export CFLAGS="-I/usr/local/opt/openssl/include/ -I/usr/local/include -I/usr/local/opt/expat/include"
        export CPPFLAGS="-I/usr/local/opt/openssl/include/ -I/usr/local/include -I/usr/local/opt/expat/include"
        ./configure --prefix=$HOME/my-ejabberd --enable-mysql
        make && make install

Note that the previous command reference the previously installed
dependencies from [Homebrew](http://brew.sh).

The exports are sometimes needed to help the build chain find the
libraries installed by Homebrew, not the one provided by OSX.

However, often, it is just enough to for for Homebrew update its symlinks for OpenSSL:

~~~ bash
brew link openssl --force
~~~


## Running ejabberd

* From your ejabberd build directory, go to the installation directory:  `cd $HOME/my-ejabberd`
* To start the ejabberd server, run the following command:  `sbin/ejabberdctl start`
* To verify that ejabberd is running, enter the following:  `sbin/ejabberdctl status`
If the server is running, response should be as follow:
```
  $ sbin/ejabberdctl status
  The node ejabberd@localhost is started with status: started
  ejabberd 14.12.40 is running in that node
```
* To connect to the ejabberd console after starting the server:  `sbin/ejabberdctl debug`
* Alternatively, you can also run the server in interactive mode:  `sbin/ejabberdctl live`

## Registering a user

The default XMPP domain served by ejabberd right after the build is
`localhost`. This is different from the IP address, DNS name of the
server. It means remote users can connect to ejabberd even if it is
running on your machine with `localhost` XMPP domain, by using your
computer IP address or DNS name.
This can prove handy in development phase to get more testers.

##### Adium

Adium is a popular XMPP client on OSX. You can use it 

1. Launch Adium. If the Adium Setup Assistant opens, close it.
2. In the **Adium** menu, select **Preferences**, and then select the **Accounts** tab.
3. Click the **+** button and select **XMPP (Jabber)**.
4. Enter a Jabber ID (for example, “user1@localhost”) and password, and then click **Register New Account**.
5. In the **Server** field, enter the following:
	* Users registering on the computer on which ejabberd is running: `localhost`
	* Users registering from a different computer: the ejabberd server’s IP address
6. Click **Request New Account**.

After registration, the user will connect automatically.

Registered users wishing to add an existing account to Adium should
enter the ejabberd server’s IP address in the **Connect Server**
field on the **Options** tab.

##### Command line

You can register a user with the `ejabberdctl` utility:  
`ejabberdctl register user domain password`

For example:  
`ejabberdctl register user1 localhost myp4ssw0rd`

#### Domains

To use your system’s domain name instead of localhost, edit the
following ejabberd configuration file:
`$HOME/my-ejabberd/etc/ejabberd.yml` (point to the place of your real
installation).

Note: You may find example `ejabberd.cfg` files. This is the old
obsolete format for configuration file. You can ignore the and focus
on the new and more user friendly Yaml format.

Find the line listing the hosts:
```
hosts:
  - "localhost"
```

Replace `localhost` with your XMPP domain name, for example:
```
hosts:
  - "example.org"
```

Save the configuration file and restart the ejabberd server.
A user’s Jabber ID will then use the domain instead of localhost,
for example: `user1@example.org`

You can also configure multiple (virtual) domains for one server:
```
hosts:
  - "example1.org"
  - "example2.org"
```

#### Get chatting!

Users that are registered on your server can now add their accounts in
a chat application like Adium (specifying either the server’s IP
address or domain name), add each other as contacts, and start
chatting.


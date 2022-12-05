---
title: Developing ejabberd with VSCode
menu: VSCode
order: 230
toc: true
---

The ejabberd git repository includes basic configuration and a few
scripts to get started with VSCode.

There are several VSCode alternatives for ejabberd development:

* [Visual Studio Code](#visual-studio-code) desktop app –
  local development with no dependencies
* [VSCodium](#vscodium) desktop app –
  local development installing dependencies
* [Coder's code-server](#coders-code-server) container image –
  local or remote development
* [GitHub Codespaces](#github-codespaces) service –
  quick and short remote development


Visual Studio Code
------------------

The official [Visual Studio Code](https://code.visualstudio.com/)
installers provided by Microsoft can use the official marketplace.
That allows to install the Dev Container extension
to compile and run ejabberd inside a prepared container,
which includes Erlang/OTP and all the required libraries,
so you don't need to install them in your machine.

However that installer is licensed under a not-FLOSS license
and contains telemetry/tracking.

Once installed: install Git as suggested,
clone the ejabberd git repository locally,
let it install the Dev Container extension,
then let it reopen the path inside the devcontainer.


VSCodium
--------

[VSCodium](https://github.com/VSCodium/vscodium)
provides Free/Libre Open Source Software Binaries of VSCode.
This is a great alternative to the official VSCode installer.

However, it can't use the official marketplace,
uses instead the open-vsx.com marketplace,
and the Dev Containers extension is not available.
This means that you must install the ejabberd dependencies
in your system to compile and debug ejabberd.

Once installed: open your local ejabberd git clone.
It's highly recommended to go the EXTENSIONS tab
and install the [Erlang LS extension](https://github.com/erlang-ls/vscode).


Coder's code-server
-------------------

An easy, zero-cost, way to use VSCode in a web browser is through
the ejabberd's code-server container image.
This image is based in the [Debian docker image](https://hub.docker.com/_/debian)
and includes [Coder's code-server](https://github.com/coder/code-server),
Erlang/OTP, Elixir, and all the required libraries.

Download and start the container,
and provide as volume the path of your local ejabberd git clone:
```
docker run \
    --name coder \
    -it \
    -p 5208:5208 \
    -v $(pwd)/ejabberd:/workspaces/ejabberd \
    ghcr.io/processone/code-server
```
Now open in your web browser: `http://0.0.0.0:5208/`

The next time it can be started with `docker start -i coder`


GitHub Codespaces
-----------------

The ejabberd git repository contains default configuration to use it in the
GitHub Codespaces service.

This can be used remotely over a web browser, no need to install anything.
Notice this is a service that can be used for free several hours each month,
and later requires a subscription.

To start using it:

1. Go to https://github.com/codespaces
2. Click "New codespace"
3. Select ejabberd repository, desired branch, click "Create codespace"


Basic Usage
-----------

Once you have VSCode running and ejabberd git repository opened,
open some erlang file, so Erlang LS extension gets started,
and now you can go to RUN and run ejabberd for the first time.
The first time it will take some time to compile, be patient.

Now you can:

- In RUN click `▷ Relive` to compile and start ejabberd
- In EXPLORER open any source code, and add a breakpoint
- In TERMINAL you can call: `ejabberdctl register admin localhost somepass`
- In PORTS you can view the addresses you can use to connect to the running ejabberd

The ejabberd configuration file is in `_build/relive/conf/ejabberd.yml`.

You can connect to ejabberd using a XMPP client using HTTPS BOSH or WS on port 5443.
Webadmin is on port 5280, if it complains 404, add `admin/` to the URL.



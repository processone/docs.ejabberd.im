---
title: ejabberd for Developers
menu: For Developers
order: 30
toc: true
---

As a developer, you can customize ejabberd to design almost every type
of XMPP related type of solutions.

As a starting point, we recommend that you get extremely familiar with
both the core XMPP protocol itself and its extensions.

From that, once you understand well XMPP, you can tame ejabberd to
build your dream messaging system.

# Getting started

## Source code

ejabberd source is available on Github:
[ejabberd](https://github.com/processone/ejabberd)

You will need to get familiar with it to start learning about ejabberd
module writing. The first place to start? You should read the
[time module](https://github.com/processone/ejabberd/blob/master/src/mod_time.erl). This
is one of the simplest possible module for ejabberd.

Another great source of inspiration and knowledge is to read the
source code of the many contributed ejabberd modules. Many of them are
available from
[ejabberd-contribs](https://github.com/processone/ejabberd-contrib)
repository.

For a complete overview of ejabberd source code and its dependencies, please refer to [ejabberd and related repositories](repositories/)

## Development Environment

The first step to develop for ejabberd is to install and
configure your development environment:

* Check the [Source Code Installation](/admin/installation/#source-code) section
* If using Emacs, install [erlang-mode](https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html) in your operating system
* If using OSX, check the [OSX development environment](install-osx/) section
* For Visual Studio Code and alternatives, check the [Developing ejabberd with VSCode](vscode/) section

# Customizing ejabberd

* [ejabberd development guide](guide/)
* [ejabberd modules development](extending-ejabberd/modules/)


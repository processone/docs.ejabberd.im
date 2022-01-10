---
title: ejabberd for Developers
menu: For Developers
order: 30
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

## Development environment

One of the first step to develop for ejabberd is to install /
configure your development environment:

* [Installing ejabberd development environment on OSX](install-osx/)
* [Use Ansible to auto provision a development environment in a VM](https://github.com/processone/ejabberd-vagrant-dev):
  The main benefit of that environment is that it is preconfigured
  with all the database backend supported by ejabberd. You can both
  start developing immediately for any backend, but also run the full
  test suite to make sure you did not break any important feature.

# Customizing ejabberd

* [ejabberd development guide](guide/)
* [ejabberd modules development](extending-ejabberd/modules/)


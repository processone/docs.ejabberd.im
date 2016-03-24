---
title: ejabberd Test Suites
---

# ejabberd Test Suites

ejabberd comes with comprehensive test suite to cover various part of
the software platform.

## XMPP end-to-end protocol test suite

This test suite is acting like XMPP clients and is testing ejabberd at
the protocol level.

This test suite is used for end-to-end testing, and also test the
various ejabberd backends.

The test suite is modular and can be run in part, to focus on a group
of features or a specific backend.

The environment variable CT_BACKEND can be use to define what backend
are available.

Possible CT_BACKENDS are:

- mnesia. Technically, Mnesia is included in ejabberd and should
  always be available.
- redis
- mysql
- pgsql
- sqlite: SQLite is a library and is available if you have enabled it
  in ejabberd at compile time.
- ldap
- extauth
- riak

**Note:** We assume that you have also build ejabberd with proper
backend support. Please, check `configure` build options to make sure
you have build ejabberd with adequate backend support.

Other options you can use to limit the test to run is to passe a list
of groups to test. Possible high level groups to run are:

- no_db: Runs subgroups `generic` and `test_proxy65`.
- ldap
- mnesia
- redis
- mysql
- pgsql
- sqlite
- extauth
- riak

Usually, it is enough to just limit tests with CT_BACKENDS and let the
test suite decide to run relevant tests, but you may for example want
to focus only on a specific backend, skipping the generic `no_db`
tests.

Example commands to run the XMPP end-to-end test suite are:

~~~ bash
CT_BACKENDS=riak,mnesia rebar ct suites=ejabberd
CT_BACKENDS=mnesia rebar ct suites=ejabberd groups=mnesia
CT_BACKENDS=mnesia rebar ct suites=ejabberd groups=generic
~~~

If you have every backend configured, you can run all tests with:

~~~ bash
make test
~~~

## Unit test

We have another level of test available that focus on unit
testing. Those tests are written in Elixir. Each module is tested
independantly with tests that do not require a full ejabberd to
run. Only the needed service are started.

To be able to run those test you need to build ejabberd with Elixir and development tools support. This can be done at build time with following configure command:

~~~ bash
./configure --enable-elixir --enable-tools && make
~~~

When ejabberd is built You can run quick tests with the command:

~~~ bash
make quicktest
~~~

## Dependency tests

ejabberd depends on a lot of dependent module. Each dependency can be
tested independantly, by checking them out and running their own test
suite directly.

## Test status

We are running test for ejabberd and dependencies automatically with
Travis-CI. We have a Dashboard available on Github to check the
overall test status for all our projects:
[ProcessOne Github Dashboard](http://processone.github.io)

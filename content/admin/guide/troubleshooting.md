---
title: Troubleshooting ejabberd
toc: true
menu: Troubleshooting
---

# Log Files

An `ejabberd` node writes three log files:

**`ejabberd.log`**:   is the ejabberd service log, with the messages reported by
	`ejabberd` code

**`error.log`**:   is the file accumulating error messages from `ejabberd.log`

**`crash.log`**:   is the Erlang/OTP log, with the crash messages reported by
	Erlang/OTP using SASL (System Architecture Support Libraries)

The option `loglevel` modifies the verbosity of the file ejabberd.log.
The syntax:

**`loglevel: Level`**:   The standard form to set a global log level.

The possible `Level` are:

* **`0`**:   No ejabberd log at all (not recommended)

* **`1`**:   Critical

* **`2`**:   Error

* **`3`**:   Warning

* **`4`**:   Info

* **`5`**:   Debug

For example, the default configuration is:

	loglevel: 4

By default `ejabberd` rotates the log files when they get grown above a
certain size. The exact value is controlled by `log_rotate_size` option.
The syntax is:

**`log_rotate_size: N`**:   Where N is the maximum size of a log file in bytes. The default
	value is 10485760 (10Mb).

However, you can rotate the log files manually. You can
either use an external tool for log rotation and the ejabberdctl command
`reopen-log` to reopen the log files, or the ejabberdctl command
`rotate-log` to perform both steps (please refer to section [ejabberd
Commands](/admin/guide/managing/#ejabberd-commands)).

The option `log_rotate_count` defines the number of rotated files to
keep by `reopen-log` command. Every such file has a numeric suffix. The
exact format is:

**`log_rotate_count: N`**:   The default value is 1, which means only `ejabberd.log.0`,
	`error.log.0` and `crash.log.0` will be kept.

# Debug Console

The Debug Console is an Erlang shell attached to an already running
`ejabberd` server. With this Erlang shell, an experienced administrator
can perform complex tasks.

This shell gives complete control over the `ejabberd` server, so it is
important to use it with extremely care. There are some simple and safe
examples in the article
[`Interconnecting Erlang Nodes`][1]

To exit the shell, close the window or press the keys: control+c
control+c.

# Too many db tables

When running ejabberd, the log shows this error:

    ** Too many db tables **

The number of concurrent ETS and Mnesia tables is limited. If this
error occurs, it means that you have reached this limit.

For a solution, please read the
[section about ERL_MAX_ETS_TABLES on the Performance Tuning page](https://ejabberd.im/tuning#erl_max_ets_tables).

[1]:	https://ejabberd.im/interconnect-erl-nodes

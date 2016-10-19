---
title: Troubleshooting ejabberd
toc: true
menu: Troubleshooting
order: 110
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

Option `log_rate_limit` is useful if you want to protect the logging
mechanism from being overloaded by excessive amount of log messages. The
syntax is:

**`log_rate_limit: N`**:   Where N is a maximum number of log messages per second. The default
	value is 100.

When the limit is reached the similar warning message is logged:

	lager_error_logger_h dropped 800 messages in the last second that exceeded the limit of 100 messages/sec

By default `ejabberd` rotates the log files when they get grown above a
certain size. The exact value is controlled by `log_rotate_size` option.
The syntax is:

**`log_rotate_size: N`**:   Where N is the maximum size of a log file in bytes. The default
	value is 10485760 (10Mb).

`ejabberd` can also rotates the log files at given date interval. The
exact value is controlled by `log_rotate_date` option. The syntax is:

**`log_rotate_date: D`**:   Where D is a string with syntax is taken from the syntax newsyslog
	uses in newsyslog.conf. The default value is `` (no rotation
	triggered by date).

However, you can rotate the log files manually. For doing this, set
`log_rotate_size` option to 0 and `log_rotate_date` to empty list, then,
when you need to rotate the files, rename and then reopen them. You can
either use an external tool for log rotation and the ejabberdctl command
`reopen-log` to reopen the log files, or the ejabberdctl command
`rotate-log` to perform both steps (please refer to section [ejabberd
Commands](../managing/#ejabberd-commands)).

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

# Watchdog Alerts

`ejabberd` includes a watchdog mechanism that may be useful to
developers when troubleshooting a problem related to memory usage. If a
process in the `ejabberd` server consumes more memory than the
configured threshold, a message is sent to the XMPP accounts defined
with the option `watchdog_admins` in the `ejabberd` configuration file.

The syntax is:

**`watchdog_admins: [JID, ...]`**:  

The memory consumed is measured in `words`: a word on 32-bit
architecture is 4 bytes, and a word on 64-bit architecture is 8 bytes.
The threshold by default is 1000000 words. This value can be configured
with the option `watchdog_large_heap`, or in a conversation with the
watchdog alert bot.

The syntax is:

**`watchdog_large_heap: Number`**

Example configuration:

	#!yaml
	watchdog_admins:
	  - "admin2@localhost"
	  - "admin2@example.org"
	watchdog_large_heap: 30000000

To remove watchdog admins, remove them in the option. To remove all
watchdog admins, set the option with an empty list:

	#!yaml
	watchdog_admins: []

[1]:	http://www.ejabberd.im/interconnect-erl-nodes

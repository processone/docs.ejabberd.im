# Upgrade to ejabberd 23.01

There is a new module, new hooks, new options, and some option accepts additional values,
but there are no breaking changes in SQL schemas, configuration, or commands API.

Please check the ejabberd [23.01](../../archive/23.01/index.md) release announcement for details
about the improvements.

## Changes in option `outgoing_s2s_families`

The `outgoing_s2s_families` top-level option specifies which address families to try, in what order.

The default value has now been changed to try IPv6 first, as servers are within data centers where IPv6 is more commonly enabled (contrary to clients). And if it’s not present, then it’ll just fall back to IPv4.

By the way, this option is obsolete and irrelevant when using ejabberd 23.01 and Erlang/OTP 22, or newer versions of them.


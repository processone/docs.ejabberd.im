---
title: API Permissions
toc: true
menu: API Permissions
order: 20
---

*This page describes the new flexible permission mechanism introduced in
ejabberd 16.11. This version will be released at end of November 2016, but
development can already be tried by installing ejabberd master branch from
source.*

# Configuring permissions to API endpoints

Access to all available endpoints are configured using `api_permissions` option.

It allows to define multiple groups, each one with separate list of filters
on who and what are allowed by rules specified inside it.

Basic rule may looks like this:

``` yaml
api_permissions:
  "admin access":
    - who:
      - admin
    - what
      - "*"
      - "!stop"
```

It tells that group named `Admin access` allows all users that are accepted by
ACL rule `admin` to execute all commands except command `stop`.

Each group has associated name (that is just used in log messages), `who` section
for rules that authentication details must match, `what` section for specifying
list of command, and `from` with list of modules that API was delivered to.

## Rules inside `who` section

There are 3 types of rules that can be placed in `who` section:

`- acl: Name|ACLDefinition` or shortcut version `- Name|ACLRule`
: It accepts command when authentication used to execute command matches
  rules of `Name` ACL (or inline rules from `ACLDefinition` or `ACLRule`)

`- access: Name|AccessDefinition`
: This will allow execution if access `Name` or inline `AccessDefinition`
  will return `allowed` for command's authentication details

`- oauth: ListOfRules`
: This rule (and only this) will match for commands that were executed
  with OAuth authentication. Inside ListOfRules you can use any rule
  described above (`- acl: Name`, `- AClName`, `- access: Name`) and
  additionally you must include `-scope: ListOfScopeNames` with OAuth
  scope names that must match scope used to generate OAuth token used
  in command authentication.

`who` section will allow command to be executed if at least one rule
included inside it will be successfully matched, if you need to have
ensure that multiple rules must be matched at this same time, you
need to use `- access` rule for that.

Missing `who` rule is equivalent to `- who: none` which will stop group
from accepting any command.

### Examples of `who` rules

``` yaml
- who:
  - user: "admin@server.com"
  - ip: "127.0.0.1/8"
```

This will accept user `admin@server.com` or commands originating
from localhost

``` yaml
- who:
  - access:
    - allow:
      - user: "admin@server.com"
      - ip: "127.0.0.1/8"
```

This will allow execution for commands from `admin@server.com` and
localhost address

``` yaml
- who:
  - muc_admin
  - oauth
    - scope: "can_muc_admin"
    - muc_admin
```

Those rules will match for users from `muc_admin` ACL both using regular
authentication and OAuth (but only for tokens created with can_muc_admin scope)

## Rules in `what` section

Rules in `what` section are constructed from `"strings"` literals, you can
use name of existing commands like `"register"` or `"connected_users"`, you
can also use wildcard `"*"` rule to match all commands, or you can use rule
`"[tag:roster]"` to allow all commands that have been declared with tag `roster`

Additionally each rule can be prepended with `!` character to change
it into negative assertion rule. Command names that would match what is
after `!` character will be removed from list of allowed commands.

Missing `what` rule is equivalent to `- what: "!*"` which will stop group
from accepting any command.

### Example of `what` rules

``` yaml
- what:
  - "*"
  - "!stop"
```

This will allow execution of all command except command `stop`

``` yaml
- what:
  - "status"
  - "[tag:account]"
```

This will allow execution of `status` and commands with tag `session`
(like `num_resources` or `status_list`)

``` yaml
- what:
  - "start"
  - "!*"
```

This will match no command.

## Rules in `from` section

This section allows to specify list of allowed module names that expose API
to outside world. Currently those modules are `ejabberd_xmlrpc`, `mod_http_api`
and `ejabberd_ctl`.

If `from` section is missing from group then all endpoints are accepted,
if it's specified endpoint must be listed inside it to be allowed to execute.


## Examples

``` yaml
api_permissions:
  "console commands":
    from:
      - ejabberd_ctl
    who: all
    what: "*"
  "admin access":
    who:
      - admin
      - oauth:
        - scope: "ejabberd:admin"
        - admin
    what:
      - "*"
      - "!stop"
      - "!start"
```

Rules include in this will allow execution of any command invoked
by`ejabberdctl` shell command, or all command except `start` and `stop`
for users in ACL admin, with regular authentication or `ejabberd:admin`
scoped OAuth tokens.

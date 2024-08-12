# API Permissions

<!-- md:version added in 16.12 -->

This page describes ejabberd's flexible permission mechanism.

Access to all available endpoints are configured using
the [`api_permissions`](../../admin/configuration/toplevel.md#api_permissions) top-level option.

It allows to define multiple groups, each one with separate list of filters on who and what are allowed by rules specified inside it.

Basic rule looks like this:

``` yaml
api_permissions:
  "admin access":
    who:
      - admin
    what:
      - "*"
      - "!stop"
    from:
      - ejabberd_ctl
      - mod_http_api
```

It tells that group named `Admin access` allows all users that are accepted by ACL rule `admin` to execute all commands except command `stop`, using the command-line tool `ejabberdctl` or sending a ReST query handled by
[mod_http_api](../../admin/configuration/modules.md#mod_http_api).

Each group has associated name (that is just used in log messages), `who` section for rules that authentication details must match, `what` section for specifying list of command, and `from` with list of modules that API was delivered to.

## Rules inside `who` section

There are 3 types of rules that can be placed in `who` section:

- **acl:** *Name | ACLDefinition*  
  or the short version:  
  *Name | ACLRule*  
  This accepts a command when the authentication provided matches
  rules of `Name`
  [Access Control List](../../admin/configuration/basic.md#acl)
  (or inline rules from `ACLDefinition` or `ACLRule`)

- **access:** *Name | AccessDefinition*  
  This allows execution if
  the [Access Rule](../../admin/configuration/basic.md#access-rules)
  `Name` or inline `AccessDefinition`
  returns `allowed` for command's authentication details

- **oauth:** *ListOfRules*  
  This rule (and only this) will match for commands that were executed
  with [OAuth](oauth.md) authentication.
  Inside ListOfRules you can use any rule
  described above (`acl: Name`, `AClName`, `access: Name`) and
  additionally you must include `scope: ListOfScopeNames` with OAuth
  scope names that must match scope used to generate OAuth token used
  in command authentication.

`who` allows the command to be executed if at least one rule matches.

If you want to require several rules to match at this same time, use `access` (see examples below).

Missing `who` rule is equivalent to `who: none` which will stop group from accepting any command.

### Examples of `who` rules

This accepts user `admin@server.com` or commands originating from localhost:

``` yaml
who:
  user: admin@server.com
  ip: 127.0.0.1/8
```

This only allows execution of a command if it's invoked by user `admin@server.com` and comes from localhost address.
If one of those restrictions isn't satisfied, execution will fail:

``` yaml
who:
  access:
    allow:
      user: admin@server.com
      ip: 127.0.0.1/8
```

Those rules match for users from `muc_admin` ACL both using regular authentication and OAuth:

``` yaml
who:
  access:
    allow:
      acl: muc_admin
  oauth:
    scope: "ejabberd:admin"
    access:
      allow:
        acl: muc_admin
```

## Rules in `what` section

Rules in `what` section are constructed from `"strings"` literals. You can use:

- *"command_name"* of an existing [API command](admin-api.md)
- *command_name* is same as before, but no need to provide `"`
- **"*"** is a wildcard rule to match all commands
- **"[tag:** *tagname* **]"** allows all commands that have been declared with tag `tagname`.
  You can consult the list of tags and their commands with: `ejabberdctl help tags`

Additionally each rule can be prepended with `!` character to change it into negative assertion rule. Command names that would match what is after `!` character will be removed from list of allowed commands.

Missing `what` rule is equivalent to `what: "!*"` which will stop group from accepting any command.

### Example of `what` rules

This allows execution of all commands except command `stop`:

``` yaml
what:
  - "*"
  - "!stop"
```

This allows execution of `status` and commands with tag `session`
(like `num_resources` or `status_list`):

``` yaml
what:
  - status
  - "[tag:account]"
```

This matches no command:

``` yaml
what:
  - start
  - "!*"
```

## Rules in `from` section

This section allows to specify list of allowed module names that expose API to outside world. Currently those modules are `ejabberd_xmlrpc`, `mod_http_api` and `ejabberd_ctl`.

If `from` section is missing from group then all endpoints are accepted, if it's specified endpoint must be listed inside it to be allowed to execute.

## Examples

Those rules allow execution of any command invoked by `ejabberdctl` shell command, or all command except `start` and `stop` for users in ACL admin, with regular authentication or `ejabberd:admin` scoped OAuth tokens.

``` yaml
api_permissions:
  "console commands":
    from:
      - ejabberd_ctl
    who: all
    what: "*"
  "admin access":
    who:
      access:
        allow:
          - acl: admin
      oauth:
        scope: "ejabberd:admin"
        access:
          allow:
            - acl: admin
    what:
      - "*"
      - "!stop"
      - "!start"
```

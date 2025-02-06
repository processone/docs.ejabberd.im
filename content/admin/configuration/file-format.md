# File format

## Yaml File Format

ejabberd loads its configuration file during startup.
This configuration file is written in
[`YAML`](https://en.wikipedia.org/wiki/YAML) format,
and its file name MUST have “.yml” or “.yaml” extension.
This helps ejabberd to differentiate between this new format
and the [legacy configuration file](#legacy-configuration-file) format.

Please, consult `ejabberd.log` for configuration errors. ejabberd will
report syntax related errors, as well as complains about unknown options
and invalid values. Make sure you respect indentation (YAML is
sensitive to this) or you will get pretty cryptic errors.

Note that ejabberd never edits the configuration file. If you are
changing parameters at runtime from web admin interface, you will need to apply
them to configuration file manually. This is to prevent messing up
with your config file comments, syntax, etc.

## Reload at Runtime

You can modify the ejabberd configuration file
and reload it at runtime:
the changes you made are applied immediately,
no need to restart ejabberd.
This applies to adding, changing or removing
vhosts, listened ports, modules, ACLs or any other options.

How to do this?

1. Let's assume your ejabberd server is already running
2. Modify the configuration file
3. Run the [reload_config](../../developer/ejabberd-api/admin-api.md#reload_config) command
4. ejabberd will read that file, check its YAML syntax is valid,
   check the options are valid and known...
5. If there's any problem in the configuration file,
   the reload is aborted and an error message is logged with details,
   so you can fix the problem.
6. If the file is right, it detects the changed options,
   and applies them immediately (add/remove hosts, add/remove modules, ...)

## Legacy Configuration File

In previous ejabberd version the configuration file should be
written in Erlang terms. The format is still supported, but it is
highly recommended to convert it to the new YAML format with the
[convert_to_yaml](../../developer/ejabberd-api/admin-api.md#convert_to_yaml)
API command using [ejabberdctl](../guide/managing.md#ejabberdctl).

If you want to specify some options using the old Erlang format, you
can set them in an additional cfg file, and include it using the
`include_config_file` option, see
[Include Additional Files](#include-additional-files).

## Include Additional Files

The option [include_config_file](toplevel.md#include_config_file)
 in a configuration file instructs
ejabberd to include other configuration files immediately.

This is a basic example:

``` yaml
include_config_file: /etc/ejabberd/additional.yml
```

In this example, the included file is not allowed to contain a `listen`
option. If such an option is present, the option will not be accepted.
The file is in a subdirectory from where the main configuration file is.

``` yaml
include_config_file:
  ./example.org/additional_not_listen.yml:
    disallow: [listen]
```

Please notice that options already defined in the main configuration file
cannot be redefined in the included configuration files.
But you can use [host_config](toplevel.md#host_config)
and [append_host_config](toplevel.md#append_host_config)
as usual (see [Virtual Hosting](basic.md#virtual-hosting)).

In this example, `ejabberd.yml` defines some ACL for the whole ejabberd server, and later includes another file:

``` yaml
acl:
  admin:
    user:
      - admin@localhost
include_config_file:
  /etc/ejabberd/acl.yml
```

The file `acl.yml` can add additional administrators to one of the virtual hosts:

``` yaml
append_host_config:
  localhost:
    acl:
      admin:
        user:
          - bob@localhost
          - jan@localhost
```

## Macros and Keywords

<!-- md:version improved in [25.03](../../archive/25.03/index.md) -->

In the ejabberd configuration file,
you can define a macro or keyword for a value (atom, integer, string...)
and later use it when configuring an ejabberd option.

**Macros** is a feature implemented internally
by the [yconf](https://github.com/processone/yconf) library
and are replaced early and transparently to ejabberd.
However, macros [cannot](#macro-and-host_config) be defined inside `host_config`.

**Keywords** is a feature similar to macros,
implemented by ejabberd itself,
and are replaced after macro replacement.
Keywords can be defined inside `host_config` for module options, but not for toplevel options.
Keywords cannot be used in those toplevel options:
[hosts](toplevel.md#hosts), [loglevel](toplevel.md#loglevel), [version](toplevel.md#version).

First [define_macro](toplevel.md#define_macro) and [define_keyword](toplevel.md#define_keyword)
and then use them like this:

```yaml
define_macro:
  NAME1: value1

define_keyword:
  NAME2: "value2"

some_option1: NAME1
other_option1: "I am @NAME1@"

some_option2: NAME2
other_option2: "I am @NAME2@"
```

where:

- **NAME**: should be specified in capital letters for convenience.
  Duplicated macro/keyword names are not allowed.
  If a macro is defined with the same name than a keyword, the macro is used.

- **value**: for all options, the value can be any valid YAML element.
  It is also possible to use as value the name of another macro.

- **use** a macro/keyword when configuring the option:
  simply set `NAME` instead of option value.
  Macros are processed after additional configuration files have been
  included, so it is possible to use macros that are defined in
  configuration files included before the usage.

- **use inside a string**: surround its name with `@` characters

Let's see examples of all this in detail:

### Atom

``` yaml
define_macro:
  ANON: both

define_keyword:
  TLS: optional

anonymous_protocol: ANON
s2s_use_starttls: TLS
```

The resulting configuration is:

``` yaml
anonymous_protocol: both
s2s_use_starttls: optional
```

### Integer

``` yaml
define_macro:
  LOG_LEVEL_NUMBER: 5
  NUMBER_PORT_C2S: 5222

define_keyword:
  NUMBER_PORT_HTTP: 5280

loglevel: LOG_LEVEL_NUMBER

listen:
  -
    port: NUMBER_PORT_C2S
    module: ejabberd_c2s
  -
    port: NUMBER_PORT_HTTP
    module: ejabberd_http
```

The resulting configuration is:

``` yaml
loglevel: 5

listen:
  -
    port: 5222
    module: ejabberd_c2s
  -
    port: 5280
    module: ejabberd_http
```

### Map

Option values can be any arbitrary YAML value:

``` yaml
define_macro:
  USERBOB:
    user:
      - bob@localhost

define_keyword:
  USERJAN:
    user:
      - jan@localhost

acl:
  admin: USERBOB
  moderator: USERJAN
```

The resulting configuration is:

``` yaml
acl:
  admin:
    user:
      - bob@localhost
  moderator:
    user:
      - jan@localhost
```

### String

``` yaml
define_macro:
  NAME: "MUC Service"
  PERSISTENT: true

define_keyword:
  TITLE: "Example Room"

modules:
  mod_muc:
    name: NAME
    default_room_options:
      persistent: true
      title: TITLE
```

The resulting configuration is:

``` yaml
modules:
  mod_muc:
    name: "MUC Service"
    default_room_options:
      persistent: PERSISTENT
      title: "Example Room"
```

### Inside string

A macro or keyword can be used inside an option string:

``` yaml
define_keyword:
  CMD: "captcha"

captcha_cmd: "tools/@CMD@.sh"
```

is equivalent to:

``` yaml
define_keyword:
  CMD: "tools/captcha.sh"

captcha_cmd: "@CMD@"
```

is equivalent to:

``` yaml
define_keyword:
  CMD: "tools/captcha.sh"

captcha_cmd: CMD
```

The resulting configuration in all the cases is:

``` yaml
captcha_cmd: tools/captcha.sh
```

### Macro over keyword

If a macro and a keyword are defined with the same name,
the macro definition takes precedence and the keyword definition is ignored:

``` yaml
define_macro:
  LANGUAGE: "bg"

define_keyword:
  LANGUAGE: "pt"

language: LANGUAGE
```

The resulting configuration is:

``` yaml
language: "bg"
```

### Keyword inside macro

A macro definition can use a keyword:

``` yaml
define_macro:
  MACRO: "tools/@KEYWORD@"

define_keyword:
  KEYWORD: "captcha.sh"

captcha_cmd: MACRO
```

The resulting configuration is:

``` yaml
captcha_cmd: "tools/captcha.sh"
```

### Predefined keywords

Several keywords are predefined automatically by ejabberd,
so you can use them without need to define them explicitly:

- **HOST**: the virtual [host name](basic.md#host-names), for example `"example.org"`.
  That keyword is only predefined for module options, not toplevel options.
- **HOME**: the home directory of the user running ejabberd, for example `"/home/ejabberd"`
- **VERSION**: ejabberd version number in `XX.YY` format, for example `"24.05"`
- **SEMVER**: ejabberd version number in [semver format](https://hexdocs.pm/elixir/1.18.2/Version.html) when compiled with Elixir’s mix (`"24.5"`), otherwise it's in `XX.YY` format (`"24.05"`)

It is possible to overwrite predefined keywords,
global or for a vhost like in this example:

```yaml
host_config:
  localhost:
    define_keyword:
      VERSION: "1.2.3"

ext_api_url: "http://localhost/@VERSION@/api"
```

The resulting behaviour is equivalent to a configuration like:

``` yaml
host_config:
  localhost:
    ext_api_url: "http://localhost/1.2.3/api"

ext_api_url: "http://localhost/25.03/api"
```

### Macro and host_config

Macros can be **used** inside [host_config](toplevel.md#host_config):

``` yaml
define_macro:
  MYSQL_PORT: 1234
  PGSQL_PORT: 4567

host_config:
  mysql.localhost:
    sql_port: MYSQL_PORT
  pgsql.localhost:
    sql_port: MYSQL_PORT
```

The resulting configuration is:

``` yaml
host_config:
  mysql.localhost:
    sql_port: 1234
  pgsql.localhost:
    sql_port: 4567
```

!!! warning "Don't use macro defined in host_config"

    Macros can not be **defined** inside host_config.
    Use the previous method instead.
    That problematic macro is not replaced:

    ``` yaml
    host_config:
      mysql.localhost:
        define_macro:
          SQL_PORT: 1234
      pgsql.localhost:
        define_macro:
          SQL_PORT: 4567

    sql_port: SQL_PORT

    # [critical] Failed to start ejabberd application:
    #   Invalid value of option sql_port:
    #   Expected integer, got string instead
    ```

### Keyword and host_config

Keywords can be **used** and **defined** inside [host_config](toplevel.md#host_config):

``` yaml
hosts:
 - localhost
 - example.org

define_keyword:
  HOSTNAME: "Generic Name"

host_config:
  example.org:
    define_keyword:
      HOSTNAME: "Example Host"

modules:
  mod_vcard:
    name: "vJUD of @HOSTNAME@"
```

The resulting configuration is:

``` yaml
host_config:
  localhost:
    modules:
      mod_vcard:
        name: "vJUD of Generic Name"
  example.org:
    modules:
      mod_vcard:
        name: "vJUD of Example Host"
```

!!! warning "Don't use in toplevel a keyword defined in host_config"

    Keywords can be defined inside `host_config`,
    but only if they are being used in module options,
    not in toplevel options.
    That problematic keyword is not replaced:

    ``` yaml
    host_config:
      mysql.localhost:
        define_keyword:
          SQL_PORT: 1234
      pgsql.localhost:
        define_keyword:
          SQL_PORT: 4567

    sql_port: SQL_PORT

    # [critical] Failed to start ejabberd application:
    #   Invalid value of option sql_port:
    #   Expected integer, got string instead
    ```


# Database Configuration

ejabberd uses its internal Mnesia database by default. However, it is
possible to use a relational database, key-value storage or an LDAP
server to store persistent, long-living data.

ejabberd is very
flexible: you can configure different authentication methods for
different virtual hosts, you can configure different authentication
mechanisms for the same virtual host (fallback), you can set different
storage systems for modules, and so forth.

## Supported storages

The following databases are supported by ejabberd:

-   [`Mnesia`](https://www.erlang.org/doc/apps/mnesia/). Used by default, nothing to setup to start using it

-   [`MySQL`](https://www.mysql.com/). Check the tutorial [Using ejabberd with MySQL](../../tutorials/mysql.md)

-   [`PostgreSQL`](https://www.postgresql.org/)

-   [`MS SQL Server/SQL Azure`](https://www.microsoft.com/sql-server).
    Check the [Microsoft SQL Server](#microsoft-sql-server) section

-   [`SQLite`](https://sqlite.org/)

-   Any [`ODBC`](https://en.wikipedia.org/wiki/Open_Database_Connectivity) compatible database

-   [`Redis`](https://redis.io/)(only for transient data).
    Check the [Redis](#redis) section

- `LDAP` is documented in the [LDAP](ldap.md) section

## Virtual Hosting

If you define several [host names](basic.md#host-names)
in the `ejabberd.yml` configuration file,
probably you want that each
virtual host uses a different configuration of database, authentication
and storage, so that usernames do not conflict and mix between different
virtual hosts.

For that purpose, the options described in the next
sections must be set inside the [host_config](toplevel.md#host_config) top-level option
for [each virtual host](basic.md#virtual-hosting)).

For example:

``` yaml
host_config:
  public.example.org:
    sql_type: pgsql
    sql_server: localhost
    sql_database: database-public-example-org
    sql_username: ejabberd
    sql_password: password
    auth_method: [sql]
```

## Default database

You can simplify your configuration by setting the default database
with the [`default_db`](toplevel.md#default_db) top-level option:

- it sets the default authentication method when the
  [`auth_method`](toplevel.md#auth_method) top-level option is not configured
- it defines the database to use in ejabberd modules that support the `db_type` option,
  when that option is not configured.

## Database Schema

<!-- md:version updated in [24.06](../../archive/24.06/index.md) -->

def:update
: Modify the database schema and all its tables to match the installed ejabberd version.
  Not to be confused with [upgrade ejabberd](def:upgrade) or [switch schema](def:switch).

The [update_sql_schema](toplevel.md#update_sql_schema) top-level option
allows ejabberd to create and update the tables automatically in the SQL database
when using MySQL, PostgreSQL or SQLite.
That option was added in ejabberd [23.10](../../archive/23.10/index.md),
and enabled by default in [24.06](../../archive/24.06/index.md).
If you can use that feature:

1. Create the database in your SQL server
2. Create an account in the SQL server and grant it rights in the database
3. Configure in ejabberd the [SQL Options](#sql-options) that allow it to connect
4. Start ejabberd ...
5. and it will take care to create the tables (or update them if they exist from a previous ejabberd version)

If that option is disabled, or you are using a different SQL database,
or an older ejabberd release,
then you must create the tables in the database manually before starting ejabberd.
The SQL database schema files are available:

- If installing ejabberd from sources, sql files are
  in the installation directory. By default: `/usr/local/lib/ejabberd/priv/sql`

- If installing ejabberd from Process-One installer, sql files are
  in the ejabberd's installation path under `<base>/lib/ejabberd*/priv/sql`

See [ejabberd SQL Database Schema](../../developer/sql-schema.md)
for details on database schemas.

## _Default_ and _New_ Schemas

If using MySQL, PostgreSQL, Microsoft SQL or SQLite,
you can choose between two database schemas:

- the _default_ schema is preferable when serving one massive domain,
- the _new_ schema is preferable when serving many small domains.

The _default_ schema stores only one XMPP domain in the database.
The [XMPP domain](basic.md#xmpp-domains)
is not stored as this is the same for all the accounts,
and this saves space in massive deployments.
However, to handle several domains,
you have to setup one database per domain
and configure each one independently
using [host_config](basic.md#virtual-hosting),
so in that case you may prefer the _new_ schema.

The _new_ schema stores the XMPP domain in a new column `server_host`
in the database entries,
so it allows to handle several XMPP domains in a single ejabberd database.
Using this schema is preferable when serving several XMPP domains
and changing domains from time to time.
However, if you have only one massive domain, you may prefer to use the _default_ schema.

To use the _new_ schema, edit the ejabberd configuration file and enable
[new_sql_schema](toplevel.md#new_sql_schema) top-level option:

``` yaml
new_sql_schema: true
```

When creating the tables, if ejabberd can use the [update_sql_schema](toplevel.md#update_sql_schema)
top-level option as explained in the [Database Schema](#database-schema) section,
it will take care to create the tables with the correct schema.

On the other hand, if you are creating the tables manually,
remember to use the proper SQL schema!
For example, if you are using MySQL and choose the _default_ schema, use `mysql.sql`.
If you are using PostgreSQL and need the _new_ schema, use `pg.new.sql`.

def:switch
: Change the database schema and all its tables from `default` schema to `new` schema.
  Not to be confused with [upgrade ejabberd](def:upgrade) or [update schema](def:update).

If you already have a MySQL or PostgreSQL database with the _default_ schema and contents,
you can switch it to the _new_ schema:

* *MySQL*:
Edit the file `sql/mysql.old-to.new.sql` which is included with ejabberd,
fill DEFAULT_HOST in the first line,
and import that SQL file in your database.
Then enable the `new_sql_schema` option in the ejabberd configuration,
and restart ejabberd.

* *PostgreSQL*:
First enable `new_sql_schema` and
[mod_admin_update_sql](modules.md#mod_admin_update_sql)
in your ejabberd configuration:
    ``` yaml
    new_sql_schema: true
    modules:
      mod_admin_update_sql: {}
    ```
then restart ejabberd, and finally execute the
[update_sql](../../developer/ejabberd-api/admin-api.md#update_sql) command:
    ``` sh
    ejabberdctl update_sql
    ```

## SQL Options

The actual database access is defined in the options with `sql_`
prefix. The values are used to define if we want to use ODBC, or one of
the two native interface available, PostgreSQL or MySQL.

To configure SQL there are several top-level options:

- [sql_type](toplevel.md#sql_type)
- [sql_server](toplevel.md#sql_server)
- [sql_port](toplevel.md#sql_port)
- [sql_database](toplevel.md#sql_database)
- [sql_username](toplevel.md#sql_username)
- [sql_password](toplevel.md#sql_password)
- [sql_ssl](toplevel.md#sql_ssl), see section [SQL with SSL connection](#sql-with-ssl-connection)
- [sql_ssl_verify](toplevel.md#sql_ssl_verify)
- [sql_ssl_cafile](toplevel.md#sql_ssl_cafile)
- [sql_ssl_certfile](toplevel.md#sql_ssl_certfile)
- [sql_pool_size](toplevel.md#sql_pool_size)
- [sql_keepalive_interval](toplevel.md#sql_keepalive_interval)
- [sql_odbc_driver](toplevel.md#sql_odbc_driver)
- [sql_start_interval](toplevel.md#sql_start_interval)
- [sql_prepared_statements](toplevel.md#sql_prepared_statements)
- [update_sql_schema](toplevel.md#update_sql_schema), see section [Database Schema](#database-schema)
- [new_sql_schema](toplevel.md#new_sql_schema), see section [Default and New Schemas](#default-and-new-schemas)

Example of plain ODBC connection:

``` yaml
sql_server: "DSN=database;UID=ejabberd;PWD=password"
```

Example of MySQL connection:

``` yaml
sql_type: mysql
sql_server: server.company.com
sql_port: 3306 # the default
sql_database: mydb
sql_username: user1
sql_password: "**********"
sql_pool_size: 5
```

## SQL with SSL Connection

The [sql_ssl](toplevel.md#sql_ssl) top-level option
allows SSL encrypted connections to MySQL, PostgreSQL, and Microsoft SQL servers.

Please notice that ejabberd verifies the certificate presented
by the SQL server against the CA certificate list.
For that reason, if your SQL server uses a self-signed certificate,
you need to setup [sql_ssl_verify](toplevel.md#sql_ssl_verify)
and [sql_ssl_cafile](toplevel.md#sql_ssl_cafile),
for example:

``` yaml
sql_ssl: true
sql_ssl_verify: false
sql_ssl_cafile: "/path/to/sql_server_cacert.pem"
```

This tells ejabberd to ignore problems from not matching any CA certificate
from default list, and instead try to verify using the specified CA certificate.

## SQL Authentication

You can authenticate users against an SQL database, see the option `auth_method`
in the [Authentication](authentication.md) section.

To store the passwords in SCRAM format instead of plaintext,
see the [SCRAM](authentication.md#scram) section.

## SQL Storage

Several ejabberd [modules](modules.md)
have options called `db_type`, and can store their tables
in an SQL database instead of internal.

In this sense, if you defined your database access using the
[SQL Options](#sql-options),
you can configure a module to use your database
by adding the option `db_type: sql` to that module.

Alternatively, if you want all modules to use your SQL database when possible,
you may prefer to set SQL as your [default database](#default-database).

## Microsoft SQL Server

For now, MS SQL is only supported in Unix-like OS'es. You need to have
[`unixODBC`](https://www.unixodbc.org/) installed on your machine, and your Erlang/OTP
must be compiled with ODBC support.
Also, in some cases you need to add machine name to `sql_username`, especially
when you have `sql_server` defined as an IP address, e.g.:

``` yaml
sql_type: mssql
sql_server: 1.2.3.4
sql_username: user1@host
```

By default, ejabberd will use the [`FreeTDS`](https://www.freetds.org/) driver.
You need to have the driver file `libtdsodbc.so` installed in your library PATH
on your system.

If the FreeTDS driver is not installed in a standard location, or if you want
to use another ODBC driver, you can specify the path to the driver using the
[sql_odbc_driver](toplevel.md#sql_odbc_driver) option,
available in ejabberd [20.12](../../archive/20.12/index.md) or later.
For example, if you want to use Microsoft
ODBC Driver 17 for SQL Server:

``` yaml
sql_odbc_driver: "/opt/microsoft/msodbcsql17/lib64/libmsodbcsql-17.3.so.1.1"
```

Note that if you use a Microsoft driver, you may have to use an IP address
instead of a host name for the `sql_server` option.

If hostname (or IP address) is specified in `sql_server` option, ejabberd will
connect using a an ODBC DSN connection string constructed with:

- SERVER=[sql_server](toplevel.md#sql_server)
- DATABASE=[sql_database](toplevel.md#sql_database)
- UID=[sql_username](toplevel.md#sql_username)
- PWD=[sql_password](toplevel.md#sql_password)
- PORT=[sql_port](toplevel.md#sql_port)
- ENCRYPTION=required (only if [sql_ssl](toplevel.md#sql_ssl) is true)
- CLIENT_CHARSET=UTF-8

Since ejabberd [23.04](../../archive/23.04/index.md),
t is possible to use different connection options by
putting a full ODBC connection string in `sql_server` (e.g.
`DSN=database;UID=ejabberd;PWD=password`). The DSN must be configured in
existing system or user odbc.ini file, where it can be configured as desired,
using a driver from system odbcinst.ini. The [sql_odbc_driver](toplevel.md#sql_odbc_driver)
option will have no effect in this case.

If specifying an ODBC connection string, an ODBC connection string must also be
specified for any other hosts using MS SQL DB, otherwise the auto-generated
ODBC configuration will interfere.

## Redis

[`Redis`](https://redis.io/) is an advanced key-value cache and store. You can
use it to store transient data, such as records for C2S (client) sessions.

The available top-level options are:

- [redis_server](toplevel.md#redis_server)
- [redis_port](toplevel.md#redis_port)
- [redis_password](toplevel.md#redis_password)
- [redis_db](toplevel.md#redis_db)
- [redis_connect_timeout](toplevel.md#redis_connect_timeout)

Example configuration:

``` yaml
redis_server: redis.server.com
redis_db: 1
```

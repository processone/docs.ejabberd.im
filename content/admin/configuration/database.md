---
title: Database Configuration
toc: true
menu: Databases
order: 50
---

# Supported storages

`ejabberd` uses its internal Mnesia database by default. However, it is
possible to use a relational database, key-value storage or an LDAP
server to store persistent, long-living data. `ejabberd` is very
flexible: you can configure different authentication methods for
different virtual hosts, you can configure different authentication
mechanisms for the same virtual host (fallback), you can set different
storage systems for modules, and so forth.

The following databases are supported by `ejabberd`:

-   [`Mnesia`](https://erlang.org/doc/apps/mnesia/)

-   [`MySQL`](https://www.mysql.com/). Check the tutorial [Using ejabberd with MySQL](/tutorials/mysql/)

-   Any [`ODBC`](https://en.wikipedia.org/wiki/Open_Database_Connectivity) compatible database

-   [`PostgreSQL`](https://www.postgresql.org/)

-   [`MS SQL Server/SQL Azure`](https://www.microsoft.com/sql-server)

-   [`SQLite`](https://sqlite.org/)

-   [`Redis`](https://redis.io/)(only for transient data)

Please check
[LDAP Configuration](/admin/configuration/ldap/)
section for documentation about using LDAP.

# Database Schema

When using external database backend, ejabberd does not create schema and tables
by itself. If you plan to use MySQL, PostgreSQL, MS SQL or SQLite,
you must create the schema before you run ejabberd.

- If installing ejabberd from sources, you will find sql script for your backend
  in the installation directory. By default: `/usr/local/lib/ejabberd/priv/sql`

- If installing ejabberd from Process-One installer, the init scripts are located
  in the ejabberd's installation path under `<base>/lib/ejabberd*/priv/sql`

If using MySQL or PostgreSQL, you can choose between the
[default or the new schemas](#default-and-new-schemas).

See [ejabberd SQL Database Schema](/developer/sql-schema/)
for details on database schemas.

# Virtual Hosting

Important note about virtual hosting: if you define several domains in
ejabberd.yml (see section [Host Names](/admin/configuration/basic/#host-names)), you probably want that each
virtual host uses a different configuration of database, authentication
and storage, so that usernames do not conflict and mix between different
virtual hosts. For that purpose, the options described in the next
sections must be set inside a [host_config](/admin/configuration/toplevel/#host-config) for each vhost (see section
[Virtual Hosting](/admin/configuration/basic/#virtual-hosting)). For example:


	host_config:
	  public.example.org:
	    sql_type: pgsql
	    sql_server: localhost
	    sql_database: database-public-example-org
	    sql_username: ejabberd
	    sql_password: password
	    auth_method: [sql]

# Default database

You can simplify the configuration by setting the default database.
This can be done with the
[`default_db`](/admin/configuration/toplevel/#default-db)
top-level option:

**`default_db: mnesia|sql`**:  This will define the default database for a module lacking `db_type` option or if `auth_method` option is not set.

# Relational Databases

## Default and New Schemas

There are two database schemas available in ejabberd:
the default schema is preferable when serving one massive domain,
the new schema is preferable when serving many small domains.

The default schema stores only one XMPP domain in the database.
The [XMPP domain](/admin/configuration/basic/#xmpp-domains)
is not stored as this is the same for all the accounts,
and this saves space in massive deployments.
However, to handle several domains,
you have to setup one database per domain
and configure each one independently
using [host_config](/admin/configuration/basic/#virtual-hosting),
so in that case you may prefer the new schema.

The 'new' schema stores the XMPP domain in the database entries,
so it allows to handle several XMPP domains in a single ejabberd database.
Using this schema is preferable when serving several XMPP domains
and changing domains from time to time.
However, if you have only one massive domain, you may prefer to use the default schema.

To use the new schema, edit the ejabberd configuration file and enable
[new_sql_schema](/admin/configuration/toplevel/#new-sql-schema) top-level option:

	new_sql_schema: true

Now, when creating the new database, remember to use the proper SQL schema!
For example, if you are using MySQL and choose the default schema, use `mysql.sql`.
If you are using PostgreSQL and need the new schema, use `pg.new.sql`.

If you already have a MySQL or PostgreSQL database with the default schema and contents,
you can upgrade it to the new schema:

* *MySQL*:
Edit the file `sql/mysql.old-to.new.sql` which is included with ejabberd,
fill DEFAULT_HOST in the first line,
and import that SQL file in your database.
Then enable the `new_sql_schema` option in the ejabberd configuration,
and restart ejabberd.

* *PostgreSQL*:
First enable `new_sql_schema` and
[mod_admin_update_sql](/admin/configuration/modules/#mod-admin-update-sql)
in your ejabberd configuration:

            new_sql_schema: true
            modules:
              mod_admin_update_sql: {}

    then restart ejabberd, and finally execute the
    [update_sql](/developer/ejabberd-api/admin-api/#update-sql) command:

            ejabberdctl update_sql

## SQL Options

The actual database access is defined in the options with `sql_`
prefix. The values are used to define if we want to use ODBC, or one of
the two native interface available, PostgreSQL or MySQL.

To configure SQL there are several top-level options:

- [sql_type](/admin/configuration/toplevel/#sql-type)
- [sql_server](/admin/configuration/toplevel/#sql-server)
- [sql_port](/admin/configuration/toplevel/#sql-port)
- [sql_database](/admin/configuration/toplevel/#sql-database)
- [sql_username](/admin/configuration/toplevel/#sql-username)
- [sql_password](/admin/configuration/toplevel/#sql-password)
- [sql_ssl](/admin/configuration/toplevel/#sql-ssl)
- [sql_ssl_verify](/admin/configuration/toplevel/#sql-ssl-verify)
- [sql_ssl_cafile](/admin/configuration/toplevel/#sql-ssl-cafile)
- [sql_ssl_certfile](/admin/configuration/toplevel/#sql-ssl-certfile)
- [sql_pool_size](/admin/configuration/toplevel/#sql-pool-size)
- [sql_keepalive_interval](/admin/configuration/toplevel/#sql-keepalive-interval)
- [sql_odbc_driver](/admin/configuration/toplevel/#sql-odbc-driver)
- [sql_start_interval](/admin/configuration/toplevel/#sql-start-interval)
- [sql_prepared_statements](/admin/configuration/toplevel/#sql-prepared-statements)

Example of plain ODBC connection:


	sql_server: "DSN=database;UID=ejabberd;PWD=password"

Example of MySQL connection:


	sql_type: mysql
	sql_server: server.company.com
	sql_port: 3306 # the default
	sql_database: mydb
	sql_username: user1
	sql_password: "**********"
	sql_pool_size: 5

## SQL Authentication

You can authenticate users against an SQL database, see the option `auth_method`
in the [Authentication](/admin/configuration/authentication/) section.

To store the passwords in SCRAM format instead of plaintext,
see the [SCRAM](/admin/configuration/authentication/#scram) section.

## SQL Storage

Several `ejabberd` [modules](/admin/configuration/modules/)
have options called `db_type`, and can store their tables
in an SQL database instead of internal.

In this sense, if you defined your database access using the
[SQL Options](#sql-options),
you can configure a module to use your database
by adding the option `db_type: sql` to that module.

Alternatively, if you want all modules to use your SQL database when possible,
you may prefer to set SQL as your [default database](#default-database).

# Redis

[`Redis`](https://redis.io/) is an advanced key-value cache and store. You can
use it to store transient data, such as records for C2S (client) sessions.
There are several options available:

**`redis_server: String`**:   A hostname of the Redis server. The default is `localhost`.

**`redis_port: Port`**:   The port where the Redis server is accepting connections. The default
	is 6379.

**`redis_password: String`**:   The password to the Redis server. The default is an empty string,
	i.e. no password.

**`redis_db: N`**:   Redis database number. The default is 0.

**`redis_connect_timeout: N`**:   A number of seconds to wait for the connection to be established to the Redis
	server. The default is 1 second.

Example configuration:


	redis_server: redis.server.com
	redis_db: 1

# Microsoft SQL Server

For now, MS SQL is only supported in Unix-like OS'es. You need to have
[`unixODBC`](http://www.unixodbc.org/) installed on your machine.
Also, in some cases you need to add machine name to `sql_username`, especially
when you have `sql_server` defined as an IP address, e.g.:


	sql_type: mssql
	sql_server: 1.2.3.4
	...
	sql_username: user1@host

By default, ejabberd will use the [`FreeTDS`](https://www.freetds.org/) driver. You need to have the driver file `libtdsodbc.so` installed in your library PATH on your system.

If the FreeTDS driver is not installed in a standard location, or if you want to use another ODBC driver, you can specify the path to the driver using the [sql_odbc_driver](/admin/configuration/toplevel/#sql-odbc-driver) option, available in ejabberd 20.12 or later. For example, if you want to use Microsoft ODBC Driver 17 for SQL Server:

	sql_odbc_driver: "/opt/microsoft/msodbcsql17/lib64/libmsodbcsql-17.3.so.1.1"

Note that if you use a Microsoft driver, you may have to use an IP address instead of a host name for the `sql_server` option.

---
title: Using ejabberd with MySQL
menu: MySQL
toc: true
---

ejabberd is bundled with native Erlang driver to use MySQL as a
backend for persistent storage.

Using MySQL as backend is thus extremely straightforward.

# ejabberd installation

ejabberd packages and binary installers contains all the module needed
to connect to your MySQL server. You have no extra module to install
anymore.

If you are building ejabberd from source, you need to make sure that
you configure ejabberd to include MySQL module. It can be done by
passing option `--enable-mysql` to `configure` script. For example:

~~~ bash
cd ejabberd-source
./configure --enable-mysql
~~~

# MySQL installation

You need a MySQL server that you can point your ejabberd configuration
to. The database does not have to be on the same server than ejabberd.

## Requirements

ejabberd make use of FULLTEXT indexes with InnoDB. Thus, you need
MySQL 5.6 or greater to use with ejabberd.

**Note:** If you do not store message archive in database however, you
can try using older 5.5 version. You may need to adapt MySQL database
schema to cope with those older MySQL versions.

## MySQL on Linux

This documentation will not get into the details of making MySQL
running on Linux for production. It is dependant on Linux distribution
and system admistrators preferences and habits.

It is also well documented, so it should not be an issue.

## Amazon RDS compliance

ejabberd is fully compliant with
[MySQL on Amazon RDS](http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html).

You just need to make sure to use MySQL ersion 5.6 or greater when you
create your database.

## Example MySQL installation on OSX with homebrew

For testing / development, it is common to start experimenting with
MySQL with Homebrew installation.

Here is how to get started to help with setup up environment.

With homebrew properly installed, you can use the following command to install MySQL:

~~~ bash
brew install mysql
~~~

You can then follow instruction to finish the installation, for
example by running `mysql_secure_installation`.

You can manually start server with:

~~~ bash
mysql.server start
~~~

To connect to your local MySQL sercer using `mysql` command-line,
assuming you kept the default set up, use:

~~~ bash
mysql -uroot
~~~

To stop it, use:

~~~ bash
mysql.server stop
~~~

<!--

Should we also show how to restart from scratch by removing old homebrew versions ?

Here is how to do it:

brew remove mysql
brew uninstall --force mysql
sudo rm -rf /usr/local/var/mysql

-->

## Install with Windows Bash

On Windows you can install MySQL easily like on Linux using Ubuntu Bash:

~~~ bash
sudo apt-get install mysql-server-5.6
~~~

After configuration, you can start MySQL with:

~~~ bash
sudo /etc/init.d/mysql start
~~~

You can connect on the database with your created admin password:

~~~ bash
mysql -uroot -ppassword
~~~


# MySQL database creation

## Create ejabberd user and database

MySQL admins should use that schema and grant right to a dedicated
'ejabberd' user (replace password with your desired password):

~~~ bash
echo "GRANT ALL ON ejabberd.* TO 'ejabberd'@'localhost' IDENTIFIED BY 'password';" | mysql -h localhost -u root
~~~

You can then create a dedicated 'ejabberd' database (use password
created earlier):

~~~ bash
echo "CREATE DATABASE ejabberd;" | mysql -h localhost -u ejabberd -p
~~~

You should now be able to connect to 'ejabberd' database with user
'ejabberd' (use password defined on GRANT command):

~~~ bash
mysql -h localhost -u ejabberd -p -D ejabberd

Welcome to the MySQL monitor.  Commands end with ; or \g.
Your MySQL connection id is 8
Server version: 5.7.11 Homebrew

Copyright (c) 2000, 2016, Oracle and/or its affiliates. All rights reserved.

Oracle is a registered trademark of Oracle Corporation and/or its
affiliates. Other names may be trademarks of their respective
owners.

Type 'help;' or '\h' for help. Type '\c' to clear the current input statement.

mysql>
~~~

## Download and adapt MySQL schema

The schema files can be found in ejabberd priv directory. MySQL
default schema is defined in a file called `mysql.sql`.

* After build and install from source, the SQL schemas are copied here:

  ~~~ bash
  PREFIX/lib/ejabberd-VERSION/priv/sql
  ~~~

* After installing ejabberd from binary installer, you can find the
  SQL schema in similar directory:

  ~~~ bash
  INSTALLDIR/lib/ejabberd-VERSION/priv/sql
  ~~~

Starting from ejabberd 16.03, database schemas are installed in the
more usual directory:

~~~ bash
INSTALLDIR_OR_PREFIX/share/doc/ejabberd
~~~

Finally, you can always find the lastest version of the MySQL schema
in ejabberd Github repository:
[mysql.sql](https://github.com/processone/ejabberd/blob/master/sql/mysql.sql)

You can download it with command:

~~~ bash
wget https://raw.githubusercontent.com/processone/ejabberd/master/sql/mysql.sql
~~~

From there, you can read it and check that it suites your production
constrains. You can learn more about the database schema, you can read
[ejabberd SQL database schema documentation](/developer/sql-schema/).

## Load database schema into your new database

You can load the schema in your new 'ejabberd' database with the following command:

~~~ bash
mysql -h localhost -D ejabberd -u ejabberd -p < mysql.sql
~~~

To make sure all looks fine, you can show the list of SQL tables:

~~~ bash
echo "SHOW TABLES;" | mysql -h localhost -D ejabberd -u ejabberd -p --table

mysql: [Warning] Using a password on the command line interface can be insecure.
+-------------------------+
| Tables_in_ejabberd      |
+-------------------------+
| archive                 |
| archive_prefs           |
| caps_features           |
| irc_custom              |
| last                    |
| motd                    |
| muc_registered          |
| muc_room                |
| privacy_default_list    |
| privacy_list            |
| privacy_list_data       |
| private_storage         |
| pubsub_item             |
| pubsub_node             |
| pubsub_node_option      |
| pubsub_node_owner       |
| pubsub_state            |
| pubsub_subscription_opt |
| roster_version          |
| rostergroups            |
| rosterusers             |
| sm                      |
| spool                   |
| sr_group                |
| sr_user                 |
| users                   |
| vcard                   |
| vcard_search            |
| vcard_xupdate           |
+-------------------------+
~~~

Your database is now ready to connect with ejabberd.

# ejabberd configuration

ejabberd default backend is Mnesia internal database. However,
ejabberd is extremely flexible and you can decide to use MySQL instead
on a module-by-module basis.

## Adding MySQL connection configuration to ejabberd config file

In `ejabberd.yml`, define your database parameters:

~~~ yaml
sql_type: mysql
sql_server: "localhost"
sql_database: "ejabberd"
sql_username: "ejabberd"
sql_password: "password"
## If you want to specify the port:
sql_port: 3306
~~~

Those parameters are mandatory if you want to use MySQL with ejabberd.

## Configure desired authentication backend

If you decide to store user password in ejabberd, you need to tell
ejabberd to use MySQL instead of internal database for authentication.

You thus need to change ejabberd configuration `auth_method` to
replace `internal` authentication with `sql`:

~~~ yaml
auth_method: sql
~~~

If you restart ejabberd, it should connect to your database for
authentication. In case it does not work as expected, check your
config file syntax and log files (`ejabberd.log`, `error.log`,
`crash.log`)

For example, you can create a user in database with `ejabberdctl`:

~~~ bash
/sbin/ejabberdctl register "testuser" "localhost" "passw0rd"

User testuser@localhost successfully registered
~~~

You should now be able to connect XMPP users based on MySQL user base.

## Switch modules to use MySQL instead of Mnesia

At this stage, only the authentication / user base has been moved to
MySQL. For data managed by modules, ejabberd still use internal
database as default.

For each modules that support SQL backend, you can pass option
`db_type: sql` to use your configured MySQL database. Switch can be
done on a module by module basis. For example, if you want to store
contact list in MySQL, you can do:

~~~ yaml
modules:
  ...
  mod_roster:
    db_type: sql
  ...
~~~

However, if you want to use MySQL for all modules that support MySQL
as db_type, you can simply use global option `default_db: sql`:

~~~ yaml
default_db: sql
~~~

**Note:** even if you move all the persistent data you can to MySQL,
Mnesia will still be started and used to manage clustering.

# Migrating data from internal database to MySQL

To migrate your data, once you have setup your sql service, you can
move most of the data to your database.

You need to take precautions before you launch the migration:

1. Before you launch migration from internal database, make sure you have
made a proper backup.

2. Always try the migration first on an instance created from your data
backup, to make sure the migration script will work fine on your
dataset.

3. Then, when doing final migration, make sure your instance is not
accepting connections by blocking incoming connections, for example
with firewall rules (block port 5222, 5269 and 5280 as default).

When you are ready, you can:

1. Connect to a running ejabberd:

   ~~~ bash
   ./ejabberdctl debug
   ~~~

2. Alternatively, use `ejabberdctl live` to launch ejabberd with an Erlang shell attached.

3. Launch the migration command `ejd2sql:export/2` from Erlang
shell. First parameter is the XMPP domain name you want to migrate
(i.e `localhost`). Second parameter `sql` tells ejabberd to export to
configured MySQL database. For example:

   ~~~ erlang
   ejd2sql:export(<<"localhost">>, sql).
   ~~~

You should be set now.

# Getting further

To get further you can read
[ejabberd Installation and Operation Guide: Database and LDAP configuration](/admin/guide/configuration/#database-and-ldap-configuration)

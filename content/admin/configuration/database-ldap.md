---
title: Database and LDAP Configuration
toc: true
menu: Database and LDAP
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

-   [`Mnesia`](http://www.erlang.org/doc/apps/mnesia/index.html)

-   [`MySQL`](http://www.mysql.com/). Check the tutorial [Using ejabberd with MySQL](/tutorials/mysql/)

-   Any [`ODBC`](http://en.wikipedia.org/wiki/Open_Database_Connectivity) compatible database

-   [`PostgreSQL`](http://www.postgresql.org/)

-   [`MS SQL Server/SQL Azure`](https://en.wikipedia.org/wiki/Microsoft_SQL_Server)

-   [`SQLite`](https://www.sqlite.org)

-   [`Redis`](http://redis.io/)(only for transient data)

The following LDAP servers are tested with `ejabberd`:

-   [`Active Directory`](http://www.microsoft.com/activedirectory/)(see
	section [Active Directory](#active-directory))

-   [`OpenLDAP`](http://www.openldap.org/)

-   [`CommuniGate Pro`](http://www.communigate.com/)

-   Normally any LDAP compatible server should work; inform us about
	your success with a not-listed server so that we can list it here.

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

You can simplify the configuration by setting the default database. This can be done with `default_db` option:

**`default_db: mnesia|sql`**:  This will define the default database for a module lacking `db_type` option or if `auth_method` option is not set.

# Relational Databases

There are two schemas usable for ejabberd. The default lecacy schema allows to
store one XMPP domain into one ejabberd database. The 'new' schema allows to
handle several XMPP domains in a single ejabberd database. Using this new schema
is best when serving several XMPP domains and/or changing domains from time to
time. This avoid need to manage several databases and handle complex
configuration changes.

You need to upload SQL schema to your SQL server.
Choose the one from [`this`](http://www.unixodbc.org) list.
If you are using MySQL and choose the default schema, use `mysql.sql`. If you
are using PostgreSQL and need the new schema, use `pg.new.sql`.

If you choose the new schema, you MUST add an extra line into `ejabberd.yml`
configuration file to enable the feature:

	new_sql_schema: true

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

# Redis

[`Redis`](http://redis.io/) is an advanced key-value cache and store. You can
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

# Microsoft SQL

For now, MS SQL is only supported in Unix-like OS'es. You need to have
[`FreeTDS`](http://www.freetds.org) and
[`unixODBC`](http://www.unixodbc.org) installed on your machine.
Also, in some cases you need to add machine name to `sql_username`, especially
when you have `sql_server` defined as an IP address, e.g.:


	sql_type: mssql
	sql_server: 1.2.3.4
	...
	sql_username: user1@host

## SQL Authentication

You can authenticate users against an SQL database, see the option
`auth_method` in section [Authentication](#authentication).

The option `auth_password_format` is supported,
for details see section [Internal](#internal).
Please note that if you use SQL auth method and set SCRAM format,
old plain passwords that may be stored in the database are not
automatically scrammed. For that, you can execute the command:


	ejabberdctl convert_to_scram example.org

## SQL Storage

An ODBC compatible database also can be used to store information into
from several `ejabberd` modules. See section [Modules Overview](#modules-overview) to see which
modules can be used with relational databases like MySQL. To enable
storage to your database, just make sure that your database is running
well (see previous sections), and add the module option `db_type: sql`
or set `default_db: sql` globally if you want to use SQL for all modules.


# LDAP

`ejabberd` has built-in LDAP support. You can authenticate users against
LDAP server and use LDAP directory as vCard storage.

Usually `ejabberd` treats LDAP as a read-only storage: it is possible to
consult data, but not possible to create accounts or edit vCard that is
stored in LDAP. However, it is possible to change passwords if
`mod_register` module is enabled and LDAP server supports
[`RFC 3062`](http://tools.ietf.org/html/rfc3062).

## LDAP Connection

Two connections are established to the LDAP server per vhost, one for
authentication and other for regular calls.

To configure the LDAP connection there are these top-level options:

- [ldap_servers](/admin/configuration/toplevel/#ldap-servers)
- [ldap_encrypt](/admin/configuration/toplevel/#ldap-encrypt)
- [ldap_tls_verify](/admin/configuration/toplevel/#ldap-tls-verify)
- [ldap_tls_cacertfile](/admin/configuration/toplevel/#ldap-tls-cacertfile)
- [ldap_tls_depth](/admin/configuration/toplevel/#ldap-tls-depth)
- [ldap_port](/admin/configuration/toplevel/#ldap-port)
- [ldap_rootdn](/admin/configuration/toplevel/#ldap-rootdn)
- [ldap_password](/admin/configuration/toplevel/#ldap-password)
- [ldap_deref_aliases](/admin/configuration/toplevel/#ldap-deref-aliases)

Example:


	auth_method: [ldap]
	ldap_servers:
	  - ldap1.example.org
	ldap_port: 389
	ldap_rootdn: "cn=Manager,dc=domain,dc=org"
	ldap_password: "**********"

## LDAP Authentication

You can authenticate users against an LDAP directory. Note that current
LDAP implementation does not support SASL authentication.

To configure LDAP authentication there are these top-level options:

- [ldap_base](/admin/configuration/toplevel/#ldap-base)
- [ldap_uids](/admin/configuration/toplevel/#ldap-uids)
- [ldap_uidattr](/admin/configuration/toplevel/#ldap-uidattr)
- [ldap_uidattr_format](/admin/configuration/toplevel/#ldap-uidattr-format)
- [ldap_filter](/admin/configuration/toplevel/#ldap-filter)
- [ldap_dn_filter](/admin/configuration/toplevel/#ldap-dn-filter)

## LDAP Examples

### Common example

Let’s say `ldap.example.org` is the name of our LDAP server. We have
users with their passwords in `ou=Users,dc=example,dc=org` directory.
Also we have addressbook, which contains users emails and their
additional infos in `ou=AddressBook,dc=example,dc=org` directory. The
connection to the LDAP server is encrypted using TLS, and using the
custom port 6123. Corresponding authentication section should looks like
this:


	## Authentication method
	auth_method: [ldap]
	## DNS name of our LDAP server
	ldap_servers: [ldap.example.org]
	## Bind to LDAP server as "cn=Manager,dc=example,dc=org" with password "secret"
	ldap_rootdn: "cn=Manager,dc=example,dc=org"
	ldap_password: secret
	ldap_encrypt: tls
	ldap_port: 6123
	## Define the user's base
	ldap_base: "ou=Users,dc=example,dc=org"
	## We want to authorize users from 'shadowAccount' object class only
	ldap_filter: "(objectClass=shadowAccount)"

Now we want to use users LDAP-info as their vCards. We have four
attributes defined in our LDAP schema: `mail` — email address,
`givenName` — first name, `sn` — second name, `birthDay` — birthday.
Also we want users to search each other. Let’s see how we can set it up:


	modules:
	  ...
	  mod_vcard:
	    db_type: ldap
	    ## We use the same server and port, but want to bind anonymously because
	    ## our LDAP server accepts anonymous requests to
	    ## "ou=AddressBook,dc=example,dc=org" subtree.
	    ldap_rootdn: ""
	    ldap_password: ""
	    ## define the addressbook's base
	    ldap_base: "ou=AddressBook,dc=example,dc=org"
	    ## uidattr: user's part of JID is located in the "mail" attribute
	    ## uidattr_format: common format for our emails
	    ldap_uids:
	      mail: "%u@mail.example.org"
	    ## We have to define empty filter here, because entries in addressbook does not
	    ## belong to shadowAccount object class
	    ldap_filter: ""
	    ## Now we want to define vCard pattern
	    ldap_vcard_map:
	     NICKNAME: {"%u": []} # just use user's part of JID as their nickname
	     GIVEN: {"%s": [givenName]}
	     FAMILY: {"%s": [sn]}
	     FN: {"%s, %s": [sn, givenName]} # example: "Smith, John"
	     EMAIL: {"%s": [mail]}
	     BDAY: {"%s": [birthDay]}]}
	    ## Search form
	    ldap_search_fields:
	      User: "%u"
	      Name: givenName
	      "Family Name": sn
	      Email: mail
	      Birthday: birthDay
	    ## vCard fields to be reported
	    ## Note that JID is always returned with search results
	    ldap_search_reported:
	      "Full Name": FN
	      Nickname: NICKNAME
	      Birthday: BDAY
	  ...

Note that `mod_vcard` with LDAP backend checks for the existence of the user
before searching their information in LDAP.

### Active Directory

Active Directory is just an LDAP-server with predefined attributes. A
sample configuration is shown below:


	auth_method: [ldap]
	ldap_servers: [office.org]  # List of LDAP servers
	ldap_base: "DC=office,DC=org" # Search base of LDAP directory
	ldap_rootdn: "CN=Administrator,CN=Users,DC=office,DC=org" # LDAP manager
	ldap_password: "*******" # Password to LDAP manager
	ldap_uids: [sAMAccountName]
	ldap_filter: "(memberOf=*)"

	modules:
	  ...
	  mod_vcard:
	    db_type: ldap
	    ldap_vcard_map:
	      NICKNAME: {"%u": []}
	      GIVEN: {"%s": [givenName]}
	      MIDDLE: {"%s": [initials]}
	      FAMILY: {"%s": [sn]}
	      FN: {"%s": [displayName]}
	      EMAIL: {"%s": [mail]}
	      ORGNAME: {"%s": [company]}
	      ORGUNIT: {"%s": [department]}
	      CTRY: {"%s": [c]}
	      LOCALITY: {"%s": [l]}
	      STREET: {"%s": [streetAddress]}
	      REGION: {"%s": [st]}
	      PCODE: {"%s": [postalCode]}
	      TITLE: {"%s": [title]}
	      URL: {"%s": [wWWHomePage]}
	      DESC: {"%s": [description]}
	      TEL: {"%s": [telephoneNumber]}]}
	    ldap_search_fields:
	      User: "%u"
	      Name: givenName
	      "Family Name": sn
	      Email: mail
	      Company: company
	      Department: department
	      Role: title
	      Description: description
	      Phone: telephoneNumber
	    ldap_search_reported:
	      "Full Name": FN
	      Nickname: NICKNAME
	      Email: EMAIL
	  ...

## Shared Roster in LDAP

Since [mod_shared_roster_ldap](/admin/configuration/modules/#mod-shared-roster-ldap) has a few complex options,
some of them are documented with more detail here:

### Filters

**`ldap_ufilter`**:   “User Filter” – used for retrieving the human-readable name of
	roster entries (usually full names of people in the roster). See
	also the parameters `ldap_userdesc` and `ldap_useruid`. If
	unspecified, defaults to the top-level parameter of the same name.
	If that one also is unspecified, then the filter is assembled from
	values of other parameters as follows (`[ldap_SOMETHING]` is used to
	mean “the value of the configuration parameter
	`ldap_SOMETHING`”):

	  (&(&([ldap_memberattr]=[ldap_memberattr_format])([ldap_groupattr]=%g))[ldap_filter])

Subsequently `%u` and `%g` are replaced with a \*.
This means that given the defaults, the filter sent to the LDAP
server would be `(&(memberUid=*)(cn=*))`. If however the
`ldap_memberattr_format` is something like
`uid=%u,ou=People,o=org`, then the filter will be
`(&(memberUid=uid=*,ou=People,o=org)(cn=*))`.

**`ldap_filter`**:   Additional filter which is AND-ed together with *User
	Filter* and *Group Filter*. If unspecified,
	defaults to the top-level parameter of the same name. If that one is
	also unspecified, then no additional filter is merged with the other
	filters.

Note that you will probably need to manually define the
*User* and *Group Filter* (since the
auto-assembled ones will not work) if:

-   your `ldap_memberattr_format` is anything other than a
	simple `%u`,

-   **and** the attribute specified with
	`ldap_memberattr` does not support substring matches.

An example where it is the case is OpenLDAP and
*(unique)MemberName* attribute from the
*groupOf(Unique)Names* objectClass. A symptom of this problem
is that you will see messages such as the following in your
`slapd.log`:


	get_filter: unknown filter type=130
	filter="(&(?=undefined)(?=undefined)(something=else))"

### Control parameters

These parameters control the behaviour of the module.

**`ldap_memberattr_format_re`**:   A regex for extracting user ID from the value of the attribute named
	by `ldap_memberattr`.

An example value
`“CN=(\\w*),(OU=.*,)*DC=company,DC=com”`
works for user IDs such as the following:

-   `CN=Romeo,OU=Montague,DC=company,DC=com`
-   `CN=Abram,OU=Servants,OU=Montague,DC=company,DC=com`
-   `CN=Juliet,OU=Capulet,DC=company,DC=com`
-   `CN=Peter,OU=Servants,OU=Capulet,DC=company,DC=com`

In case:

-   the option is unset,
-   or the `re` module in unavailable in the current
    Erlang environment,
-   or the regular expression does not compile,

then instead of a regular expression, a simple format specified by
`ldap_memberattr_format` is used. Also, in the last two
cases an error message is logged during the module initialization.

Also, note that in all cases `ldap_memberattr_format`
(and `*not*` the regex version) is used for constructing
the default “User/Group Filter” — see section [Filters](#msrl-filters).

### Retrieving the roster

When the module is called to retrieve the shared roster for a user, the
following algorithm is used:

1.  [step:rfilter] A list of names of groups to display is created: the
	*Roster Filter* is run against the base DN, retrieving
	the values of the attribute named by `ldap_groupattr`.

2.  Unless the group cache is fresh (see the
	`ldap_group_cache_validity` option), it is refreshed:

	1.  Information for all groups is retrieved using a single query:
		the *Group Filter* is run against the Base DN,
		retrieving the values of attributes named by
		`ldap_groupattr` (group ID),
		`ldap_groupdesc` (group “Display Name”) and
		`ldap_memberattr` (IDs of group members).

	2.  group “Display Name”, read from the attribute named by
		`ldap_groupdesc`, is stored in the cache for the
		given group

	3.  the following processing takes place for each retrieved value of
		attribute named by `ldap_memberattr`:

		1.  the user ID part of it is extracted using
			`ldap_memberattr_format(_re)`,

		2.  then (unless `ldap_auth_check` is set to
			`off`) for each found user ID, the module checks
			(using the `ejabberd` authentication subsystem) whether such
			user exists in the given virtual host. It is skipped if the
			check is enabled and fails.

			This step is here for historical reasons. If you have a tidy
			DIT and properly defined “Roster Filter” and “Group Filter”,
			it is safe to disable it by setting
			`ldap_auth_check` to `off` — it will
			speed up the roster retrieval.

		3.  the user ID is stored in the list of members in the cache
			for the given group

3.  For each item (group name) in the list of groups retrieved in
	step [step:rfilter]:

	1.  the display name of a shared roster group is retrieved from the
		group cache

	2.  for each IDs of users which belong to the group, retrieved from
		the group cache:

		1.  the ID is skipped if it’s the same as the one for which we
			are retrieving the roster. This is so that the user does not
			have himself in the roster.

		2.  the display name of a shared roster user is retrieved:

			1.  first, unless the user name cache is fresh (see the
				`ldap_user_cache_validity` option), it is
				refreshed by running the *User Filter*,
				against the Base DN, retrieving the values of attributes
				named by `ldap_useruid` and
				`ldap_userdesc`.

			2.  then, the display name for the given user ID is
				retrieved from the user name cache.

### Configuration examples

Since there are many possible
[`DIT`](http://en.wikipedia.org/wiki/Directory_Information_Tree)
layouts, it will probably be easiest to understand how to configure the
module by looking at an example for a given DIT (or one resembling it).

#### Flat DIT

This seems to be the kind of DIT for which this module was initially
designed. Basically there are just user objects, and group membership is
stored in an attribute individually for each user. For example in a
layout shown in figure [fig:msrl-dit-flat], the group of each user is
stored in its `ou` attribute.

Such layout has a few downsides, including:

-   information duplication – the group name is repeated in every member
	object

-   difficult group management – information about group members is not
	centralized, but distributed between member objects

-   inefficiency – the list of unique group names has to be computed by
	iterating over all users

This however seems to be a common DIT layout, so the module keeps
supporting it. You can use the following configuration…


	modules:
	  ...
	  mod_shared_roster_ldap:
	    ldap_base: "ou=flat,dc=nodomain"
	    ldap_rfilter: "(objectClass=inetOrgPerson)"
	    ldap_groupattr: ou
	    ldap_memberattr: cn
	    ldap_filter: "(objectClass=inetOrgPerson)"
	    ldap_userdesc: displayName
	  ...

…to be provided with a roster as shown in figure [fig:msrl-roster-flat]
upon connecting as user `czesio`.

#### Deep DIT

This type of DIT contains distinctly typed objects for users and groups
– see figure [fig:msrl-dit-deep]. They are shown separated into
different subtrees, but it’s not a requirement.

If you use the following example module configuration with it:


	modules:
	  ...
	  mod_shared_roster_ldap:
	    ldap_base: "ou=deep,dc=nodomain"
	    ldap_rfilter: "(objectClass=groupOfUniqueNames)"
	    ldap_filter: ""
	    ldap_gfilter: "(&(objectClass=groupOfUniqueNames)(cn=%g))"
	    ldap_groupdesc: description
	    ldap_memberattr: uniqueMember
	    ldap_memberattr_format: "cn=%u,ou=people,ou=deep,dc=nodomain"
	    ldap_ufilter: "(&(objectClass=inetOrgPerson)(cn=%u))"
	    ldap_userdesc: displayName
	  ...

…and connect as user `czesio`, then `ejabberd` will provide
you with the roster shown in figure [fig:msrl-roster-deep].

## vCard in LDAP

Since LDAP may be complex to configure in [mod_vcard](/admin/configuration/modules/#mod-vcard),
this section provides more details.

`ejabberd` can map LDAP attributes to vCard fields. This feature is
enabled when the `mod_vcard` module is configured with `db_type:
ldap`. Notice that it does not depend on the authentication method
(see [LDAP Authentication](#ldap-authentication)).

Usually `ejabberd` treats LDAP as a read-only storage: it is possible to
consult data, but not possible to create accounts or edit vCard that is
stored in LDAP. However, it is possible to change passwords if
`mod_register` module is enabled and LDAP server supports
[`RFC 3062`](http://tools.ietf.org/html/rfc3062).

This feature has its own optional parameters. The first
group of parameters has the same meaning as the top-level LDAP
parameters to set the authentication method: `ldap_servers`,
`ldap_port`, `ldap_rootdn`, `ldap_password`, `ldap_base`, `ldap_uids`,
`ldap_deref_aliases` and `ldap_filter`. See section
[LDAP Authentication](#ldap-authentication) for
detailed information about these options. If one of these options is not
set, `ejabberd` will look for the top-level option with the same name.

Examples:

-   Let’s say `ldap.example.org` is the name of our LDAP server. We have
	users with their passwords in `ou=Users,dc=example,dc=org`
	directory. Also we have addressbook, which contains users emails and
	their additional infos in `ou=AddressBook,dc=example,dc=org`
	directory. Corresponding authentication section should looks like
	this:


		## authentication method
		auth_method: ldap
		## DNS name of our LDAP server
		ldap_servers:
		  - ldap.example.org
		## We want to authorize users from 'shadowAccount' object class only
		ldap_filter: "(objectClass=shadowAccount)"

Now we want to use users LDAP-info as their vCards. We have four
attributes defined in our LDAP schema: `mail` — email address,
`givenName` — first name, `sn` — second name, `birthDay` — birthday.
Also we want users to search each other. Let’s see how we can set it
up:


		modules:
		  mod_vcard:
		    db_type: ldap
		    ## We use the same server and port, but want to bind anonymously because
		    ## our LDAP server accepts anonymous requests to
		    ## "ou=AddressBook,dc=example,dc=org" subtree.
		    ldap_rootdn: ""
		    ldap_password: ""
		    ## define the addressbook's base
		    ldap_base: "ou=AddressBook,dc=example,dc=org"
		    ## uidattr: user's part of JID is located in the "mail" attribute
		    ## uidattr_format: common format for our emails
		    ldap_uids: {"mail": "%u@mail.example.org"}
		    ## Now we want to define vCard pattern
		    ldap_vcard_map:
		      NICKNAME: {"%u": []} # just use user's part of JID as their nickname
		      FIRST: {"%s": [givenName]}
		      LAST: {"%s": [sn]}
		      FN: {"%s, %s": [sn, givenName]} # example: "Smith, John"
		      EMAIL: {"%s": [mail]}
		      BDAY: {"%s": [birthDay]}
		    ## Search form
		    ldap_search_fields:
		      User: "%u"
		      Name: givenName
		      "Family Name": sn
		      Email: mail
		      Birthday: birthDay
		    ## vCard fields to be reported
		    ## Note that JID is always returned with search results
		    ldap_search_reported:
		      "Full Name": FN
		      Nickname: NICKNAME
		      Birthday: BDAY

Note that `mod_vcard` with LDAP backend checks an existence of the user
before searching their info in LDAP.

-   `ldap_vcard_map` example:


		ldap_vcard_map:
		  NICKNAME: {"%u": []} # just use user's part of JID as their nickname
		  FN: {"%s": [displayName]}
		  CTRY: {Russia: []}
		  EMAIL: {"%u@%d": []}
		  DESC: {"%s\n%s": [title, description]}

-   `ldap_search_fields` example:


		ldap_search_fields:
		  User: uid
		  "Full Name": displayName
		  Email: mail

-   `ldap_search_reported` example:


		ldap_search_reported:
		  "Full Name": FN
		  Email: EMAIL
		  Birthday: BDAY
		  Nickname: NICKNAME


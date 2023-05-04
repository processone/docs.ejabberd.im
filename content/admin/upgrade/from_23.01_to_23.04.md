---
title: Upgrade to ejabberd 23.04
toc: true
---

There is a new module, new hooks, new options, and some option accepts additional values,
and more importantly, there are many improvements in the SQL schemas, and a change in the `ecs` container image.

Please check the ejabberd [23.04](/archive/23_04/) release announcement for details
about the improvements.

## Many improvements in SQL databases

There are many improvements in the SQL databases field (see [#3980](https://github.com/processone/ejabberd/pull/3980) and [#3982](https://github.com/processone/ejabberd/pull/3982)):

- Added support to migrate MySQL and MS SQL to [new schema](https://docs.ejabberd.im/admin/configuration/database/#default-and-new-schemas), fixed a long standing bug, and many other improvements.
- Regarding MS SQL, there are schema fixes, added support to `new` schema, and the corresponding schema migration, along other minor improvements and bugfixes.
- The automated ejabberd testing now also runs tests on upgraded schema databases, and supports for running tests on MS SQL
- And also fixed other minor SQL schema inconsistencies, removed unnecessary indexes and changed PostgreSQL SERIAL to BIGSERIAL columns.

Please upgrade your existing SQL database, check the notes later in this document!


## Erlang node name in `ecs` container image

The `ecs` container image is built using the files from [docker-ejabberd/ecs](https://github.com/processone/docker-ejabberd/tree/master/ecs), and published in [docker.io/ejabberd/ecs](https://hub.docker.com/r/ejabberd/ecs/). This image in general gets only minimal fixes, no major or breaking changes, but in this release it got a change that will require the administrator intervention.

The Erlang node name is now by default fixed to `ejabberd@localhost`, instead of being variably set by the container host name. If you previously allowed ejabberd to decide its node name (which was random), then it will now create a new mnesia database instead of using the previous one:
```bash
$ docker exec -it ejabberd ls /home/ejabberd/database/
ejabberd@1ca968a0301a
ejabberd@localhost
...
```

A simple solution is to create the container providing `ERLANG_NODE_ARG` with the old erlang node name, for example:
```bash
docker run ... -e ERLANG_NODE_ARG=ejabberd@1ca968a0301a
```
or in docker-compose.yml
```yaml
version: '3.7'
services:
  main:
    image: ejabberd/ecs
    environment:
      - ERLANG_NODE_ARG=ejabberd@1ca968a0301a
```

Another solution is to [change the mnesia node name](https://github.com/processone/docker-ejabberd/tree/master/ecs#change-mnesia-node-name) in the mnesia spool files.


## SQL databases update

Those notes allow to apply the improvements in the SQL database schemas from this ejabberd release to your existing SQL database. Please take into account what database you use, and whether it is the [default or the new schema](https://docs.ejabberd.im/admin/configuration/database/#default-and-new-schemas).

### PostgreSQL new schema

Fix a long standing bug in new schema on PostgreSQL. The fix for any existing impacted installations is the same:
```sql
ALTER TABLE vcard_search DROP CONSTRAINT vcard_search_pkey;
ALTER TABLE vcard_search ADD PRIMARY KEY (server_host, lusername);
```

### PostgreSQL default/new schema

To convert columns to allow up to 2 billion rows in these tables. This conversion will require full table rebuilds, and will take a long time if tables already have lots of rows. Optional: this is not necessary if the tables are never likely to grow large.

```sql
ALTER TABLE archive ALTER COLUMN id TYPE BIGINT;
ALTER TABLE privacy_list ALTER COLUMN id TYPE BIGINT;
ALTER TABLE pubsub_node ALTER COLUMN nodeid TYPE BIGINT;
ALTER TABLE pubsub_state ALTER COLUMN stateid TYPE BIGINT;
ALTER TABLE spool ALTER COLUMN seq TYPE BIGINT;
```

### PostgreSQL/SQLite default schema

```sql
DROP INDEX i_rosteru_username;
DROP INDEX i_sr_user_jid;
DROP INDEX i_privacy_list_username;
DROP INDEX i_private_storage_username;
DROP INDEX i_muc_online_users_us;
DROP INDEX i_route_domain;
DROP INDEX i_mix_participant_chan_serv;
DROP INDEX i_mix_subscription_chan_serv_ud;
DROP INDEX i_mix_subscription_chan_serv;
DROP INDEX i_mix_pam_us;
```

### PostgreSQL/SQLite new schema

```sql
DROP INDEX i_rosteru_sh_username;
DROP INDEX i_sr_user_sh_jid;
DROP INDEX i_privacy_list_sh_username;
DROP INDEX i_private_storage_sh_username;
DROP INDEX i_muc_online_users_us;
DROP INDEX i_route_domain;
DROP INDEX i_mix_participant_chan_serv;
DROP INDEX i_mix_subscription_chan_serv_ud;
DROP INDEX i_mix_subscription_chan_serv;
DROP INDEX i_mix_pam_us;
```

And now add index that might be missing

In PostgreSQL:
```sql
CREATE INDEX i_push_session_sh_username_timestamp ON push_session USING btree (server_host, username, timestamp);
```

In SQLite:
```sql
CREATE INDEX i_push_session_sh_username_timestamp ON push_session (server_host, username, timestamp);
```

### MySQL default schema

```sql
ALTER TABLE rosterusers DROP INDEX i_rosteru_username;
ALTER TABLE sr_user DROP INDEX i_sr_user_jid;
ALTER TABLE privacy_list DROP INDEX i_privacy_list_username;
ALTER TABLE private_storage DROP INDEX i_private_storage_username;
ALTER TABLE muc_online_users DROP INDEX i_muc_online_users_us;
ALTER TABLE route DROP INDEX i_route_domain;
ALTER TABLE mix_participant DROP INDEX i_mix_participant_chan_serv;
ALTER TABLE mix_participant DROP INDEX i_mix_subscription_chan_serv_ud;
ALTER TABLE mix_participant DROP INDEX i_mix_subscription_chan_serv;
ALTER TABLE mix_pam DROP INDEX i_mix_pam_u;
```

### MySQL new schema

```sql
ALTER TABLE rosterusers DROP INDEX i_rosteru_sh_username;
ALTER TABLE sr_user DROP INDEX i_sr_user_sh_jid;
ALTER TABLE privacy_list DROP INDEX i_privacy_list_sh_username;
ALTER TABLE private_storage DROP INDEX i_private_storage_sh_username;
ALTER TABLE muc_online_users DROP INDEX i_muc_online_users_us;
ALTER TABLE route DROP INDEX i_route_domain;
ALTER TABLE mix_participant DROP INDEX i_mix_participant_chan_serv;
ALTER TABLE mix_participant DROP INDEX i_mix_subscription_chan_serv_ud;
ALTER TABLE mix_participant DROP INDEX i_mix_subscription_chan_serv;
ALTER TABLE mix_pam DROP INDEX i_mix_pam_us;
```
Add index that might be missing:
```sql
CREATE INDEX i_push_session_sh_username_timestamp ON push_session (server_host, username(191), timestamp);
```

### MS SQL

```sql
DROP INDEX [rosterusers_username] ON [rosterusers];
DROP INDEX [sr_user_jid] ON [sr_user];
DROP INDEX [privacy_list_username] ON [privacy_list];
DROP INDEX [private_storage_username] ON [private_storage];
DROP INDEX [muc_online_users_us] ON [muc_online_users];
DROP INDEX [route_domain] ON [route];
go
```

MS SQL schema was missing some tables added in earlier versions of ejabberd:

```sql
CREATE TABLE [dbo].[mix_channel] (
    [channel] [varchar] (250) NOT NULL,
    [service] [varchar] (250) NOT NULL,
    [username] [varchar] (250) NOT NULL,
    [domain] [varchar] (250) NOT NULL,
    [jid] [varchar] (250) NOT NULL,
    [hidden] [smallint] NOT NULL,
    [hmac_key] [text] NOT NULL,
    [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [mix_channel] ON [mix_channel] (channel, service)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [mix_channel_serv] ON [mix_channel] (service)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[mix_participant] (
    [channel] [varchar] (250) NOT NULL,
    [service] [varchar] (250) NOT NULL,
    [username] [varchar] (250) NOT NULL,
    [domain] [varchar] (250) NOT NULL,
    [jid] [varchar] (250) NOT NULL,
    [id] [text] NOT NULL,
    [nick] [text] NOT NULL,
    [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE INDEX [mix_participant] ON [mix_participant] (channel, service, username, domain)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [mix_participant_chan_serv] ON [mix_participant] (channel, service)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[mix_subscription] (
    [channel] [varchar] (250) NOT NULL,
    [service] [varchar] (250) NOT NULL,
    [username] [varchar] (250) NOT NULL,
    [domain] [varchar] (250) NOT NULL,
    [node] [varchar] (250) NOT NULL,
    [jid] [varchar] (250) NOT NULL
);

CREATE UNIQUE INDEX [mix_subscription] ON [mix_subscription] (channel, service, username, domain, node)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [mix_subscription_chan_serv_ud] ON [mix_subscription] (channel, service, username, domain)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [mix_subscription_chan_serv_node] ON [mix_subscription] (channel, service, node)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE INDEX [mix_subscription_chan_serv] ON [mix_subscription] (channel, service)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE TABLE [dbo].[mix_pam] (
    [username] [varchar] (250) NOT NULL,
    [channel] [varchar] (250) NOT NULL,
    [service] [varchar] (250) NOT NULL,
    [id] [text] NOT NULL,
    [created_at] [datetime] NOT NULL DEFAULT GETDATE()
) TEXTIMAGE_ON [PRIMARY];

CREATE UNIQUE CLUSTERED INDEX [mix_pam] ON [mix_pam] (username, channel, service)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

go
```

MS SQL also had some incompatible column types:

```sql
ALTER TABLE [dbo].[muc_online_room] ALTER COLUMN [node] VARCHAR (250);
ALTER TABLE [dbo].[muc_online_room] ALTER COLUMN [pid] VARCHAR (100);
ALTER TABLE [dbo].[muc_online_users] ALTER COLUMN [node] VARCHAR (250);
ALTER TABLE [dbo].[pubsub_node_option] ALTER COLUMN [name] VARCHAR (250);
ALTER TABLE [dbo].[pubsub_node_option] ALTER COLUMN [val] VARCHAR (250);
ALTER TABLE [dbo].[pubsub_node] ALTER COLUMN [plugin] VARCHAR (32);
go
```

... and `mqtt_pub` table was incorrectly defined in old schema:

```sql
ALTER TABLE [dbo].[mqtt_pub] DROP CONSTRAINT [i_mqtt_topic_server];
ALTER TABLE [dbo].[mqtt_pub] DROP COLUMN [server_host];
ALTER TABLE [dbo].[mqtt_pub] ALTER COLUMN [resource] VARCHAR (250);
ALTER TABLE [dbo].[mqtt_pub] ALTER COLUMN [topic] VARCHAR (250);
ALTER TABLE [dbo].[mqtt_pub] ALTER COLUMN [username] VARCHAR (250);
CREATE UNIQUE CLUSTERED INDEX [dbo].[mqtt_topic] ON [mqtt_pub] (topic)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);
go
```

... and `sr_group` index/PK was inconsistent with other DBs:

```sql
ALTER TABLE [dbo].[sr_group] DROP CONSTRAINT [sr_group_PRIMARY];
CREATE UNIQUE CLUSTERED INDEX [sr_group_name] ON [sr_group] ([name])
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);
go
```


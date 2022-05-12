---
title: Upgrade to ejabberd 22.05
toc: true
---

## New Indexes in SQL for MUC

Two new indexes were added to optimize MUC. Those indexes can be added in the database before upgrading to 22.05, that will not affect older versions.

To update an existing database, depending on the schema used to create it:

- MySQL (`mysql.sql` or `mysql.new.sql`):
```sql
CREATE INDEX i_muc_room_host_created_at ON muc_room(host(75), created_at);
CREATE INDEX i_muc_room_subscribers_jid USING BTREE ON muc_room_subscribers(jid);
```

- PostgreSQL (`pg.sql` or `pg.new.sql`):
```sql
CREATE INDEX i_muc_room_host_created_at ON muc_room USING btree (host, created_at);
CREATE INDEX i_muc_room_subscribers_jid ON muc_room_subscribers USING btree (jid);
```

- SQLite (`lite.sql` or `lite.new.sql`):
```sql
CREATE INDEX i_muc_room_host_created_at ON muc_room (host, created_at);
CREATE INDEX i_muc_room_subscribers_jid ON muc_room_subscribers(jid);
```

- MS SQL (`mssql.sql`):
```sql
CREATE INDEX [muc_room_host_created_at] ON [muc_registered] (host, nick)
    WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);
CREATE INDEX [muc_room_subscribers_jid] ON [muc_room_subscribers] (jid);
```

## Fixes in PostgreSQL New Schema

If you moved your PostgreSQL database from old to new schema using [mod_admin_update_sql](https://docs.ejabberd.im/admin/configuration/modules/#mod-admin-update-sql) or the [update_sql](https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#update-sql) API command, be aware that those methods forgot to perform some updates.

To fix an existing PostgreSQL database schema, apply those changes manually:

```sql
ALTER TABLE archive DROP CONSTRAINT i_archive_sh_peer;
ALTER TABLE archive DROP CONSTRAINT i_archive_sh_bare_peer;
CREATE INDEX i_archive_sh_username_peer ON archive USING btree (server_host, username, peer);
CREATE INDEX i_archive_sh_username_bare_peer ON archive USING btree (server_host, username, bare_peer);

DROP TABLE carboncopy;

ALTER TABLE push_session DROP CONSTRAINT i_push_session_susn;
CREATE UNIQUE INDEX i_push_session_susn ON push_session USING btree (server_host, username, service, node);

ALTER TABLE mix_pam DROP CONSTRAINT i_mix_pam;
ALTER TABLE mix_pam DROP CONSTRAINT i_mix_pam_us;
CREATE UNIQUE INDEX i_mix_pam ON mix_pam (username, server_host, channel, service);
CREATE INDEX i_mix_pam_us ON mix_pam (username, server_host);

ALTER TABLE route DROP CONSTRAINT i_route;
CREATE UNIQUE INDEX i_route ON route USING btree (domain, server_host, node, pid);

ALTER TABLE mqtt_pub DROP CONSTRAINT i_mqtt_topic;
CREATE UNIQUE INDEX i_mqtt_topic_server ON mqtt_pub (topic, server_host);
```

## API Changes

The `oauth_revoke_token` API command has changed its returned result. Check [oauth_revoke_token](https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#oauth-revoke-token) documentation.



# Upgrade to ejabberd 21.12

## PostgreSQL new schema

If you migrated your PostgreSQL database from old to new schema using previous ejabberd versions,
your database may be missing the migration steps for the `push_session` table.
You can update it now with:
``` sql
ALTER TABLE push_session ADD COLUMN server_host text NOT NULL DEFAULT '<HOST>';
DROP INDEX i_push_usn;
DROP INDEX i_push_ut;
ALTER TABLE push_session ADD PRIMARY KEY (server_host, username, timestamp);
CREATE UNIQUE INDEX i_push_session_susn ON push_session USING btree (server_host, username, service, node);
```

In the PostgreSQL new schema, the primary key for the `vcard_search` table was wrong.
How to update an existing database:

``` sql
ALTER TABLE vcard_search DROP CONSTRAINT vcard_search_pkey;
ALTER TABLE vcard_search ADD PRIMARY KEY (server_host, lusername);
```

## mod_register_web restrictions

[mod_register_web](../../admin/configuration/modules.md#mod_register_web)
is now affected by the restrictions that you configure in
[mod_register](../../admin/configuration/modules.md#mod_register).


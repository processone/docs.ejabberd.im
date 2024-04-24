# Upgrade to ejabberd 18.03

You should follow this procedure if you are upgrading from ejabberd 18.01
and running an SQL backend for archives (mod_mam).
You should also have a look at all new configuration options.

## Database

There is a change on indexes of archive table on SQL backend.
Peer is now indexed and old indexes must be updated to reflect this change.
You must open an sql client connected to your ejabberd database, and run the
following commands.
Note: if your archive table is big, this action may take a while to complete.

### MySQL
``` bash
DROP INDEX i_username_timestamp ON archive;
DROP INDEX i_peer ON archive;
DROP INDEX i_bare_peer ON archive;
CREATE INDEX i_username_timestamp USING BTREE ON archive(username(191), timestamp);
CREATE INDEX i_username_peer USING BTREE ON archive(username(191), peer(191));
CREATE INDEX i_username_bare_peer USING BTREE ON archive(username(191), bare_peer(191));
```

### PostgreSQL
``` bash
DROP INDEX i_peer ON archive;
DROP INDEX i_bare_peer ON archive;
CREATE INDEX i_username_peer ON archive USING btree (username, peer);
CREATE INDEX i_username_bare_peer ON archive USING btree (username, bare_peer);
```

### Sqlite
``` bash
DROP INDEX i_peer ON archive;
DROP INDEX i_bare_peer ON archive;
CREATE INDEX i_archive_username_peer ON archive (username, peer);
CREATE INDEX i_archive_username_bare_peer ON archive (username, bare_peer);
```

### MsSql
``` bash
DROP INDEX [archive_username_timestamp] ON [archive];
DROP INDEX [archive_peer] ON [archive];
DROP INDEX [archive_bare_peer] ON [archive];
CREATE INDEX [archive_username_peer] ON [archive] (username, peer)
 WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);
CREATE INDEX [archive_username_bare_peer] ON [archive] (username, bare_peer)
 WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);
CREATE INDEX [archive_timestamp] ON [archive] (timestamp)
 WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);
```

## ejabberd configuration

There are many new configuration option. We highly recommend to read details
on [release blogpost](https://www.process-one.net/blog/ejabberd-18-03/)
for good understanding of changes that may impact your configuration.

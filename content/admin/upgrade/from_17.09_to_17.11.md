# Upgrade to ejabberd 17.11

You should follow this procedure if you are upgrading from ejabberd 17.09
and running an SQL backend for archives (mod_mam) and/or PubSub (mod_pubsub)
and/or MucSub (mod_muc) and/or push (mod_push).

## System

For best experience with new ACME features, it's recommended to install
inotify-tools package on Linux. This allows automatic certificates reload.
Other systems should be fine without adding extra dependency.

## Database

There is a minor change on indexes of archive table on SQL backend. This
change gives speed improvements on archive requests. You have to open a client
connected to your ejabberd database, and run the following commands.
Note: if your archive table is big, this action may take a while to complete.

There is a column rename in pubsub_node table, to avoid using reserved words and
need extra quoting which may not be supported on all backends. You MUST apply
this upgrade procedure if you're using PubSub with an SQL backend.

There are two new tables, one which allows optimization on mucsub subscriptions,
and another one needed by mod_push.

### MySQL
``` bash
DROP INDEX i_username ON archive;
CREATE INDEX i_username_timestamp USING BTREE ON archive(username,timestamp);

ALTER TABLE pubsub_node CHANGE type plugin text NOT NULL;

CREATE TABLE muc_room_subscribers (
   room varchar(191) NOT NULL,
   host varchar(191) NOT NULL,
   jid varchar(191) NOT NULL,
   nick text NOT NULL,
   nodes text NOT NULL,
   created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE KEY i_muc_room_subscribers_host_room_jid (host, room, jid)
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_muc_room_subscribers_host_jid USING BTREE ON muc_room_subscribers(host, jid);

CREATE TABLE push_session (
    username text NOT NULL,
    timestamp bigint NOT NULL,
    service text NOT NULL,
    node text NOT NULL,
    xml text NOT NULL
);

CREATE UNIQUE INDEX i_push_usn ON push_session (username(191), service(191), node(191));
CREATE UNIQUE INDEX i_push_ut ON push_session (username(191), timestamp);
```

### PostgreSQL
``` bash
DROP INDEX i_username;
CREATE INDEX i_username_timestamp ON archive USING btree (username, timestamp);

ALTER TABLE pubsub_node RENAME COLUMN type TO plugin;

CREATE TABLE muc_room_subscribers (
   room text NOT NULL,
   host text NOT NULL,
   jid text NOT NULL,
   nick text NOT NULL,
   nodes text NOT NULL,
   created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_muc_room_subscribers_host_jid ON muc_room_subscribers USING btree (host, jid);
CREATE UNIQUE INDEX i_muc_room_subscribers_host_room_jid ON muc_room_subscribers USING btree (host, room, jid);

CREATE TABLE push_session (
    username text NOT NULL,
    timestamp bigint NOT NULL,
    service text NOT NULL,
    node text NOT NULL,
    xml text NOT NULL
);

CREATE UNIQUE INDEX i_push_usn ON push_session USING btree (username, service, node);
CREATE UNIQUE INDEX i_push_ut ON push_session USING btree (username, timestamp);
```

### SQLite
``` bash
DROP INDEX i_username ON archive;
CREATE INDEX i_username_timestamp ON archive(username, timestamp);

ALTER TABLE pubsub_node ADD plugin text NOT NULL;
UPDATE pubsub_node SET plugin = type;
ALTER TABLE pubsub_node DROP COLUMN type;

CREATE TABLE muc_room_subscribers (
   room text NOT NULL,
   host text NOT NULL,
   jid text NOT NULL,
   nick text NOT NULL,
   nodes text NOT NULL,
   created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX i_muc_room_subscribers_host_jid ON muc_room_subscribers(host, jid);
CREATE UNIQUE INDEX i_muc_room_subscribers_host_room_jid ON muc_room_subscribers(host, room, jid);

CREATE TABLE push_session (
    username text NOT NULL,
    timestamp bigint NOT NULL,
    service text NOT NULL,
    node text NOT NULL,
    xml text NOT NULL
);

CREATE UNIQUE INDEX i_push_usn ON push_session (username, service, node);
CREATE UNIQUE INDEX i_push_ut ON push_session (username, timestamp);
```

### MsSQL
``` bash
DROP INDEX [archive_username] ON [archive];
CREATE INDEX [archive_username_timestamp] ON [archive] (username, timestamp)
 WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);
 
EXEC sp_rename '[dbo].[pubsub_node].[type]', 'plugin';

CREATE TABLE [dbo].[muc_room_subscribers] (
        [room] [varchar] (191) NOT NULL,
        [host] [varchar] (191) NOT NULL,
        [jid] [varchar] (191) NOT NULL,
        [nick] [text] NOT NULL,
        [nodes] [text] NOT NULL,
        [created_at] [datetime] NOT NULL DEFAULT GETDATE()
);

CREATE UNIQUE CLUSTERED INDEX [muc_room_subscribers_host_room_jid] ON [muc_room_subscribers] (host, room, jid);
CREATE INDEX [muc_room_subscribers_host_jid] ON [muc_room_subscribers] (host, jid);

CREATE TABLE [dbo].[push_session] (
    [username] [varchar] (255) NOT NULL,
    [timestamp] [bigint] NOT NULL,
    [service] [varchar] (255) NOT NULL,
    [node] [varchar] (255) NOT NULL,
    [xml] [varchar] (255) NOT NULL
);

CREATE UNIQUE CLUSTERED INDEX [i_push_usn] ON [push_session] (username, service, node)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);

CREATE UNIQUE CLUSTERED INDEX [i_push_ut] ON [push_session] (username, timestamp)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);
```

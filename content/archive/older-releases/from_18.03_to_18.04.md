# Upgrade to ejabberd 18.04

You may follow this procedure if you are upgrading from ejabberd 18.03 or
older, and running an SQL backend for PubSub.

## Database

There is a change on pubsub_item table on SQL backend.
The type of creation and modification changed from 'text' to 'varchar(32)'.
This change will speedup requests reading and sorting items.
Note: if you're happy with performances, you don't need to apply this change.

### MySQL
``` bash
ALTER TABLE pubsub_item
 MODIFY creation varchar(32) NOT NULL,
 MODIFY modification varchar(32) NOT NULL;
```

### PostgreSQL
``` bash
ALTER TABLE pubsub_item
 ALTER COLUMN creation TYPE varchar(32),
 ALTER COLUMN creation SET NOT NULL,
 ALTER COLUMN modification TYPE varchar(32),
 ALTER COLUMN modification SET NOT NULL;
```

### Sqlite
``` bash
ALTER TABLE pubsub_item
 MODIFY creation varchar(32) NOT NULL,
 MODIFY modification varchar(32) NOT NULL;
```

### MsSql

Note: We do not provide tested upgrade procedure on MsSQL Server.
Following query should to the conversion. If you have problems with it please
create an issue on ejabberd's github page.
``` bash
ALTER TABLE [pubsub_item]
 ALTER COLUMN creation varchar(32) NOT NULL,
 ALTER COLUMN modification varchar(32) NOT NULL;
```

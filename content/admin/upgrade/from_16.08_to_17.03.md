# Upgrade to ejabberd 17.03

If you are upgrading from ejabberd 16.08, 16.09, 16.12 or 17.01, and
are using an SQL backend, you need to alter tables for better
PubSub support before starting ejabberd 17.03.

## MySQL database upgrade

If you're running MySQL, this change in not mandatory but highly recommended

``` bash
mysql -h host -u user database -p << EOF
ALTER TABLE rosterusers MODIFY subscribe text NOT NULL;

UPDATE pubsub_node SET parent='' WHERE parent=NULL;

ALTER TABLE pubsub_node
 MODIFY host TEXT NOT NULL,
 MODIFY node TEXT NOT NULL,
 MODIFY parent VARCHAR(191) NOT NULL DEFAULT '',
 MODIFY type TEXT NOT NULL;

ALTER TABLE pubsub_node_option 
 MODIFY name text NOT NULL,
 MODIFY val text NOT NULL;

ALTER TABLE pubsub_node_owner
 MODIFY owner text NOT NULL;

UPDATE pubsub_state SET subscriptions='' WHERE subscriptions=NULL;

ALTER TABLE pubsub_state
 MODIFY jid text NOT NULL,
 MODIFY subscriptions VARCHAR(191) NOT NULL DEFAULT '';

ALTER TABLE pubsub_item
 MODIFY itemid text NOT NULL,
 MODIFY publisher text NOT NULL,
 MODIFY creation text NOT NULL,
 MODIFY modification text NOT NULL,
 MODIFY payload text NOT NULL;

ALTER TABLE pubsub_subscription_opt
 MODIFY subid text NOT NULL,
 MODIFY opt_value text NOT NULL;
EOF
```

## PostgreSQL database upgrade

If you're running PostgreSQL, this change is mandatory.

``` bash
psql -W -h host database user << EOF
ALTER TABLE rosterusers ALTER COLUMN subscribe SET NOT NULL;

UPDATE pubsub_node SET parent='' WHERE parent=NULL;

ALTER TABLE pubsub_node
ALTER COLUMN host SET NOT NULL,
ALTER COLUMN node SET NOT NULL,
ALTER COLUMN parent SET NOT NULL,
ALTER COLUMN parent SET DEFAULT '',
ALTER COLUMN type SET NOT NULL;

ALTER TABLE pubsub_node_option 
ALTER COLUMN name SET NOT NULL,
ALTER COLUMN val SET NOT NULL;

ALTER TABLE pubsub_node_owner
ALTER COLUMN  owner SET NOT NULL;

UPDATE pubsub_state SET subscriptions='' WHERE subscriptions=NULL;

ALTER TABLE pubsub_state
ALTER COLUMN jid SET NOT NULL,
ALTER COLUMN subscriptions SET NOT NULL,
ALTER COLUMN subscriptions SET DEFAULT '';

ALTER TABLE pubsub_item
ALTER COLUMN itemid SET NOT NULL,
ALTER COLUMN publisher SET NOT NULL,
ALTER COLUMN creation SET NOT NULL,
ALTER COLUMN modification SET NOT NULL,
ALTER COLUMN payload SET NOT NULL;

ALTER TABLE pubsub_subscription_opt
ALTER COLUMN subid SET NOT NULL,
ALTER COLUMN opt_value SET NOT NULL;
EOF
```

## SQLite database upgrade

If you're running SQLite, you have to create a new database with schema file
provided in ejabberd 17.03 sources. Then you have to export all data from your
current database and import into the newly created database.

## MsSQL database upgrade

We do not provide tested upgrade procedure on MsSQL Server.
The upgrade may not be mandatory (this was not tested) but highly recommended.
You have to create a new database with schema file provided in ejabberd 17.03
sources. Then you have to export all data from your current database and import
into the newly created database.

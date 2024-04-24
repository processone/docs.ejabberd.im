# Upgrade to ejabberd 18.09

You need to update your database schema if you're using MySQL. Else there is
no special upgrade process for this version.

## Database

### MySQL

When using stanza larger than 64 KiB, payload were silently trunced by MySQL.
This raised errors when ejabberd was trying to read the content of the database.

You need to alter your MySQL schema if you're using big stanzas and archive on
offline table. Else you can simply dismiss this change.
Note: this operation can take a long time with large archives.

``` bash
mysql -h host -u user database -p << EOF
ALTER TABLE spool MODIFY xml mediumtext NOT NULL;
ALTER TABLE archive MODIFY xml mediumtext NOT NULL;
ALTER TABLE archive MODIFY txt mediumtext;
ALTER TABLE pubsub_item MODIFY payload mediumtext NOT NULL;
EOF
```

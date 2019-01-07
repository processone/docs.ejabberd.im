---
title: Upgrade to ejabberd 18.12
---

# Ejabberd upgrade process

You may need to update your database schema if you're using MySQL.
A minor change on pubsub_item table was added right after 18.09 release and
your schema may not have been updated already.

If your users are using bookmarks, you also must migrate them to PEP before
starting the service.

If you're using ProcessOne installers, you MUST be aware API and all web
ports are now using TLS, and served on port 5443 instead of 5280.
If you're a packager, we highly recommend to apply this change as well.

## Bookmarks and XEP-0411

As ejabberd now supports XEP-0411, you must perform some actions before starting the service.
If your users are NOT USING bookmarks, you can dismiss this.
```bash
$ for host in $(ejabberdctl registered-vhosts); do
      ejabberdctl registered-users "$host" | while read user; do
          ejabberdctl bookmarks-to-pep "$user" "$host"
      done
  done
```
This might take a while if the number of users is large. Also note that this will overwrite any preexisting PEP bookmarks.
However, if this is not performed, users might end up with empty bookmark lists after upgrading to the new ejabberd version,
as clients might rely on PEP bookmarks once they detect that ejabberd's XEP-0411 support.  

## Database

### MySQL

When using stanza larger than 64 KiB, payload were silently trunced by MySQL.
This raised errors when ejabberd was trying to read the content of the database.

You not done yet, you need to alter your MySQL schema if you're using big stanzas
on for pubsub items. Else you can simply dismiss this change.

```bash
mysql -h host -u user database -p << EOF
ALTER TABLE pubsub_item MODIFY payload mediumtext NOT NULL;
EOF
```

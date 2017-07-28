---
title: Upgrade to ejabberd 17.06
---

# Ejabberd upgrade process

## ejabberdctl script

Due to a major refactor of the ejabberdctl script, which also remove
all bashismes, you should check your packaging and your system's version of
ejabberdctl script. While old script still works, you are encouraged to use
the new one. This may depends on media you install ejabberd from.

## Database

There are no changes on database schema since 17.04 which requires
migration procedure. However, we removed the `vcard_xupdate` table.
If you want to cleanup your database, you can remove that table,
as it is no longer used by the module.

---
title: Upgrade to ejabberd 21.07
toc: true
---

## Database changes

### Add missing indexes to SQL `sr_group` table

The `sr_group` table in SQL databases got a new index.

If you store Shared Roster Groups in a SQL database, you can create the index corresponding to your database type, check the example SQL queries in [the commit](https://github.com/processone/ejabberd/commit/95fa43aa96514b7e8b77fa7c29d2c0b5b1c1331a).


### MySQL Backend Patch for scram-sha512

The MySQL database schema has improved to support scram-sha512 ([#3582](https://github.com/processone/ejabberd/issues/3582))

If you have a MySQL database, you can update your schema with:
```sql
ALTER TABLE users MODIFY serverkey varchar(128) NOT NULL DEFAULT ’’;
ALTER TABLE users MODIFY salt varchar(128) NOT NULL DEFAULT ’’;
```

## API change

### Change in `srg_create`

That command API has changed the `name` argument to `label`, for coherence with the changes in the ejabberd Web Admin page.

Check `ejabberdctl help srg_create` or the [API page](https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#srg-create).

## Command-line change

### Changes in `ejabberdctl help`

The `help` command has changed its usage and results.

If you use that command in some automatic script, please check `ejabberdctl help` and adapt your scripts accordingly.


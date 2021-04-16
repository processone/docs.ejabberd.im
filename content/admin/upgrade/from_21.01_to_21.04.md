---
title: Upgrade to ejabberd 21.04
---

## Database changes

### MySQL

When migrating from any recent version to 21.04, you may want to apply this
MySQL database definition improvement.

We updated the database definition to fix the `specified key was too long`
warnings.
By default, the new character set and collation
(`utf8mb4` and `utf8mb4_unicode_ci`)
will only be used with newly created databases.
The existing installations don’t need to convert anything.

However, if you feel like it, after you upgrade to ejabberd 21.04,
you can apply the following SQL command to convert
your existing MySQL database character set to the latest definition:

```sql
alter table push_session convert to character set utf8mb4 collate utf8mb4_unicode_ci;
alter table mqtt_pub convert to character set utf8mb4 collate utf8mb4_unicode_ci;
```


# Upgrade to ejabberd 25.10

If you upgrade ejabberd from a previous release to [25.10](../../archive/25.10/index.md),
there are no mandatory changes in SQL schemas, configuration, API commands or hooks.

But if you are using the new SQL schema, it's recommended to update your setup
to use the new arguments and option:

When preparing configuration, replace the previous argument with the current argument:

```
./configure --enable-new-sql-schema
./configure --enable-multihost-sql-schema
```

When configuring ejabberd, replace the previous option with the current option:

```
new_sql_schema: true
sql_schema_multihost: true
```

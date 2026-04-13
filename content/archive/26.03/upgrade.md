# Upgrade to ejabberd 26.03

If you upgrade ejabberd from a previous release to [26.03](../../archive/26.03/index.md),
please review those changes:

- Update the SQL schema
- SASL channel binding changes

## <a name="sql"></a>Update the SQL schema

The table `rosterusers` has a new boolean column `approved` (see [commit 47d898bb](https://github.com/processone/ejabberd/commit/47d898bb)). You have two methods to update the SQL schema of your existing database:

If using MySQL, PosgreSQL or SQLite, you can enable the option [`update_sql_schema`](../../admin/configuration/toplevel.md#update_sql_schema) and ejabberd will take care to update the SQL schema when needed: add in your ejabberd configuration file the line `update_sql_schema: true`

If you are using other database, or prefer to update manually the SQL schema:

``` sql
ALTER TABLE rosterusers ADD COLUMN approved boolean NOT NULL DEFAULT false;
ALTER TABLE rosterusers ALTER COLUMN approved DROP DEFAULT;```

You can ignore the second query on SQLite.

## <a name="sasl"></a> SASL channel binding changes

This version adds ability to configure handling of client flag "wanted to use channel-bindings but was not offered one".
By default ejabberd will abort connections that present this flag, as that could mean that between server and client is
rogue MITM proxy that strips exchanged data with informations that are required for this. 

This can cause problems for servers that use proxy server that terminated TLS connection (there is MITM proxy, but approved by server admin). To be able to handle this situation, we added code that ignore this flag, if server admin disable channel-binding handling by disabling -PLUS auth mechanisms in config file:

```yaml
disable_sasl_mechanisms:
  - SCRAM-SHA-1-PLUS
  - SCRAM-SHA-256-PLUS
  - SCRAM-SHA-512-PLUS
```

We also ignore this flag for SASL2 connections if offered authentication methods filtered by available user passwords did disable all -PLUS mechanisms.

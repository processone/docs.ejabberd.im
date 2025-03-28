# Upgrade to ejabberd 25.03

If you upgrade ejabberd from a previous release to [25.03](../../archive/25.03/index.md),
please review those changes:

- Update the SQL schema

## <a name="sql"></a>Update the SQL schema

This release requires SQL database schema update to allow storage of multiple passwords per user. This task can be performed automatically by ejabberd, if your config has enabled [`update_sql_schema`](https://docs.ejabberd.im/admin/configuration/toplevel/#update_sql_schema) toplevel option.

If you prefer to perform the SQL schema update manually yourself, check the corresponding instructions, depending if your config has enabled [`new_sql_schema`](https://docs.ejabberd.im/admin/configuration/toplevel/#new_sql_schema):

* MySQL default schema:
```sql
ALTER TABLE users ADD COLUMN type smallint NOT NULL DEFAULT 0;
ALTER TABLE users ALTER COLUMN type DROP DEFAULT;
ALTER TABLE users DROP PRIMARY KEY, ADD PRIMARY KEY (username(191), type);
```

* MySQL new schema:
```sql
ALTER TABLE users ADD COLUMN type smallint NOT NULL DEFAULT 0;
ALTER TABLE users ALTER COLUMN type DROP DEFAULT;
ALTER TABLE users DROP PRIMARY KEY, ADD PRIMARY KEY (server_host(191), username(191), type);
```

* PostgreSQL default schema:
```sql
ALTER TABLE users ADD COLUMN "type" smallint NOT NULL DEFAULT 0;
ALTER TABLE users ALTER COLUMN type DROP DEFAULT;
ALTER TABLE users DROP CONSTRAINT users_pkey, ADD PRIMARY KEY (username, type);
```

* PostgreSQL new schema:
```sql
ALTER TABLE users ADD COLUMN "type" smallint NOT NULL DEFAULT 0;
ALTER TABLE users ALTER COLUMN type DROP DEFAULT;
ALTER TABLE users DROP CONSTRAINT users_pkey, ADD PRIMARY KEY (server_host, username, type);
```

* SQLite default schema:
```sql
ALTER TABLE users ADD COLUMN type smallint NOT NULL DEFAULT 0;
CREATE TABLE new_users (
    username text NOT NULL,
    type smallint NOT NULL,
    password text NOT NULL,
    serverkey text NOT NULL DEFAULT '',
    salt text NOT NULL DEFAULT '',
    iterationcount integer NOT NULL DEFAULT 0,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (username, type)
);
INSERT INTO new_users SELECT * FROM users;
DROP TABLE users;
ALTER TABLE new_users RENAME TO users;
```

* SQLite new schema:
```sql
ALTER TABLE users ADD COLUMN type smallint NOT NULL DEFAULT 0;
CREATE TABLE new_users (
    username text NOT NULL,
    server_host text NOT NULL,
    type smallint NOT NULL,
    password text NOT NULL,
    serverkey text NOT NULL DEFAULT '',
    salt text NOT NULL DEFAULT '',
    iterationcount integer NOT NULL DEFAULT 0,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (server_host, username, type)
);
INSERT INTO new_users SELECT * FROM users;
DROP TABLE users;
ALTER TABLE new_users RENAME TO users;
```


# Upgrade to ejabberd 26.01

If you upgrade ejabberd from a previous release to [26.01](../../archive/26.01/index.md),
please check the proposed changes in SQL schema below.

There are no mandatory changes in SQL schemas, configuration, API commands or hooks.

## <a name="sql"></a>SQL table for `mod_invites`

There is a new table `invite_token` in SQL schemas, used by the new `mod_invites`.
If you want to use this module,
there are two methods to update the SQL schema of your existing database:

If using MySQL or PosgreSQL, you can enable the option [`update_sql_schema`](../../admin/configuration/toplevel.md#update_sql_schema) and ejabberd will take care to update the SQL schema when needed: add in your ejabberd configuration file the line `update_sql_schema: true`

Notice that support for MSSQL in `mod_invites` has not yet been implemented or tested.

If you are using other database, or prefer to update manually the SQL schema:

* MySQL singlehost schema:

    ``` sql
    CREATE TABLE invite_token (
        token text NOT NULL,
        username text NOT NULL,
        invitee text NOT NULL DEFAULT (''),
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        expires timestamp NOT NULL,
        type character(1) NOT NULL,
        account_name text NOT NULL,
        PRIMARY KEY (token(191))
    ) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

    CREATE INDEX i_invite_token_username USING BTREE ON invite_token(username(191));
    ```

* MySQL multihost schema:

    ``` sql
    CREATE TABLE invite_token (
        token text NOT NULL,
        username text NOT NULL,
        server_host varchar(191) NOT NULL,
        invitee text NOT NULL DEFAULT '',
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        expires timestamp NOT NULL,
        type character(1) NOT NULL,
        account_name text NOT NULL,
        PRIMARY KEY (token(191)),
    ) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

    CREATE INDEX i_invite_token_username USING BTREE ON invite_token(username(191));
    ```

* PostgreSQL singlehost schema:

    ``` sql
    CREATE TABLE invite_token (
        token text NOT NULL,
        username text NOT NULL,
        invitee text NOT NULL DEFAULT '',
        created_at timestamp NOT NULL DEFAULT now(),
        expires timestamp NOT NULL,
        "type" character(1) NOT NULL,
        account_name text NOT NULL,
        PRIMARY KEY (token)
    );
    CREATE INDEX i_invite_token_username ON invite_token USING btree (username);
    ```

* PostgreSQL multihost schema:

    ``` sql
    CREATE TABLE invite_token (
        token text NOT NULL,
        username text NOT NULL,
        server_host text NOT NULL,
        invitee text NOT NULL DEFAULT '',
        created_at timestamp NOT NULL DEFAULT now(),
        expires timestamp NOT NULL,
        "type" character(1) NOT NULL,
        account_name text NOT NULL,
        PRIMARY KEY (token)
    );

    CREATE INDEX i_invite_token_username_server_host ON invite_token USING btree (username, server_host);
    ```

* SQLite singlehost schema:

    ``` sql
    CREATE TABLE invite_token (
        token text NOT NULL,
        username text NOT NULL,
        invitee text NOT NULL DEFAULT '',
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        expires timestamp NOT NULL,
        type character(1) NOT NULL,
        account_name text NOT NULL,
        PRIMARY KEY (token)
    );

    CREATE INDEX i_invite_token_username ON invite_token(username);
    ```

* SQLite multihost schema:

    ``` sql
    CREATE TABLE invite_token (
        token text NOT NULL,
        username text NOT NULL,
        server_host text NOT NULL,
        invitee text NOT NULL DEFAULT '',
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        expires timestamp NOT NULL,
        type character(1) NOT NULL,
        account_name text NOT NULL,
        PRIMARY KEY (token)
    );

    CREATE INDEX i_invite_token_username_server_host ON invite_token(username, server_host);
    ```

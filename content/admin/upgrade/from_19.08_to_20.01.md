# Upgrade to ejabberd 20.01

## Database changes

To migrate from 19.08 (or 19.09) to 20.01, you have to use the following commands on your existing database, after you’ve made a backup of it:

### MySQL

If you are using the legacy `mysql.sql` schema:

``` sql
ALTER TABLE  oauth_client CHANGE `client` `client_id` text PRIMARY KEY;
ALTER TABLE  oauth_client CHANGE `secret` `client_name` text NOT NULL;
```

If you are using the newer `mysql.new.sql` schema:

``` sql
CREATE TABLE oauth_client (
   client_id varchar(191) NOT NULL PRIMARY KEY,
   client_name text NOT NULL,
   grant_type text NOT NULL,
   options text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
```

### PostgreSQL

``` sql
CREATE TABLE oauth_client (
    client_id text PRIMARY KEY,
    client_name text NOT NULL,
    grant_type text NOT NULL,
    options text NOT NULL
);
ALTER TABLE oauth_client RENAME COLUMN client TO client_id;
ALTER TABLE oauth_client RENAME COLUMN secret TO client_name;
```

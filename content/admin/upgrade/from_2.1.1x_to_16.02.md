---
title: Upgrade from 2.1.1x to 16.02
---

# Compilation

Compilation requires Erlang/OTP R17 or higher.
Requires OpenSSL 1.0.0 or higher.
exmpp is not required anymore.
The mysql and pgsql erlang libraries are now included in ejabberd source code.

The source code structure has improved:
- The compilation scripts are moved from src/ to /, including configure and Makefile.
- Source code of complex modules is moved from `src/*/*.erl` to `src/*.erl`
- The SQL example files are moved from `src/odbc/*.sql` to `sql/*.sql`

The configuration file now uses YAML syntax, not the Erlang syntax.
For that reason, the file is now named ejabberd.yml, not ejabberd.cfg
You have three options:
- You can use and adapt the new file ejabberd.yml.example
- You can still use your old ejabberd.cfg
- You can convert your old config file to YAML with the command: `ejabberdctl convert_to_yaml`

# Ejabberd upgrade process

If you are using an sql backend for authentication, you need to upgrade
your schema before starting ejabberd 16.02. This can be safely done
while a previous version of ejabberd is actually running.

## MySQL database upgrade

```bash
mysql -h host -u user database -p << EOF

-- Table for mod_caps: Entity Capabilities (XEP-0115)
CREATE TABLE caps_features (
    node varchar(250) NOT NULL,
    subnode varchar(250) NOT NULL,
    feature text,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;
CREATE INDEX i_caps_features_node_subnode ON caps_features(node(75), subnode(75));

-- Make it possible to use SQL as an SM backend:
CREATE TABLE sm (
    usec bigint NOT NULL,
    pid text NOT NULL,
    node text NOT NULL,
    username varchar(250) NOT NULL,
    resource varchar(250) NOT NULL,
    priority text NOT NULL,
    info text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8;
CREATE UNIQUE INDEX i_sid ON sm(usec, pid(75));
CREATE INDEX i_node ON sm(node(75));
CREATE INDEX i_username ON sm(username);

-- Support delete_old_messages (offline) command:
CREATE INDEX i_spool_created_at USING BTREE ON spool(created_at);

-- Add a missed SQL index on privacy_list_data table
CREATE INDEX i_privacy_list_data_id ON privacy_list_data(id);

-- Add SCRAM support to ejabberd_auth_odbc
ALTER TABLE users ADD COLUMN serverkey text NOT NULL;
ALTER TABLE users ADD COLUMN salt text NOT NULL;
ALTER TABLE users ADD COLUMN iterationcount integer NOT NULL DEFAULT 0;

-- mod_mam (XEP-0313) support
CREATE TABLE archive (
    username varchar(250) NOT NULL,
    timestamp BIGINT UNSIGNED NOT NULL,
    peer varchar(250) NOT NULL,
    bare_peer varchar(250) NOT NULL,
    xml text NOT NULL,
    txt text,
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE,
    kind varchar(10),
    nick varchar(250),
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;
CREATE FULLTEXT INDEX i_text ON archive(txt);
CREATE INDEX i_username USING BTREE ON archive(username);
CREATE INDEX i_timestamp USING BTREE ON archive(timestamp);
CREATE INDEX i_peer USING BTREE ON archive(peer);
CREATE INDEX i_bare_peer USING BTREE ON archive(bare_peer);
CREATE TABLE archive_prefs (
    username varchar(250) NOT NULL PRIMARY KEY,
    def text NOT NULL,
    always text NOT NULL,
    never text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8;

-- In Offline storage use BLOB instead of TEXT
ALTER TABLE spool MODIFY xml BLOB NOT NULL;

-- Use UTF8MB4 character set in MySQL tables
--
-- TODO
-- Commit: Use UTF8MB4 character set in MySQL tables
--         7d1c75d0e817db0e04fe1133e06ba35cd367d76e
--
EOF
```


## PgSQL database upgrade

```bash
psql -W -h host database user << EOF

-- Table for mod_caps: Entity Capabilities (XEP-0115)
CREATE TABLE caps_features (
    node text NOT NULL,
    subnode text NOT NULL,
    feature text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);
CREATE INDEX i_caps_features_node_subnode ON caps_features USING btree (node, subnode);

-- Make it possible to use SQL as an SM backend:
CREATE TABLE sm (
    usec bigint NOT NULL,
    pid text NOT NULL,
    node text NOT NULL,
    username text NOT NULL,
    resource text NOT NULL,
    priority text NOT NULL,
    info text NOT NULL
);
CREATE UNIQUE INDEX i_sm_sid ON sm USING btree (usec, pid);
CREATE INDEX i_sm_node ON sm USING btree (node);
CREATE INDEX i_sm_username ON sm USING btree (username);

-- Add a missed SQL index on privacy_list_data table
CREATE INDEX i_privacy_list_data_id ON privacy_list_data USING btree (id);

-- Add SCRAM support to ejabberd_auth_odbc
ALTER TABLE users ADD COLUMN serverkey text NOT NULL DEFAULT '';
ALTER TABLE users ADD COLUMN salt text NOT NULL DEFAULT '';
ALTER TABLE users ADD COLUMN iterationcount integer NOT NULL DEFAULT 0;

-- mod_mam (XEP-0313) support
CREATE TABLE archive (
    username text NOT NULL,
    timestamp BIGINT NOT NULL,
    peer text NOT NULL,
    bare_peer text NOT NULL,
    xml text NOT NULL,
    txt text,
    id SERIAL,
    kind text,
    nick text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);
CREATE INDEX i_username ON archive USING btree (username);
CREATE INDEX i_timestamp ON archive USING btree (timestamp);
CREATE INDEX i_peer ON archive USING btree (peer);
CREATE INDEX i_bare_peer ON archive USING btree (bare_peer);
CREATE TABLE archive_prefs (
    username text NOT NULL PRIMARY KEY,
    def text NOT NULL,
    always text NOT NULL,
    never text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);
EOF
```

## PubSub database upgrade

If you were using PubSub with sql backend and need to keep your pubsub data,
you must alter the content of pubsub_node tables, changing type 'pep' instead
of 'pep_odbc', 'flat' instead of 'flat_odbc', and 'hometree' instead of 'hometree_odbc'.

```bash
UPDATE pubsub_node SET type='pep' WHERE type='pep_odbc';
UPDATE pubsub_node SET type='flat' WHERE type='flat_odbc';
UPDATE pubsub_node SET type='hometree' WHERE type='hometree_odbc';
```

---
title: Upgrade to ejabberd 19.02
---

# Ejabberd upgrade process

19.02 adds support of latest MIX specification and MQTT protocol.
You may improve your current configuration or SQL schema depending on your needs.

## Configuration

To enable MQTT, you need to add a new listener and enable mod_mqtt.
Here is example of configuration:

```yaml
listen:
  -
    port: 1883
    ip: "::"
    module: mod_mqtt
    backlog: 1000

modules:
  mod_mqtt: {}
```

## Database

If you want to use lastest MIX (XEP-0369) with an SQL backend then
you have to create new tables.

If you have issues using PEP and MySQL, you should update your schema.

### MySQL

When using PEP with MySQL, due to limitation in size of indexes, some long JIDs could have broken support.
Index size on pubsub_node was modified to fix use of PEP. This change is optional and required
only if you have issues when using PEP.

```bash
mysql -h host -u user database -p << EOF
DROP INDEX i_pubsub_node_tuple ON pubsub_node;
CREATE UNIQUE INDEX i_pubsub_node_tuple ON pubsub_node(host(71), node(120));
EOF
```

If you want to use new MIX implementation, you must create some tables.

```sql
CREATE TABLE mix_channel (
    channel text NOT NULL,
    service text NOT NULL,
    username text NOT NULL,
    domain text NOT NULL,
    jid text NOT NULL,
    hidden boolean NOT NULL,
    hmac_key text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_mix_channel ON mix_channel (channel(191), service(191));
CREATE INDEX i_mix_channel_serv ON mix_channel (service(191));

CREATE TABLE mix_participant (
    channel text NOT NULL,
    service text NOT NULL,
    username text NOT NULL,
    domain text NOT NULL,
    jid text NOT NULL,
    id text NOT NULL,
    nick text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_mix_participant ON mix_participant (channel(191), service(191), username(191), domain(191));
CREATE INDEX i_mix_participant_chan_serv ON mix_participant (channel(191), service(191));

CREATE TABLE mix_subscription (
    channel text NOT NULL,
    service text NOT NULL,
    username text NOT NULL,
    domain text NOT NULL,
    node text NOT NULL,
    jid text NOT NULL
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_mix_subscription ON mix_subscription (channel(153), service(153), username(153), domain(153), node(153));
CREATE INDEX i_mix_subscription_chan_serv_ud ON mix_subscription (channel(191), service(191), username(191), domain(191));
CREATE INDEX i_mix_subscription_chan_serv_node ON mix_subscription (channel(191), service(191), node(191));
CREATE INDEX i_mix_subscription_chan_serv ON mix_subscription (channel(191), service(191));

CREATE TABLE mix_pam (
    username text NOT NULL,
    channel text NOT NULL,
    service text NOT NULL,
    id text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE UNIQUE INDEX i_mix_pam ON mix_pam (username(191), channel(191), service(191));
CREATE INDEX i_mix_pam_u ON mix_pam (username(191));
```

If you want to use new MQTT feature, you need to create a table:

```sql
CREATE TABLE mqtt_pub (
    username varchar(191) NOT NULL,
    server_host varchar(191) NOT NULL,
    resource varchar(191) NOT NULL,
    topic text NOT NULL,
    qos tinyint NOT NULL,
    payload blob NOT NULL,
    payload_format tinyint NOT NULL,
    content_type text NOT NULL,
    response_topic text NOT NULL,
    correlation_data blob NOT NULL,
    user_properties blob NOT NULL,
    expiry int unsigned NOT NULL,
    UNIQUE KEY i_mqtt_topic_server (topic(191))
);
```

### PostgreSQL

If you want to use new MIX implementation, you must create some tables.

```sql
CREATE TABLE mix_channel (
    channel text NOT NULL,
    service text NOT NULL,
    username text NOT NULL,
    domain text NOT NULL,
    jid text NOT NULL,
    hidden boolean NOT NULL,
    hmac_key text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_mix_channel ON mix_channel (channel, service);
CREATE INDEX i_mix_channel_serv ON mix_channel (service);

CREATE TABLE mix_participant (
    channel text NOT NULL,
    service text NOT NULL,
    username text NOT NULL,
    domain text NOT NULL,
    jid text NOT NULL,
    id text NOT NULL,
    nick text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_mix_participant ON mix_participant (channel, service, username, domain);
CREATE INDEX i_mix_participant_chan_serv ON mix_participant (channel, service);

CREATE TABLE mix_subscription (
    channel text NOT NULL,
    service text NOT NULL,
    username text NOT NULL,
    domain text NOT NULL,
    node text NOT NULL,
    jid text NOT NULL
);

CREATE UNIQUE INDEX i_mix_subscription ON mix_subscription (channel, service, username, domain, node);
CREATE INDEX i_mix_subscription_chan_serv_ud ON mix_subscription (channel, service, username, domain);
CREATE INDEX i_mix_subscription_chan_serv_node ON mix_subscription (channel, service, node);
CREATE INDEX i_mix_subscription_chan_serv ON mix_subscription (channel, service);

CREATE TABLE mix_pam (
    username text NOT NULL,
    channel text NOT NULL,
    service text NOT NULL,
    id text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_mix_pam ON mix_pam (username, channel, service);
CREATE INDEX i_mix_pam_us ON mix_pam (username);
```

If you want to use new MQTT feature, you need to create a table:

```sql
CREATE TABLE mqtt_pub (
    username text NOT NULL,
    server_host text NOT NULL,
    resource text NOT NULL,
    topic text NOT NULL,
    qos smallint NOT NULL,
    payload bytea NOT NULL,
    payload_format smallint NOT NULL,
    content_type text NOT NULL,
    response_topic text NOT NULL,
    correlation_data bytea NOT NULL,
    user_properties bytea NOT NULL,
    expiry bigint NOT NULL
);

CREATE UNIQUE INDEX i_mqtt_topic_server ON mqtt_pub (topic, server_host);
```

### SQLite

If you want to use new MIX implementation, you must create some tables.

```sql
CREATE TABLE mix_channel (
    channel text NOT NULL,
    service text NOT NULL,
    username text NOT NULL,
    domain text NOT NULL,
    jid text NOT NULL,
    hidden boolean NOT NULL,
    hmac_key text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_mix_channel ON mix_channel (channel, service);
CREATE INDEX i_mix_channel_serv ON mix_channel (service);

CREATE TABLE mix_participant (
    channel text NOT NULL,
    service text NOT NULL,
    username text NOT NULL,
    domain text NOT NULL,
    jid text NOT NULL,
    id text NOT NULL,
    nick text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_mix_participant ON mix_participant (channel, service, username, domain);
CREATE INDEX i_mix_participant_chan_serv ON mix_participant (channel, service);

CREATE TABLE mix_subscription (
    channel text NOT NULL,
    service text NOT NULL,
    username text NOT NULL,
    domain text NOT NULL,
    node text NOT NULL,
    jid text NOT NULL
);

CREATE UNIQUE INDEX i_mix_subscription ON mix_subscription (channel, service, username, domain, node);
CREATE INDEX i_mix_subscription_chan_serv_ud ON mix_subscription (channel, service, username, domain);
CREATE INDEX i_mix_subscription_chan_serv_node ON mix_subscription (channel, service, node);
CREATE INDEX i_mix_subscription_chan_serv ON mix_subscription (channel, service);

CREATE TABLE mix_pam (
    username text NOT NULL,
    channel text NOT NULL,
    service text NOT NULL,
    id text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX i_mix_pam ON mix_pam (username, channel, service);
CREATE INDEX i_mix_pam_us ON mix_pam (username);
```

If you want to use new MQTT feature, you need to create a table:

```sql
CREATE TABLE mqtt_pub (
    username text NOT NULL,
    server_host text NOT NULL,
    resource text NOT NULL,
    topic text NOT NULL,
    qos smallint NOT NULL,
    payload blob NOT NULL,
    payload_format smallint NOT NULL,
    content_type text NOT NULL,
    response_topic text NOT NULL,
    correlation_data blob NOT NULL,
    user_properties blob NOT NULL,
    expiry bigint NOT NULL
);

CREATE UNIQUE INDEX i_mqtt_topic_server ON mqtt_pub (topic);
```

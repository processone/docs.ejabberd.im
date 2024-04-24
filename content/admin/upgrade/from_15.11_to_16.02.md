# Upgrade to ejabberd 16.02

The MySQL schema changed to UTF-8 encoding. If you are using MySQL backend
you must upgrade the schema before starting ejabberd 16.02.

## SQL database upgrade

Example for MySQL:
``` bash
mysql -h host -u user database -p << EOF
ALTER DATABASE ejabberd CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

ALTER TABLE users CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE users MODIFY username varchar(191);

ALTER TABLE last CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE last MODIFY username varchar(191);

ALTER TABLE rosterusers CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE rosterusers MODIFY username varchar(191);
ALTER TABLE rosterusers MODIFY jid varchar(191);

ALTER TABLE rostergroups CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE rostergroups MODIFY username varchar(191);
ALTER TABLE rostergroups MODIFY jid varchar(191);

ALTER TABLE sr_group CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE sr_group MODIFY username varchar(191);

ALTER TABLE spool CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE spool MODIFY username varchar(191);
ALTER TABLE spool MODIFY xml BLOB NOT NULL;

ALTER TABLE archive CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE archive MODIFY username varchar(191);
ALTER TABLE archive MODIFY peer varchar(191);
ALTER TABLE archive MODIFY bare_peer varchar(191);
ALTER TABLE archive MODIFY nick varchar(191);

ALTER TABLE archive_prefs CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE archive_prefs MODIFY username varchar(191);

ALTER TABLE vcard CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE vcard MODIFY username varchar(191);

ALTER TABLE vcard_xupdate CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE vcard_xupdate MODIFY username varchar(191);

ALTER TABLE vcard_search CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE vcard_search MODIFY username varchar(191);
ALTER TABLE vcard_search MODIFY lusername varchar(191);
ALTER TABLE vcard_search MODIFY lfn varchar(191);
ALTER TABLE vcard_search MODIFY lfamily varchar(191);
ALTER TABLE vcard_search MODIFY lgiven varchar(191);
ALTER TABLE vcard_search MODIFY lmiddle varchar(191);
ALTER TABLE vcard_search MODIFY lnickname varchar(191);
ALTER TABLE vcard_search MODIFY lbday varchar(191);
ALTER TABLE vcard_search MODIFY lctry varchar(191);
ALTER TABLE vcard_search MODIFY llocality varchar(191);
ALTER TABLE vcard_search MODIFY lemail varchar(191);
ALTER TABLE vcard_search MODIFY lorgname varchar(191);
ALTER TABLE vcard_search MODIFY lorgunit varchar(191);

ALTER TABLE privacy_default_list CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE privacy_default_list MODIFY username varchar(191);
ALTER TABLE privacy_default_list MODIFY name varchar(191);

ALTER TABLE privacy_list CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE privacy_list MODIFY username varchar(191);
ALTER TABLE privacy_list MODIFY name varchar(191);

ALTER TABLE privacy_list_data CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

ALTER TABLE private_storage CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE private_storage MODIFY username varchar(191);
ALTER TABLE private_storage MODIFY namespace varchar(191);

ALTER TABLE roster_version CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE roster_version MODIFY username varchar(191);

ALTER TABLE pubsub_node CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE pubsub_node_option CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE pubsub_node_state CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE pubsub_node_item CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE pubsub_subscription_opt CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

ALTER TABLE muc_room CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE muc_registered CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

ALTER TABLE irc_custom CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

ALTER TABLE motd CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE motd MODIFY username varchar(191);

ALTER TABLE caps_features CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE caps_features MODIFY node varchar(191);
ALTER TABLE caps_features MODIFY subnode varchar(191);

ALTER TABLE sm CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE sm MODIFY username varchar(191);
ALTER TABLE sm MODIFY resource varchar(191);

ALTER TABLE  CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE  MODIFY username varchar(191);
EOF
```

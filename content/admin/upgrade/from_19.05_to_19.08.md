---
title: Upgrade to ejabberd 19.08
---

## Database

Riak support has been removed in this version, so don't upgrade to 19.08 if you store data in Riak.

The only SQL schema that changed since 19.05 is mysql.new.sql.

### MySQL

mysql.new.sql from 19.05 schema can work 19.08, so the migration to 19.08 schema is optional, as it's just a type change from TEXT to VARCHAR for performances reason, but it will improve index utilization.

```sql
ALTER TABLE users MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE last MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE rosterusers MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE rostergroups MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE sr_group MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE sr_user MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE spool MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE archive MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE archive_prefs MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE vcard MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE vcard_search MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE privacy_default_list MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE privacy_list MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE private_storage MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE roster_version MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE muc_room MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE muc_registered MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE muc_online_room MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE muc_online_users MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE motd MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE sm MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE route MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE push_session MODIFY server_host varchar(191) NOT NULL;
ALTER TABLE mix_pam MODIFY server_host varchar(191) NOT NULL;
```

## API changes

### Renamed arguments from `Server` to `Host`

Several ejabberd commands still used as argument name `Server`, instead of the more common `Host`. Such arguments have been renamed, and backward support allows old calls to work correctly.

The eight commands affected are:
– add_rosteritem
– bookmarks_to_pep
– delete_rosteritem
– get_offline_count
– get_presence
– get_roster
– remove_mam_for_user
– remove_mam_for_user_with_peer

If you are using this calls, please start updating your parameter names to Host when moving to ejabberd 19.08. You will thus use a more consistent API and be future proof.


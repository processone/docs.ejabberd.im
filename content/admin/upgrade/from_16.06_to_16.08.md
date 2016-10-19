---
title: Upgrade to ejabberd 16.08
---

# Ejabberd upgrade process

You need to create a new table to support the new OAuth feature
before starting ejabberd 16.08.

## SQL database upgrade

	#!console
	mysql -h host -u user database -p << EOF
	CREATE TABLE oauth_token (
	  token varchar(191) NOT NULL PRIMARY KEY,
	  jid text NOT NULL,
	  scope text NOT NULL,
	  expire bigint NOT NULL
	) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
	EOF

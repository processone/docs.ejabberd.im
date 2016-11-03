---
title: Upgrade to ejabberd 16.03
---

# Ejabberd upgrade process

If you are using sql backend for authentication, you need to upgrade
your schema before starting ejabberd 16.03. This can be safely done
while a previous version of ejabberd is actually running.

## SQL database upgrade

```bash
mysql -h host -u user database -p << EOF
ALTER TABLE users ADD COLUMN serverkey text NOT NULL DEFAULT '';
ALTER TABLE users ADD COLUMN salt text NOT NULL DEFAULT '';
ALTER TABLE users ADD COLUMN iterationcount integer NOT NULL DEFAULT 0;
EOF
```

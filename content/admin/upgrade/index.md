---
title: Upgrade Procedure for ejabberd
menu: Upgrading ejabberd
categories: ecs
toc: true
order: 15
---

This document contains administration procedure for each version upgrade.
Only upgrade from version N to N+1 is documented and supported.
If you upgrade from an older version than previous one, you have to review all
upgrade notes and apply each steps one by one for the possible database changes.
You also have to stop your old ejabberd server, and start the new one.

Until release note explicitly state you must restart the server for upgrade,
you should be able to run soft upgrade using a cluster.  If you don't have
cluster, upgrade from older release than previous one, or have explicit note
soft upgrade does not work, then you have to fallback to standalone upgrade
process.

# Generic upgrade process

This is the simplest process, and require service restart.

- read the corresponding [upgrade notes](#specific-version-upgrade-notes)
- apply the required changes in database from the upgrade note.
- stop old node
- archive content of mnesia database directory (database, i.e. `/opt/ejabberd-XX.YY/database`, `/usr/local/var/lib/ejabberd`, ...)
- install new version
- extract database archive in new path
- start new node

# Soft upgrade process

This process needs you to run in cluster, with at least two nodes. In this case,
we assume you run node A and B with version N, and will upgrade to version N+1.

- read the corresponding [upgrade notes](#specific-version-upgrade-notes),
make sure it does not explicitly states "soft upgrade is not supported".
- apply the required changes in database from the upgrade note.
- make sure node A is running
- run [leave_cluster](/developer/ejabberd-api/admin-api/#leave-cluster) on node B
- stop old node B
- install new version on B's host
- start new node B
- run [join_cluster](/developer/ejabberd-api/admin-api/#join-cluster) on node B, passing node A as parameter
- make sure both nodes are running and working as expected
- run [leave_cluster](/developer/ejabberd-api/admin-api/#leave-cluster) on node A
- stop old node A
- install new version on A's host
- start new node A
- run [join_cluster](/developer/ejabberd-api/admin-api/#join-cluster) on node A, passing node B as parameter

# Module update process

Instead of upgrading all ejabberd to a brand new version,
maybe you just want to update a few modules with bugfixes...
in that case you can update only specific modules.

This process is only recommended for bugfixes that involve functional changes,
and do not involve structural or memory changes
(those ones are usually detected and applied at server start only).

How to do this?

1. Apply the fixes to your source code, compile and reinstall ejabberd,
   so the new `*.beam` files replace the old ones
2. In the ejabberd Web Admin go to `Nodes` -> your node -> `Update`
3. This will detect what `*.beam` files have changed in the installation
4. Select which modules you want to update now, and click `Update`
5. This will load into memory the corresponding `*.beam` files

If you prefer to use commands, check
[update_list](/developer/ejabberd-api/admin-api/#update-list)
+ [update](/developer/ejabberd-api/admin-api/#update).

Notice this does not restart [modules](/admin/configuration/modules/)
or any other tasks. If the fix you plan to apply requires a module restart,
you can use this alternative:
[restart_module](/developer/ejabberd-api/admin-api/#restart-module).

# Note on database schema upgrade

`ejabberd` automatically updates the Mnesia table definitions at startup when needed.
If you also use an external [database](/admin/configuration/database/) (like MySQL, ...)
for storage of some modules, check in the corresponding
[upgrade notes](#specific-version-upgrade-notes)
of the new ejabberd version if you need to update those tables yourself manually.

# Specific version upgrade notes

- [Upgrading from ejabberd 21.07 to 21.12](/admin/upgrade/from_21.07_to_21.12/)
- [Upgrading from ejabberd 21.04 to 21.07](/admin/upgrade/from_21.04_to_21.07/)
- [Upgrading from ejabberd 21.01 to 21.04](/admin/upgrade/from_21.01_to_21.04/)
- [Upgrading from ejabberd 19.08 to 20.01](/admin/upgrade/from_19.08_to_20.01/)
- [Upgrading from ejabberd 19.05 to 19.08](/admin/upgrade/from_19.05_to_19.08/)
- [Upgrading from ejabberd 19.02 to 19.05](/admin/upgrade/from_19.02_to_19.05/)
- [Upgrading from ejabberd 18.12 to 19.02](/admin/upgrade/from_18.12_to_19.02/)
- [Upgrading from ejabberd 18.09 to 18.12](/admin/upgrade/from_18.09_to_18.12/)
- [Upgrading from ejabberd 18.06 to 18.09](/admin/upgrade/from_18.06_to_18.09/)
- [Upgrading from ejabberd 18.04 to 18.06](/admin/upgrade/from_18.04_to_18.06/)
- [Upgrading from ejabberd 18.03 to 18.04](/admin/upgrade/from_18.03_to_18.04/)
- [Upgrading from ejabberd 18.01 to 18.03](/admin/upgrade/from_18.01_to_18.03/)
- [Upgrading from ejabberd 17.11 to 18.01](/admin/upgrade/from_17.11_to_18.01/)
- [Upgrading from ejabberd 17.09 to 17.11](/admin/upgrade/from_17.09_to_17.11/)
- [Upgrading from ejabberd ≥17.06 and ≤17.08 to 17.09](/admin/upgrade/from_17.06_to_17.09/)
- [Upgrading from ejabberd 17.03 or 17.04 to 17.06](/admin/upgrade/from_17.03_to_17.06/)
- [Upgrading from ejabberd ≥16.08 and ≤17.01 to 17.03](/admin/upgrade/from_16.08_to_17.03/)
- [Upgrading from ejabberd 16.06 to 16.08](/admin/upgrade/from_16.06_to_16.08/)
- [Upgrading from ejabberd 16.04 to 16.06](/admin/upgrade/from_16.04_to_16.06/)
- [Upgrading from ejabberd 16.03 to 16.04](/admin/upgrade/from_16.03_to_16.04/)
- [Upgrading from ejabberd 16.02 to 16.03](/admin/upgrade/from_16.02_to_16.03/)
- [Upgrading from ejabberd 15.11 to 16.02](/admin/upgrade/from_15.11_to_16.02/)
- [Upgrading from ejabberd 2.1.1x to 16.02](/admin/upgrade/from_2.1.1x_to_16.02/)

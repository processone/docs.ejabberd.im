# Upgrade to ejabberd 26.02

If you upgrade ejabberd from a previous release to [26.02](../../archive/26.02/index.md),
there are no changes in SQL schemas, configuration, API commands or hooks.

Notice that [mod_muc](https://docs.ejabberd.im/admin/configuration/modules/#mod_muc) now incorporates the feature from [mod_muc_occupantid](https://docs.ejabberd.im/archive/26.01/modules/#mod_muc_occupantid), and that module has been removed. You can remove `mod_muc_occupantid` in your configuration file as it is unnecessary now, and ejabberd simply ignores it.

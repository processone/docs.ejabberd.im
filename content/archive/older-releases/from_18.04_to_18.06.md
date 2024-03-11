# Upgrade to ejabberd 18.06

There are few option changes in this version. Please check the blogpost:
https://www.process-one.net/blog/ejabberd-18-06/

Note: Starting with ejabberd 18.06, ejabberd will not ignore unknown options
and doesn't allow to have options with malformed values. The rationale for
this is to avoid unexpected behaviour during runtime, i.e. to conform to
“fail early” approach.
FOR PACKAGE BUILDERS: You must ensure your configuration is valid.

You may cleanup SQL database if you are upgrading from ejabberd 18.04 or
older, and running an SQL backend for mod_irc.

## Database

mod_irc had been obsoleted, the module is still available in ejabberd-contrib
and can be installed as external module anyway. If you're not using that module
or stop using it, you can safely remove the irc_custom SQL table.

# Upgrade to ejabberd 18.01

There is no significant change in code and database since 17.11. There is no
specific upgrade procedure.
Anyway, due to TLS improvements and ACME support, you need to check your
configuration to keep it up-to-date with latest patterns.

## ejabberd configuration

While your old ejabberd.yml is still supported, you may prefer to simplify it
thanks to latest TLS driver which enables good defaults by itself. You may also
need to use the new ACME feature, which requires minor changes in ejabberd
configuration file. See ejabberd.yml.example in ejabberd sources for reference.

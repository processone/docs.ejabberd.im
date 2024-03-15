# Upgrade to ejabberd 19.05

19.05 don't add changes on data or configuration.
You may alter your configuration if you want to use new feature
to store offline messages in archives anyway. See more details
on blogpost.

## Database

Schema for MQTT tables was added after 19.02 release despite full support
was already available. If you're using MQTT and need SQL backend, in case you
dismissed the change, please report to [ejabberd 19.02 upgrade note](from_18.12_to_19.02.md)

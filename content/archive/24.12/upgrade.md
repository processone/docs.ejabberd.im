# Upgrade to ejabberd 24.12

If you upgrade ejabberd from a previous release to [24.12](../../archive/24.12/index.md),
be aware there are some changes in commands.
There are no changes in configuration, SQL schema or hooks.

## <a name="apiv3"></a> Commands API v3

This ejabberd 24.12 release introduces ejabberd Commands API v3 because some commands have changed arguments and result formatting. You can continue using API v2; or you can update your API client to use API v3. Check the [API Versions History](https://docs.ejabberd.im/developer/ejabberd-api/api_versioning/#api-versions-history).

Some commands that accepted accounts or rooms as arguments, or returned JIDs, have changed their arguments and results names and format to be consistent with the other commands:

- Arguments that refer to a user account are now named `user` and `host`
- Arguments that refer to a MUC room are now named `room` and `service`
- As seen, each argument is now only the local or server part, not the JID
- On the other hand, results that refer to user account or MUC room are now the JID

In practice, the commands that change in API v3 are:

- [get_room_affiliations](https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#get_room_affiliations)
- [muc_register_nick](https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#muc_register_nick)
- [muc_unregister_nick](https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#muc_unregister_nick)
- [set_room_affiliation](https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#set_room_affiliation)
- [status_list](https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#status_list)
- [status_list_host](https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#status_list_host)
- [subscribe_room](https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#subscribe_room)
- [subscribe_room_many](https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#subscribe_room_many)
- [unsubscribe_room](https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#unsubscribe_room)

If you want to update ejabberd to 24.12, but prefer to continue using an old API version with `mod_http_api`, you can set this new option:
```yaml
modules:
  mod_http_api:
    default_version: 2
```

# Upgrade to ejabberd 24.06

If you upgrade ejabberd from a previous release to [24.06](../../archive/24.06/index.md),
please review those changes:

- Update commands to API v2
- Or continue using previous API version
- WebAdmin hooks changes

## <a name="apiv2"></a>Update commands to API v2

ejabberd 24.06 has changed some commands, which are tagged as
[API v2](/developer/ejabberd-api/admin-tags.md#v1).

Check [what commands have changed](#apiv2) as announced in the ejabberd 24.06 release notes,
and update them in your API client if you use any of them.

## <a name="apiv0"></a>Continue using previous API version

You can update your API client to use the new API version 2... or you can continue using API version 0 or version 1 and delay API update a few weeks or months.

To continue using API version 0:

- if using ejabberdctl, use the switch `--version 0`. For example: `ejabberdctl --version 0 get_roster admin localhost`
- if using mod_http_api, in ejabberd configuration file add `v0` to the `request_handlers` path. For example: `/api/v0: mod_http_api`

Check the full documentation in [ejabberd Docs: API Versioning](../../developer/ejabberd-api/api_versioning.md).

## <a name="webadmin-hooks"></a>WebAdmin hook changes

There are several changes in WebAdmin hooks that now provide the whole HTTP request instead of only some of its elements.

Check [what hooks have changed](#webadmin-hooks) as announced in the ejabberd 24.06 release notes,
and update them in your custom ejabberd modules if you use any of them.

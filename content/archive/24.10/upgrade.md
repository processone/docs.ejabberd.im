# Upgrade to ejabberd 24.10

If you upgrade ejabberd from a previous release to [24.10](../../archive/24.10/index.md),
be aware there are some changes in commands and hooks.
There are no changes in configuration and SQL schema.

## <a name="commands"></a>Fixes in commands

- `set_presence`: Return error when session not found

- `send_direct_invitation`: Better handling of malformed jids

- `update`: Fix command output

    So far, `ejabberd_update:update/0` returned the return value of
    `release_handler_1:eval_script/1`.  That function returns the list of
    updated but unpurged modules, i.e., modules where one or more processes
    are still running an old version of the code.  Since commit
    `5a34020d23f455f80a144bcb0d8ee94770c0dbb1`, the ejabberd `update` command
    assumes that value to be the list of updated modules instead.  As
    that seems more useful, modify `ejabberd_update:update/0` accordingly.
    This fixes the `update` command output.

- `get_mam_count`: New command to get number of archived messages for an account

## <a name="hooks"></a>Changes in hooks

- New `check_register_user` hook in `ejabberd_auth.erl` to allow blocking account registration when a tombstone exists.

- Modified `room_destroyed` hook in `mod_muc_room.erl`
    Until now the hook passed as arguments: `LServer, Room, Host`.
    Now it passes: `LServer, Room, Host, Persistent`
    That new `Persistent` argument passes the room `persistent` option,
    required by mod_tombstones because only persistent rooms should generate
    a tombstone, temporary ones should not.
    And the `persistent` option should not be completely overwritten, as we must
    still known its real value even when room is being destroyed.



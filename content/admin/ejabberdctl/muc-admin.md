# Multi User Chat Administration Commands

## Prerequisite

Most of the command to manage MUC service depends on the activation of `mod_muc_admin` module in ejabberd.

`mod_muc_admin` is included in ejabberd main code base since [ejabberd 15.04](https://www.process-one.net/blog/ejabberd-15-04/).

To enable `mod_muc_admin`-dependant `ejabberdctl` commands, you just need to add `mod_muc_admin` in `modules` section of ejabberd config file.

For example, in `ejabberd.yml` format:

``` yaml
modules:
  mod_muc_admin: {}
```

## Commands

The most updated and detailed list of commands for managing
a MUC service and MUC rooms is available
in the [muc API tag](../../developer/ejabberd-api/admin-tags.md#muc)
and the [muc_room API tag](../../developer/ejabberd-api/admin-tags.md#muc_room).

### Online Rooms

List existing rooms ('global' to get all vhosts).

``` sh
ejabberdctl muc_online_rooms [global]
```

### Unregister nickname

Unregister the nick in the MUC service.

``` sh
ejabberdctl muc_unregister_nick nickname
```

### Create MUC room

Create a MUC room `name@service` in host.

``` sh
ejabberdctl create_room room_name muc_service xmpp_domain
```

### Destroy MUC room

Destroy a MUC chat room.

``` sh
ejabberdctl destroy_room room_name muc_service
```

### Create multiple rooms from file

Create the rooms listed in a local file.

``` sh
ejabberdctl create_rooms_file filename
```

TODO: Describe the file format.

### Destroy multiple rooms from file

Destroy the rooms indicated in a local file.

``` sh
ejabberdctl destroy_rooms_file filename
```

### List unused MUC rooms

List rooms that have not been used in several days on an XMPP domain.

``` sh
ejabberdctl rooms_unused_list xmpp_domain number_of_days
```

### Destroy unused MUC rooms

Destroy rooms that have not been used in several days on an XMPP domain.

``` sh
ejabberdctl rooms_unused_destroy xmpp_domain number_of_days
```

### List rooms joined by a given user

Get the list of rooms where a user is occupant.

``` sh
ejabberdctl get_user_rooms user_real_jid xmpp_domain
```

### List the occupants of a MUC room

Get the list of occupants of a MUC room.

``` sh
ejabberdctl get_room_occupants room_name muc_service
```

### Retrieve number of occupants in a MUC room

Get the number of occupants of a MUC room.

``` sh
ejabberdctl get_room_occupants_number room_name muc_service
```

### Invite several users to a MUC room

Send a direct invitation to several JIDs. Password and Message can
also be: none. Users JIDs are separated with ':'.

``` sh
ejabberdctl send_direct_invitation room_name muc_service password reason jid1[:jid2]
```

### Change option for a MUC room

Change an option in a MUC room.

``` sh
ejabberdctl change_room_option room_name muc_service option value
```

<!--
TODO: add example to show how options list is represented.
-->

### Get options from a MUC room

Get options from a MUC room.

``` sh
ejabberdctl get_room_options room_name muc_service
```

### Change affiliation for a user in a MUC room

Change an affiliation in a MUC room. Affiliation can be one of: owner,
admin, member, outcast, none.

``` sh
ejabberdctl set_room_affiliation room_name muc_service user_jid affiliation
```

### Get affiliations for a MUC room

Get the list of affiliations of a MUC room.

``` sh
ejabberdctl get_room_affiliations room_name muc_service
```

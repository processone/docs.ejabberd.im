---
search:
  boost: 1
---

# API Reference

!!! info "Please note"

    This section describes API commands of ejabberd [25.03](../../archive/25.03/index.md).  If you are using an old ejabberd release, please refer to the corresponding archived version of this page in the [Archive](../../archive/index.md).

    The commands that changed in this version are marked with 🟤



## abort_delete_old_mam_messages

<!-- md:version added in [22.05](../../archive/22.05/index.md) -->

Abort currently running delete old MAM messages operation

__Arguments:__

- *host* :: string : Name of host where operation should be aborted

__Result:__

- *status* :: string : Status text

__Tags:__
[mam](admin-tags.md#mam), [purge](admin-tags.md#purge)

__Module:__
[mod_mam](../../admin/configuration/modules.md#mod_mam)

__Examples:__


~~~ json
POST /api/abort_delete_old_mam_messages
{
  "host": "localhost"
}

HTTP/1.1 200 OK
"Operation aborted"
~~~




## abort_delete_old_messages

<!-- md:version added in [22.05](../../archive/22.05/index.md) -->

Abort currently running delete old offline messages operation

__Arguments:__

- *host* :: string : Name of host where operation should be aborted

__Result:__

- *status* :: string : Status text

__Tags:__
[offline](admin-tags.md#offline), [purge](admin-tags.md#purge)

__Examples:__


~~~ json
POST /api/abort_delete_old_messages
{
  "host": "localhost"
}

HTTP/1.1 200 OK
"Operation aborted"
~~~




## add_rosteritem

<!-- md:version updated in [24.02](../../archive/24.02/index.md) -->

Add an item to a user's roster (supports ODBC)

__Arguments:__

- *localuser* :: string : User name
- *localhost* :: string : Server name
- *user* :: string : Contact user name
- *host* :: string : Contact server name
- *nick* :: string : Nickname
- *groups* :: [group::string] : Groups
- *subs* :: string : Subscription

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[roster](admin-tags.md#roster), [v1](admin-tags.md#v1)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/add_rosteritem
{
  "localuser": "user1",
  "localhost": "myserver.com",
  "user": "user2",
  "host": "myserver.com",
  "nick": "User 2",
  "groups": [
    "Friends",
    "Team 1"
  ],
  "subs": "both"
}

HTTP/1.1 200 OK
""
~~~




## backup


Backup the Mnesia database to a binary file

__Arguments:__

- *file* :: string : Full path for the destination backup file

__Result:__

- *res* :: string : Raw result string

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/backup
{
  "file": "/var/lib/ejabberd/database.backup"
}

HTTP/1.1 200 OK
"Success"
~~~




## ban_account

<!-- md:version improved in [24.06](../../archive/24.06/index.md) -->

Ban an account


This command kicks the account sessions, sets a random password, and stores ban details in the account private storage. This command requires [mod_private](../../admin/configuration/modules.md#mod_private) to be enabled. Check also [get_ban_details](#get_ban_details) API and [unban_account](#unban_account) API.

__Arguments:__

- *user* :: string : User name to ban
- *host* :: string : Server name
- *reason* :: string : Reason for banning user

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[accounts](admin-tags.md#accounts), [v2](admin-tags.md#v2)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/ban_account
{
  "user": "attacker",
  "host": "myserver.com",
  "reason": "Spaming other users"
}

HTTP/1.1 200 OK
""
~~~




## bookmarks_to_pep


Export private XML storage bookmarks to PEP

__Arguments:__

- *user* :: string : Username
- *host* :: string : Server

__Result:__

- *res* :: string : Raw result string

__Tags:__
[private](admin-tags.md#private)

__Module:__
[mod_private](../../admin/configuration/modules.md#mod_private)

__Examples:__


~~~ json
POST /api/bookmarks_to_pep
{
  "user": "bob",
  "host": "example.com"
}

HTTP/1.1 200 OK
"Bookmarks exported"
~~~




## change_password


Change the password of an account

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name
- *newpass* :: string : New password for user

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[accounts](admin-tags.md#accounts)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/change_password
{
  "user": "peter",
  "host": "myserver.com",
  "newpass": "blank"
}

HTTP/1.1 200 OK
""
~~~




## change_room_option


Change an option in a MUC room

__Arguments:__

- *room* :: string : Room name
- *service* :: string : MUC service
- *option* :: string : Option name
- *value* :: string : Value to assign

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[muc_room](admin-tags.md#muc_room)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/change_room_option
{
  "room": "room1",
  "service": "conference.example.com",
  "option": "members_only",
  "value": "true"
}

HTTP/1.1 200 OK
""
~~~




## check_account


Check if an account exists or not

__Arguments:__

- *user* :: string : User name to check
- *host* :: string : Server to check

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[accounts](admin-tags.md#accounts)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/check_account
{
  "user": "peter",
  "host": "myserver.com"
}

HTTP/1.1 200 OK
""
~~~




## check_password


Check if a password is correct

__Arguments:__

- *user* :: string : User name to check
- *host* :: string : Server to check
- *password* :: string : Password to check

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[accounts](admin-tags.md#accounts)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/check_password
{
  "user": "peter",
  "host": "myserver.com",
  "password": "secret"
}

HTTP/1.1 200 OK
""
~~~




## check_password_hash


Check if the password hash is correct


Allows hash methods from the Erlang/OTP [crypto](https://www.erlang.org/doc/apps/crypto/crypto.html) application.

__Arguments:__

- *user* :: string : User name to check
- *host* :: string : Server to check
- *passwordhash* :: string : Password's hash value
- *hashmethod* :: string : Name of hash method

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[accounts](admin-tags.md#accounts)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/check_password_hash
{
  "user": "peter",
  "host": "myserver.com",
  "passwordhash": "5ebe2294ecd0e0f08eab7690d2a6ee69",
  "hashmethod": "md5"
}

HTTP/1.1 200 OK
""
~~~




## clear_cache


Clear database cache on all nodes

__Arguments:__


__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[server](admin-tags.md#server)

__Examples:__


~~~ json
POST /api/clear_cache
{
  
}

HTTP/1.1 200 OK
""
~~~




## compile


Recompile and reload Erlang source code file

__Arguments:__

- *file* :: string : Filename of erlang source file to compile

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[erlang](admin-tags.md#erlang)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/compile
{
  "file": "/home/me/srcs/ejabberd/mod_example.erl"
}

HTTP/1.1 200 OK
""
~~~




## connected_users


List all established sessions

__Arguments:__


__Result:__

- *connected_users* :: [sessions::string] : List of users sessions full JID

__Tags:__
[session](admin-tags.md#session)

__Examples:__


~~~ json
POST /api/connected_users
{
  
}

HTTP/1.1 200 OK
[
  "user1@example.com/Home",
  "user2@example.com/54134"
]
~~~




## connected_users_info


List all established sessions and their information

__Arguments:__


__Result:__

- *connected_users_info* :: [{jid::string, connection::string, ip::string, port::integer, priority::integer, node::string, uptime::integer, status::string, resource::string, statustext::string}]

__Tags:__
[session](admin-tags.md#session)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/connected_users_info
{
  
}

HTTP/1.1 200 OK
[
  {
    "jid": "user1@myserver.com/tka",
    "connection": "c2s",
    "ip": "127.0.0.1",
    "port": 42656,
    "priority": 8,
    "node": "ejabberd@localhost",
    "uptime": 231,
    "status": "dnd",
    "resource": "tka",
    "statustext": ""
  }
]
~~~




## connected_users_number


Get the number of established sessions

__Arguments:__


__Result:__

- *num_sessions* :: integer

__Tags:__
[session](admin-tags.md#session), [statistics](admin-tags.md#statistics)

__Examples:__


~~~ json
POST /api/connected_users_number
{
  
}

HTTP/1.1 200 OK
2
~~~




## connected_users_vhost


Get the list of established sessions in a vhost

__Arguments:__

- *host* :: string : Server name

__Result:__

- *connected_users_vhost* :: [sessions::string] : List of sessions full JIDs

__Tags:__
[session](admin-tags.md#session)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/connected_users_vhost
{
  "host": "myexample.com"
}

HTTP/1.1 200 OK
[
  "user1@myserver.com/tka",
  "user2@localhost/tka"
]
~~~




## convert_to_scram


Convert the passwords of users to SCRAM

__Arguments:__

- *host* :: string : Vhost which users' passwords will be scrammed

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[sql](admin-tags.md#sql)

__Examples:__


~~~ json
POST /api/convert_to_scram
{
  "host": "example.com"
}

HTTP/1.1 200 OK
""
~~~




## convert_to_yaml


Convert the input file from Erlang to YAML format

__Arguments:__

- *in* :: string : Full path to the original configuration file
- *out* :: string : And full path to final file

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[config](admin-tags.md#config)

__Examples:__


~~~ json
POST /api/convert_to_yaml
{
  "in": "/etc/ejabberd/ejabberd.cfg",
  "out": "/etc/ejabberd/ejabberd.yml"
}

HTTP/1.1 200 OK
""
~~~




## create_room


Create a MUC room name@service in host

__Arguments:__

- *room* :: string : Room name
- *service* :: string : MUC service
- *host* :: string : Server host

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[muc_room](admin-tags.md#muc_room)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/create_room
{
  "room": "room1",
  "service": "conference.example.com",
  "host": "example.com"
}

HTTP/1.1 200 OK
""
~~~




## create_room_with_opts 🟤

<!-- md:version modified in [25.03](../../archive/25.03/index.md) -->

Create a MUC room name@service in host with given options


Options `affiliations` and `subscribers` are lists of tuples. The tuples in the list are separated with `;` and the elements in each tuple are separated with `=` (until ejabberd [24.12](../../archive/24.12/index.md) the separators were `,` and `:` respectively). Each subscriber can have one or more nodes. In summary, `affiliations` is like `Type1=JID1;Type2=JID2` and `subscribers` is like `JID1=Nick1=Node1A=Node1B=Node1C;JID2=Nick2=Node2`.

__Arguments:__

- *room* :: string : Room name
- *service* :: string : MUC service
- *host* :: string : Server host
- *options* :: [{name::string, value::string}] : List of options

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[muc_room](admin-tags.md#muc_room), [muc_sub](admin-tags.md#muc_sub)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/create_room_with_opts
{
  "room": "room1",
  "service": "conference.example.com",
  "host": "localhost",
  "options": [
    {
      "name": "members_only",
      "value": "true"
    },
    {
      "name": "affiliations",
      "value": "owner=user1@localhost;member=user2@localhost"
    },
    {
      "name": "subscribers",
      "value": "user3@localhost=User3=messages=subject;user4@localhost=User4=messages"
    }
  ]
}

HTTP/1.1 200 OK
""
~~~




## create_rooms_file

<!-- md:version improved in [24.12](../../archive/24.12/index.md) -->

Create the rooms indicated in file


Provide one room JID per line. Rooms will be created after restart.

__Arguments:__

- *file* :: string : Path to the text file with one room JID per line

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[muc](admin-tags.md#muc)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/create_rooms_file
{
  "file": "/home/ejabberd/rooms.txt"
}

HTTP/1.1 200 OK
""
~~~




## delete_expired_messages


Delete expired offline messages from database

__Arguments:__


__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[offline](admin-tags.md#offline), [purge](admin-tags.md#purge)

__Examples:__


~~~ json
POST /api/delete_expired_messages
{
  
}

HTTP/1.1 200 OK
""
~~~




## delete_expired_pubsub_items

<!-- md:version added in [21.12](../../archive/21.12/index.md) -->

Delete expired PubSub items

__Arguments:__


__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[purge](admin-tags.md#purge)

__Module:__
[mod_pubsub](../../admin/configuration/modules.md#mod_pubsub)

__Examples:__


~~~ json
POST /api/delete_expired_pubsub_items
{
  
}

HTTP/1.1 200 OK
""
~~~




## delete_mnesia


Delete elements in Mnesia database for a given vhost

__Arguments:__

- *host* :: string : Vhost which content will be deleted in Mnesia database

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/delete_mnesia
{
  "host": "example.com"
}

HTTP/1.1 200 OK
""
~~~




## delete_old_mam_messages


Delete MAM messages older than DAYS


Valid message TYPEs: `chat`, `groupchat`, `all`.

__Arguments:__

- *type* :: string : Type of messages to delete (`chat`, `groupchat`, `all`)
- *days* :: integer : Days to keep messages

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[mam](admin-tags.md#mam), [purge](admin-tags.md#purge)

__Module:__
[mod_mam](../../admin/configuration/modules.md#mod_mam)

__Examples:__


~~~ json
POST /api/delete_old_mam_messages
{
  "type": "all",
  "days": 31
}

HTTP/1.1 200 OK
""
~~~




## delete_old_mam_messages_batch

<!-- md:version added in [22.05](../../archive/22.05/index.md) -->

Delete MAM messages older than DAYS


Valid message TYPEs: `chat`, `groupchat`, `all`.

__Arguments:__

- *host* :: string : Name of host where messages should be deleted
- *type* :: string : Type of messages to delete (`chat`, `groupchat`, `all`)
- *days* :: integer : Days to keep messages
- *batch_size* :: integer : Number of messages to delete per batch
- *rate* :: integer : Desired rate of messages to delete per minute

__Result:__

- *res* :: string : Raw result string

__Tags:__
[mam](admin-tags.md#mam), [purge](admin-tags.md#purge)

__Module:__
[mod_mam](../../admin/configuration/modules.md#mod_mam)

__Examples:__


~~~ json
POST /api/delete_old_mam_messages_batch
{
  "host": "localhost",
  "type": "all",
  "days": 31,
  "batch_size": 1000,
  "rate": 10000
}

HTTP/1.1 200 OK
"Removal of 5000 messages in progress"
~~~




## delete_old_mam_messages_status

<!-- md:version added in [22.05](../../archive/22.05/index.md) -->

Status of delete old MAM messages operation

__Arguments:__

- *host* :: string : Name of host where messages should be deleted

__Result:__

- *status* :: string : Status test

__Tags:__
[mam](admin-tags.md#mam), [purge](admin-tags.md#purge)

__Module:__
[mod_mam](../../admin/configuration/modules.md#mod_mam)

__Examples:__


~~~ json
POST /api/delete_old_mam_messages_status
{
  "host": "localhost"
}

HTTP/1.1 200 OK
"Operation in progress, delete 5000 messages"
~~~




## delete_old_messages


Delete offline messages older than DAYS

__Arguments:__

- *days* :: integer : Number of days

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[offline](admin-tags.md#offline), [purge](admin-tags.md#purge)

__Examples:__


~~~ json
POST /api/delete_old_messages
{
  "days": 31
}

HTTP/1.1 200 OK
""
~~~




## delete_old_messages_batch

<!-- md:version added in [22.05](../../archive/22.05/index.md) -->

Delete offline messages older than DAYS

__Arguments:__

- *host* :: string : Name of host where messages should be deleted
- *days* :: integer : Days to keep messages
- *batch_size* :: integer : Number of messages to delete per batch
- *rate* :: integer : Desired rate of messages to delete per minute

__Result:__

- *res* :: string : Raw result string

__Tags:__
[offline](admin-tags.md#offline), [purge](admin-tags.md#purge)

__Examples:__


~~~ json
POST /api/delete_old_messages_batch
{
  "host": "localhost",
  "days": 31,
  "batch_size": 1000,
  "rate": 10000
}

HTTP/1.1 200 OK
"Removal of 5000 messages in progress"
~~~




## delete_old_messages_status

<!-- md:version added in [22.05](../../archive/22.05/index.md) -->

Status of delete old offline messages operation

__Arguments:__

- *host* :: string : Name of host where messages should be deleted

__Result:__

- *status* :: string : Status test

__Tags:__
[offline](admin-tags.md#offline), [purge](admin-tags.md#purge)

__Examples:__


~~~ json
POST /api/delete_old_messages_status
{
  "host": "localhost"
}

HTTP/1.1 200 OK
"Operation in progress, delete 5000 messages"
~~~




## delete_old_pubsub_items

<!-- md:version added in [21.12](../../archive/21.12/index.md) -->

Keep only NUMBER of PubSub items per node

__Arguments:__

- *number* :: integer : Number of items to keep per node

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[purge](admin-tags.md#purge)

__Module:__
[mod_pubsub](../../admin/configuration/modules.md#mod_pubsub)

__Examples:__


~~~ json
POST /api/delete_old_pubsub_items
{
  "number": 1000
}

HTTP/1.1 200 OK
""
~~~




## delete_old_push_sessions


Remove push sessions older than DAYS

__Arguments:__

- *days* :: integer

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[purge](admin-tags.md#purge)

__Module:__
[mod_push](../../admin/configuration/modules.md#mod_push)

__Examples:__


~~~ json
POST /api/delete_old_push_sessions
{
  "days": 1
}

HTTP/1.1 200 OK
""
~~~




## delete_old_users


Delete users that didn't log in last days, or that never logged


To protect admin accounts, configure this for example:
``` yaml
access_rules:
  protect_old_users:
    - allow: admin
    - deny: all
```


__Arguments:__

- *days* :: integer : Last login age in days of accounts that should be removed

__Result:__

- *res* :: string : Raw result string

__Tags:__
[accounts](admin-tags.md#accounts), [purge](admin-tags.md#purge)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/delete_old_users
{
  "days": 30
}

HTTP/1.1 200 OK
"Deleted 2 users: ["oldman@myserver.com", "test@myserver.com"]"
~~~




## delete_old_users_vhost


Delete users that didn't log in last days in vhost, or that never logged


To protect admin accounts, configure this for example:
``` yaml
access_rules:
  delete_old_users:
    - deny: admin
    - allow: all
```


__Arguments:__

- *host* :: string : Server name
- *days* :: integer : Last login age in days of accounts that should be removed

__Result:__

- *res* :: string : Raw result string

__Tags:__
[accounts](admin-tags.md#accounts), [purge](admin-tags.md#purge)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/delete_old_users_vhost
{
  "host": "myserver.com",
  "days": 30
}

HTTP/1.1 200 OK
"Deleted 2 users: ["oldman@myserver.com", "test@myserver.com"]"
~~~




## delete_rosteritem


Delete an item from a user's roster (supports ODBC)

__Arguments:__

- *localuser* :: string : User name
- *localhost* :: string : Server name
- *user* :: string : Contact user name
- *host* :: string : Contact server name

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[roster](admin-tags.md#roster)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/delete_rosteritem
{
  "localuser": "user1",
  "localhost": "myserver.com",
  "user": "user2",
  "host": "myserver.com"
}

HTTP/1.1 200 OK
""
~~~




## destroy_room


Destroy a MUC room

__Arguments:__

- *room* :: string : Room name
- *service* :: string : MUC service

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[muc_room](admin-tags.md#muc_room)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/destroy_room
{
  "room": "room1",
  "service": "conference.example.com"
}

HTTP/1.1 200 OK
""
~~~




## destroy_rooms_file


Destroy the rooms indicated in file


Provide one room JID per line.

__Arguments:__

- *file* :: string : Path to the text file with one room JID per line

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[muc](admin-tags.md#muc)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/destroy_rooms_file
{
  "file": "/home/ejabberd/rooms.txt"
}

HTTP/1.1 200 OK
""
~~~




## dump


Dump the Mnesia database to a text file

__Arguments:__

- *file* :: string : Full path for the text file

__Result:__

- *res* :: string : Raw result string

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/dump
{
  "file": "/var/lib/ejabberd/database.txt"
}

HTTP/1.1 200 OK
"Success"
~~~




## dump_config


Dump configuration in YAML format as seen by ejabberd

__Arguments:__

- *out* :: string : Full path to output file

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[config](admin-tags.md#config)

__Examples:__


~~~ json
POST /api/dump_config
{
  "out": "/tmp/ejabberd.yml"
}

HTTP/1.1 200 OK
""
~~~




## dump_table


Dump a Mnesia table to a text file

__Arguments:__

- *file* :: string : Full path for the text file
- *table* :: string : Table name

__Result:__

- *res* :: string : Raw result string

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/dump_table
{
  "file": "/var/lib/ejabberd/table-muc-registered.txt",
  "table": "muc_registered"
}

HTTP/1.1 200 OK
"Success"
~~~




## evacuate_kindly

<!-- md:version added in [24.12](../../archive/24.12/index.md) -->

Evacuate kindly all users (kick and prevent login)


Inform users and rooms, don't allow login, wait, restart the server, and don't allow new logins.
Provide the delay in seconds, and the announcement quoted, for example: 
`ejabberdctl evacuate_kindly 60 \"The server will stop in one minute.\"`

__Arguments:__

- *delay* :: integer : Seconds to wait
- *announcement* :: string : Announcement to send, with quotes

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[server](admin-tags.md#server)

__Examples:__


~~~ json
POST /api/evacuate_kindly
{
  "delay": 60,
  "announcement": "Server will stop now."
}

HTTP/1.1 200 OK
""
~~~




## export2sql


Export virtual host information from Mnesia tables to SQL file


Configure the modules to use SQL, then call this command. After correctly exported the database of a vhost, you may want to delete from mnesia with the [delete_mnesia](#delete_mnesia) API.

__Arguments:__

- *host* :: string : Vhost
- *file* :: string : Full path to the destination SQL file

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/export2sql
{
  "host": "example.com",
  "file": "/var/lib/ejabberd/example.com.sql"
}

HTTP/1.1 200 OK
""
~~~




## export_piefxis


Export data of all users in the server to PIEFXIS files (XEP-0227)

__Arguments:__

- *dir* :: string : Full path to a directory

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/export_piefxis
{
  "dir": "/var/lib/ejabberd/"
}

HTTP/1.1 200 OK
""
~~~




## export_piefxis_host


Export data of users in a host to PIEFXIS files (XEP-0227)

__Arguments:__

- *dir* :: string : Full path to a directory
- *host* :: string : Vhost to export

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/export_piefxis_host
{
  "dir": "/var/lib/ejabberd/",
  "host": "example.com"
}

HTTP/1.1 200 OK
""
~~~




## gc

<!-- md:version added in [20.01](../../archive/20.01/index.md) -->

Force full garbage collection

__Arguments:__


__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[server](admin-tags.md#server)

__Examples:__


~~~ json
POST /api/gc
{
  
}

HTTP/1.1 200 OK
""
~~~




## gen_html_doc_for_commands


Generates html documentation for ejabberd_commands

__Arguments:__

- *file* :: string : Path to file where generated documentation should be stored
- *regexp* :: string : Regexp matching names of commands or modules that will be included inside generated document
- *examples* :: string : Comma separated list of languages (chosen from `java`, `perl`, `xmlrpc`, `json`) that will have example invocation include in markdown document

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[documentation](admin-tags.md#documentation)

__Examples:__


~~~ json
POST /api/gen_html_doc_for_commands
{
  "file": "/home/me/docs/api.html",
  "regexp": "mod_admin",
  "examples": "java,json"
}

HTTP/1.1 200 OK
""
~~~




## gen_markdown_doc_for_commands


Generates markdown documentation for ejabberd_commands

__Arguments:__

- *file* :: string : Path to file where generated documentation should be stored
- *regexp* :: string : Regexp matching names of commands or modules that will be included inside generated document, or `runtime` to get commands registered at runtime
- *examples* :: string : Comma separated list of languages (chosen from `java`, `perl`, `xmlrpc`, `json`) that will have example invocation include in markdown document

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[documentation](admin-tags.md#documentation)

__Examples:__


~~~ json
POST /api/gen_markdown_doc_for_commands
{
  "file": "/home/me/docs/api.html",
  "regexp": "mod_admin",
  "examples": "java,json"
}

HTTP/1.1 200 OK
""
~~~




## gen_markdown_doc_for_tags

<!-- md:version added in [21.12](../../archive/21.12/index.md) -->

Generates markdown documentation for ejabberd_commands

__Arguments:__

- *file* :: string : Path to file where generated documentation should be stored

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[documentation](admin-tags.md#documentation)

__Examples:__


~~~ json
POST /api/gen_markdown_doc_for_tags
{
  "file": "/home/me/docs/tags.md"
}

HTTP/1.1 200 OK
""
~~~




## get_ban_details

<!-- md:version added in [24.06](../../archive/24.06/index.md) -->

Get ban details about an account


Check [ban_account](#ban_account) API.

__Arguments:__

- *user* :: string : User name to unban
- *host* :: string : Server name

__Result:__

- *ban_details* :: [{name::string, value::string}]

__Tags:__
[accounts](admin-tags.md#accounts), [v2](admin-tags.md#v2)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/get_ban_details
{
  "user": "attacker",
  "host": "myserver.com"
}

HTTP/1.1 200 OK
[
  {
    "name": "reason",
    "value": "Spamming other users"
  },
  {
    "name": "bandate",
    "value": "2024-04-22T09:16:47.975312Z"
  },
  {
    "name": "lastdate",
    "value": "2024-04-22T08:39:12Z"
  },
  {
    "name": "lastreason",
    "value": "Connection reset by peer"
  }
]
~~~




## get_cookie


Get the Erlang cookie of this node

__Arguments:__


__Result:__

- *cookie* :: string : Erlang cookie used for authentication by ejabberd

__Tags:__
[erlang](admin-tags.md#erlang)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/get_cookie
{
  
}

HTTP/1.1 200 OK
"MWTAVMODFELNLSMYXPPD"
~~~




## get_last


Get last activity information


Timestamp is UTC and [XEP-0082](https://xmpp.org/extensions/xep-0082.html) format, for example: `2017-02-23T22:25:28.063062Z     ONLINE`

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name

__Result:__

- *last_activity* :: {timestamp::string, status::string} : Last activity timestamp and status

__Tags:__
[last](admin-tags.md#last)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/get_last
{
  "user": "user1",
  "host": "myserver.com"
}

HTTP/1.1 200 OK
{
  "timestamp": "2017-06-30T14:32:16.060684Z",
  "status": "ONLINE"
}
~~~




## get_loglevel


Get the current loglevel

__Arguments:__


__Result:__

- *levelatom* :: string : Tuple with the log level number, its keyword and description

__Tags:__
[logs](admin-tags.md#logs)

__Examples:__


~~~ json
POST /api/get_loglevel
{
  
}

HTTP/1.1 200 OK
"warning"
~~~




## get_mam_count

<!-- md:version added in [24.10](../../archive/24.10/index.md) -->

Get number of MAM messages in a local user archive

__Arguments:__

- *user* :: string
- *host* :: string

__Result:__

- *value* :: integer : Number

__Tags:__
[mam](admin-tags.md#mam)

__Module:__
[mod_mam](../../admin/configuration/modules.md#mod_mam)

__Examples:__


~~~ json
POST /api/get_mam_count
{
  "user": "aaaaa",
  "host": "bbbbb"
}

HTTP/1.1 200 OK
5
~~~




## get_master

<!-- md:version added in [24.06](../../archive/24.06/index.md) -->

Get master node of the clustered Mnesia tables


If there is no master, returns `none`.

__Arguments:__


__Result:__

- *nodename* :: string

__Tags:__
[cluster](admin-tags.md#cluster)

__Examples:__


~~~ json
POST /api/get_master
{
  
}

HTTP/1.1 200 OK
"aaaaa"
~~~




## get_offline_count


Get the number of unread offline messages

__Arguments:__

- *user* :: string
- *host* :: string

__Result:__

- *value* :: integer : Number

__Tags:__
[offline](admin-tags.md#offline)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/get_offline_count
{
  "user": "aaaaa",
  "host": "bbbbb"
}

HTTP/1.1 200 OK
5
~~~




## get_presence


Retrieve the resource with highest priority, and its presence (show and status message) for a given user.


The `jid` value contains the user JID with resource.

The `show` value contains the user presence flag. It can take limited values:

 - `available`
 - `chat` (Free for chat)
 - `away`
 - `dnd` (Do not disturb)
 - `xa` (Not available, extended away)
 - `unavailable` (Not connected)

`status` is a free text defined by the user client.

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name

__Result:__

- *presence* :: {jid::string, show::string, status::string}

__Tags:__
[session](admin-tags.md#session)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/get_presence
{
  "user": "peter",
  "host": "myexample.com"
}

HTTP/1.1 200 OK
{
  "jid": "user1@myserver.com/tka",
  "show": "dnd",
  "status": "Busy"
}
~~~




## get_room_affiliation


Get affiliation of a user in MUC room

__Arguments:__

- *room* :: string : Room name
- *service* :: string : MUC service
- *jid* :: string : User JID

__Result:__

- *affiliation* :: string : Affiliation of the user

__Tags:__
[muc_room](admin-tags.md#muc_room)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/get_room_affiliation
{
  "room": "room1",
  "service": "conference.example.com",
  "jid": "user1@example.com"
}

HTTP/1.1 200 OK
"member"
~~~




## get_room_affiliations

<!-- md:version updated in [24.12](../../archive/24.12/index.md) -->

Get the list of affiliations of a MUC room

__Arguments:__

- *room* :: string : Room name
- *service* :: string : MUC service

__Result:__

- *affiliations* :: [{jid::string, affiliation::string, reason::string}] : The list of affiliations with jid, affiliation and reason

__Tags:__
[muc_room](admin-tags.md#muc_room), [v3](admin-tags.md#v3)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/get_room_affiliations
{
  "room": "room1",
  "service": "conference.example.com"
}

HTTP/1.1 200 OK
[
  {
    "jid": "user1@example.com",
    "affiliation": "member",
    "reason": "member"
  }
]
~~~




## get_room_history

<!-- md:version added in [23.04](../../archive/23.04/index.md) -->

Get history of messages stored inside MUC room state

__Arguments:__

- *room* :: string : Room name
- *service* :: string : MUC service

__Result:__

- *history* :: [{timestamp::string, message::string}]

__Tags:__
[muc_room](admin-tags.md#muc_room)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/get_room_history
{
  "room": "room1",
  "service": "conference.example.com"
}

HTTP/1.1 200 OK
[
  {
    "timestamp": "aaaaa",
    "message": "bbbbb"
  },
  {
    "timestamp": "ccccc",
    "message": "ddddd"
  }
]
~~~




## get_room_occupants


Get the list of occupants of a MUC room

__Arguments:__

- *room* :: string : Room name
- *service* :: string : MUC service

__Result:__

- *occupants* :: [{jid::string, nick::string, role::string}] : The list of occupants with JID, nick and affiliation

__Tags:__
[muc_room](admin-tags.md#muc_room)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/get_room_occupants
{
  "room": "room1",
  "service": "conference.example.com"
}

HTTP/1.1 200 OK
[
  {
    "jid": "user1@example.com/psi",
    "nick": "User 1",
    "role": "owner"
  }
]
~~~




## get_room_occupants_number


Get the number of occupants of a MUC room

__Arguments:__

- *room* :: string : Room name
- *service* :: string : MUC service

__Result:__

- *occupants* :: integer : Number of room occupants

__Tags:__
[muc_room](admin-tags.md#muc_room)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/get_room_occupants_number
{
  "room": "room1",
  "service": "conference.example.com"
}

HTTP/1.1 200 OK
7
~~~




## get_room_options


Get options from a MUC room

__Arguments:__

- *room* :: string : Room name
- *service* :: string : MUC service

__Result:__

- *options* :: [{name::string, value::string}] : List of room options tuples with name and value

__Tags:__
[muc_room](admin-tags.md#muc_room)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/get_room_options
{
  "room": "room1",
  "service": "conference.example.com"
}

HTTP/1.1 200 OK
[
  {
    "name": "members_only",
    "value": "true"
  }
]
~~~




## get_roster

<!-- md:version improved in [23.10](../../archive/23.10/index.md) -->

Get list of contacts in a local user roster


`subscription` can be: `none`, `from`, `to`, `both`.

`pending` can be: `in`, `out`, `none`.

__Arguments:__

- *user* :: string
- *host* :: string

__Result:__

- *contacts* :: [{jid::string, nick::string, subscription::string, pending::string, groups::[group::string]}]

__Tags:__
[roster](admin-tags.md#roster)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/get_roster
{
  "user": "aaaaa",
  "host": "bbbbb"
}

HTTP/1.1 200 OK
[
  {
    "jid": "user2@localhost",
    "nick": "User 2",
    "subscription": "none",
    "pending": "subscribe",
    "groups": [
      "Group1"
    ]
  }
]
~~~




## get_roster_count

<!-- md:version added in [24.06](../../archive/24.06/index.md) -->

Get number of contacts in a local user roster

__Arguments:__

- *user* :: string
- *host* :: string

__Result:__

- *value* :: integer : Number

__Tags:__
[roster](admin-tags.md#roster)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/get_roster_count
{
  "user": "aaaaa",
  "host": "bbbbb"
}

HTTP/1.1 200 OK
5
~~~




## get_subscribers


List subscribers of a MUC conference

__Arguments:__

- *room* :: string : Room name
- *service* :: string : MUC service

__Result:__

- *subscribers* :: [jid::string] : The list of users that are subscribed to that room

__Tags:__
[muc_room](admin-tags.md#muc_room), [muc_sub](admin-tags.md#muc_sub)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/get_subscribers
{
  "room": "room1",
  "service": "conference.example.com"
}

HTTP/1.1 200 OK
[
  "user2@example.com",
  "user3@example.com"
]
~~~




## get_user_rooms


Get the list of rooms where this user is occupant

__Arguments:__

- *user* :: string : Username
- *host* :: string : Server host

__Result:__

- *rooms* :: [room::string]

__Tags:__
[muc](admin-tags.md#muc)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/get_user_rooms
{
  "user": "tom",
  "host": "example.com"
}

HTTP/1.1 200 OK
[
  "room1@conference.example.com",
  "room2@conference.example.com"
]
~~~




## get_user_subscriptions

<!-- md:version added in [21.04](../../archive/21.04/index.md) -->

Get the list of rooms where this user is subscribed

__Arguments:__

- *user* :: string : Username
- *host* :: string : Server host

__Result:__

- *rooms* :: [{roomjid::string, usernick::string, nodes::[node::string]}]

__Tags:__
[muc](admin-tags.md#muc), [muc_sub](admin-tags.md#muc_sub)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/get_user_subscriptions
{
  "user": "tom",
  "host": "example.com"
}

HTTP/1.1 200 OK
[
  {
    "roomjid": "room1@conference.example.com",
    "usernick": "Tommy",
    "nodes": [
      "mucsub:config"
    ]
  }
]
~~~




## get_vcard


Get content from a vCard field


Some vcard field names in `get`/`set_vcard` are:

* FN           - Full Name
* NICKNAME     - Nickname
* BDAY         - Birthday
* TITLE        - Work: Position
* ROLE         - Work: Role

For a full list of vCard fields check [XEP-0054: vcard-temp](https://xmpp.org/extensions/xep-0054.html)

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name
- *name* :: string : Field name

__Result:__

- *content* :: string : Field content

__Tags:__
[vcard](admin-tags.md#vcard)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/get_vcard
{
  "user": "user1",
  "host": "myserver.com",
  "name": "NICKNAME"
}

HTTP/1.1 200 OK
"User 1"
~~~




## get_vcard2


Get content from a vCard subfield


Some vcard field names and subnames in `get`/`set_vcard2` are:

* N FAMILY     - Family name
* N GIVEN      - Given name
* N MIDDLE     - Middle name
* ADR CTRY     - Address: Country
* ADR LOCALITY - Address: City
* TEL HOME     - Telephone: Home
* TEL CELL     - Telephone: Cellphone
* TEL WORK     - Telephone: Work
* TEL VOICE    - Telephone: Voice
* EMAIL USERID - E-Mail Address
* ORG ORGNAME  - Work: Company
* ORG ORGUNIT  - Work: Department

For a full list of vCard fields check [XEP-0054: vcard-temp](https://xmpp.org/extensions/xep-0054.html)

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name
- *name* :: string : Field name
- *subname* :: string : Subfield name

__Result:__

- *content* :: string : Field content

__Tags:__
[vcard](admin-tags.md#vcard)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/get_vcard2
{
  "user": "user1",
  "host": "myserver.com",
  "name": "N",
  "subname": "FAMILY"
}

HTTP/1.1 200 OK
"Schubert"
~~~




## get_vcard2_multi


Get multiple contents from a vCard field


Some vcard field names and subnames in `get`/`set_vcard2` are:

* N FAMILY     - Family name
* N GIVEN      - Given name
* N MIDDLE     - Middle name
* ADR CTRY     - Address: Country
* ADR LOCALITY - Address: City
* TEL HOME     - Telephone: Home
* TEL CELL     - Telephone: Cellphone
* TEL WORK     - Telephone: Work
* TEL VOICE    - Telephone: Voice
* EMAIL USERID - E-Mail Address
* ORG ORGNAME  - Work: Company
* ORG ORGUNIT  - Work: Department

For a full list of vCard fields check [XEP-0054: vcard-temp](https://xmpp.org/extensions/xep-0054.html)

__Arguments:__

- *user* :: string
- *host* :: string
- *name* :: string
- *subname* :: string

__Result:__

- *contents* :: [value::string]

__Tags:__
[vcard](admin-tags.md#vcard)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/get_vcard2_multi
{
  "user": "aaaaa",
  "host": "bbbbb",
  "name": "ccccc",
  "subname": "ddddd"
}

HTTP/1.1 200 OK
[
  "aaaaa",
  "bbbbb"
]
~~~




## halt

<!-- md:version added in [23.10](../../archive/23.10/index.md) -->

Halt ejabberd abruptly with status code 1

__Arguments:__


__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[server](admin-tags.md#server)

__Examples:__


~~~ json
POST /api/halt
{
  
}

HTTP/1.1 200 OK
""
~~~




## help


Get list of commands, or help of a command (only ejabberdctl)


This command is exclusive for the ejabberdctl command-line script, don't attempt to execute it using any other API frontend.

__Arguments:__


__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[ejabberdctl](admin-tags.md#ejabberdctl)

__Examples:__


~~~ json
POST /api/help
{
  
}

HTTP/1.1 200 OK
""
~~~




## import_dir


Import users data from jabberd14 spool dir

__Arguments:__

- *file* :: string : Full path to the jabberd14 spool directory

__Result:__

- *res* :: string : Raw result string

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/import_dir
{
  "file": "/var/lib/ejabberd/jabberd14/"
}

HTTP/1.1 200 OK
"Success"
~~~




## import_file


Import user data from jabberd14 spool file

__Arguments:__

- *file* :: string : Full path to the jabberd14 spool file

__Result:__

- *res* :: string : Raw result string

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/import_file
{
  "file": "/var/lib/ejabberd/jabberd14.spool"
}

HTTP/1.1 200 OK
"Success"
~~~




## import_piefxis


Import users data from a PIEFXIS file (XEP-0227)

__Arguments:__

- *file* :: string : Full path to the PIEFXIS file

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/import_piefxis
{
  "file": "/var/lib/ejabberd/example.com.xml"
}

HTTP/1.1 200 OK
""
~~~




## import_prosody


Import data from Prosody


Note: this requires ejabberd to be compiled with `./configure --enable-lua` (which installs the `luerl` library).

__Arguments:__

- *dir* :: string : Full path to the Prosody data directory

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[mnesia](admin-tags.md#mnesia), [sql](admin-tags.md#sql)

__Examples:__


~~~ json
POST /api/import_prosody
{
  "dir": "/var/lib/prosody/datadump/"
}

HTTP/1.1 200 OK
""
~~~




## incoming_s2s_number


Number of incoming s2s connections on the node

__Arguments:__


__Result:__

- *s2s_incoming* :: integer

__Tags:__
[statistics](admin-tags.md#statistics), [s2s](admin-tags.md#s2s)

__Examples:__


~~~ json
POST /api/incoming_s2s_number
{
  
}

HTTP/1.1 200 OK
1
~~~




## install_fallback


Install Mnesia database from a binary backup file


The binary backup file is installed as fallback: it will be used to restore the database at the next ejabberd start. This means that, after running this command, you have to restart ejabberd. This command requires less memory than [restore](#restore) API.

__Arguments:__

- *file* :: string : Full path to the fallback file

__Result:__

- *res* :: string : Raw result string

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/install_fallback
{
  "file": "/var/lib/ejabberd/database.fallback"
}

HTTP/1.1 200 OK
"Success"
~~~




## join_cluster

<!-- md:version improved in [24.06](../../archive/24.06/index.md) -->

Join our local node into the cluster handled by Node


This command returns immediately,
			even before the joining process has
			completed. Consequently, if you are using
			`ejabberdctl` (or some `CTL_ON_` container
			environment variables) to run more commands
			afterwards, you may want to precede them with
			the [started](admin-tags.md#started) command to ensure the
			clustering process has completed before
			proceeding. For example: `join_cluster
			ejabberd@main` > `started` > `list_cluster`.

__Arguments:__

- *node* :: string : Nodename of the node to join

__Result:__

- *res* :: string : Raw result string

__Tags:__
[cluster](admin-tags.md#cluster)

__Examples:__


~~~ json
POST /api/join_cluster
{
  "node": "ejabberd1@machine7"
}

HTTP/1.1 200 OK
"Success"
~~~




## join_cluster_here

<!-- md:version added in [24.06](../../archive/24.06/index.md) -->

Join a remote Node here, into our cluster

__Arguments:__

- *node* :: string : Nodename of the node to join here

__Result:__

- *res* :: string : Raw result string

__Tags:__
[cluster](admin-tags.md#cluster)

__Examples:__


~~~ json
POST /api/join_cluster_here
{
  "node": "ejabberd1@machine7"
}

HTTP/1.1 200 OK
"Success"
~~~




## kick_session


Kick a user session

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name
- *resource* :: string : User's resource
- *reason* :: string : Reason for closing session

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[session](admin-tags.md#session)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/kick_session
{
  "user": "peter",
  "host": "myserver.com",
  "resource": "Psi",
  "reason": "Stuck connection"
}

HTTP/1.1 200 OK
""
~~~




## kick_user

<!-- md:version modified in [24.06](../../archive/24.06/index.md) -->

Disconnect user's active sessions

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name

__Result:__

- *res* :: string : Raw result string

__Tags:__
[session](admin-tags.md#session), [v2](admin-tags.md#v2)

__Examples:__


~~~ json
POST /api/kick_user
{
  "user": "user1",
  "host": "example.com"
}

HTTP/1.1 200 OK
"Kicked sessions: 2"
~~~




## leave_cluster


Remove and shutdown Node from the running cluster


This command can be run from any running node of the cluster, even the node to be removed. In the removed node, this command works only when using ejabberdctl, not [mod_http_api](../../admin/configuration/modules.md#mod_http_api) or other code that runs inside the same ejabberd node that will leave.

__Arguments:__

- *node* :: string : Nodename of the node to kick from the cluster

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[cluster](admin-tags.md#cluster)

__Examples:__


~~~ json
POST /api/leave_cluster
{
  "node": "ejabberd1@machine8"
}

HTTP/1.1 200 OK
""
~~~




## list_certificates


Lists all ACME certificates

__Arguments:__


__Result:__

- *certificates* :: [{domain::string, file::string, used::string}]

__Tags:__
[acme](admin-tags.md#acme)

__Examples:__


~~~ json
POST /api/list_certificates
{
  
}

HTTP/1.1 200 OK
[
  {
    "domain": "aaaaa",
    "file": "bbbbb",
    "used": "ccccc"
  },
  {
    "domain": "ddddd",
    "file": "eeeee",
    "used": "fffff"
  }
]
~~~




## list_cluster


List running nodes that are part of this cluster

__Arguments:__


__Result:__

- *nodes* :: [node::string]

__Tags:__
[cluster](admin-tags.md#cluster)

__Examples:__


~~~ json
POST /api/list_cluster
{
  
}

HTTP/1.1 200 OK
[
  "ejabberd1@machine7",
  "ejabberd1@machine8"
]
~~~




## list_cluster_detailed

<!-- md:version added in [24.06](../../archive/24.06/index.md) -->

List nodes (both running and known) and some stats

__Arguments:__


__Result:__

- *nodes* :: [{name::string, running::string, status::string, online_users::integer, processes::integer, uptime_seconds::integer, master_node::string}]

__Tags:__
[cluster](admin-tags.md#cluster)

__Examples:__


~~~ json
POST /api/list_cluster_detailed
{
  
}

HTTP/1.1 200 OK
[
  {
    "name": "ejabberd@localhost",
    "running": "true",
    "status": "The node ejabberd is started. Status...",
    "online_users": 7,
    "processes": 348,
    "uptime_seconds": 60,
    "master_node": "none"
  }
]
~~~




## load


Restore Mnesia database from a text dump file


Restore immediately. This is not recommended for big databases, as it will consume much time, memory and processor. In that case it's preferable to use [backup](#backup) API and [install_fallback](#install_fallback) API.

__Arguments:__

- *file* :: string : Full path to the text file

__Result:__

- *res* :: string : Raw result string

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/load
{
  "file": "/var/lib/ejabberd/database.txt"
}

HTTP/1.1 200 OK
"Success"
~~~




## man

<!-- md:version added in [20.01](../../archive/20.01/index.md) -->

Generate Unix manpage for current ejabberd version

__Arguments:__


__Result:__

- *res* :: string : Raw result string

__Tags:__
[documentation](admin-tags.md#documentation)

__Examples:__


~~~ json
POST /api/man
{
  
}

HTTP/1.1 200 OK
"Success"
~~~




## mnesia_change_nodename


Change the erlang node name in a backup file

__Arguments:__

- *oldnodename* :: string : Name of the old erlang node
- *newnodename* :: string : Name of the new node
- *oldbackup* :: string : Path to old backup file
- *newbackup* :: string : Path to the new backup file

__Result:__

- *res* :: string : Raw result string

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/mnesia_change_nodename
{
  "oldnodename": "ejabberd@machine1",
  "newnodename": "ejabberd@machine2",
  "oldbackup": "/var/lib/ejabberd/old.backup",
  "newbackup": "/var/lib/ejabberd/new.backup"
}

HTTP/1.1 200 OK
"Success"
~~~




## mnesia_info


Dump info on global Mnesia state

__Arguments:__


__Result:__

- *res* :: string

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/mnesia_info
{
  
}

HTTP/1.1 200 OK
"aaaaa"
~~~




## mnesia_info_ctl

<!-- md:version renamed in [24.02](../../archive/24.02/index.md) -->

Show information of Mnesia system (only ejabberdctl)


This command is exclusive for the ejabberdctl command-line script, don't attempt to execute it using any other API frontend.

__Arguments:__


__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[ejabberdctl](admin-tags.md#ejabberdctl), [mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/mnesia_info_ctl
{
  
}

HTTP/1.1 200 OK
""
~~~




## mnesia_list_tables 🟤

<!-- md:version added in [25.03](../../archive/25.03/index.md) -->

List of Mnesia tables

__Arguments:__


__Result:__

- *tables* :: [{name::string, storage_type::string, elements::integer, memory_kb::integer, memory_mb::integer}]

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/mnesia_list_tables
{
  
}

HTTP/1.1 200 OK
[
  {
    "name": "aaaaa",
    "storage_type": "bbbbb",
    "elements": 1,
    "memory_kb": 2,
    "memory_mb": 3
  },
  {
    "name": "ccccc",
    "storage_type": "ddddd",
    "elements": 4,
    "memory_kb": 5,
    "memory_mb": 6
  }
]
~~~




## mnesia_table_change_storage 🟤

<!-- md:version added in [25.03](../../archive/25.03/index.md) -->

Change storage type of a Mnesia table


Storage type can be: `ram_copies`, `disc_copies`, `disc_only_copies`, `remote_copy`.

__Arguments:__

- *table* :: string
- *storage_type* :: string

__Result:__

- *res* :: string : Raw result string

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/mnesia_table_change_storage
{
  "table": "aaaaa",
  "storage_type": "bbbbb"
}

HTTP/1.1 200 OK
"Success"
~~~




## mnesia_table_info


Dump info on Mnesia table state

__Arguments:__

- *table* :: string : Mnesia table name

__Result:__

- *res* :: string

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/mnesia_table_info
{
  "table": "roster"
}

HTTP/1.1 200 OK
"aaaaa"
~~~




## module_check


Check the contributed module repository compliance

__Arguments:__

- *module* :: string : Module name

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[modules](admin-tags.md#modules)

__Examples:__


~~~ json
POST /api/module_check
{
  "module": "mod_rest"
}

HTTP/1.1 200 OK
""
~~~




## module_install


Compile, install and start an available contributed module

__Arguments:__

- *module* :: string : Module name

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[modules](admin-tags.md#modules)

__Examples:__


~~~ json
POST /api/module_install
{
  "module": "mod_rest"
}

HTTP/1.1 200 OK
""
~~~




## module_uninstall


Uninstall a contributed module

__Arguments:__

- *module* :: string : Module name

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[modules](admin-tags.md#modules)

__Examples:__


~~~ json
POST /api/module_uninstall
{
  "module": "mod_rest"
}

HTTP/1.1 200 OK
""
~~~




## module_upgrade


Upgrade the running code of an installed module


In practice, this uninstalls and installs the module

__Arguments:__

- *module* :: string : Module name

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[modules](admin-tags.md#modules)

__Examples:__


~~~ json
POST /api/module_upgrade
{
  "module": "mod_rest"
}

HTTP/1.1 200 OK
""
~~~




## modules_available


List the contributed modules available to install

__Arguments:__


__Result:__

- *modules* :: [{name::string, summary::string}] : List of tuples with module name and description

__Tags:__
[modules](admin-tags.md#modules)

__Examples:__


~~~ json
POST /api/modules_available
{
  
}

HTTP/1.1 200 OK
{
  "mod_cron": "Execute scheduled commands",
  "mod_rest": "ReST frontend"
}
~~~




## modules_installed


List the contributed modules already installed

__Arguments:__


__Result:__

- *modules* :: [{name::string, summary::string}] : List of tuples with module name and description

__Tags:__
[modules](admin-tags.md#modules)

__Examples:__


~~~ json
POST /api/modules_installed
{
  
}

HTTP/1.1 200 OK
{
  "mod_cron": "Execute scheduled commands",
  "mod_rest": "ReST frontend"
}
~~~




## modules_update_specs


Update the module source code from Git


A connection to Internet is required

__Arguments:__


__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[modules](admin-tags.md#modules)

__Examples:__


~~~ json
POST /api/modules_update_specs
{
  
}

HTTP/1.1 200 OK
""
~~~




## muc_online_rooms


List existing rooms


Ask for a specific host, or `global` to use all vhosts.

__Arguments:__

- *service* :: string : MUC service, or `global` for all

__Result:__

- *rooms* :: [room::string] : List of rooms JIDs

__Tags:__
[muc](admin-tags.md#muc)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/muc_online_rooms
{
  "service": "conference.example.com"
}

HTTP/1.1 200 OK
[
  "room1@conference.example.com",
  "room2@conference.example.com"
]
~~~




## muc_online_rooms_by_regex


List existing rooms filtered by regexp


Ask for a specific host, or `global` to use all vhosts.

__Arguments:__

- *service* :: string : MUC service, or `global` for all
- *regex* :: string : Regex pattern for room name

__Result:__

- *rooms* :: [{jid::string, public::string, participants::integer}] : List of rooms with summary

__Tags:__
[muc](admin-tags.md#muc)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/muc_online_rooms_by_regex
{
  "service": "conference.example.com",
  "regex": "^prefix"
}

HTTP/1.1 200 OK
[
  {
    "jid": "room1@conference.example.com",
    "public": "true",
    "participants": 10
  },
  {
    "jid": "room2@conference.example.com",
    "public": "false",
    "participants": 10
  }
]
~~~




## muc_register_nick

<!-- md:version updated in [24.12](../../archive/24.12/index.md) -->

Register a nick to a User JID in a MUC service

__Arguments:__

- *nick* :: string : nick
- *user* :: string : user name
- *host* :: string : user host
- *service* :: string : MUC service

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[muc](admin-tags.md#muc), [v3](admin-tags.md#v3)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/muc_register_nick
{
  "nick": "Tim",
  "user": "tim",
  "host": "example.org",
  "service": "conference.example.org"
}

HTTP/1.1 200 OK
""
~~~




## muc_unregister_nick

<!-- md:version updated in [24.12](../../archive/24.12/index.md) -->

Unregister the nick registered by that account in the MUC service

__Arguments:__

- *user* :: string : user name
- *host* :: string : user host
- *service* :: string : MUC service

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[muc](admin-tags.md#muc), [v3](admin-tags.md#v3)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/muc_unregister_nick
{
  "user": "tim",
  "host": "example.org",
  "service": "conference.example.org"
}

HTTP/1.1 200 OK
""
~~~




## num_resources


Get the number of resources of a user

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name

__Result:__

- *resources* :: integer : Number of active resources for a user

__Tags:__
[session](admin-tags.md#session)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/num_resources
{
  "user": "peter",
  "host": "myserver.com"
}

HTTP/1.1 200 OK
5
~~~




## oauth_add_client_implicit


Add OAuth client_id with implicit grant type

__Arguments:__

- *client_id* :: string
- *client_name* :: string
- *redirect_uri* :: string

__Result:__

- *res* :: string : Raw result string

__Tags:__
[oauth](admin-tags.md#oauth)

__Examples:__


~~~ json
POST /api/oauth_add_client_implicit
{
  "client_id": "aaaaa",
  "client_name": "bbbbb",
  "redirect_uri": "ccccc"
}

HTTP/1.1 200 OK
"Success"
~~~




## oauth_add_client_password


Add OAuth client_id with password grant type

__Arguments:__

- *client_id* :: string
- *client_name* :: string
- *secret* :: string

__Result:__

- *res* :: string : Raw result string

__Tags:__
[oauth](admin-tags.md#oauth)

__Examples:__


~~~ json
POST /api/oauth_add_client_password
{
  "client_id": "aaaaa",
  "client_name": "bbbbb",
  "secret": "ccccc"
}

HTTP/1.1 200 OK
"Success"
~~~




## oauth_issue_token

<!-- md:version updated in [24.02](../../archive/24.02/index.md) -->

Issue an OAuth token for the given jid

__Arguments:__

- *jid* :: string : Jid for which issue token
- *ttl* :: integer : Time to live of generated token in seconds
- *scopes* :: [scope::string] : List of scopes to allow

__Result:__

- *result* :: {token::string, scopes::[scope::string], expires_in::string}

__Tags:__
[oauth](admin-tags.md#oauth), [v1](admin-tags.md#v1)

__Examples:__


~~~ json
POST /api/oauth_issue_token
{
  "jid": "user@server.com",
  "ttl": 3600,
  "scopes": [
    "connected_users_number",
    "muc_online_rooms"
  ]
}

HTTP/1.1 200 OK
{
  "token": "aaaaa",
  "scopes": [
    "bbbbb",
    "ccccc"
  ],
  "expires_in": "ddddd"
}
~~~




## oauth_list_tokens


List OAuth tokens, user, scope, and seconds to expire (only Mnesia)


List  [OAuth](oauth.md) tokens, their user and scope, and how many seconds remain until expiry

__Arguments:__


__Result:__

- *tokens* :: [{token::string, user::string, scope::string, expires_in::string}]

__Tags:__
[oauth](admin-tags.md#oauth)

__Examples:__


~~~ json
POST /api/oauth_list_tokens
{
  
}

HTTP/1.1 200 OK
[
  {
    "token": "aaaaa",
    "user": "bbbbb",
    "scope": "ccccc",
    "expires_in": "ddddd"
  },
  {
    "token": "eeeee",
    "user": "fffff",
    "scope": "ggggg",
    "expires_in": "hhhhh"
  }
]
~~~




## oauth_remove_client


Remove OAuth client_id

__Arguments:__

- *client_id* :: string

__Result:__

- *res* :: string : Raw result string

__Tags:__
[oauth](admin-tags.md#oauth)

__Examples:__


~~~ json
POST /api/oauth_remove_client
{
  "client_id": "aaaaa"
}

HTTP/1.1 200 OK
"Success"
~~~




## oauth_revoke_token

<!-- md:version changed in [22.05](../../archive/22.05/index.md) -->

Revoke authorization for an OAuth token

__Arguments:__

- *token* :: string

__Result:__

- *res* :: string : Raw result string

__Tags:__
[oauth](admin-tags.md#oauth)

__Examples:__


~~~ json
POST /api/oauth_revoke_token
{
  "token": "aaaaa"
}

HTTP/1.1 200 OK
"Success"
~~~




## outgoing_s2s_number


Number of outgoing s2s connections on the node

__Arguments:__


__Result:__

- *s2s_outgoing* :: integer

__Tags:__
[statistics](admin-tags.md#statistics), [s2s](admin-tags.md#s2s)

__Examples:__


~~~ json
POST /api/outgoing_s2s_number
{
  
}

HTTP/1.1 200 OK
1
~~~




## print_sql_schema

<!-- md:version added in [24.02](../../archive/24.02/index.md) -->

Print SQL schema for the given RDBMS (only ejabberdctl)


This command is exclusive for the ejabberdctl command-line script, don't attempt to execute it using any other API frontend.

__Arguments:__

- *db_type* :: string : Database type: pgsql | mysql | sqlite
- *db_version* :: string : Your database version: 16.1, 8.2.0...
- *new_schema* :: string : Use new schema: 0, false, 1 or true

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[ejabberdctl](admin-tags.md#ejabberdctl), [sql](admin-tags.md#sql)

__Examples:__


~~~ json
POST /api/print_sql_schema
{
  "db_type": "pgsql",
  "db_version": "16.1",
  "new_schema": "true"
}

HTTP/1.1 200 OK
""
~~~




## privacy_set


Send a IQ set privacy stanza for a local account

__Arguments:__

- *user* :: string : Username
- *host* :: string : Server name
- *xmlquery* :: string : Query XML element

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[stanza](admin-tags.md#stanza)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/privacy_set
{
  "user": "user1",
  "host": "myserver.com",
  "xmlquery": "<query xmlns='jabber:iq:privacy'>..."
}

HTTP/1.1 200 OK
""
~~~




## private_get


Get some information from a user private storage

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name
- *element* :: string : Element name
- *ns* :: string : Namespace

__Result:__

- *res* :: string

__Tags:__
[private](admin-tags.md#private)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/private_get
{
  "user": "user1",
  "host": "myserver.com",
  "element": "storage",
  "ns": "storage:rosternotes"
}

HTTP/1.1 200 OK
"aaaaa"
~~~




## private_set


Set to the user private storage

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name
- *element* :: string : XML storage element

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[private](admin-tags.md#private)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/private_set
{
  "user": "user1",
  "host": "myserver.com",
  "element": "<storage xmlns='storage:rosternotes'/>"
}

HTTP/1.1 200 OK
""
~~~




## process_rosteritems


List/delete rosteritems that match filter


Explanation of each argument:

* `action`: what to do with each rosteritem that matches all the filtering options
* `subs`: subscription type
* `asks`: pending subscription
* `users`: the JIDs of the local user
* `contacts`: the JIDs of the contact in the roster

**Mnesia backend:**

Allowed values in the arguments:

* `action` = `list` | `delete`
* `subs` = `any` | SUB[:SUB]*
* `asks` = `any` | ASK[:ASK]*
* `users` = `any` | JID[:JID]*
* `contacts` = `any` | JID[:JID]*

where

* SUB = `none` | `from `| `to` | `both`
* ASK = `none` | `out` | `in`
* JID = characters valid in a JID, and can use the globs: `*`, `?`, `!` and `[...]`

This example will list roster items with subscription `none`, `from` or `to` that have any ask property, of local users which JID is in the virtual host `example.org` and that the contact JID is either a bare server name (without user part) or that has a user part and the server part contains the word `icq`:
  `list none:from:to any *@example.org *:*@*icq*`

**SQL backend:**

Allowed values in the arguments:

* `action` = `list` | `delete`
* `subs` = `any` | SUB
* `asks` = `any` | ASK
* `users` = JID
* `contacts` = JID

where

* SUB = `none` | `from` | `to` | `both`
* ASK = `none` | `out` | `in`
* JID = characters valid in a JID, and can use the globs: `_` and `%`

This example will list roster items with subscription `to` that have any ask property, of local users which JID is in the virtual host `example.org` and that the contact JID's server part contains the word `icq`:
  `list to any %@example.org %@%icq%`

__Arguments:__

- *action* :: string
- *subs* :: string
- *asks* :: string
- *users* :: string
- *contacts* :: string

__Result:__

- *response* :: [{user::string, contact::string}]

__Tags:__
[roster](admin-tags.md#roster)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/process_rosteritems
{
  "action": "aaaaa",
  "subs": "bbbbb",
  "asks": "ccccc",
  "users": "ddddd",
  "contacts": "eeeee"
}

HTTP/1.1 200 OK
[
  {
    "user": "aaaaa",
    "contact": "bbbbb"
  },
  {
    "user": "ccccc",
    "contact": "ddddd"
  }
]
~~~




## push_alltoall


Add all the users to all the users of Host in Group

__Arguments:__

- *host* :: string : Server name
- *group* :: string : Group name

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[roster](admin-tags.md#roster)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/push_alltoall
{
  "host": "myserver.com",
  "group": "Everybody"
}

HTTP/1.1 200 OK
""
~~~




## push_roster


Push template roster from file to a user


The text file must contain an erlang term: a list of tuples with username, servername, group and nick. For example:
`[{"user1", "localhost", "Workers", "User 1"},
 {"user2", "localhost", "Workers", "User 2"}].`

If there are problems parsing UTF8 character encoding, provide the corresponding string with the `<<"STRING"/utf8>>` syntax, for example:
`[{"user2", "localhost", "Workers", <<"User 2"/utf8>>}]`.

__Arguments:__

- *file* :: string : File path
- *user* :: string : User name
- *host* :: string : Server name

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[roster](admin-tags.md#roster)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/push_roster
{
  "file": "/home/ejabberd/roster.txt",
  "user": "user1",
  "host": "localhost"
}

HTTP/1.1 200 OK
""
~~~




## push_roster_all


Push template roster from file to all those users


The text file must contain an erlang term: a list of tuples with username, servername, group and nick. Example:
`[{"user1", "localhost", "Workers", "User 1"},
 {"user2", "localhost", "Workers", "User 2"}].`

__Arguments:__

- *file* :: string : File path

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[roster](admin-tags.md#roster)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/push_roster_all
{
  "file": "/home/ejabberd/roster.txt"
}

HTTP/1.1 200 OK
""
~~~




## register


Register a user

__Arguments:__

- *user* :: string : Username
- *host* :: string : Local vhost served by ejabberd
- *password* :: string : Password

__Result:__

- *res* :: string : Raw result string

__Tags:__
[accounts](admin-tags.md#accounts)

__Examples:__


~~~ json
POST /api/register
{
  "user": "bob",
  "host": "example.com",
  "password": "SomEPass44"
}

HTTP/1.1 200 OK
"Success"
~~~




## registered_users


List all registered users in HOST

__Arguments:__

- *host* :: string : Local vhost

__Result:__

- *users* :: [username::string] : List of registered accounts usernames

__Tags:__
[accounts](admin-tags.md#accounts)

__Examples:__


~~~ json
POST /api/registered_users
{
  "host": "example.com"
}

HTTP/1.1 200 OK
[
  "user1",
  "user2"
]
~~~




## registered_vhosts


List all registered vhosts in SERVER

__Arguments:__


__Result:__

- *vhosts* :: [vhost::string] : List of available vhosts

__Tags:__
[server](admin-tags.md#server)

__Examples:__


~~~ json
POST /api/registered_vhosts
{
  
}

HTTP/1.1 200 OK
[
  "example.com",
  "anon.example.com"
]
~~~




## reload_config


Reload config file in memory

__Arguments:__


__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[config](admin-tags.md#config)

__Examples:__


~~~ json
POST /api/reload_config
{
  
}

HTTP/1.1 200 OK
""
~~~




## remove_mam_for_user


Remove mam archive for user

__Arguments:__

- *user* :: string : Username
- *host* :: string : Server

__Result:__

- *res* :: string : Raw result string

__Tags:__
[mam](admin-tags.md#mam)

__Module:__
[mod_mam](../../admin/configuration/modules.md#mod_mam)

__Examples:__


~~~ json
POST /api/remove_mam_for_user
{
  "user": "bob",
  "host": "example.com"
}

HTTP/1.1 200 OK
"MAM archive removed"
~~~




## remove_mam_for_user_with_peer


Remove mam archive for user with peer

__Arguments:__

- *user* :: string : Username
- *host* :: string : Server
- *with* :: string : Peer

__Result:__

- *res* :: string : Raw result string

__Tags:__
[mam](admin-tags.md#mam)

__Module:__
[mod_mam](../../admin/configuration/modules.md#mod_mam)

__Examples:__


~~~ json
POST /api/remove_mam_for_user_with_peer
{
  "user": "bob",
  "host": "example.com",
  "with": "anne@example.com"
}

HTTP/1.1 200 OK
"MAM archive removed"
~~~




## reopen_log


Reopen maybe the log files after being renamed


Has no effect on ejabberd main log files, only on log files generated by some modules.
This can be useful when an external tool is used for log rotation. See [Log Files](../../admin/guide/troubleshooting.md#log-files).

__Arguments:__


__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[logs](admin-tags.md#logs)

__Examples:__


~~~ json
POST /api/reopen_log
{
  
}

HTTP/1.1 200 OK
""
~~~




## request_certificate


Requests certificates for all or some domains


Domains can be `all`, or a list of domains separared with comma characters

__Arguments:__

- *domains* :: string : Domains for which to acquire a certificate

__Result:__

- *res* :: string : Raw result string

__Tags:__
[acme](admin-tags.md#acme)

__Examples:__


~~~ json
POST /api/request_certificate
{
  "domains": "example.com,domain.tld,conference.domain.tld"
}

HTTP/1.1 200 OK
"Success"
~~~




## resource_num


Resource string of a session number

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name
- *num* :: integer : ID of resource to return

__Result:__

- *resource* :: string : Name of user resource

__Tags:__
[session](admin-tags.md#session)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/resource_num
{
  "user": "peter",
  "host": "myserver.com",
  "num": 2
}

HTTP/1.1 200 OK
"Psi"
~~~




## restart


Restart ejabberd gracefully

__Arguments:__


__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[server](admin-tags.md#server)

__Examples:__


~~~ json
POST /api/restart
{
  
}

HTTP/1.1 200 OK
""
~~~




## restart_module


Stop an ejabberd module, reload code and start

__Arguments:__

- *host* :: string : Server name
- *module* :: string : Module to restart

__Result:__

- *res* :: integer : Returns integer code:
 - `0`: code reloaded, module restarted
 - `1`: error: module not loaded
 - `2`: code not reloaded, but module restarted

__Tags:__
[erlang](admin-tags.md#erlang)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/restart_module
{
  "host": "myserver.com",
  "module": "mod_admin_extra"
}

HTTP/1.1 200 OK
0
~~~




## restore


Restore the Mnesia database from a binary backup file


This restores immediately from a binary backup file the internal Mnesia database. This will consume a lot of memory if you have a large database, you may prefer [install_fallback](#install_fallback) API.

__Arguments:__

- *file* :: string : Full path to the backup file

__Result:__

- *res* :: string : Raw result string

__Tags:__
[mnesia](admin-tags.md#mnesia)

__Examples:__


~~~ json
POST /api/restore
{
  "file": "/var/lib/ejabberd/database.backup"
}

HTTP/1.1 200 OK
"Success"
~~~




## revoke_certificate


Revokes the selected ACME certificate

__Arguments:__

- *file* :: string : Filename of the certificate

__Result:__

- *res* :: string : Raw result string

__Tags:__
[acme](admin-tags.md#acme)

__Examples:__


~~~ json
POST /api/revoke_certificate
{
  "file": "aaaaa"
}

HTTP/1.1 200 OK
"Success"
~~~




## rooms_empty_destroy

<!-- md:version modified in [24.06](../../archive/24.06/index.md) -->

Destroy the rooms that have no messages in archive


The MUC service argument can be `global` to get all hosts.

__Arguments:__

- *service* :: string : MUC service, or `global` for all

__Result:__

- *res* :: string : Raw result string

__Tags:__
[muc](admin-tags.md#muc), [v2](admin-tags.md#v2)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/rooms_empty_destroy
{
  "service": "conference.example.com"
}

HTTP/1.1 200 OK
"Destroyed rooms: 2"
~~~




## rooms_empty_list


List the rooms that have no messages in archive


The MUC service argument can be `global` to get all hosts.

__Arguments:__

- *service* :: string : MUC service, or `global` for all

__Result:__

- *rooms* :: [room::string] : List of empty rooms

__Tags:__
[muc](admin-tags.md#muc)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/rooms_empty_list
{
  "service": "conference.example.com"
}

HTTP/1.1 200 OK
[
  "room1@conference.example.com",
  "room2@conference.example.com"
]
~~~




## rooms_unused_destroy


Destroy the rooms that are unused for many days in the service


The room recent history is used, so it's recommended  to wait a few days after service start before running this. The MUC service argument can be `global` to get all hosts.

__Arguments:__

- *service* :: string : MUC service, or `global` for all
- *days* :: integer : Number of days

__Result:__

- *rooms* :: [room::string] : List of unused rooms that has been destroyed

__Tags:__
[muc](admin-tags.md#muc)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/rooms_unused_destroy
{
  "service": "conference.example.com",
  "days": 31
}

HTTP/1.1 200 OK
[
  "room1@conference.example.com",
  "room2@conference.example.com"
]
~~~




## rooms_unused_list


List the rooms that are unused for many days in the service


The room recent history is used, so it's recommended  to wait a few days after service start before running this. The MUC service argument can be `global` to get all hosts.

__Arguments:__

- *service* :: string : MUC service, or `global` for all
- *days* :: integer : Number of days

__Result:__

- *rooms* :: [room::string] : List of unused rooms

__Tags:__
[muc](admin-tags.md#muc)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/rooms_unused_list
{
  "service": "conference.example.com",
  "days": 31
}

HTTP/1.1 200 OK
[
  "room1@conference.example.com",
  "room2@conference.example.com"
]
~~~




## rotate_log


Rotate maybe log file of some module


Has no effect on ejabberd main log files, only on log files generated by some modules.

__Arguments:__


__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[logs](admin-tags.md#logs)

__Examples:__


~~~ json
POST /api/rotate_log
{
  
}

HTTP/1.1 200 OK
""
~~~




## send_direct_invitation

<!-- md:version updated in [24.02](../../archive/24.02/index.md) -->

Send a direct invitation to several destinations


Since ejabberd [20.12](../../archive/20.12/index.md), this command is asynchronous: the API call may return before the server has send all the invitations.

`password` and `message` can be set to `none`.

__Arguments:__

- *room* :: string : Room name
- *service* :: string : MUC service
- *password* :: string : Password, or `none`
- *reason* :: string : Reason text, or `none`
- *users* :: [jid::string] : List of users JIDs

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[muc_room](admin-tags.md#muc_room), [v1](admin-tags.md#v1)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/send_direct_invitation
{
  "room": "room1",
  "service": "conference.example.com",
  "password": "",
  "reason": "Check this out!",
  "users": [
    "user2@localhost",
    "user3@example.com"
  ]
}

HTTP/1.1 200 OK
""
~~~




## send_message


Send a message to a local or remote bare of full JID


When sending a groupchat message to a MUC room, `from` must be the full JID of a room occupant, or the bare JID of a MUC service admin, or the bare JID of a MUC/Sub subscribed user.

__Arguments:__

- *type* :: string : Message type: `normal`, `chat`, `headline`, `groupchat`
- *from* :: string : Sender JID
- *to* :: string : Receiver JID
- *subject* :: string : Subject, or empty string
- *body* :: string : Body

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[stanza](admin-tags.md#stanza)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/send_message
{
  "type": "headline",
  "from": "admin@localhost",
  "to": "user1@localhost",
  "subject": "Restart",
  "body": "In 5 minutes"
}

HTTP/1.1 200 OK
""
~~~




## send_stanza


Send a stanza; provide From JID and valid To JID

__Arguments:__

- *from* :: string : Sender JID
- *to* :: string : Destination JID
- *stanza* :: string : Stanza

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[stanza](admin-tags.md#stanza)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/send_stanza
{
  "from": "admin@localhost",
  "to": "user1@localhost",
  "stanza": "<message><ext attr='value'/></message>"
}

HTTP/1.1 200 OK
""
~~~




## send_stanza_c2s


Send a stanza from an existing C2S session


`user`@`host`/`resource` must be an existing C2S session. As an alternative, use [send_stanza](#send_stanza) API instead.

__Arguments:__

- *user* :: string : Username
- *host* :: string : Server name
- *resource* :: string : Resource
- *stanza* :: string : Stanza

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[stanza](admin-tags.md#stanza)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/send_stanza_c2s
{
  "user": "admin",
  "host": "myserver.com",
  "resource": "bot",
  "stanza": "<message to='user1@localhost'><ext attr='value'/></message>"
}

HTTP/1.1 200 OK
""
~~~




## set_last


Set last activity information


Timestamp is the seconds since `1970-01-01 00:00:00 UTC`. For example value see `date +%s`

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name
- *timestamp* :: integer : Number of seconds since epoch
- *status* :: string : Status message

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[last](admin-tags.md#last)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/set_last
{
  "user": "user1",
  "host": "myserver.com",
  "timestamp": 1500045311,
  "status": "GoSleeping"
}

HTTP/1.1 200 OK
""
~~~




## set_loglevel


Set the loglevel


Possible loglevels: `none`, `emergency`, `alert`, `critical`,
			           `error`, `warning`, `notice`, `info`, `debug`.

__Arguments:__

- *loglevel* :: string : Desired logging level

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[logs](admin-tags.md#logs)

__Examples:__


~~~ json
POST /api/set_loglevel
{
  "loglevel": "debug"
}

HTTP/1.1 200 OK
""
~~~




## set_master


Set master node of the clustered Mnesia tables


If `nodename` is set to `self`, then this node will be set as its own master.

__Arguments:__

- *nodename* :: string : Name of the erlang node that will be considered master of this node

__Result:__

- *res* :: string : Raw result string

__Tags:__
[cluster](admin-tags.md#cluster)

__Examples:__


~~~ json
POST /api/set_master
{
  "nodename": "ejabberd@machine7"
}

HTTP/1.1 200 OK
"Success"
~~~




## set_nickname


Set nickname in a user's vCard

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name
- *nickname* :: string : Nickname

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[vcard](admin-tags.md#vcard)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/set_nickname
{
  "user": "user1",
  "host": "myserver.com",
  "nickname": "User 1"
}

HTTP/1.1 200 OK
""
~~~




## set_presence

<!-- md:version updated in [24.02](../../archive/24.02/index.md) -->

Set presence of a session

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name
- *resource* :: string : Resource
- *type* :: string : Type: `available`, `error`, `probe`...
- *show* :: string : Show: `away`, `chat`, `dnd`, `xa`.
- *status* :: string : Status text
- *priority* :: integer : Priority, provide this value as an integer

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[session](admin-tags.md#session), [v1](admin-tags.md#v1)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/set_presence
{
  "user": "user1",
  "host": "myserver.com",
  "resource": "tka1",
  "type": "available",
  "show": "away",
  "status": "BB",
  "priority": 7
}

HTTP/1.1 200 OK
""
~~~




## set_room_affiliation

<!-- md:version updated in [24.12](../../archive/24.12/index.md) -->

Change an affiliation in a MUC room


If affiliation is `none`, then the affiliation is removed.

__Arguments:__

- *room* :: string : room name
- *service* :: string : MUC service
- *user* :: string : user name
- *host* :: string : user host
- *affiliation* :: string : affiliation to set

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[muc_room](admin-tags.md#muc_room), [v3](admin-tags.md#v3)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/set_room_affiliation
{
  "room": "room1",
  "service": "conference.example.com",
  "user": "sun",
  "host": "localhost",
  "affiliation": "member"
}

HTTP/1.1 200 OK
""
~~~




## set_vcard


Set content in a vCard field


Some vcard field names in `get`/`set_vcard` are:

* FN           - Full Name
* NICKNAME     - Nickname
* BDAY         - Birthday
* TITLE        - Work: Position
* ROLE         - Work: Role

For a full list of vCard fields check [XEP-0054: vcard-temp](https://xmpp.org/extensions/xep-0054.html)

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name
- *name* :: string : Field name
- *content* :: string : Value

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[vcard](admin-tags.md#vcard)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/set_vcard
{
  "user": "user1",
  "host": "myserver.com",
  "name": "URL",
  "content": "www.example.com"
}

HTTP/1.1 200 OK
""
~~~




## set_vcard2


Set content in a vCard subfield


Some vcard field names and subnames in `get`/`set_vcard2` are:

* N FAMILY     - Family name
* N GIVEN      - Given name
* N MIDDLE     - Middle name
* ADR CTRY     - Address: Country
* ADR LOCALITY - Address: City
* TEL HOME     - Telephone: Home
* TEL CELL     - Telephone: Cellphone
* TEL WORK     - Telephone: Work
* TEL VOICE    - Telephone: Voice
* EMAIL USERID - E-Mail Address
* ORG ORGNAME  - Work: Company
* ORG ORGUNIT  - Work: Department

For a full list of vCard fields check [XEP-0054: vcard-temp](https://xmpp.org/extensions/xep-0054.html)

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name
- *name* :: string : Field name
- *subname* :: string : Subfield name
- *content* :: string : Value

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[vcard](admin-tags.md#vcard)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/set_vcard2
{
  "user": "user1",
  "host": "myserver.com",
  "name": "TEL",
  "subname": "NUMBER",
  "content": "123456"
}

HTTP/1.1 200 OK
""
~~~




## set_vcard2_multi


Set multiple contents in a vCard subfield


Some vcard field names and subnames in `get`/`set_vcard2` are:

* N FAMILY     - Family name
* N GIVEN      - Given name
* N MIDDLE     - Middle name
* ADR CTRY     - Address: Country
* ADR LOCALITY - Address: City
* TEL HOME     - Telephone: Home
* TEL CELL     - Telephone: Cellphone
* TEL WORK     - Telephone: Work
* TEL VOICE    - Telephone: Voice
* EMAIL USERID - E-Mail Address
* ORG ORGNAME  - Work: Company
* ORG ORGUNIT  - Work: Department

For a full list of vCard fields check [XEP-0054: vcard-temp](https://xmpp.org/extensions/xep-0054.html)

__Arguments:__

- *user* :: string
- *host* :: string
- *name* :: string
- *subname* :: string
- *contents* :: [value::string]

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[vcard](admin-tags.md#vcard)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/set_vcard2_multi
{
  "user": "aaaaa",
  "host": "bbbbb",
  "name": "ccccc",
  "subname": "ddddd",
  "contents": [
    "eeeee",
    "fffff"
  ]
}

HTTP/1.1 200 OK
""
~~~




## srg_add

<!-- md:version added in [24.06](../../archive/24.06/index.md) -->

Add/Create a Shared Roster Group (without details)

__Arguments:__

- *group* :: string : Group identifier
- *host* :: string : Group server name

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[shared_roster_group](admin-tags.md#shared_roster_group)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/srg_add
{
  "group": "group3",
  "host": "myserver.com"
}

HTTP/1.1 200 OK
""
~~~




## srg_add_displayed

<!-- md:version added in [24.06](../../archive/24.06/index.md) -->

Add a group to displayed_groups of a Shared Roster Group

__Arguments:__

- *group* :: string : Group identifier
- *host* :: string : Group server name
- *add* :: string : Group to add to displayed_groups

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[shared_roster_group](admin-tags.md#shared_roster_group)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/srg_add_displayed
{
  "group": "group3",
  "host": "myserver.com",
  "add": "group1"
}

HTTP/1.1 200 OK
""
~~~




## srg_create

<!-- md:version updated in [24.02](../../archive/24.02/index.md) -->

Create a Shared Roster Group

__Arguments:__

- *group* :: string : Group identifier
- *host* :: string : Group server name
- *label* :: string : Group name
- *description* :: string : Group description
- *display* :: [group::string] : List of groups to display

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[shared_roster_group](admin-tags.md#shared_roster_group), [v1](admin-tags.md#v1)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/srg_create
{
  "group": "group3",
  "host": "myserver.com",
  "label": "Group3",
  "description": "Third group",
  "display": [
    "group1",
    "group2"
  ]
}

HTTP/1.1 200 OK
""
~~~




## srg_del_displayed

<!-- md:version added in [24.06](../../archive/24.06/index.md) -->

Delete a group from displayed_groups of a Shared Roster Group

__Arguments:__

- *group* :: string : Group identifier
- *host* :: string : Group server name
- *del* :: string : Group to delete from displayed_groups

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[shared_roster_group](admin-tags.md#shared_roster_group)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/srg_del_displayed
{
  "group": "group3",
  "host": "myserver.com",
  "del": "group1"
}

HTTP/1.1 200 OK
""
~~~




## srg_delete


Delete a Shared Roster Group

__Arguments:__

- *group* :: string : Group identifier
- *host* :: string : Group server name

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[shared_roster_group](admin-tags.md#shared_roster_group)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/srg_delete
{
  "group": "group3",
  "host": "myserver.com"
}

HTTP/1.1 200 OK
""
~~~




## srg_get_displayed

<!-- md:version added in [24.06](../../archive/24.06/index.md) -->

Get displayed groups of a Shared Roster Group

__Arguments:__

- *group* :: string : Group identifier
- *host* :: string : Group server name

__Result:__

- *display* :: [group::string] : List of groups to display

__Tags:__
[shared_roster_group](admin-tags.md#shared_roster_group)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/srg_get_displayed
{
  "group": "group3",
  "host": "myserver.com"
}

HTTP/1.1 200 OK
[
  "group1",
  "group2"
]
~~~




## srg_get_info


Get info of a Shared Roster Group

__Arguments:__

- *group* :: string : Group identifier
- *host* :: string : Group server name

__Result:__

- *informations* :: [{key::string, value::string}] : List of group information, as key and value

__Tags:__
[shared_roster_group](admin-tags.md#shared_roster_group)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/srg_get_info
{
  "group": "group3",
  "host": "myserver.com"
}

HTTP/1.1 200 OK
[
  {
    "key": "name",
    "value": "Group 3"
  },
  {
    "key": "displayed_groups",
    "value": "group1"
  }
]
~~~




## srg_get_members


Get members of a Shared Roster Group

__Arguments:__

- *group* :: string : Group identifier
- *host* :: string : Group server name

__Result:__

- *members* :: [member::string] : List of group identifiers

__Tags:__
[shared_roster_group](admin-tags.md#shared_roster_group)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/srg_get_members
{
  "group": "group3",
  "host": "myserver.com"
}

HTTP/1.1 200 OK
[
  "user1@localhost",
  "user2@localhost"
]
~~~




## srg_list


List the Shared Roster Groups in Host

__Arguments:__

- *host* :: string : Server name

__Result:__

- *groups* :: [id::string] : List of group identifiers

__Tags:__
[shared_roster_group](admin-tags.md#shared_roster_group)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/srg_list
{
  "host": "myserver.com"
}

HTTP/1.1 200 OK
[
  "group1",
  "group2"
]
~~~




## srg_set_info

<!-- md:version added in [24.06](../../archive/24.06/index.md) -->

Set info of a Shared Roster Group

__Arguments:__

- *group* :: string : Group identifier
- *host* :: string : Group server name
- *key* :: string : Information key: label, description
- *value* :: string : Information value

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[shared_roster_group](admin-tags.md#shared_roster_group)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/srg_set_info
{
  "group": "group3",
  "host": "myserver.com",
  "key": "label",
  "value": "Family"
}

HTTP/1.1 200 OK
""
~~~




## srg_user_add


Add the JID user@host to the Shared Roster Group

__Arguments:__

- *user* :: string : Username
- *host* :: string : User server name
- *group* :: string : Group identifier
- *grouphost* :: string : Group server name

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[shared_roster_group](admin-tags.md#shared_roster_group)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/srg_user_add
{
  "user": "user1",
  "host": "myserver.com",
  "group": "group3",
  "grouphost": "myserver.com"
}

HTTP/1.1 200 OK
""
~~~




## srg_user_del


Delete this JID user@host from the Shared Roster Group

__Arguments:__

- *user* :: string : Username
- *host* :: string : User server name
- *group* :: string : Group identifier
- *grouphost* :: string : Group server name

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[shared_roster_group](admin-tags.md#shared_roster_group)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/srg_user_del
{
  "user": "user1",
  "host": "myserver.com",
  "group": "group3",
  "grouphost": "myserver.com"
}

HTTP/1.1 200 OK
""
~~~




## stats


Get some statistical value for the whole ejabberd server


Allowed statistics `name` are: `registeredusers`, `onlineusers`, `onlineusersnode`, `uptimeseconds`, `processes`.

__Arguments:__

- *name* :: string : Statistic name

__Result:__

- *stat* :: integer : Integer statistic value

__Tags:__
[statistics](admin-tags.md#statistics)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/stats
{
  "name": "registeredusers"
}

HTTP/1.1 200 OK
6
~~~




## stats_host


Get some statistical value for this host


Allowed statistics `name` are: `registeredusers`, `onlineusers`.

__Arguments:__

- *name* :: string : Statistic name
- *host* :: string : Server JID

__Result:__

- *stat* :: integer : Integer statistic value

__Tags:__
[statistics](admin-tags.md#statistics)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/stats_host
{
  "name": "registeredusers",
  "host": "example.com"
}

HTTP/1.1 200 OK
6
~~~




## status


Get status of the ejabberd server

__Arguments:__


__Result:__

- *res* :: string : Raw result string

__Tags:__
[server](admin-tags.md#server)

__Examples:__


~~~ json
POST /api/status
{
  
}

HTTP/1.1 200 OK
"The node ejabberd@localhost is started with status: startedejabberd X.X is running in that node"
~~~




## status_list

<!-- md:version updated in [24.12](../../archive/24.12/index.md) -->

List of logged users with this status

__Arguments:__

- *status* :: string : Status type to check

__Result:__

- *users* :: [{jid::string, priority::integer, status::string}]

__Tags:__
[session](admin-tags.md#session), [v3](admin-tags.md#v3)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/status_list
{
  "status": "dnd"
}

HTTP/1.1 200 OK
[
  {
    "jid": "peter@myserver.com/tka",
    "priority": 6,
    "status": "Busy"
  }
]
~~~




## status_list_host

<!-- md:version updated in [24.12](../../archive/24.12/index.md) -->

List of users logged in host with their statuses

__Arguments:__

- *host* :: string : Server name
- *status* :: string : Status type to check

__Result:__

- *users* :: [{jid::string, priority::integer, status::string}]

__Tags:__
[session](admin-tags.md#session), [v3](admin-tags.md#v3)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/status_list_host
{
  "host": "myserver.com",
  "status": "dnd"
}

HTTP/1.1 200 OK
[
  {
    "jid": "peter@myserver.com/tka",
    "priority": 6,
    "status": "Busy"
  }
]
~~~




## status_num


Number of logged users with this status

__Arguments:__

- *status* :: string : Status type to check

__Result:__

- *users* :: integer : Number of connected sessions with given status type

__Tags:__
[session](admin-tags.md#session), [statistics](admin-tags.md#statistics)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/status_num
{
  "status": "dnd"
}

HTTP/1.1 200 OK
23
~~~




## status_num_host


Number of logged users with this status in host

__Arguments:__

- *host* :: string : Server name
- *status* :: string : Status type to check

__Result:__

- *users* :: integer : Number of connected sessions with given status type

__Tags:__
[session](admin-tags.md#session), [statistics](admin-tags.md#statistics)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/status_num_host
{
  "host": "myserver.com",
  "status": "dnd"
}

HTTP/1.1 200 OK
23
~~~




## stop


Stop ejabberd gracefully

__Arguments:__


__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[server](admin-tags.md#server)

__Examples:__


~~~ json
POST /api/stop
{
  
}

HTTP/1.1 200 OK
""
~~~




## stop_kindly


Stop kindly the server (informing users)


Inform users and rooms, wait, and stop the server.
Provide the delay in seconds, and the announcement quoted, for example: 
`ejabberdctl stop_kindly 60 \"The server will stop in one minute.\"`

__Arguments:__

- *delay* :: integer : Seconds to wait
- *announcement* :: string : Announcement to send, with quotes

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[server](admin-tags.md#server)

__Examples:__


~~~ json
POST /api/stop_kindly
{
  "delay": 60,
  "announcement": "Server will stop now."
}

HTTP/1.1 200 OK
""
~~~




## stop_s2s_connections


Stop all s2s outgoing and incoming connections

__Arguments:__


__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[s2s](admin-tags.md#s2s)

__Examples:__


~~~ json
POST /api/stop_s2s_connections
{
  
}

HTTP/1.1 200 OK
""
~~~




## subscribe_room

<!-- md:version updated in [24.12](../../archive/24.12/index.md) -->

Subscribe to a MUC conference

__Arguments:__

- *user* :: string : user name
- *host* :: string : user host
- *nick* :: string : user nick
- *room* :: string : room name
- *service* :: string : MUC service
- *nodes* :: [node::string] : list of nodes

__Result:__

- *nodes* :: [node::string] : The list of nodes that has subscribed

__Tags:__
[muc_room](admin-tags.md#muc_room), [muc_sub](admin-tags.md#muc_sub), [v3](admin-tags.md#v3)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/subscribe_room
{
  "user": "tom",
  "host": "localhost",
  "nick": "Tom",
  "room": "room1",
  "service": "conference.localhost",
  "nodes": [
    "urn:xmpp:mucsub:nodes:messages",
    "urn:xmpp:mucsub:nodes:affiliations"
  ]
}

HTTP/1.1 200 OK
[
  "urn:xmpp:mucsub:nodes:messages",
  "urn:xmpp:mucsub:nodes:affiliations"
]
~~~




## subscribe_room_many

<!-- md:version updated in [24.12](../../archive/24.12/index.md) -->

Subscribe several users to a MUC conference


This command accepts up to 50 users at once (this is configurable with the [mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin) option `subscribe_room_many_max_users`)

__Arguments:__

- *users* :: [{user::string, host::string, nick::string}] : List of tuples with users name, host and nick
- *room* :: string : room name
- *service* :: string : MUC service
- *nodes* :: [node::string] : nodes separated by commas: `,`

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[muc_room](admin-tags.md#muc_room), [muc_sub](admin-tags.md#muc_sub), [v3](admin-tags.md#v3)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/subscribe_room_many
{
  "users": [
    {
      "user": "tom",
      "host": "localhost",
      "nick": "Tom"
    },
    {
      "user": "jerry",
      "host": "localhost",
      "nick": "Jerry"
    }
  ],
  "room": "room1",
  "service": "conference.localhost",
  "nodes": [
    "urn:xmpp:mucsub:nodes:messages",
    "urn:xmpp:mucsub:nodes:affiliations"
  ]
}

HTTP/1.1 200 OK
""
~~~




## unban_account

<!-- md:version added in [24.06](../../archive/24.06/index.md) -->

Revert the ban from an account: set back the old password


Check [ban_account](#ban_account) API.

__Arguments:__

- *user* :: string : User name to unban
- *host* :: string : Server name

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[accounts](admin-tags.md#accounts), [v2](admin-tags.md#v2)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/unban_account
{
  "user": "gooduser",
  "host": "myserver.com"
}

HTTP/1.1 200 OK
""
~~~




## unban_ip


Remove banned IP addresses from the fail2ban table


Accepts an IP address with a network mask. Returns the number of unbanned addresses, or a negative integer if there were any error.

__Arguments:__

- *address* :: string : IP address, optionally with network mask.

__Result:__

- *unbanned* :: integer : Amount of unbanned entries, or negative in case of error.

__Tags:__
[accounts](admin-tags.md#accounts)

__Module:__
[mod_fail2ban](../../admin/configuration/modules.md#mod_fail2ban)

__Examples:__


~~~ json
POST /api/unban_ip
{
  "address": "::FFFF:127.0.0.1/128"
}

HTTP/1.1 200 OK
3
~~~




## unregister


Unregister a user


This deletes the authentication and all the data associated to the account (roster, vcard...).

__Arguments:__

- *user* :: string : Username
- *host* :: string : Local vhost served by ejabberd

__Result:__

- *res* :: string : Raw result string

__Tags:__
[accounts](admin-tags.md#accounts)

__Examples:__


~~~ json
POST /api/unregister
{
  "user": "bob",
  "host": "example.com"
}

HTTP/1.1 200 OK
"Success"
~~~




## unsubscribe_room

<!-- md:version updated in [24.12](../../archive/24.12/index.md) -->

Unsubscribe from a MUC conference

__Arguments:__

- *user* :: string : user name
- *host* :: string : user host
- *room* :: string : room name
- *service* :: string : MUC service

__Result:__

- *res* :: integer : Status code (`0` on success, `1` otherwise)

__Tags:__
[muc_room](admin-tags.md#muc_room), [muc_sub](admin-tags.md#muc_sub), [v3](admin-tags.md#v3)

__Module:__
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)

__Examples:__


~~~ json
POST /api/unsubscribe_room
{
  "user": "tom",
  "host": "localhost",
  "room": "room1",
  "service": "conference.localhost"
}

HTTP/1.1 200 OK
""
~~~




## update

<!-- md:version improved in [24.10](../../archive/24.10/index.md) -->

Update the given module


To update all the possible modules, use `all`.

__Arguments:__

- *module* :: string

__Result:__

- *res* :: string : Raw result string

__Tags:__
[server](admin-tags.md#server)

__Examples:__


~~~ json
POST /api/update
{
  "module": "all"
}

HTTP/1.1 200 OK
"Updated modules: mod_configure, mod_vcard"
~~~




## update_list


List modified modules that can be updated

__Arguments:__


__Result:__

- *modules* :: [module::string]

__Tags:__
[server](admin-tags.md#server)

__Examples:__


~~~ json
POST /api/update_list
{
  
}

HTTP/1.1 200 OK
[
  "mod_configure",
  "mod_vcard"
]
~~~




## user_resources


List user's connected resources

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name

__Result:__

- *resources* :: [resource::string]

__Tags:__
[session](admin-tags.md#session)

__Examples:__


~~~ json
POST /api/user_resources
{
  "user": "user1",
  "host": "example.com"
}

HTTP/1.1 200 OK
[
  "tka1",
  "Gajim",
  "mobile-app"
]
~~~




## user_sessions_info


Get information about all sessions of a user

__Arguments:__

- *user* :: string : User name
- *host* :: string : Server name

__Result:__

- *sessions_info* :: [{connection::string, ip::string, port::integer, priority::integer, node::string, uptime::integer, status::string, resource::string, statustext::string}]

__Tags:__
[session](admin-tags.md#session)

__Module:__
[mod_admin_extra](../../admin/configuration/modules.md#mod_admin_extra)

__Examples:__


~~~ json
POST /api/user_sessions_info
{
  "user": "peter",
  "host": "myserver.com"
}

HTTP/1.1 200 OK
[
  {
    "connection": "c2s",
    "ip": "127.0.0.1",
    "port": 42656,
    "priority": 8,
    "node": "ejabberd@localhost",
    "uptime": 231,
    "status": "dnd",
    "resource": "tka",
    "statustext": ""
  }
]
~~~



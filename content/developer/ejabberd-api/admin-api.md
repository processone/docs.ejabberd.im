---
title: Administration API reference
bodyclass: nocomment
menu: Administration API
order: 40
---

## *add_rosteritem* - Add an item to a user's roster (supports ODBC)


Group can be several groups separated by ; for example: "g1;g2;g3"


### Arguments:
- *localuser* :: string : User name
- *localserver* :: string : Server name
- *user* :: string : Contact user name
- *server* :: string : Contact server name
- *nick* :: string : Nickname
- *group* :: string : Group
- *subs* :: string : Subscription


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/add_rosteritem
    {
      "localuser": "user1",
      "localserver": "myserver.com",
      "user": "user2",
      "server": "myserver.com",
      "nick": "User 2",
      "group": "Friends",
      "subs": "both"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *backup* - Store the database to backup file


Store the database to backup file


### Arguments:
- *file* :: string : Full path for the destination backup file


### Result:
- *res* :: string : Raw result string


### Examples:

~~~ json
    POST /api/backup
    {
      "file": "/var/lib/ejabberd/database.backup"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~




## *ban_account* - Ban an account: kick sessions and set random password


Ban an account: kick sessions and set random password


### Arguments:
- *user* :: string : User name to ban
- *host* :: string : Server name
- *reason* :: string : Reason for banning user


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *change_password* - Change the password of an account


Change the password of an account


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name
- *newpass* :: string : New password for user


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *change_room_option* - Change an option in a MUC room


Change an option in a MUC room


### Arguments:
- *name* :: string : Room name
- *service* :: string : MUC service
- *option* :: string : Option name
- *value* :: string : Value to assign


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/change_room_option
    {
      "name": "room1",
      "service": "muc.example.com",
      "option": "members_only",
      "value": "true"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *check_account* - Check if an account exists or not


Check if an account exists or not


### Arguments:
- *user* :: string : User name to check
- *host* :: string : Server to check


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/check_account
    {
      "user": "peter",
      "host": "myserver.com"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *check_password* - Check if a password is correct


Check if a password is correct


### Arguments:
- *user* :: string : User name to check
- *host* :: string : Server to check
- *password* :: string : Password to check


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *check_password_hash* - Check if the password hash is correct


Allowed hash methods: md5, sha.


### Arguments:
- *user* :: string : User name to check
- *host* :: string : Server to check
- *passwordhash* :: string : Password's hash value
- *hashmethod* :: string : Name of hash method


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *clear_cache* - Clear database cache on all nodes


Clear database cache on all nodes


### Arguments:


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/clear_cache
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *compile* - Recompile and reload Erlang source code file


Recompile and reload Erlang source code file


### Arguments:
- *file* :: string : Filename of erlang source file to compile


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/compile
    {
      "file": "/home/me/srcs/ejabberd/mod_example.erl"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *connected_users* - List all established sessions


List all established sessions


### Arguments:


### Result:
- *connected_users* :: [sessions::string] : List of users sessions


### Examples:

~~~ json
    POST /api/connected_users
    {
      
    }
    
    HTTP/1.1 200 OK
    [
      "user1@example.com",
      "user2@example.com"
    ]
~~~




## *connected_users_info* - List all established sessions and their information


List all established sessions and their information


### Arguments:


### Result:
- *connected_users_info* :: [{jid::string, connection::string, ip::string, port::integer, priority::integer, node::string, uptime::integer}]


### Examples:

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
        "port": 40092,
        "priority": 8,
        "node": "ejabberd@localhost",
        "uptime": 28
      }
    ]
~~~




## *connected_users_number* - Get the number of established sessions


Get the number of established sessions


### Arguments:


### Result:
- *num_sessions* :: integer


### Examples:

~~~ json
    POST /api/connected_users_number
    {
      
    }
    
    HTTP/1.1 200 OK
        {"num_sessions": 2}
~~~




## *connected_users_vhost* - Get the list of established sessions in a vhost


Get the list of established sessions in a vhost


### Arguments:
- *host* :: string : Server name


### Result:
- *connected_users_vhost* :: [sessions::string]


### Examples:

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




## *convert_to_scram* - Convert the passwords in 'users' ODBC table to SCRAM


Convert the passwords in 'users' ODBC table to SCRAM


### Arguments:
- *host* :: string : Vhost which users' passwords will be scrammed


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/convert_to_scram
    {
      "host": "example.com"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *convert_to_yaml* - Convert the input file from Erlang to YAML format


Convert the input file from Erlang to YAML format


### Arguments:
- *in* :: string : Full path to the original configuration file
- *out* :: string : And full path to final file


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/convert_to_yaml
    {
      "in": "/etc/ejabberd/ejabberd.cfg",
      "out": "/etc/ejabberd/ejabberd.yml"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *create_room* - Create a MUC room name@service in host


Create a MUC room name@service in host


### Arguments:
- *name* :: string : Room name
- *service* :: string : MUC service
- *host* :: string : Server host


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/create_room
    {
      "name": "room1",
      "service": "muc.example.com",
      "host": "example.com"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *create_room_with_opts* - Create a MUC room name@service in host with given options


Create a MUC room name@service in host with given options


### Arguments:
- *name* :: string : Room name
- *service* :: string : MUC service
- *host* :: string : Server host
- *options* :: [{name::string, value::string}] : List of options


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/create_room_with_opts
    {
      "name": "room1",
      "service": "muc.example.com",
      "host": "localhost",
      "options": [
        {
          "name": "members_only",
          "value": "true"
        }
      ]
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *create_rooms_file* - Create the rooms indicated in file


Provide one room JID per line. Rooms will be created after restart.


### Arguments:
- *file* :: string : Path to the text file with one room JID per line


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/create_rooms_file
    {
      "file": "/home/ejabberd/rooms.txt"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *delete_expired_messages* - Delete expired offline messages from database


Delete expired offline messages from database


### Arguments:


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/delete_expired_messages
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *delete_mnesia* - Delete elements in Mnesia database for a given vhost


Delete elements in Mnesia database for a given vhost


### Arguments:
- *host* :: string : Vhost which content will be deleted in Mnesia database


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/delete_mnesia
    {
      "host": "example.com"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *delete_old_messages* - Delete offline messages older than DAYS


Delete offline messages older than DAYS


### Arguments:
- *days* :: integer : Number of days


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/delete_old_messages
    {
      "days": 31
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *delete_old_users* - Delete users that didn't log in last days, or that never logged


To protect admin accounts, configure this for example:
access_rules:
  protect_old_users:
    - allow: admin
    - deny: all



### Arguments:
- *days* :: integer : Last login age in days of accounts that should be removed


### Result:
- *res* :: string : Raw result string


### Examples:

~~~ json
    POST /api/delete_old_users
    {
      "days": 30
    }
    
    HTTP/1.1 200 OK
    "Deleted 2 users: ["oldman@myserver.com", "test@myserver.com"]"
~~~




## *delete_old_users_vhost* - Delete users that didn't log in last days in vhost, or that never logged


To protect admin accounts, configure this for example:
access_rules:
  delete_old_users:
    - deny: admin
    - allow: all



### Arguments:
- *host* :: string : Server name
- *days* :: integer : Last login age in days of accounts that should be removed


### Result:
- *res* :: string : Raw result string


### Examples:

~~~ json
    POST /api/delete_old_users_vhost
    {
      "host": "myserver.com",
      "days": 30
    }
    
    HTTP/1.1 200 OK
    "Deleted 2 users: ["oldman@myserver.com", "test@myserver.com"]"
~~~




## *delete_rosteritem* - Delete an item from a user's roster (supports ODBC)


Delete an item from a user's roster (supports ODBC)


### Arguments:
- *localuser* :: string : User name
- *localserver* :: string : Server name
- *user* :: string : Contact user name
- *server* :: string : Contact server name


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/delete_rosteritem
    {
      "localuser": "user1",
      "localserver": "myserver.com",
      "user": "user2",
      "server": "myserver.com"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *destroy_room* - Destroy a MUC room


Destroy a MUC room


### Arguments:
- *name* :: string : Room name
- *service* :: string : MUC service


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/destroy_room
    {
      "name": "room1",
      "service": "muc.example.com"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *destroy_rooms_file* - Destroy the rooms indicated in file


Provide one room JID per line.


### Arguments:
- *file* :: string : Path to the text file with one room JID per line


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/destroy_rooms_file
    {
      "file": "/home/ejabberd/rooms.txt"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *dump* - Dump the database to a text file


Dump the database to a text file


### Arguments:
- *file* :: string : Full path for the text file


### Result:
- *res* :: string : Raw result string


### Examples:

~~~ json
    POST /api/dump
    {
      "file": "/var/lib/ejabberd/database.txt"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~




## *dump_table* - Dump a table to a text file


Dump a table to a text file


### Arguments:
- *file* :: string : Full path for the text file
- *table* :: string : Table name


### Result:
- *res* :: string : Raw result string


### Examples:

~~~ json
    POST /api/dump_table
    {
      "file": "/var/lib/ejabberd/table-muc-registered.txt",
      "table": "muc_registered"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~




## *export2sql* - Export virtual host information from Mnesia tables to SQL file


Configure the modules to use SQL, then call this command.


### Arguments:
- *host* :: string : Vhost
- *file* :: string : Full path to the destination SQL file


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/export2sql
    {
      "host": "example.com",
      "file": "/var/lib/ejabberd/example.com.sql"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *export_piefxis* - Export data of all users in the server to PIEFXIS files (XEP-0227)


Export data of all users in the server to PIEFXIS files (XEP-0227)


### Arguments:
- *dir* :: string : Full path to a directory


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/export_piefxis
    {
      "dir": "/var/lib/ejabberd/"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *export_piefxis_host* - Export data of users in a host to PIEFXIS files (XEP-0227)


Export data of users in a host to PIEFXIS files (XEP-0227)


### Arguments:
- *dir* :: string : Full path to a directory
- *host* :: string : Vhost to export


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/export_piefxis_host
    {
      "dir": "/var/lib/ejabberd/",
      "host": "example.com"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *gen_html_doc_for_commands* - Generates html documentation for ejabberd_commands


Generates html documentation for ejabberd_commands


### Arguments:
- *file* :: string : Path to file where generated documentation should be stored
- *regexp* :: string : Regexp matching names of commands or modules that will be included inside generated document
- *examples* :: string : Comma separated list of languages (choosen from java, perl, xmlrpc, json)that will have example invocation include in markdown document


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *gen_markdown_doc_for_commands* - Generates markdown documentation for ejabberd_commands


Generates markdown documentation for ejabberd_commands


### Arguments:
- *file* :: string : Path to file where generated documentation should be stored
- *regexp* :: string : Regexp matching names of commands or modules that will be included inside generated document
- *examples* :: string : Comma separated list of languages (choosen from java, perl, xmlrpc, json)that will have example invocation include in markdown document


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *get_cookie* - Get the Erlang cookie of this node


Get the Erlang cookie of this node


### Arguments:


### Result:
- *cookie* :: string : Erlang cookie used for authentication by ejabberd


### Examples:

~~~ json
    POST /api/get_cookie
    {
      
    }
    
    HTTP/1.1 200 OK
        {"cookie": "MWTAVMODFELNLSMYXPPD"}
~~~




## *get_last* - Get last activity information


Timestamp is UTC and XEP-0082 format, for example: 2017-02-23T22:25:28.063062Z     ONLINE


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name


### Result:
- *last_activity* :: {timestamp::string, status::string} : Last activity timestamp and status


### Examples:

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




## *get_loglevel* - Get the current loglevel


Get the current loglevel


### Arguments:


### Result:
- *leveltuple* :: {levelnumber::integer, levelatom::string, leveldesc::string} : Tuple with the log level number, its keyword and description


### Examples:

~~~ json
    POST /api/get_loglevel
    {
      
    }
    
    HTTP/1.1 200 OK
    {
      "levelnumber": 4,
      "levelatom": "info",
      "leveldesc": "Info"
    }
~~~




## *get_offline_count* - Get the number of unread offline messages


Get the number of unread offline messages


### Arguments:


### Result:
- *value* :: integer : Number


### Examples:

~~~ json
    POST /api/get_offline_count
    {
      
    }
    
    HTTP/1.1 200 OK
        {"value": 5}
~~~




## *get_presence* - Retrieve the resource with highest priority, and its presence (show and status message) for a given user.


The 'jid' value contains the user jid with resource.
The 'show' value contains the user presence flag. It can take limited values:
 - available
 - chat (Free for chat)
 - away
 - dnd (Do not disturb)
 - xa (Not available, extended away)
 - unavailable (Not connected)

'status' is a free text defined by the user client.


### Arguments:
- *user* :: string : User name
- *server* :: string : Server name


### Result:
- *presence* :: {jid::string, show::string, status::string}


### Examples:

~~~ json
    POST /api/get_presence
    {
      "user": "peter",
      "server": "myexample.com"
    }
    
    HTTP/1.1 200 OK
    {
      "jid": "user1@myserver.com/tka",
      "show": "dnd",
      "status": "Busy"
    }
~~~




## *get_room_affiliations* - Get the list of affiliations of a MUC room


Get the list of affiliations of a MUC room


### Arguments:
- *name* :: string : Room name
- *service* :: string : MUC service


### Result:
- *affiliations* :: [{username::string, domain::string, affiliation::string, reason::string}] : The list of affiliations with username, domain, affiliation and reason


### Examples:

~~~ json
    POST /api/get_room_affiliations
    {
      "name": "room1",
      "service": "muc.example.com"
    }
    
    HTTP/1.1 200 OK
    [
      {
        "username": "user1",
        "domain": "example.com",
        "affiliation": "member",
        "reason": "member"
      }
    ]
~~~




## *get_room_occupants* - Get the list of occupants of a MUC room


Get the list of occupants of a MUC room


### Arguments:
- *name* :: string : Room name
- *service* :: string : MUC service


### Result:
- *occupants* :: [{jid::string, nick::string, role::string}] : The list of occupants with JID, nick and affiliation


### Examples:

~~~ json
    POST /api/get_room_occupants
    {
      "name": "room1",
      "service": "muc.example.com"
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




## *get_room_occupants_number* - Get the number of occupants of a MUC room


Get the number of occupants of a MUC room


### Arguments:
- *name* :: string : Room name
- *service* :: string : MUC service


### Result:
- *occupants* :: integer : Number of room occupants


### Examples:

~~~ json
    POST /api/get_room_occupants_number
    {
      "name": "room1",
      "service": "muc.example.com"
    }
    
    HTTP/1.1 200 OK
        {"occupants": 7}
~~~




## *get_room_options* - Get options from a MUC room


Get options from a MUC room


### Arguments:
- *name* :: string : Room name
- *service* :: string : MUC service


### Result:
- *options* :: [{name::string, value::string}] : List of room options tuples with name and value


### Examples:

~~~ json
    POST /api/get_room_options
    {
      "name": "room1",
      "service": "muc.example.com"
    }
    
    HTTP/1.1 200 OK
    [
      {
        "name": "members_only",
        "value": "true"
      }
    ]
~~~




## *get_roster* - Get roster of a local user


Get roster of a local user


### Arguments:


### Result:
- *contacts* :: [{jid::string, nick::string, subscription::string, ask::string, group::string}]


### Examples:

~~~ json
    POST /api/get_roster
    {
      
    }
    
    HTTP/1.1 200 OK
    [
      {
        "jid": "aaaaa",
        "nick": "bbbbb",
        "subscription": "ccccc",
        "ask": "ddddd",
        "group": "eeeee"
      },
      {
        "jid": "fffff",
        "nick": "ggggg",
        "subscription": "hhhhh",
        "ask": "iiiii",
        "group": "jjjjj"
      }
    ]
~~~




## *get_subscribers* - List subscribers of a MUC conference


List subscribers of a MUC conference


### Arguments:
- *name* :: string : Room name
- *service* :: string : MUC service


### Result:
- *subscribers* :: [jid::string] : The list of users that are subscribed to that room


### Examples:

~~~ json
    POST /api/get_subscribers
    {
      "name": "room1",
      "service": "muc.example.com"
    }
    
    HTTP/1.1 200 OK
    [
      "user2@example.com",
      "user3@example.com"
    ]
~~~




## *get_user_rooms* - Get the list of rooms where this user is occupant


Get the list of rooms where this user is occupant


### Arguments:
- *user* :: string : Username
- *host* :: string : Server host


### Result:
- *rooms* :: [room::string]


### Examples:

~~~ json
    POST /api/get_user_rooms
    {
      "user": "tom",
      "host": "example.com"
    }
    
    HTTP/1.1 200 OK
    [
      "room1@muc.example.com",
      "room2@muc.example.com"
    ]
~~~




## *get_vcard* - Get content from a vCard field


Some vcard field names in get/set_vcard are:
 FN		- Full Name
 NICKNAME	- Nickname
 BDAY		- Birthday
 TITLE		- Work: Position
 ROLE		- Work: Role
Some vcard field names and subnames in get/set_vcard2 are:
 N FAMILY	- Family name
 N GIVEN	- Given name
 N MIDDLE	- Middle name
 ADR CTRY	- Address: Country
 ADR LOCALITY	- Address: City
 TEL HOME      - Telephone: Home
 TEL CELL      - Telephone: Cellphone
 TEL WORK      - Telephone: Work
 TEL VOICE     - Telephone: Voice
 EMAIL USERID	- E-Mail Address
 ORG ORGNAME	- Work: Company
 ORG ORGUNIT	- Work: Department

For a full list of vCard fields check XEP-0054: vcard-temp at http://www.xmpp.org/extensions/xep-0054.html


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name
- *name* :: string : Field name


### Result:
- *content* :: string : Field content


### Examples:

~~~ json
    POST /api/get_vcard
    {
      "user": "user1",
      "host": "myserver.com",
      "name": "NICKNAME"
    }
    
    HTTP/1.1 200 OK
        {"content": "User 1"}
~~~




## *get_vcard2* - Get content from a vCard subfield


Some vcard field names and subnames in get/set_vcard2 are:
 N FAMILY	- Family name
 N GIVEN	- Given name
 N MIDDLE	- Middle name
 ADR CTRY	- Address: Country
 ADR LOCALITY	- Address: City
 TEL HOME      - Telephone: Home
 TEL CELL      - Telephone: Cellphone
 TEL WORK      - Telephone: Work
 TEL VOICE     - Telephone: Voice
 EMAIL USERID	- E-Mail Address
 ORG ORGNAME	- Work: Company
 ORG ORGUNIT	- Work: Department

Some vcard field names in get/set_vcard are:
 FN		- Full Name
 NICKNAME	- Nickname
 BDAY		- Birthday
 TITLE		- Work: Position
 ROLE		- Work: Role
For a full list of vCard fields check XEP-0054: vcard-temp at http://www.xmpp.org/extensions/xep-0054.html


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name
- *name* :: string : Field name
- *subname* :: string : Subfield name


### Result:
- *content* :: string : Field content


### Examples:

~~~ json
    POST /api/get_vcard2
    {
      "user": "user1",
      "host": "myserver.com",
      "name": "N",
      "subname": "FAMILY"
    }
    
    HTTP/1.1 200 OK
        {"content": "Schubert"}
~~~




## *get_vcard2_multi* - Get multiple contents from a vCard field


Some vcard field names and subnames in get/set_vcard2 are:
 N FAMILY	- Family name
 N GIVEN	- Given name
 N MIDDLE	- Middle name
 ADR CTRY	- Address: Country
 ADR LOCALITY	- Address: City
 TEL HOME      - Telephone: Home
 TEL CELL      - Telephone: Cellphone
 TEL WORK      - Telephone: Work
 TEL VOICE     - Telephone: Voice
 EMAIL USERID	- E-Mail Address
 ORG ORGNAME	- Work: Company
 ORG ORGUNIT	- Work: Department

Some vcard field names in get/set_vcard are:
 FN		- Full Name
 NICKNAME	- Nickname
 BDAY		- Birthday
 TITLE		- Work: Position
 ROLE		- Work: Role
For a full list of vCard fields check XEP-0054: vcard-temp at http://www.xmpp.org/extensions/xep-0054.html


### Arguments:
- *user* :: string
- *host* :: string
- *name* :: string
- *subname* :: string


### Result:
- *contents* :: [value::string]


### Examples:

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




## *import_dir* - Import users data from jabberd14 spool dir


Import users data from jabberd14 spool dir


### Arguments:
- *file* :: string : Full path to the jabberd14 spool directory


### Result:
- *res* :: string : Raw result string


### Examples:

~~~ json
    POST /api/import_dir
    {
      "file": "/var/lib/ejabberd/jabberd14/"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~




## *import_file* - Import user data from jabberd14 spool file


Import user data from jabberd14 spool file


### Arguments:
- *file* :: string : Full path to the jabberd14 spool file


### Result:
- *res* :: string : Raw result string


### Examples:

~~~ json
    POST /api/import_file
    {
      "file": "/var/lib/ejabberd/jabberd14.spool"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~




## *import_piefxis* - Import users data from a PIEFXIS file (XEP-0227)


Import users data from a PIEFXIS file (XEP-0227)


### Arguments:
- *file* :: string : Full path to the PIEFXIS file


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/import_piefxis
    {
      "file": "/var/lib/ejabberd/example.com.xml"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *import_prosody* - Import data from Prosody


Import data from Prosody


### Arguments:
- *dir* :: string : Full path to the Prosody data directory


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/import_prosody
    {
      "dir": "/var/lib/prosody/datadump/"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *incoming_s2s_number* - Number of incoming s2s connections on the node


Number of incoming s2s connections on the node


### Arguments:


### Result:
- *s2s_incoming* :: integer


### Examples:

~~~ json
    POST /api/incoming_s2s_number
    {
      
    }
    
    HTTP/1.1 200 OK
        {"s2s_incoming": 1}
~~~




## *install_fallback* - Install the database from a fallback file


Install the database from a fallback file


### Arguments:
- *file* :: string : Full path to the fallback file


### Result:
- *res* :: string : Raw result string


### Examples:

~~~ json
    POST /api/install_fallback
    {
      "file": "/var/lib/ejabberd/database.fallback"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~




## *join_cluster* - Join this node into the cluster handled by Node


Join this node into the cluster handled by Node


### Arguments:
- *node* :: string : Nodename of the node to join


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/join_cluster
    {
      "node": "ejabberd1@machine7"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *kick_session* - Kick a user session


Kick a user session


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name
- *resource* :: string : User's resource
- *reason* :: string : Reason for closing session


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *kick_user* - Disconnect user's active sessions


Disconnect user's active sessions


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name


### Result:
- *num_resources* :: integer : Number of resources that were kicked


### Examples:

~~~ json
    POST /api/kick_user
    {
      "user": "user1",
      "host": "example.com"
    }
    
    HTTP/1.1 200 OK
        {"num_resources": 3}
~~~




## *leave_cluster* - Remove and shutdown Node from the running cluster


This command can be run from any running node of the cluster, even the node to be removed.


### Arguments:
- *node* :: string : Nodename of the node to kick from the cluster


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/leave_cluster
    {
      "node": "ejabberd1@machine8"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *list_cluster* - List nodes that are part of the cluster handled by Node


List nodes that are part of the cluster handled by Node


### Arguments:


### Result:
- *nodes* :: [node::string]


### Examples:

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




## *load* - Restore the database from a text file


Restore the database from a text file


### Arguments:
- *file* :: string : Full path to the text file


### Result:
- *res* :: string : Raw result string


### Examples:

~~~ json
    POST /api/load
    {
      "file": "/var/lib/ejabberd/database.txt"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~




## *mnesia_change_nodename* - Change the erlang node name in a backup file


Change the erlang node name in a backup file


### Arguments:
- *oldnodename* :: string : Name of the old erlang node
- *newnodename* :: string : Name of the new node
- *oldbackup* :: string : Path to old backup file
- *newbackup* :: string : Path to the new backup file


### Result:
- *res* :: string : Raw result string


### Examples:

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




## *module_check* - Check the contributed module repository compliance


Check the contributed module repository compliance


### Arguments:
- *module* :: string : Module name


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/module_check
    {
      "module": "mod_rest"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *module_install* - Compile, install and start an available contributed module


Compile, install and start an available contributed module


### Arguments:
- *module* :: string : Module name


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/module_install
    {
      "module": "mod_rest"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *module_uninstall* - Uninstall a contributed module


Uninstall a contributed module


### Arguments:
- *module* :: string : Module name


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/module_uninstall
    {
      "module": "mod_rest"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *module_upgrade* - Upgrade the running code of an installed module


In practice, this uninstalls and installs the module


### Arguments:
- *module* :: string : Module name


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/module_upgrade
    {
      "module": "mod_rest"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *modules_available* - List the contributed modules available to install


List the contributed modules available to install


### Arguments:


### Result:
- *modules* :: [{name::string, summary::string}] : List of tuples with module name and description


### Examples:

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




## *modules_installed* - List the contributed modules already installed


List the contributed modules already installed


### Arguments:


### Result:
- *modules* :: [{name::string, summary::string}] : List of tuples with module name and description


### Examples:

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




## *modules_update_specs* - Update the module source code from Git


A connection to Internet is required


### Arguments:


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/modules_update_specs
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *muc_online_rooms* - List existing rooms ('global' to get all vhosts)


List existing rooms ('global' to get all vhosts)


### Arguments:
- *host* :: string : Server domain where the MUC service is, or 'global' for all


### Result:
- *rooms* :: [room::string] : List of rooms


### Examples:

~~~ json
    POST /api/muc_online_rooms
    {
      "host": "example.com"
    }
    
    HTTP/1.1 200 OK
    [
      "room1@muc.example.com",
      "room2@muc.example.com"
    ]
~~~




## *muc_online_rooms_by_regex* - List existing rooms ('global' to get all vhosts) by regex


List existing rooms ('global' to get all vhosts) by regex


### Arguments:
- *host* :: string : Server domain where the MUC service is, or 'global' for all
- *regex* :: string : Regex pattern for room name


### Result:
- *rooms* :: [{jid::string, public::string, participants::integer}]


### Examples:

~~~ json
    POST /api/muc_online_rooms
    {
      "host": "example.com",
      "regex": "^room"
    }

    HTTP/1.1 200 OK
    [
      {"jid": "room1@muc.example.com", "public": "true", "participants": 10},
      {"jid": "room2@muc.example.com", "public": "false", "participants": 10}
    ]
~~~




## *muc_register_nick* - Register a nick in the MUC service


Register a nick in the MUC service


### Arguments:
- *nick* :: string : Nick
- *jid* :: string : User JID
- *domain* :: string : MUC service


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/muc_register_nick
    {
      "nick": "Tim",
      "jid": "tim@example.org",
      "domain": "muc.example.org"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *muc_unregister_nick* - Unregister the nick in the MUC service


Unregister the nick in the MUC service


### Arguments:
- *nick* :: string


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/muc_unregister_nick
    {
      "nick": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *num_active_users* - Get number of users active in the last days


Get number of users active in the last days


### Arguments:
- *host* :: string : Name of host to check
- *days* :: integer : Number of days to calculate sum


### Result:
- *users* :: integer : Number of users active on given server in last n days


### Examples:

~~~ json
    POST /api/num_active_users
    {
      "host": "myserver.com",
      "days": 3
    }
    
    HTTP/1.1 200 OK
        {"users": 123}
~~~




## *num_resources* - Get the number of resources of a user


Get the number of resources of a user


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name


### Result:
- *resources* :: integer : Number of active resources for a user


### Examples:

~~~ json
    POST /api/num_resources
    {
      "user": "peter",
      "host": "myserver.com"
    }
    
    HTTP/1.1 200 OK
        {"resources": 5}
~~~




## *oauth_issue_token* - Issue an oauth token for the given jid


Issue an oauth token for the given jid


### Arguments:
- *jid* :: string : Jid for which issue token
- *ttl* :: integer : Time to live of generated token in seconds
- *scopes* :: string : List of scopes to allow, separated by ';'


### Result:
- *result* :: {token::string, scopes::string, expires_in::string}


### Examples:

~~~ json
    POST /api/oauth_issue_token
    {
      "jid": "user@server.com",
      "ttl": 3600,
      "scopes": "connected_users_number;muc_online_rooms"
    }
    
    HTTP/1.1 200 OK
    {
      "token": "aaaaa",
      "scopes": "bbbbb",
      "expires_in": "ccccc"
    }
~~~




## *oauth_list_tokens* - List oauth tokens, user, scope, and seconds to expire (only Mnesia)


List oauth tokens, their user and scope, and how many seconds remain until expirity


### Arguments:


### Result:
- *tokens* :: [{token::string, user::string, scope::string, expires_in::string}]


### Examples:

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




## *oauth_revoke_token* - Revoke authorization for a token (only Mnesia)


Revoke authorization for a token (only Mnesia)


### Arguments:
- *token* :: string


### Result:
- *tokens* :: [{token::string, user::string, scope::string, expires_in::string}] : List of remaining tokens


### Examples:

~~~ json
    POST /api/oauth_revoke_token
    {
      "token": "aaaaa"
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




## *outgoing_s2s_number* - Number of outgoing s2s connections on the node


Number of outgoing s2s connections on the node


### Arguments:


### Result:
- *s2s_outgoing* :: integer


### Examples:

~~~ json
    POST /api/outgoing_s2s_number
    {
      
    }
    
    HTTP/1.1 200 OK
        {"s2s_outgoing": 1}
~~~




## *privacy_set* - Send a IQ set privacy stanza for a local account


Send a IQ set privacy stanza for a local account


### Arguments:
- *user* :: string : Username
- *host* :: string : Server name
- *xmlquery* :: string : Query XML element


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *private_get* - Get some information from a user private storage


Get some information from a user private storage


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name
- *element* :: string : Element name
- *ns* :: string : Namespace


### Result:
- *res* :: string


### Examples:

~~~ json
    POST /api/private_get
    {
      "user": "user1",
      "host": "myserver.com",
      "element": "storage",
      "ns": "storage:rosternotes"
    }
    
    HTTP/1.1 200 OK
        {"res": "aaaaa"}
~~~




## *private_set* - Set to the user private storage


Set to the user private storage


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name
- *element* :: string : XML storage element


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/private_set
    {
      "user": "user1",
      "host": "myserver.com",
      "element": "<query xmlns='jabber:iq:private'> <storage xmlns='storage:rosternotes'/></query>"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *process_rosteritems* - List/delete rosteritems that match filter (only Mnesia)


Explanation of each argument:
 - action: what to do with each rosteritem that matches all the filtering options
 - subs: subscription type
 - asks: pending subscription
 - users: the JIDs of the local user
 - contacts: the JIDs of the contact in the roster

Allowed values in the arguments:
  ACTION = list | delete
  SUBS = SUB[:SUB]* | any
  SUB = none | from | to | both
  ASKS = ASK[:ASK]* | any
  ASK = none | out | in
  USERS = JID[:JID]* | any
  CONTACTS = JID[:JID]* | any
  JID = characters valid in a JID, and can use the globs: *, ?, ! and [...]

This example will list roster items with subscription 'none', 'from' or 'to' that have any ask property, of local users which JID is in the virtual host 'example.org' and that the contact JID is either a bare server name (without user part) or that has a user part and the server part contains the word 'icq':
  list none:from:to any *@example.org *:*@*icq*


### Arguments:
- *action* :: string
- *subs* :: string
- *asks* :: string
- *users* :: string
- *contacts* :: string


### Result:
- *response* :: [{user::string, contact::string}]


### Examples:

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




## *push_alltoall* - Add all the users to all the users of Host in Group


Add all the users to all the users of Host in Group


### Arguments:
- *host* :: string : Server name
- *group* :: string : Group name


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/push_alltoall
    {
      "host": "myserver.com",
      "group": "Everybody"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *push_roster* - Push template roster from file to a user


The text file must contain an erlang term: a list of tuples with username, servername, group and nick. Example:
[{"user1", "localhost", "Workers", "User 1"},
 {"user2", "localhost", "Workers", "User 2"}].


### Arguments:
- *file* :: string : File path
- *user* :: string : User name
- *host* :: string : Server name


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *push_roster_all* - Push template roster from file to all those users


The text file must contain an erlang term: a list of tuples with username, servername, group and nick. Example:
[{"user1", "localhost", "Workers", "User 1"},
 {"user2", "localhost", "Workers", "User 2"}].


### Arguments:
- *file* :: string : File path


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/push_roster_all
    {
      "file": "/home/ejabberd/roster.txt"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *register* - Register a user


Register a user


### Arguments:
- *user* :: string : Username
- *host* :: string : Local vhost served by ejabberd
- *password* :: string : Password


### Result:
- *res* :: string : Raw result string


### Examples:

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




## *registered_users* - List all registered users in HOST


List all registered users in HOST


### Arguments:
- *host* :: string : Local vhost


### Result:
- *users* :: [username::string] : List of registered accounts usernames


### Examples:

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




## *registered_vhosts* - List all registered vhosts in SERVER


List all registered vhosts in SERVER


### Arguments:


### Result:
- *vhosts* :: [vhost::string] : List of available vhosts


### Examples:

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




## *reload_config* - Reload config file in memory


Reload config file in memory


### Arguments:


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/reload_config
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *reopen_log* - Reopen the log files


Reopen the log files


### Arguments:


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/reopen_log
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *resource_num* - Resource string of a session number


Resource string of a session number


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name
- *num* :: integer : ID of resource to return


### Result:
- *resource* :: string : Name of user resource


### Examples:

~~~ json
    POST /api/resource_num
    {
      "user": "peter",
      "host": "myserver.com",
      "num": 2
    }
    
    HTTP/1.1 200 OK
        {"resource": "Psi"}
~~~




## *restart* - Restart ejabberd gracefully


Restart ejabberd gracefully


### Arguments:


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/restart
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *restart_module* - Stop an ejabberd module, reload code and start


Stop an ejabberd module, reload code and start


### Arguments:
- *host* :: string : Server name
- *module* :: string : Module to restart


### Result:
- *res* :: integer : Returns integer code:
 - 0: code reloaded, module restarted
 - 1: error: module not loaded
 - 2: code not reloaded, but module restarted


### Examples:

~~~ json
    POST /api/restart_module
    {
      "host": "myserver.com",
      "module": "mod_admin_extra"
    }
    
    HTTP/1.1 200 OK
        {"res": 0}
~~~




## *restore* - Restore the database from backup file


Restore the database from backup file


### Arguments:
- *file* :: string : Full path to the backup file


### Result:
- *res* :: string : Raw result string


### Examples:

~~~ json
    POST /api/restore
    {
      "file": "/var/lib/ejabberd/database.backup"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~




## *rooms_unused_destroy* - Destroy the rooms that are unused for many days in host


Destroy the rooms that are unused for many days in host


### Arguments:
- *host* :: string : Server host
- *days* :: integer : Number of days


### Result:
- *rooms* :: [room::string] : List of unused rooms that has been destroyed


### Examples:

~~~ json
    POST /api/rooms_unused_destroy
    {
      "host": "example.com",
      "days": 31
    }
    
    HTTP/1.1 200 OK
    [
      "room1@muc.example.com",
      "room2@muc.example.com"
    ]
~~~




## *rooms_unused_list* - List the rooms that are unused for many days in host


List the rooms that are unused for many days in host


### Arguments:
- *host* :: string : Server host
- *days* :: integer : Number of days


### Result:
- *rooms* :: [room::string] : List of unused rooms


### Examples:

~~~ json
    POST /api/rooms_unused_list
    {
      "host": "example.com",
      "days": 31
    }
    
    HTTP/1.1 200 OK
    [
      "room1@muc.example.com",
      "room2@muc.example.com"
    ]
~~~




## *rotate_log* - Rotate the log files


Rotate the log files


### Arguments:


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/rotate_log
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *send_direct_invitation* - Send a direct invitation to several destinations


Password and Message can also be: none. Users JIDs are separated with : 


### Arguments:
- *name* :: string : Room name
- *service* :: string : MUC service
- *password* :: string : Password, or none
- *reason* :: string : Reason text, or none
- *users* :: string : Users JIDs separated with : characters


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/send_direct_invitation
    {
      "name": "room1",
      "service": "muc.example.com",
      "password": "",
      "reason": "Check this out!",
      "users": "user2@localhost:user3@example.com"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *send_message* - Send a message to a local or remote bare of full JID


Send a message to a local or remote bare of full JID


### Arguments:
- *type* :: string : Message type: normal, chat, headline
- *from* :: string : Sender JID
- *to* :: string : Receiver JID
- *subject* :: string : Subject, or empty string
- *body* :: string : Body


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *send_stanza* - Send a stanza; provide From JID and valid To JID


Send a stanza; provide From JID and valid To JID


### Arguments:
- *from* :: string : Sender JID
- *to* :: string : Destination JID
- *stanza* :: string : Stanza


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *send_stanza_c2s* - Send a stanza as if sent from a c2s session


Send a stanza as if sent from a c2s session


### Arguments:
- *user* :: string : Username
- *host* :: string : Server name
- *resource* :: string : Resource
- *stanza* :: string : Stanza


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *set_last* - Set last activity information


Timestamp is the seconds since1970-01-01 00:00:00 UTC, for example: date +%s


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name
- *timestamp* :: integer : Number of seconds since epoch
- *status* :: string : Status message


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *set_loglevel* - Set the loglevel (0 to 5)


Set the loglevel (0 to 5)


### Arguments:
- *loglevel* :: integer : Integer of the desired logging level, between 1 and 5


### Result:
- *logger* :: string : The type of logger module used


### Examples:

~~~ json
    POST /api/set_loglevel
    {
      "loglevel": 5
    }
    
    HTTP/1.1 200 OK
        {"logger": "lager"}
~~~




## *set_master* - Set master node of the clustered Mnesia tables


If you provide as nodename "self", this node will be set as its own master.


### Arguments:
- *nodename* :: string : Name of the erlang node that will be considered master of this node


### Result:
- *res* :: string : Raw result string


### Examples:

~~~ json
    POST /api/set_master
    {
      "nodename": "ejabberd@machine7"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~




## *set_nickname* - Set nickname in a user's vCard


Set nickname in a user's vCard


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name
- *nickname* :: string : Nickname


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *set_presence* - Set presence of a session


Set presence of a session


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name
- *resource* :: string : Resource
- *type* :: string : Type: available, error, probe...
- *show* :: string : Show: away, chat, dnd, xa.
- *status* :: string : Status text
- *priority* :: string : Priority, provide this value as an integer


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/set_presence
    {
      "user": "user1",
      "host": "myserver.com",
      "resource": "tka1",
      "type": "available",
      "show": "away",
      "status": "BB",
      "priority": "7"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *set_room_affiliation* - Change an affiliation in a MUC room


Change an affiliation in a MUC room


### Arguments:
- *name* :: string : Room name
- *service* :: string : MUC service
- *jid* :: string : User JID
- *affiliation* :: string : Affiliation to set


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/set_room_affiliation
    {
      "name": "room1",
      "service": "muc.example.com",
      "jid": "user2@example.com",
      "affiliation": "member"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *set_vcard* - Set content in a vCard field


Some vcard field names in get/set_vcard are:
 FN		- Full Name
 NICKNAME	- Nickname
 BDAY		- Birthday
 TITLE		- Work: Position
 ROLE		- Work: Role
Some vcard field names and subnames in get/set_vcard2 are:
 N FAMILY	- Family name
 N GIVEN	- Given name
 N MIDDLE	- Middle name
 ADR CTRY	- Address: Country
 ADR LOCALITY	- Address: City
 TEL HOME      - Telephone: Home
 TEL CELL      - Telephone: Cellphone
 TEL WORK      - Telephone: Work
 TEL VOICE     - Telephone: Voice
 EMAIL USERID	- E-Mail Address
 ORG ORGNAME	- Work: Company
 ORG ORGUNIT	- Work: Department

For a full list of vCard fields check XEP-0054: vcard-temp at http://www.xmpp.org/extensions/xep-0054.html


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name
- *name* :: string : Field name
- *content* :: string : Value


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *set_vcard2* - Set content in a vCard subfield


Some vcard field names and subnames in get/set_vcard2 are:
 N FAMILY	- Family name
 N GIVEN	- Given name
 N MIDDLE	- Middle name
 ADR CTRY	- Address: Country
 ADR LOCALITY	- Address: City
 TEL HOME      - Telephone: Home
 TEL CELL      - Telephone: Cellphone
 TEL WORK      - Telephone: Work
 TEL VOICE     - Telephone: Voice
 EMAIL USERID	- E-Mail Address
 ORG ORGNAME	- Work: Company
 ORG ORGUNIT	- Work: Department

Some vcard field names in get/set_vcard are:
 FN		- Full Name
 NICKNAME	- Nickname
 BDAY		- Birthday
 TITLE		- Work: Position
 ROLE		- Work: Role
For a full list of vCard fields check XEP-0054: vcard-temp at http://www.xmpp.org/extensions/xep-0054.html


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name
- *name* :: string : Field name
- *subname* :: string : Subfield name
- *content* :: string : Value


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *set_vcard2_multi* - Set multiple contents in a vCard subfield


Some vcard field names and subnames in get/set_vcard2 are:
 N FAMILY	- Family name
 N GIVEN	- Given name
 N MIDDLE	- Middle name
 ADR CTRY	- Address: Country
 ADR LOCALITY	- Address: City
 TEL HOME      - Telephone: Home
 TEL CELL      - Telephone: Cellphone
 TEL WORK      - Telephone: Work
 TEL VOICE     - Telephone: Voice
 EMAIL USERID	- E-Mail Address
 ORG ORGNAME	- Work: Company
 ORG ORGUNIT	- Work: Department

Some vcard field names in get/set_vcard are:
 FN		- Full Name
 NICKNAME	- Nickname
 BDAY		- Birthday
 TITLE		- Work: Position
 ROLE		- Work: Role
For a full list of vCard fields check XEP-0054: vcard-temp at http://www.xmpp.org/extensions/xep-0054.html


### Arguments:
- *user* :: string
- *host* :: string
- *name* :: string
- *subname* :: string
- *contents* :: [value::string]


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *srg_create* - Create a Shared Roster Group


If you want to specify several group identifiers in the Display argument,
put  \ " around the argument and
separate the identifiers with \ \ n
For example:
  ejabberdctl srg_create group3 myserver.com name desc \"group1\\ngroup2\"


### Arguments:
- *group* :: string : Group identifier
- *host* :: string : Group server name
- *name* :: string : Group name
- *description* :: string : Group description
- *display* :: string : Groups to display


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/srg_create
    {
      "group": "group3",
      "host": "myserver.com",
      "name": "Group3",
      "description": "Third group",
      "display": "group1\\ngroup2"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *srg_delete* - Delete a Shared Roster Group


Delete a Shared Roster Group


### Arguments:
- *group* :: string : Group identifier
- *host* :: string : Group server name


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/srg_delete
    {
      "group": "group3",
      "host": "myserver.com"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *srg_get_info* - Get info of a Shared Roster Group


Get info of a Shared Roster Group


### Arguments:
- *group* :: string : Group identifier
- *host* :: string : Group server name


### Result:
- *informations* :: [{key::string, value::string}] : List of group informations, as key and value


### Examples:

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




## *srg_get_members* - Get members of a Shared Roster Group


Get members of a Shared Roster Group


### Arguments:
- *group* :: string : Group identifier
- *host* :: string : Group server name


### Result:
- *members* :: [member::string] : List of group identifiers


### Examples:

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




## *srg_list* - List the Shared Roster Groups in Host


List the Shared Roster Groups in Host


### Arguments:
- *host* :: string : Server name


### Result:
- *groups* :: [id::string] : List of group identifiers


### Examples:

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




## *srg_user_add* - Add the JID user@host to the Shared Roster Group


Add the JID user@host to the Shared Roster Group


### Arguments:
- *user* :: string : Username
- *host* :: string : User server name
- *group* :: string : Group identifier
- *grouphost* :: string : Group server name


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *srg_user_del* - Delete this JID user@host from the Shared Roster Group


Delete this JID user@host from the Shared Roster Group


### Arguments:
- *user* :: string : Username
- *host* :: string : User server name
- *group* :: string : Group identifier
- *grouphost* :: string : Group server name


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

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




## *stats* - Get statistical value: registeredusers onlineusers onlineusersnode uptimeseconds processes


Get statistical value: registeredusers onlineusers onlineusersnode uptimeseconds processes


### Arguments:
- *name* :: string : Statistic name


### Result:
- *stat* :: integer : Integer statistic value


### Examples:

~~~ json
    POST /api/stats
    {
      "name": "registeredusers"
    }
    
    HTTP/1.1 200 OK
        {"stat": 6}
~~~




## *stats_host* - Get statistical value for this host: registeredusers onlineusers


Get statistical value for this host: registeredusers onlineusers


### Arguments:
- *name* :: string : Statistic name
- *host* :: string : Server JID


### Result:
- *stat* :: integer : Integer statistic value


### Examples:

~~~ json
    POST /api/stats_host
    {
      "name": "registeredusers",
      "host": "example.com"
    }
    
    HTTP/1.1 200 OK
        {"stat": 6}
~~~




## *status* - Get status of the ejabberd server


Get status of the ejabberd server


### Arguments:


### Result:
- *res* :: string : Raw result string


### Examples:

~~~ json
    POST /api/status
    {
      
    }
    
    HTTP/1.1 200 OK
    "The node ejabberd@localhost is started with status: startedejabberd X.X is running in that node"
~~~




## *status_list* - List of logged users with this status


List of logged users with this status


### Arguments:
- *status* :: string : Status type to check


### Result:
- *users* :: [{user::string, host::string, resource::string, priority::integer, status::string}]


### Examples:

~~~ json
    POST /api/status_list
    {
      "status": "dnd"
    }
    
    HTTP/1.1 200 OK
    [
      {
        "user": "peter",
        "host": "myserver.com",
        "resource": "tka",
        "priority": 6,
        "status": "Busy"
      }
    ]
~~~




## *status_list_host* - List of users logged in host with their statuses


List of users logged in host with their statuses


### Arguments:
- *host* :: string : Server name
- *status* :: string : Status type to check


### Result:
- *users* :: [{user::string, host::string, resource::string, priority::integer, status::string}]


### Examples:

~~~ json
    POST /api/status_list_host
    {
      "host": "myserver.com",
      "status": "dnd"
    }
    
    HTTP/1.1 200 OK
    [
      {
        "user": "peter",
        "host": "myserver.com",
        "resource": "tka",
        "priority": 6,
        "status": "Busy"
      }
    ]
~~~




## *status_num* - Number of logged users with this status


Number of logged users with this status


### Arguments:
- *status* :: string : Status type to check


### Result:
- *users* :: integer : Number of connected sessions with given status type


### Examples:

~~~ json
    POST /api/status_num
    {
      "status": "dnd"
    }
    
    HTTP/1.1 200 OK
        {"users": 23}
~~~




## *status_num_host* - Number of logged users with this status in host


Number of logged users with this status in host


### Arguments:
- *host* :: string : Server name
- *status* :: string : Status type to check


### Result:
- *users* :: integer : Number of connected sessions with given status type


### Examples:

~~~ json
    POST /api/status_num_host
    {
      "host": "myserver.com",
      "status": "dnd"
    }
    
    HTTP/1.1 200 OK
        {"users": 23}
~~~




## *stop* - Stop ejabberd gracefully


Stop ejabberd gracefully


### Arguments:


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/stop
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *stop_s2s_connections* - Stop all outgoing and incoming connections


Stop all s2s outgoing and incoming connections


### Arguments:


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/stop_s2s_connections
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *stop_kindly* - Inform users and rooms, wait, and stop the server


Provide the delay in seconds, and the announcement quoted, for example: 
ejabberdctl stop_kindly 60 \"The server will stop in one minute.\"


### Arguments:
- *delay* :: integer : Seconds to wait
- *announcement* :: string : Announcement to send, with quotes


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/stop_kindly
    {
      "delay": 60,
      "announcement": "Server will stop now."
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *subscribe_room* - Subscribe to a MUC conference


Subscribe to a MUC conference


### Arguments:
- *user* :: string : Full JID, including some resource
- *nick* :: string : a user's nick
- *room* :: string : the room to subscribe
- *nodes* :: string : nodes separated by commas: ,


### Result:
- *nodes* :: [node::string] : The list of nodes that has subscribed


### Examples:

~~~ json
    POST /api/subscribe_room
    {
      "user": "tom@localhost/dummy",
      "nick": "Tom",
      "room": "room1@conference.localhost",
      "nodes": "urn:xmpp:mucsub:nodes:messages,urn:xmpp:mucsub:nodes:affiliations"
    }
    
    HTTP/1.1 200 OK
    [
      "urn:xmpp:mucsub:nodes:messages",
      "urn:xmpp:mucsub:nodes:affiliations"
    ]
~~~




## *unregister* - Unregister a user


Unregister a user


### Arguments:
- *user* :: string : Username
- *host* :: string : Local vhost served by ejabberd


### Result:
- *res* :: string : Raw result string


### Examples:

~~~ json
    POST /api/unregister
    {
      "user": "bob",
      "host": "example.com"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~




## *unsubscribe_room* - Unsubscribe from a MUC conference


Unsubscribe from a MUC conference


### Arguments:
- *user* :: string : User JID
- *room* :: string : the room to subscribe


### Result:
- *res* :: integer : Status code (0 on success, 1 otherwise)


### Examples:

~~~ json
    POST /api/unsubscribe_room
    {
      "user": "tom@localhost",
      "room": "room1@conference.localhost"
    }
    
    HTTP/1.1 200 OK
    ""
~~~




## *update* - Update the given module, or use the keyword: all


Update the given module, or use the keyword: all


### Arguments:
- *module* :: string


### Result:
- *res* :: string : Raw result string


### Examples:

~~~ json
    POST /api/update
    {
      "module": "mod_vcard"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~




## *update_list* - List modified modules that can be updated


List modified modules that can be updated


### Arguments:


### Result:
- *modules* :: [module::string]


### Examples:

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




## *user_resources* - List user's connected resources


List user's connected resources


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name


### Result:
- *resources* :: [resource::string]


### Examples:

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




## *user_sessions_info* - Get information about all sessions of a user


Get information about all sessions of a user


### Arguments:
- *user* :: string : User name
- *host* :: string : Server name


### Result:
- *sessions_info* :: [{connection::string, ip::string, port::integer, priority::integer, node::string, uptime::integer, status::string, resource::string, statustext::string}]


### Examples:

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



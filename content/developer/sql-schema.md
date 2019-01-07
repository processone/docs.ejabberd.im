---
title: ejabberd SQL Database Schema
toc: true
menu: SQL Schema
order: 40
---

This page explains ejabberd SQL database schema.  We present the tables that might be in use, depending on your server configuration, together with a short explanation of the fields involved and their intended use. Tables are presented roughly grouped by related functionality.

Consider this document a work in progress, not all tables are documented yet.

Latest version of database schema are available in [ejabberd Github repository](https://github.com/processone/ejabberd):

* [MySQL schema](https://github.com/processone/ejabberd/blob/master/sql/mysql.sql)
* [Postgres schema](https://github.com/processone/ejabberd/blob/master/sql/pg.sql)
* [SQLite schema](https://github.com/processone/ejabberd/blob/master/sql/lite.sql)
* [MS SQL Server schema](https://github.com/processone/ejabberd/blob/master/sql/mssql.sql). This
  schema need testing / feedback and possibly improvement from SQL
  Server users.

# Authentication

## Table `users`

Contains the information required to authenticate users.

| Field                | Type             | Usage                                                                                   |
| -------------------- | ---------------- | --------------------------------------------------------------------------------------- |
| username             | string           | User                                                                                    |
| password             | string           | User password, can be hashed                                                            |
| created_at           | timestamp        | When the user account was created                                                       |


The password are hashed if you use SCRAM authentication. In that case the next fields are also defined

| Field                | Type             | Usage                                                                                   |
| -------------------- | ---------------- | --------------------------------------------------------------------------------------- |
| serverkey            | string           | support for salted passwords                                                            |
| salt                 | string           | support for salted passwords                                                            |
| iterationcount       | integer          | support for salted passwords                                                            |


# Rosters

## Table `rosterusers`

This is a quite complex table, used as a store for a quite complex protocol that is the one defined to manage
rosters and subscriptions on [rfc6121](http://tools.ietf.org/html/rfc6121).  

In the common case of two users adding each other as contacts, entries in the roster table follows a series of steps
 as they moves from a subscription request to the final approval and bi-directional subscription being established. 
This process can be initiated either by the user, or by the (possible remote) peer. Also need to account for the case
where the user, or the contact, might not be online at the moment of the subscription request is made.

Steps are further complicated by the fact that entries in the roster aren't required to have corresponding subscriptions. 
For details of the meaning of the different fields, refer to [the protocol itself](http://tools.ietf.org/html/rfc6121#section-2), as these are mostly a direct mapping of it. 
 
Note:
If you manage users contacts from outside the roster workflow of XMPP (for example your site backends perform the linking between
users),  it is likely that you only need to care about the  username, jid and nick fields,  and set the subscription field to be always
'B' for a mutual link between users.


| Field                | Type             | Usage                                                                                   |
| -------------------- | ---------------- | --------------------------------------------------------------------------------------- |
| username             | string           | User                                                                                    |
| jid                  | string           | Contact jid                                                                             |
| nick                 | string           | Contact nickname                                                                        |
| subscription         | char             | 'B'=both &#124; 'T'=To &#124; 'F'=From &#124; 'N'=none                                  |
| ask                  | char             | 'S'=subscribe &#124; 'U'=unsubscribe &#124; B='both' &#124; 'O'=out &#124; 'I'=in &#124; 'N'=none                |
| askmessage           | string           |  Message to be displayed on the subscription request                                    |
| server               | char             | 'N' for normal users contacts                                                           |
| subscribe            | string           |                                                                                         |
| type                 | string           | "item"                                                                                  |
| created_at           | timestamp        | Creation date of this roster entry                                                      |




## Table `rostergroups`

## Table `sr_group`

## Table `sr_user`



# Messages

## Table `spool`
Messages sent to users that are offline are stored in this table. 
Do not confuse this with general message archiving: messages are only temporarily stored in this table, removed as soon as the target user 
is back online and the pending messages delivered to it.

| Field                | Type             | Usage                                                                                   |
| -------------------- | ---------------- | --------------------------------------------------------------------------------------- |
| username             | string           | User                                                                                    |
| xml                  | blob             | Raw packet                                                                              |
| seq                  | integer          | Unique, autoincrement sequence number.                                                  |
| created_at           | timestamp        | When the message was stored                                                             |

The seq field is used for sorting, and to easily identify a particular user message.


## Table `privacy_list_data`

The table is used to store privacy rules.

The table is a direct translation of the XMPP packet used to set
privacy lists. For more details, please read
[XEP-0016: Privacy Lists, Syntax and Semantics](http://xmpp.org/extensions/xep-0016.html#protocol-syntax). Here
is an example packet coming from privacy list specification:

    
    <item
         type='[jid|group|subscription]'
         value='bar'
         action='[allow|deny]'
         order='unsignedInt'>
        [<message/>]
        [<presence-in/>]
        [<presence-out/>]
        [<iq/>]
     </item>

The table fields are defined as follow:

| Field                | Type             | Usage                                                                                   |
| -------------------- | ---------------- | --------------------------------------------------------------------------------------- |
| id                   | int              | Privacy list rule id.                                                                   |
| t                    | char             | Privacy rule type: 'j' for jid, 'g' for group and 's' for subscription.                 |
| value                | string           | Privacy list value for match, whose content depends on privacy list rule type.          |
| action               | char             | Privacy list action: 'd' for deny and 'a' for allow.                                    |
| ord                  | int              | Order for applying the privacy list rule.                                               |
| match\_all           | boolean (0 or 1) | If true (1), means any packet types will be matched. Other matches should be false (0). |
| match\_iq            | boolean (0 or 1) | If true (1), means iq packets will be matched by rule.                                  |
| match\_message       | boolean (0 or 1) | If true (1), means message packets type will be matched by rule.                        |
| match\_presence\_in  | boolean (0 or 1) | If true (1), means inbound presence packets type will be matched by rule.               |
| match\_presence\_out | boolean (0 or 1) | If true (1), means outbound packets type will be matched by rule.                       |



# Multiuser Chat Rooms

## Table `muc_room`
It is used to store *persistent* rooms, that is, rooms that must be automatically started with the server.

| Field                | Type             | Usage                                                                                   |
| -------------------- | ---------------- | --------------------------------------------------------------------------------------- |
| name                 | string           | Room name                                                                               |
| host                 | string           | Hostname of the conference component                                                    |
| opts                 | string           | Room options, encoded as erlang terms                                                   |
| created_at           | timestamp        | Creation date                                                                           |

The opts field is legible, but not mean to be modified directly. It contents depends on the implementation of mod_muc. 
It contains the room configuration and affiliations.


## Table `muc_registered`
Contains a map of user to nicknames.  When a user register a nickname with the conference module, that nick is reserved and can't be used by 
anyone else, in any room from that conference host.

| Field                | Type             | Usage                                                                                   |
| -------------------- | ---------------- | --------------------------------------------------------------------------------------- |
| jid                  | string           | User jid                                                                                |
| host                 | string           | Hostname of the conference component                                                    |
| nick                 | string           | Room options, encoded as erlang terms                                                   |
| created_at           | timestamp        | Creation date                                                                           |


## Table `room_history`
In ejabberd Business Edition,
this table is used if persistent room history is enabled. If so, recent room history is saved to the DB before ejabberd is stopped,
allowing the recent history to survive server restarts.

| Field                | Type             | Usage                                                                                   |
| -------------------- | ---------------- | --------------------------------------------------------------------------------------- |
| room                 | string           | Room jid                                                                                |
| nick                 | string           | Nickname that sent the message                                                          |
| packet               | string           | XML stanza with the message                                                             |
| have_subject         | boolean          | True if the message stanza had subject                                                  |
| created_at           | timestamp        | Creation date                                                                           |
| size                 | integer          | Size in bytes of the xml packet                                                         |


# VCard

## Table `vcard`

The table is used to store raw vCard content for delivery of the vCard
"as is".

The table fields are defined as follow:

| Field                | Type             | Usage                |
| -------------------- | ---------------- | -------------------- |
| username             | string           | Owner of the Vcard   |
| vcard                | text             | Raw Vcard            |
| created_at           | timestamp        | Record creation date |

## Table `vcard_search`

The table is used to store vCard index on a few of the Vcard field
used for vCard search in users directory.

You can learn more about the vCard specification on Wikipedia
[vCard](https://en.wikipedia.org/wiki/VCard) page.

The table fields are defined as follow:

| Field                | Type             | Usage                                         |
| -------------------- | ---------------- | --------------------------------------------- |
| username             | string           | Raw username for display                      |
| lusername            | string           | Lowercase username for search                 |
| fn                   | string           | Raw fullname for display                      |
| lfn                  | string           | Lowercase fullname for search                 |
| familly              | string           | Raw family name for display                   |
| lfamilly             | string           | Lowercase family name for search              |
| given                | string           | Raw given name for display                    |
| lgiven               | string           | Lowercase given name for search               |
| middle               | string           | Raw middle name for display                   |
| lmiddle              | string           | Lowercase middle name for search              |
| nickname             | string           | Raw nickname for display                      |
| lnickname            | string           | Lowercase nickname for search                 |
| bday                 | string           | Raw birthday for display                      |
| lbday                | string           | Lowercase and processed birthday for search   |
| ctry                 | string           | Raw country for display                       |
| lctry                | string           | Lowercase country for search                  |
| locality             | string           | Raw city for display                          |
| llocality            | string           | Lowercase city for search                     |
| email                | string           | Raw email for display                         |
| lemail               | string           | Lowercase email for search                    |
| orgname              | string           | Raw organisation name for display             |
| lorgname             | string           | Lowercase organisation name for search        |
| orgunit              | string           | Raw organisation department name for display    |
| lorgunit             | string           | Lowercase organisation department for search |

# Others

## Table `last`
This table is used to store the last time the user was seen online.  
It is defined as follow:

| Field                | Type             | Usage                                                                                   |
| -------------------- | ---------------- | --------------------------------------------------------------------------------------- |
| username             | string           | User                                                                                    |
| seconds              | string           | Timestamp for the last time the user was seen online                                    |
| state                | string           | Why user got disconnected. Usually is empty                                             |

Note that the table is *not* updated while the user has the session open. 



## Table `caps_features`
Ejabberd uses this table to keep a list of the entity capabilities discovered.

| Field                | Type             | Usage                                                                                   |
| -------------------- | ---------------- | --------------------------------------------------------------------------------------- |
| node                 | string           | Node                                                                                    |
| subnode              | string           | Subnode                                                                                 |
| feature              | string           | Entity feature                                                                          |
| created_at           | timestamp        | Creation date                                                                           |

The subnode field correspond to the 'ver' ("verification string") of XEP-0115. 
There is one entry in this table for each feature advertised by the given (node,subnode) pair.

## Table `private_storage`
Used for user private data storage.

| Field                | Type             | Usage                                                                                   |
| -------------------- | ---------------- | --------------------------------------------------------------------------------------- |
| username             | string           | User                                                                                    |
| namespace            | string           | XEP-0049 namespace of the stored data                                                   |
| data                 | string           | Raw xml                                                                                 |
| created_at           | timestamp        | Creation date                                                                           |


---
title: ejabberd SQL database schema | ejabberd documentation
---

# ejabberd SQL database schema

This page explains ejabberd SQL database schema

## Table `privacy_list_data`

The table is used to store privacy rules.

The table is a direct translation of the XMPP packet used to set
privacy lists. For more details, please read
[XEP-0016: Privacy Lists, Syntax and Smemantics](http://xmpp.org/extensions/xep-0016.html#protocol-syntax). Here
is an example packet coming from privacy list specification:

    #!xml
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
| orgunit              | string           | Raw organation department name for display    |
| lorgunit             | string           | Lowercase organisation departement for search |


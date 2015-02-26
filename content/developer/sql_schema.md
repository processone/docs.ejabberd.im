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

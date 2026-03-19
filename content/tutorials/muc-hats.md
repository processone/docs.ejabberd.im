# MUC Hats

!!! info "Please note"

    This page is useful only for ejabberd versions older than 25.10.
    If you are using ejabberd 25.10 or newer,
    please consult [XEP-0317 Hats](https://xmpp.org/extensions/xep-0317.html)
    as ejabberd nowadays supports XEP-0317 0.3.1.

<!-- md:version improved in [25.10](../archive/25.10/index.md) -->

ejabberd [21.12](../archive/21.12/index.md) introduced support for
[XEP-0317 Hats](https://xmpp.org/extensions/xep-0317.html).

In ejabberd [25.03](../archive/25.03/index.md) support was improved to XEP version 0.2.0,
with some minor differences to the examples in the protocol,
which are described in this page.

In ejabberd [25.10](../archive/25.10/index.md) support has been improved to XEP version 0.3.1,
strictly following the protocol and all its examples.
Consequently, if you are using ejabberd 25.10 or newer,
please use [XEP-0317 Hats](https://xmpp.org/extensions/xep-0317.html)
instead of this page.

## Configuration

To test those examples, let's apply some configuration changes to the default ejabberd configuration file:

```diff
diff --git a/ejabberd.yml.example b/ejabberd.yml.example
index 0964afa06..ae782dde0 100644
--- a/ejabberd.yml.example
+++ b/ejabberd.yml.example
@@ -16,6 +16,7 @@
 
 hosts:
   - localhost
+  - example.edu
 
 loglevel: info
 
@@ -188,6 +189,7 @@ modules:
     default: always
   mod_mqtt: {}
   mod_muc:
+    host: "courses.@HOST@"
     access:
       - allow
     access_admin:
@@ -198,6 +200,8 @@ modules:
       - allow
     default_room_options:
       mam: true
+      enable_hats: true
+      persistent: true
   mod_muc_admin: {}
   mod_muc_occupantid: {}
   mod_offline:

```

## Adding a Hat

When adding a hat as documented in
[3.2 Adding a Hat](https://xmpp.org/extensions/xep-0317.html#add)
there are a few differences:

- the form provided by ejabberd is slightly different to the examples,
  consequently the form filling must be updated.
- form submission in example 5 should have in the `command` element `action='complete'`

In summary, the flow is:

### Admin Requests to Add a Hat

Identical to [Example 3](https://xmpp.org/extensions/xep-0317.html#example-3):

```xml
<iq from='professor@example.edu/office'
    id='fdi3n2b6'
    to='physicsforpoets@courses.example.edu'
    type='set'
    xml:lang='en'>
  <command xmlns='http://jabber.org/protocol/commands'
           action='execute'
           node='urn:xmpp:hats:commands:don'/>
</iq>
```

### Service Returns Form to Admin

The formulary provided by ejabberd is slightly different than the one in
[Example 4](https://xmpp.org/extensions/xep-0317.html#example-4):

```xml
<iq from='physicsforpoets@courses.example.edu'
	id='fdi3n2b6'
    to='professor@example.edu/office'
	type='result'
	xml:lang='end'>
  <command xmlns='http://jabber.org/protocol/commands'
           node='urn:xmpp:hats:commands:don'
           sessionid='2025-02-14T12:23:47.445692Z'
           status='executing'>
    <actions execute='complete'>
      <complete/>
    </actions>
    <x type='form' xmlns='jabber:x:data'>
      <title>Add a hat to a user</title>
      <field var='jid'
             type='jid-single'
             label='Jabber ID'>
        <required/>
      </field>
      <field var='hat_title'
             type='text-single'
             label='Hat title'/>
      <field var='hat_uri'
             type='text-single'
             label='Hat URI'>
        <required/>
      </field>
    </x>
  </command>
</iq>
```

### Admin Submits Form

Compared to [Example 5](https://xmpp.org/extensions/xep-0317.html#example-5),
the `command` element includes `action='complete'`,
and the form fields are different:

```xml
<iq from='professor@example.edu/office'
    id='9fens61z'
    to='physicsforpoets@courses.example.edu'
    type='set'
    xml:lang='en'>
  <command xmlns='http://jabber.org/protocol/commands'
           node='urn:xmpp:hats:commands:don'
           action='complete'
           sessionid='2025-02-14T12:23:47.445692Z'>
    <x xmlns='jabber:x:data' type='submit'>
      <field type='hidden' var='FORM_TYPE'>
        <value>urn:xmpp:hats:commands</value>
      </field>
      <field var='jid'>
        <value>terry.anderson@example.edu</value>
      </field>
      <field var='hat_title'>
        <value>Teacher Assistant title</value>
      </field>
      <field var='hat_uri'>
        <value>http://tech.example.edu/hats#TeacherAssistant</value>
      </field>
    </x>
  </command>
</iq>
```

### Service Informs Admin of Completion

The result stanza is similar to [Example 6](https://xmpp.org/extensions/xep-0317.html#example-6):

```xml
<iq from='physicsforpoets@courses.example.edu'
	id='9fens61z'
    to='professor@example.edu/office'
	type='result'
	xml:lang='en'>
  <command status='completed'
           sessionid='2025-02-14T12:23:47.445692Z'
           node='urn:xmpp:hats:commands:don'
           xmlns='http://jabber.org/protocol/commands'/>
</iq>
```

## Listing Hats

It's useful to be able to list the existing room hats,
but the XEP doesn't document that possibility,
so a custom method is implemented in ejabberd:
sending a command query to node `urn:xmpp:hats:commands:dlist`:

### Admin Requests to List Hats

```xml
<iq from='professor@example.edu/office'
    id='fdi3n2b6'
    to='physicsforpoets@courses.example.edu'
    type='set'
    xml:lang='en'>
  <command xmlns='http://jabber.org/protocol/commands'
           action='execute'
           node='urn:xmpp:hats:commands:dlist'/>
</iq>
```

### Service Returns List of Hats

```xml
<iq xml:lang='en'
	to='professor@example.edu/office'
	from='physicsforpoets@courses.example.edu'
	type='result'
	id='fdi3n2b6'>
  <command status='completed'
           sessionid='2025-02-14T12:27:33.414328Z'
           node='urn:xmpp:hats:commands:dlist'
           xmlns='http://jabber.org/protocol/commands'>
    <x type='result' xmlns='jabber:x:data'>
      <title>List of users with hats</title>
      <reported>
        <field var='jid' label='Jabber ID'/>
        <field var='hat_title' label='Hat title'/>
        <field var='hat_uri' label='Hat URI'/>
      </reported>
      <item>
        <field var='jid'>
          <value>terry.anderson@example.edu</value>
        </field>
        <field var='hat_title'>
          <value>http://tech.example.edu/hats#TeacherAssistant</value>
        </field>
        <field var='hat_uri'>
          <value>Teacher Assistant title</value>
        </field>
      </item>
    </x>
  </command>
</iq>

```

## Including a Hat in Presence

In the previous examples, `professor` added a hat to `terry.anderson`.
Then let's imagine `terry.anderson` joins the room.
Finally, when any client joins the room, for example `steve`,
he receives a stanza like
the one in [Example 1](https://xmpp.org/extensions/xep-0317.html#example-1):

```xml
<presence from='physicsforpoets@courses.example.edu/Terry'
          id='34:271777'
          to='steve@example.edu/tablet'
          xml:lang='es'>
  <x xmlns='http://jabber.org/protocol/muc#user'>
    <item role='participant' affiliation='none'/>
  </x>
  <hats xmlns='urn:xmpp:hats:0'>
    <hat uri='http://tech.example.edu/hats#TeacherAssistant'
         title='Teacher Assistant title'/>
  </hats>
  <status>status user1</status>
</presence>
```

## Removing a Hat

This works similarly to the examples in
[3.3 Removing a Hat](https://xmpp.org/extensions/xep-0317.html#remove)

### Admin Requests to Remove a Hat

Remember that the form for the hat is different than the one in
[Example 7](https://xmpp.org/extensions/xep-0317.html#example-7).

```xml
<iq from='professor@example.edu/office'
    id='9fens61z'
    to='physicsforpoets@courses.example.edu'
    type='set'
    xml:lang='en'>
  <command xmlns='http://jabber.org/protocol/commands'
           node='urn:xmpp:hats:commands:doff'
           action='complete'
           sessionid='2025-02-14T12:23:47.445692Z'>
    <x xmlns='jabber:x:data' type='submit'>
      <field type='hidden' var='FORM_TYPE'>
        <value>urn:xmpp:hats:commands</value>
      </field>
      <field var='jid'>
        <value>terry.anderson@example.edu</value>
      </field>
      <field var='hat_title'>
        <value>Teacher Assistant title</value>
      </field>
      <field var='hat_uri'>
        <value>http://tech.example.edu/hats#TeacherAssistant</value>
      </field>
    </x>
  </command>
</iq>
```

### Service Informs Admin of Completion

The response is similar to
[Example 8](https://xmpp.org/extensions/xep-0317.html#example-8):

```xml
<iq from='physicsforpoets@courses.example.edu'
	id='9fens61z'
	to='professor@example.edu/office'
	type='result'
    xml:lang='en'>
  <command xmlns='http://jabber.org/protocol/commands'
           node='urn:xmpp:hats:commands:doff'
           sessionid='2025-02-14T12:23:47.445692Z'
           status='completed'/>
</iq>
```


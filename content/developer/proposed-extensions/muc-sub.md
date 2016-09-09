---
title: Understanding ejabberd and its dependencies
---
# Multi-User Chat Subscriptions

In XMPP, Multi-User Chat rooms design rely on presence. To participate
in a MUC room, you need to send a presence to the room. When you get
disconnected, you leave the room and stopped being part of the
room. User involvement in MUC rooms is not permanent.

This is an issue with the rise of mobile applications. Chatting with
friends in a room is a big part of messaging usage on mobile. However,
to save battery life, mobile devices will freeze mobile XMPP
application after a while when they get to background. It means that
the connection is lost and that the session is usually terminated.

Some workaround have been used to try letting user keep on receiving
messages from MUC room when the app is in background. The most common
one is to keep the session open for a while until a timeout
happens. This is the approach promoted on mobile by XEP-0198 - Stream
Management. When messages are received and no TCP/IP connection is
attached, server usually fallback sending the message to the user
using mobile push notification service to warn the user that a message
has been received.

This approach has many drawbacks:

1. It is not permanent. The idea of having the session kept into
   memory for a while is interesting but it is just a long
   timeout. After that timeout, the session is closed and the user
   will leave the room. No message will be received anymore.
2. It does not play well with normal server / cluster operations. If
   you restart the service where the user session is kept, it will
   disappear. You can dump them to disk and recreate them on start,
   but it means that if the node crashes, your session will be losts
   and user will stop receiving messages.
3. It does not change the fundamental nature of MUC chat room. They
   are still presence-based. It means that if you need to restart the
   MUC service, or if it crashes, presence are lost. For connected
   clients, they are expected to join the MUC room again. However, for
   mobile clients, it cannot happens until user reopens the
   app. Moreover, it means that on new session start, user client is
   expected to join all the MUC room he wants to keep track of on
   connect.

This specification tries to solve those issues by keeping most of
the logic of the MUC room intact. There is attempt to rewrite XMPP
Multi-User chat rooms by splitting presence from ability to receive
and send messages (XEP-0369: Mediated Information eXchange
(MIX)). However, the features covered by the MUC protocol are quite
comprehensive and the MIX protocol is not yet ready to cover all the
MUC use cases yet. The goal is to produce an intermediate state that
is compliant with MUC and leverage most of the MUC features, while
adding the most basic feature possible to implement the MUC Sub
extension.

This specifications tries to merge ideas to produce a MUC extension
that make MUC rooms mobile clients friendly.

To play well with mobile, MUC room need to support the following
features:

- Add the ability to send and receive messages to a room without
  having to send presence to the room. More generally allow other type
  of interaction with the room (like configuration changes for example
  or kick and ban). We will leverage existing publish and subscribe
  approach.
- Add the ability to resync the client for missed messages on
  reconnect. We will leverage Message Archive Management service for
  MUC.
- Finally, ensure that a server can implement push notification
  service to ensure alerting of offline users when MUC messages are
  received.

The goal is to learn from real life working implementation to help feeding
MIX with feedback from the field, without having to reinvent a complete new
protocol.

## General principle

The core idea is to expose MUC rooms as pubsub nodes and to introduce
the concept of MUC rooms subscribers.

A user affiliated to a MUC room should be able to subscribe to MUC
node events and have them routed to his jid, even if he is not a
participant in the room. It means that a user can receive messages
without having to send presence to the room. In that sense, "joining
the room" in XEP-0045 becomes more "Being available in the MUC room".

## Discovering support

### Discovering support on MUC service

You can check if MUC Sub feature is available on MUC service by sending disco info IQ:

~~~ xml
<iq from='hag66@shakespeare.example/pda'
    to='muc.shakespeare.example'
    type='get'
    id='ik3vs715'>
  <query xmlns='http://jabber.org/protocol/disco#info'/>
</iq>
~~~

MUC service will show a feature of type 'urn:xmpp:mucsub:0' to the response if the
feature is supported and enabled:

~~~ xml
<iq from="muc.shakespeare.example"
    to="hag66@shakespeare.example/pda"
    type="result"
    id="ik3vs715">
  <query xmlns="http://jabber.org/protocol/disco#info">
    <identity category="conference"
              type="text"
              name="Chatrooms" />
    ...
    <feature var="urn:xmpp:mucsub:0" />
    ...
  </query>
</iq>
~~~

### Discovering support on a specific MUC

A user can discover support for MUC/Sub feature on a MUC room as
follow:

~~~ xml
<iq from='hag66@shakespeare.example/pda'
    to='coven@muc.shakespeare.example'
    type='get'
    id='ik3vs715'>
  <query xmlns='http://jabber.org/protocol/disco#info'/>
</iq>
~~~

A conference MUST add 'urn:xmpp:mucsub:0' to the response if the
feature is supported and enabled:

~~~ xml
<iq from='coven@muc.shakespeare.example'
    to='hag66@shakespeare.example/pda'
    type='result'
    id='ik3vs715'>
  <query xmlns='http://jabber.org/protocol/disco#info'>
    <identity category='conference'
              name='A Dark Cave'
              type='text' />
    <feature var='http://jabber.org/protocol/muc' />
    ...
    <feature var='urn:xmpp:mucsub:0' />
    ...
  </query>
</iq>
~~~

## Option MUC room support for subscriptions

Even if MUC room supports MUC/Sub feature, it MAY be explicitly
enabled or disabled thanks to a new configuration option:

- Allow subscription: Users can subscribe to MUC/Sub events.

## Subscriber role

Until a subscriber is not joined a conference (see [Joining a MUC Room](#joining-a-muc-room)),
a subscriber role MUST be 'none'. When a subscriber is joined a conference its role is
changed according to [XEP-0045](http://xmpp.org/extensions/xep-0045.html) rules, that is,
it becomes either 'visitor', 'participant' or 'moderator'.

## Subscribing to MUC/Sub events

User can subscribe to the following events, by subscribing to specific
nodes:

- urn:xmpp:mucsub:nodes:presence
- urn:xmpp:mucsub:nodes:messages
- urn:xmpp:mucsub:nodes:affiliations
- urn:xmpp:mucsub:nodes:config
- urn:xmpp:mucsub:nodes:subject
- urn:xmpp:mucsub:nodes:system

Example: User Subscribe to MUC/Sub events

~~~ xml
<iq from='hag66@shakespeare.example'
    to='coven@muc.shakespeare.example'
    type='set'
    id='E6E10350-76CF-40C6-B91B-1EA08C332FC7'>
  <subscribe xmlns='urn:xmpp:mucsub:0'
             nick='mynick'>
    <event node='urn:xmpp:mucsub:nodes:messages' />
    <event node='urn:xmpp:mucsub:nodes:affiliations' />
    <event node='urn:xmpp:mucsub:nodes:subject' />
    <event node='urn:xmpp:mucsub:nodes:config' />
  </subscribe>
</iq>
~~~

If user is allowed to subscribe, server replies with success.

Example: Server replies with success

~~~ xml
<iq from='coven@muc.shakespeare.example'
    to='hag66@shakespeare.example'
    type='result'
    id='E6E10350-76CF-40C6-B91B-1EA08C332FC7'>
  <subscribe xmlns='urn:xmpp:mucsub:0'>
    <event node='urn:xmpp:mucsub:nodes:messages' />
    <event node='urn:xmpp:mucsub:nodes:affiliations' />
    <event node='urn:xmpp:mucsub:nodes:subject' />
    <event node='urn:xmpp:mucsub:nodes:config' />
  </subscribe>
</iq>
~~~

Subscription is associated with a nick. It will implicitly register the
nick. Server should otherwise make sure that subscription match the
user registered nickname in that room.
In order to change the nick and/or subscription nodes the same request
MUST be sent with a different nick or nodes information.

Example: User changes subscription data

~~~ xml
<iq from='hag66@shakespeare.example'
    to='coven@muc.shakespeare.example'
    type='set'
    id='E6E10350-76CF-40C6-B91B-1EA08C332FC7'>
  <subscribe xmlns='urn:xmpp:mucsub:0'
             nick='newnick'>
    <event node='urn:xmpp:mucsub:nodes:messages' />
    <event node='urn:xmpp:mucsub:nodes:presence' />
  </subscribe>
</iq>
~~~

## Unsubscribing from a MUC Room

At any time a user can unsubsribe from MUC Room events.

Example: User unsubscribes from a MUC Room

~~~ xml
<iq from='hag66@shakespeare.example'
    to='coven@muc.shakespeare.example'
    type='set'
    id='E6E10350-76CF-40C6-B91B-1EA08C332FC7'>
  <unsubscribe xmlns='urn:xmpp:mucsub:0' />
</iq>
~~~

Example: A MUC Room responds to unsubscribe request

~~~ xml
<iq from='hag66@shakespeare.example'
    to='coven@muc.shakespeare.example'
    type='result'
    id='E6E10350-76CF-40C6-B91B-1EA08C332FC7' />
~~~

## Subscriber actions

If not stated otherwise in this document, a subscriber MUST perform any actions in the conference
as described in [XEP-0045](http://xmpp.org/extensions/xep-0045.html). For example,
it MUST send messages to all occupants according to
[7.4 Sending a Message to All Occupants](http://xmpp.org/extensions/xep-0045.html#message),
it MUST configure a conference according to
[10.2 Subsequent Room Configuration](http://xmpp.org/extensions/xep-0045.html#roomconfig)
and so on.

Here are a few examples:

### Sending a message

Sending a message is like sending a standard groupchat message in MUC room:

~~~ xml
<message from="hag66@shakespeare.example"
         to="coven@muc.shakespeare.example"
         type="groupchat">
  <body>Test</body>
</message>
~~~

No need to join it after you connect. As a subscriber, you can send messages at any time.

### Joining a MUC Room

If a user wants to be present in the room, he just have to join the room as defined in XEP-0045.

A subscriber MAY decide to join a conference
(in the [XEP-0045](http://xmpp.org/extensions/xep-0045.html) sense).
In this case a conference MUST behave as described in
[XEP-0045 7.2 Entering a Room](http://xmpp.org/extensions/xep-0045.html#enter-muc).
A conference MUST process events as described under
[XEP-0045 7.1 Order of Events](http://xmpp.org/extensions/xep-0045.html#order)
except it MUST not send room history.
When a subscriber is joined, a conference MUST stop sending subscription events and
MUST switch to a regular groupchat protocol (as described in
[XEP-0045](http://xmpp.org/extensions/xep-0045.html)) until a subscriber
leaves.

## Receiving events

Here is as an example message received by a subscriber when a message is posted to a MUC room:

~~~ xml
<message from="coven@muc.shakespeare.example"
         to="hag66@shakespeare.example/pda">
  <event xmlns="http://jabber.org/protocol/pubsub#event">
    <items node="urn:xmpp:mucsub:nodes:messages">
      <item id="18277869892147515942">
        <message from="coven@muc.shakespeare.example/secondwitch"
                 to="hag66@shakespeare.example/pda"
                 type="groupchat"
                 xmlns="jabber:client">
          <archived xmlns="urn:xmpp:mam:tmp"
                    by="muc.shakespeare.example"
                    id="1467896732929849" />
          <stanza-id xmlns="urn:xmpp:sid:0"
                     by="muc.shakespeare.example"
                     id="1467896732929849" />
          <body>Hello from the MUC room !</body>
        </message>
      </item>
    </items>
  </event>
</message>
~~~

Presence changes in the MUC room are received wrapped in the same way:

~~~ xml
<message from="coven@muc.shakespeare.example"
         to="hag66@shakespeare.example/pda">
  <event xmlns="http://jabber.org/protocol/pubsub#event">
    <items node="urn:xmpp:mucsub:nodes:presences">
      <item id="8170705750417052518">
        <presence xmlns="jabber:client"
                  from="coven@muc.shakespeare.example/secondwitch"
                  type="unavailable"
                  to="hag66@shakespeare.example/pda">
          <x xmlns="http://jabber.org/protocol/muc#user">
            <item affiliation="none"
                  role="none" />
          </x>
        </presence>
      </item>
    </items>
  </event>
</message>
~~~

TODO: Describe the mapping of MUC protocol stanzas to MUC/Sub events for configuration updates.

## List of subscriptions

A user can query the MUC service to get his list of
subscriptions.

Example: User asks for subscriptions list

~~~ xml
<iq from='hag66@shakespeare.example'
    to='muc.shakespeare.example'
    type='get'
    id='E6E10350-76CF-40C6-B91B-1EA08C332FC7'>
  <subscriptions xmlns='urn:xmpp:mucsub:0' />
</iq>
~~~

Example: Server replies with subscriptions list

~~~ xml
<iq from='muc.shakespeare.example'
    to='hag66@shakespeare.example'
    type='result'
    id='E6E10350-76CF-40C6-B91B-1EA08C332FC7'>
  <subscriptions xmlns='urn:xmpp:mucsub:0'>
    <subscription jid='coven@muc.shakespeare.example' />
    <subscription jid='chat@muc.shakespeare.example' />
  </subscriptions>
</iq>
~~~

A moderator can get the list of subscribers by sending <subscriptions/>
request directly to MUC JID.

Example: Moderator asks for subscribers list

~~~ xml
<iq from='hag66@shakespeare.example'
    to='coven@muc.shakespeare.example'
    type='get'
    id='E6E10350-76CF-40C6-B91B-1EA08C332FC7'>
  <subscriptions xmlns='urn:xmpp:mucsub:0' />
</iq>
~~~

Example: Server replies with subscribers list

~~~ xml
<iq from='coven@muc.shakespeare.example'
    to='hag66@shakespeare.example'
    type='result'
    id='E6E10350-76CF-40C6-B91B-1EA08C332FC7'>
  <subscriptions xmlns='urn:xmpp:mucsub:0'>
    <subscription jid='juliet@shakespeare.example' />
    <subscription jid='romeo@shakespeare.example' />
  </subscriptions>
</iq>
~~~

## Compliance with existing MUC clients

MUC/Sub approach is compliant with existing MUC service and MUC
clients. MUC clients compliant with XEP-0045 will receive message
posted by subscribers. They may not see the user presence, but it
should not be an issue for most clients. Most clients already support
receiving messages from user that are not currently in MUC room through
history retrieval.

This approach should also help most client support better integration with
third-party services posting to MUC room through API (as )

However, a server could choose to send presence on behalf of
subscribers when a user join the MUC (in the xep-0045 sense) so that
subscriber will be shown in MUC roster of legacy clients.

## Synchronization of MUC messages: Leveraging MAM support

To be friendly with mobile, the MAM service should allow a user to
connect and easily resync his history for all MUC subscriptions.

Thanks to ability to get the list of all the existing subscription, client
can get a starting point to interact with MAM service to resync history.

There is no special addition need to resync using MAM MUC service
support, except maybe future optimization to get a better idea of the room that
need to be resynced. It is possible by having offline module flag room
as obsolete for a given user and offer a way to get the obsolete list.

## Push support compliance

Subscriptions are compliant with push mechanism. It is supported out of
the box when using ProcessOne p1:push implementation (deployed on ejabberd
SaaS for example).

More generally, it is straightforward to handle them through ejabberd
developer API to implement custom mechanisms.

Subscription are delivered to online users. If user has no active
session, serveur can choose to broadcast to user through a push
notification.

When a session is open, if the server detect that the user has not
been recently active, or for any other reason, the server can still
forward the message to a push notification service to warn the user
that new messages are available in a MUC room.

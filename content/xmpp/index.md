---
title: XMPP use cases
---

# XMPP use cases

[XMPP](http://www.xmpp.org) is a very versatile protocol designed to
address many use cases of modern realtime messaging needs. However, it
is also a very large protocol and it is difficult to understand at
first sight all the use cases that XMPP adequately address.

This page is gathering XMPP specifications that make XMPP a good fit
for a given use case of industry.

## Realtime web

XMPP was designed before the advent of realtime web. However, it
managed to adapt thanks to the following specifications:

* XMPP PubSub is defined in
  [XEP-0060](http://www.xmpp.org/extensions/xep-0060.html). This is a
  very powerful mechanism that define channel based communication on
  top of XMPP protocol itself. A server can handles millions of
  channels, call Pubsub nodes. Users interested in specific channels
  can subscribe to nodes. When data need to be send to a given
  channel, authorized publishers can send data to that node. The XMPP
  server sill then broadcast the content to all subscribers. This is
  very adequate for realtime web as it allows you to broadcast
  relevant events to web pages.

* Websocket: XMPP over websocket is a defined in
  [RFC 7395](https://tools.ietf.org/html/rfc7395). It is more
  efficient and more scalable than XMPP for web previous
  specifications called
  [BOSH](http://xmpp.org/extensions/xep-0124.html). Websocket being a
  true bidirectional channel, it allows lower latency messaging and is
  very reliable. Note that BOSH can still be used transparently along
  with Websocket to support old web browsers.

**Use cases**: News, interactive web page, web chat, web games.

**Supported by ejabberd**: Yes.


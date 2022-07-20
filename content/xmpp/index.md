---
title: XMPP use cases
---

# XMPP use cases

[XMPP](https://xmpp.org) is a very versatile protocol designed to
address many use cases of modern real-time messaging needs. However, it
is also a very large protocol and it is difficult to understand at
first sight all the use cases that XMPP adequately addresses.

This page is gathering XMPP specifications that make XMPP a good fit
for a given use case of industry.

## Realtime web

XMPP was designed before the advent of realtime web. However, it
managed to adapt thanks to the following specifications:

* XMPP PubSub is defined in
  [XEP-0060](https://xmpp.org/extensions/xep-0060.html). This is a
  very powerful mechanism that defines channel based communication on
  top of the XMPP protocol itself. A server can handle millions of
  channels, called Pubsub nodes. Users interested in specific channels
  can subscribe to nodes. When data needs to be send to a given
  channel, authorized publishers can send data to that node. The XMPP
  server will then broadcast the content to all subscribers. This is
  very adequate for realtime web as it allows you to broadcast
  relevant events to web pages.

* WebSocket: XMPP over WebSocket is defined in
  [RFC 7395](https://tools.ietf.org/html/rfc7395). It is more
  efficient and more scalable than XMPP for web's previous
  specifications called
  [BOSH](https://xmpp.org/extensions/xep-0124.html). WebSocket being a
  true bidirectional channel, it allows lower latency messaging and is
  very reliable. Note that BOSH can still be used transparently along
  with WebSocket to support old web browsers.

**Use cases**: News, interactive web page, web chat, web games.

**Supported by ejabberd**: Yes.


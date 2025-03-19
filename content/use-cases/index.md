# ejabberd Use Cases

ejabberd is very versatile and is a solid choice to build messaging
services across a large number of industries:

## ejabberd

### Mobile messaging

ejabberd's massive scalability makes it the most solid choice as the
backbone for a very large number of mobile messaging services.

This includes:

* Chaatz
* [Libon](https://www.process-one.net/blog/libon-2-0-by-orange-vallee-uses-processone-to-benefit-from-smart-chatting/)
* [Nokia OVI Chat](https://en.wikipedia.org/wiki/Ovi_(Nokia))
* Roo Kids : Safe & fun instant messaging app for kids with minimum yet critical parental controls.
* Swisscom IO
* Versapp
* [Whatsapp](https://highscalability.com/the-whatsapp-architecture-facebook-bought-for-19-billion/)

### Gaming

* [Electronic Arts](https://www.ea.com/)
* [FACEIT](https://www.faceit.com/)
* [Kixeye](https://www.kixeye.com/)
* [Machine Zone (Game of War)](https://www.mz.com)
* [Nokia nGage](https://en.wikipedia.org/wiki/N-Gage_(service))
* [Riot Games (League of Legends)](https://highscalability.com/how-league-of-legends-scaled-chat-to-70-million-players-it-t/)

### Voice and video messaging

* [Nimbuzz](https://en.wikipedia.org/wiki/Nimbuzz)
* [ooVoo](https://www.oovoo.com/)
* [Sipphone](https://www.process-one.net/resources/case_studies/ProcessOne_SIP_Phone_Case_Study_v3.pdf)
* WowApp

### Internet of Things

* AeroFS
* IMA Teleassistance
* [Nabaztag](https://en.wikipedia.org/wiki/Nabaztag) (Violet, Mindscape, then Aldebaran Robotics)

### Telecom / Hosting

* [Fastmail](https://www.fastmail.com/blog/new-xmppjabber-server/)
* GMX
* [Mailfence](https://blog.mailfence.com/mailfence-groups/)
* Orange
* [SAPO - Portugal Telecom](https://www.process-one.net/resources/case_studies/ProcessOne_SAPO_Case_Study_v7.pdf)

### Customer chat / CRM

* CoBrowser.net: [Coder Interview](https://www.process-one.net/blog/code-as-craft-interview-cobrowser-net/).
* iAdvize
* [LiveHelpercChat](https://livehelperchat.com): [Blog post: Full XMPP chat support for ejabberd](https://livehelperchat.com/full-xmpp-chat-support-for-ejabberd-423a.html)

### Media

* [AFP](https://www.afp.com/en/)
* [BBC](https://www.process-one.net/resources/case_studies/ProcessOne_BBC_Case_Study_v2.pdf)

### Social media

* [Facebook](https://www.quora.com/Why-was-Erlang-chosen-for-use-in-Facebook-chat)
* Nasza Klasa (NKTalk messenger)
* [StudiVZ](https://en.wikipedia.org/wiki/StudiVZ)
* [Sify](https://highscalability.com/sifycom-architecture-a-portal-at-3900-requests-per-second/)
* [Tuenti](https://en.wikipedia.org/wiki/Tuenti)

### Sport

* [Major League of Baseball (MLB)](https://www.process-one.net/resources/case_studies/ProcessOne_ML_Baseball_Case_Study_v5.pdf)

### Education

* Apollo group
* Laureate

### Push alerts

* [Nokia push notifications](https://www.process-one.net/blog/sea-beyond-2011-talk-7-jukka-alakontiola-on-nokia-push-notifications/)
* [Notify.me](https://highscalability.com/notifyme-architecture-synchronicity-kills/))

### Dating

* Grindr
* [Meetic](https://www.meetic.com/)

### Community sites

* Jabber.at
* Talkr.im

## XMPP Use Cases

[XMPP](https://xmpp.org) is a very versatile protocol designed to
address many use cases of modern real-time messaging needs. However, it
is also a very large protocol and it is difficult to understand at
first sight all the use cases that XMPP adequately addresses.

This page is gathering XMPP specifications that make XMPP a good fit
for a given use case of industry.

### Realtime web

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

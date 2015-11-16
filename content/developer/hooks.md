---
title: Events and Hooks | ejabberd documentation
---

# Events and Hooks

## Introduction

ejabberd provides a very flexible way to tie together modular
components through an event system. This allows for loose coupling
between components of the system by calling only those which are
available and configured to be used at runtime.

Each module can subscribe to events and a hook in the module code is
called, passing all relevant data and parameters when the event occurs
in the server.

The end result is an extensible system with pluggable extra
functionality, that allows developers to customize ejabberd server
without limits.

## Example

The module
[mod_offline.erl](https://github.com/processone/ejabberd/blob/master/src/mod_offline.erl)
is a great example of how the events/hooks mechanism can be used.

`mod_offline` which is responsible for storing messages for delivery
to users unavailable at the time of sending. `mod_offline` is an
implementation of [XEP-0203](http://xmpp.org/extensions/xep-0203.html).

## Hook API

### Adding and removing hooks

To subscribe to events ejabberd modules register to hooks using the
following API:

When your ejabberd module start, you generally will want as a
developer to register the code (an handler function) to be executed in
your module when some of ejabberd specific events happen. This is done
with `ejabberd_hooks:add/5`:

    #!erlang
    ejabberd_hooks:add(Hook, Host, Module, Function, Priority)

As a consequence, you need to tell ejabberd to stop calling anymore
your when that event is triggered, when you stop your ejabberd
module. This is done with the `ejabberd_hooks:remove/5`.

    #!erlang
    ejabberd_hooks:remove(Hook, Host, Module, Function, Priority)

### Parameters

The parameter for the add / remove hooks are defined as follow:

* Hook = `atom()`
* Host = `string()`
* Module = `atom()`
* Function = `atom()`
* Priority = `integer()`

The `Hook` parameter is the name of the event (see below).

`Host` is the name of the [virtual host](/developer/hosts/) related to
the event.

`Module` and `Function` describe the hook to be called when the event occurs.

`Priority` is the hook rank, to determine in which order the hooks are
run (when several hooks are defined for the same event). You can use
it if you have dependencies between hooks.

### Types of event handlers

There is two types of hooks that you can register with in
ejabberd. The two type of hooks are roughly modeled upon the
[MapReduce](http://en.wikipedia.org/wiki/MapReduce) paradigm. It is
very important to know the type of the hooks you are dealing with, as
this is needed by your event handlers to know what can be returned in
your callback function.

* `run` hooks: This is hooks whose only role is to propagate events to
  handlers throughout the code. When that hook is run, it does not
  gather data. `run` hooks support two type of return value from their
  handler: 'stop' or anything else (that is ignored, so you can return
  the atom ok as a convention). If you return `stop` from your
  handler, it means no further processing will be done, i.e. that
  handlers defined for the same hook with lower priority will not be
  called. This approach is call `Enum.map/2` in Elixir.

* `run_fold` hooks: This is hooks whose purpose is to retrieve /
  gather data. It is name after the Erlang `lists:foldl/3` function,
  which is having data passed iteratively through a function to
  produce a final data set. This is called `Enum.reduce/3` in
  Elixir. As a developer of the handler function, you will receive
  data as an addition to parameters and you will have to return those
  data as a starting point for processing by the next handler
  registered to that event (if any). `run_fold` handler can return an
  accumulator `Value` or `{stop, Value}`.

It means that hooks registration priority is important for two primary
reasons:

* changing the prioriry (and thus order) of the `run_fold` handlers can
  change the final data set produced (depending of the process performed
  in each handler).

* changing the prioriry (and thus order) of both the `run` and
  `run_fold` handlers can prevent some of them from executed if they
  return the `stop` or `{stop, FinalValue}`.

## ejabberd standard events

ejabberd offers close to 100 standard hooks that are used for various
purposes inside ejabberd server. Many of them are used to decouple
some components in the core of the ejabberd application server
itself. As an extension developer, you will likely use a much smaller
subsets of the available hooks.

### Most common hooks for module developers

In this section, we will introduce you to a few hooks that are very
handy to write your own custom ejabberd modules.

### `filter_packet` (run_hook)

Filter packet is one of the most useful hook. It is a `run_fold` hook
run by `ejabberd_router` as soon as the packet is routed via
`ejabberd_router:route/3` as follow:

    #!erlang
    ejabberd_hooks:run_fold(filter_packet, {OrigFrom, OrigTo, OrigPacket}, []).

This hook takes as parameter:

* `{OrigFrom, OrigTo, OrigPacket}`: this is the data passed to the
  handler as accumulator. OrigFrom is the From Jabberd ID (`#jid`
  record). OrigTo is the To Jabber ID (`#jid` record) and Packet is
  the XMPP packet in Erlang preparsed format.

* No special extra arguments.

Your handler can return as value:

* `{FromJID, ToJID, Packet}`: You can modify any value in the JID or
  packet. This is why this hook is so powerful. You can for example
  change the text of the messages between two users to replace bad
  word with stars.
* `drop`: this is indended to simply ignore the packet. It iis very
  useful to implement basic blocking features to prevent a user from
  messaging another for example. In that case, you most likely want to
  stop processing, so returning `{stop, drop}` is probably what you
  want.

Note that this hook is run with `ejabberd_hooks:run_fold/3`, not the
usual `ejabberd_hooks:run_fold/4`. The ternary variant doesn't take
the XMPP domain argument and hence it's not possible to register
per-domain handlers for this hook.

### Events list

Here is the list of available events in ejabberd. The types of the
corresponding hooks parameters is described below.

* adhoc_local_items(Acc, From, To, Lang) -> Adhoc
* adhoc_sm_items(Acc, From, To, Lang) -> Adhoc
* anonymous_purge_hook(User, Server) -> ok
* c2s_auth_result(bool(), User, Server, IP) -> ok
* c2s_broadcast_recipients(Acc, Server, StateData, Type, From, Packet) -> []
* c2s_filter_packet(Acc, Server, C2SState, Feature, To, Packet) -> bool()
* c2s_filter_packet_in(Acc, JID, From, To) -> FixedPacket
* c2s_loop_debug({route, From, To, Packet}) -> ok
* c2s_loop_debug(Text) -> ok
* c2s_loop_debug({xmlstreamelement, Packet}) -> ok
* c2s_post_auth_features(Acc, Server) -> []
* c2s_presence_in(Acc, {From, To, Packet}) -> C2SState
* c2s_stream_features(Acc, Server) -> []
* c2s_unauthenticated_iq(Acc, Server, IQ, IP) -> empty \| Packet
* c2s_update_presence(Acc, User, Server) -> Packet
* caps_update(From, To, get_features(Server, Caps)) -> ok
* csi_filter_stanza(Acc, Stanza) -> send
* disco_info(Acc, Host, Module, Node, Lang) -> []
* disco_local_features(Acc, From, To, Node, Lang) -> Disco
* disco_local_identity(Acc, From, To, Node, Lang) -> []
* disco_local_items(Acc, From, To, Node, Lang) -> Disco
* disco_sm_features(Acc, From, To, Node, Lang) -> Disco
* disco_sm_identity(Acc, From, To, Node, Lang) -> []
* disco_sm_items(Acc, From, To, Node, Lang) -> Disco
* filter_packet(Acc) -> OrigPacket
* forbidden_session_hook(JID) -> ok
* http_request_debug({LocalPath, Request}) -> ok
* local_send_to_resource_hook(From, To, Packet) -> ok
* muc_filter_message(Stanza, MUCState, RoomJID, FromJID, FromNick) -> Stanza | drop
* muc_filter_presence(Stanza, MUCState, RoomJID, FromJID, FromNick) -> Stanza | drop
* offline_message_hook(From, To, Packet) -> ok
* presence_probe_hook(From, To, Pid) -> ok
* privacy_check_packet(Acc, User, Server, PrivacyList, {From, To, Packet}, Dir) -> Auth
* privacy_get_user_list(Acc, User, Server) -> #userlist{}
* privacy_iq_get(Acc, From, To, IQ, PrivacyList) -> {result, Packet} \| {error, Error}
* privacy_iq_set(Acc, From, To, IQ) -> {result, Packet} \| {error, Error}
* privacy_updated_list(Acc, PrivacyList, PrivList) -> bool()
* pubsub_create_node(ServerHost, Host, Node, NodeId, NodeOptions) -> ok
* pubsub_delete_node(ServerHost, Host, Node, NodeId) -> ok
* pubsub_publish_item(ServerHost, Node, Publisher, service_jid(Host), ItemId, Payload) -> ok
* register_user(User, Server) -> ok
* remove_user(User, Server) -> ok
* reopen_log_hook() -> ok
* resend_offline_messages_hook(Acc, User, Server) -> []
* resend_subscription_requests_hook(Acc, User, Server) -> []
* roster_get(Acc, {User, Server}) -> []
* roster_get_jid_info(Acc, User, Server, From) -> []}
* roster_get_subscription_lists(Acc, User, Server) -> []}
* roster_get_versioning_feature(Acc, Server) -> []
* roster_groups(Acc, ServerHost) -> []
* roster_in_subscription(Acc, User, Server, From, SubscriptionInType, Reason) -> bool()
* roster_out_subscription(User, Server, To, SubscriptionOutType) -> ok
* roster_process_item(Acc, Server) -> RosterItem
* s2s_allow_host(Acc, Host, Host) -> Auth
* s2s_connect_hook(Host, Server) -> ok
* s2s_loop_debug({xmlstreamelement, Packet}) -> ok
* s2s_receive_packet(From, To, Packet) -> ok
* s2s_send_packet(From, To, Packet) -> ok
* s2s_stream_features(Acc, Server) -> []
* set_presence_hook(User, Server, Resource, Presence) -> ok
* sm_register_connection_hook(SID, JID, Info) -> ok
* sm_remove_connection_hook(SID, JID, Info) -> ok
* unset_presence_hook(User, Server, Resource, Status) -> ok
* user_available_hook(JID) -> ok
* user_ping_timeout(JID) -> ok
* user_receive_packet(Packet, C2SState, JID, From, To) -> Packet
* user_send_packet(Packet, C2SState, From, To) -> Packet
* vcard_set(User, Server, VCARD) -> ok
* webadmin_menu_host(Acc, Host, Lang) -> []
* webadmin_menu_hostnode(Acc, Host, Node, Lang) -> []
* webadmin_user(Acc, User, Server, Lang) -> []
* webadmin_user_parse_query(Acc, Action, User, Server, Query) -> []

## Hooks parameters data types

* To = From = JID = ServerJID = #jid (see jlib.h)
* Packet = Payload = {xmlelement, Name, Attrs, SubEl}
* IQ = #iq (see jlib.h)
* Error = ?STANZA_ERROR/3 (see jlib.h)
* Lang = binary()
* Dir = in \| out
* Auth = allow \| deny
* PrivacyList = OldPrivacyList = NewPrivacyList = none \| #userlist
* CtlStatus = false \| ?STATUS_SUCCESS \| ?STATUS_ERROR \| ?STATUS_USAGE \| ?STATUS_BADRPC (see ejabberd_ctl.hrl)
* Adhoc = {result, I} \| {error, Error} \| empty
* Disco = {result, Items} \| {error, Error}
* Items = Packet
* Arg = [string()]
* Node = [string()]
* ItemID = string()
* Route = {route, From, To, Packet}
* RosterItem = #roster (see mod_roster.hrl)
* Subscription = none \| from \| to \| both \| remove
* SubscriptionInType = subscribe \| unsubscribe
* SubscriptionOutType = subscribed \| unsubscribed
* Reason = binary()
* Groups = [string()]
* SimpleJID = FromSubscription = ToSubscription = {User, Server, Resource}
* User = binary()
* Server = binary()
* Resource = binary()
* Status = binary()
* SID = {Time, pid()}
* Time = {MegaSecs, Secs, MicroSecs} (see erlang:now/0)
* MegaSecs = Secs = MicroSecs = int()
* Acc = same type as the return type of the function

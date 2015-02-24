---
title: Events and Hooks | ejabberd documentation
---

# Events and Hooks

## Introduction

ejabberd provides a flexible way to tie together modular components
through an event system.

Each module can subscribe to events and a hook in the module code is
called when the event occurs.

## Example

The module
[mod_offline.erl](https://github.com/processone/ejabberd/blob/master/src/mod_offline.erl)
is an example of how the events/hooks mechanism can be used.

## Hook API

To subscribe to events ejabberd modules register to hooks using the following API:

    #!erlang
    ejabberd_hooks:add(Hook, Host, Module, Function, Priority)
    ejabberd_hooks:remove(Hook, Host, Module, Function, Priority)

It uses the following Erlang data types:

* Hook = `atom()`
* Host = `string()`
* Module = `atom()`
* Function = `atom()`
* Priority = `integer()`

The `Hook` parameter is the name of the event (see below).

`Host` is the name of the virtual host related to the event.

`Module` and `Function` describe the hook to be called when the event occurs.

`Priority` is the hook rank, to determine in which order the hooks are
run (when several hooks are defined for the same event). You can use
it if you have dependencies between hooks.

## List of events

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
* c2s_unauthenticated_iq(Acc, Server, IQ, IP) -> empty|Packet
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
* filter_packet(Acc) -> OrigPacket}
* forbidden_session_hook(JID) -> ok
* http_request_debug({LocalPath, Request}) -> ok
* local_send_to_resource_hook(From, To, Packet) -> ok
* offline_message_hook(From, To, Packet) -> ok
* presence_probe_hook(From, To, Pid) -> ok
* privacy_check_packet(Acc, User, Server, PrivacyList, {From, To, Packet}, Dir) -> Auth
* privacy_get_user_list(Acc, User, Server) -> #userlist{}
* privacy_iq_get(Acc, From, To, IQ, PrivacyList) -> {result, Packet} | {error, Error}
* privacy_iq_set(Acc, From, To, IQ) -> {result, Packet} | {error, Error}
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
* user_receive_packet(JID, From, To, Packet) -> ok
* user_send_packet(JID, JID, Packet) -> ok
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
* Dir = in | out
* Auth = allow | deny
* PrivacyList = OldPrivacyList = NewPrivacyList = none | #userlist
* CtlStatus = false | ?STATUS_SUCCESS | ?STATUS_ERROR | ?STATUS_USAGE | ?STATUS_BADRPC (see ejabberd_ctl.hrl)
* Adhoc = {result, I} | {error, Error} | empty
* Disco = {result, Items} | {error, Error}
* Items = Packet
* Arg = [string()]
* Node = [string()]
* ItemID = string()
* Route = {route, From, To, Packet}
* RosterItem = #roster (see mod_roster.hrl)
* Subscription = none | from | to | both | remove
* SubscriptionInType = subscribe | unsubscribe
* SubscriptionOutType = subscribed | unsubscribed
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

# Introduction

`ejabberd` is a *free and open source* instant messaging server
written in [`Erlang/OTP`](http://www.erlang.org/).

`ejabberd` is *cross-platform*, distributed, fault-tolerant, and based
on open standards to achieve real-time communication.

`ejabberd` is designed to be a *rock-solid and feature rich* XMPP
server.

`ejabberd` is suitable for small deployments, whether they need to be
*scalable* or not, as well as extremely big deployments.

## Key Features

`ejabberd` is:

- *Cross-platform:* `ejabberd` runs under Microsoft Windows and Unix
  derived systems such as Linux, FreeBSD and NetBSD.

- *Distributed:* You can run `ejabberd` on a cluster of machines and
  all of them will serve the same Jabber domain(s). When you need
  more capacity you can simply add a new cheap node to your cluster.
  Accordingly, you do not need to buy an expensive high-end machine
  to support tens of thousands concurrent users.

- *Fault-tolerant:* You can deploy an `ejabberd` cluster so that all
  the information required for a properly working service will be
  replicated permanently on all nodes. This means that if one of the
  nodes crashes, the others will continue working without disruption.
  In addition, nodes also can be added or replaced ‘on the fly’.

- *Administrator Friendly:* `ejabberd` is built on top of the Open
  Source Erlang. As a result you do not need to install an external
  database, an external web server, amongst others because everything
  is already included, and ready to run out of the box. Other
  administrator benefits include:

    - Comprehensive documentation.

    - Straightforward installers for Linux, Mac OS X, and Windows.

    - Web Administration.

    - Shared Roster Groups.

    - Command line administration tool.

    - Can integrate with existing authentication mechanisms.

    - Capability to send announce messages.

- *Internationalized:* `ejabberd` leads in internationalization. Hence
  it is very well suited in a globalized world. Related features are:

    - Translated to 25 languages.

    - Full Unicode support

    - Support for [`IDNA`](http://tools.ietf.org/html/rfc3490).

- *Open Standards:* `ejabberd` is the first Open Source Jabber server
  claiming to fully comply to the XMPP standard.

    - Fully XMPP compliant.

    - XML-based protocol.

    - [`Many protocols supported`](http://www.ejabberd.im/protocols).

# Additional Features

Moreover, `ejabberd` comes with a wide range of other state-of-the-art
features:

- Modular

    - Load only the modules you want.

    - Extend `ejabberd` with your own custom modules.

- Security

    - SASL and STARTTLS for c2s and s2s connections.

    - STARTTLS and Dialback s2s connections.

    - Web Admin accessible via HTTPS secure access.

    - Password encryption in database using SCRAM algorithm.

- Databases

    - Internal database for fast deployment (Mnesia).

    - Native MySQL support.

    - Native PostgreSQL support.

    - ODBC data storage support.

    - Microsoft SQL Server support.

    - Riak NoSQL database support.

- Authentication

    - Internal Authentication.

    - PAM, LDAP, ODBC and Riak.

    - External Authentication script.

- Others

    - Support for virtual hosting.

    - Compressing XML streams with Stream Compression
      ([`XEP-0138`](http://xmpp.org/extensions/xep-0138.html)).

    - Statistics via Statistics Gathering
      ([`XEP-0039`](http://xmpp.org/extensions/xep-0039.html)).

    - IPv6 support both for c2s and s2s connections.

    - [`Multi-User Chat`](http://xmpp.org/extensions/xep-0045.html)
      module with support for clustering and HTML logging.

    - Users Directory based on users vCards.

    - [`Publish-Subscribe`](http://xmpp.org/extensions/xep-0060.html)
      component with support for
      [`Personal Eventing via Pubsub`](http://xmpp.org/extensions/xep-0163.html).

    - Support for web clients:
      [`HTTP Polling`](http://xmpp.org/extensions/xep-0025.html) and
      [`HTTP Binding (BOSH)`](http://xmpp.org/extensions/xep-0206.html)
      services.

    - IRC transport.

    - SIP support.

    - Component support: interface with networks such as AIM, ICQ and
      MSN installing special tranports.

## How it Works

A XMPP domain is served by one or more `ejabberd` nodes. These nodes
can be run on different machines that are connected via a network. The
set of all ejabberd nodes serving an XMPP domain is call a cluster.

All nodes in the cluster must have the ability to connect to port 4369
of all another nodes, and must have the same _magic cookie_ (see
Erlang/OTP documentation, in other words the file
`~ejabberd/.erlang.cookie` must be the same on all nodes). This is
needed because all nodes exchange information about connected users,
S2S connections, registered services, etc…

Each `ejabberd` node have following core modules available:

- router.

- local router.

- session manager: It is responsible for managing C2S (client-to-server) connection.

- S2S manager: This is the server-to-server component, used for XMPP
  federation. Note that federation can be blocked, if you do not want
  the users on your server to have the ability to talk to users for
  other servers, or use the features provided by other servers.

The core modules are running and are available without any special
addition in the configuration file.

As ejabberd server is highly extensible, most of the remaining
features are provided by modules, that need to be enabled in the
configuration file.

### Router

This module is the main router of XMPP packets on each node. It routes
them based on their destinations domains. It has two tables: local and
global routes. First, domain of packet destination searched in local
table, and if it found, then the packet is routed to appropriate
process. If no, then it searches in global table, and is routed to the
appropriate `ejabberd` node or process. If it does not exists in either
tables, then it sent to the S2S manager.

### Local Router

This module routes packets which have a destination domain equal to this
server name. If destination JID has a non-empty user part, then it
routed to the session manager, else it is processed depending on it’s
content.

### Session Manager

This module routes packets to local users. It searches for what user
resource packet must be sent via presence table. If this resource is
connected to this node, it is routed to C2S process, if it connected via
another node, then the packet is sent to session manager on that node.

### S2S Manager

This module routes packets to other XMPP servers. First, it checks if an
open S2S connection from the domain of the packet source to the domain
of packet destination already exists. If it is open on another node,
then it routes the packet to S2S manager on that node, if it is open on
this node, then it is routed to the process that serves this connection,
and if a connection does not exist, then it is opened and registered.

## Authentication

#### External

The external authentication script follows
[`the erlang port driver API`](http://www.erlang.org/doc/tutorial/c_portdriver.html).

That script is supposed to do theses actions, in an infinite loop:

-   read from stdin: AABBBBBBBBB.....

    -   A: 2 bytes of length data (a short in network byte order)

    -   B: a string of length found in A that contains operation in
        plain text operation are as follows:

        -   auth:User:Server:Password (check if a username/password pair
            is correct)

        -   isuser:User:Server (check if it’s a valid user)

        -   setpass:User:Server:Password (set user’s password)

        -   tryregister:User:Server:Password (try to register an
            account)

        -   removeuser:User:Server (remove this account)

        -   removeuser3:User:Server:Password (remove this account if the
            password is correct)

-   write to stdout: AABB

    -   A: the number 2 (coded as a short, which is bytes length of
        following result)

    -   B: the result code (coded as a short), should be 1 for
        success/valid, or 0 for failure/invalid

Example python script

    #!/usr/bin/python

    import sys
    from struct import *

    def from_ejabberd():
        input_length = sys.stdin.read(2)
        (size,) = unpack('>h', input_length)
        return sys.stdin.read(size).split(':')

    def to_ejabberd(bool):
        answer = 0
        if bool:
            answer = 1
        token = pack('>hh', 2, answer)
        sys.stdout.write(token)
        sys.stdout.flush()

    def auth(username, server, password):
        return True

    def isuser(username, server):
        return True

    def setpass(username, server, password):
        return True

    while True:
        data = from_ejabberd()
        success = False
        if data[0] == "auth":
            success = auth(data[1], data[2], data[3])
        elif data[0] == "isuser":
            success = isuser(data[1], data[2])
        elif data[0] == "setpass":
            success = setpass(data[1], data[2], data[3])
        to_ejabberd(success)

## XML Representation

Each XML stanza is represented as the following tuple:

    XMLElement = {xmlelement, Name, Attrs, [ElementOrCDATA]}
            Name = string()
            Attrs = [Attr]
            Attr = {Key, Val}
            Key = string()
            Val = string()
            ElementOrCDATA = XMLElement | CDATA
            CDATA = {xmlcdata, string()}

E.g. this stanza:

    <message to='test@conference.example.org' type='groupchat'>
      <body>test</body>
    </message>

is represented as the following structure:

    {xmlelement, "message",
        [{"to", "test@conference.example.org"},
         {"type", "groupchat"}],
        [{xmlelement, "body",
             [],
             [{xmlcdata, "test"}]}]}}

## Module `xml`

<span>`element_to_string(El) -> string()`</span>

    El = XMLElement

Returns string representation of XML stanza `El`.

<span>`crypt(S) -> string()`</span>

    S = string()

Returns string which correspond to `S` with encoded XML special
characters.

<span>`remove_cdata(ECList) -> EList`</span>

    ECList = [ElementOrCDATA]
    EList = [XMLElement]

`EList` is a list of all non-CDATA elements of ECList.

<span>`get_path_s(El, Path) -> Res`</span>

    El = XMLElement
    Path = [PathItem]
    PathItem = PathElem | PathAttr | PathCDATA
    PathElem = {elem, Name}
    PathAttr = {attr, Name}
    PathCDATA = cdata
    Name = string()
    Res = string() | XMLElement

If `Path` is empty, then returns `El`. Else sequentially consider
elements of `Path`. Each element is one of:

<span>`{elem, Name}`</span> `Name` is name of subelement of `El`, if
such element exists, then this element considered in following steps,
else returns empty string.

<span>`{attr, Name}`</span> If `El` have attribute `Name`, then returns
value of this attribute, else returns empty string.

<span>`cdata`</span> Returns CDATA of `El`.

<span>TODO:</span>

             get_cdata/1, get_tag_cdata/1
             get_attr/2, get_attr_s/2
             get_tag_attr/2, get_tag_attr_s/2
             get_subtag/2

Module `xml_stream`

<span>`parse_element(Str) -> XMLElement | {error, Err}`</span>

    Str = string()
    Err = term()

Parses `Str` using XML parser, returns either parsed element or error
tuple.

## Modules

### Module gen\_iq\_handler

The module `gen_iq_handler` allows to easily write handlers for IQ
packets of particular XML namespaces that addressed to server or to
users bare JIDs.

In this module the following functions are defined:

<span>`add_iq_handler(Component, Host, NS, Module, Function, Type)`</span>

    Component = Module = Function = atom()
    Host = NS = string()
    Type = no_queue | one_queue | parallel

Registers function `Module:Function` as handler for IQ packets on
virtual host `Host` that contain child of namespace `NS` in `Component`.
Queueing discipline is `Type`. There are at least two components
defined:

<span>`ejabberd_local`</span> Handles packets that addressed to server
JID;

<span>`ejabberd_sm`</span> Handles packets that addressed to users bare
JIDs.

<span>`remove_iq_handler(Component, Host, NS)`</span>

    Component = atom()
    Host = NS = string()

Removes IQ handler on virtual host `Host` for namespace `NS` from
`Component`.

Handler function must have the following type:

<span>`Module:Function(From, To, IQ)`</span>

    From = To = jid()

    -module(mod_cputime).

    -behaviour(gen_mod).

    -export([start/2,
             stop/1,
             process_local_iq/3]).

    -include("ejabberd.hrl").
    -include("jlib.hrl").

    -define(NS_CPUTIME, "ejabberd:cputime").

    start(Host, Opts) ->
        IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
        gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_CPUTIME,
                                      ?MODULE, process_local_iq, IQDisc).

    stop(Host) ->
        gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_CPUTIME).

    process_local_iq(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
        case Type of
            set ->
                {iq, ID, error, XMLNS,
                 [SubEl, ?ERR_NOT_ALLOWED]};
            get ->
                CPUTime = element(1, erlang:statistics(runtime))/1000,
                SCPUTime = lists:flatten(io_lib:format("~.3f", CPUTime)),
                {iq, ID, result, XMLNS,
                 [{xmlelement, "query",
                   [{"xmlns", ?NS_CPUTIME}],
                   [{xmlelement, "cputime", [], [{xmlcdata, SCPUTime}]}]}]}
        end.

### Services

    -module(mod_echo).

    -behaviour(gen_mod).

    -export([start/2, init/1, stop/1]).

    -include("ejabberd.hrl").
    -include("jlib.hrl").

    start(Host, Opts) ->
        MyHost = gen_mod:get_opt(host, Opts, "echo." ++ Host),
        register(gen_mod:get_module_proc(Host, ?PROCNAME),
                 spawn(?MODULE, init, [MyHost])).

    init(Host) ->
        ejabberd_router:register_local_route(Host),
        loop(Host).

    loop(Host) ->
        receive
            {route, From, To, Packet} ->
                ejabberd_router:route(To, From, Packet),
                loop(Host);
            stop ->
                ejabberd_router:unregister_route(Host),
                ok;
            _ ->
                loop(Host)
        end.

    stop(Host) ->
        Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
        Proc ! stop,
        {wait, Proc}.

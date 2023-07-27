---
title: ejabberd developer guide
toc: true
menu: Internals
order: 96
---

# Introduction

`ejabberd` is a *free and open source* instant messaging server
written in [`Erlang/OTP`](https://erlang.org/).

`ejabberd` is *cross-platform*, distributed, fault-tolerant, and based
on open standards to achieve real-time communication.

`ejabberd` is designed to be a *rock-solid and feature rich* XMPP
server.

`ejabberd` is suitable for small deployments, whether they need to be
*scalable* or not, as well as extremely big deployments.

## Goals

This guide is a brief explanation of ejabberd internals. It is not
intended to be a comprehensive ejabberd's internal API documentation.
You still need to read and understand ejabberd's source code. However,
the guide is believed to help you understanding ejabberd's code faster:
it provides entry points from where to start reading relevant parts of
the code and ignore irrelevant ones. Note that there is absolutely no
need to know every line of code of ejabberd, but some parts are crucial
to understand.

## Requirements

In order to read and understand the guide you must be pretty fluent with
Erlang programming language and understand basics of the XMPP protocol:
there is no detailed explanation of Erlang syntax and/or features and
it's assumed that you're familiar with such terms as `xml stream`,
`stanza`, `c2s`, `s2s` and so on. If you see these words for the first
time in your life you're unlikely to understand the guide.

## Version

The guide describes ejabberd 17.03. Previous and next versions can differ
drastically from the one described herein.

# Coding style convention

> **NOTE**: this section is only relevant for ejabberd contributors. If
> you're hacking ejabberd for internal needs, you are free to choose
> whatever coding style you like.

ejabberd follows [Erlang Coding Standards &
Guidelines](https://github.com/inaka/erlang_guidelines/blob/master/README.md)
or at least tries to do so: there is still a lot of poorly written
legacy code (which is being leisurely rewritten), but the new code
should be written with keeping these rules in mind. In some cases the
rules can be bypassed, but the reason doing so should be really
weighty. The rules shouldn't be ignored just because a contributor
doesn't like them.

The typical coding style rules found violated in contributors' code are:

- [100 column per
line](https://github.com/inaka/erlang_guidelines/blob/master/README.md#100-column-per-line):
in fact we have defined 80 columns as a soft and 100 columns as a hard
limit, which means most of your lines should be no longer than 80
characters and the rest must never be longer than 100 characters.
- [no deep
nesting](https://github.com/inaka/erlang_guidelines/blob/master/README.md#avoid-deep-nesting)
- [no boolean parameters in case
control](https://github.com/inaka/erlang_guidelines/blob/master/README.md#avoid-boolean-parameters)
- [only CamelCase variables
name](https://github.com/inaka/erlang_guidelines/blob/master/README.md#variable-names)
- [no
macros](https://github.com/inaka/erlang_guidelines/blob/master/README.md#no-macros)
- [no
case-catch](https://github.com/inaka/erlang_guidelines/blob/master/README.md#dont-use-case-catch)

It's worth noting that the code itself should be indented using Emacs
indentation style (that is the standard indentation style for Erlang
programs). If you're not using Emacs for ejabberd development, indent
the code using it first before making a PR/commit.

# Start-up procedure

ejabberd is written as a standard OTP application, so the startup
module can be found in `src/ejabberd.app.src` or, if ejabberd is
compiled, in `ebin/ejabberd.app` file: that is, `ejabberd_app.erl`
module from where `start/2` function is called by Erlang application
controller. This function makes some initialization (such as logger,
mnesia, configuration file, etc.) and ends up by starting the main
ejabberd supervisor - `ejabberd_sup`. Thus, for further startup order
refer to `ejabberd_sup.erl` module (this is a simple list-like module
with supervisor childspecs).

> **WARNING**: only "core stuff" should be attached to `ejabberd_sup`.
> For attaching modules use gen_mod's supervisor (via
> `gen_mod:start_child/3,4` functions), for attaching database backend
> modules use `ejabberd_backend_sup` supervisor, etc.

Once `ejabberd_sup` is started, ejabberd application is considered to
be started.

# Core

The ejabberd core is not well-defined. Moreover, the described core
layers are pure abstraction grouping several modules together by some
criteria for better understanding of ejabberd internal processing rules.

## Network Layer

Once ejabberd is started, some external events should obviously make it
doing something. Besides explicit administrative commands, the most
relevant such events are incoming connections. Incoming connections are
handled inside `Network Layer`. The layer implemented by
`ejabberd_listener.erl`, `ejabberd_receiver.erl` and
`ejabberd_socket.erl` modules.

> **NOTE**: `ejabberd_listner.erl` is able to handle raw TCP and UDP
> connections, however only XMPP connections are described here.

Once a connection is accepted by `ejabberd_listener.erl`, an instance
(a process) of `ejabberd_receiver.erl` is started and it becomes the
socket owner, where it performs the following operations:

- Throttles a connection using shapers from `shaper.erl` module
- Performs TLS decoding using
[fast_tls](https://github.com/processone/fast_tls) library
- Performs stream decompression using
[ezlib](https://github.com/processone/ezlib) library
- Parses incoming raw XML data into `#xmlel{}` packets using
[fast_xml](https://github.com/processone/fast_xml) library

`ejabberd_socket.erl` does the same but in a reverse order, i.e. it
performs stream compression and/or TLS encoding, serializes `#xmlel{}`
packets into raw XML data and puts them into a socket (note that
shapers do not apply for outgoing data).

Once `xmlel{}` packet is constructed by `ejabberd_receiver.erl` it's
passed to `XMPP Stream Layer`.

## XMPP Stream Layer

`XMPP Stream Layer` is represented by `xmpp_stream_in.erl` and
`xmpp_stream_out.erl` modules. An instance (i.e. a process) of
`xmpp_stream_in.erl` is started along with an instance of
`ejabberd_receiver.erl` and all incoming `#xmlel{}` packets are passed
from the latter to the former. `xmpp_stream_in.erl` module does the
following:

- Encodes/decodes `#xmlel{}` packets using
[xmpp](https://github.com/processone/xmpp) library from/to internal
structures (records) defined in
[xmpp_codec.hrl](https://github.com/processone/xmpp/blob/master/include/xmpp_codec.hrl).
- Performs negotiation of _inbound_ XMPP streams
- Performs STARTTLS negotiation (if needed)
- Performs compression negotiation (if needed)
- Performs SASL authentication

> **NOTE**: `XMPP Stream Layer` was only introduced in ejabberd 17.03.
> Prior to this XMPP stream negotiation was handled inside `ejabberd_c2s.erl`,
> `ejabberd_s2s_in.erl`, `ejabberd_service.erl` and `ejabberd_s2s_out.erl`.
> This has lead to unmaintainable monolithic spaghetti code with a lot
> of code duplication between these modules. It's believed introducing
> `xmpp_stream_in.erl` and `xmpp_stream_out.erl` modules now solves this
> problem.

During these procedures `xmpp_stream_in.erl` calls functions from its
callback modules, i.e. the modules of `xmpp_stream_in` behaviour:
`ejabberd_c2s.erl`, `ejabberd_s2s_in.erl` or `ejabberd_service.erl`,
depending on the stream namespace.

`xmpp_stream_out.erl` does the same but for _outbound_ XMPP streams.
The only its callback module is `ejabberd_s2s_out.erl`.

> **NOTE**: `xmpp_stream_in.erl` shares the same process and state with
> its callback modules, i.e. functions from `xmpp_stream_in.erl` and
> functions from `ejabberd_c2s/s2s_in/service.erl` modules are
> evaluated inside the same process. This is also true for
> `xmpp_stream_out.erl` and `ejabberd_s2s_out.erl`. The state is
> represented by a `map()` in both cases.

### ejabberd_c2s, ejabberd_s2s_in and ejabberd_service

These are modules of `xmpp_stream_in` behaviour. The only purpose of
these modules is to provide callback functions for `xmpp_stream_in.erl`
module. Examples of such callback functions are:

- `tls_enabled/1`: tells whether or not TLS is enabled in the configuration
- `check_password_fun/1`: provides a function for SASL authentication
- `handle_authenticated_packet/2`: what to do with packets after
authentication is completed

Roughly, they represent an intermediate (or "glue") code between
`XMPP Stream Layer` and `Routing Layer` for inbound XMPP streams.

`ejabberd_s2s_out.erl` is described [elsewhere](#ejabberd_s2s-and-ejabberd_s2s_out)

## Routing Layer

### ejabberd_router

`ejabberd_router.erl` module is the main dispatcher of XMPP stanzas.

It's pretty small and straightforward module whose the only task is to
find the "route" for a stanza. `ejabberd_router.erl` only operates with
`#message{}`, `#presence{}` and `#iq{}` packets (defined in
[xmpp_codec.hrl](https://github.com/processone/xmpp/blob/master/include/xmpp_codec.hrl)),
so please note, that it is not possible to route arbitrary `#xmlel{}`
packets or any other Erlang terms through `ejabberd_router`.

The only valid routes are:

- _local_ route: stanzas of this route type are destined to the local
server itself, i.e. stanzas with `to` attribute in the form of
`domain.com` or `domain.com/resource`, where `domain.com` is a virtual
host serviced by ejabberd. `ejabberd_router` passes such stanzas to
`ejabberd_local.erl` module via `ejabberd_local:route/1` function call.
- _session manager_ route: stanzas of this route type are destined to
local users, i.e. stanzas with `to` attribute in the form of
`user@domain.com` or `user@domain.com/resource` where `domain.com` is a
virtual host serviced by ejabberd. `ejabberd_router` passes such
stanzas to `ejabberd_sm.erl` module via `ejabberd_sm:route/1` function
call.
- _registered_ route: if a stanza is not destined to local virtual
host, ejabberd first checks if there is a "registered" route for the
stanza, i.e. a domain registered via `ejabberd_router:register_route/2`
function. For doing this it looks up the routing table and if there is
a process `Pid` registered on this domain, ejabberd routes the stanza
as `Pid ! {route, Stanza}`. The routing table is backend-dependent and
is implemented in the corresponding backend module such as
`ejabberd_router_mnesia.erl`.
- _s2s_ route: if a stanza is neither destined to local virtual host
nor to registered route, `ejabberd_router` passes it to
`ejabberd_s2s.erl` module via `ejabberd_s2s:route/1` function call.

Mentioned modules are explained in more details in the following
sections. You're encouraged to inspect exported functions of
`ejabberd_router.erl`, because most likely you will use some of them.

### ejabberd_local

`ejabberd_local.erl` handles stanzas destined to the local server
itself. For `#message{}` and `#presence{}` it only calls hooks, while
for `#iq{}` it finds the corresponding "IQ handler" by looking up its
internal table to find a correspondence between a namespace of IQ's
child element and the handler. Once the handler (an erlang function) is
found, it passes further IQ processing to `gen_iq_handler.erl` via
`gen_iq_handler:handle/5` call.

`ejabberd_local.erl` is also able to send IQ requests and to process
responses for them. This is implemented in `ejabberd_local:route_iq/2,3`
functions. This is also the most notable function of the module.
Calling to other functions is not recommended.

### ejabberd_sm

`ejabberd_sm.erl` handles stanzas destined to local users. For
`#message{}`, `#presence{}` and full-JID `#iq{}` it looks up its
internal table (aka `session` table) for the corresponding
`ejabberd_c2s` process and, if the process is found, it routes the
stanza to this process via `ejabberd_c2s:route/2` call.

Bare-JID `#iq{}` stanzas are processed in a similar way as in
`ejabberd_local.erl`. The internal `session` table is backend-dependent
and is implemented in the corresponding backend module:
`ejabberd_sm_mnesia.erl`, `ejabberd_sm_redis.erl` and so on.

The most notable functions of the module are:

- `get_user_resources/2`
- `dirty_get_sessions_list/0`
- `dirty_get_my_sessions_list/0`
- `get_vh_session_list/1`
- `get_vh_session_number/1`
- `get_vh_by_backend/1`
- `get_session_pid/3`
- `get_user_info/2`
- `get_user_info/3`
- `get_user_ip/3`
- `is_existing_resource/3`

### route-registered processes

Any process can register a route to itself. It's done by calling to
`ejabberd_router:route/2` function. Note that a route should be
unregistered via `ejabberd_router:unregister_route/1` function if the
registering process terminates or the route is no longer needed. Once a
route is registered to a process, this process will receive Erlang
messages in the form of `{route, Stanza}`.

> **NOTE**: `from` and `to` fields are always set in the `Stanza`,
> so it's safe to assume that `xmpp:get_from(Stanza)` and
> `xmpp:get_to(Stanza)` always return `#jid{}` and never `undefined`.

Refer to the code of `mod_muc.erl` or `ejabberd_service.erl` for an
example of a route-registered process.

### ejabberd_s2s and ejabberd_s2s_out

If a stanza is destined neither to local virtual host not to a
route-registered process, it's passed to `ejabberd_s2s.erl` module via
`ejabberd_s2s:route/1` function call. `ejabberd_s2s` in its turn will
look up the internal table (currently it's `s2s` Mnesia table) for the
`ejabberd_s2s_out` process and, if found, passes the stanza to this
process or, otherwise, will start new `ejabberd_s2s_out` process.

`ejabberd_s2s_out.erl` handles _outbound_ XMPP S2S streams. This is the
only callback module of `xmpp_stream_out` behaviour.

# Adding new functionality

There are two common ways to add new functionality to ejabberd:

- using [IQ Handlers](#iq-handlers)
- using [hooks](#hooks)

Here is a rule of thumb on which way to choose:

- if you want to handle newly introduced IQs (that is, to generate
replies for them), use [IQ handlers](#iq-handlers)
- if you want to modify ejabberd behaviour along the way of a stanza
passing through all layers or want to "listen" for some internal events
(like ejabberd configuration change), use [hooks](#hooks).

## IQ Handlers

An `IQ Handler` is a function processing an IQ stanza (internally
represented as `#iq{}` record). There are two types of IQ handlers:
`local` and `sm`.
- `local` IQ handler is a function processing IQs coming from
`ejabberd_local`, that is, an IQ destined to the local server itself as
described in [ejabberd_local](#ejabberd-local).
- `sm` IQ handler is a function processing IQs coming from
`ejabberd_sm`, that is, a bare-JID IQ destined to a local user as
described in [ejabberd_sm](#ejabberd-sm).

An IQ handler is registered as:
```erlang
gen_iq_handler:add_iq_handler(Type :: ejabberd_local | ejabberd_sm,
                              Host :: binary(),
                              Namespace :: binary(),
                              Module :: module(),
                              Function :: atom(),
                              IQDisc :: gen_iq_handler:type()) -> ok
```
where:
- `Type` is `ejabberd_local` for `local` handlers or `ejabberd_sm` for
`sm` handlers
- `Host` is a virtual host for which the IQ is to be processed
- `Namespace` is an XML namespace of IQ's child element
- `IQDisc` is an [IQ discipline](#iq-discipline)

Once registered, matching `IQ` stanzas are handled by calling
`Module:Function(IQ)`. The result should be in the form of `#iq{}` or
`ignore`. When `#iq{}` is returned, it's treated as a reply and routed
back to the IQ originator, otherwise, if `ignore` is returned, the
further processing stops.

> **NOTE**: `from` and `to` fields are always set in the `IQ`,
> so it's safe to assume that `xmpp:get_from(IQ)` and
> `xmpp:get_to(IQ)` always return `#jid{}` and never `undefined`.

If a handler is no longer needed it should be unregistered as:
```erlang
gen_iq_handler:remove_iq_handler(Type :: ejabberd_local | ejabberd_sm,
                                 Host :: binary(),
                                 Namespace :: binary()) -> ok
```
with the same meaning of the arguments.

### IQ discipline

An `IQ discipline` defines how an IQ handler (a function) will be
executed. There are the following disciplines:
- `no_queue`: all IQs matching the handler are executed sequentially
inside the calling process. This is a fast method to execute a handler,
however, the drawback is that it blocks the caller.
- `one_queue`: a process is created for the handler and all matching
IQs are relayed to this process. The drawback is that the created
process' message queue can be overloaded if it doesn't process incoming
IQs fast enough, which, in the worst case may crash emulator due to OOM
(out-of-memory).
- `N :: integer()`: `N` parallel processes are created for the handler
and all matching IQs are relayed to one of these processes. The
processes are picked up _randomly_. This solves the "message queue
overflow" problem of `one_queue` discipline. However, the drawback is
that the order of IQs is not maintained, i.e. matching IQs can be
re-ordered (because one process can be slower than another) and this
might be a problem in some cases. Care should be taken on choosing too
large value for `N` because picking up a process from the pool has
`O(N)` complexity. Anyway, in practice, seems like there is no much
benefit to set `N` larger than `50`.
- `parallel`: for every matching IQ a process is created to execute the
handler. This discipline is not recommended because uncontrolled
processes creation is in general a bad idea.

## Hooks

When ejabberd is processing an arbitrary event (incoming IQ, outgoing
presence, configuration change, etc), it is convenient to consider some
of them notable. In order for someone to be notified of such events,
ejabberd executes "hooks". A hook is represented by a unique name. All
functions associated with the hook's name will be called in some
specified order.

> **NOTE**: The conception of hooking is not ejabberd specific, see
> [Hooking](https://en.wikipedia.org/wiki/Hooking) Wikipedia page for a
> general description.

For example, when a packet is received on a client connection, ejabberd
runs `user_send_packet` hook. Several modules need to listen for an
event represented by this hook (that is, a packet and a C2S state),
so they associate their internal functions with it: `mod_ping.erl`
associates `user_send/1` function, `mod_privacy.erl` associates
`user_send_packet/1` function and so on. The event is passed as an
argument to the "hooked" functions, thus, the function from
`mod_ping.erl` will be called as `mod_ping:user_send({Stanza,
C2SState})`, the function from `mod_privacy.erl` will be called as
`mod_privacy:user_send_packet({Stanza, C2SState})` and so on.

There are two types of hooks: _with_ an accumulator and _without_ an
accumulator.

- a hook with an accumulator, as its name suggests, accumulates some
state during execution of a list of associated functions: the first
argument of the hooked function will always be an accumulator and the
function must return the new value for the accumulator (whether it's
modified or not) in the form of `NewAcc` or `{stop, NewAcc}`. If `{stop, NewAcc}`
is returned, a hook is considered evaluated and next functions
in its associated list are not called. Otherwise, the new value
`NewAcc` is passed to the next function in the associated list. An
example of hooks with accumulator are: `disco_info`, `filter_packet`,
`muc_process_iq` and so on.
- a hook without accumulator doesn't accumulate anything during
execution of a list of associated functions: the returning values of
such functions are simply ignored unless `stop` is returned. In the
latter case, evaluation of next functions in the associated list is not
performed. An example of hooks without accumulator are:
`config_reloaded`, `component_init` and so on.

Both types of hooks have _local_ or _global_ scope.

- a hook with local scope is associated with particular virtual host
and is run only when an event is matching this host. Most of the hooks
have local scope.
- a hook with global scope is not associated with any virtual host and
is run for an event matching any hosts. A very few hooks have global
scope.

A function gets associated with a local hook as follows (the type of a
hook doesn't matter):
```erlang
ejabberd_hooks:add(Hook :: atom(),
                   Host :: binary(),
                   Module :: module(),
                   Function :: atom(),
                   Seq :: integer() -> ok
```
where:

- `Hook` is a hook name
- `Host` is a virtual host
- `Seq` is a sequence number. This number defines position of the
function in the list to maintain execution order. Functions with lower
sequence number are executed _before_ those with bigger sequence
number. For functions with the same sequence number the order is
unspecified. A function associated with an accumulating hook is called
as `Module:Function(Acc, Arg1, Arg2, ...)` where `Acc` is an
accumulator value, `Arg1`, `Arg2`, ... - arguments of the hook. Recall
that such function must return a new accumulator value (whether it's
modified or not) in the form of `NewAcc` or `{stop, NewAcc}` where
`NewAcc` is the new accumulator value. A function associated with a
hook without an accumulator is called as `Module:Function(Arg1,
Arg2, ...)`. All returning values except `stop` are ignored.

> **WARNING**: a `Function` with the corresponding arity should be
> exported by a `Module`

A function for a global hook gets associated as follows (the type of a
hook doesn't matter):
```erlang
ejabberd_hooks:add(Hook :: atom(),
                   Module :: module(),
                   Function :: atom(),
                   Seq :: integer()) -> ok
```
with the same meaning of the arguments. Note that `Host` argument is
omitted in this case.

For any types of hooks, if an association is no longer needed, it can
be deleted by calling `ejabberd_hooks:delete/5,6` functions with
exactly the same arguments used to create an association.

In some cases a new hook should be introduced. There is no need to
explicitly register the new hook, one only needs to run a hook in the
required place. The following functions can be used for this:

- for local hooks with accumulator: `ejabberd_hooks:run_fold(Hook,
Host, Acc, Args)`. The function returns a new accumulator value.
- for local hooks without accumulator: `ejabberd_hooks:run(Hook, Host,
Args)`. The function always returns `ok`.
- for global hooks with accumulator: `ejabberd_hooks:run_fold(Hook,
Acc, Args)`. The function returns a new accumulator value.
- for global hooks without accumulator: `ejabbed_hooks:run(Hook,
Args)`. The function always returns `ok`.

where `Args` is a list of arguments (other variables have the same
meaning as above).

There is a helper script that you can use to
check hook correctness and find mishooked functions. The script also
generates a module `src/hooks_type_test.erl` from where you can learn
about existing hooks and check execution order. You can place your code
inside `src` directory (if any), and run:
```
$ make hooks
```

# Modules

## gen_mod behaviour

As you might know, ejabberd is a modular software. The best method to
add new functionality to it is to write a new module. For doing this
one should create an Erlang module of `gen_mod` behaviour:
```erlang
%% file mod_foo.erl
-module(mod_foo).
...
-behaviour(gen_mod).
...
```
Several callbacks should be defined in the module:

- `Module:start(Host, Opts)` where `Host` is a virtual host where the
module is about to start and `Opts` is an option list (typically
defined in the `modules` section of `ejabberd.yml`). The function is
executed when a module is being started. It is intended to initialize a
module. This is a good place to register hooks and IQ handlers, as well
as to create an initial state of a module (if needed). The function
should return either `ok` or `{ok, pid()}`.
- `Module:stop(Host)` where `Host` is a virtual host. The function is
executed when a module is being stopped. It is intended to make some
module cleanup: most likely unregistering hooks and IQ handlers. The
returning value is ignored
- `Module:reload(Host, NewOpts, OldOpts)` where `NewOpts` and `OldOpts`
is the new and old options list respectively. The function is called
every time a module is being reloaded. This is the only optional callback,
thus, if undefined, the module will be reloaded by calling sequentially
`Module:stop/1` and `Module:start/2`.
- `Module:depends(Host, Opts)` where the meaning of the arguments is
the same. The function is called to build modules dependencies on
startup. The function must return a list of type `[{module(), DependencyType}]`,
where `DependencyType` is one of `hard` or `soft`.
The `hard` dependency means the module is non-functional if the other
module is not loaded. The `soft` dependency means the module has
suboptimal functionality if the other module is not loaded.
- `Module:mod_opt_type(Option)`. The function is used to process
configuration options of `Module`. The function has the same meaning
as `Module:opt_type/1` callback described in
[Configuration validation](#validation) section.

## Stateful modules

While some modules don't need to maintain an internal state
("stateless" modules), others are required to do this ("stateful"
modules). The common practice is to implement a stateful module as a
`gen_server` process. There is a couple of helpers to deal with such
modules:

- `gen_mod:start_child(Module, Host, Opts)` where `Module` is a name of
a stateful module. This function should be called as the last function
inside of `Module:start/2`. It will create a `gen_server` process with
a registered name and will attach it to `ejabberd_gen_mod_sup` supervisor.
- `gen_mod:stop_child(Module, Host)` should be used inside of
`Module:stop/1` function and will terminate the corresponding
registered `gen_server` process.
- `gen_mod:get_module_proc(Host, Module)` can be used to obtain a
registered name of a stateful module (i.e. its `gen_server`'s name).

> **WARNING**: don't forget to set `process_flag(trap_exit, true)`
> inside `Module:init/1` callback function, otherwise,
> `Module:terminate/2` callback will never be called when a module is
> being stopped.

> **WARNING**: keeping module's configuration options in an internal
> state is not recommended. Use `gen_mod:get_module_opt/4,5` functions
> to retrieve the options: in this case you don't need to re-initialize
> options in the state inside `Module:reload/3` callback.

If a stateful module is intended to maintain a state in the form of a
table, `ETS` can be used for this. In this case there is no need to
implement it as a `gen_server` process. But make sure you're not
calling `ets:new/2` several times for several virtual hosts (`badarg`
will be raised in this case). E.g., the following code is
*incorrect*:
```erlang
start(Host, Opts) ->
    ...
    ets:new(some_table, named_table, ...]),
    ...
```
The correct code will look something like that:
```erlang
start(Host, Opts) ->
    ...
    try ets:new(some_table, [named_table, ...])
    catch _:badarg -> ok end,
    ...
```
There is a plenty of examples of modules: pick up any file starting
with `mod_` inside `src` directory.

## gen_mod module

Module `gen_mod.erl` has various useful functions to work with modules,
the most notable are:

- `is_loaded/2`: whether or not the module in question is loaded at
a given virtual host
- `get_opt/3,4`: gets a value of an option from module's options list
(see description of `ejabberd_config:get_option/3` function from
[Fetching configuration options](#fetching-options) for details)
- `get_module_opt/4,5`: the same as above, but an option is referenced
by a virtual host and a module.

# Configuration

ejabberd has quite powerful configuration processor -
`ejabberd_config.erl`. It performs configuration file parsing and
validation.

## Validation

In order to validate options `ejabberd_config` has to install feedback
with the rest of the code. For doing this, it provides
`ejabberd_config` behaviour with a single callback function:
`Module:opt_type/1`. The callback accepts an option name as an `atom()`
and must return either validating function if an option is known for
the `Module` or a list of available options (as a list of atoms). A
validating function is a `fun()` of a single argument - the value of
the option. The validating function must return any new value for the
option (whether it's modified or not) or should crash if the value
doesn't match expected format. Here is an example:

```erlang
%% file: some.erl
-module(some).
-behaviour(ejabberd_config).
-export([opt_type/1]).
...
opt_type(max_connections_number) ->
    %% max_connections_number should be non-negative integer
    %% if the condition is satisfied, return this integer
    %% fail with function_clause otherwise
    fun(I) when is_integer(I), I>=0 -> I end;
opt_type(_) ->
    %% only max_connections_number is known
    [max_connections_number].
```

> **NOTE**: `gen_mod` behaviour defines a very similar callback -
> `Module:mod_opt_type/1` with the same meaning of arguments and
> returning values, except the callback is called to validate the
> `Module`'s specific options (i.e. options defined in the
> corresponding subsection of the `modules` section of a configuration
> file).

## Fetching options

The most notable function of the module is:
```erlang
get_option(Option :: atom() | {atom(), binary() | global},
           ValidatingFun :: fun(),
           Default :: term()) -> Value :: term().
```
The function is used to get a value `Value` of a configuration option
`Option`. The `ValidatingFun` is a validating function described in
the previous section and `Default` is the default value if the option
is not defined in the config.

# Using XMPP library

## xmpp module

Prior to version 16.12, ejabberd used to operate with `#xmlel{}`
packets directly: [fast_xml](https://github.com/processone/fast_xml)
API functions have been used for manipulating with `#xmlel{}` packets
(such as `fast_xml:get_subtag/2`, `fast_xml:get_attr_s/2`,
`fast_xml:get_path_s/2` and so on) as well as some functions from
`jlib.erl` module.

This is now deprecated and actually not possible. Instead, the new API
functions are used from brand new [xmpp](https://github.com/processone/xmpp)
library.

> **NOTE**: although direct calling of `fast_xml` API is deprecated,
> there are still two useful functions: `fxml_stream:parse_element/1` and
> `fxml:element_to_binary/1`. You can use these functions for
> (de)serialization of data stored on disc or in a database.

The library is built on top of `XMPP Codec`: a number of
decoding/encoding modules automatically generated by [Fast XML
generator](https://github.com/processone/fast_xml/blob/master/src/fxml_gen.erl)
from the specification file
[xmpp_codec.spec](https://github.com/processone/xmpp/blob/master/specs/xmpp_codec.spec).
The goal is to avoid manual processing of XML trees and, instead, using
well-typed auto-generated structures defined in
[xmpp_codec.hrl](https://github.com/processone/xmpp/blob/master/include/xmpp.hrl).
Every particular XML packet within some namespace has to have a
specification defined in `xmpp_codec.spec`. The advantage of such
approach is that you tell the generator *what* to parse instead of
taming `fast_xml` library *how* to parse.

> **NOTE**: describing how to write XMPP codec specification is out of
> scope of this guide

> **WARNING**: you should never use functions from `xmpp_codec.erl` module
> directly: use functions from `xmpp.erl` module. The same is true for
> header files: do **NOT** include `xmpp_codec.hrl` -- include `xmpp.hrl` instead

### XMPP codec

Once a raw XML packet is parsed by `ejabberd_receiver.erl` into `#xmlel{}` record,
it's passed to `xmpp_stream_in.erl` module, where decoding of `#xmlel{}` into
`xmpp_element()` format (i.e. into well-known record type defined in `xmpp_codec.hrl`)
is performed (refer to [XMPP Stream Layer](#xmpp-stream-layer) section for details).
At that level "lazy" decoding is applied: only top-level element is decoded.
For example, an `xmlel()` packet
```erlang
#xmlel{name = <<"message">>,
       attrs = [{<<"type">>,<<"chat">>}],
       children = [#xmlel{name = <<"composing">>,
                          attrs = [{<<"xmlns">>,
                                    <<"http://jabber.org/protocol/chatstates">>}],
                          children = []}]}
```
is decoded into the following `xmpp_element()`:
```erlang
#message{id = <<>>,type = chat,lang = <<>>,from = undefined,
         to = undefined,subject = [],body = [],thread = undefined,
         sub_els = [#xmlel{name = <<"composing">>,
                           attrs = [{<<"xmlns">>,
                                     <<"http://jabber.org/protocol/chatstates">>}],
                           children = []}],
         meta =
```
Note that the sub-element is still in `xmlel()` format. This "semi-decoded" packet
is then passed upstream (at the [Routing Layer](#routing-layer)). Thus, a programmer
should explicitly decode sub-elements if needed. To accomplish this one can use the
following function:
```erlang
xmpp:decode(El :: xmlel(), Namespace :: binary(), [Option]) -> xmpp_element()`
```
where the only supported `Option` is `ignore_els`: with this option lazy decoding
is performed. By default, full decoding is applied, i.e. all known sub-elements get
decoded. `Namespace` is a "top-level" namespace: it should be provided only if
`<<"xmlns">>` attribute is omitted in `El`, otherwise decoding would fail (see below).

There is also `xmpp:decode(El :: xmlel()) -> xmpp_element()` function, which is
a short-hand for `xmpp:decode(El, ?NS_CLIENT, [])` (where `?NS_CLIENT` is a
predefined namespace for `<<"jabber:client">>`, see [Namespaces](#namespaces)
section).

Both functions might **fail** with `{xmpp_codec, Why}` exception. The value of `Why`
can be used to format the failure reason into human readable description using
`xmpp:format_error/1` function, e.g., using sub-element from example `#message{}`
above, we can write:
```erlang
try xmpp:decode(El) of
    #chatstate{} = ChatState -> process_chatstate(ChatState)
catch _:{xmpp_codec, Why} ->
    Text = xmpp:format_error(Why),
    ?ERROR_MSG("failed to decode element: ~s", [Txt])
end
```
To apply reverse operation use `xmpp:encode/2` functions:
```erlang
xmpp:encode(Pkt :: xmpp_element(), Namespace :: binary()) -> El :: xmlel()
```
There is also
`xmpp:encode(Pkt :: xmpp_element()) -> El :: xmlel()` function which
is a short-hand for `xmpp:encode(Pkt, <<>>)`.

`Namespace` is a "top-level" namespace: it is used to tell the codec whether
to include `<<"xmlns">>` attribute into resulting `#xmlel{}` element or not --
if the `Pkt` is within the same `Namespace`, `<<"xmlns">>` attribute will be
omitted in the result. For example:
```erlang
> rr(xmpp).
...
> Msg.
#message{id = <<>>,type = chat,lang = <<>>,from = undefined,
         to = undefined,subject = [],body = [],thread = undefined,
         sub_els = [#chatstate{type = composing}],
         meta =
> xmpp:encode(Msg).
#xmlel{name = <<"message">>,
       attrs = [{<<"type">>,<<"chat">>},
                {<<"xmlns">>,<<"jabber:client">>}],
       children = [#xmlel{name = <<"composing">>,
                          attrs = [{<<"xmlns">>,
                                    <<"http://jabber.org/protocol/chatstates">>}],
                          children = []}]}
> xmpp:encode(Msg, <<"jabber:client">>).
#xmlel{name = <<"message">>,
       attrs = [{<<"type">>,<<"chat">>}],
       children = [#xmlel{name = <<"composing">>,
                          attrs = [{<<"xmlns">>,
                                    <<"http://jabber.org/protocol/chatstates">>}],
                          children = []}]}
```
> **NOTE**: `xmpp:encode/1,2` functions would never fail as long as the
> provided input is a valid `xmpp_element()` with valid values of its record
> fields. Use dialyzer checks of your code for validation.

> **NOTE**: there is no need to explicitly decode a sub-element of an IQ
> passed into an [IQ handler](#iq-handlers) because decoding is performed
> inside `gen_iq_handler.erl` module and a handler actually will never
> receive malformed sub-elements.

Luckily, there is a helper function for sub-elements decoding, described
in the next section and in a lot of cases it's more convenient to use it.

### Getting sub-elements

Once a programmer gets a stanza in `xmpp_element()` format, (s)he might
want to get its subelement. To accomplish this the following function
can be used:
```erlang
xmpp:get_subtag(Stanza :: stanza(), Tag :: xmpp_element()) -> Pkt :: xmpp_element() | false
```
This function finds a `Tag` by its well-known record inside sub-elements of
the `Stanza`. It automatically performs decoding (if needed) and returns either
found `xmpp_element()` or `false` if no elements have matched. Note that
the function doesn't fail if some of sub-elements are invalid.

Example:
```erlang
> rr(xmpp).
...
> Msg.
#message{id = <<>>,type = chat,lang = <<>>,from = undefined,
         to = undefined,subject = [],body = [],thread = undefined,
         sub_els = [#xmlel{name = <<"composing">>,
                           attrs = [{<<"xmlns">>,
                                     <<"http://jabber.org/protocol/chatstates">>}],
                           children = []}],
         meta =
> xmpp:get_subtag(Msg, #chatstate{type = composing}).
#chatstate{type = composing}
> xmpp:get_subtag(Msg, #chatstate{type = inactive}).
false
> xmpp:get_subtag(Msg, #disco_info{}).
false
```

### Setting and removing sub-elements

In order to inject a sub-element into or delete one from arbitrary
`stanza()` one can use `xmpp:set_subtag/2` and `xmpp:remove_subtag/2`
respectively.

### from and to

Every `stanza()` element has `from` and `to` record fields. In order to
get/set them one can manipulate with these record fields directly, e.g.
via `Msg#message.from` or `Pres#presence.to` expressions, or, use
`xmpp:get_from/1`, `xmpp:get_to/1`, `xmpp:set_from/2`, `xmpp:set_to/2`
and `xmpp:set_from_to/3` functions, depending on which approach is
more convenient in the current situation.

> **NOTE**: although in general `from` and `to` fields may have `undefined`
> values, these fields are always filled with correct `#jid{}` records at
> [XMPP Stream Layer](#xmpp-stream-layer), thus, it is safe to assume
> that the fields always possess valid `#jid{}` values.

### Metadata

Every `stanza()` element has `meta` field represented as a `map()`.
It's useful when there is a need to attach some metadata to the stanza
before routing it further.
A programmer can manipulate with this field directly using
[maps](https://erlang.org/doc/man/maps.html) module,
or use `xmpp:get_meta/1,2,3`, `xmpp:set_meta/2`, `xmpp:put_meta/3`,
`xmpp:update_meta/3` and `xmpp:del_meta/2` functions, which is almost
always more convenient (except pattern matching).

### Text elements

Some `xmpp_element()`s has fields defined in `[#text{}]` format. The example
is `#message.body` and `#presence.status` fields. To avoid writing a lot
of extracting code the following functions can be used: `xmpp:mk_text/1,2` to
convert some binary text written in some language into `[#text{}]` term,
or `xmpp:get_text/1,2` to extract binary text from the `[#text{}]` element
by a language.

### Generating errors

In order to generate stanza errors or stream errors `xmpp:err_/0,2`
or `xmpp:serr_*/0,2` can be used respectively, such as
`xmpp:err_service_unavailable()` or `xmpp:serr_not_authorized()`.
If a stanza should be bounced back with an error, `xmpp:make_error/2`
function can be used

### Namespaces

There are many predefined macros for XML namespaces in
[ns.hrl](https://github.com/processone/xmpp/blob/master/include/ns.hrl).
However, this file must **NOT** be included, as it's already included
in `xmpp.hrl`.

A function `xmpp:get_ns/1` can be used to retrieve a namespace
from `xmpp_element()` or from `xmlel()` directly:
```erlang
> rr(xmpp).
...
> xmpp:get_ns(#message{}).
<<"jabber:client">>.
> xmpp:get_ns(xmpp:encode(#presence{})).
<<"jabber:client">>.
```

## jid module

`jid.erl` module provides functions to work with XMPP addresses (aka "JIDs").
There are two common types of internal representation of JIDs:

- `jid()`: a JID is represented by a record `#jid{}` defined in
  [jid.hrl](https://github.com/processone/xmpp/blob/master/include/jid.hrl)
- `ljid()`: a JID is represented by a tuple `{User, Server, Resource}` where
  `User`, `Server` and `Resource` are stringprepped version of a nodepart,
  namepart and resourcepart of a JID respectively. This representation is useful to
  use for JIDs comparison and when a JID should be used as a key (in a Mnesia
  database, ETS table, etc.)

The most notable functions in this module are:

- `decode(Input :: binary()) -> jid()`:
  decodes binary data into `jid()`. Fails with `{bad_jid, Input}` otherwise.
- `encode(JID :: jid() | ljid()) -> binary()`:
  encodes `JID` into binary data
- `remove_resource(JID :: jid() | ljid()) -> jid() | ljid()`:
  removes resource part of a `JID`
- `replace_resource(JID :: jid() | ljid(), Resource :: binary()) -> jid() | ljid()`:
  replaces resource part of a `JID`
- `tolower(JID :: jid() | ljid()) -> ljid()`:
  transforms `JID` into `ljid()` representation
- `make(LJID :: ljid() | jid()) -> jid()`:
  transforms `LJID` into `jid()` representation

Inspect exported functions of
[jid.erl](https://github.com/processone/xmpp/blob/master/src/jid.erl)
for more details.

# External Authentication

You can configure ejabberd to use as authentication method an external script,
as described in the Administrator section:
[External Script](/admin/configuration/authentication/#external-script).

Let's see the interface between ejabberd and your script,
and several example scripts.
There are also several old [example scripts](https://ejabberd.im/extauth).

## Extauth Interface

The external authentication script follows the
[Erlang port driver API](https://erlang.org/doc/tutorial/c_portdriver.html).

That script is supposed to do these actions, in an infinite loop:

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

As you noticed, the `:` character is used to separate the fields.
This is possible because the User and Server fields can't contain the `:` character;
and Password can have that character, but is always the last field.
So it is always possible to parse the input characters unambiguously.

## Perl Example Script

This is a simple example Perl script;
for example if the file is copied to the path `/etc/ejabberd/check_pass_null.pl`
then configure ejabberd like this:

```yaml
auth_method: [external]
extauth_program: /etc/ejabberd/check_pass_null.pl
```

Content of `check_pass_null.pl`:

```perl
#!/usr/bin/perl

use Unix::Syslog qw(:macros :subs);

my $domain = $ARGV[0] || "example.com";

while(1)
  {
   # my $rin = '',$rout;
   # vec($rin,fileno(STDIN),1) = 1;
   # $ein = $rin;
   # my $nfound = select($rout=$rin,undef,undef,undef);

    my $buf = "";
    syslog LOG_INFO,"waiting for packet";
    my $nread = sysread STDIN,$buf,2;
    do { syslog LOG_INFO,"port closed"; exit; } unless $nread == 2;
    my $len = unpack "n",$buf;
    my $nread = sysread STDIN,$buf,$len;

    my ($op,$user,$host,$password) = split /:/,$buf;
    #$user =~ s/\./\//og;
    my $jid = "$user\@$domain";
    my $result;

    syslog(LOG_INFO,"request (%s)", $op);

  SWITCH:
      {
	$op eq 'auth' and do
	  {
             $result = 1;
	  },last SWITCH;

	$op eq 'setpass' and do
	  {
             $result = 1;
	  },last SWITCH;

        $op eq 'isuser' and do
          {
             # password is null. Return 1 if the user $user\@$domain exitst.
             $result = 1;
          },last SWITCH;

        $op eq 'tryregister' and do
          {
             $result = 1;
          },last SWITCH;

        $op eq 'removeuser' and do
          {
             # password is null. Return 1 if the user $user\@$domain exitst.
             $result = 1;
          },last SWITCH;

        $op eq 'removeuser3' and do
          {
             $result = 1;
          },last SWITCH;
      };
    my $out = pack "nn",2,$result ? 1 : 0;
    syswrite STDOUT,$out;
  }

closelog;
```

## Python Example Script

Example Python script:

    #!/usr/bin/python

    import sys
    import struct

    def read_from_stdin(bytes):
      if hasattr(sys.stdin, 'buffer'):
        return sys.stdin.buffer.read(bytes)
      else:
        return sys.stdin.read(bytes)

    def read():
        (pkt_size,) = struct.unpack('>H', read_from_stdin(2))
        pkt = sys.stdin.read(pkt_size)
        cmd = pkt.split(':')[0]
        if cmd == 'auth':
            u, s, p = pkt.split(':', 3)[1:]
            if u == "wrong":
                write(False)
            else:
                write(True)
        elif cmd == 'isuser':
            u, s = pkt.split(':', 2)[1:]
            if u == "wrong":
                write(False)
            else:
                write(True)
        elif cmd == 'setpass':
            u, s, p = pkt.split(':', 3)[1:]
            write(True)
        elif cmd == 'tryregister':
            u, s, p = pkt.split(':', 3)[1:]
            write(True)
        elif cmd == 'removeuser':
            u, s = pkt.split(':', 2)[1:]
            write(True)
        elif cmd == 'removeuser3':
            u, s, p = pkt.split(':', 3)[1:]
            write(True)
        else:
            write(False)
        read()

    def write(result):
        if result:
            sys.stdout.write('\x00\x02\x00\x01')
        else:
            sys.stdout.write('\x00\x02\x00\x00')
        sys.stdout.flush()

    if __name__ == "__main__":
        try:
            read()
        except struct.error:
            pass


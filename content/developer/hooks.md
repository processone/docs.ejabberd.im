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

---
title: Runtime Error Messages
toc: true
---

# \*\* Too many db tables \*\*

## Error

When running ejabberd, the log shows this error:

    ** Too many db tables **

## Explanation

The number of concurrent ETS and Mnesia tables is limited. If this
error occurs, it means that you have reached this limit.

## Solution

Read the
[section about ERL_MAX_ETS_TABLES on the Performance Tuning page](https://ejabberd.im/tuning#erl_max_ets_tables).

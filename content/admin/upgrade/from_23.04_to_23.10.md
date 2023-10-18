---
title: Upgrade to ejabberd 23.10
toc: true
---

There is a new module, several new options, an API command has changed,
but there are no breaking changes in the configuration, SQL schema.

The `get_roster` API command has a different output, please check the release announcement for details.

The MUC room option `allow_private_messages` is converted to `allowpm`. This conversion is automatic, you don't need to perform any task. However, once you update to ejabberd 23.10, the stored rooms options will be converted, and you should not attempt to use that content with ejabberd versions older than 23.10.

`gen_mod` API is simplified. If you write your own ejabberd modules, you can optionally use that new API.

In summary, you can update from previous ejabberd version to this one without performing any upgrade tasks.

Please check the ejabberd [23.10](/archive/23_10/) release announcement for details about the improvements.


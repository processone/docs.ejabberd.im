---
title: ejabberd operation FAQ | ejabberd documentation
---

# Frequently Asked Questions

## Multi-User Chat

### How to list all MUC room on a server

Thanks to contributed module 'mod_muc_admin', you can get a better control on the rooms on your server:

1. Install
   '[mod_muc_admin](https://github.com/processone/ejabberd-contrib/tree/master/mod_muc_admin)'
   module from the 'ejabberd-contrib' repository
2. Run the following command:

    ```
    ejabberdctl muc_online_rooms global
    ```


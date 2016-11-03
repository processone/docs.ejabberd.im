---
title: Simple ejabberd Rest API Configuration
menu: Simple Configuration
order: 10
---

# Basic ejabberd configuration with OAuth authentication

Before being able to interact with ejabberd API, you need to configure
ejabberd with OAuth support enabled. This is is documented in
[ejabberd OAuth support](https://docs.ejabberd.im/admin/guide/oauth/).

Here are example entries to check / change in your ejabberd
configuration file:

1. Add a listener for OAuth and ReST API:

    ``` yaml
    listen:
      -
        # Using a separate port for oauth and API to make it easy to protect it
        # differently than BOSH and Websocket HTTP interface.
        port: 5281
        # oauth and API only listen on localhost interface for security reason
        # You can set ip to "0.0.0.0" to open it widely, but be careful!
        ip: "127.0.0.1"
        module: ejabberd_http
        request_handlers:
          "/oauth": ejabberd_oauth
          "/api": mod_http_api
    ```

2. You can then configure the OAuth commands you want to expose.
   Check `commands_admin_access` to make sure ACLs for passing commands as
   admin are set properly:

      ``` yaml
      commands_admin_access:
        - allow:
          - user: "admin@localhost"
      commands:
        - add_commands: [user, admin, open]
      # Tokens are valid for a year as default:
      auth_expire: 31536000
      oauth_access: all
      ```

3. Finally, make sure the modules, you need to use the command from
   are enabled, for example:

    ```yaml
    modules:
      mod_admin_extra: {}
    ```


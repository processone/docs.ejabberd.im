---
title: Simple ejabberd Rest API Configuration
menu: Simple Configuration
order: 10
toc: true
---

# Using ejabberd API from localhost, without authentication

If you are planning to use ejabberd API for admin purpose, it is often enough to configure it to be available local commands.
Access is thus generally limited by IP addresses, either restricted to localhost only, or restricted to one of your platform back-end.

Configuring the API that way is very easy. All you need is:

1. Make sure an HTTP handler is configure to expose ejabberd `mod_http_api` on a given root URL and on a desired port:

    ``` yaml
    listen:
      -
        port: 5281
        module: ejabberd_http
        request_handlers:
          "/api": mod_http_api
     ```
     
     You can optionally pass the ip parameter to the listener to make it listen only on the local interface (127.0.0.1) instead of listening on all interface (0.0.0.0):

    ``` yaml
    listen:
      -
        port: 5281
        ip: "127.0.0.1"
      ...
    ```
2. By defining `api_permissions`, you can then allow HTTP request is from a specific IP to trigger API commands execution without user credentials:

    ``` yaml
    api_permissions:
      "API used from localhost allows all calls":
        - who:
          - ip: "127.0.0.1/8"
        - what:
          - "*"
          - "!stop"
          - "!start"
    ```

    _Note: stop and start commands are disabled in that example as they are usually restricted to ejabberdctl command-line tool. They are consider too sensitive to be exposed through API._

With that setup you can call ejabberd commands through API. For a list of available commands, you can check the [Administration API](/developer/ejabberd-api/admin-api/) page.

<!--
TODO:

  - Link to page showing how to configure TLS on listeners
  - Link to page showing how to use Go ejabberd API client

-->

# Basic ejabberd configuration with OAuth authentication

Before being able to interact with ejabberd API, you need to configure
ejabberd with OAuth support enabled. This is is documented in
[ejabberd OAuth support](/admin/guide/oauth/).

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


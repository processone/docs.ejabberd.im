---
title: Simple ejabberd Rest API Configuration
menu: Simple Config
order: 0
toc: true
---

<!--
TODO:

  - Link to page showing how to configure TLS on listeners
  - Link to page showing how to use Go ejabberd API client

-->

# Restrict to Local network

If you are planning to use ejabberd API for admin purpose, it is often enough to configure it to be available local commands.
Access is thus generally limited by IP addresses, either restricted to localhost only, or restricted to one of your platform back-end.

1. Make sure an [ejabberd_http](/admin/configuration/listen/#ejabberd-http)
listener is using [mod_http_api](/admin/configuration/modules/#mod-http-api)
 on a given root URL and on a desired port:

    ``` yaml
    listen:
      -
        port: 5281
        module: ejabberd_http
        ip: 127.0.0.1
        request_handlers:
          /api: mod_http_api
     ```

     The `ip` option ensures it listens only on the local interface (127.0.0.1) instead of listening on all interface (0.0.0.0).

2. By defining `api_permissions`, you can then allow HTTP request from a specific IP to trigger API commands execution without user credentials:

    ``` yaml
    api_permissions:
      "API used from localhost allows all calls":
        who:
          ip: 127.0.0.1/8
        what:
          - "*"
          - "!stop"
          - "!start"
    ```

    _Note: stop and start commands are disabled in that example as they are usually restricted to ejabberdctl command-line tool. They are consider too sensitive to be exposed through API._

3. Now you can query the API, for example:

    ``` bash
    curl '127.0.0.1:5281/api/registered_users?host=localhost'

    ["user2","user8"]
    ```

# Encryption

If you already defined certificates and your connection is not on a local network,
you may want to use encryption.

1. Setup encryption like this:

    ``` yaml
    listen:
      -
        port: 5281
        module: ejabberd_http
        tls: true
        request_handlers:
          /api: mod_http_api
    ```

2. Now you can query using HTTPS:

    ``` bash
    curl 'https://127.0.0.1:5281/api/registered_users?host=localhost'

    ["user2","user8"]
    ```

3. If you are using a self-signed certificate, you can bypass the corresponding error message:

    ``` bash
    curl --insecure 'https://127.0.0.1:5281/api/registered_users?host=localhost'

    ["user2","user8"]
    ```

# Basic Authentication

Quite probably you will want to require authentication to execute API queries,
either using basic auth or OAuth.

1. Assuming you have the simple listener:

    ``` yaml
    listen:
      -
        port: 5281
        module: ejabberd_http
        ip: 127.0.0.1
        request_handlers:
          /api: mod_http_api
     ```

2. Define an ACL with the account that you will use to authenticate:

    ``` yaml
    acl:
      apicommands:
        user: john@localhost
    ```

3. Allow only that ACL to use the API:

    ``` yaml
    api_permissions:
      "some playing":
        from:
          - ejabberd_ctl
          - mod_http_api
        who:
          acl: apicommands
        what: "*"
    ```

4. If that account does not yet exist,
   [register it](/admin/installation/#administration-account):

    ``` bash
    ejabberdctl register john localhost somePass
    ```

5. Now, when sending an API query, provide the authentication for that account:

    ``` bash
    curl --basic --user john@localhost:somePass \
         '127.0.0.1:5281/api/registered_users?host=localhost'

    ["user2","user8","john"]
    ```

# OAuth Authentication

Before using OAuth to interact with ejabberd API,
you need to configure [OAuth support in ejabberd](/admin/guide/oauth/).

Here are example entries to check / change in your ejabberd
configuration file:

1. Add a request handler for OAuth:

    ``` yaml
    listen:
      -
        # Using a separate port for oauth and API to make it easy to protect it
        # differently than BOSH and Websocket HTTP interface.
        port: 5281
        # oauth and API only listen on localhost interface for security reason
        # You can set ip to 0.0.0.0 to open it widely, but be careful!
        ip: 127.0.0.1
        module: ejabberd_http
        request_handlers:
          /api: mod_http_api
          /oauth: ejabberd_oauth
    ```

2. Set the [oauth_access](/admin/configuration/toplevel/#oauth-access)
   top-level option to allow token creation:

    ``` yaml
    oauth_access: all
    ```

3. Define an ACL with the account that you will use to authenticate:

    ``` yaml
    acl:
      apicommands:
        user: john@localhost
    ```

4. You can then configure the OAuth commands you want to expose and who can use them:

    ``` yaml
    api_permissions:
      "admin access":
        who:
          oauth:
            scope: "ejabberd:admin"
            scope: "registered_users"
            access:
              allow:
                acl: apicommands
    ```

5. If that account does not yet exist,
   [register it](/admin/installation/#administration-account):

    ``` bash
    ejabberdctl register john localhost somePass
    ```

6. Request an authorization token. A quick way is using ejabberdctl:
    ``` bash
    ejabberdctl oauth_issue_token user123@localhost 3600 ejabberd:admin
    erHymcBiT2r0QsuOpDjIrsEvnOS4grkj   [<<"ejabberd:admin">>]   3600 seconds
    ```

7. Now, when sending an API query, provide the authentication for that account:

    ``` yaml
    curl -H "Authorization: Bearer erHymcBiT2r0QsuOpDjIrsEvnOS4grkj" \
         '127.0.0.1:5281/api/registered_users?host=localhost'

    ["user2","user8","john"]
    ```

    Or quite simply:

    ``` yaml
    curl --oauth2-bearer erHymcBiT2r0QsuOpDjIrsEvnOS4grkj \
         '127.0.0.1:5281/api/registered_users?host=localhost'

    ["user2","user8","john"]
    ```


---
title: OAuth Support
toc: true
menu: OAuth Support
order: 15
---

# Introduction

ejabberd includes a full support OAuth 2.0 deep inside the ejabberd
stack.

This OAuth integration makes ejabberd:

- an ideal project to develop XMPP applications with Web in mind, as
  it exposes ejabberd features as ReST or XML-RPC HTTP based API
  endpoints. **OAuth makes ejabberd the ideal XMPP server to integrate in a
  larger Web / HTTP ecosystem**.

- a more **secure tool that can leverage the use of oAuth token to
  authenticate, hiding your real password from the client itself**. As
  your password is never shared with client directly with our X-OAUTH2
  authentication mechanism, user have less risks of having their
  primary password leaked.

- a tool that can be used at the core of larger platforms as oauth
  token can be used by users and admins to **delegate rights to
  subcomponents / subservices**.

- a tool that is friendly to other online services as users can
  delegate rights to others SaaS platform they are using. This will
  be possible to let services access your message archive, show your
  offline message count or with future commands send message to users
  and chatrooms on your behalf. This is done in a granular way, with a
  scope limited to a specific function. And the delegation rights for
  a specific app / third party can always be revoked at any time as
  this is usually the case with OAuth services.

You can read more on OAuth from [OAuth website](https://oauth.net).

OAuth support is available in ejabberd 15.09 and newest releases.

# Configuration

## Authentication method

An X-OAUTH2 SASL mechanism is available as default for authentication
in ejabberd.

If `ejabberd_oauth` HTTP request handlers is not enabled, there is no
way to generate token from outside ejabberd. However, you can still
explicitly disabled X-OAUTH2 with the
[disable_sasl_mechanisms](/admin/configuration/toplevel/#disable-sasl-mechanisms)
top-level option in `ejabberd.yml` file, either at global or at virtual host
level:

    
    disable_sasl_mechanisms: ["X-OAUTH2"]

## ejabberd listeners

To enable OAuth support in ejabberd, you need to edit your
`ejabberd.yml` file to add the following snippets.

You first need to expose more HTTP endpoint in
[ejabberd_http](/admin/configuration/listen/#ejabberd-http) modules:

- `ejabberd_oauth` is the request handler that will allow
       generating token for third-parties (clients, services). It is
       usually exposed on "/oauth" endpoint. This handler is mandatory
       to support OAuth.
- [mod_http_api](/admin/configuration/modules/#mod-http-api) request handler
       enables ReST API
       endpoint to perform delegated actions on ejabberd using an HTTP
       JSON API. This handler is usually exposed on "/api"
       endpoint. It is optional.

- [ejabberd_xmlrpc](/admin/configuration/listen/#ejabberd-xmlrpc) listener
  can be set on a separate port to query commands using the XML-RPC protocol.

Here is a example of the `listen` section in ejabberd configuration
file, focusing on HTTP handlers:

    
    listen:
      -
        port: 4560
        module: ejabberd_http
        request_handlers:
          ## Handle ejabberd commands using XML-RPC
          /: ejabberd_xmlrpc
      -
        port: 5280
        module: ejabberd_http
        request_handlers:
          /websocket: ejabberd_http_ws
          /log: mod_log_http
          # OAuth support:
          /oauth: ejabberd_oauth
          # ReST API:
          /api: mod_http_api

## Module configuration

Some commands are implemented by ejabberd internals and are always available,
but other commands are implemented by optional modules.
If the documentation of a command you want to use mentions a module,
make sure you have enabled that module in `ejabberd.yml`.
For example the
[add_rosteritem](/developer/ejabberd-api/admin-api/#add-rosteritem)
command is implemented in the mod_admin_extra module.

By the way, ejabberd implements several commands to manage OAuth,
check the [oauth tag](/developer/ejabberd-api/admin-tags/#oauth) documentation.

## OAuth specific parameters

OAuth is configured using those top-level options:

- [oauth_access](/admin/configuration/toplevel/#oauth-access)
- [oauth_cache_life_time](/admin/configuration/toplevel/#oauth-cache-life-time)
- [oauth_cache_missed](/admin/configuration/toplevel/#oauth-cache-missed)
- [oauth_cache_rest_failure_life_time](/admin/configuration/toplevel/#oauth-cache-rest-failure-life_time)
- [oauth_cache_size](/admin/configuration/toplevel/#oauth-cache-size)
- [oauth_client_id_check](/admin/configuration/toplevel/#oauth-client-id-check)
- [oauth_db_type](/admin/configuration/toplevel/#oauth-db-type)
- [oauth_expire](/admin/configuration/toplevel/#oauth-expire)
- [oauth_use_cache](/admin/configuration/toplevel/#oauth-use-cache)

In this example, tokens expire after an hour, and all users can create tokens:

    
    oauth_expire: 3600
    oauth_access: all

<!--
## Database / Back-ends for OAuth tokens

Currently, OAuth tokens are stored in Mnesia database. In a future
release, we plan to support multiple token backends.
-->

# authorization_token

An easy way to generate a token is using the
[oauth_issue_token](/developer/ejabberd-api/admin-api/#oauth-issue-token)
command with the ejabberdctl shell script:

``` bash
ejabberdctl oauth_issue_token user123@localhost 3600 ejabberd:admin

erHymcBiT2r0QsuOpDjIrsEvnOS4grkj  [<<"ejabberd:admin">>]  3600 seconds
```

But probably you want your users to be able to generate tokens themselves.

<!--
Open this page in a web browser:
    http://localhost:5281/oauth/authorization_token?response_type=token&client_id=Client1&redirect_uri=http://client.uri&scope=get_roster+sasl_auth

In that page, provide as User (jid): john@localhost and its password somePass

You will get redirected to an URL similar to:
    http://client.uri/?access_token=HtP7SpKAidA3SeOzSUOmhrPt25mgzH3c&token_type=bearer&expires_in=31536000&scope=get_roster%20sasl_auth&state=

Take note of the access_token value, as this will be used later.
-->


To generate a token you can have the user open the
`/oauth/authorization_token` in a webview in your application or in a
browser.

For example, URL can be:

    http://example.net:5280/oauth/authorization_token?response_type=token&client_id=Client1&redirect_uri=http://client.uri&scope=get_roster+sasl_auth

**Note::** To use `get_roster` scope, you need to have
`mod_admin_extra` enabled. Otherwise, the command is unknown and you
will get an invalid_scope error.

Parameters are described in OAuth 2.0 specification:

- `response_type`: Should be `token`.
- `client_id`: This is the name of the application that is asking for Oauth token.
- `scope`: This is the scope of the rights being delegated to the
  application. It will limit the feature the application can perform
  and thus ensure the user is not giving away more right than expected
  by the application. As a developer, you should always limit the
  scope to what you actually need.
- `redirect_uri`: After token is generated, token is passed to the
  application using the redirect URI. It can obviously work for web
  applications, but also for mobile applications, using a redirect URI
  that the mobile application have registered: Proper code for
  handling the token will thus be executed directly in the mobile
  application.
- `state`: State parameter is optional and use by client to pass
  information that will be passed as well as state parameter in the
  redirect URI.

Directing the user to this URL will present an authentication form
summarizing what is the app requiring the token and the scope / rights
that are going to be granted.

The user can then put their login and password to confirm that they
accept granting delegating rights and confirm the token generation.
If the provided credentials are valid, the browser or webview will
redirect the user to the redirect_uri, to actually let ejabberd pass
the token to the app that requested it. It can be either a Web app or
`a mobile / desktop application.

# redirect_uri

The `redirect_uri` originally passed in the authorization_token
request will be called on successfull validation of user credentials,
with added parameters.

For example, redirect URI called by ejabberd can be:

    http://client.uri/?access_token=RHIT8DoudzOctdzBhYL9bYvXz28xQ4Oj&token_type=bearer&expires_in=3600&scope=user_get_roster+sasl_auth&state=

Parameters are described in OAuth specification:

- `access_token`: This is the actual token that the client
  application can use for OAuth authentication.
- `token_type`: ejabberd supports `bearer` token type.
- `expires_in`: This is the validity duration of the token, in
  seconds. When the token expires, a new authorization token will need
  to be generated an approved by the user.

<!--- TODO: Does oauth2 allow token refresh ? Is it implemented or could
  it be implemented in ejabberd ? -->
- `scope`: Confirms the granted scope to the requesting
  application. Several scopes can be passed, separated by '+'.
- `state`: If a state parameter was passed by requesting application
  in authorization_token URL, it will be passed back to the
  application as a parameter of the `redirect_uri` to help with the
  client workflow.

<!--- TODO: Add Android and iOS examples on how to get the token.  -->

# Scopes

- `sasl_auth`: This scope is use to generate a token that can login
  over XMPP using SASL X-OAUTH2 mechanism.
- `ejabberd:admin`
- `ejabberd:user`
- And one scope for each existing [API command](/developer/ejabberd-api/admin-api/).
  For example, there is a scope `registered_users`
  because there is a command called `registered_users`.

<!--- TODO: As scope are generally provided by commands, which are
provided by enabled modules, we need to command to list enabled
scopes.  We should also probably support scopes that are categories of
commands (like user for example). Finally, we should probably allow
getting scope as a command tag, to grant access to all MUC commands
for example. The authorize_token form should list the commands that
will be enabled by the scope at token generation time. -->

# X-OAuth2 Authentication

You can connect to ejabberd using an X-OAUTH2 token that is valid in
the scope `sasl_auth`. You can use an OAuth token as generated in the
previous steps instead of a password when connecting to ejabberd
servers support OAuth SASL mechanism.

When enabled, X-OAUTH2 SASL mechanism is advertised in server stream
features:

    
    <stream:features>
      <c xmlns="http://jabber.org/protocol/caps" node="http://www.process-one.net/en/ejabberd/" ver="nM19M+JK0ZBMXK7iJAvKnmDuQus=" hash="sha-1"/>
      <register xmlns="http://jabber.org/features/iq-register"/>
      <mechanisms xmlns="urn:ietf:params:xml:ns:xmpp-sasl">
        <mechanism>PLAIN</mechanism>
        <mechanism>DIGEST-MD5</mechanism>
        <mechanism>X-OAUTH2</mechanism>
        <mechanism>SCRAM-SHA-1</mechanism>
      </mechanisms>
    </stream:features>

Authentication with X-OAUTH2 is done by modifying the SASL auth
element as follow:

    
    <auth mechanism='X-OAUTH2'
          xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>
      base64("\0" + user_name + "\0" + oauth_token)
    </auth>

The content in the auth element should be the base64 encoding of a
string containing a null byte, followed by the user name, another null
byte and the string representation of the user’s OAuth token. This is
similar to how to authenticate with a password using the PLAIN
mechanism, except the token is added instead of the user’s password.

The response is standard for SASL XMPP authentication. For example, on
success, server will reply with:

    
    <success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>

# ReST / XML-RPC API

It is possible to use OAuth to authenticate a client when
attempting to perform a ReST or XML-RPC query.

## Passing credentials

When using ReST, the client authorization is done by using a bearer token
(no need to pass the user and host parameters).
For that, include an Authorization HTTP header like:

    
    Authorization: Bearer Qi4CyTCDtqpUNW3fnRSZLb0OG3XOOjvx


For XML-RPC, credentials must be passed as XML-RPC parameters,
including token but also user and host parameters. This is for legacy
reason, but will likely change in a future version, making user and
host implicit, thanks to bearer token.

<!--
## Acting as an admin

With both HTTP remote call mechanisms, you can either act as a user or
act as an admin (See previous reference about access rules).

To act as an admin from a ReST API call, the HTTP request must contain
the following header:

    
    X-Admin: true

To act as an admin from an XML-RPC query, the XML-RPC query must contain:

    
    {admin,true}
-->

## ReST JSON example

With a command like [get_roster](/developer/ejabberd-api/admin-api/#get-roster)
you can get your own roster, or act as an admin to get any user roster.

The HTTP endpoint does not take any parameter, so we can just do an
HTTP post with empty JSON structure list (see `-d` option):

    
    curl --oauth2-bearer erHymcBiT2r0QsuOpDjIrsEvnOS4grkj \
         '127.0.0.1:5281/api/registered_users?host=localhost'

<!--
For admin queries, add "X-Admin: true" header:

    
    curl -v -X POST -H "X-Admin: true" -H "Authorization: Bearer Qi4CyTCDtqpUNW3fnRSZLb0OG3XOOjvx" http://localhost:5280/api/get_roster -d '{"user": "test10", "server": "localhost"}'
-->

<!--- There is - - oauth2-bearer curl option, which probably should add
      that authorization header, but it does nothing in my curl version -->

## XML-RPC examples

With a command like [get_roster](/developer/ejabberd-api/admin-api/#get-roster)
you can get your own roster, or act as an admin to get any user roster.

Here is an (Erlang) XML-RPC example on how to get your own roster:

    
    xmlrpc:call({127, 0, 0, 1}, 4560, "/",
      {call, get_roster, [
        {struct, [{user, "peter"},
                  {server, "example.com"},
                  {token, "0n6LaEjyAOxVDyZChzZfoKMYxc8uUk6L"}]}]},
      false, 60000, "Host: localhost\r\n", []).

This will lead to sending this XML-RPC payload to server:

    
    <?xml version="1.0" encoding="UTF-8"?>
    <methodCall>
      <methodName>get_roster</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>server</name>
                <value>
                  <string>example.com</string>
                </value>
              </member>
              <member>
                <name>user</name>
                <value>
                  <string>peter</string>
                </value>
              </member>
              <member>
                <name>token</name>
                <value>
                  <string>0n6LaEjyAOxVDyZChzZfoKMYxc8uUk6L</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>

To get roster of other user using admin authorization, this erlang
XML-RPC code can be used:

    
    xmlrpc:call({127, 0, 0, 1}, 4560, "/",
      {call, get_roster, [
        {struct, [{user, "admin"},
                  {server, "example.com"},
                  {token, "0n6LaEjyAOxVDyZChzZfoKMYxc8uUk6L"}
                  {admin, true}]},
        {struct, [{user, "peter"},
                  {server, "example.com"}]}]},
      false, 60000, "Host: localhost\r\n", []).

This is an equivalent Python 2 script:

    
    import xmlrpclib

    server_url = 'http://127.0.0.1:4560'
    server = xmlrpclib.ServerProxy(server_url)

    LOGIN = {'user': 'admin',
             'server': 'example.com',
             'token': '0n6LaEjyAOxVDyZChzZfoKMYxc8uUk6L',
             'admin': True}

    def calling(command, data):
	fn = getattr(server, command)
	return fn(LOGIN, data)

    print calling('get_roster', {'user':'peter', 'server':'example.com'})

And this is an equivalent Python 3 script:

    
    from xmlrpc import client

    server_url = 'http://127.0.0.1:4560'
    server = client.ServerProxy(server_url)

    LOGIN = {'user': 'admin',
             'server': 'example.com',
             'token': '0n6LaEjyAOxVDyZChzZfoKMYxc8uUk6L',
             'admin': True}

    def calling(command, data):
	fn = getattr(server, command)
	return fn(LOGIN, data)

    result = calling('get_roster', {'user':'peter', 'server':'example.com'})
    print(result)

Those calls would send this XML to server:

    
    <?xml version="1.0" encoding="UTF-8"?>
    <methodCall>
      <methodName>get_roster</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>admin</name>
                <value>
                  <boolean>1</boolean>
                </value>
              </member>
              <member>
                <name>server</name>
                <value>
                  <string>example.com</string>
                </value>
              </member>
              <member>
                <name>user</name>
                <value>
                  <string>admin</string>
                </value>
              </member>
              <member>
                <name>token</name>
                <value>
                  <string>0n6LaEjyAOxVDyZChzZfoKMYxc8uUk6L</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>peter</string>
                </value>
              </member>
              <member>
                <name>server</name>
                <value>
                  <string>example.com</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>

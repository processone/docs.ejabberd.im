# OAuth support

## Introduction

ejabberd includes a full support OAuth 2.0 deep inside the ejabberd
stack.

This OAuth integration makes ejabberd:

- an ideal project to develop XMPP application with web in mind, as it
  exposes ejabberd features as ReST or XML-RPC HTTP based API
  endpoints.

- a more secure tool that can leverage the use of oAuth token to
  authenticate, hiding your real password from the client itself. As
  your password is never shared with client directly with our X-OAUTH2
  authentication mechanism, user have less risks of having their
  primary password leaked.

- a tool that can be used at the core of larger platforms as oauth
  token can be used by users and admins to delegate rights to
  subcomponents / subservices.

- a tool that is friendly to other online services as users can
  delegate rights to others SaaS platoform they are using. This will
  be possible to let services access your message archive, show your
  offline message count or with future commands send message to users
  and chatrooms on your behalf. This is done in a granular way, with a
  scope limited to a specific function. And the delegation rights for
  a specific app / third partye can always be revoked at any time as
  this is usually the case with OAuth services.

You can read more on OAuth from [OAuth website](http://oauth.net).

OAuth support is available in ejabberd 15.09 and newest release.

## Configuration

### Authentication method

An X-OAUTH2 SASL mechanism is available as default for authentication
in ejabberd.

You can check availability in the SASL mechanisms advertised by
ejabberd:

```
  <mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>
    <mechanism>PLAIN</mechanism>
    <mechanism>X-OAUTH2</mechanism>
    <mechanism>SCRAM-SHA-1</mechanism>
  </mechanisms>
```

If `ejabberd_oauth` HTTP request handlers is not enabled, there is no
way to generate token from outside ejabberd. However, you can still
explicitely disabled X-OAUTH2 with the `disable_sasl_mechanisms`
option in `ejabberd.yml` file, either at global or at virtual host
level:

    disable_sasl_mechanisms: ["X-OAUTH2"]

### ejabberd listeners

To enable OAuth support in ejabberd, you need to edit your
`ejabberd.yml` file to add the following snippets.

1. You first need to expose more HTTP endpoint in `ejabberd_http` modules.

    1. `ejabberd_oauth` is the request handler that will allow
       generating token for third-parties (clients, services). It is
       usually exposed on "/oauth" endpoint. This handler is mandatory
       to support OAuth.
    2. `mod_http_api` is the request handler that enable ReST API
       endpoint to perform delegated actions on ejabberd using an HTTP
       JSON API. This handler is usually exposed on "/api"
       endpoint. It is optional.

If you want to support commands using the XML-RPC protocol, you can add
`ejabberd_xmlrpc` as a specific listener on a seperate port.

Here is a example of the `listen` section in ejabberd configuration
file, focusing on HTTP handlers:

```
listen:
  ## To handle ejabberd commands using XML-RPC
  - 
    port: 4560
    module: ejabberd_xmlrpc
  - 
    port: 5280
    module: ejabberd_http
    request_handlers:
      "/websocket": ejabberd_http_ws
      "/log": mod_log_http
      # OAuth support:
      "/oauth": ejabberd_oauth
      # ReST API:
      "/api": mod_http_api
    web_admin: true
    http_bind: true
    captcha: true
    
... other listeners
```

### Module configuration

Make sure you have enabled module supporting OAuth enabled commands.

For now the only module supporting OAuth is `mod_oauth_test`.

<!--- TODO Do we want to commit it / package it for demonstration
      purpose ? -->

### OAuth specific parameters

Here are the following available parameters to tweak the behaviour of
OAuth and the available commands:

- ** commands_admin_access**: This option will reference an ejabberd
  access rule that will define the user that will be able to use
  commands that are defined as admin only.
- **commands**: This option is used to define the list of commands we
  want to enable through a remote mechanism (ReST or XML-RPC). This is
  a list of `add_commands` and `remove_commands` options. You can
  either use a complete categories of commands or a list of commands.
  The `add_commands` and `remove_commands` directives are executed in
  order and you can have several of them to control the precise list
  of commands you want to expose through OAuth.
  Possible categories of commands are:
  
  - *open*: no auth required for that command. Typically, for generic
    service information related commands.
  - *admin*: Only an admin user can use the method.
  - *user*: User can use the method to access server level information
    (like browse MUC room list as user) or there own data. Admin can
    still use it to access any user data.
  - *restricted*: Such admin command is not exposed over internet and
    is only available through local secure tool like `ejabberdctl`
    command-line tool.
- **oauth_expire**: Time during which the token is valid, in
  seconds. After that amount of time, the token expires and the
  delegated credential cannot be used and is removed from the
  database.

Here is an example, for OAuth specific parameters configuration:

```
commands_admin_access: configure 
commands:
  - add_commands: user
oauth_expire: 3600
```

In the previous case, tokens expires after an hour. All commands in
categorie `user` are exposed. Admin access is granted to users that
can pass the `configure` access rule defined in the config file.

### Database / Back-ends for OAuth tokens

Currently, OAuth tokens are stored in Mnesia database. In a future
release, we plan to support multiple token

## Using ejabberd OAuth API from your applications

### authorization_token: Generating OAuth token

To generate a token you can have the user open the `/oauth/authorization_token` in a webview in your application or in a browser.

For example, URL can be:

    http://example.net:5280/oauth/authorization_token?response_type=token&client_id=Client1&redirect_uri=http://client.uri&scope=user_get_roster

Parameters are described in OAuth 2.0 specification:

- **response_type**: Should be `token`.
- **client_id**: This is the name of the application that is asking for Oauth token.
- **scope**: This is the scope of the rights being delegated to the
  application. It will limit the feature the application can perform
  and thus ensure the user is not giving away more right than expected
  by the application. As a developer, you should always limit the
  scope to what you actually need.
- **redirect_uri**: After token is generated, token is passed to the
  application using the redirect URI. It can obviously work for web
  applications, but also for mobile applications, using a redirect URI
  that the mobile application have registered: Proper code for
  handling the token will thus be executed directly in the mobile
  application.
- **state**: State parameter is optional and use by client to pass
  information that will be passed as well as state parameter in the
  redirect URI.

Directing the user to this URL will present an authentication form
summarizing what is the app requiring the token and the scope / rights
that are going to be granted.

The user can then put his login and password to confirm that he
accepts granting delegating rights and confirms the token generation.
If the provided credentials are valid, the browser or webview will
redirect the user to the redirect_uri, to actually let ejabberd pass
the token to the app that requested it. It can be either a Web app or
a mobile / desktop application.

### redirect_uri: Receiving OAuth token

The `redirect_uri` originally passed in the authorization_token
request will be called on successfull validation of user credentials,
with added parameters.

For example, redirect URI called by ejabberd can be:

    http://client.uri/?access_token=RHIT8DoudzOctdzBhYL9bYvXz28xQ4Oj&token_type=bearer&expires_in=3600&scope=user_get_roster&state=

Parameters are described in OAuth specification:

- **access_token**: This is the actual token that the client
  application can use for OAuth authentication.
- **token_type**: ejabberd supports `bearer` token type.
- **expires_in**: This is the validity duration of the token, in
  seconds. When the token expires, a new authorization token will need
  to be generated an approved by the user.
  <!--- TODO: Does oauth2 allow token refresh ? Is it implemented or could
  it be implemented in ejabberd ? -->
- **scope**: Confirms the granted scope to the requesting application.
- **state**: If a state parameter was passed by requesting application
  in authorization_token URL, it will be passed back to the
  application as a parameter of the `redirect_uri` to help with the
  client workflow.

<!--- TODO: Add Android and iOS examples on how to get the token.  -->

### Implementing X-OAuth2 authentication in XMPP client

You can connect to ejabberd using and X-OAUTH2 token. You can use an
OAuth token as generated in the previous steps instead of a password
when connecting to ejabberd servers support OAuth authentication
methods (See `auth_method` configuration).

When enabled, X-OAUTH2 authentication method is advertised as follow:


This is done by modifying the SASL auth element as follows:





### Writing ejabberd commands supporting OAuth

If you have existing command that we want to make OAuth compliant, you
need to change it as follow:
<!-- TODO -->

---
title: OAuth support | ejabberd Installation and Operation Guide
bodyclass: nocomment
---

# OAuth support

## Introduction

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

You can read more on OAuth from [OAuth website](http://oauth.net).

OAuth support is available in ejabberd 15.09 and newest releases.

## Configuration

### Authentication method

An X-OAUTH2 SASL mechanism is available as default for authentication
in ejabberd.

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

- **commands_admin_access**: This option will reference an ejabberd
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
- **oauth_access**: If you want to limit which users can create OAuth
  tokens, you can refer to an ejabberd access rule in the `oauth_access`
  option.

Here is an example, for OAuth specific parameters configuration:

```
commands_admin_access: configure
commands:
  - add_commands: user
oauth_expire: 3600
```

In the previous example, tokens expire after an hour. All commands in
categorie `user` are exposed. Admin access is granted to users that
can pass the `configure` access rule defined in the config file.

### Database / Back-ends for OAuth tokens

Currently, OAuth tokens are stored in Mnesia database. In a future
release, we plan to support multiple token

## Using ejabberd OAuth API from your applications

### authorization_token: Generating OAuth token

To generate a token you can have the user open the `/oauth/authorization_token` in a webview in your application or in a browser.

For example, URL can be:

    http://example.net:5280/oauth/authorization_token?response_type=token&client_id=Client1&redirect_uri=http://client.uri&scope=user_get_roster+sasl_auth

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
`a mobile / desktop application.

### redirect_uri: Receiving OAuth token

The `redirect_uri` originally passed in the authorization_token
request will be called on successfull validation of user credentials,
with added parameters.

For example, redirect URI called by ejabberd can be:

    http://client.uri/?access_token=RHIT8DoudzOctdzBhYL9bYvXz28xQ4Oj&token_type=bearer&expires_in=3600&scope=user_get_roster+sasl_auth&state=

Parameters are described in OAuth specification:

- **access_token**: This is the actual token that the client
  application can use for OAuth authentication.
- **token_type**: ejabberd supports `bearer` token type.
- **expires_in**: This is the validity duration of the token, in
  seconds. When the token expires, a new authorization token will need
  to be generated an approved by the user.

<!--- TODO: Does oauth2 allow token refresh ? Is it implemented or could
  it be implemented in ejabberd ? -->
- **scope**: Confirms the granted scope to the requesting
  application. Several scopes can be passed, separated by '+'.
- **state**: If a state parameter was passed by requesting application
  in authorization_token URL, it will be passed back to the
  application as a parameter of the `redirect_uri` to help with the
  client workflow.

<!--- TODO: Add Android and iOS examples on how to get the token.  -->

### Available default scopes

- **sasl_auth**: This scope is use to generate a token that can login
  over XMPP using SASL X-OAUTH2 mechanism.
- **user_get_roster** (*mod_oauth_test*): This scope is used to allow
  retrieving user roster.

<!--- TODO: As scope are generally provided by commands, which are
provided by enabled modules, we need to command to list enabled
scopes.  We should also probably support scopes that are categories of
commands (like user for example). Finally, we should probably allow
getting scope as a command tag, to grant access to all MUC commands
for example. The authorize_token form should list the commands that
will be enabled by the scope at token generation time. -->

### Implementing X-OAuth2 authentication in XMPP client

You can connect to ejabberd using an X-OAUTH2 token that is valid in
the scope `sasl_auth`. You can use an OAuth token as generated in the
previous steps instead of a password when connecting to ejabberd
servers support OAuth SASL mechanism.

When enabled, X-OAUTH2 SASL mechanism is advertised in server stream
features:

```
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
```

Authentication with X-OAUTH2 is done by modifying the SASL auth
element as follow:

```
<auth mechanism='X-OAUTH2'
      xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>
  base64("\0" + user_name + "\0" + oauth_token)
</auth>
```

The content in the auth element should be the base64 encoding of a
string containing a null byte, followed by the user name, another null
byte and the string representation of the user’s OAuth token. This is
similar to how to authenticate with a password using the PLAIN
mechanism, except the token is added instead of the user’s password.

The response is standard for SASL XMPP authentication. For example, on
success, server will reply with:

    <success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>

### Using commands with ReST / XML-RPC API

#### Passing credentials

To pass your bearer token using ReST API, you need to pass your token
as Bearer token in Authorization HTTP header:

    Authorization: Bearer Qi4CyTCDtqpUNW3fnRSZLb0OG3XOOjvx

For XML-RPC, credentials must be passed as XML-RPC parameters.

#### Acting as an admin

With both HTTP remote call mechanism, you can either act as a user or
act as an admin (See previous reference about access rules).

To act as an admin from a ReST API call, the HTTP request must contain
the following header:

    X-Admin: true

To act as an admin from an XML-RPC query, the XML-RPC query must contain:

    {admin,true}

#### XML-RPC example

With a command like `user_get_roster`, you can get your own roster, or
act as an admin to get any user roster.

Here is an (Erlang) XML-RPC example on how to get your own roster:

```
xmlrpc:call({127, 0, 0, 1}, 4560, "/",
  {call, get_roster, [
    {struct, [{user, "peter"},
              {server, "example.com"},
              {token, "0n6LaEjyAOxVDyZChzZfoKMYxc8uUk6L"}]}]},
  false, 60000, "Host: localhost\r\n", []).
```

This will lead to sending this XML-RPC payload to server:

```
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
```

To get roster of ther user using admin authorization, this erlang
XML-RPC code can be used:

```
xmlrpc:call({127, 0, 0, 1}, 4560, "/",
  {call, get_roster, [
    {struct, [{user, "admin"},
              {server, "example.com"},
              {token, "0n6LaEjyAOxVDyZChzZfoKMYxc8uUk6L"}
              {admin, true}]},
    {struct, [{user, "peter"},
              {server, "example.com"}]}]},
  false, 60000, "Host: localhost\r\n", []).
```

that would send this XML to server:

```
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
```




<!-- TODO -->

## ejabberd development

### Writing ejabberd commands supporting OAuth

If you have existing commands that you want to make OAuth compliant,
you can make them OAuth compliant very easily.

An ejabberd command is defined by an `#ejabberd_commands` Erlang
record. The record requires a few fields:

- **name**: This is an atom defining the name of the command.
- **tags**: This is a list of atoms used to group the command into
  consistent group of commands. This is mostly used to group commands
  in `ejabberdctl` command-line tool. Existing categories are:

    - `session`: For commands related to user XMPP sessions.
    - `roster`: Commands related to contact list management.
    - TODO: List other tags that we already use.
- **desc**: Description of the command for online help.
- **module** and **function**: Module and function to call to execute
  the command logic.
- **args**: Argument of the command. An argument is defined by a tuple
  of atoms of the form `{argument_name, data_type}`. `data_type` can be
  one of:

    - binary
    - TODO other types
- **result**: defines what the command will return.
- **policy**: Is an optional field, containing an atom that define
  restriction policy of the command. It can be on of: `open`, `admin`,
  `user`, `restricted`. Default is `restricted`, meaning the command
  can be used from ejabberdctl command-line tool.

<!-- TODO explain what the result field should look likes -->

To define a command that can be used by server user over ReST or
XML-RPC API, you just have to define it with policy `user`. Then, you
have to make sure that the function will take a user binary and a host
binary as first parameter of the function. They do not have to be put
in the `args` list in `#ejabberd_commands` record as the `user policy
implicitely expect them.

That's all you need to have commands that can be used in a variety of
ways.

Here is a example way to register commands when

```
start(_Host, _Opts) ->
    ejabberd_commands:register_commands(commands()).

stop(_Host) ->
    ejabberd_commands:unregister_commands(commands()).

%%%
%%% Register commands
%%%

commands() ->
    [#ejabberd_commands{name = user_get_roster,
                        tags = [roster],
                        desc = "Retrieve the roster",
                        longdesc =
                            "Returns a list of the contacts in a "
                            "user roster.\n\nAlso returns the state "
                            "of the contact subscription. Subscription "
                            "can be either  \"none\", \"from\", \"to\", "
                            "\"both\". Pending can be \"in\", \"out\" "
                            "or \"none\".",
                        module = ?MODULE, function = get_roster,
                        args = [],
                        policy = user,
                        result =
                            {contacts,
                             {list,
                              {contact,
                               {tuple,
                                [{jid, string},
                                 {groups, {list, {group, string}}},
                                 {nick, string}, {subscription, string},
                                 {pending, string}]}}}}}
        ].
```

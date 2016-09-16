---
title: Administration API reference
bodyclass: nocomment
---

## *add_rosteritem* - Add an item to a user's roster (supports SQL)


Add an item to a user's roster (supports SQL)


### Arguments:
- *localuser* :: binary
- *localserver* :: binary
- *user* :: binary
- *server* :: binary
- *nick* :: binary
- *group* :: binary
- *subs* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("add_rosteritem", new HashMap<String, Object>() {{
      put("localuser", "aaaaa");
      put("localserver", "bbbbb");
      put("user", "ccccc");
      put("server", "ddddd");
      put("nick", "eeeee");
      put("group", "fffff");
      put("subs", "ggggg");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("add_rosteritem", {
      localuser => "aaaaa",
      localserver => "bbbbb",
      user => "ccccc",
      server => "ddddd",
      nick => "eeeee",
      group => "fffff",
      subs => "ggggg"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>add_rosteritem</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>localuser</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>localserver</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>user</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>server</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
              <member>
                <name>nick</name>
                <value>
                  <string>eeeee</string>
                </value>
              </member>
              <member>
                <name>group</name>
                <value>
                  <string>fffff</string>
                </value>
              </member>
              <member>
                <name>subs</name>
                <value>
                  <string>ggggg</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/add_rosteritem
    {
      "localuser": "aaaaa",
      "localserver": "bbbbb",
      "user": "ccccc",
      "server": "ddddd",
      "nick": "eeeee",
      "group": "fffff",
      "subs": "ggggg"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *backup* - Store the database to backup file


Store the database to backup file


### Arguments:
- *file* :: string


### Result:
{res,restuple}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("backup", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("backup", {
      file => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>backup</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/backup
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~
{: .code-samples-tabs}



## *ban_account* - Ban an account: kick sessions and set random password


Ban an account: kick sessions and set random password


### Arguments:

*user* :: binary

: User name to ban

*host* :: binary

: Server name

*reason* :: binary

: Reason for banning user


### Result:

{res,rescode}

: Status code: 0 on success, 1 otherwise


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("ban_account", new HashMap<String, Object>() {{
      put("user", "attacker");
      put("host", "myserver.com");
      put("reason", "Spaming other users");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("ban_account", {
      user => "attacker",
      host => "myserver.com",
      reason => "Spaming other users"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>ban_account</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>attacker</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>myserver.com</string>
                </value>
              </member>
              <member>
                <name>reason</name>
                <value>
                  <string>Spaming other users</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/ban_account
    {
      "user": "attacker",
      "host": "myserver.com",
      "reason": "Spaming other users"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *change_password* - Change the password of an account


Change the password of an account


### Arguments:

*user* :: binary

: User name

*host* :: binary

: Server name

*newpass* :: binary

: New password for user


### Result:

{res,rescode}

: Status code: 0 on success, 1 otherwise


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("change_password", new HashMap<String, Object>() {{
      put("user", "peter");
      put("host", "myserver.com");
      put("newpass", "blank");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("change_password", {
      user => "peter",
      host => "myserver.com",
      newpass => "blank"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>change_password</methodName>
      <params>
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
                <name>host</name>
                <value>
                  <string>myserver.com</string>
                </value>
              </member>
              <member>
                <name>newpass</name>
                <value>
                  <string>blank</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/change_password
    {
      "user": "peter",
      "host": "myserver.com",
      "newpass": "blank"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *change_room_option* - Change an option in a MUC room


Change an option in a MUC room


### Arguments:
- *name* :: binary
- *service* :: binary
- *option* :: binary
- *value* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("change_room_option", new HashMap<String, Object>() {{
      put("name", "aaaaa");
      put("service", "bbbbb");
      put("option", "ccccc");
      put("value", "ddddd");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("change_room_option", {
      name => "aaaaa",
      service => "bbbbb",
      option => "ccccc",
      value => "ddddd"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>change_room_option</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>name</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>service</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>option</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>value</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/change_room_option
    {
      "name": "aaaaa",
      "service": "bbbbb",
      "option": "ccccc",
      "value": "ddddd"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *check_account* - Check if an account exists or not


Check if an account exists or not


### Arguments:

*user* :: binary

: User name to check

*host* :: binary

: Server to check


### Result:

{res,rescode}

: Status code: 0 on success, 1 otherwise


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("check_account", new HashMap<String, Object>() {{
      put("user", "peter");
      put("host", "myserver.com");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("check_account", {
      user => "peter",
      host => "myserver.com"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>check_account</methodName>
      <params>
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
                <name>host</name>
                <value>
                  <string>myserver.com</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/check_account
    {
      "user": "peter",
      "host": "myserver.com"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *check_password* - Check if a password is correct


Check if a password is correct


### Arguments:

*user* :: binary

: User name to check

*host* :: binary

: Server to check

*password* :: binary

: Password to check


### Result:

{res,rescode}

: Status code: 0 on success, 1 otherwise


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("check_password", new HashMap<String, Object>() {{
      put("user", "peter");
      put("host", "myserver.com");
      put("password", "secret");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("check_password", {
      user => "peter",
      host => "myserver.com",
      password => "secret"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>check_password</methodName>
      <params>
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
                <name>host</name>
                <value>
                  <string>myserver.com</string>
                </value>
              </member>
              <member>
                <name>password</name>
                <value>
                  <string>secret</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/check_password
    {
      "user": "peter",
      "host": "myserver.com",
      "password": "secret"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *check_password_hash* - Check if the password hash is correct


Allowed hash methods: md5, sha.


### Arguments:

*user* :: binary

: User name to check

*host* :: binary

: Server to check

*passwordhash* :: string

: Password's hash value

*hashmethod* :: string

: Name of hash method


### Result:

{res,rescode}

: Status code: 0 on success, 1 otherwise


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("check_password_hash", new HashMap<String, Object>() {{
      put("user", "peter");
      put("host", "myserver.com");
      put("passwordhash", "5ebe2294ecd0e0f08eab7690d2a6ee69");
      put("hashmethod", "md5");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("check_password_hash", {
      user => "peter",
      host => "myserver.com",
      passwordhash => "5ebe2294ecd0e0f08eab7690d2a6ee69",
      hashmethod => "md5"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>check_password_hash</methodName>
      <params>
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
                <name>host</name>
                <value>
                  <string>myserver.com</string>
                </value>
              </member>
              <member>
                <name>passwordhash</name>
                <value>
                  <string>5ebe2294ecd0e0f08eab7690d2a6ee69</string>
                </value>
              </member>
              <member>
                <name>hashmethod</name>
                <value>
                  <string>md5</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/check_password_hash
    {
      "user": "peter",
      "host": "myserver.com",
      "passwordhash": "5ebe2294ecd0e0f08eab7690d2a6ee69",
      "hashmethod": "md5"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *compile* - Recompile and reload Erlang source code file


Recompile and reload Erlang source code file


### Arguments:

*file* :: string

: Filename of erlang source file to compile


### Result:

{res,rescode}

: Status code: 0 on success, 1 otherwise


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("compile", new HashMap<String, Object>() {{
      put("file", "/home/me/srcs/ejabberd/mod_example.erl");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("compile", {
      file => "/home/me/srcs/ejabberd/mod_example.erl"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>compile</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>/home/me/srcs/ejabberd/mod_example.erl</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/compile
    {
      "file": "/home/me/srcs/ejabberd/mod_example.erl"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *connected_users* - List all established sessions


List all established sessions


### Arguments:


### Result:
{connected_users,{list,{sessions,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("connected_users", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("connected_users", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>connected_users</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/connected_users
    {
      
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *connected_users_info* - List all established sessions and their information


List all established sessions and their information


### Arguments:


### Result:
{connected_users_info,
    {list,
        {sessions,
            {tuple,
                [{jid,string},
                 {connection,string},
                 {ip,string},
                 {port,integer},
                 {priority,integer},
                 {node,string},
                 {uptime,integer}]}}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("connected_users_info", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("connected_users_info", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>connected_users_info</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/connected_users_info
    {
      
    }
    
    HTTP/1.1 200 OK
    [
      {
        "jid": "aaaaa",
        "connection": "bbbbb",
        "ip": "ccccc",
        "port": 1,
        "priority": 2,
        "node": "ddddd",
        "uptime": 3
      },
      {
        "jid": "eeeee",
        "connection": "fffff",
        "ip": "ggggg",
        "port": 4,
        "priority": 5,
        "node": "hhhhh",
        "uptime": 6
      }
    ]
~~~
{: .code-samples-tabs}



## *connected_users_number* - Get the number of established sessions


Get the number of established sessions


### Arguments:


### Result:
{num_sessions,integer}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("connected_users_number", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("connected_users_number", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>connected_users_number</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/connected_users_number
    {
      
    }
    
    HTTP/1.1 200 OK
        {"num_sessions": 1    }
~~~
{: .code-samples-tabs}



## *connected_users_vhost* - Get the list of established sessions in a vhost


Get the list of established sessions in a vhost


### Arguments:
- *host* :: binary


### Result:
{connected_users_vhost,{list,{sessions,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("connected_users_vhost", new HashMap<String, Object>() {{
      put("host", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("connected_users_vhost", {
      host => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>connected_users_vhost</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>host</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/connected_users_vhost
    {
      "host": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *convert_to_scram* - Convert the passwords in 'users' SQL table to SCRAM


Convert the passwords in 'users' SQL table to SCRAM


### Arguments:
- *host* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("convert_to_scram", new HashMap<String, Object>() {{
      put("host", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("convert_to_scram", {
      host => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>convert_to_scram</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>host</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/convert_to_scram
    {
      "host": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *convert_to_yaml* - Convert the input file from Erlang to YAML format


Convert the input file from Erlang to YAML format


### Arguments:
- *in* :: string
- *out* :: string


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("convert_to_yaml", new HashMap<String, Object>() {{
      put("in", "aaaaa");
      put("out", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("convert_to_yaml", {
      in => "aaaaa",
      out => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>convert_to_yaml</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>in</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>out</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/convert_to_yaml
    {
      "in": "aaaaa",
      "out": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *create_room* - Create a MUC room name@service in host


Create a MUC room name@service in host


### Arguments:
- *name* :: binary
- *service* :: binary
- *host* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("create_room", new HashMap<String, Object>() {{
      put("name", "aaaaa");
      put("service", "bbbbb");
      put("host", "ccccc");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("create_room", {
      name => "aaaaa",
      service => "bbbbb",
      host => "ccccc"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>create_room</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>name</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>service</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/create_room
    {
      "name": "aaaaa",
      "service": "bbbbb",
      "host": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *create_room_with_opts* - Create a MUC room name@service in host with given options


Create a MUC room name@service in host with given options


### Arguments:
- *name* :: binary
- *service* :: binary
- *host* :: binary
- *options* :: {list,{option,{tuple,[{name,binary},{value,binary}]}}}


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("create_room_with_opts", new HashMap<String, Object>() {{
      put("name", "aaaaa");
      put("service", "bbbbb");
      put("host", "ccccc");
      put("options", new Object[] {
        new HashMap<String, Object>() {{ put("option", new HashMap<String, Object>() {{
            put("name", "ddddd");
            put("value", "eeeee");
          }}) }},
        new HashMap<String, Object>() {{ put("option", new HashMap<String, Object>() {{
            put("name", "fffff");
            put("value", "ggggg");
          }}) }}
      });
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("create_room_with_opts", {
      name => "aaaaa",
      service => "bbbbb",
      host => "ccccc",
      options => [{option => {name => "ddddd", value => "eeeee"}}, {option => {name => "fffff", value => "ggggg"}}]
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>create_room_with_opts</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>name</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>service</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>options</name>
                <value>
                  <array>
                    <data>
                      <value>
                        <struct>
                          <member>
                            <name>option</name>
                            <value>
                              <struct>
                              <member>
                                <name>name</name>
                                <value>
                                  <string>ddddd</string>
                                </value>
                              </member>
                              <member>
                                <name>value</name>
                                <value>
                                  <string>eeeee</string>
                                </value>
                              </member>
                              </struct>
                            </value>
                          </member>
                        </struct>
                      </value>
                      <value>
                        <struct>
                          <member>
                            <name>option</name>
                            <value>
                              <struct>
                              <member>
                                <name>name</name>
                                <value>
                                  <string>fffff</string>
                                </value>
                              </member>
                              <member>
                                <name>value</name>
                                <value>
                                  <string>ggggg</string>
                                </value>
                              </member>
                              </struct>
                            </value>
                          </member>
                        </struct>
                      </value>
                    </data>
                  </array>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/create_room_with_opts
    {
      "name": "aaaaa",
      "service": "bbbbb",
      "host": "ccccc",
      "options": [
        {
          "name": "ddddd",
          "value": "eeeee"
        },
        {
          "name": "fffff",
          "value": "ggggg"
        }
      ]
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *create_rooms_file* - Create the rooms indicated in file


Provide one room JID per line. Rooms will be created after restart.


### Arguments:
- *file* :: string


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("create_rooms_file", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("create_rooms_file", {
      file => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>create_rooms_file</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/create_rooms_file
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *delete_expired_messages* - Delete expired offline messages from database


Delete expired offline messages from database


### Arguments:


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("delete_expired_messages", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("delete_expired_messages", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>delete_expired_messages</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/delete_expired_messages
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *delete_mnesia* - Export all tables as SQL queries to a file


Export all tables as SQL queries to a file


### Arguments:
- *host* :: string


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("delete_mnesia", new HashMap<String, Object>() {{
      put("host", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("delete_mnesia", {
      host => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>delete_mnesia</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>host</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/delete_mnesia
    {
      "host": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *delete_old_mam_messages* - Delete MAM messages older than DAYS


Valid message TYPEs: "chat", "groupchat", "all".


### Arguments:
- *type* :: binary
- *days* :: integer


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("delete_old_mam_messages", new HashMap<String, Object>() {{
      put("type", "aaaaa");
      put("days", new Integer(1));
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("delete_old_mam_messages", {
      type => "aaaaa",
      days => 1
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>delete_old_mam_messages</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>type</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>days</name>
                <value>
                  <integer>1</integer>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/delete_old_mam_messages
    {
      "type": "aaaaa",
      "days": 1
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *delete_old_messages* - Delete offline messages older than DAYS


Delete offline messages older than DAYS


### Arguments:
- *days* :: integer


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("delete_old_messages", new HashMap<String, Object>() {{
      put("days", new Integer(1));
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("delete_old_messages", {
      days => 1
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>delete_old_messages</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>days</name>
                <value>
                  <integer>1</integer>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/delete_old_messages
    {
      "days": 1
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *delete_old_users* - Delete users that didn't log in last days, or that never logged


Delete users that didn't log in last days, or that never logged


### Arguments:

*days* :: integer

: Last login age in days of accounts that should be removed


### Result:

{res,restuple}

: Result tuple


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("delete_old_users", new HashMap<String, Object>() {{
      put("days", new Integer(30));
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("delete_old_users", {
      days => 30
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>delete_old_users</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>days</name>
                <value>
                  <integer>30</integer>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/delete_old_users
    {
      "days": 30
    }
    
    HTTP/1.1 200 OK
    "Deleted 2 users: ["oldman@myserver.com", "test@myserver.com"]"
~~~
{: .code-samples-tabs}



## *delete_old_users_vhost* - Delete users that didn't log in last days in vhost, or that never logged


Delete users that didn't log in last days in vhost, or that never logged


### Arguments:

*host* :: binary

: Server name

*days* :: integer

: Last login age in days of accounts that should be removed


### Result:

{res,restuple}

: Result tuple


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("delete_old_users_vhost", new HashMap<String, Object>() {{
      put("host", "myserver.com");
      put("days", new Integer(30));
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("delete_old_users_vhost", {
      host => "myserver.com",
      days => 30
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>delete_old_users_vhost</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>host</name>
                <value>
                  <string>myserver.com</string>
                </value>
              </member>
              <member>
                <name>days</name>
                <value>
                  <integer>30</integer>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/delete_old_users_vhost
    {
      "host": "myserver.com",
      "days": 30
    }
    
    HTTP/1.1 200 OK
    "Deleted 2 users: ["oldman@myserver.com", "test@myserver.com"]"
~~~
{: .code-samples-tabs}



## *delete_rosteritem* - Delete an item from a user's roster (supports SQL)


Delete an item from a user's roster (supports SQL)


### Arguments:
- *localuser* :: binary
- *localserver* :: binary
- *user* :: binary
- *server* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("delete_rosteritem", new HashMap<String, Object>() {{
      put("localuser", "aaaaa");
      put("localserver", "bbbbb");
      put("user", "ccccc");
      put("server", "ddddd");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("delete_rosteritem", {
      localuser => "aaaaa",
      localserver => "bbbbb",
      user => "ccccc",
      server => "ddddd"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>delete_rosteritem</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>localuser</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>localserver</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>user</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>server</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/delete_rosteritem
    {
      "localuser": "aaaaa",
      "localserver": "bbbbb",
      "user": "ccccc",
      "server": "ddddd"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *destroy_room* - Destroy a MUC room


Destroy a MUC room


### Arguments:
- *name* :: binary
- *service* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("destroy_room", new HashMap<String, Object>() {{
      put("name", "aaaaa");
      put("service", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("destroy_room", {
      name => "aaaaa",
      service => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>destroy_room</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>name</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>service</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/destroy_room
    {
      "name": "aaaaa",
      "service": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *destroy_rooms_file* - Destroy the rooms indicated in file


Provide one room JID per line.


### Arguments:
- *file* :: string


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("destroy_rooms_file", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("destroy_rooms_file", {
      file => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>destroy_rooms_file</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/destroy_rooms_file
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *dump* - Dump the database to text file


Dump the database to text file


### Arguments:
- *file* :: string


### Result:
{res,restuple}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("dump", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("dump", {
      file => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>dump</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/dump
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~
{: .code-samples-tabs}



## *dump_table* - Dump a table to text file


Dump a table to text file


### Arguments:
- *file* :: string
- *table* :: string


### Result:
{res,restuple}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("dump_table", new HashMap<String, Object>() {{
      put("file", "aaaaa");
      put("table", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("dump_table", {
      file => "aaaaa",
      table => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>dump_table</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>table</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/dump_table
    {
      "file": "aaaaa",
      "table": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~
{: .code-samples-tabs}



## *export2sql* - Export virtual host information from Mnesia tables to SQL files


Export virtual host information from Mnesia tables to SQL files


### Arguments:
- *host* :: string
- *file* :: string


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("export2sql", new HashMap<String, Object>() {{
      put("host", "aaaaa");
      put("file", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("export2sql", {
      host => "aaaaa",
      file => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>export2sql</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>host</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>file</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/export2sql
    {
      "host": "aaaaa",
      "file": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *export_piefxis* - Export data of all users in the server to PIEFXIS files (XEP-0227)


Export data of all users in the server to PIEFXIS files (XEP-0227)


### Arguments:
- *dir* :: string


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("export_piefxis", new HashMap<String, Object>() {{
      put("dir", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("export_piefxis", {
      dir => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>export_piefxis</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>dir</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/export_piefxis
    {
      "dir": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *export_piefxis_host* - Export data of users in a host to PIEFXIS files (XEP-0227)


Export data of users in a host to PIEFXIS files (XEP-0227)


### Arguments:
- *dir* :: string
- *host* :: string


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("export_piefxis_host", new HashMap<String, Object>() {{
      put("dir", "aaaaa");
      put("host", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("export_piefxis_host", {
      dir => "aaaaa",
      host => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>export_piefxis_host</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>dir</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/export_piefxis_host
    {
      "dir": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *gen_html_doc_for_commands* - Generates html documentation for ejabberd_commands


Generates html documentation for ejabberd_commands


### Arguments:

*file* :: binary

: Path to file where generated documentation should be stored

*regexp* :: binary

: Regexp matching names of commands or modules that will be included inside generated document

*examples* :: binary

: Comma separated list of languages (choosen from java, perl, xmlrpc, json)that will have example invocation include in markdown document


### Result:

{res,rescode}

: 0 if command failed, 1 when succedded


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("gen_html_doc_for_commands", new HashMap<String, Object>() {{
      put("file", "/home/me/docs/api.html");
      put("regexp", "mod_admin");
      put("examples", "java,json");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("gen_html_doc_for_commands", {
      file => "/home/me/docs/api.html",
      regexp => "mod_admin",
      examples => "java,json"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>gen_html_doc_for_commands</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>/home/me/docs/api.html</string>
                </value>
              </member>
              <member>
                <name>regexp</name>
                <value>
                  <string>mod_admin</string>
                </value>
              </member>
              <member>
                <name>examples</name>
                <value>
                  <string>java,json</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/gen_html_doc_for_commands
    {
      "file": "/home/me/docs/api.html",
      "regexp": "mod_admin",
      "examples": "java,json"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *gen_markdown_doc_for_commands* - Generates markdown documentation for ejabberd_commands


Generates markdown documentation for ejabberd_commands


### Arguments:

*file* :: binary

: Path to file where generated documentation should be stored

*regexp* :: binary

: Regexp matching names of commands or modules that will be included inside generated document

*examples* :: binary

: Comma separated list of languages (choosen from java, perl, xmlrpc, json)that will have example invocation include in markdown document


### Result:

{res,rescode}

: 0 if command failed, 1 when succedded


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("gen_markdown_doc_for_commands", new HashMap<String, Object>() {{
      put("file", "/home/me/docs/api.html");
      put("regexp", "mod_admin");
      put("examples", "java,json");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("gen_markdown_doc_for_commands", {
      file => "/home/me/docs/api.html",
      regexp => "mod_admin",
      examples => "java,json"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>gen_markdown_doc_for_commands</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>/home/me/docs/api.html</string>
                </value>
              </member>
              <member>
                <name>regexp</name>
                <value>
                  <string>mod_admin</string>
                </value>
              </member>
              <member>
                <name>examples</name>
                <value>
                  <string>java,json</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/gen_markdown_doc_for_commands
    {
      "file": "/home/me/docs/api.html",
      "regexp": "mod_admin",
      "examples": "java,json"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *get_cookie* - Get the Erlang cookie of this node


Get the Erlang cookie of this node


### Arguments:


### Result:

{cookie,string}

: Erlang cookie used for authentication by ejabberd


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_cookie", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("get_cookie", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>get_cookie</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/get_cookie
    {
      
    }
    
    HTTP/1.1 200 OK
        {"cookie": "MWTAVMODFELNLSMYXPPD"    }
~~~
{: .code-samples-tabs}



## *get_last* - Get last activity information (timestamp and status)


Timestamp is the seconds since1970-01-01 00:00:00 UTC, for example: date +%s


### Arguments:
- *user* :: binary
- *host* :: binary


### Result:
{last_activity,string}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_last", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("get_last", {
      user => "aaaaa",
      host => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>get_last</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/get_last
    {
      "user": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
        {"last_activity": "aaaaa"    }
~~~
{: .code-samples-tabs}



## *get_loglevel* - Get the current loglevel


Get the current loglevel


### Arguments:


### Result:
{leveltuple,{tuple,[{levelnumber,integer},
                    {levelatom,atom},
                    {leveldesc,string}]}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_loglevel", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("get_loglevel", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>get_loglevel</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/get_loglevel
    {
      
    }
    
    HTTP/1.1 200 OK
    {
      "levelnumber": 1,
      "levelatom": "aaaaa",
      "leveldesc": "bbbbb"
    }
~~~
{: .code-samples-tabs}



## *get_offline_count* - Get the number of unread offline messages


Get the number of unread offline messages


### Arguments:


### Result:
{value,integer}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_offline_count", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("get_offline_count", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>get_offline_count</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/get_offline_count
    {
      
    }
    
    HTTP/1.1 200 OK
        {"value": 1    }
~~~
{: .code-samples-tabs}



## *get_room_affiliations* - Get the list of affiliations of a MUC room


Get the list of affiliations of a MUC room


### Arguments:
- *name* :: binary
- *service* :: binary


### Result:
{affiliations,
    {list,
        {affiliation,
            {tuple,
                [{username,string},
                 {domain,string},
                 {affiliation,atom},
                 {reason,string}]}}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_room_affiliations", new HashMap<String, Object>() {{
      put("name", "aaaaa");
      put("service", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("get_room_affiliations", {
      name => "aaaaa",
      service => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>get_room_affiliations</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>name</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>service</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/get_room_affiliations
    {
      "name": "aaaaa",
      "service": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    [
      {
        "username": "aaaaa",
        "domain": "bbbbb",
        "affiliation": "ccccc",
        "reason": "ddddd"
      },
      {
        "username": "eeeee",
        "domain": "fffff",
        "affiliation": "ggggg",
        "reason": "hhhhh"
      }
    ]
~~~
{: .code-samples-tabs}



## *get_room_occupants* - Get the list of occupants of a MUC room


Get the list of occupants of a MUC room


### Arguments:
- *name* :: binary
- *service* :: binary


### Result:
{occupants,{list,{occupant,{tuple,[{jid,string},
                                   {nick,string},
                                   {role,string}]}}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_room_occupants", new HashMap<String, Object>() {{
      put("name", "aaaaa");
      put("service", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("get_room_occupants", {
      name => "aaaaa",
      service => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>get_room_occupants</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>name</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>service</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/get_room_occupants
    {
      "name": "aaaaa",
      "service": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    [
      {
        "jid": "aaaaa",
        "nick": "bbbbb",
        "role": "ccccc"
      },
      {
        "jid": "ddddd",
        "nick": "eeeee",
        "role": "fffff"
      }
    ]
~~~
{: .code-samples-tabs}



## *get_room_occupants_number* - Get the number of occupants of a MUC room


Get the number of occupants of a MUC room


### Arguments:
- *name* :: binary
- *service* :: binary


### Result:
{occupants,integer}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_room_occupants_number", new HashMap<String, Object>() {{
      put("name", "aaaaa");
      put("service", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("get_room_occupants_number", {
      name => "aaaaa",
      service => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>get_room_occupants_number</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>name</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>service</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/get_room_occupants_number
    {
      "name": "aaaaa",
      "service": "bbbbb"
    }
    
    HTTP/1.1 200 OK
        {"occupants": 1    }
~~~
{: .code-samples-tabs}



## *get_room_options* - Get options from a MUC room


Get options from a MUC room


### Arguments:
- *name* :: binary
- *service* :: binary


### Result:
{options,{list,{option,{tuple,[{name,string},{value,string}]}}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_room_options", new HashMap<String, Object>() {{
      put("name", "aaaaa");
      put("service", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("get_room_options", {
      name => "aaaaa",
      service => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>get_room_options</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>name</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>service</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/get_room_options
    {
      "name": "aaaaa",
      "service": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    [
      {
        "name": "aaaaa",
        "value": "bbbbb"
      },
      {
        "name": "ccccc",
        "value": "ddddd"
      }
    ]
~~~
{: .code-samples-tabs}



## *get_roster* - Get roster of a local user


Get roster of a local user


### Arguments:


### Result:
{contacts,{list,{contact,{tuple,[{jid,string},
                                 {nick,string},
                                 {subscription,string},
                                 {ask,string},
                                 {group,string}]}}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_roster", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("get_roster", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>get_roster</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/get_roster
    {
      
    }
    
    HTTP/1.1 200 OK
    [
      {
        "jid": "aaaaa",
        "nick": "bbbbb",
        "subscription": "ccccc",
        "ask": "ddddd",
        "group": "eeeee"
      },
      {
        "jid": "fffff",
        "nick": "ggggg",
        "subscription": "hhhhh",
        "ask": "iiiii",
        "group": "jjjjj"
      }
    ]
~~~
{: .code-samples-tabs}



## *get_subscribers* - List subscribers of a MUC conference


List subscribers of a MUC conference


### Arguments:
- *name* :: binary
- *service* :: binary


### Result:
{subscribers,{list,{jid,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_subscribers", new HashMap<String, Object>() {{
      put("name", "aaaaa");
      put("service", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("get_subscribers", {
      name => "aaaaa",
      service => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>get_subscribers</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>name</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>service</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/get_subscribers
    {
      "name": "aaaaa",
      "service": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *get_user_rooms* - Get the list of rooms where this user is occupant


Get the list of rooms where this user is occupant


### Arguments:
- *user* :: binary
- *host* :: binary


### Result:
{rooms,{list,{room,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_user_rooms", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("get_user_rooms", {
      user => "aaaaa",
      host => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>get_user_rooms</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/get_user_rooms
    {
      "user": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *get_vcard* - Get content from a vCard field


Some vcard field names in get/set_vcard are:
 FN		- Full Name
 NICKNAME	- Nickname
 BDAY		- Birthday
 TITLE		- Work: Position
 ROLE		- Work: Role
Some vcard field names and subnames in get/set_vcard2 are:
 N FAMILY	- Family name
 N GIVEN	- Given name
 N MIDDLE	- Middle name
 ADR CTRY	- Address: Country
 ADR LOCALITY	- Address: City
 TEL HOME      - Telephone: Home
 TEL CELL      - Telephone: Cellphone
 TEL WORK      - Telephone: Work
 TEL VOICE     - Telephone: Voice
 EMAIL USERID	- E-Mail Address
 ORG ORGNAME	- Work: Company
 ORG ORGUNIT	- Work: Department

For a full list of vCard fields check XEP-0054: vcard-temp at http://www.xmpp.org/extensions/xep-0054.html


### Arguments:
- *user* :: binary
- *host* :: binary
- *name* :: binary


### Result:
{content,string}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_vcard", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("name", "ccccc");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("get_vcard", {
      user => "aaaaa",
      host => "bbbbb",
      name => "ccccc"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>get_vcard</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>name</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/get_vcard
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "name": "ccccc"
    }
    
    HTTP/1.1 200 OK
        {"content": "aaaaa"    }
~~~
{: .code-samples-tabs}



## *get_vcard2* - Get content from a vCard field


Some vcard field names and subnames in get/set_vcard2 are:
 N FAMILY	- Family name
 N GIVEN	- Given name
 N MIDDLE	- Middle name
 ADR CTRY	- Address: Country
 ADR LOCALITY	- Address: City
 TEL HOME      - Telephone: Home
 TEL CELL      - Telephone: Cellphone
 TEL WORK      - Telephone: Work
 TEL VOICE     - Telephone: Voice
 EMAIL USERID	- E-Mail Address
 ORG ORGNAME	- Work: Company
 ORG ORGUNIT	- Work: Department

Some vcard field names in get/set_vcard are:
 FN		- Full Name
 NICKNAME	- Nickname
 BDAY		- Birthday
 TITLE		- Work: Position
 ROLE		- Work: Role
For a full list of vCard fields check XEP-0054: vcard-temp at http://www.xmpp.org/extensions/xep-0054.html


### Arguments:
- *user* :: binary
- *host* :: binary
- *name* :: binary
- *subname* :: binary


### Result:
{content,string}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_vcard2", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("name", "ccccc");
      put("subname", "ddddd");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("get_vcard2", {
      user => "aaaaa",
      host => "bbbbb",
      name => "ccccc",
      subname => "ddddd"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>get_vcard2</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>name</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>subname</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/get_vcard2
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "name": "ccccc",
      "subname": "ddddd"
    }
    
    HTTP/1.1 200 OK
        {"content": "aaaaa"    }
~~~
{: .code-samples-tabs}



## *get_vcard2_multi* - Get multiple contents from a vCard field


Some vcard field names and subnames in get/set_vcard2 are:
 N FAMILY	- Family name
 N GIVEN	- Given name
 N MIDDLE	- Middle name
 ADR CTRY	- Address: Country
 ADR LOCALITY	- Address: City
 TEL HOME      - Telephone: Home
 TEL CELL      - Telephone: Cellphone
 TEL WORK      - Telephone: Work
 TEL VOICE     - Telephone: Voice
 EMAIL USERID	- E-Mail Address
 ORG ORGNAME	- Work: Company
 ORG ORGUNIT	- Work: Department

Some vcard field names in get/set_vcard are:
 FN		- Full Name
 NICKNAME	- Nickname
 BDAY		- Birthday
 TITLE		- Work: Position
 ROLE		- Work: Role
For a full list of vCard fields check XEP-0054: vcard-temp at http://www.xmpp.org/extensions/xep-0054.html


### Arguments:
- *user* :: binary
- *host* :: binary
- *name* :: binary
- *subname* :: binary


### Result:
{contents,{list,{value,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_vcard2_multi", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("name", "ccccc");
      put("subname", "ddddd");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("get_vcard2_multi", {
      user => "aaaaa",
      host => "bbbbb",
      name => "ccccc",
      subname => "ddddd"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>get_vcard2_multi</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>name</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>subname</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/get_vcard2_multi
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "name": "ccccc",
      "subname": "ddddd"
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *import_dir* - Import users data from jabberd14 spool dir


Import users data from jabberd14 spool dir


### Arguments:
- *file* :: string


### Result:
{res,restuple}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("import_dir", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("import_dir", {
      file => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>import_dir</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/import_dir
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~
{: .code-samples-tabs}



## *import_file* - Import user data from jabberd14 spool file


Import user data from jabberd14 spool file


### Arguments:
- *file* :: string


### Result:
{res,restuple}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("import_file", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("import_file", {
      file => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>import_file</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/import_file
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~
{: .code-samples-tabs}



## *import_piefxis* - Import users data from a PIEFXIS file (XEP-0227)


Import users data from a PIEFXIS file (XEP-0227)


### Arguments:
- *file* :: string


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("import_piefxis", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("import_piefxis", {
      file => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>import_piefxis</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/import_piefxis
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *import_prosody* - Import data from Prosody


Import data from Prosody


### Arguments:
- *dir* :: string


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("import_prosody", new HashMap<String, Object>() {{
      put("dir", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("import_prosody", {
      dir => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>import_prosody</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>dir</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/import_prosody
    {
      "dir": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *incoming_s2s_number* - Number of incoming s2s connections on the node


Number of incoming s2s connections on the node


### Arguments:


### Result:
{s2s_incoming,integer}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("incoming_s2s_number", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("incoming_s2s_number", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>incoming_s2s_number</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/incoming_s2s_number
    {
      
    }
    
    HTTP/1.1 200 OK
        {"s2s_incoming": 1    }
~~~
{: .code-samples-tabs}



## *install_fallback* - Install the database from a fallback file


Install the database from a fallback file


### Arguments:
- *file* :: string


### Result:
{res,restuple}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("install_fallback", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("install_fallback", {
      file => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>install_fallback</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/install_fallback
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~
{: .code-samples-tabs}



## *join_cluster* - Join this node into the cluster handled by Node


Join this node into the cluster handled by Node


### Arguments:
- *node* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("join_cluster", new HashMap<String, Object>() {{
      put("node", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("join_cluster", {
      node => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>join_cluster</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>node</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/join_cluster
    {
      "node": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *kick_session* - Kick a user session


Kick a user session


### Arguments:

*user* :: binary

: User name

*host* :: binary

: Server name

*resource* :: binary

: User's resource

*reason* :: binary

: Reason for closing session


### Result:

{res,rescode}

: Status code: 0 on success, 1 otherwise


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("kick_session", new HashMap<String, Object>() {{
      put("user", "peter");
      put("host", "myserver.com");
      put("resource", "Psi");
      put("reason", "Stuck connection");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("kick_session", {
      user => "peter",
      host => "myserver.com",
      resource => "Psi",
      reason => "Stuck connection"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>kick_session</methodName>
      <params>
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
                <name>host</name>
                <value>
                  <string>myserver.com</string>
                </value>
              </member>
              <member>
                <name>resource</name>
                <value>
                  <string>Psi</string>
                </value>
              </member>
              <member>
                <name>reason</name>
                <value>
                  <string>Stuck connection</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/kick_session
    {
      "user": "peter",
      "host": "myserver.com",
      "resource": "Psi",
      "reason": "Stuck connection"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *kick_user* - Disconnect user's active sessions


Disconnect user's active sessions


### Arguments:
- *user* :: binary
- *host* :: binary


### Result:
{num_resources,integer}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("kick_user", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("kick_user", {
      user => "aaaaa",
      host => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>kick_user</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/kick_user
    {
      "user": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
        {"num_resources": 1    }
~~~
{: .code-samples-tabs}



## *leave_cluster* - Remove node handled by Node from the cluster


Remove node handled by Node from the cluster


### Arguments:
- *node* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("leave_cluster", new HashMap<String, Object>() {{
      put("node", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("leave_cluster", {
      node => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>leave_cluster</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>node</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/leave_cluster
    {
      "node": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *list_cluster* - List nodes that are part of the cluster handled by Node


List nodes that are part of the cluster handled by Node


### Arguments:


### Result:
{nodes,{list,{node,atom}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("list_cluster", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("list_cluster", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>list_cluster</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/list_cluster
    {
      
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *load* - Restore the database from text file


Restore the database from text file


### Arguments:
- *file* :: string


### Result:
{res,restuple}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("load", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("load", {
      file => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>load</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/load
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~
{: .code-samples-tabs}



## *mnesia_change_nodename* - Change the erlang node name in a backup file


Change the erlang node name in a backup file


### Arguments:
- *oldnodename* :: string
- *newnodename* :: string
- *oldbackup* :: string
- *newbackup* :: string


### Result:
{res,restuple}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("mnesia_change_nodename", new HashMap<String, Object>() {{
      put("oldnodename", "aaaaa");
      put("newnodename", "bbbbb");
      put("oldbackup", "ccccc");
      put("newbackup", "ddddd");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("mnesia_change_nodename", {
      oldnodename => "aaaaa",
      newnodename => "bbbbb",
      oldbackup => "ccccc",
      newbackup => "ddddd"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>mnesia_change_nodename</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>oldnodename</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>newnodename</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>oldbackup</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>newbackup</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/mnesia_change_nodename
    {
      "oldnodename": "aaaaa",
      "newnodename": "bbbbb",
      "oldbackup": "ccccc",
      "newbackup": "ddddd"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~
{: .code-samples-tabs}



## *module_check* - 





### Arguments:
- *module* :: binary


### Result:
{res,integer}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("module_check", new HashMap<String, Object>() {{
      put("module", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("module_check", {
      module => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>module_check</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>module</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/module_check
    {
      "module": "aaaaa"
    }
    
    HTTP/1.1 200 OK
        {"res": 1    }
~~~
{: .code-samples-tabs}



## *module_install* - 





### Arguments:
- *module* :: binary


### Result:
{res,integer}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("module_install", new HashMap<String, Object>() {{
      put("module", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("module_install", {
      module => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>module_install</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>module</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/module_install
    {
      "module": "aaaaa"
    }
    
    HTTP/1.1 200 OK
        {"res": 1    }
~~~
{: .code-samples-tabs}



## *module_uninstall* - 





### Arguments:
- *module* :: binary


### Result:
{res,integer}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("module_uninstall", new HashMap<String, Object>() {{
      put("module", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("module_uninstall", {
      module => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>module_uninstall</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>module</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/module_uninstall
    {
      "module": "aaaaa"
    }
    
    HTTP/1.1 200 OK
        {"res": 1    }
~~~
{: .code-samples-tabs}



## *module_upgrade* - 





### Arguments:
- *module* :: binary


### Result:
{res,integer}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("module_upgrade", new HashMap<String, Object>() {{
      put("module", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("module_upgrade", {
      module => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>module_upgrade</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>module</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/module_upgrade
    {
      "module": "aaaaa"
    }
    
    HTTP/1.1 200 OK
        {"res": 1    }
~~~
{: .code-samples-tabs}



## *modules_available* - 





### Arguments:


### Result:
{modules,{list,{module,{tuple,[{name,atom},{summary,string}]}}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("modules_available", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("modules_available", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>modules_available</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/modules_available
    {
      
    }
    
    HTTP/1.1 200 OK
    {
      "aaaaa": "bbbbb",
      "ccccc": "ddddd"
    }
~~~
{: .code-samples-tabs}



## *modules_installed* - 





### Arguments:


### Result:
{modules,{list,{module,{tuple,[{name,atom},{summary,string}]}}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("modules_installed", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("modules_installed", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>modules_installed</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/modules_installed
    {
      
    }
    
    HTTP/1.1 200 OK
    {
      "aaaaa": "bbbbb",
      "ccccc": "ddddd"
    }
~~~
{: .code-samples-tabs}



## *modules_update_specs* - 





### Arguments:


### Result:
{res,integer}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("modules_update_specs", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("modules_update_specs", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>modules_update_specs</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/modules_update_specs
    {
      
    }
    
    HTTP/1.1 200 OK
        {"res": 1    }
~~~
{: .code-samples-tabs}



## *muc_online_rooms* - List existing rooms ('global' to get all vhosts)


List existing rooms ('global' to get all vhosts)


### Arguments:
- *host* :: binary


### Result:
{rooms,{list,{room,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("muc_online_rooms", new HashMap<String, Object>() {{
      put("host", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("muc_online_rooms", {
      host => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>muc_online_rooms</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>host</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/muc_online_rooms
    {
      "host": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *muc_unregister_nick* - Unregister the nick in the MUC service


Unregister the nick in the MUC service


### Arguments:
- *nick* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("muc_unregister_nick", new HashMap<String, Object>() {{
      put("nick", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("muc_unregister_nick", {
      nick => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>muc_unregister_nick</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>nick</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/muc_unregister_nick
    {
      "nick": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *num_active_users* - Get number of users active in the last days


Get number of users active in the last days


### Arguments:

*host* :: binary

: Name of host to check

*days* :: integer

: Number of days to calculate sum


### Result:

{users,integer}

: Number of users active on given server in last n days


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("num_active_users", new HashMap<String, Object>() {{
      put("host", "myserver.com");
      put("days", new Integer(3));
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("num_active_users", {
      host => "myserver.com",
      days => 3
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>num_active_users</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>host</name>
                <value>
                  <string>myserver.com</string>
                </value>
              </member>
              <member>
                <name>days</name>
                <value>
                  <integer>3</integer>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/num_active_users
    {
      "host": "myserver.com",
      "days": 3
    }
    
    HTTP/1.1 200 OK
        {"users": 123    }
~~~
{: .code-samples-tabs}



## *num_resources* - Get the number of resources of a user


Get the number of resources of a user


### Arguments:

*user* :: binary

: User name

*host* :: binary

: Server name


### Result:

{resources,integer}

: Number of active resources for a user


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("num_resources", new HashMap<String, Object>() {{
      put("user", "peter");
      put("host", "myserver.com");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("num_resources", {
      user => "peter",
      host => "myserver.com"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>num_resources</methodName>
      <params>
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
                <name>host</name>
                <value>
                  <string>myserver.com</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/num_resources
    {
      "user": "peter",
      "host": "myserver.com"
    }
    
    HTTP/1.1 200 OK
        {"resources": 5    }
~~~
{: .code-samples-tabs}



## *outgoing_s2s_number* - Number of outgoing s2s connections on the node


Number of outgoing s2s connections on the node


### Arguments:


### Result:
{s2s_outgoing,integer}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("outgoing_s2s_number", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("outgoing_s2s_number", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>outgoing_s2s_number</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/outgoing_s2s_number
    {
      
    }
    
    HTTP/1.1 200 OK
        {"s2s_outgoing": 1    }
~~~
{: .code-samples-tabs}



## *privacy_set* - Send a IQ set privacy stanza for a local account


Send a IQ set privacy stanza for a local account


### Arguments:
- *user* :: binary
- *host* :: binary
- *xmlquery* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("privacy_set", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("xmlquery", "ccccc");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("privacy_set", {
      user => "aaaaa",
      host => "bbbbb",
      xmlquery => "ccccc"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>privacy_set</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>xmlquery</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/privacy_set
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "xmlquery": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *private_get* - Get some information from a user private storage


Get some information from a user private storage


### Arguments:
- *user* :: binary
- *host* :: binary
- *element* :: binary
- *ns* :: binary


### Result:
{res,string}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("private_get", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("element", "ccccc");
      put("ns", "ddddd");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("private_get", {
      user => "aaaaa",
      host => "bbbbb",
      element => "ccccc",
      ns => "ddddd"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>private_get</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>element</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>ns</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/private_get
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "element": "ccccc",
      "ns": "ddddd"
    }
    
    HTTP/1.1 200 OK
        {"res": "aaaaa"    }
~~~
{: .code-samples-tabs}



## *private_set* - Set to the user private storage


Set to the user private storage


### Arguments:
- *user* :: binary
- *host* :: binary
- *element* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("private_set", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("element", "ccccc");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("private_set", {
      user => "aaaaa",
      host => "bbbbb",
      element => "ccccc"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>private_set</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>element</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/private_set
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "element": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *process_rosteritems* - List or delete rosteritems that match filtering options


Explanation of each argument:
 - action: what to do with each rosteritem that matches all the filtering options
 - subs: subscription type
 - asks: pending subscription
 - users: the JIDs of the local user
 - contacts: the JIDs of the contact in the roster

Allowed values in the arguments:
  ACTION = list | delete
  SUBS = SUB[:SUB]* | any
  SUB = none | from | to | both
  ASKS = ASK[:ASK]* | any
  ASK = none | out | in
  USERS = JID[:JID]* | any
  CONTACTS = JID[:JID]* | any
  JID = characters valid in a JID, and can use the globs: *, ?, ! and [...]

This example will list roster items with subscription 'none', 'from' or 'to' that have any ask property, of local users which JID is in the virtual host 'example.org' and that the contact JID is either a bare server name (without user part) or that has a user part and the server part contains the word 'icq':
  list none:from:to any *@example.org *:*@*icq*


### Arguments:
- *action* :: string
- *subs* :: string
- *asks* :: string
- *users* :: string
- *contacts* :: string


### Result:
{response,{list,{pairs,{tuple,[{user,string},{contact,string}]}}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("process_rosteritems", new HashMap<String, Object>() {{
      put("action", "aaaaa");
      put("subs", "bbbbb");
      put("asks", "ccccc");
      put("users", "ddddd");
      put("contacts", "eeeee");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("process_rosteritems", {
      action => "aaaaa",
      subs => "bbbbb",
      asks => "ccccc",
      users => "ddddd",
      contacts => "eeeee"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>process_rosteritems</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>action</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>subs</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>asks</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>users</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
              <member>
                <name>contacts</name>
                <value>
                  <string>eeeee</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/process_rosteritems
    {
      "action": "aaaaa",
      "subs": "bbbbb",
      "asks": "ccccc",
      "users": "ddddd",
      "contacts": "eeeee"
    }
    
    HTTP/1.1 200 OK
    [
      {
        "user": "aaaaa",
        "contact": "bbbbb"
      },
      {
        "user": "ccccc",
        "contact": "ddddd"
      }
    ]
~~~
{: .code-samples-tabs}



## *push_alltoall* - Add all the users to all the users of Host in Group


Add all the users to all the users of Host in Group


### Arguments:
- *host* :: binary
- *group* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("push_alltoall", new HashMap<String, Object>() {{
      put("host", "aaaaa");
      put("group", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("push_alltoall", {
      host => "aaaaa",
      group => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>push_alltoall</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>host</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>group</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/push_alltoall
    {
      "host": "aaaaa",
      "group": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *push_roster* - Push template roster from file to a user


Push template roster from file to a user


### Arguments:
- *file* :: binary
- *user* :: binary
- *host* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("push_roster", new HashMap<String, Object>() {{
      put("file", "aaaaa");
      put("user", "bbbbb");
      put("host", "ccccc");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("push_roster", {
      file => "aaaaa",
      user => "bbbbb",
      host => "ccccc"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>push_roster</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>user</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/push_roster
    {
      "file": "aaaaa",
      "user": "bbbbb",
      "host": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *push_roster_all* - Push template roster from file to all those users


Push template roster from file to all those users


### Arguments:
- *file* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("push_roster_all", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("push_roster_all", {
      file => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>push_roster_all</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/push_roster_all
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *register* - Register a user


Register a user


### Arguments:
- *user* :: binary
- *host* :: binary
- *password* :: binary


### Result:
{res,restuple}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("register", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("password", "ccccc");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("register", {
      user => "aaaaa",
      host => "bbbbb",
      password => "ccccc"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>register</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>password</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/register
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "password": "ccccc"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~
{: .code-samples-tabs}



## *registered_users* - List all registered users in HOST


List all registered users in HOST


### Arguments:
- *host* :: binary


### Result:
{users,{list,{username,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("registered_users", new HashMap<String, Object>() {{
      put("host", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("registered_users", {
      host => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>registered_users</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>host</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/registered_users
    {
      "host": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *registered_vhosts* - List all registered vhosts in SERVER


List all registered vhosts in SERVER


### Arguments:


### Result:
{vhosts,{list,{vhost,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("registered_vhosts", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("registered_vhosts", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>registered_vhosts</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/registered_vhosts
    {
      
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *reload_config* - Reload config file in memory (only affects ACL and Access)


Reload config file in memory (only affects ACL and Access)


### Arguments:


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("reload_config", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("reload_config", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>reload_config</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/reload_config
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *remove_node* - Remove an ejabberd node from Mnesia clustering config


Remove an ejabberd node from Mnesia clustering config


### Arguments:

*node* :: string

: Name of erlang node to remove


### Result:

{res,rescode}

: Status code: 0 on success, 1 otherwise


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("remove_node", new HashMap<String, Object>() {{
      put("node", "ejabberd@server2");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("remove_node", {
      node => "ejabberd@server2"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>remove_node</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>node</name>
                <value>
                  <string>ejabberd@server2</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/remove_node
    {
      "node": "ejabberd@server2"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *reopen_log* - Reopen the log files


Reopen the log files


### Arguments:


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("reopen_log", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("reopen_log", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>reopen_log</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/reopen_log
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *resource_num* - Resource string of a session number


Resource string of a session number


### Arguments:

*user* :: binary

: User name

*host* :: binary

: Server name

*num* :: integer

: ID of resource to return


### Result:

{resource,string}

: Name of user resource


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("resource_num", new HashMap<String, Object>() {{
      put("user", "peter");
      put("host", "myserver.com");
      put("num", new Integer(2));
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("resource_num", {
      user => "peter",
      host => "myserver.com",
      num => 2
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>resource_num</methodName>
      <params>
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
                <name>host</name>
                <value>
                  <string>myserver.com</string>
                </value>
              </member>
              <member>
                <name>num</name>
                <value>
                  <integer>2</integer>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/resource_num
    {
      "user": "peter",
      "host": "myserver.com",
      "num": 2
    }
    
    HTTP/1.1 200 OK
        {"resource": "Psi"    }
~~~
{: .code-samples-tabs}



## *restart* - Restart ejabberd gracefully


Restart ejabberd gracefully


### Arguments:


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("restart", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("restart", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>restart</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/restart
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *restore* - Restore the database from backup file


Restore the database from backup file


### Arguments:
- *file* :: string


### Result:
{res,restuple}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("restore", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("restore", {
      file => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>restore</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>file</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/restore
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~
{: .code-samples-tabs}



## *rooms_unused_destroy* - Destroy the rooms that are unused for many days in host


Destroy the rooms that are unused for many days in host


### Arguments:
- *host* :: binary
- *days* :: integer


### Result:
{rooms,{list,{room,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("rooms_unused_destroy", new HashMap<String, Object>() {{
      put("host", "aaaaa");
      put("days", new Integer(1));
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("rooms_unused_destroy", {
      host => "aaaaa",
      days => 1
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>rooms_unused_destroy</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>host</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>days</name>
                <value>
                  <integer>1</integer>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/rooms_unused_destroy
    {
      "host": "aaaaa",
      "days": 1
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *rooms_unused_list* - List the rooms that are unused for many days in host


List the rooms that are unused for many days in host


### Arguments:
- *host* :: binary
- *days* :: integer


### Result:
{rooms,{list,{room,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("rooms_unused_list", new HashMap<String, Object>() {{
      put("host", "aaaaa");
      put("days", new Integer(1));
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("rooms_unused_list", {
      host => "aaaaa",
      days => 1
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>rooms_unused_list</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>host</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>days</name>
                <value>
                  <integer>1</integer>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/rooms_unused_list
    {
      "host": "aaaaa",
      "days": 1
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *rotate_log* - Rotate the log files


Rotate the log files


### Arguments:


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("rotate_log", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("rotate_log", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>rotate_log</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/rotate_log
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *send_direct_invitation* - Send a direct invitation to several destinations


Password and Message can also be: none. Users JIDs are separated with : 


### Arguments:
- *name* :: binary
- *service* :: binary
- *password* :: binary
- *reason* :: binary
- *users* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("send_direct_invitation", new HashMap<String, Object>() {{
      put("name", "aaaaa");
      put("service", "bbbbb");
      put("password", "ccccc");
      put("reason", "ddddd");
      put("users", "eeeee");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("send_direct_invitation", {
      name => "aaaaa",
      service => "bbbbb",
      password => "ccccc",
      reason => "ddddd",
      users => "eeeee"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>send_direct_invitation</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>name</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>service</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>password</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>reason</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
              <member>
                <name>users</name>
                <value>
                  <string>eeeee</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/send_direct_invitation
    {
      "name": "aaaaa",
      "service": "bbbbb",
      "password": "ccccc",
      "reason": "ddddd",
      "users": "eeeee"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *send_message* - Send a message to a local or remote bare of full JID


Send a message to a local or remote bare of full JID


### Arguments:
- *type* :: binary
- *from* :: binary
- *to* :: binary
- *subject* :: binary
- *body* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("send_message", new HashMap<String, Object>() {{
      put("type", "aaaaa");
      put("from", "bbbbb");
      put("to", "ccccc");
      put("subject", "ddddd");
      put("body", "eeeee");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("send_message", {
      type => "aaaaa",
      from => "bbbbb",
      to => "ccccc",
      subject => "ddddd",
      body => "eeeee"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>send_message</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>type</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>from</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>to</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>subject</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
              <member>
                <name>body</name>
                <value>
                  <string>eeeee</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/send_message
    {
      "type": "aaaaa",
      "from": "bbbbb",
      "to": "ccccc",
      "subject": "ddddd",
      "body": "eeeee"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *send_stanza* - Send a stanza; provide From JID and valid To JID


Send a stanza; provide From JID and valid To JID


### Arguments:
- *from* :: binary
- *to* :: binary
- *stanza* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("send_stanza", new HashMap<String, Object>() {{
      put("from", "aaaaa");
      put("to", "bbbbb");
      put("stanza", "ccccc");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("send_stanza", {
      from => "aaaaa",
      to => "bbbbb",
      stanza => "ccccc"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>send_stanza</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>from</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>to</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>stanza</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/send_stanza
    {
      "from": "aaaaa",
      "to": "bbbbb",
      "stanza": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *send_stanza_c2s* - Send a stanza as if sent from a c2s session


Send a stanza as if sent from a c2s session


### Arguments:
- *user* :: binary
- *host* :: binary
- *resource* :: binary
- *stanza* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("send_stanza_c2s", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("resource", "ccccc");
      put("stanza", "ddddd");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("send_stanza_c2s", {
      user => "aaaaa",
      host => "bbbbb",
      resource => "ccccc",
      stanza => "ddddd"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>send_stanza_c2s</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>resource</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>stanza</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/send_stanza_c2s
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "resource": "ccccc",
      "stanza": "ddddd"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *set_last* - Set last activity information


Timestamp is the seconds since1970-01-01 00:00:00 UTC, for example: date +%s


### Arguments:
- *user* :: binary
- *host* :: binary
- *timestamp* :: integer
- *status* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("set_last", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("timestamp", new Integer(1));
      put("status", "ccccc");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("set_last", {
      user => "aaaaa",
      host => "bbbbb",
      timestamp => 1,
      status => "ccccc"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>set_last</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>timestamp</name>
                <value>
                  <integer>1</integer>
                </value>
              </member>
              <member>
                <name>status</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/set_last
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "timestamp": 1,
      "status": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *set_loglevel* - Set the loglevel (0 to 5)


Set the loglevel (0 to 5)


### Arguments:
- *loglevel* :: integer


### Result:
{logger,atom}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("set_loglevel", new HashMap<String, Object>() {{
      put("loglevel", new Integer(1));
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("set_loglevel", {
      loglevel => 1
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>set_loglevel</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>loglevel</name>
                <value>
                  <integer>1</integer>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/set_loglevel
    {
      "loglevel": 1
    }
    
    HTTP/1.1 200 OK
        {"logger": "aaaaa"    }
~~~
{: .code-samples-tabs}



## *set_master* - Set master node of the clustered Mnesia tables


If you provide as nodename "self", this node will be set as its own master.


### Arguments:
- *nodename* :: string


### Result:
{res,restuple}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("set_master", new HashMap<String, Object>() {{
      put("nodename", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("set_master", {
      nodename => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>set_master</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>nodename</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/set_master
    {
      "nodename": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~
{: .code-samples-tabs}



## *set_nickname* - Set nickname in a user's vCard


Set nickname in a user's vCard


### Arguments:
- *user* :: binary
- *host* :: binary
- *nickname* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("set_nickname", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("nickname", "ccccc");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("set_nickname", {
      user => "aaaaa",
      host => "bbbbb",
      nickname => "ccccc"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>set_nickname</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>nickname</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/set_nickname
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "nickname": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *set_presence* - Set presence of a session


Set presence of a session


### Arguments:
- *user* :: binary
- *host* :: binary
- *resource* :: binary
- *type* :: binary
- *show* :: binary
- *status* :: binary
- *priority* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("set_presence", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("resource", "ccccc");
      put("type", "ddddd");
      put("show", "eeeee");
      put("status", "fffff");
      put("priority", "ggggg");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("set_presence", {
      user => "aaaaa",
      host => "bbbbb",
      resource => "ccccc",
      type => "ddddd",
      show => "eeeee",
      status => "fffff",
      priority => "ggggg"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>set_presence</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>resource</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>type</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
              <member>
                <name>show</name>
                <value>
                  <string>eeeee</string>
                </value>
              </member>
              <member>
                <name>status</name>
                <value>
                  <string>fffff</string>
                </value>
              </member>
              <member>
                <name>priority</name>
                <value>
                  <string>ggggg</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/set_presence
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "resource": "ccccc",
      "type": "ddddd",
      "show": "eeeee",
      "status": "fffff",
      "priority": "ggggg"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *set_room_affiliation* - Change an affiliation in a MUC room


Change an affiliation in a MUC room


### Arguments:
- *name* :: binary
- *service* :: binary
- *jid* :: binary
- *affiliation* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("set_room_affiliation", new HashMap<String, Object>() {{
      put("name", "aaaaa");
      put("service", "bbbbb");
      put("jid", "ccccc");
      put("affiliation", "ddddd");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("set_room_affiliation", {
      name => "aaaaa",
      service => "bbbbb",
      jid => "ccccc",
      affiliation => "ddddd"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>set_room_affiliation</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>name</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>service</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>jid</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>affiliation</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/set_room_affiliation
    {
      "name": "aaaaa",
      "service": "bbbbb",
      "jid": "ccccc",
      "affiliation": "ddddd"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *set_vcard* - Set content in a vCard field


Some vcard field names in get/set_vcard are:
 FN		- Full Name
 NICKNAME	- Nickname
 BDAY		- Birthday
 TITLE		- Work: Position
 ROLE		- Work: Role
Some vcard field names and subnames in get/set_vcard2 are:
 N FAMILY	- Family name
 N GIVEN	- Given name
 N MIDDLE	- Middle name
 ADR CTRY	- Address: Country
 ADR LOCALITY	- Address: City
 TEL HOME      - Telephone: Home
 TEL CELL      - Telephone: Cellphone
 TEL WORK      - Telephone: Work
 TEL VOICE     - Telephone: Voice
 EMAIL USERID	- E-Mail Address
 ORG ORGNAME	- Work: Company
 ORG ORGUNIT	- Work: Department

For a full list of vCard fields check XEP-0054: vcard-temp at http://www.xmpp.org/extensions/xep-0054.html


### Arguments:
- *user* :: binary
- *host* :: binary
- *name* :: binary
- *content* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("set_vcard", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("name", "ccccc");
      put("content", "ddddd");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("set_vcard", {
      user => "aaaaa",
      host => "bbbbb",
      name => "ccccc",
      content => "ddddd"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>set_vcard</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>name</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>content</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/set_vcard
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "name": "ccccc",
      "content": "ddddd"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *set_vcard2* - Set content in a vCard subfield


Some vcard field names and subnames in get/set_vcard2 are:
 N FAMILY	- Family name
 N GIVEN	- Given name
 N MIDDLE	- Middle name
 ADR CTRY	- Address: Country
 ADR LOCALITY	- Address: City
 TEL HOME      - Telephone: Home
 TEL CELL      - Telephone: Cellphone
 TEL WORK      - Telephone: Work
 TEL VOICE     - Telephone: Voice
 EMAIL USERID	- E-Mail Address
 ORG ORGNAME	- Work: Company
 ORG ORGUNIT	- Work: Department

Some vcard field names in get/set_vcard are:
 FN		- Full Name
 NICKNAME	- Nickname
 BDAY		- Birthday
 TITLE		- Work: Position
 ROLE		- Work: Role
For a full list of vCard fields check XEP-0054: vcard-temp at http://www.xmpp.org/extensions/xep-0054.html


### Arguments:
- *user* :: binary
- *host* :: binary
- *name* :: binary
- *subname* :: binary
- *content* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("set_vcard2", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("name", "ccccc");
      put("subname", "ddddd");
      put("content", "eeeee");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("set_vcard2", {
      user => "aaaaa",
      host => "bbbbb",
      name => "ccccc",
      subname => "ddddd",
      content => "eeeee"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>set_vcard2</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>name</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>subname</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
              <member>
                <name>content</name>
                <value>
                  <string>eeeee</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/set_vcard2
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "name": "ccccc",
      "subname": "ddddd",
      "content": "eeeee"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *set_vcard2_multi* - Set multiple contents in a vCard subfield


Some vcard field names and subnames in get/set_vcard2 are:
 N FAMILY	- Family name
 N GIVEN	- Given name
 N MIDDLE	- Middle name
 ADR CTRY	- Address: Country
 ADR LOCALITY	- Address: City
 TEL HOME      - Telephone: Home
 TEL CELL      - Telephone: Cellphone
 TEL WORK      - Telephone: Work
 TEL VOICE     - Telephone: Voice
 EMAIL USERID	- E-Mail Address
 ORG ORGNAME	- Work: Company
 ORG ORGUNIT	- Work: Department

Some vcard field names in get/set_vcard are:
 FN		- Full Name
 NICKNAME	- Nickname
 BDAY		- Birthday
 TITLE		- Work: Position
 ROLE		- Work: Role
For a full list of vCard fields check XEP-0054: vcard-temp at http://www.xmpp.org/extensions/xep-0054.html


### Arguments:
- *user* :: binary
- *host* :: binary
- *name* :: binary
- *subname* :: binary
- *contents* :: {list,{value,binary}}


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("set_vcard2_multi", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("name", "ccccc");
      put("subname", "ddddd");
      put("contents", new Object[] {
        new HashMap<String, Object>() {{ put("value", "eeeee"); }},
        new HashMap<String, Object>() {{ put("value", "fffff"); }}
      });
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("set_vcard2_multi", {
      user => "aaaaa",
      host => "bbbbb",
      name => "ccccc",
      subname => "ddddd",
      contents => [{value => "eeeee"}, {value => "fffff"}]
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>set_vcard2_multi</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>name</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>subname</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
              <member>
                <name>contents</name>
                <value>
                  <array>
                    <data>
                      <value>
                        <struct>
                          <member>
                            <name>value</name>
                            <value>
                              <string>eeeee</string>
                            </value>
                          </member>
                        </struct>
                      </value>
                      <value>
                        <struct>
                          <member>
                            <name>value</name>
                            <value>
                              <string>fffff</string>
                            </value>
                          </member>
                        </struct>
                      </value>
                    </data>
                  </array>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/set_vcard2_multi
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "name": "ccccc",
      "subname": "ddddd",
      "contents": [
        "eeeee",
        "fffff"
      ]
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *srg_create* - Create a Shared Roster Group


If you want to specify several group identifiers in the Display argument,
put  \ " around the argument and
separate the identifiers with \ \ n
For example:
  ejabberdctl srg_create group3 localhost name desc \"group1\\ngroup2\"


### Arguments:
- *group* :: binary
- *host* :: binary
- *name* :: binary
- *description* :: binary
- *display* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("srg_create", new HashMap<String, Object>() {{
      put("group", "aaaaa");
      put("host", "bbbbb");
      put("name", "ccccc");
      put("description", "ddddd");
      put("display", "eeeee");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("srg_create", {
      group => "aaaaa",
      host => "bbbbb",
      name => "ccccc",
      description => "ddddd",
      display => "eeeee"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>srg_create</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>group</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>name</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>description</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
              <member>
                <name>display</name>
                <value>
                  <string>eeeee</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/srg_create
    {
      "group": "aaaaa",
      "host": "bbbbb",
      "name": "ccccc",
      "description": "ddddd",
      "display": "eeeee"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *srg_delete* - Delete a Shared Roster Group


Delete a Shared Roster Group


### Arguments:
- *group* :: binary
- *host* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("srg_delete", new HashMap<String, Object>() {{
      put("group", "aaaaa");
      put("host", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("srg_delete", {
      group => "aaaaa",
      host => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>srg_delete</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>group</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/srg_delete
    {
      "group": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *srg_get_info* - Get info of a Shared Roster Group


Get info of a Shared Roster Group


### Arguments:
- *group* :: binary
- *host* :: binary


### Result:
{informations,{list,{information,{tuple,[{key,string},{value,string}]}}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("srg_get_info", new HashMap<String, Object>() {{
      put("group", "aaaaa");
      put("host", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("srg_get_info", {
      group => "aaaaa",
      host => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>srg_get_info</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>group</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/srg_get_info
    {
      "group": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    [
      {
        "key": "aaaaa",
        "value": "bbbbb"
      },
      {
        "key": "ccccc",
        "value": "ddddd"
      }
    ]
~~~
{: .code-samples-tabs}



## *srg_get_members* - Get members of a Shared Roster Group


Get members of a Shared Roster Group


### Arguments:
- *group* :: binary
- *host* :: binary


### Result:
{members,{list,{member,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("srg_get_members", new HashMap<String, Object>() {{
      put("group", "aaaaa");
      put("host", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("srg_get_members", {
      group => "aaaaa",
      host => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>srg_get_members</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>group</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/srg_get_members
    {
      "group": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *srg_list* - List the Shared Roster Groups in Host


List the Shared Roster Groups in Host


### Arguments:
- *host* :: binary


### Result:
{groups,{list,{id,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("srg_list", new HashMap<String, Object>() {{
      put("host", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("srg_list", {
      host => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>srg_list</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>host</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/srg_list
    {
      "host": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *srg_user_add* - Add the JID user@host to the Shared Roster Group


Add the JID user@host to the Shared Roster Group


### Arguments:
- *user* :: binary
- *host* :: binary
- *group* :: binary
- *grouphost* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("srg_user_add", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("group", "ccccc");
      put("grouphost", "ddddd");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("srg_user_add", {
      user => "aaaaa",
      host => "bbbbb",
      group => "ccccc",
      grouphost => "ddddd"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>srg_user_add</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>group</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>grouphost</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/srg_user_add
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "group": "ccccc",
      "grouphost": "ddddd"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *srg_user_del* - Delete this JID user@host from the Shared Roster Group


Delete this JID user@host from the Shared Roster Group


### Arguments:
- *user* :: binary
- *host* :: binary
- *group* :: binary
- *grouphost* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("srg_user_del", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("group", "ccccc");
      put("grouphost", "ddddd");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("srg_user_del", {
      user => "aaaaa",
      host => "bbbbb",
      group => "ccccc",
      grouphost => "ddddd"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>srg_user_del</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>group</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>grouphost</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/srg_user_del
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "group": "ccccc",
      "grouphost": "ddddd"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *stats* - Get statistical value: registeredusers onlineusers onlineusersnode uptimeseconds processes


Get statistical value: registeredusers onlineusers onlineusersnode uptimeseconds processes


### Arguments:
- *name* :: binary


### Result:
{stat,integer}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("stats", new HashMap<String, Object>() {{
      put("name", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("stats", {
      name => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>stats</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>name</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/stats
    {
      "name": "aaaaa"
    }
    
    HTTP/1.1 200 OK
        {"stat": 1    }
~~~
{: .code-samples-tabs}



## *stats_host* - Get statistical value for this host: registeredusers onlineusers


Get statistical value for this host: registeredusers onlineusers


### Arguments:
- *name* :: binary
- *host* :: binary


### Result:
{stat,integer}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("stats_host", new HashMap<String, Object>() {{
      put("name", "aaaaa");
      put("host", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("stats_host", {
      name => "aaaaa",
      host => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>stats_host</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>name</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/stats_host
    {
      "name": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
        {"stat": 1    }
~~~
{: .code-samples-tabs}



## *status* - Get status of the ejabberd server


Get status of the ejabberd server


### Arguments:


### Result:
{res,restuple}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("status", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("status", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>status</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/status
    {
      
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~
{: .code-samples-tabs}



## *status_list* - List of logged users with this status


List of logged users with this status


### Arguments:
- *status* :: binary


### Result:
{users,{list,{userstatus,{tuple,[{user,string},
                                 {host,string},
                                 {resource,string},
                                 {priority,integer},
                                 {status,string}]}}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("status_list", new HashMap<String, Object>() {{
      put("status", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("status_list", {
      status => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>status_list</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>status</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/status_list
    {
      "status": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    [
      {
        "user": "aaaaa",
        "host": "bbbbb",
        "resource": "ccccc",
        "priority": 1,
        "status": "ddddd"
      },
      {
        "user": "eeeee",
        "host": "fffff",
        "resource": "ggggg",
        "priority": 2,
        "status": "hhhhh"
      }
    ]
~~~
{: .code-samples-tabs}



## *status_list_host* - List of users logged in host with their statuses


List of users logged in host with their statuses


### Arguments:
- *host* :: binary
- *status* :: binary


### Result:
{users,{list,{userstatus,{tuple,[{user,string},
                                 {host,string},
                                 {resource,string},
                                 {priority,integer},
                                 {status,string}]}}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("status_list_host", new HashMap<String, Object>() {{
      put("host", "aaaaa");
      put("status", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("status_list_host", {
      host => "aaaaa",
      status => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>status_list_host</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>host</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>status</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/status_list_host
    {
      "host": "aaaaa",
      "status": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    [
      {
        "user": "aaaaa",
        "host": "bbbbb",
        "resource": "ccccc",
        "priority": 1,
        "status": "ddddd"
      },
      {
        "user": "eeeee",
        "host": "fffff",
        "resource": "ggggg",
        "priority": 2,
        "status": "hhhhh"
      }
    ]
~~~
{: .code-samples-tabs}



## *status_num* - Number of logged users with this status


Number of logged users with this status


### Arguments:

*status* :: binary

: Status type to check


### Result:

{users,integer}

: Number of connected sessions with given status type


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("status_num", new HashMap<String, Object>() {{
      put("status", "dnd");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("status_num", {
      status => "dnd"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>status_num</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>status</name>
                <value>
                  <string>dnd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/status_num
    {
      "status": "dnd"
    }
    
    HTTP/1.1 200 OK
        {"users": 23    }
~~~
{: .code-samples-tabs}



## *status_num_host* - Number of logged users with this status in host


Number of logged users with this status in host


### Arguments:

*host* :: binary

: Server name

*status* :: binary

: Status type to check


### Result:

{users,integer}

: Number of connected sessions with given status type


### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("status_num_host", new HashMap<String, Object>() {{
      put("host", "myserver.com");
      put("status", "dnd");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("status_num_host", {
      host => "myserver.com",
      status => "dnd"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>status_num_host</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>host</name>
                <value>
                  <string>myserver.com</string>
                </value>
              </member>
              <member>
                <name>status</name>
                <value>
                  <string>dnd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/status_num_host
    {
      "host": "myserver.com",
      "status": "dnd"
    }
    
    HTTP/1.1 200 OK
        {"users": 23    }
~~~
{: .code-samples-tabs}



## *stop* - Stop ejabberd gracefully


Stop ejabberd gracefully


### Arguments:


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("stop", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("stop", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>stop</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/stop
    {
      
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *stop_kindly* - Inform users and rooms, wait, and stop the server


Provide the delay in seconds, and the announcement quoted, for example: 
ejabberdctl stop_kindly 60 \"The server will stop in one minute.\"


### Arguments:
- *delay* :: integer
- *announcement* :: string


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("stop_kindly", new HashMap<String, Object>() {{
      put("delay", new Integer(1));
      put("announcement", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("stop_kindly", {
      delay => 1,
      announcement => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>stop_kindly</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>delay</name>
                <value>
                  <integer>1</integer>
                </value>
              </member>
              <member>
                <name>announcement</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/stop_kindly
    {
      "delay": 1,
      "announcement": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *subscribe_room* - Subscribe to a MUC conference


Subscribe to a MUC conference


### Arguments:
- *user* :: binary
- *nick* :: binary
- *room* :: binary
- *nodes* :: binary


### Result:
{nodes,{list,{node,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("subscribe_room", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("nick", "bbbbb");
      put("room", "ccccc");
      put("nodes", "ddddd");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("subscribe_room", {
      user => "aaaaa",
      nick => "bbbbb",
      room => "ccccc",
      nodes => "ddddd"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>subscribe_room</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>nick</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
              <member>
                <name>room</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>nodes</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/subscribe_room
    {
      "user": "aaaaa",
      "nick": "bbbbb",
      "room": "ccccc",
      "nodes": "ddddd"
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *unregister* - Unregister a user


Unregister a user


### Arguments:
- *user* :: binary
- *host* :: binary


### Result:
{res,restuple}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("unregister", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("unregister", {
      user => "aaaaa",
      host => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>unregister</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/unregister
    {
      "user": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~
{: .code-samples-tabs}



## *unsubscribe_room* - Unsubscribe from a MUC conference


Unsubscribe from a MUC conference


### Arguments:
- *user* :: binary
- *room* :: binary


### Result:
{res,rescode}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("unsubscribe_room", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("room", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("unsubscribe_room", {
      user => "aaaaa",
      room => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>unsubscribe_room</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>room</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/unsubscribe_room
    {
      "user": "aaaaa",
      "room": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    ""
~~~
{: .code-samples-tabs}



## *update* - Update the given module, or use the keyword: all


Update the given module, or use the keyword: all


### Arguments:
- *module* :: string


### Result:
{res,restuple}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("update", new HashMap<String, Object>() {{
      put("module", "aaaaa");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("update", {
      module => "aaaaa"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>update</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>module</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/update
    {
      "module": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"
~~~
{: .code-samples-tabs}



## *update_list* - List modified modules that can be updated


List modified modules that can be updated


### Arguments:


### Result:
{modules,{list,{module,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("update_list", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("update_list", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>update_list</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/update_list
    {
      
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *user_resources* - List user's connected resources


List user's connected resources


### Arguments:


### Result:
{resources,{list,{resource,string}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("user_resources", new HashMap<String, Object>() {{
      
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("user_resources", {
      
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>user_resources</methodName>
      <params>
        <param>
          <value>
            <struct>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/user_resources
    {
      
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]
~~~
{: .code-samples-tabs}



## *user_sessions_info* - Get information about all sessions of a user


Get information about all sessions of a user


### Arguments:
- *user* :: binary
- *host* :: binary


### Result:
{sessions_info,{list,{session,{tuple,[{connection,string},
                                      {ip,string},
                                      {port,integer},
                                      {priority,integer},
                                      {node,string},
                                      {uptime,integer},
                                      {status,string},
                                      {resource,string},
                                      {statustext,string}]}}}}

### Examples:

* Java
* Perl
* XmlRPC
* JSON
{: .code-samples-labels}

* ~~~ java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("user_sessions_info", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
    }});
~~~

* ~~~ perl
    XMLRPC::Lite->proxy($url)->call("user_sessions_info", {
      user => "aaaaa",
      host => "bbbbb"
    })->results()
~~~

* ~~~ xml
    <methodCall>
      <methodName>user_sessions_info</methodName>
      <params>
        <param>
          <value>
            <struct>
              <member>
                <name>user</name>
                <value>
                  <string>aaaaa</string>
                </value>
              </member>
              <member>
                <name>host</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
~~~

* ~~~ json
    POST /api/user_sessions_info
    {
      "user": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    [
      {
        "connection": "aaaaa",
        "ip": "bbbbb",
        "port": 1,
        "priority": 2,
        "node": "ccccc",
        "uptime": 3,
        "status": "ddddd",
        "resource": "eeeee",
        "statustext": "fffff"
      },
      {
        "connection": "ggggg",
        "ip": "hhhhh",
        "port": 4,
        "priority": 5,
        "node": "iiiii",
        "uptime": 6,
        "status": "jjjjj",
        "resource": "kkkkk",
        "statustext": "lllll"
      }
    ]
~~~
{: .code-samples-tabs}


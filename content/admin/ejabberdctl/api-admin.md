
## *add_rosteritem* - Add an item to a user's roster (supports ODBC)


Add an item to a user's roster (supports ODBC)

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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
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

## *backup* - Store the database to backup file


Store the database to backup file

### Arguments:
- *file* :: string

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("backup", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/backup
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"

## *ban_account* - Ban an account: kick sessions and set random password


Ban an account: kick sessions and set random password

### Arguments:
- *user* :: binary
- *host* :: binary
- *reason* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("ban_account", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("reason", "ccccc");
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>ban_account</methodName>
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
                <name>reason</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
JSON example


    #!json
    POST /api/ban_account
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "reason": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""

## *change_password* - Change the password of an account


Change the password of an account

### Arguments:
- *user* :: binary
- *host* :: binary
- *newpass* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("change_password", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("newpass", "ccccc");
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>change_password</methodName>
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
                <name>newpass</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
JSON example


    #!json
    POST /api/change_password
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "newpass": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""

## *check_account* - Check if an account exists or not


Check if an account exists or not

### Arguments:
- *user* :: binary
- *host* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("check_account", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>check_account</methodName>
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
JSON example


    #!json
    POST /api/check_account
    {
      "user": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    ""

## *check_password* - Check if a password is correct


Check if a password is correct

### Arguments:
- *user* :: binary
- *host* :: binary
- *password* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("check_password", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("password", "ccccc");
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>check_password</methodName>
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
JSON example


    #!json
    POST /api/check_password
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "password": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""

## *check_password_hash* - Check if the password hash is correct


Allowed hash methods: md5, sha.

### Arguments:
- *user* :: binary
- *host* :: binary
- *passwordhash* :: string
- *hashmethod* :: string

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("check_password_hash", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("passwordhash", "ccccc");
      put("hashmethod", "ddddd");
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>check_password_hash</methodName>
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
                <name>passwordhash</name>
                <value>
                  <string>ccccc</string>
                </value>
              </member>
              <member>
                <name>hashmethod</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
JSON example


    #!json
    POST /api/check_password_hash
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "passwordhash": "ccccc",
      "hashmethod": "ddddd"
    }
    
    HTTP/1.1 200 OK
    ""

## *compile* - Recompile and reload Erlang source code file


Recompile and reload Erlang source code file

### Arguments:
- *file* :: string

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("compile", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>compile</methodName>
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
JSON example


    #!json
    POST /api/compile
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""

## *connected_users* - List all established sessions


List all established sessions

### Arguments:

### Result:
{connected_users,{list,{sessions,string}}}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("connected_users", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/connected_users
    {
      
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]

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
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("connected_users_info", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
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

## *connected_users_number* - Get the number of established sessions


Get the number of established sessions

### Arguments:

### Result:
{num_sessions,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("connected_users_number", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/connected_users_number
    {
      
    }
    
    HTTP/1.1 200 OK
        {"num_sessions": 1    }

## *connected_users_vhost* - Get the list of established sessions in a vhost


Get the list of established sessions in a vhost

### Arguments:
- *host* :: binary

### Result:
{connected_users_vhost,{list,{sessions,string}}}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("connected_users_vhost", new HashMap<String, Object>() {{
      put("host", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/connected_users_vhost
    {
      "host": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]

## *convert_to_scram* - Convert the passwords in 'users' ODBC table to SCRAM


Convert the passwords in 'users' ODBC table to SCRAM

### Arguments:
- *host* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("convert_to_scram", new HashMap<String, Object>() {{
      put("host", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/convert_to_scram
    {
      "host": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""

## *convert_to_yaml* - Convert the input file from Erlang to YAML format


Convert the input file from Erlang to YAML format

### Arguments:
- *in* :: string
- *out* :: string

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("convert_to_yaml", new HashMap<String, Object>() {{
      put("in", "aaaaa");
      put("out", "bbbbb");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/convert_to_yaml
    {
      "in": "aaaaa",
      "out": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    ""

## *delete_expired_messages* - Delete expired offline messages from database


Delete expired offline messages from database

### Arguments:

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("delete_expired_messages", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/delete_expired_messages
    {
      
    }
    
    HTTP/1.1 200 OK
    ""

## *delete_old_messages* - Delete offline messages older than DAYS


Delete offline messages older than DAYS

### Arguments:
- *days* :: integer

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("delete_old_messages", new HashMap<String, Object>() {{
      put("days", new Integer(1));
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/delete_old_messages
    {
      "days": 1
    }
    
    HTTP/1.1 200 OK
    ""

## *delete_old_users* - Delete users that didn't log in last days, or that never logged


Delete users that didn't log in last days, or that never logged

### Arguments:
- *days* :: integer

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("delete_old_users", new HashMap<String, Object>() {{
      put("days", new Integer(1));
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>delete_old_users</methodName>
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
JSON example


    #!json
    POST /api/delete_old_users
    {
      "days": 1
    }
    
    HTTP/1.1 200 OK
    "Success"

## *delete_old_users_vhost* - Delete users that didn't log in last days in vhost, or that never logged


Delete users that didn't log in last days in vhost, or that never logged

### Arguments:
- *host* :: binary
- *days* :: integer

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("delete_old_users_vhost", new HashMap<String, Object>() {{
      put("host", "aaaaa");
      put("days", new Integer(1));
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>delete_old_users_vhost</methodName>
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
JSON example


    #!json
    POST /api/delete_old_users_vhost
    {
      "host": "aaaaa",
      "days": 1
    }
    
    HTTP/1.1 200 OK
    "Success"

## *delete_rosteritem* - Delete an item from a user's roster (supports ODBC)


Delete an item from a user's roster (supports ODBC)

### Arguments:
- *localuser* :: binary
- *localserver* :: binary
- *user* :: binary
- *server* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/delete_rosteritem
    {
      "localuser": "aaaaa",
      "localserver": "bbbbb",
      "user": "ccccc",
      "server": "ddddd"
    }
    
    HTTP/1.1 200 OK
    ""

## *dump* - Dump the database to text file


Dump the database to text file

### Arguments:
- *file* :: string

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("dump", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/dump
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"

## *dump_table* - Dump a table to text file


Dump a table to text file

### Arguments:
- *file* :: string
- *table* :: string

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("dump_table", new HashMap<String, Object>() {{
      put("file", "aaaaa");
      put("table", "bbbbb");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/dump_table
    {
      "file": "aaaaa",
      "table": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    "Success"

## *export2odbc* - Export virtual host information from Mnesia tables to SQL files


Export virtual host information from Mnesia tables to SQL files

### Arguments:
- *host* :: string
- *directory* :: string

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("export2odbc", new HashMap<String, Object>() {{
      put("host", "aaaaa");
      put("directory", "bbbbb");
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>export2odbc</methodName>
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
                <name>directory</name>
                <value>
                  <string>bbbbb</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
JSON example


    #!json
    POST /api/export2odbc
    {
      "host": "aaaaa",
      "directory": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    ""

## *export_odbc* - Export all tables as SQL queries to a file


Export all tables as SQL queries to a file

### Arguments:
- *host* :: string
- *file* :: string

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("export_odbc", new HashMap<String, Object>() {{
      put("host", "aaaaa");
      put("file", "bbbbb");
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>export_odbc</methodName>
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
JSON example


    #!json
    POST /api/export_odbc
    {
      "host": "aaaaa",
      "file": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    ""

## *export_piefxis* - Export data of all users in the server to PIEFXIS files (XEP-0227)


Export data of all users in the server to PIEFXIS files (XEP-0227)

### Arguments:
- *dir* :: string

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("export_piefxis", new HashMap<String, Object>() {{
      put("dir", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/export_piefxis
    {
      "dir": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""

## *export_piefxis_host* - Export data of users in a host to PIEFXIS files (XEP-0227)


Export data of users in a host to PIEFXIS files (XEP-0227)

### Arguments:
- *dir* :: string
- *host* :: string

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("export_piefxis_host", new HashMap<String, Object>() {{
      put("dir", "aaaaa");
      put("host", "bbbbb");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/export_piefxis_host
    {
      "dir": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    ""

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

0 if command failed, 1 when succedded

### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("gen_html_doc_for_commands", new HashMap<String, Object>() {{
      put("file", "/home/me/docs/api.html");
      put("regexp", "mod_admin");
      put("examples", "java,json");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/gen_html_doc_for_commands
    {
      "file": "/home/me/docs/api.html",
      "regexp": "mod_admin",
      "examples": "java,json"
    }
    
    HTTP/1.1 200 OK
    ""

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

0 if command failed, 1 when succedded

### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("gen_markdown_doc_for_commands", new HashMap<String, Object>() {{
      put("file", "/home/me/docs/api.html");
      put("regexp", "mod_admin");
      put("examples", "java,json");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/gen_markdown_doc_for_commands
    {
      "file": "/home/me/docs/api.html",
      "regexp": "mod_admin",
      "examples": "java,json"
    }
    
    HTTP/1.1 200 OK
    ""

## *get_cookie* - Get the Erlang cookie of this node


Get the Erlang cookie of this node

### Arguments:

### Result:
{cookie,string}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_cookie", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/get_cookie
    {
      
    }
    
    HTTP/1.1 200 OK
        {"cookie": "aaaaa"    }

## *get_last* - Get last activity information (timestamp and status)


Timestamp is the seconds since1970-01-01 00:00:00 UTC, for example: date +%s

### Arguments:
- *user* :: binary
- *host* :: binary

### Result:
{last_activity,string}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_last", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/get_last
    {
      "user": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
        {"last_activity": "aaaaa"    }

## *get_loglevel* - Get the current loglevel


Get the current loglevel

### Arguments:

### Result:
{leveltuple,{tuple,[{levelnumber,integer},
                    {levelatom,atom},
                    {leveldesc,string}]}}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_loglevel", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/get_loglevel
    {
      
    }
    
    HTTP/1.1 200 OK
    {
      "levelnumber": 1,
      "levelatom": "aaaaa",
      "leveldesc": "bbbbb"
    }

## *get_offline_count* - Get the number of unread offline messages


Get the number of unread offline messages

### Arguments:

### Result:
{res,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_offline_count", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/get_offline_count
    {
      
    }
    
    HTTP/1.1 200 OK
        {"res": 1    }

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
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_roster", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
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
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("get_vcard", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("name", "ccccc");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/get_vcard
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "name": "ccccc"
    }
    
    HTTP/1.1 200 OK
        {"content": "aaaaa"    }

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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/get_vcard2
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "name": "ccccc",
      "subname": "ddddd"
    }
    
    HTTP/1.1 200 OK
        {"content": "aaaaa"    }

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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
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

## *import_dir* - Import users data from jabberd14 spool dir


Import users data from jabberd14 spool dir

### Arguments:
- *file* :: string

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("import_dir", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/import_dir
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"

## *import_file* - Import user data from jabberd14 spool file


Import user data from jabberd14 spool file

### Arguments:
- *file* :: string

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("import_file", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/import_file
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"

## *import_piefxis* - Import users data from a PIEFXIS file (XEP-0227)


Import users data from a PIEFXIS file (XEP-0227)

### Arguments:
- *file* :: string

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("import_piefxis", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/import_piefxis
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""

## *incoming_s2s_number* - Number of incoming s2s connections on the node


Number of incoming s2s connections on the node

### Arguments:

### Result:
{s2s_incoming,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("incoming_s2s_number", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/incoming_s2s_number
    {
      
    }
    
    HTTP/1.1 200 OK
        {"s2s_incoming": 1    }

## *install_fallback* - Install the database from a fallback file


Install the database from a fallback file

### Arguments:
- *file* :: string

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("install_fallback", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/install_fallback
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"

## *join_cluster* - Join this node into the cluster handled by Node


Join this node into the cluster handled by Node

### Arguments:
- *node* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("join_cluster", new HashMap<String, Object>() {{
      put("node", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/join_cluster
    {
      "node": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""

## *kick_session* - Kick a user session


Kick a user session

### Arguments:
- *user* :: binary
- *host* :: binary
- *resource* :: binary
- *reason* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("kick_session", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("resource", "ccccc");
      put("reason", "ddddd");
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>kick_session</methodName>
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
                <name>reason</name>
                <value>
                  <string>ddddd</string>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
JSON example


    #!json
    POST /api/kick_session
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "resource": "ccccc",
      "reason": "ddddd"
    }
    
    HTTP/1.1 200 OK
    ""

## *kick_user* - Disconnect user's active sessions


Disconnect user's active sessions

### Arguments:
- *user* :: binary
- *host* :: binary

### Result:
{num_resources,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("kick_user", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/kick_user
    {
      "user": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
        {"num_resources": 1    }

## *leave_cluster* - Remove node handled by Node from the cluster


Remove node handled by Node from the cluster

### Arguments:
- *node* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("leave_cluster", new HashMap<String, Object>() {{
      put("node", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/leave_cluster
    {
      "node": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""

## *list_cluster* - List nodes that are part of the cluster handled by Node


List nodes that are part of the cluster handled by Node

### Arguments:

### Result:
{nodes,{list,{node,atom}}}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("list_cluster", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/list_cluster
    {
      
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]

## *load* - Restore the database from text file


Restore the database from text file

### Arguments:
- *file* :: string

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("load", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/load
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"

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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/mnesia_change_nodename
    {
      "oldnodename": "aaaaa",
      "newnodename": "bbbbb",
      "oldbackup": "ccccc",
      "newbackup": "ddddd"
    }
    
    HTTP/1.1 200 OK
    "Success"

## *module_check* - 




### Arguments:
- *module* :: binary

### Result:
{res,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("module_check", new HashMap<String, Object>() {{
      put("module", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/module_check
    {
      "module": "aaaaa"
    }
    
    HTTP/1.1 200 OK
        {"res": 1    }

## *module_install* - 




### Arguments:
- *module* :: binary

### Result:
{res,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("module_install", new HashMap<String, Object>() {{
      put("module", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/module_install
    {
      "module": "aaaaa"
    }
    
    HTTP/1.1 200 OK
        {"res": 1    }

## *module_uninstall* - 




### Arguments:
- *module* :: binary

### Result:
{res,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("module_uninstall", new HashMap<String, Object>() {{
      put("module", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/module_uninstall
    {
      "module": "aaaaa"
    }
    
    HTTP/1.1 200 OK
        {"res": 1    }

## *module_upgrade* - 




### Arguments:
- *module* :: binary

### Result:
{res,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("module_upgrade", new HashMap<String, Object>() {{
      put("module", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/module_upgrade
    {
      "module": "aaaaa"
    }
    
    HTTP/1.1 200 OK
        {"res": 1    }

## *modules_available* - 




### Arguments:

### Result:
{modules,{list,{module,{tuple,[{name,atom},{summary,string}]}}}}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("modules_available", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/modules_available
    {
      
    }
    
    HTTP/1.1 200 OK
    {
      "aaaaa": "bbbbb",
      "ccccc": "ddddd"
    }

## *modules_installed* - 




### Arguments:

### Result:
{modules,{list,{module,{tuple,[{name,atom},{summary,string}]}}}}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("modules_installed", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/modules_installed
    {
      
    }
    
    HTTP/1.1 200 OK
    {
      "aaaaa": "bbbbb",
      "ccccc": "ddddd"
    }

## *modules_update_specs* - 




### Arguments:

### Result:
{res,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("modules_update_specs", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/modules_update_specs
    {
      
    }
    
    HTTP/1.1 200 OK
        {"res": 1    }

## *num_active_users* - Get number of users active in the last days


Get number of users active in the last days

### Arguments:
- *host* :: binary
- *days* :: integer

### Result:
{users,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("num_active_users", new HashMap<String, Object>() {{
      put("host", "aaaaa");
      put("days", new Integer(1));
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>num_active_users</methodName>
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
JSON example


    #!json
    POST /api/num_active_users
    {
      "host": "aaaaa",
      "days": 1
    }
    
    HTTP/1.1 200 OK
        {"users": 1    }

## *num_resources* - Get the number of resources of a user


Get the number of resources of a user

### Arguments:
- *user* :: binary
- *host* :: binary

### Result:
{resources,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("num_resources", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>num_resources</methodName>
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
JSON example


    #!json
    POST /api/num_resources
    {
      "user": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
        {"resources": 1    }

## *outgoing_s2s_number* - Number of outgoing s2s connections on the node


Number of outgoing s2s connections on the node

### Arguments:

### Result:
{s2s_outgoing,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("outgoing_s2s_number", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/outgoing_s2s_number
    {
      
    }
    
    HTTP/1.1 200 OK
        {"s2s_outgoing": 1    }

## *privacy_set* - Send a IQ set privacy stanza for a local account


Send a IQ set privacy stanza for a local account

### Arguments:
- *user* :: binary
- *host* :: binary
- *xmlquery* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("privacy_set", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("xmlquery", "ccccc");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/privacy_set
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "xmlquery": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""

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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/private_get
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "element": "ccccc",
      "ns": "ddddd"
    }
    
    HTTP/1.1 200 OK
        {"res": "aaaaa"    }

## *private_set* - Set to the user private storage


Set to the user private storage

### Arguments:
- *user* :: binary
- *host* :: binary
- *element* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("private_set", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("element", "ccccc");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/private_set
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "element": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""

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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
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

## *push_alltoall* - Add all the users to all the users of Host in Group


Add all the users to all the users of Host in Group

### Arguments:
- *host* :: binary
- *group* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("push_alltoall", new HashMap<String, Object>() {{
      put("host", "aaaaa");
      put("group", "bbbbb");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/push_alltoall
    {
      "host": "aaaaa",
      "group": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    ""

## *push_roster* - Push template roster from file to a user


Push template roster from file to a user

### Arguments:
- *file* :: binary
- *user* :: binary
- *host* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("push_roster", new HashMap<String, Object>() {{
      put("file", "aaaaa");
      put("user", "bbbbb");
      put("host", "ccccc");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/push_roster
    {
      "file": "aaaaa",
      "user": "bbbbb",
      "host": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""

## *push_roster_all* - Push template roster from file to all those users


Push template roster from file to all those users

### Arguments:
- *file* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("push_roster_all", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/push_roster_all
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""

## *register* - Register a user


Register a user

### Arguments:
- *user* :: binary
- *host* :: binary
- *password* :: binary

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("register", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("password", "ccccc");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/register
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "password": "ccccc"
    }
    
    HTTP/1.1 200 OK
    "Success"

## *registered_users* - List all registered users in HOST


List all registered users in HOST

### Arguments:
- *host* :: binary

### Result:
{users,{list,{username,string}}}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("registered_users", new HashMap<String, Object>() {{
      put("host", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/registered_users
    {
      "host": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]

## *registered_vhosts* - List all registered vhosts in SERVER


List all registered vhosts in SERVER

### Arguments:

### Result:
{vhosts,{list,{vhost,string}}}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("registered_vhosts", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/registered_vhosts
    {
      
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]

## *reload_config* - Reload config file in memory (only affects ACL and Access)


Reload config file in memory (only affects ACL and Access)

### Arguments:

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("reload_config", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/reload_config
    {
      
    }
    
    HTTP/1.1 200 OK
    ""

## *remove_node* - Remove an ejabberd node from Mnesia clustering config


Remove an ejabberd node from Mnesia clustering config

### Arguments:
- *node* :: string

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("remove_node", new HashMap<String, Object>() {{
      put("node", "aaaaa");
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>remove_node</methodName>
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
JSON example


    #!json
    POST /api/remove_node
    {
      "node": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""

## *reopen_log* - Reopen the log files


Reopen the log files

### Arguments:

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("reopen_log", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/reopen_log
    {
      
    }
    
    HTTP/1.1 200 OK
    ""

## *resource_num* - Resource string of a session number


Resource string of a session number

### Arguments:
- *user* :: binary
- *host* :: binary
- *num* :: integer

### Result:
{resource,string}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("resource_num", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("num", new Integer(1));
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>resource_num</methodName>
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
                <name>num</name>
                <value>
                  <integer>1</integer>
                </value>
              </member>
            </struct>
          </value>
        </param>
      </params>
    </methodCall>
JSON example


    #!json
    POST /api/resource_num
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "num": 1
    }
    
    HTTP/1.1 200 OK
        {"resource": "aaaaa"    }

## *restart* - Restart ejabberd gracefully


Restart ejabberd gracefully

### Arguments:

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("restart", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/restart
    {
      
    }
    
    HTTP/1.1 200 OK
    ""

## *restore* - Restore the database from backup file


Restore the database from backup file

### Arguments:
- *file* :: string

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("restore", new HashMap<String, Object>() {{
      put("file", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/restore
    {
      "file": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"

## *rotate_log* - Rotate the log files


Rotate the log files

### Arguments:

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("rotate_log", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/rotate_log
    {
      
    }
    
    HTTP/1.1 200 OK
    ""

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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/send_stanza_c2s
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "resource": "ccccc",
      "stanza": "ddddd"
    }
    
    HTTP/1.1 200 OK
    ""

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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/set_last
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "timestamp": 1,
      "status": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""

## *set_loglevel* - Set the loglevel (0 to 5)


Set the loglevel (0 to 5)

### Arguments:
- *loglevel* :: integer

### Result:
{logger,atom}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("set_loglevel", new HashMap<String, Object>() {{
      put("loglevel", new Integer(1));
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/set_loglevel
    {
      "loglevel": 1
    }
    
    HTTP/1.1 200 OK
        {"logger": "aaaaa"    }

## *set_master* - Set master node of the clustered Mnesia tables


If you provide as nodename "self", this node will be set as its own master.

### Arguments:
- *nodename* :: string

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("set_master", new HashMap<String, Object>() {{
      put("nodename", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/set_master
    {
      "nodename": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"

## *set_nickname* - Set nickname in a user's vCard


Set nickname in a user's vCard

### Arguments:
- *user* :: binary
- *host* :: binary
- *nickname* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("set_nickname", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
      put("nickname", "ccccc");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/set_nickname
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "nickname": "ccccc"
    }
    
    HTTP/1.1 200 OK
    ""

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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/set_vcard
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "name": "ccccc",
      "content": "ddddd"
    }
    
    HTTP/1.1 200 OK
    ""

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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
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

## *srg_delete* - Delete a Shared Roster Group


Delete a Shared Roster Group

### Arguments:
- *group* :: binary
- *host* :: binary

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("srg_delete", new HashMap<String, Object>() {{
      put("group", "aaaaa");
      put("host", "bbbbb");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/srg_delete
    {
      "group": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    ""

## *srg_get_info* - Get info of a Shared Roster Group


Get info of a Shared Roster Group

### Arguments:
- *group* :: binary
- *host* :: binary

### Result:
{informations,{list,{information,{tuple,[{key,string},{value,string}]}}}}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("srg_get_info", new HashMap<String, Object>() {{
      put("group", "aaaaa");
      put("host", "bbbbb");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
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

## *srg_get_members* - Get members of a Shared Roster Group


Get members of a Shared Roster Group

### Arguments:
- *group* :: binary
- *host* :: binary

### Result:
{members,{list,{member,string}}}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("srg_get_members", new HashMap<String, Object>() {{
      put("group", "aaaaa");
      put("host", "bbbbb");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
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

## *srg_list* - List the Shared Roster Groups in Host


List the Shared Roster Groups in Host

### Arguments:
- *host* :: binary

### Result:
{groups,{list,{id,string}}}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("srg_list", new HashMap<String, Object>() {{
      put("host", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/srg_list
    {
      "host": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]

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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/srg_user_add
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "group": "ccccc",
      "grouphost": "ddddd"
    }
    
    HTTP/1.1 200 OK
    ""

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
Java example


    #!java
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

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/srg_user_del
    {
      "user": "aaaaa",
      "host": "bbbbb",
      "group": "ccccc",
      "grouphost": "ddddd"
    }
    
    HTTP/1.1 200 OK
    ""

## *stats* - Get statistical value: registeredusers onlineusers onlineusersnode uptimeseconds


Get statistical value: registeredusers onlineusers onlineusersnode uptimeseconds

### Arguments:
- *name* :: binary

### Result:
{stat,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("stats", new HashMap<String, Object>() {{
      put("name", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/stats
    {
      "name": "aaaaa"
    }
    
    HTTP/1.1 200 OK
        {"stat": 1    }

## *stats_host* - Get statistical value for this host: registeredusers onlineusers


Get statistical value for this host: registeredusers onlineusers

### Arguments:
- *name* :: binary
- *host* :: binary

### Result:
{stat,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("stats_host", new HashMap<String, Object>() {{
      put("name", "aaaaa");
      put("host", "bbbbb");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/stats_host
    {
      "name": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
        {"stat": 1    }

## *status* - Get status of the ejabberd server


Get status of the ejabberd server

### Arguments:

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("status", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/status
    {
      
    }
    
    HTTP/1.1 200 OK
    "Success"

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
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("status_list", new HashMap<String, Object>() {{
      put("status", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
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
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("status_list_host", new HashMap<String, Object>() {{
      put("host", "aaaaa");
      put("status", "bbbbb");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
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

## *status_num* - Number of logged users with this status


Number of logged users with this status

### Arguments:
- *status* :: binary

### Result:
{users,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("status_num", new HashMap<String, Object>() {{
      put("status", "aaaaa");
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>status_num</methodName>
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
JSON example


    #!json
    POST /api/status_num
    {
      "status": "aaaaa"
    }
    
    HTTP/1.1 200 OK
        {"users": 1    }

## *status_num_host* - Number of logged users with this status in host


Number of logged users with this status in host

### Arguments:
- *host* :: binary
- *status* :: binary

### Result:
{users,integer}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("status_num_host", new HashMap<String, Object>() {{
      put("host", "aaaaa");
      put("status", "bbbbb");
    }});

XmlRPC example


    #!xml
    <methodCall>
      <methodName>status_num_host</methodName>
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
JSON example


    #!json
    POST /api/status_num_host
    {
      "host": "aaaaa",
      "status": "bbbbb"
    }
    
    HTTP/1.1 200 OK
        {"users": 1    }

## *stop* - Stop ejabberd gracefully


Stop ejabberd gracefully

### Arguments:

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("stop", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/stop
    {
      
    }
    
    HTTP/1.1 200 OK
    ""

## *stop_kindly* - Inform users and rooms, wait, and stop the server


Provide the delay in seconds, and the announcement quoted, for example: 
ejabberdctl stop_kindly 60 \"The server will stop in one minute.\"

### Arguments:
- *delay* :: integer
- *announcement* :: string

### Result:
{res,rescode}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("stop_kindly", new HashMap<String, Object>() {{
      put("delay", new Integer(1));
      put("announcement", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/stop_kindly
    {
      "delay": 1,
      "announcement": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    ""

## *unregister* - Unregister a user


Unregister a user

### Arguments:
- *user* :: binary
- *host* :: binary

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("unregister", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/unregister
    {
      "user": "aaaaa",
      "host": "bbbbb"
    }
    
    HTTP/1.1 200 OK
    "Success"

## *update* - Update the given module, or use the keyword: all


Update the given module, or use the keyword: all

### Arguments:
- *module* :: string

### Result:
{res,restuple}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("update", new HashMap<String, Object>() {{
      put("module", "aaaaa");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/update
    {
      "module": "aaaaa"
    }
    
    HTTP/1.1 200 OK
    "Success"

## *update_list* - List modified modules that can be updated


List modified modules that can be updated

### Arguments:

### Result:
{modules,{list,{module,string}}}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("update_list", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/update_list
    {
      
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]

## *user_resources* - List user's connected resources


List user's connected resources

### Arguments:

### Result:
{resources,{list,{resource,string}}}
### Examples:
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("user_resources", new HashMap<String, Object>() {{
      
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
    POST /api/user_resources
    {
      
    }
    
    HTTP/1.1 200 OK
    [
      "aaaaa",
      "bbbbb"
    ]

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
Java example


    #!java
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(url);
    
    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);
    
    client.execute("user_sessions_info", new HashMap<String, Object>() {{
      put("user", "aaaaa");
      put("host", "bbbbb");
    }});

XmlRPC example


    #!xml
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
JSON example


    #!json
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

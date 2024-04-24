# ejabberd for Elixir Developers

<!-- md:version improved in [21.07](../../archive/21.07/index.md) -->

## Building ejabberd with Mix

You can build ejabberd with [Elixir](https://elixir-lang.org/) `mix` tool.
This allows ejabberd to use Elixir libraries
and ejabberd modules written in Elixir.

Please note: Elixir 1.10.3 or higher is required to build a release.
Also, if using Erlang/OTP 24, then Elixir 1.11.4 or higher is required.

<!--- Code blocks in lists have indentation of 3 spaces at first ~~~ -->

1. Make sure you have the
   [requirements](../../admin/install/source.md/#requirements)
   installed. On MacOS you need to use Homebrew and
   [set up your environment](../../admin/install/homebrew.md).

1. Clone ejabberd project from Github:

    ``` sh
    git clone https://github.com/processone/ejabberd.git
    cd ejabberd
    ```

1. [Compile](../../admin/install/source.md/#compilation) ejabberd:

    ``` sh
    ./autogen.sh
    ./configure --with-rebar=mix
    make
    ```

1. [Build a development release](../../admin/install/source.md/#development-release):

    ``` sh
    make dev
    ```

1. There are many ways to start ejabberd, using the `ejabberdctl` or `ejabberd` scripts:

    ``` sh
    _build/prod/rel/ejabberd/bin/ejabberdctl iexlive
    _build/prod/rel/ejabberd/bin/ejabberdctl live
    _build/prod/rel/ejabberd/bin/ejabberd start_iex
    ```

1. You should see that ejabberd is properly started:

    ``` iex
    Erlang/OTP 23 [erts-11.1.8] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1]

    2021-08-03 13:37:36.561603+02:00 [info] Loading configuration from /home/bernar/e/git/ejabberd/_build/dev/rel/ejabberd/etc/ejabberd/ejabberd.yml
    2021-08-03 13:37:37.541688+02:00 [info] Configuration loaded successfully
    ...
    2021-08-03 13:37:40.201590+02:00 [info] ejabberd 21.7.9 is started in the node ejabberd@atenea in 3.86s
    2021-08-03 13:37:40.203678+02:00 [info] Start accepting TCP connections at [::]:5222 for ejabberd_c2s

    Interactive Elixir (1.10.3) - press Ctrl+C to exit (type h() ENTER for help)
    iex(ejabberd@localhost)1>
    ```

1. Now that ejabberd starts correctly, adapt to your needs the default ejabberd
   configuration file located at `_build/dev/rel/ejabberd/etc/ejabberd/ejabberd.yml`
   For example, enable this example Elixir ejabberd module:

    ``` yaml
    modules:
      'ModPresenceDemo': {}
      mod_adhoc: {}
    ```

## Embed ejabberd in an elixir app

ejabberd is available as an Hex.pm
application: [ejabberd on hex.pm](https://hex.pm/packages/ejabberd).

This means you can build a customized XMPP messaging
platform with Elixir on top of ejabberd by leveraging ejabberd code
base in your app and providing only your custom modules.
This makes the management of your ejabberd plugins easier and cleaner.

To create your own application depending on ejabberd, you can go
through the following steps:

1. Create a new Elixir app using `mix`:

    ``` sh
    mix new ejapp
    cd ejapp
    ```

1. Add [ejabberd package](https://hex.pm/packages/ejabberd) as a
   dependency in your `mix.exs` file:

    ``` elixir
    defmodule Ejapp.MixProject do
      defp deps do
        [
         {:ejabberd, "~> 21.7"}
        ]
      end
    end
    ```

1. Compile everything:

    ``` sh
    mix do deps.get, compile
    ```

1. Create paths and files for ejabberd:

    ``` sh
    mkdir config
    mkdir logs
    mkdir mnesia
    wget -O config/ejabberd.yml https://raw.githubusercontent.com/processone/ejabberd/master/ejabberd.yml.example
    ```

1. Define those paths in `config/config.exs`:

    ``` elixir
    import Config
    config :ejabberd,
      file: "config/ejabberd.yml",
      log_path: 'logs/ejabberd.log'
    config :mnesia,
      dir: 'mnesia/'
    ```

1. Start your app, ejabberd will be started as a dependency:

    ``` sh
    iex -S mix
    ```

1. You should see that ejabberd is properly started:

    ``` iex
    Erlang/OTP 23 [erts-11.1.8] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1]
    
    Compiling 1 file (.ex)
    Generated ejapp app
    
    17:58:35.955 [info]  Loading configuration from config/ejabberd.yml
    
    17:58:36.459 [info]  Configuration loaded successfully
    ...
    17:58:39.897 [info]  ejabberd 21.7.0 is started in the node :nonode@nohost in 4.07s
    ...
    17:58:39.908 [info]  Start accepting TCP connections at [::]:5222 for :ejabberd_c2s
    
    Interactive Elixir (1.10.3) - press Ctrl+C to exit (type h() ENTER for help)
    iex(1)>
    ```

1. Register user from Elixir console:

    ``` elixir
    :ejabberd_auth.try_register("test", "localhost", "passw0rd")
    ```

1. You are all set, you can now connect with an XMPP client !

## Call elixir code in erlang code

It's possible to use Elixir libraries in an Erlang module,
both the ones included in Elixir, or any other you add as a dependency.

This simple example invokes Elixir's
[String.duplicate/2 function](https://hexdocs.pm/elixir/1.13.4/String.html#duplicate/2)
as shown in one of its documentation examples,
and uses the result in the ejabberd vCard nickname field:

``` diff
--- a/src/mod_vcard.erl
+++ b/src/mod_vcard.erl
@@ -209,6 +209,7 @@ process_local_iq(#iq{type = get, to = To, lang = Lang} = IQ) ->
     VCard = case mod_vcard_opt:vcard(ServerHost) of
   undefined ->
       #vcard_temp{fn = <<"ejabberd">>,
+    nickname = 'Elixir.String':duplicate(<<"abc">>, 2),
     url = ejabberd_config:get_uri(),
     desc = misc:get_descr(Lang, ?T("Erlang XMPP Server")),
     bday = <<"2002-11-16">>};
```

Notice that the elixir code:

``` elixir
String.duplicate("abc", 2)
```

is written in erlang as:

``` erlang
'Elixir.String':duplicate(<<"abc">>, 2),
```

Check [Erlang/Elixir Syntax: A Crash Course](https://elixir-lang.org/crash-course.html)
for details.

## Use elixir library in erlang code

This example demonstrates how to add an elixir library as a dependency in ejabberd,
and use it in an ejabberd module written in erlang.

It will use [QRCodeEx](https://hex.pm/packages/qrcode_ex)
elixir library to build a QR code of ejabberd's URI
and return it as the server vCard photo.

First add the dependency to `mix.exs`:

``` diff
--- a/mix.exs
+++ b/mix.exs
@@ -46,7 +46,7 @@ defmodule Ejabberd.MixProject do
                     :p1_utils, :stringprep, :yconf],
      included_applications: [:mnesia, :os_mon,
                              :cache_tab, :eimp, :mqtree, :p1_acme,
-                             :p1_oauth2, :pkix, :xmpp]
+                             :p1_oauth2, :pkix, :xmpp, :qrcode_ex]
      ++ cond_apps()]
   end

@@ -113,6 +113,7 @@ defmodule Ejabberd.MixProject do
      {:p1_oauth2, "~> 0.6"},
      {:p1_utils, "~> 1.0"},
      {:pkix, "~> 1.0"},
+     {:qrcode_ex, "~> 0.1.1"},
      {:stringprep, ">= 1.0.26"},
      {:xmpp, "~> 1.5"},
      {:yconf, "~> 1.0"}]
```

Then call [QRCodeEx.encode/2](https://hexdocs.pm/qrcode_ex/QRCodeEx.Encode.html#encode/2), [QRCodeEx.png/2](https://hexdocs.pm/qrcode_ex/QRCodeEx.PNG.html#png/2), and provide the result as the photo in the server vcard:

``` diff
--- a/src/mod_vcard.erl
+++ b/src/mod_vcard.erl
@@ -206,9 +206,13 @@ process_local_iq(#iq{type = set, lang = Lang} = IQ) ->
     xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
 process_local_iq(#iq{type = get, to = To, lang = Lang} = IQ) ->
     ServerHost = ejabberd_router:host_of_route(To#jid.lserver),
+    PhotoEncoded = 'Elixir.QRCodeEx':encode(ejabberd_config:get_uri()),
+    PhotoBin = 'Elixir.QRCodeEx':png(PhotoEncoded, [{color, <<17, 120, 0>>}]),
+    PhotoEl = #vcard_photo{type = <<"image/png">>, binval = PhotoBin},
     VCard = case mod_vcard_opt:vcard(ServerHost) of
   undefined ->
       #vcard_temp{fn = <<"ejabberd">>,
+    photo = PhotoEl,
     url = ejabberd_config:get_uri(),
     desc = misc:get_descr(Lang, ?T("Erlang XMPP Server")),
     bday = <<"2002-11-16">>};
```

## Write ejabberd module in elixir

If you plan to write an ejabberd module that heavily depends on Elixir dependencies, you may want to write it in elixir from scratch.

The Elixir source code is placed in the [ejabberd's lib/](https://github.com/processone/ejabberd/tree/master/lib) path.
Any elixir module placed in `lib/` will be compiled by Mix, installed with all the other erlang modules, and available for you to use.

As you can see, there's a file named [mod_presence_demo.ex](https://github.com/processone/ejabberd/blob/master/lib/mod_presence_demo.ex) which defines an ejabberd module written in elixir called `ModPresenceDemo`.
To enable `ModPresenceDemo`, add it to `ejabberd.yml` like this:

``` erlang
modules:
  'Elixir.ModPresenceDemo': {}
```

Let's write a new ejabberd module in elixir, add it to ejabberd's source code, compile and install it.
This example module requires the QRCodeEx Elixir library, and adds a simple web page that generates QR code of any given JID.

1. Copy the [mod_qrcode.ex](#mod_qrcodeex) source code to ejabberd's `lib/` path:

    ``` sh
    lib/mod_qrcode.ex
    ```

2. Recompile and reinstall ejabberd.

3. Enable the module in `ejabberd.yml`:

    ``` yaml
    listen:
      -
        port: 5280
        request_handlers:
          /qrcode: 'Elixir.ModQrcode'

    modules:
      'Elixir.ModQrcode': {}
    ```

4. When restarting ejabberd, it will show in the logs:

    ``` erl
    2022-07-06 13:14:35.363081+02:00 [info] Starting ejabberd module Qrcode
    ```

5. Now the ejabberd internal web server provides QR codes of any given JID.
   Try visiting an URL like `http://localhost:5280/qrcode/anyusername/somedomain/`

## Elixir module in ejabberd-contrib

Using [ejabberd-contrib](modules.md#ejabberd-contrib) it's possible to install additional ejabberd modules without compiling ejabberd, or requiring ejabberd source code.
This is useful if you install ejabberd using binary installers or a container image.

And it's possible to write a custom module and [Add your module](modules.md#add-your-module) to an existing ejabberd installation...

Let's write a new ejabberd module in elixir, compile and install in an existing ejabberd deployment without requiring its source code.
This example module adds a simple section listing PIDs in the users page in ejabberd WebAdmin.

1. First, create this path

    ``` sh
    $HOME/.ejabberd-modules/sources/mod_webadmin_pid/lib/
    ```

2. and copy the [mod_webadmin_pid.ex](#mod_webadmin_pidex) source code to:

    ``` sh
    $HOME/.ejabberd-modules/sources/mod_webadmin_pid/lib/mod_webadmin_pid.ex
    ```

3. Create a specification file in YAML format as `mod_webadmin_pid.spec`
(see examples from ejabberd-contrib). So, create the file

    ``` sh
    $HOME/.ejabberd-modules/sources/mod_webadmin_pid/mod_webadmin_pid.spec
    ```

    with this content:

    ``` yaml
    summary: "Display PIDs in User page in Web Admin"
    ```

4. From that point you should see it as available module:

    ``` sh
    ejabberdctl modules_available
    mod_webadmin_pid Display PIDs in User page in Web Admin
    ```

5. Now you can compile and install that module:

    ``` sh
    ejabberdctl module_install mod_webadmin_pid
    ```

6. Enable the module in `ejabberd.yml`:

    ``` yaml
    modules:
      'Elixir.ModWebAdminPid': {}
    ```

7. When restarting ejabberd, it will show in the logs:

    ``` erl
    2022-07-06 13:14:35.363081+02:00 [info] Starting ejabberd module WebAdminPid
    ```

8. Finally, go to ejabberd WebAdmin -> Virtual Hosts -> your vhost -> Users ->
   some online user -> and there will be a new section "PIDs".

## Record definition

To use an erlang record defined in ejabberd's header file, use Elixir's [Record](https://hexdocs.pm/elixir/Record.html) to extract the fields and define an Elixir record with its usage macros.

For example, add this to the beginning of [mod_presence_demo.ex](https://github.com/processone/ejabberd/blob/master/lib/mod_presence_demo.ex):

``` elixir
require Record

Record.defrecord(:presence,
  Record.extract(:presence, from_lib: "xmpp/include/xmpp.hrl"))
```

Later you can use those macros, named like your record, see the [examples](https://hexdocs.pm/elixir/Record.html#defrecord/3-examples).

In our example, let's improve the `on_presence` function and use the `presence` macros to get the `to` field:

``` elixir
def on_presence(_user, _server, _resource, packet) do
  to_jid = presence(packet, :to)
  to_str = :jid.to_string(to_jid)
  info('Received presence for #{to_str}:~n~p', [packet])
  :none
end
```

## `mod_qrcode.ex`

Example ejabberd module written in elixir:

``` elixir title="mod_qrcode.ex"
defmodule ModQrcode do
  use Ejabberd.Module

  def start(host, _opts) do
    info('Starting ejabberd module Qrcode')
    :ok
  end

  def stop(host) do
    info('Stopping ejabberd module Qrcode')
    :ok
  end

  def process([username, hostname] = _path, _query) do
    uri = <<"xmpp:", username::binary, "@", hostname::binary>>
    qr = QRCodeEx.svg(QRCodeEx.encode(uri), [{:color, "#3fb0d2"}])
    qxmlel = :fxml_stream.parse_element(qr)
    {200,
     [{<<"Server">>, <<"ejabberd">>},
      {<<"Content-Type">>, <<"image/svg+xml">>}],
     :ejabberd_web.make_xhtml([], [qxmlel])}
  end

  def process(path, _query) do
    info('Received HTTP query with path: ~p', [path])
    {404, [], "Not Found"}
  end

  def depends(_host, _opts) do
    []
  end

  def mod_options(_host) do
    []
  end

  def mod_doc() do
    %{:desc => 'This is just a demonstration.'}
  end

end
```

## `mod_webadmin_pid.ex`

Example ejabberd module written in elixir:

``` elixir title="mod_webadmin_pid.ex"
defmodule ModWebAdminPid do
  use Ejabberd.Module

  require Record

  Record.defrecord(:xmlel,
    Record.extract(:xmlel, from_lib: "xmpp/include/xmpp.hrl"))

  Record.defrecord(:request,
    Record.extract(:request, from: "include/ejabberd_http.hrl"))

  ##====================================================================
  ## gen_mod callbacks
  ##====================================================================

  def start(host, _opts) do
    info('Starting ejabberd module WebAdminPid')
    :ejabberd_hooks.add(:webadmin_user, host, __MODULE__, :webadmin_user, 60)
    :ejabberd_hooks.add(:webadmin_page_host, host, __MODULE__, :webadmin_page, 60)
    :ok
  end

  def stop(host) do
    info('Stopping ejabberd module WebAdminPid')
    :ejabberd_hooks.delete(:webadmin_user, host, __MODULE__, :webadmin_user, 60)
    :ejabberd_hooks.delete(:webadmin_page_host, host, __MODULE__, :webadmin_page, 60)
    :ok
  end

  def depends(_host, _opts) do
    []
  end

  def mod_options(_host) do
    []
  end

  def mod_doc() do
    %{:desc => 'This is just a demonstration.'}
  end

  ##====================================================================
  ## Web Admin
  ##====================================================================

  def webadmin_user(acc, user, server, _lang) do
    resources = :ejabberd_sm.get_user_resources(user, server)

    pids_elements = Enum.map(resources,
      fn resource ->
        pid = :ejabberd_sm.get_session_pid(user, server, resource)
        pid_string = :erlang.pid_to_list(pid)
        xmlel(name: "a", attrs: [{"href", "pid/#{pid_string}"}], children: [xmlcdata: pid_string])
      end)

    pids_separated = Enum.intersperse(pids_elements, {:xmlcdata, ", "})

    new_element = xmlel(name: "h3", children: [xmlcdata: "PIDs:"])

    acc ++ [new_element] ++ pids_separated
  end

  def webadmin_page(_acc, host, request(path: ["user", user, "pid", pid])) do
    res = webadmin_pid(user, host, pid)
    {:stop, res}
  end

  def webadmin_page(acc, _host, _request) do
    acc
  end

  def webadmin_pid(user, host, pid_string) do
    us = :jid.to_string(:jid.make(user, host))
    page_title = 'Pid #{pid_string} of #{us}'

    pid = :erlang.list_to_pid(String.to_charlist(pid_string))
    pid_info = Process.info(pid)
    pid_info_string = :io_lib.format("~p", [pid_info])

    [xmlel(name: "h1", children: [xmlcdata: page_title]),
     xmlel(name: "pre", children: [xmlcdata: pid_info_string])]
  end

end
```

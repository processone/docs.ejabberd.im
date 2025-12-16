# Next Steps

## Starting ejabberd

Depending on how you installed ejabberd, it may be started automatically by the operating system at system boot time.

You can use the `ejabberdctl` command line administration script to start and stop ejabberd, check its status and many other administrative tasks.

If you provided the configure option `--enable-user=USER` (see compilation `options`, you can execute `ejabberdctl` with either that system account or root.

Usage example:

``` sh
prompt> ejabberdctl start

prompt> ejabberdctl status
The node ejabberd@localhost is started with status: started
ejabberd is running in that node

prompt> ejabberdctl stop
```

If ejabberd doesn't start correctly and a crash dump file is generated, there was a severe problem.
You can try to start ejabberd in interactive mode with the command `bin/ejabberdctl live` to see the error messages provided by Erlang and identify the exact the problem.

The `ejabberdctl` administration script is included in the `bin` directory in the Linux Installers and Docker image.

Please refer to the section [ejabberdctl](../guide/managing.md#ejabberdctl) for details about `ejabberdctl`, and configurable options to fine tune the Erlang runtime system.

## Autostart on Linux

If you compiled ejabberd from source code or some other method that doesn't setup autostarting ejabberd, you can try this method.

On a \*nix system, create a system user called 'ejabberd', give it write access to the directories `database/` and `logs/`, and set that as home.

If you want ejabberd to be started as daemon at boot time with that user, copy `ejabberd.init` from the `bin` directory to something like `/etc/init.d/ejabberd`.
Then you can call `/etc/inid.d/ejabberd start` to start the server.

Or if you have a `systemd` distribution:

1. copy `ejabberd.service` to `/etc/systemd/system/`
2. run `systemctl daemon-reload`
3. run `systemctl enable ejabberd.service`
4. To start the server, you can run `systemctl start ejabberd`

When ejabberd is started, the processes that are started in the system
are `beam` or `beam.smp`, and also `epmd`. For more information
regarding `epmd` consult the section relating to
[epmd](../guide/distribution.md#epmd).

## Administration Account

def:admin account
: Account registered in ejabberd with administrative privileges granted in the ejabberd configuration file.
  Features that take into consideration those privileges:
  [api_permissions](../configuration/toplevel.md#api_permissions),
  [ejabberd_web_admin](../guide/managing.md#web-admin),
  [mod_announce](../configuration/modules.md#mod_announce),
  [mod_configure](../configuration/modules.md#mod_configure),
  [mod_http_api](../configuration/modules.md#mod_http_api),
  [mod_muc](../configuration/modules.md#mod_muc), ...

Some ejabberd installation methods ask you details for the first account, and take care to register that account and grant it administrative rights;
in that case you can skip this section.

After installing ejabberd from source code or other methods, you may want to register the first XMPP account and grant it administrative rights:

1. Register an XMPP account on your ejabberd server.
    For example, if `example.org` is configured in the
    [hosts](../configuration/basic.md#host-names)
    section in your ejabberd configuration file,
    then you may want to register an account with JID `admin1@example.org`.

    There are two ways to register an XMPP account in ejabberd:

    - Using an XMPP client and [In-Band Registration](../configuration/modules.md#mod_register).

    - Using [ejabberdctl](../guide/managing.md#ejabberdctl):

        ``` sh
        ejabberdctl register admin1 example.org password
        ```

2. Edit the ejabberd configuration file to give administration
    rights to the XMPP account you registered:

    ``` yaml
    acl:
      admin:
        user: admin1@example.org

    access_rules:
      configure:
        allow: admin
    ```

    You can grant administrative privileges to many XMPP accounts, and
    also to accounts in other XMPP servers.

3. Restart ejabberd to load the new configuration, or run the
    [reload_config](../../developer/ejabberd-api/admin-api.md#reload_config) command.

4. Open the Web Admin page in your favourite browser.
    The exact address depends on your ejabberd configuration,
    and may be:
    - [http://localhost:5280/admin/](http://localhost:5280/admin/)
      on [binary installers](binary-installer.md)
    - [https://localhost:5443/admin/](https://localhost:5443/admin/)
      on [binary installers](binary-installer.md)
    - [https://localhost:5280/admin/](https://localhost:5280/admin/)
      on [Debian package](os-package.md)

5. Your web browser shows a login window. Introduce the **full JID**,
    in this example `admin1@example.org`, and the account password.
    If the web address hostname is the same that the account JID,
    you can provide simply the username instead of the full JID: `admin1`.
    See [Web Admin](../guide/managing.md#web-admin) for details.

## Configuring ejabberd

Now that you got ejabberd installed and running, it's time to configure it to your needs. You can follow on the [Configuration](../configuration/index.md) section and take also a look at the [Tutorials](../../tutorials/index.md).

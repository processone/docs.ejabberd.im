# Binary Installers

## Linux RUN Installer

The `*.run` binary installer will deploy and configure a full featured ejabberd server and does not require any extra dependencies.
It includes a stripped down version of Erlang.
As such, when using ejabberd installer, you do not need to install Erlang separately.

Those instructions assume installation on `localhost` for development purposes.
In this document, when mentioning `ejabberd-YY.MM`, we assume `YY.MM` is the release number, for example 18.01.

Installation using the `*.run` binary installer:

1. Go to [ejabberd GitHub Releases](https://github.com/processone/ejabberd/releases).

2. Download the `run` package for your architecture

3. Right-click on the downloaded file and select "Properties", click on the "Permissions" tab and tick the box that says "Allow executing file as program".
   Alternatively, you can set the installer as executable using the command line:

    ``` sh
    chmod +x ejabberd-YY.MM-1-linux-x64.run
    ```

4. If the installer runs as superuser (by `root` or using `sudo`),
   it installs ejabberd binaries in `/opt/ejabberd-XX.YY/`;
   installs your configuration, Mnesia database and logs in `/opt/ejabberd/`,
   and setups an ejabberd service unit in `systemd`:

    ``` sh
    sudo ./ejabberd-YY.MM-1-linux-x64.run
    ```

5. If the installer runs as a regular user,
   it asks the base path where ejabberd should be installed.
   In that case, the ejabberd service unit is not set in `systemd`,
   and `systemctl` cannot be used to start ejabberd; start it manually.

6. After successful installation by root, ejabberd is automatically started.
   Check its status with

    ``` sh
    systemctl status ejabberd
    ```

7. Now that ejabberd is installed and running with the default configuration,
   it's time to do some basic setup: edit `/opt/ejabberd/conf/ejabberd.yml`
   and setup in the `hosts` option the domain that you want ejabberd to serve.
   By default it's set to the name of your computer on the local network.

8. Restart ejabberd completely using systemctl, or using ejabberdctl,
   or simply tell it to reload the configuration file:

    ``` sh
    sudo systemctl restart ejabberd
    sudo /opt/ejabberd-22.05/bin/ejabberdctl restart
    sudo /opt/ejabberd-22.05/bin/ejabberdctl reload_config
    ```

9. Quite probably you will want to register an account and grant it admin rights,
   please check [Next Steps: Administration Account](next-steps.md#administration-account).

10. Now you can go to the web dashboard at `http://localhost:5280/admin/`
   and fill the username field with the full account JID,
   for example `admin@domain` (or `admin@localhost` as above).
   Then fill the password field with that account's `password`.
   The next step is to get to know [how to configure ejabberd](../configuration/index.md).

11. If something goes wrong during the installation
   and you would like to start from scratch,
   you will find the steps to uninstall in the file
   `/opt/ejabberd-22.05/uninstall.txt`.

## Linux DEB and RPM Installers

ProcessOne provides DEB and RPM all-in-one binary installers with the same content
that the `*.run` binary installer mentioned in the previous section.

Those are self-sufficient packages that contain a minimal Erlang distribution,
this ensures that it does not interfere with your existing Erlang version
and is also a good way to make sure ejabberd will run with the latest Erlang version.

Those packages install ejabberd in `/opt/ejabberd-XX.YY/`.
Your configuration, Mnesia database and logs are available in `/opt/ejabberd/`.

You can download directly the DEB and RPM packages from
[ejabberd GitHub Releases](https://github.com/processone/ejabberd/releases).

If you prefer, you can also get those packages from our official [ejabberd packages repository](https://repo.process-one.net).


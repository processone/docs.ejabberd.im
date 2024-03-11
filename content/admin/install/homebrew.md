# Install ejabberd on macOS

## Homebrew

[Homebrew](https://brew.sh/) is a package manager for macOS that aims to port the many Unix & Linux software that is not easily available or compatible. Homebrew installation is simple and the instruction is available on its website.

Check also the guide for [Installing ejabberd development environment on OSX](../../developer/install-osx.md)

The ejabberd configuration included in Homebrew's ejabberd has as default domain `localhost`, and has already granted administrative privileges to the account `admin@localhost`.

1. Once you have Homebrew installed, open Terminal. Run

    ``` sh
    brew install ejabberd
    ```

    This should install the latest or at most the one-before-latest version of ejabberd. The installation directory should be reported at the end of this process, but usually the main executable is stored at `/usr/local/sbin/ejabberdctl`.

2. Start ejabberd in interactive mode, which prints useful messages in the Terminal.

    ``` sh
    /usr/local/sbin/ejabberdctl live
    ```

3. Create the account `admin@localhost` with password set as `password`:

    ``` sh
    /usr/local/sbin/ejabberdctl register admin localhost password
    ```

4. Now you can go to the web dashboard at `http://localhost:5280/admin/` and fill the username field with the full account JID, for example `admin@localhost`, then fill the password field with that account's `password`.

5. Without configuration there's not much to see here, therefore the next step is to get to know [how to configure ejabberd](../configuration/basic.md).

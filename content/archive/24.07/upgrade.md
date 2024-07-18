# Upgrade to ejabberd 24.07

If you upgrade ejabberd from a previous release to [24.07](../../archive/24.07/index.md),
please update the ejabberd configuration file:

## <a name="webadmin-config"></a>WebAdmin API permissions configuration

The ejabberd 24.06 release notes announced the [Improved WebAdmin with commands usage](https://www.process-one.net/blog/ejabberd-24-06/#webadmin-commands), and mentioned some [`api_permissions` configuration details](https://www.process-one.net/blog/ejabberd-24-06/#webadmin-config), but it was not explicit enough about this fact: with the default ejabberd configuration, an admin was allowed to login in WebAdmin from any machine, but only was allowed to execute commands from the loopback IP address! The WebAdmin showed the page sections, but all them were empty. Additionally, there was a bug that showed similar symptoms when entering the WebAdmin using a host and then logging in as an account in another host. Both problems and their solutions are described in [#4249](https://github.com/processone/ejabberd/issues/4249).

Please update your configuration accordingly, adding permission from web admin to execute all commands to accounts logged in with admin privilege:

```yaml
api_permissions:
  "webadmin commands":
    from: ejabberd_web_admin
    who: admin
    what: "*"
```

Of course you can customize that access as much as you want: only from specific IP addresses, only to certain accounts, only for specific commands...

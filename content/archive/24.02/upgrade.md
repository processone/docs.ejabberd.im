# Upgrade to ejabberd 24.02

If you upgrade ejabberd from a previous release to [24.02](../../archive/24.02/index.md),
please review those changes:

- Update the SQL schema
- Update API commands as explained below, or use API versioning
- Mix or Rebar3 used by default instead of Rebar to compile ejabberd
- Authentication workaround for Converse.js and Strophe.js

## <a name="sql"></a>Update the SQL schema

The table `archive` has a text column named `origin_id` (see [commit 975681](https://github.com/processone/ejabberd/commit/975681)). You have two methods to update the SQL schema of your existing database:

If using MySQL or PosgreSQL, you can enable the option [`update_sql_schema`](../../admin/configuration/toplevel.md#update_sql_schema) and ejabberd will take care to update the SQL schema when needed: add in your ejabberd configuration file the line `update_sql_schema: true`

If you are using other database, or prefer to update manually the SQL schema:

* MySQL default schema:
``` sql
ALTER TABLE archive ADD COLUMN origin_id text NOT NULL DEFAULT '';
ALTER TABLE archive ALTER COLUMN origin_id DROP DEFAULT;
CREATE INDEX i_archive_username_origin_id USING BTREE ON archive(username(191), origin_id(191));
```

* MySQL new schema:
``` sql
ALTER TABLE archive ADD COLUMN origin_id text NOT NULL DEFAULT '';
ALTER TABLE archive ALTER COLUMN origin_id DROP DEFAULT;
CREATE INDEX i_archive_sh_username_origin_id USING BTREE ON archive(server_host(191), username(191), origin_id(191));
```

* PostgreSQL default schema:
``` sql
ALTER TABLE archive ADD COLUMN origin_id text NOT NULL DEFAULT '';
ALTER TABLE archive ALTER COLUMN origin_id DROP DEFAULT;
CREATE INDEX i_archive_username_origin_id ON archive USING btree (username, origin_id);
```

* PostgreSQL new schema:
``` sql
ALTER TABLE archive ADD COLUMN origin_id text NOT NULL DEFAULT '';
ALTER TABLE archive ALTER COLUMN origin_id DROP DEFAULT;
CREATE INDEX i_archive_sh_username_origin_id ON archive USING btree (server_host, username, origin_id);
```

* MSSQL default schema:
``` sql
ALTER TABLE [dbo].[archive] ADD [origin_id] VARCHAR (250) NOT NULL;
CREATE INDEX [archive_username_origin_id] ON [archive] (username, origin_id)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);
```

* MSSQL new schema:
``` sql
ALTER TABLE [dbo].[archive] ADD [origin_id] VARCHAR (250) NOT NULL;
CREATE INDEX [archive_sh_username_origin_id] ON [archive] (server_host, username, origin_id)
WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON);
```

* SQLite default schema:
``` sql
ALTER TABLE archive ADD COLUMN origin_id text NOT NULL DEFAULT '';
CREATE INDEX i_archive_username_origin_id ON archive (username, origin_id);
```

* SQLite new schema:
``` sql
ALTER TABLE archive ADD COLUMN origin_id text NOT NULL DEFAULT '';
CREATE INDEX i_archive_sh_username_origin_id ON archive (server_host, username, origin_id);
```

## <a name="api"></a>Support for API versioning

Until now, when a new ejabberd release changed some API command (an argument renamed, a result in a different format...), then you had to update your API client to the new API at the same time that you updated ejabberd.

Now the ejabberd API commands can have different versions, by default the most recent one is used, and the API client can specify the API version it supports.

In fact, this feature was [implemented seven years ago](https://github.com/processone/ejabberd/commit/3dc55c6d47e3093a6147ce275c7269a7d08ffc45), included in [ejabberd 16.04](https://www.process-one.net/blog/ejabberd-16-04/), documented in [ejabberd Docs: API Versioning](../../developer/ejabberd-api/api_versioning.md)... but it was never actually used!

This ejabberd release includes many fixes to get API versioning up to date, and it starts being used by several commands.

Let's say that ejabberd 23.10 implemented API version 0, and this ejabberd 24.02 adds API version 1. You may want to update your API client to use the new API version 1... or you can continue using API version 0 and delay API update a few weeks or months.

To continue using API version 0:
- if using ejabberdctl, use the switch `--version 0`. For example: `ejabberdctl --version 0 get_roster admin localhost`
- if using mod_http_api, in ejabberd configuration file add `v0` to the `request_handlers` path. For example: `/api/v0: mod_http_api`

Check the ejabberd [24.02](../../archive/24.02/index.md) full release notes for more details about the changed commands.

Check the full documentation in [ejabberd Docs: API Versioning](../../developer/ejabberd-api/api_versioning.md).

## <a name="mixdefault"></a>Use Mix or Rebar3 by default instead of Rebar to compile ejabberd

ejabberd uses [Rebar](https://github.com/rebar/rebar) to manage dependencies and compilation since ejabberd [13.10](https://www.process-one.net/blog/ejabberd-community-13-10/) [4d8f770](https://github.com/processone/ejabberd/commit/4d8f7706240a1603468968f47fc7b150b788d62f). However, that tool is obsolete and unmaintained since years ago, because there is a complete replacement:

[Rebar3](https://github.com/erlang/rebar3) is supported by ejabberd since [20.12](https://www.process-one.net/blog/ejabberd-20-12/) [0fc1aea](https://github.com/processone/ejabberd/commit/0fc1aea379924b6f83f274f173d0bbd163cae1c2). Among other benefits, this allows to download dependencies from [hex.pm](https://hex.pm/) and cache them in your system instead of downloading them from git every time, and allows to compile Elixir files and Elixir dependencies.

In fact, ejabberd can be compiled using `mix` (a tool included with the [Elixir programming language](https://elixir-lang.org/)) since ejabberd [15.04](https://www.process-one.net/blog/ejabberd-15-04/) [ea8db99](https://github.com/processone/ejabberd/commit/ea8db9967fbfe53f581c3ae721657d9e6f919864) (with improvements in ejabberd [21.07](https://www.process-one.net/blog/ejabberd-21-07/) [4c5641a](https://github.com/processone/ejabberd/commit/4c5641a6489d0669b4220b5ac759a4e1271af3b5))

For those reasons, the tool selection performed by `./configure` will now be:

- If `--with-rebar=rebar3` but Rebar3 not found installed in the system, use the `rebar3` binary included with ejabberd
- Use the program specified in option: `--with-rebar=/path/to/bin`
- If none is specified, use the system `mix`
- If Elixir not found, use the system `rebar3`
- If Rebar3 not found, use the `rebar3` binary included with ejabberd

## <a name="converse"></a>Authentication workaround for Converse.js and Strophe.js

This ejabberd release includes support for [XEP-0474: SASL SCRAM Downgrade Protection](https://xmpp.org/extensions/xep-0474.html), and some clients may not support it correctly yet.

If you are using [Converse.js](https://github.com/conversejs/converse.js) 10.1.6 or older, [Movim](https://github.com/movim/movim) 0.23 Kojima or older, or any other client based in [Strophe.js](https://github.com/strophe/strophejs) v1.6.2 or older, you may notice that they cannot authenticate correctly to ejabberd.

To solve that problem, either update to newer versions of those programs (if they exist), or you can enable temporarily the option [`disable_sasl_scram_downgrade_protection`](../../admin/configuration/toplevel.md#disable_sasl_scram_downgrade_protection) in the ejabberd configuration file `ejabberd.yml` like this:
``` yaml
disable_sasl_scram_downgrade_protection: true
```

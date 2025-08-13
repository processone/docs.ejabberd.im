# Understanding ejabberd and its dependencies

## Overview

We wanted to make sure that ejabberd is modular and that parts that can be of interest for other Erlang projects can be reused.

Not only we are massive open source contributors to Erlang community and ecosystem, but we are also trying to help even more by reviewing your pull requests. Do not hesitate to submit some on any of the many repositories mentioned here.

ejabberd codebase is split among several repositories, so effectively ejabberd code is much more than what is in its primary repository. And each of those repositories is now a dependency of ejabberd.

## Build tools

The dependencies are managed differently depending on the build tool being used:

* [Elixir Mix](https://hexdocs.pm/mix/Mix.html)

    - reads dependencies from file `mix.exs` (package version or git commit):
      ```elixir
      {:fast_tls, ">= 1.1.18"}
      {:yconf, git: "https://github.com/processone/yconf",
               ref: "9898754f16cbd4585a1c2061d72fa441ecb2e938",
               override: true}
      ```
    - downloads package from [hex.pm](https://hex.pm/), or git repository
    - caches package in `$HOME/.hex/`
    - and compiles in `deps/`

* [Rebar3](https://rebar3.org/)

    - reads dependencies from file `rebar.config` (package version or git commit):
      ```erlang
      {fast_tls, "~> 1.1.19", ...}
      {yconf, ".*", {git, "https://github.com/processone/yconf",
                          "9898754f16cbd4585a1c2061d72fa441ecb2e938"}}
      ```
    - downloads package from [hex.pm](https://hex.pm/), or git repository
    - caches package in `$HOME/.cache/rebar3/hex/`
    - and compiles in `_build/default/lib/`

* [Rebar2](https://github.com/rebar/rebar/wiki)

    - reads dependencies from file `rebar.config` (git tag or git commit):
      ```erlang
      {fast_tls, ..., {git, "https://github.com/processone/fast_tls",
                            {tag, "1.1.24"}}}
      {yconf, ..., {git, "https://github.com/processone/yconf",
                         "9898754f16cbd4585a1c2061d72fa441ecb2e938"}}
      ```
    - downloads from the corresponding git repository
    - does not cache it
    - and compiles in `deps/`

The dependencies are automatically downloaded from Internet, if not already cached on your machine.
Alternatively, you can copy the cache directory from another machine.

## Mandatory

The main ejabberd repository is [processone/ejabberd](https://github.com/processone/ejabberd).
There is hundreds of forks, but we actively maintain ejabberd to make it the most reliable and up to date version. This is thus your best starting point.

When you build ejabberd yourself, the build chain will download a few Erlang dependencies:

* [processone/cache_tab](https://github.com/processone/cache_tab):   Flexible in-memory Erlang caching module.
* [processone/fast_tls](https://github.com/processone/fast_tls): Erlang native driver for TLS / SSL. It is build for performance and is more scalable that Erlang SSL driver. If your Erlang server is handling heavy load and is using TLS, we strongly recommend you check / compare with this driver.
* [processone/fast_xml](https://github.com/processone/fast_xml): Fast native Expat based Erlang XML parsing library. XML is the core of XMPP so we needed to provide the fast and more robust XML parsing stack as possible. It means that if you are looking for a great XML parser, reusing p1_xml is probably a great idea.
* [processone/fast_yaml](https://github.com/processone/fast_yaml): Native Erlang interface to libyaml, for fast robust YAML parsing. This is needed by our new config file format.
* [processone/iconv](https://github.com/processone/iconv): Native iconv driver for Erlang. This is use for fast character encoding conversion.
* [processone/p1_utils](https://github.com/processone/p1_utils): This is extra Erlang modules developed for ejabberd but that can be useful in other Erlang applications.
* [processone/stringprep](https://github.com/processone/stringprep): Fast and efficient Erlang Stringprep native driver. Stringprep is heavily used in XMPP to define encoding rules of various XMPP objects.
* [basho/lager](https://github.com/basho/lager): Erlang logger module.

## Optional

Then, we use a few other dependent repositories that may be used if you have enabled support
in the [`./configure` script](../admin/install/source.md#configure):

* [processone/epam](https://github.com/processone/epam): epam helper for Erlang / Elixir PAM authentication support
* [processone/erlang-sqlite3](https://github.com/processone/erlang-sqlite3): Sqlite gen_server port for Erlang.
* [processone/esip](https://github.com/processone/esip): ProcessOne SIP protocol support to add SIP-based voice capabilities to ejabberd.
* [processone/ezlib](https://github.com/processone/ezlib): Native zlib driver for Erlang. Used for fast / efficient stream compression.
* [processone/mysql](https://github.com/processone/mysql): Pure Erlang MySQL driver.
* [processone/pgsql](https://github.com/processone/pgsql): Pure Erlang PostgreSQL driver
* [processone/stun](https://github.com/processone/stun): Implementation of [Session Traversal Utilities for NAT](https://en.wikipedia.org/wiki/STUN). It is used for XMPP and SIP media capabilities, to help client discover their visible IP address and allow them to get in touch through NAT. This is basically useful for voice and file transfers.
* [Nordix/eredis](https://github.com/Nordix/eredis): Erlang Redis client
* [rvirding/luerl](https://github.com/rvirding/luerl): Lua in Erlang

On the other hand, you can install additional ejabberd modules
from the [ejabberd-contrib repository](../admin/guide/modules.md#ejabberd-contrib).

---
title: ejabberd Frequently Asked Questions (FAQ)
toc: true
menu: FAQ
order: 40
---

# Development process

## Why is there a commercial version of ejabberd?

The needs from corporations and large scale deployments are very
different from the needs of smaller deployments and community.

While we put a huge development effort to have a product that are on
the edge of innovation with ejabberd community version, we are
requested to provide a stable version with long term commitment and
high level of quality, testing, audit, etc.

Maintaining such a version in parallel of the community version, along
with extremely strong commitment in term of availability and 24/7
support has a cost. With ejabberd business edition we commit on a
level of scalability and optimize thew software until it is performing
to the level agreed with the customer.

Covering all those costs, along with all our Research and Development
cost on ejabberd community in general is the real reason we need a
business edition.

The business edition is also a way for our customers to share the code
between our customers only, thus retaining a huge competitive edge for
a limited time (See next section).

So, even if you are not using our business edition, this is a great
benefit for you as a user of the community edition. This version is a
healthy contributors to ejabberd community edition and is the reason
you have seen so much improvements since 2002. Thanks to our business
edition customers, ejabberd project itself is a [major contributor to
Erlang and Elixir community](/developer/repositories/).


## Does ProcessOne voluntarily hold some code in ejabberd community to push toward the business edition?

No. We never do that and have no plan doing so with the code we
produce and we own.

However, when the code is paid by customer, they retain the ownership
of the code. Part of our agreement is that the code produced for them
will be limited to a restricted user base, ejabberd business edition
until an agreed time expires, generally between 6 months and 1 year.

This is extremely important for both the users of the commercial
edition and the users of the community edition:

- For the customers of the business edition, it means this is a way to
  ask for new developments and keep their business advantage. This is
  fair as they paid for the development.

- This is a great incentive for our customers as they also benefit of
  the development of other customers (I guess they agree for the
  reciprocal sharing of their own code with customers).

- This is fair for the community as the community edition users know
  they will benefit from new extremely advanced features in a not to
  long future. For example, [websocket module]() was contributed to
  ejabberd community as part of this process.

This is the model we have found to be fair to our all user base and
keep on producing an amazing code base that benefit to all our user
base.

This dual model is the core strength of our approach and our secret
sauce to make sure everyone benefits.

# Performance

## Is ejabberd the most scalable version?

Yes. Definitely. Despite claims that there is small change you can
make to make it more scalable, we already performed the changes during
the past year, that make those claims unfunded:

- ejabberd reduced memory consumption in 2013 by switching to binary
  representation of string instead of list. This can reduce given
  string by 8.

- We have improved the C code performance a lot, using new Erlang
  NIF. This provide better performance, removes bottleneck

- We have different clustering mechanism available to make sure we can
  scale to large clusters

This is a common misconception, but our feedback for production
service on various customer sites shows that ejabberd is the most
scalable version. Once it is properly configured, optimized and tuned,
you can handle tens of millions of users on ejabberd systems.

... And we are still improving :)

As a reference, you should read blog post:
[ejabberd Massive Scalability: 1 Node — 2+ Million Concurrent Users](https://blog.process-one.net/ejabberd-massive-scalability-1node-2-million-concurrent-users/)

## What are the tips to optimize performance?

Optimisation of XMPP servers performance, including ejabberd, is
highly dependant on the use case. You really need to find your
bottleneck by monitoring the process queues and finding out what is
your limiting factor, and move to the next one.

The first step is to make sure you run the latest ejabberd. Each new
release comes with a bunch of optimisations and a specific bottleneck
you are facing may have gone away in the latest version.

To give you an idea ejabberd 15.07 is 2 to 3 times more efficient in
memory, latency and CPU compared to ejabberd 2.1.

You should also make sure that you are also using the latest Erlang
version. Each release of Erlang comes with more optimisation regarding
locks, especially on SMP server, and using the latest Erlang version
can also help tremendously.

# Erlang support

## Is ejabberd conform to the best Erlang practices?

Yes. Our build system is primarily based on rebar. However, as we are
multiplatform and need to run in many various environments, we rely on a
toolchain that can detect required library dependancies using
autotools.

This gives developers and admins the best of both worlds. A very
flexible and very versatile build chain, that is very adequate to make
sure ejabberd can be used in most operating systems and even
integrated in Linux distributions.

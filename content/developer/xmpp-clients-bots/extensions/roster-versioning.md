# Roster versioning

Roster versioning as implemented currently by ejabberd is a simplified approach to roster versioning.

This is an all-or-nothing approach that does not support the granular diff as explained in [RFC-6121](https://tools.ietf.org/html/rfc6121#section-2.6).

Our implementation conforms to [version 0.6 of XEP-0237](https://xmpp.org/extensions/attic/xep-0237-0.6.html#example-3), sending the full roster in case of change or empty result if the roster did not change.

As a result, as a client developer, when implementing support for roster versioning, you should expect both the traditional form for returning the roster, with version (iq result) and the incremental roster changes (iq set).

### Example

As a summary, here is how you should expect it to work.

First, you can check that the feature is advertised in the `stream:features` as `urn:xmpp:features:rosterver`:

``` xml
<stream:features>
 <bind xmlns="urn:ietf:params:xml:ns:xmpp-bind"/>
 <session xmlns="urn:ietf:params:xml:ns:xmpp-session">
  <optional/>
  </session>
  <c xmlns="http://jabber.org/protocol/caps" node="http://www.process-one.net/en/ejabberd/" ver="/lmQr0llUEtX/pIt+6BDAbnIT/U=" hash="sha-1"/>
  <sm xmlns="urn:xmpp:sm:2"/>
  <sm xmlns="urn:xmpp:sm:3"/>
  <ver xmlns="urn:xmpp:features:rosterver"/>
</stream:features>
```

You can then bootstrap the use of roster versioning using empty `ver` attribute when sending your roster `get` iq:

``` xml
<iq id='roster1' to='myuser@domain.com' type='get'>
 <query xmlns='jabber:iq:roster' ver=''/>
</iq>
```

In return, you get a full roster with the current version:

``` xml
<iq from="myuser@domain.com" type="result" xml:lang="en" to="myuser@domain.com/resource" id="roster1">
 <query xmlns="jabber:iq:roster" ver="81cb523a7b77c7011552be85a3dde55189297590">
  <item subscription="both" jid="contact@domain.com">
   <group>Test</group>
  </item>
  ...
 </query>
</iq>
```

The client can store this version to send subsequent roster queries.

If client send a roster query with reference version it received get an empty iq result meaning the roster did not change:

``` xml
<iq id="roster2" to="myuser@domain.com" type="get">
 <query xmlns='jabber:iq:roster' ver='81cb523a7b77c7011552be85a3dde55189297590'/>
</iq>
```

``` xml
<iq from="myuser@domain.com" type="result" xml:lang="en" to="myuser@domain.com/resource" id="roster2"/>
```

If client send roster query with any other reference version, it will receive the full roster again in the roster iq result.

# MQTT Support

## Benefits

ejabberd is a multiprotocol server that supports [MQTT](https://mqtt.org/)
out of the box since ejabberd Business Edition 4.0 and
ejabberd Community Server 19.02

There are major benefits in using MQTT service embedded in ejabberd:

1. MQTT service relies on ejabberd infrastructure code, that has been battle tested since 15+ years, like the clustering
engine. ejabberd MQTT service has been tested on large scale and can support millions of concurrent connections highly
efficiently. ejabberd MQTT is rock-solid and highly scalable.
2. The ejabberd APIs and modules can be reused in MQTT. Authentication, virtual hosting, database backends, ... They both
work with XMPP and MQTT. You can also share your security policy, as defined in the configuration file between the two
protocols.
3. You can leverage existing skills and plugins you have written for ejabberd, like for example custom authentication.
4. You can deploy services that take advantage of both protocols and have them interoperate with each other, on a single
platform, with a single tool.
5. ejabberd supports MQTT 5: it is a state of the art, modern MQTT server.
And it also supports MQTT 3.1.1 in case you want to use previous clients.

In summary:

- You can switch between XMPP and MQTT as you wish, even use both protocols on the same infrastructure.
- You will save on infrastructure, given the high-performance of the platform.
- You get support on solution design for real-time infrastructure and can get help choosing between XMPP and MQTT, from
a vendor that has no interest in selling one protocol more than another.

ejabberd Business Edition offers a different clustering than [](def:eCS). Using MQTT with ejabberd Business Edition means
you can leverage:

- The clustering engine of [](def:eBE) will be used for the MQTT service. It means that you have a more scalable cluster, that supports
geoclustering. With geoclustering, you can deploy a single MQTT service across different datacenters, spread in different
regions. You can deploy a truly global service.
- The backend integration that are supported in ejabberd Business Edition will be available in MQTT. You have no need
to develop support for new API.

## Basic Setup

Maybe you already have MQTT enabled in your ejabberd server,
as it comes enabled by default in many distributions.

MQTT support in ejabberd is enabled by adding `mod_mqtt`
to the list of `listen` and the list of `modules` like this:

``` yaml
listen:
  -
    port: 1883
    module: mod_mqtt
    backlog: 1000

modules:
  mod_mqtt: {}
```

The listener on port 1883 is MQTT over cleartext TCP/IP connection;
you can later setup encryption, WebSocket, and encrypted WebSocket.

For available options you can consult
the [mod_mqtt listener](../../configuration/listen.md#mod_mqtt)
and the [mod_mqtt module](../../configuration/modules.md#mod_mqtt).

## Test Setup

Start ejabberd server and you can connect to ejabberd MQTT service with your preferred MQTT client.

Let's use the clients included with [mosquitto](https://mosquitto.org/),
available in [Debian](https://packages.debian.org/sid/mosquitto-clients),
[Brew](https://formulae.brew.sh/formula/mosquitto)
and many others (see [mosquitto downloads](https://mosquitto.org/download/)).

First of all register several accounts and subscribe one to the topic `test/1` with:

``` sh
ejabberdctl register author localhost Pass
ejabberdctl register user1 localhost Pass

mosquitto_sub -u user1@localhost -P Pass -t "test/1" -d -v

Client (null) sending CONNECT
Client (null) received CONNACK (0)
Client (null) sending SUBSCRIBE (Mid: 1, Topic: test/1, QoS: 0, Options: 0x00)
Client (null) received SUBACK
Subscribed (mid: 1): 0
```

Then go to another terminal or window and publish something on that topic:

``` sh
mosquitto_pub -u author@localhost -P Pass -t "test/1" -d -m "ABC"

Client (null) sending CONNECT
Client (null) received CONNACK (0)
Client (null) sending PUBLISH (d0, q0, r0, m1, 'test/1', ... (3 bytes))
Client (null) sending DISCONNECT
```

You will see the message received and displayed in the `mosquitto_sub` window:

``` sh
Client (null) received PUBLISH (d0, q0, r0, m0, 'test/1', ... (3 bytes))
test/1 ABC
```

## Access Control

The [mod_mqtt module](../../configuration/modules.md#mod_mqtt)
provides two options for access control:

- `access_subscribe` to restrict access for subscribers,
- and `access_publish` to restrict access for publishers.

Both options accept mapping `filter: rule`
where `filter` is an MQTT topic filter and `rule` is the standard
ejabberd [Access Rule](../../configuration/basic.md#access-rules).

As an example, let's say only `author@localhost` is allowed to publish
to topic "/test/1/" and its subtopics,
while only `user1@localhost` is allowed to subscribe
to this topic and its subtopics,
and nobody else can publish or subscribe to anything else.
The configuration will look something like this:

``` yaml
acl:
  publisher:
    user: author@localhost
  subscriber:
    user: user1@localhost

modules:
  mod_mqtt:
    access_publish:
      "test/1/#":
        - allow: publisher
        - deny
      "#":
        - deny
    access_subscribe:
      "test/1/#":
        - allow: subscriber
        - deny
      "#":
        - deny
```

## Encryption

### Self-Signed Certificate

If you have already setup encryption in ejabberd, you can bypass this step.

If you want to use TLS, you may want to create a self-signed certificate
(at least to get started). The following page is a nice guide:
[Mosquitto SSL Configuration -MQTT TLS Security](http://www.steves-internet-guide.com/mosquitto-tls/).

Here is a summary of the steps, adapted for ejabberd MQTT:

``` sh
openssl genrsa -des3 -out ca.key 4096
openssl req -new -x509 -days 1826 -key ca.key -out ca.crt
openssl genrsa -out server.key 4096
openssl req -new -out server.csr -key server.key
openssl x509 -req -in server.csr -CA ca.crt -CAkey ca.key -CAcreateserial -out server.crt -days 360
cat server.crt server.key > mqtt.pem
```

Now copy `mqtt.pem` to the path with ejabberd configuration files,
and configure accordingly:

``` yaml
certfiles:
  - "/etc/ejabberd/mqtt.pem"
```

### Configure Encryption

Add a new listener with `tls` option
in the port number 8883 (the standard for encrypted MQTT):

``` yaml
listen:
  -
    port: 1883
    module: mod_mqtt
    backlog: 1000
  -
    port: 8883
    module: mod_mqtt
    backlog: 1000
    tls: true
```

The listener on port 1883 is MQTT over cleartext TCP/IP connection.
The listener on port 8883 is MQTT over TLS.
You can enable both or only one of them depending on your needs.

### Test Encryption

You can repeat the commands from previous test,
appending `-p 8883` to use the encrypted port.
If you are using a self-signed certificate as explained previously,
you will also have to append `--cafile server.crt`.
For example:

``` sh
mosquitto_sub -u user1@localhost -P Pass -t "test/1" -d -v -p 8883 --cafile server.crt
```

## WebSocket

### Setup WS

Add `mod_mqtt` as a
[request_handler](../../configuration/listen-options.md#request_handlers)
on the [ejabberd_http](../../configuration/listen.md#ejabberd_http) listener:

``` yaml
listen:
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      /mqtt: mod_mqtt
```

This configuration maps the path `/mqtt` to the MQTT WebSocket handler
on the main ejabberd HTTP listener.

You can enable listeners independently, for example enable only
the WebSocket listener and not the TCP/IP ones.

### Test WS

Our beloved mosquitto client does not support MQTT over WebSocket,
so you may have to find some capable MQTT client.
For example, in [MQTTX](https://mqttx.app/), setup in the login window:

- Host: `ws://` `localhost`
- Port: 5280
- Path:`/mqtt`

If you need an example on how to use MQTTJS library, you can check our small example project:
[mqttjs-demo](https://github.com/processone/mqttjs-demo)

### Encrypted WS

To enable encryption on WebSocket, enable `tls` like this:

``` yaml
listen:
  -
    port: 5281
    ip: "::"
    module: ejabberd_http
    tls: true
    request_handlers:
      /mqtt: mod_mqtt
```

For testing this in the MQTTX client:

- Host: `wss://` `localhost`
- Port: 5281
- Path: `/mqtt`
- SSL/TLS: true
- Certificate: CA signed server
- If you used a self-signed certificate, you will have to disable SSL Secure

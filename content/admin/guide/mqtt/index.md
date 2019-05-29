---
title: MQTT Support
menu: MQTT
order: 35
toc: true
---

ejabberd is a multiprotocol server that can support MQTT out of the box, since ejabberd Business Edition 4.0 and
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
5. ejabberd MQTT supports MQTT 5. It is a state of the art, modern MQTT server.

The bottom line is simple:

- You can switch between XMPP and MQTT as you wish, even use both protocols on the same infrastructure.
- You will save on infrastructure, given the high-performance of the platform.
- You get support on solution design for real-time infrastructure and can get help choosing between XMPP and MQTT, from
a vendor that has no interest in selling one protocol more than another.

## Specific benefits of MQTT service in eBE

ejabberd Business Edition offers a different clustering than eCS. Using MQTT with ejabberd Business Edition means
you can leverage:

- The clustering engine of eBE will be used for the MQTT service. It means that you have a more scalable cluster, that supports
geoclustering. With geoclustering, you can deploy a single MQTT service across different datacenters, spread in different
regions. You can deploy a truly global service.
- The backend integration that are supported in ejabberd Business Edition will be available in MQTT. You have no need
to develop support for new API.

## Enabling MQTT service in ejabberd

### Creating self-signed TLS certificate

If you want to use TLS, you may want to create a self-signed certificate (at least to get started). The following page
is a nice guide: [Mosquitto SSL Configuration -MQTT TLS Security](http://www.steves-internet-guide.com/mosquitto-tls/).

Here is a summary of the steps, adapted for ejabberd MQTT:

```bash
openssl genrsa -des3 -out ca.key 4096
openssl req -new -x509 -days 1826 -key ca.key -out ca.crt
openssl genrsa -out server.key 4096
openssl req -new -out server.csr -key server.key
openssl x509 -req -in server.csr -CA ca.crt -CAkey ca.key -CAcreateserial -out server.crt -days 360
cat server.crt server.key > mqtt.pem
```

### Adding MQTT service in ejabberd.yml

Enabling it is as simple as adding `mod_mqtt` in ejabberd modules list and adding one or two listeners:

```yaml
hosts:
  - "localhost"

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

# adapt the path to your own certfile    
certfiles:
  - "../../conf/mqtt.pem"

modules:
...
  mod_mqtt: {}
```

The listener on port 1883 is MQTT over cleartext TCP/IP connection. The listener on port 8883 is MQTT over TLS. You can 
enable both or only one of them depending on your needs.

**Be careful**: option `certfile` does not work directly on listener, you must use global *certfiles* option.

Start ejabberd server and you should be able to connect to ejabberd MQTT service.

## MQTT over Websocket

You can add an extra listener to enabled MQTT over Websocket:

```yaml
listen:
      ...
      -
        port: 5280
        module: ejabberd_http
        request_handlers:
           ...
           "/mqtt": mod_mqtt
```

This config directive maps the path */mqtt* to the MQTT Websocket handler, on the main ejabberd HTTP listener. 

_Note:_ You can enabled listeners independently. It is possible to enable only the Websocket listener and not the TCP/IP 
ones. 

If you need an example on how to use MQTTJS library, you can check our small example project: 
[mqttjs-demo](https://github.com/processone/mqttjs-demo)

## Access control configuration

MQTT support in ejabberd supports advanced configuration using ejabberd ACLs.

The module mod_mqtt provides two options for access control: `access_subscribe` and `access_publish`. The former can be  
used to restrict access for subscribers and the latter can be used to restrict access for publishers. Both accept  
mapping `filter: rule` where `filter` is an MQTT topic filter and `rule` is the standard 
[ejabberd access rule](/admin/configuration/#acl-definition).

As an example, let's say `user1@domain.tld` is only able to publish to topic "/foo/bar/" and its subtopics, 
while `user2@domain.tld` is only able to subscribe to this topic and its subtopics. The configuration will look 
something like this:

```yaml
acl:
  ...
  publisher:
    user:
      "user1" : "domain.tld"
  subscriber:
    user:
      "user2" : "domain.tld"

modules:
  ...
  mod_mqtt:
    access_publish:
      "/foo/bar/#":
        - allow: publisher
        - deny
      "#":
        - deny
    access_subscribe:
      "/foo/bar/#":
        - allow: subscriber
        - deny
      "#":
        - deny
```


## Testing MQTT from the client perspective

If you are a developer and would like to try connecting your client to MQTT service, you can either use our test public
MQTT broker, or on your own server.

### Testing on mqtt.fluux.io

ProcessOne has deployed a public MQTT broker with anonymous access. This is one of the first MQTT 5 public brokers available.

The broker is deployed at `mqtt.fluux.io` on port 1883 and 8883 (TLS) and allows anonymous access.

You can, for example, test our broker with mosquitto client. On macOS, you can install mosquitto via Terminal using Homebrew with:

```bash
brew install mosquitto
```

From there, you can subscribe to the topic `test/topic` with:

```bash
mosquitto_sub -h mqtt.fluux.io -p 1883 -V mqttv311 -v -t "test/topic" -q 2
```

and then (in another Terminal tab or window) you can publish with:

```bash
mosquitto_pub -h mqtt.fluux.io -p 1883 -V mqttv311 -t "test/topic" -m "message" -q 2
```

You should see your message received and displayed below the `mosquitto_sub` command in your previous Terminal tab or window.

*Note: In this example, Mosquitto client is using MQTT v3.1.1, as it doesn’t support version 5.0 at the moment of writing.*

### Testing on your own server with authentication and TLS

I assume that you have configured ejabberd on `localhost` (for development) and have created user `mqttuser@localhost`
with password `mqtt`. You can then connect using TLS with the `mosquitto` client.

First, subscribe to `test/topic` by executing this in a Terminal:

```
mosquitto_sub -V mqttv311 -v -t 'test/topic' -h localhost -p 8883 -u mqttuser@localhost -P mqtt --cafile ca.crt
```

**Note**: We are using our Certificate Authority cert that we created earlier, so that mosquitto can trust our server.

You can then publish in another Terminal and check that your subscriber properly receives your message:

```
mosquitto_pub -V mqttv311 -t test/topic -m "Hi, from ejabberd MQTT"  -h localhost -p 8883 -u mqttuser@localhost -P mqtt --cafile ca.crt
```

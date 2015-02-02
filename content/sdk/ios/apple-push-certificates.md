---
title: Apple Push Certificates | Boxcar Push Service
---

# Apple Push Certificates: creating .pem

To register an iOS client on Boxcar Push Service, you need to provide Apple push certificates for development and production version.

It is a `.pem` file without password protection. You can generate it from any of the following:

* `.p12` keychain export
* `.cer` file coming from Apple Provisioning portal

Follow Apple documentation, you can find on [Provisioning and Development][1]. This will allow you to export a `.p12` file
from Apple Keychain (without password).

From there you can use the following commands / script to generate your .pem certificate:

~~~ bash
#!/bin/bash
openssl pkcs12 -clcerts -nokeys -in aps_developer_identity_cert.p12 \
    -out aps_developer_identity_cert.pem
openssl pkcs12 -nocerts -in aps_developer_identity_cert.p12 \
    -out aps_developer_identity.pem
openssl rsa -in aps_developer_identity.pem -out aps_developer_identity_key.pem
cat aps_developer_identity_cert.pem aps_developer_identity_key.pem > apd.pem
~~~

You have to do that once for development certificate and once for production certificate.

Sandbox [^1] and production passwordless certificates `.pem` can be then upload on Boxcar Push Service.

[^1]: Sandbox is for "development" mode. You use it for applications uploaded to your device directly from XCode.

[1]:	https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/ProvisioningDevelopment.html

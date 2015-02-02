---
title: Preparing project, client applications and publisher settings | Boxcar Push Service
---

# Boxcar Developer Console: Projects settings

This guide assume that you have created a brand new account on [Boxcar Console](http://console.boxcar.io) and thus are one of the owner for your organisation.

## Create a project

A project is a "consistent group of applications": For example, a project can be the name of your main application.
It can contains several clients which are releases of your application on the same mobile OS but on different platform, or on totally different mobile OS.

Typically, to get started, just create a project which is the name of your application.

→ [Create a project](https://console.boxcar.io/organizations/0/projects/new)

![][image-1]

## Create at least a client

A client is needed to use your project and start registering devices. You cannot add devices from Boxcar Console. The devices add themselves to the console when you run.

You can have many clients serving your project, for example: an iOS client, an Android client. You can also have different clients for the same OS, for example: an iPhone client and an iPad client.
Note: If you use Phonegap SDK, you need to create a client per platform (i.e. one for iOS and one for Android)

→ [Create an iOS client for APNS](https://console.boxcar.io/organizations/0/projects/0/client_applications/new?target_os=ios)

→ [Create an Android client for GCM](https://console.boxcar.io/organizations/0/projects/0/client_applications/new?target_os=android)

→ [Create a Kindle Fire client for ADM](https://console.boxcar.io/organizations/0/projects/0/client_applications/new?target_os=kindle)

→ [Create a Nokia X client for Nokia Push](https://console.boxcar.io/organizations/0/projects/0/client_applications/new?target_os=nokia)

After client creation, you can get:

* API key and secret that you need to pass to our SDKs.
* Third-party push credentials (Certificates for Apple Push Notification Service, Tokens for other services).

![][image-2]

## Create a publisher (Optional)

If you want to use our publisher API to integrate with your server backend to send pushes automatically, you will need to support at least a publisher.
This is optional as you can use the push form to send pushes to your clients.

→ [Create a publisher]()

After publisher creation, you will get push credentials that will allow your backend to send push notification.
Example scripts showing how to send notifications are provided.

![][image-3]

[image-1]:	/images/console/boxcar-console_create-project.png
[image-2]:	/images/console/boxcar-console_create-client.png
[image-3]:	/images/console/boxcar-console_create_publisher.png

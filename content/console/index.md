---
title: Getting Started with Boxcar Developer Console | Boxcar Push Service
---

# Getting started with Boxcar Developer Console

[Boxcar Console](https://console.boxcar.io/) is your control center to manage [Boxcar Developer Push](http://boxcar.io/developer)
features, register your client applications, troubleshoot and analyse the impact of your notifications on your user base.

**Note:** If you are a [Boxcar iOS](http://boxcar.io/client) client user, you are probably looking for to the
[Boxcar iOS client API](https://boxcar.uservoice.com/knowledgebase/topics/48115-boxcar-api), not the Boxcar Push Developer API.
You can for example, use the API to send a notification on your own mobile in Boxcar.

## Configuration

As a developer, the first thing to do is to:

1. **Create a account** for your company on [Boxcar Console](https://console.boxcar.io). Creating a developer account is free.
2. **Configure** push console to be up and running and be ready to send notifications to your applications.
    - [Project settings](project_settings)
3. **Integrate Boxcar SDK** in your mobile application.

    Once, you have a account on Boxcar console, you can download one of our mobile SDK: [SDK Downloads](https://console.boxcar.io/downloads)

    Here are direct links to mobile SDKs documentation:

    - [iOS SDK](/sdk/ios)
    - [Android SDK](/sdk/android): Covers Google Cloud Messaging (GCM), Amazon Device Messaging (ADM) and Nokia X Push Service.
    - [Phonegap SDK](/sdk/phonegap)

4. **Integrate with publisher API** to send push from your back-end system to your own iOS applications.
This is server-side API calls. This is an optional step, as you can send push using Boxcar Push Console form.
Make sure you have created a publisher in console and try the example scripts provided on your publisher page before
getting to more complex API calls.

    - [Publisher API](/api/publisher)

## Troubleshooting

The platform offer a realtime developer console that allows you to get information on the call being made to the API.

We also have a FAQ available on our [support site](https://boxcar-api.uservoice.com) that guide you through the most common issues.

## Analytics

Our platform offers realtime analytics that give you feedback on:

- What is currently being send to users.
- How your users are interacting with the push notifications.

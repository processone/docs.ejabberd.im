---
title: Publisher API Introduction | Boxcar Push Service
version: 1.0.11
---

# Publisher API Introduction

Latest API Version: <%= @item[:version] %>

* TOC
{:toc}


## Pre-requisites

Before you can send pushes through Boxcar Push API to reach your devices, you need to obtain from Boxcar Push Console the following information:

* **HOST**: This is the server to use to send HTTPS API calls.
* **PUBLISH\_KEY** and **PUBLISH\_SECRET**: This is needed to either sign API calls or to use basic HTTP authentication to verify the application publishers rights. Note: The publisher need to be allowed to use basic authentication over HTTPS as an alternative method to URL signature over HTTPS, to be allowed to call URL without implementing signature algorithm.

## Goal of this document

This documents presents the basic set of feature to allow to integrate with back-end most common needs.

It is not a reference document presenting all the features of the API but should get you up and running quickly on using Boxcar Push API.

## General API requirements

Data are send using JSON format.

It means the "Content-Type" must be "application/json".

### CURL URL example:

	curl -X POST -u "<publish_key>:<publish_secret>" \
	     -H "Content-Type: application/json" \
	     --data '<JSON formatted payload>' \
	     https://HOST/api/push/ 

The above example uses Basic Authentication.  For description of the authentication methods supported,
refer to the  "URL Signature" section on this document.

## Publisher methods

### Sending push notifications ― POST /api/push

Notifications are send by sending a POST HTTP requests on */api/push* on the proper HOST.

Here is a basic push notification example to send data to all subscribers in the 'football' channel:

	curl -X POST -u "PuGjDgxlYfpAxOIkn:ewPljwerrjx" \
	     -H "Content-Type: application/json" \
	     --data @push.json \
	     https://HOST/api/push/ 

And here is the associated payload in the push.json file:

	{
	 "aps":{
	     "badge":"auto",
	     "alert":"Beckham in first training session with Paris-St-Germain",
	     "sound":"sport.caf"},
	 "tags":["football"],
	 "id":"WEAXPVRK"
	}

Sending an HTTP request with a JSON formatted payload is all is needed to get started sending pushes from a publisher.

After successfully accepting the request,  the server will return a 201 (Created) response.  The body of the response is a simple json document that contains the
ID of the created push:

	{"ok":"141"}

As per HTTP specs,  the full url of the created push is also returned in the "Location" header.


Here are some of the first level keys you can you in your JSON data. Except **aps** (Apple Push Service), which hold the basic content to support push, all others values are optional. It means you can start simple and build more and more complex behavior.

#### aps (iOS and Android, with some reserves)

This is the standard Apple Push Notification content. It is a mandatory object, that need to contain at least one of its optional subkeys, among badge, alert, sound.

In its most basic form, this is a JSON object itself containing three optional keys:

* **badge** (optional, iOS only, ignored on Android): Its value can be either an integer, but the most common practice on Boxcar Push Service is to use "auto" to let Boxcar platform handle automatically the counter. The platform will automatically reset that badge when the user open the target application. If omitted, the badge on the application will stay unchanged.

* **alert** (optional, iOS and Android): This is the text of the message that will be send as content for notification alert message (popup and notification center). If omitted, no popup nor alert text will be displayed.

* **sound** (optional, iOS and Android, with some restrictions): This is the sound file to play on the device when the notification is received. If omitted, no sound will be played. To play the default sound, you can simply use the value "default" or any non existing shorter name to save space.

* **category** (optional):  This corresponds to the  "category" field on APNS notifications. It is used to add notification actions, allowing the user to interact with the received notification.  If omitted, no category will be included.

More complex values of the **aps** object are supported. The platform supports the variations in the payload, as described in [Apple Push Notification Programming Guide][1]. However, they should be used with care to support cross platform notifications.

Example:

	{
	 "aps":{
	  "badge":"auto",
	  "alert":"Beckham in first training session with Paris-St-Germain",
	  "sound":"sport.caf"}
	}

This info is passed to third-party push service (reformatted for correct behaviour on each platform).

#### Device selection criteria

You have to use one and only one criterium among the following to describe to which devices set you wish your push notification to be send.

##### aliases (iOS and Android)

The **aliases** value is an array of string, defining the list of devices to send the notification to, identified by their assigned identifier. It is usually the numeric id of the user as known by the mobile device or the email address.
This allows to send notification to individuals instead of device tokens. The various publishers can thus send notification with the identifier they use in their system and Boxcar Push Service is in charge of making the matching.

This info is not passed to third-party push service and is used by our push platform to select the list of devices to push to, but is not passed to devices.

##### tags (iOS and Android)

The **tags** value is an array of string, defining the list of tagged devices that the notification need to be send to.
You can imagine tags as channels to which the devices subscribe to.

Tags cannot start with an at sign (@) as this is a reserved prefix for Boxcar system tags. We have list of predefined tags that can be used directly but the platform, for example:

* **@all**: to send to all devices in the project (no matter which type / OS they are).
* **@anonymous**: Send a notification to all devices that have no defined alias.
* **@registered**: Send a notification to all devices that have a defined alias.

For custom tags, expected naming is lower case string containing only alphanumeric chars, dash, dot and underscore). When languages are used, it is common to append it to tag.
Example:

- breaking-news\_en
- breaking-news\_fr
- sport
- my-news-category-1
- my-news.category-1

This info is not passed to third-party push service and is only used by our push platform to select the list of devices to push to. As such is not passed to devices.

Important note: When register a device with an incorrect tag, tags parameter is ignored a warning is returned, but device registration succeeds.

##### device\_tokens (iOS and Android)

The **device\_tokens** value is an array of device identifiers for iOS or Android (as string). It is very rare to use it directly outside of the development phase as it requires the sender to have access to that information.
More commonly, the publishers prefers aliases of tags to address a set of devices managed by Boxcar Push Service.

For iOS, this is the device token. For Android, this is the device registration ID.

This info is not passed in the payload of the push to third-party push service. It is used to define the list of devices to send the notification to, but is not passed to devices.

#### Application filter criteria

There is no limit on usage of application filter criteria. You can combine any or all of them with with any device filter criterium.

##### target\_os (iOS and Android)

This is an array of string containing the list of platform to push to:

* **ios**: Send a notification to all iOS devices.
* **android**: Send a notification to all Android devices.
* **kindle**: Send a notification to all Kindle Fire devices.

No value or an empty array means: "push to all available platforms".

This info is not passed to third-party push service and is used by our push platform to select the list of devices to push to, but is not passed to devices.

##### client\_ids (iOS and Android)

We have a parameter in the API that can limit to which clients in the project the notification will be limited to. The _client\_ids_ key contain a list of integer, which is the id of the client, as created in the push platform.

Example:

	{
	 "client_ids":[1,2],
	 "aps":{"badge":"auto",
	        "alert":"alert message",
	        "sound":"default.caf"},
	 "id":"JHQRKHEE"
	}

This fields can be combined with _tags_ filter.

#### Example combination of filters

Here is a more complex push payload mixing target\os and tags. Please, note that if a device appears more that once through the various parameters, the platform will take care of sending only one push notifications:

	{
	 "tags":["politics","sports"],
	 "target_os":"ios",
	 "aps":{"badge":"auto",
	        "alert":"alert message",
	        "sound":"default.caf"},
	 "id":"JHQRKHEE"
	}

#### Other fields

##### id (iOS and Android)

The **id** value is a string.
This is the id of the push in the publisher system. If no id is provided, Boxcar Push Service will generate one automatically.

This info is passed to third-party push service in the push payload.

##### rich\_content (iOS and Android)

The **rich\_content** value is extended HTML content to display on the device once the application is opened. This is the associated content to download to provide the real content matching the alert for the users.

This info is not passed in push payload to third-party push service but can be used by mobile devices to retrieve additionnal payload on our Push Service. This allows to bypass limitations of the third-party push services.

##### expires_after (iOS and Android)
Set the time to live, in seconds,  time for this push.  The push is droped if the device is not able to receive it the next T seconds after the push was sent. This
could be because the device is out of reception,  turned off, or some other circumstance that make GCM/APNS not delivering it immediately.  Refer to the official GCM/APNS docs for specific
conditions. Example, expire after 30 minutes:


	{
	 "tags":["politics","sports"],
	 "target_os":"ios",
	 "aps":{"badge":"auto",
	        "alert":"alert message",
	        "sound":"default.caf"},
	 "id":"JHQRKHEE",
	 "expires_after": 1800
	}

##### @img (Android only)
Set the URL for an image to be displayed as a big background picture when the notification is expanded. The image will be center-cropped. When the push arrives, the device establishes an HTTP connection, reads the image from the remote site and sets it as the background picture for the notification widget. Source picture should be ≤ *450 DP wide*, *~2:1 aspect*. Bitmaps outside these bounds will just be wasting RAM (and possibly exceeding Binder IPC limits).

As a reference, *450 DP* is equivalent to the following amount of pixels, depending on the density type of the hardware:

* LDPI devices: 337,5 pixels
* MDPI devices: 450 pixels
* HDPI devices: 675 pixels
* XHDPI devices: 900 pixels
* XXHDPI devices: 1350 pixels
* XXXHDPI devices: 1800 pixels

Example:

	{
	 "tags":["sports", "tennis"],
	 "target_os":"android",
	 "aps":{"badge":"auto",
	        "alert":"Petkovic wins Tournament of Champions final",
	        "sound":"default.caf"},
	 "id":"JHQRKHEE",
	 "@img":"http://mysite/news/res/petkovic.jpg"
	}

This feature is available only on Android devices with API level >= 16. 

#### Custom fields

Custom fields can be used. All value, not reversed by the platform is transparently passed in the push payload to the device.
Example, to pass a custom object\_id you can add a custom field "oid":

	{
	 "tags":["@all"],
	 "aps":{"badge":"auto",
	        "alert":"alert message",
	        "sound":"default.caf"},
	 "oid": 12,
	 "id":"JHQRKHEE"
	}

#### Other

Other values are available for specific needs, like selecting set of devices based on the event of some actions (created, registered, last seen, for example).


### Cancelling a push notifications being sent ― DELETE /api/push/$PUSH_ID
A notification being sent can be cancelled. 

Note: the push must exists.  If the push has already finished, this call has no effect.

Return code is 202 "Accepted". The actual cancellation is performed asynchronously, depending on your subscription plan delivery speed, it migh take a few seconds to completely stop.

### Getting info about an existing push - GET /api/push/$PUSH_ID

Returns a json struct with information about the given push. 
Current returned fields are:
	
	{
		"state":"delivered",
		"created_at":"2014-08-11T17:33:08Z",
		"last_delivered_at":"2014-08-11T17:35:36Z",
		"sent":3507600,
		"errors":0,
		"opened":401462
	}

state can be one of "queued" | "delivering" | "delivered" | "cancelled"

Warning: 
	don't rely too much on the structure of this response.  Fields other than "state" are susceptible to changes in future versions.




## URL Signature

If "Basic Authentication" is allowed for the publisher, implementation of the signature algorithm is optional. Basic authentication can thus be used. While being a bit less secure, it will simplify development and deployment in some complex environment.

Clients can never bypass URL signature, but the algorithm is always implemented in the provided libraries.

So, if you want to add an extra security layer to your publisher code or want to reimplement the client algorithm yourself, you can implement the signature algorithm.

The signature algorithm is already implemented in the provided *bx\_signature.rb* script. You can use it as an example for implemented the optional signature algorithm in another programming language. The value of the signature is the result of HMAC SHA1 operation with PUBLISH\_SECRET of the URL path and payload concatenated as:

	method\nhost\nabsolute_path\nbody

* method is the HTTP method (uppercase), usually "POST"
* host is the hostname (lowercase). Does not include port.
* absolute\_path is the full query path without parameters
* body is the content of the POST.

If you need to use URL signature, you will add two extra parameters in your HTTP query:

* publish\_key=\<publish\_key\>
* signature=\<generated\_signature\>

and it adds a validity duration in JSON payload:

* an "expires" key is added to JSON. This is the timestamp ([epoch date][2]) of the query. It defines at which time the query will expire and be rejected. This is define for security reason to avoid replay of the same signed identical query at a later time.

For example:

	POST /api/push?publishkey=PuGjDgxlYfpAxOIkn!hCaX5jo&signature=1ea79daec692258278c2d3d5e88ae5f5a906c7f5 HTTP/1.1
	Content-Type: application/json
	Accept: */*
	User-Agent: Ruby
	Host: localhost:8001
	Content-Length: 217

	{"expires":1360790596,
	"device_tokens":["67944d0694d688a67a7dd4d0e7cbc961eb5d5c7f3b7de761406ad3778101506e"],
	"aliases":["my_username"],
	"aps":{"badge":1,"alert":"alert message","sound":"default.caf"},
	"id":"WEAXPVRK"}

## Working examples

You can find a publish tool implementation written in Java and ready for use at our [GitHub repository](https://github.com/boxcar/boxcar-developer-push-examples):
    
    https://github.com/boxcar/boxcar-developer-push-examples/tree/master/java-publisher-client




[1]:	http://developer.apple.com/library/ios/%23documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Introduction/Introduction.html
[2]:	http://www.epochconverter.com

---
title: iOS SDK 1.1 Introduction | Boxcar Push Service
version: 1.1.0
---

# iOS SDK Introduction

Latest SDK Version: <%= @item[:version] %>

* TOC
{:toc}

## Pre-requisites

Creating push environment using Apple tools is not easy and is full of pitfalls.
In case of trouble, do not hesitate to contact ProcessOne support for Boxcar Push Service by sending a mail to [support@boxcar.uservoice.com][1].

You need to prepare the following before you can get started with the code part:

1. [Apple Push Certificate Creation](apple-push-certificates/)
2. [Boxcar Push Console Client Registration](/console/managing-mobile-clients/)
3. [Download Boxcar iOS SDK from Boxcar Developer Console](https://console.boxcar.io/downloads) (login required)

After you have completed those steps, you should have the following elements:

* Apple _Push Certificates_ for your application, likely Sandbox and production certificate.
* Boxcar _ClientKey_ and _ClientSecret_: This is two strings embedded in your client application. They allow your application to register a device, notify the server about opened notifications. They are use to sign your request and "authenticate" your application.
* Boxcar _APIURL_: This is the endpoint to use for your organisation for all API calls.

## Prepare your XCode project to use the Boxcar iOS SDK

### Add the frameworks

To use the Boxcar SDK in your project, you need to drag and drop the provided `Boxcar.framework` directory in your _Frameworks_ group in your XCode project:

![][image-1]

### Update your Info.plist

Starting from iOS 7, you can configure your application to support ability to be wake up by notifications. This is the
most flexible way to do background fetch, as it is controlled by your server. It offers a very fluid user experience,
allowing to display the right up to date content immediately when user opens the notification.

Remote notification background more is an iOS 7 feature, but the parameter is simply ignored on iOS 6, so it is safe to add.
In you project _\*info.plist_ file, add the key _UIBackgroundModes_ (XCode name: Required background modes).
This is an array, so add a new sub-item and give it the value _remote-notification_ (XCode name: App downloads content in
response to push notifications).

Thanks to this value, your application will be allowed 30 seconds running time when a notification with `content-available`
flag is set to `true`. Typically, this time is used to download the new content in background. Thanks to that, when your user will
open the notification, the content will be already there, providing a very smooth user experience.

Here is how UIBackgroundModes addition should look in your _\*info.plist_ file:

![][image-3]



### Step by step integration guide

#### Import Boxcar Framework

Make sure you import the Boxcar framework header where you need to use it. For example, add this line at the top of your
`AppDelegate.h file:

~~~ objectivec
#import <Boxcar/Boxcar.h>
~~~

#### AppDelegate: \- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions

##### Configure and start the Boxcar framework

You can prepare a NSDictionary, that contains the relevant options:

~~~ objectivec
NSDictionary *boxcarOptions = @{
     kBXC_CLIENT_KEY: @"rqet2tBuxHXzzrJmhyAfwPIyxHX6_e7lkRysPj7MXBzEfVmI",
     kBXC_CLIENT_SECRET: @"t5DWCxESkq_F6bVgpTxEroy7fs4XT4SS1pjXIeH5zhifRs4B",
     kBXC_API_URL:       @"https://console.boxcar.io",
     kBXC_LOGGING:       @YES
  };
~~~

And pass it to Boxcar instance *startWithOptions:error: method*:

~~~ objectivec
[[Boxcar sharedInstance] startWithOptions:boxcarOptions error:nil];
~~~

Mandatory parameters are:

* **kBXC\_CLIENT\_KEY**: Your application client key on Boxcar Push console.
* **kBXC\_CLIENT\_SECRET**: Your application client sevret on Boxcar Push console.
* **kBXC\_API\_URL**: API endpoint URL.

Logging can be enable / disabled with the boolean parameter:

* **kBXC\_LOGGING**: This is a boolean. We recommend you set it to *YES* while you develop and to *NO* in your Apple AppStore build (This is important to avoid you client key appearing in the logs).

##### Define your push mode

If you upload your application to your device through your cable during development phase, you will have to use the sandbox push certificate to receive push notifications. This is done by using __"development"__ mode.
If you are distributing your application (through adhoc mode, App Store or enterprise), then you have to set you application in __"production"__ mode.

If your application configuration define DEBUG only in development, then a typical pattern to use is:

~~~ objectivec
# ifdef DEBUG
[[Boxcar sharedInstance] setMode:@"development"];  // = sandbox
# else
[[Boxcar sharedInstance] setMode:@"production"];
# endif
~~~

##### (Optional) Enable the use of vendorIdentifier or advertiserIdentifier

Device identifier can be use to link a device to you own database. On iOS you have two types of identifier available as default.
VendorIdenfier can be used, especially if you have several apps available and would like to offer synergy for the user between all your applications.
AdvertisingIdentifier can be use as the identifier only if your application is displaying ads. You will have a link your iOS application against Apple AdSupport.framework if you want to use it.

The use of that identifier is disabled as a default. If you want to associate that vendorIdentifier on device registration on the server, you have to enable it explicitly:

~~~ objectivec
[[Boxcar sharedInstance] useVendorIdentifier:YES];
~~~

Here, is the alternative line to enable use of advertisingIdentifier: 

~~~ objectivec
[[Boxcar sharedInstance] useAdvertisingIdentifier:YES];
~~~

With that identifier you can know that the same device is used by several of your applications and can use that information for marketing purpose.

Note that alternativle you can also use any custom identifier you wish:

~~~ objectivec
[[Boxcar sharedInstance] setIdentifier:myNSStringIdentifier];
~~~

##### Perform launch step house keeping tasks

When launching the app, you have to:

- Extract notification from AppDelegate launchOptions to see if your application was started by opening a push notifcation:

~~~ objectivec
NSDictionary *remoteNotif =
    [[Boxcar sharedInstance]
        extractRemoteNotificationFromLaunchOptions:launchOptions];
~~~

- Make sure you gather accurate statistics on opened notifications on the server, by calling the trackNotification: method:

~~~ objectivec
[[Boxcar sharedInstance] trackNotification:remoteNotif];
~~~

- Clean and reset badge and notification center on appliation launch:

~~~ objectivec
[[Boxcar sharedInstance] cleanNotificationsAndBadge];
~~~

- If there is actually a notification, you might want to pass it to a central method in your code that will process it:

~~~ objectivec
if (remoteNotif) {
    [self myProcessNotification:remoteNotif];
}
~~~

#### AppDelegate: - (void)applicationWillEnterForeground:(UIApplication *)application *

##### House keeping

It is usually a good idea to clean / reset notification and badge here:

~~~ objectivec
[[Boxcar sharedInstance] cleanNotificationsAndBadge];
~~~

#### AppDelegate: - (void)applicationDidBecomeActive:(UIApplication *)application *

##### Restart paused process

You need to restart any tasks that were paused (or not yet started) while the application was inactive:

~~~ objectivec
[[Boxcar sharedInstance] applicationDidBecomeActive];
~~~

#### AppDelegate: - (void)application:(UIApplication *)application didRegisterForRemoteNotificationsWithDeviceToken:(NSData *)deviceToken

This method is called when the device successfully registered on the Apple Push Service. You have to pass that information to Boxcar SDK with the following command:

~~~ objectivec
[[Boxcar sharedInstance]
    didRegisterForRemoteNotificationsWithDeviceToken:deviceToken];
~~~

#### AppDelegate: - (void)application:(UIApplication *)app didFailToRegisterForRemoteNotificationsWithError:(NSError *)error

This method is called when the application could not get a push token from Apple. You need to pass that info as well to Boxcar SDK:

~~~ objectivec
[[Boxcar sharedInstance]
    didFailToRegisterForRemoteNotificationsWithError:error];
~~~

#### AppDelegate: - (void)application:(UIApplication *)app didReceiveRemoteNotification:(NSDictionary *)remoteNotif

This is the second way an application can receive a notification.

##### House keeping
You have to do some house keeping as well:

- Generate proper server-side open rate statistics:

~~~ objectivec
[[Boxcar sharedInstance] trackNotification:remoteNotif forApplication:app];
~~~

- Reset the badge and notification list in notification center:

~~~ objectivec
[[Boxcar sharedInstance] cleanNotificationsAndBadge];
~~~

You can choose to do all your house keeping in your central push processing method if you prefer. We like to make it explicit in both paths that a notification can take, but you can adapt our approach to your own way.

##### Call your central processing method:

You should call your central processing method:

~~~ objectivec
[self myProcessNotification:remoteNotif];
~~~

##### AppDelegate: Custom processing method

As a notification can take two different paths in the appDelegate code, it is a good idea to process it in a common method. Here is an example of what you could do with the notification dictionary in your code. This is a good example on how to manipulate and extract content from the notification.

~~~ objectivec
- (void)myProcessNotification:(NSDictionary *)remoteNotif {
    // aps is second level structure
    NSDictionary *APS = [remoteNotif objectForKey:@"aps"];
    [self myNotifLog:APS forKey:@"alert"];
    [self myNotifLog:APS forKey:@"sound"];

    // Custom fields
    // For example the id can be used to open the proper place
    // inside the application
    [self myNotifLog:remoteNotif forKey:@"mymetadata"];

    // Example on how to process notification somewhere else
    [[NSNotificationCenter defaultCenter]
        postNotificationName:@"pushNotification"
                      object:nil
                    userInfo:remoteNotif];
}
~~~

See Push Application demo for details.

#### Other needed calls

To support Apple Push Notifications you need a few more Boxcar SDK method call to place in your application.

##### setAlias

Aliases are used in Boxcar Push Service to notify the users without knowing their device tokens [^1]. To target a given user from the Boxcar Push Console, you need to use an alternative id that is known by developer backend system. This is the alias.

You can set the alias with the following command:

~~~ objectivec
[[Boxcar sharedInstance] setAlias:aliasString];
~~~

This method call has to be placed in a code path where you know that the alias has been certified in a given way. If you use a userid, email, login, it has to have been validated as it is considered trusted by Boxcar Push Service[^2]. If you cannot trust that ID in your application, then consider using advertisingIdentifier.

##### advertisingIdentifier

If you have no trusted identifier to use, you can alternatively use AdvertiserIdentifier. This is a random id that you can pass to your backend, that will allow you to target a given user without having to manage device tokens.

If you enabled use of AdvertisingIdentifier with the following call:

~~~ objectivec
[[Boxcar sharedInstance] useAdvertisingIdentifier:YES];
~~~

you can retrieve it when you need it with the following method:

~~~ objectivec
[[Boxcar sharedInstance] advertisingIdentifier];
~~~

##### sendDeviceParameters method

To make sure we do not make a request on every change of parameters, you have to explicitely tell the framework that you are done changing parameters (like *setAlias* or *setTags*, for example).
So, after change to parameters, do not forget to call *sendDeviceParameters* when you are done with all the changes:

~~~ objectivec
[[Boxcar sharedInstance] sendDeviceParameters];
~~~

It means you set up all your parameters and are ready to send the data to the server. You can call it during app startup or in another place (for example in your settings controller) as update is only performed, if data have changed since the last update.

##### registerDevice method

When you have everything in place and think it is a good moment to enable push. Here is the method call:

~~~ objectivec
[[Boxcar sharedInstance] registerDevice];
~~~

You need to call it only once when you think the user is ready to enable the push, understand the benefit and is likely to accept it when iOS will ask for authorization. It thus thus not a good idea to ask right on application launch. Do not forget that if user reject the push request, changing his mind can be quite complex: It will have to go into iOS notification settings directly and will manually configure the notifications for the application.

Note that the SDK takes care of refreshing / updating the tokens and the server automatically for you.

##### (optional) retrieveProjectTags

When you want to set up the channels that are available for subscription for a given project, you can call the method retrieveProjectTags.

~~~ objectivec
[[Boxcar sharedInstance] retrieveProjectTags];
~~~

It returns an NSArray of NSString objects, containing all the available tag names (all lowercase).
If you want to present those tags in a multilingual interface, we expect that you will pass those tags name as key for your application translation file.

Note: you can embed the list of tags directly on the device. Retrieving the list of project tags is optional, as tags will even be created on the fly if they do not exist yet on your project.

##### setTags:error:

If you want to update your tag subscriptions, you can call the setTags method with an NSArray of NSString.

Tags are expected to contains only alphabetical characters and numbers. The method returns NO and an error if the passed NSArray is incorrect.

##### Clean Badge and notification methods

You have three clean methods available:

* \- (void) cleanNotifications;
* \- (void) cleanNotificationsAndBadge;
* \- (void) cleanBadge;

When the badge are cleaned, the value is also reset to 0 on the Boxcar Push Service as well.

##### unregister

In case you want to completely and permanently stop using the push on that device, you can call the unregister method:

~~~ objectivec
[[Boxcar sharedInstance] unregisterDevice];
~~~

Note: This is not to use to temporarily disable push. Device unregistration destroy all reference to the device on Boxcar Push Service.

## Advanced topics

### InApp Push Notification

Boxcar Push Service implements a way to receive realtime notification from the server while the application is running or while it is still in the 10 minutes allowed for staying in background.

This allows to support a wider range of use case where Apple Push Notification only are not adequate.

For example, you can:

- Send much frequent notification through that mode (for realtime geolocation for example) while the application is running.
- You can send events to device that are triggered by server calculation, while the user is using the application (Sending badges for games for example).
- You can implement board games.
- And many more use cases …

Received events are simple NSString that can contains any type of JSON data structure.

#### BoxcarDelegate

To receive the events you need to implement BoxcarDelegate protocol with on method:

~~~ objectivec
- (void)didReceiveEvent:(NSString *)event;
~~~

Then, all you need is to call the two following method:

~~~ objectivec
[[Boxcar sharedInstance] setDelegate:self];
[[Boxcar sharedInstance] connectToEventStreamWithId:[self makeUniqueString]
                                              error:nil];
~~~

The Id is any "alias", uniqueID, etc known by your server. If this is not an unguessable value, then you have to validate that value in some way in your mobile application, before passing it to the server.

You are done and you can start receiving inApp events send by the server, while the application is being using or running.

## Boxcar Push iOS Demo

You can learn how to integrate the Boxcar iOS SDK by studying the boxcar-ios-demo XCode project is an example on how to
integrate with Boxcar Push Service. It put the step by step element into practice.

The XCode project for this demo application is provided with the SDK, on [Boxcar SDK download page](https://console.boxcar.io/downloads).







[^1]:	Device tokens are stored on Boxcar Push Service and are not expected to be used by developer to send push notifications throught our service.

[^2]:	If the alias you pass to Boxcar Push Service has not been validated by your app and cannot be trusted, consider the risk of hijack of notifications by someone using an illegitimate alias.

[1]:	mailto:support@boxcar.uservoice.com
[2]:	http://developer.apple.com/library/ios/%23documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/ProvisioningDevelopment/ProvisioningDevelopment.html

[image-1]:	/images/ios/xcode1_drag_boxcar_framework.png
[image-3]:	/images/ios/xcode3_infoplist_uibackgroundmodes.png

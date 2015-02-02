---
title: Android SDK Introduction | Boxcar Push Service
version: 2.0.2
mavenRepo: http://developer.boxcar.io/maven
---

# Android SDK v2 introduction

Latest SDK Version: <%= @item[:version] %>

* TOC
{:toc}

## Pre-requisites

### For Android Devices
Before using this SDK you will need to:

1. [Create a Google API project][2]
2. [Enable the GCM Service][3]
3. [Obtain an API key from Google API][4]
4. [Create an Android client](/console/project_settings/) on your Boxcar Console project, using the API key obtained on the previous step.

After you have completed those steps, you should have the following elements:

* Google _Sender ID_ for your application. This is a mandatory parameter for Google GCM based applications. It comes from Google and is actually the project numeric id. In the [Google API Console][5], look at the URL of your project:

		https://code.google.com/apis/console/#project:xxxxxxxxxxx
		
	_xxxxxxxxx_ is the sender ID.
* Google _API Key_
* Boxcar _ClientKey_ and _ClientSecret_: This is two strings embedded in your client application. They allow your application to register a device, notify the server about opened notifications. They are used to sign your request and "authenticate" your application.
* Boxcar _APIURL_: This is the endpoint to use for your organisation for all API calls.

#### Supported devices

All Android devices running Android 2.3 Gingerbread (API 9) or higher.

### For Nokia X Devices
1. For Nokia devices you first need a Nokia Developer Account. Once logged in, you should access [Nokia Services page][6]. After possible queries, you will see My NNA 2.0 services tab on the developer console main page. Enter the desired service identification name to the Sender ID text box and click Create.
2. [Create a Nokia client](/console/project_settings/) on your Boxcar Console project, using the key obtained on the previous step.

After you have completed those steps, you should have the following elements:

* Nokia _Sender ID_ for your application. The desired service identification name choosen on step 1.
* Nokia _API Key_
* Boxcar _ClientKey_ and _ClientSecret_: same as Android applications explained above.
* Boxcar _APIURL_: same as Android applications explained above.

### For Kindle Devices
Steps are similar to the cases above. You need to create an account at Amazon and [follow the instructions on their site][7]. Then you will need to set the _Client ID_ and _Client Secret_ to a Kindle client on your Boxcar Console project.

After you have completed those steps, you should have the following elements:

* Amazon _OAuth Client ID_ and _OAuth Client Secret_
* Boxcar _ClientKey_ and _ClientSecret_: same as Android applications explained above.
* Boxcar _APIURL_: same as Android applications explained above.

#### Supported devices

Kindle Fire devices starting from 2nd generation, running Android 4.0.3 Ice Cream Sandwich (API 15) or higher.

### To build the push client demos
* [Gradle 1.11][8]
* Java SDK 1.6 or greater (1.7 recommended)

In case of trouble, do not hesitate to contact ProcessOne support for Boxcar Push Service by sending a mail to [support@boxcar.uservoice.com][1].

## Prepare your project to use the Boxcar Android SDK

Android SDK library is hosted in our Boxcar Maven Repository.

If you manage your project with [Gradle][8] (recommended), just add the following line within your _repositories_ closure:

~~~ bash
maven { url '<%= @item[:mavenRepo] %>' }
~~~

and add the SDK as a project dependency within the _dependencies_ closure: 

~~~ xml
compile "io.boxcar.universalpush:android-push-sdk:<%= @item[:version] %>"
~~~

There are two available flavours of the SDK library. One is for debugging, the other is for production. They behave indentically, except that the development version outputs debug information into the android log system.

* Debug version: _io.boxcar.universalpush:android-push-sdk-debug:<%= @item[:version] %>_
* Production version: _io.boxcar.universalpush:android-push-sdk:<%= @item[:version] %>_

The following is an example _build.gradle_ descriptor file taken from the demo application:

~~~ java
import org.apache.tools.ant.filters.ReplaceTokens

buildscript {
    repositories {
	    mavenCentral()
    }
    dependencies {
        classpath 'com.android.tools.build:gradle:0.12.2'
    }
}

repositories {
    mavenCentral()
    mavenLocal()
	maven { url '<%= @item[:mavenRepo] %>' }
}

apply plugin: 'com.android.application'

dependencies {
    compile "io.boxcar.universalpush:android-push-sdk-debug:<%= @item[:version] %>"
}

android {
    compileSdkVersion 20
    buildToolsVersion "19.1.0"
    defaultConfig {
        minSdkVersion 9
        versionCode 200
        versionName "${demoVersion}"
        applicationId "io.boxcar.push.demo"
    }
    packagingOptions {
        exclude 'META-INF/DEPENDENCIES'
        exclude 'META-INF/NOTICE'
        exclude 'META-INF/LICENSE'
        exclude 'META-INF/LICENSE.txt'
        exclude 'META-INF/NOTICE.txt'
    }
    productFlavors {
        google {
             minSdkVersion 9
             targetSdkVersion 19
        }
        kindle {
            minSdkVersion 15
            targetSdkVersion 15
        }
        nokia {
            minSdkVersion 16
            targetSdkVersion 16
        }
    }
}
~~~

We recommend to check the demo application already packaged as a gradle project to see a working example.

### Resources hosted in our Maven repository

#### SDK

~~~ xml
group: 'io.boxcar.universalpush', name: 'android-push-sdk', version: '<%= @item[:version] %>'
group: 'io.boxcar.universalpush', name: 'android-push-sdk-debug', version: '<%= @item[:version] %>'
~~~

#### Javadoc

~~~ xml
group: 'io.boxcar.universalpush', name: 'android-push-sdk-debug',
     version: '<%= @item[:version] %>', classifier: 'javadoc'
~~~
	
#### Demo project for gradle

~~~ xml
group: 'io.boxcar.universalpush', name: 'android-push-demo',
     version: '<%= @item[:version] %>', classifier: 'gradle-project'
~~~

## Creating a new push client from scratch with core API

### Framework overview

The Boxcar Push library includes basic objects to build clients quickly. Among important classes you have:

* **Boxcar**: This is the main entry point to interact with Universal Push Notification Platform. It offers methods to start the framework, register and unregister from the remote service; retrieve push tags and track notifications. Boxcar also uses a persistent cache based on SQLite database to store incoming notifications and track its internal state (unread / read).
* **BXCConfig**: this is the configuration class to tweak the SDK. For example, the way notifications should be displayed and how they should refer to your application.
* **BXCNotification**: represents an incoming notification from Universal Push Notification Platform.
* <a name="events"></a>**io.boxcar.push.eventbus.event.\***: all the different events this library could trigger, replacing the methods from the old *BXCCallback* mechanism:
    * *RegistrationSuccessEvent*
    * *BadgeResetSuccessEvent*
    * *GetTagsSuccessEvent*
    * *PingSuccessEvent*
    * *UnregisterSuccessEvent*
    * *NotificationReceivedEvent*
    * *TrackNotificationSuccessEvent*
    * *BadgeResetFailedEvent*
    * *GetTagsFailedEvent*
    * *PingFailedEvent*
    * *UnregisterFailedEvent*
    * *TrackNotificationFailedEvent*
    * *RegistrationFailedEvent*

**Note**: some event classes, like *io.boxcar.push.eventbus.event.RegistrationSuccessEvent* do also have specific attributes with extended information. For example, *RegistrationSuccessEvent* contains the list of tags that were 'rejected' because these were marked as deprecated in the server.

### <a name="configure"></a>Configure and start the Boxcar framework

**Boxcar Push Library assumes your application does initialize the framework every time it is started.** This means your application should override the method *Application#onCreate()* and initialize it there. If you do not initialize the framework at this stage you could have errors while dealing with incoming notifications from GCM (or any other provider). The first step your O.S. accomplishes when a notification has arrived for your application is to start it (if it wasn't running), that's why the framework should be initialized at that stage and not later:

~~~ java
@Override
public void onCreate() {
    super.onCreate();
    startBXCPushFramework();
}
~~~

The following is the implementation of *startBXCPushFramework()* taken from the demo app:

~~~ java
private void startBXCPushFramework() {

    Boxcar.init(this, new BoxcarDemoDelegate(this));

    PushSupportStatus status = Boxcar.getPushSupportStatus(this);
    if (!status.equals(PushSupportStatus.supported)) {
        Log.e(TAG, "Push not supported. Reason: " + status);
        Toast.makeText(getApplicationContext(), "Push not supported",
            Toast.LENGTH_LONG).show();
    }
}
~~~

In brief, we do a single important operation on application startup:

1. Initialize the framework, passing your application delegate object

~~~ java
Boxcar.init(this, new BoxcarDemoDelegate(this));
~~~

The second parameter is a subclass from *BoxcarAppDelegate*. **You should implement a custom application delegate class previously**. In the demo application *BoxcarDemoDelegate* has this goal. It is a class extending *io.boxcar.push.BoxcarAppDelegate* and it is used by the framework any time it needs your specific configuration to initialize the SDK. See *io.boxcar.push.demo.BoxcarDemoDelegate* in the demo application for a very simple example.

#### Registering
To register on Universal Push Notification Platform we need:

##### For Android Devices:
A target device with Android API Level 9 or greater. This is required to support the new GCM client provided by *Google Play Library*.

##### For Kindle Devices:
A target device with Android API Level 15 or greater. This is required to support ADM mechanism.

##### For Nokia X Devices:
A target device with Android API Level 16 or greater. This is required to support Nokia Notification API (NNA) mechanism.

##### Parameters

To register devices against your Universal Push Notification Platform account, you need to set the credentials associated with your client as it was defined on your project.

**Note**: on the demo application the values are defined on *res/values/config.xml*. There is a common file on *src/main/res/values/config.xml* which is dynamically merged by Gradle with each variant during build time. Variants are found on *src/google/res/values/config.xml*, *src/kindle/res/values/config.xml* and *src/nokia/res/values/config.xml*. 

For example, if the project you own within the Universal Push Notification Platform is called "Foobar" and you have created two clients for it, namely "Foobar for Android" and "Foobar for Kindle", then you will have two sets of credentials, one for the Android version and another set for the Kindle flavor. Refer to [pre-requisites section](#pre-requisites) for more details. 

* *Key* and *Secret* for the corresponding Universal Push Notification Platform client application.
* Google or Nokia *senderId*. Not used on Amazon ADM based applications.
* URL of the Universal Push Notification Platform.
    * *pushScheme*: https
    * *pushHost*: console.boxcar.io
    * *pushPort*: 443

Connection details like *clientKey*, *clientSecret*, *pushScheme*, *pushHost*, *pushPort* and *senderId* are set on the configuration class *io.boxcar.push.BXCConfig*.

#### Notifications

You should also set an algorithm to handle incoming notifications. It must implement the interface defined by *io.boxcar.push.ui.BXCNotificationStrategy*. Currently the framework offers four different approaches:

* *io.boxcar.push.ui.AutomaticUINotificationStrategy*
* *io.boxcar.push.ui.MultipleUINotificationStrategy*
* *io.boxcar.push.ui.ExtendedUINotificationStrategy*
* *io.boxcar.push.ui.DummyNotificationStrategy*

with the exception of *DummyNotificationStrategy*, all of them extend from io.boxcar.push.ui.BaseUINotificationStrategy, which implements *io.boxcar.push.ui.BXCNotificationStrategy* but also offers some base methods that allow to customize some options.

##### <a name="automaticUI"></a>AutomaticUINotificationStrategy

Renders notifications on the Android Notification Center and updates it if there were pending notifications on it. In other words, it groups notifications and keeps a pending intent for the last event received. To extract the notification from the intent triggered after the user tapped on it, you should get the "notification" argument. Example:

~~~ java
protected void onNewIntent(Intent intent) {
    super.onNewIntent(intent);
    BXCNotification notification = intent.getParcelableExtra("notification");
    if (notification != null) {
    // handle notification
    }
}
~~~

##### MultipleUINotificationStrategy

Renders a new notification on the Android Notification Center and a new notification on the Notification Area. Each notification is unique and keeps the alert information specific to it. So, if application receives three alerts, it will render three different notification icons on the notification area. The way to handle intents generated by notifications built by this strategy is the same than [*AutomaticUINotificationStrategy*](#automaticUI).

##### ExtendedUINotificationStrategy

Renders a single notification showing the last three received alerts since the last time *Boxcar#cleanNotificationsAndBadge(Context context)* was called. The action to be performed when user taps on it depends on the amount of pending notifications and type. Strategy constructor allows up to three Activities to be opened, depending on the case type, which could be:

1. there is a *single* unread *plain notification*
2. there is a *single* unread *rich notification*
3. there are two or more unread notifications

###### ExtendedUINotificationStrategy instantiation example

The following example shows a possible instantiation using three different activities:

1. NormalNotificationActivity (manages a single plain notification)
2. WebViewActivitiy (manages a single rich notification)
3. InboxActivity (manages a collection of notifications, either plain or rich)

~~~ java
BXCNotificationStrategy notificationStrategy =
    new ExtendedUINotificationStrategy(icon, title, NormalNotificationActivity.class,
        WebViewActivity.class, InboxActivity.class);
~~~

For the first and second case (single notification), the BXCNotification instance representing the incoming alert
should be extracted from the notification intent within the extra argument *"notification"*. The following example
shows how to handle an incoming intent from a single rich push:

~~~ java
private void handleIntent(Intent intent) {
    String url = intent.getStringExtra("url");
    BXCNotification notification = intent.getParcelableExtra("notification");
    // ... do your processing here
}
~~~

The following example shows how to handle incoming intents containing several unread notifications. Please note that
we now extract a list of incoming notifications from the extra argument *"notifications"*:

~~~ java
private void handleIntent(Intent intent) {
    String url = intent.getStringExtra("url");
    List<BXCNotification> incomingNotifications = intent.getParcelableArrayListExtra("notifications");
    // ... do your processing here
}
~~~

##### DummyNotificationStrategy
This is a direct implementation of the *io.boxcar.push.ui.BXCNotificationStrategy* interface which does nothing when a notification is received.

You should take this class into account if you want to take complete control of the notification process. If you set an instance of this class as the notification strategy and you also listen to notification events through *BXCCallback#notificationReceived(BXCNotification notification)* you will have complete control about what to do on your application every time a push is received. 

The dummy notifcation strategy should be instantiated with the empty constructor:

~~~ java
BXCNotificationStrategy notificationStrategy = new DummyNotificationStrategy();
~~~

**Note**: In the demo applicaton all parameters are externalized into *src/${variant}/res/values/config.xml*. Where the common variant is *main* which is merged during build time with each variant (*google*, *kindle*, *nokia*).

##### Custom notification strategies
You can also define your own strategy implementing the interface *io.boxcar.push.ui.BXCNotificationStrategy*. There is also a base class with template methods based on the previous interface.

The most important thing to remember is that every time a new push arrives to the device, the SDK will call the following method: 

~~~ java
public void handleNotification(Context context, BXCNotification notification,
                               BoxcarAppDelegate delegate) {
	// your custom implementation goes here
}
~~~

Below is a source example of the *MultipleUINotificationStrategy* implementation, which extends from the aforementioned base class *BaseUINotificationStrategy*. It is a good starting point to implement your own if the provided strategies don't fit your usage scenario.
 
~~~ java
package io.boxcar.push.ui;
 
import io.boxcar.push.BoxcarAppDelegate;
import io.boxcar.push.model.BXCNotification;
import android.app.Activity;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.net.Uri;
import android.support.v4.app.NotificationCompat;
import android.util.Log;
 
/**
 * Renders one new icon in the notification area for each incoming push.
 * A different notification will be shown in the notification center too.
 * Intent targets are different depending on whether this is a simple
 * or a rich push (containing an URL).
 *  
 * @author jpcarlino
 *
 */
public class MultipleUINotificationStrategy extends BaseUINotificationStrategy {
 
    private Class<? extends Activity> normalPushActivity;
    private Class<? extends Activity> richPushActivity;
   
    /**
     * Default constructor.
     *
     * @param icon
     *            the icon (resource id) to use on the tray
     * @param title
     *            the title of the notification items
     * @param normalPushActivity
     *            the target activity to be invoked when a normal push is
     *            clicked on the notification tray
     * @param richPushActivity
     *            the target activity to be invoked when a rich push is received
     */
    public MultipleUINotificationStrategy(int icon, String title,
                    Class<? extends Activity> normalPushActivity,
                    Class<? extends Activity> richPushActivity) {
        this(icon, null, title, true, normalPushActivity, richPushActivity);
    }
 
    public MultipleUINotificationStrategy(int icon, Bitmap largeIcon,
                    String title, boolean showBadge,
                    Class<? extends Activity> normalPushActivity,
                    Class<? extends Activity> richPushActivity) {
        this.icon = icon;
        this.largeIcon = largeIcon;
        this.title = title;
        this.showBadge = showBadge;
        this.normalPushActivity = normalPushActivity;
        this.richPushActivity = richPushActivity;
    }
   
    @Override
    public void handleNotification(Context context, BXCNotification notification,
                                   BoxcarAppDelegate delegate) {
        generateNotification(context, notification, delegate);
    }
 
    /**
     * Issues a notification to inform the user that server has sent a message.
     */
    protected void generateNotification(Context context,
                    BXCNotification notification, BoxcarAppDelegate delegate) {
 
        Intent notificationIntent;
        String url = extractURL(context, notification);        
        if (url != null) {
            Log.v(TAG, "Rendering webview link for URL: " + url);
            notificationIntent = new Intent(context, richPushActivity);
            notificationIntent.putExtra("url", url);
        } else {
            Log.v(TAG, "Rendering normal notification without URL");
            notificationIntent = new Intent(context, normalPushActivity);
        }
 
        notificationIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP
                        | Intent.FLAG_ACTIVITY_SINGLE_TOP);
        notificationIntent.putExtra("notification", notification);
        notificationIntent.setData((Uri.parse("custom://"
                        + System.currentTimeMillis())));
        notificationIntent.setAction("actionstring"
                        + System.currentTimeMillis());
 
        int notificationId = getNotificationIdFor(context, notification,
                                                  delegate);
       
        PendingIntent intent = PendingIntent.getActivity(context,
                        notificationId,
                        notificationIntent, 0);
        long when = System.currentTimeMillis();
        
        // The following statement calls a hook method defined in
        // BaseUINotificationStrategy that instantiates a notification
        // builder based on the attributes set above.
        NotificationCompat.Builder notifyBuilder = prepareNotificationBuilder(
                        context, notification, when, intent, delegate);
 
        NotificationManager notificationManager = (NotificationManager) context
                        .getSystemService(Context.NOTIFICATION_SERVICE);
        notificationManager.notify(notificationId,
                        notifyBuilder.build());
    }
   
}
~~~


#### Configuring the sound to play on incoming notifications

When a push arrives, the SDK checks the payload content to know if it should play a sound when the notification is sent to the Android Notification center.
The following rule is followed by the SDK:

* if the payload doesn't define a sound file to play, it doesn't play any sound (silent push)
* if the payload defines a sound file that can't be found on the application, it plays the default system sound
* otherwise, it plays the sound defined in the payload
 
As can be seen, the push data can contain a reference to a sound file to play. The idea is that you application can have multiple sounds, one for urgent notifications, another for normal content, etc. In that case, the person that publishes the push just tells what is the sound that should be played when the message arrives at the user's device.

Every custom sound file needed (any other than the system's default) should be packaged along with the mobile application. You should simply put the sound file under res/raw folder of your project. If an incoming push tells to play 'beep' sound, then the SDK will locate a sound resource with that name on res/raw folder (for example 'res/raw/beep.wav') and play it. If the file can't be located, the system's default is played instead.

If you want a different default sound (something else than system's default) you can set it too. The method to define a default sound is the following:

~~~ java
BaseUINotificationStrategy.setDefaultSound(Context context, int resourceId);
~~~

For example:

~~~ java
notificationStrategy.setDefaultSound(this, R.raw.beep);
~~~

#### Configuring notification actions
The Boxcar Universal Push Notification Platform allows to define *categories* for the pushes to send. This is an optional attribute you can set on the push which helps the SDK to distinguish between different kind of notifications.

You can for example configure you application to add one or more buttons (actions) to the notification widget when the category is *'sports_schedule'*.

Let's say you want to add the button 'Favorite' for pushes belonging to that category. In that case you should create a mapping between the *sports_schedule* category and the action(s) you want for that notification.

Override the method *getNotificationCategoryMapping(String category, Context context)* on your BoxcarAppDelegate subclass:

~~~ java
@Override
public NotificationCategoryMapping getNotificationCategoryMapping(String category, Context context) {
    NotificationCategoryMapping mapping = null;
    if (category != null && category.equals("sports_schedule")) {
        mapping = new NotificationCategoryMapping("sports_schedule");

        mapping.addActionBuilder(new BaseActionBuilder() {

            @Override
            public int getActionIcon(Context context, BXCNotification notification) {
                return R.drawable.ic_menu_add_star_holo_dark;
            }

            @Override
            public String getActionTitle(Context context, BXCNotification notification) {
                return context.getString(R.string.fav_notif_action);
            }

            @Override
            public PendingIntent getPendingIntent(Context context,
                    BXCNotification notification) {

                Intent intent = new Intent(NotificationActionIntentService.FAV_ACTION);
                intent.setClass(context, NotificationActionIntentService.class);
                intent.putExtra(NotificationActionIntentService.ACTION_NOTIFICATION_EXTRA, notification);

                return	PendingIntent.getService(context, -1, intent,
                        PendingIntent.FLAG_CANCEL_CURRENT);
            }
        });
    }

    return mapping;
}
~~~

The example above basically creates a mapping with an action builder that will be called when a notification arrives and matches that category. Note in this example we just add a single action ("Favorite"), but you can add multiple, just calling multiple times the method *NotificationCategoryMapping#addActionBuilder(ActionBuilder builder)*. You can also have multiple mappings (different set of actions for different categories).

This will result in a notification in the *Notification Center* with a "Favorite" button. You should also define 'what to do' when the user presses that button. In the example above we just send an intent to an *IntentService* that should perform a background task. You could have set it to open a new Activity if there is no background processing to do.

**Note**: Action buttons won't appear on platforms prior to API 16 (Android 4.1).

#### Showing a big picture when the notification is expanded
The Boxcar Universal Push Notification Platform allows to define an *image URL* for a given push notification. This is an optional attribute you can set on the push which tells the SDK to fetch an image and set it as a big background picture when the notification is expanded.

For example, you can send a push about a brand new product released by your company and set a nice picture of it to be displayed on each device.

When the push arrives, the device establishes an HTTP connection, reads the image from the remote site and sets it as the background picture for the notification widget. Source picture should be ≤ *450 DP wide*, *~2:1 aspect*.

Here are two examples. A big picture notification displayed in a handheld phone device and the same notification displayed as a card in an Android watch:
 
![][image-1] ![][image-2]

Read more about how to specify pictures on the [Publisher API Introduction](/api/publisher).

**Note**: This feature is available only on Android devices with API level >= 16. 

## Boxcar Framework Demo for Android

The Boxcar Push Client demo project is an example on how to integrate with Boxcar Push Service on Android devices based on Google Cloud Messaging (GCM).

### Boxcar SDK integration

#### Setup your project

* **Step 1**: Create a new Android project. See ["Prepare your project to use the Boxcar Android SDK"](#prepare-your-project-to-use-the-boxcar-android-sdk) section for more information.
* **Step 2**: Add GCM permissions to your Android Manifest file. Permissions are specified in XML as children of &lt;manifest&gt; element. In this example our application package (as defined on 'package' attribte of &lt;manifest&gt; element) is *"io.boxcar.push.demo"*:

~~~ xml
<uses-permission android:name="com.google.android.c2dm.permission.RECEIVE" />
<permission android:name="io.boxcar.push.demo.permission.C2D_MESSAGE" android:protectionLevel="signature" />
~~~

* **Step 3**: Add the Google Play services version to your manifest as as children of &lt;application&gt; element.

~~~ xml
<meta-data android:name="com.google.android.gms.version" android:value="@integer/google_play_services_version"/>
~~~

* **Step 4**: Register the GCM service on your Android manifest file. Services are specified in XML as children of &lt;application&gt; element.

~~~ xml
<service android:name="io.boxcar.push.GooglePlayGCMIntentService" android:enabled="true"/>
~~~

* **Step 5**: Configure the corresponding receiver for GCM incoming notifications. Receivers are specified in XML as children of &lt;application&gt; element.

~~~ xml
<receiver android:name="io.boxcar.push.GooglePlayGCMReceiver" android:enabled="true" android:permission="com.google.android.c2dm.permission.SEND">
    <intent-filter>
        <!-- Receives the actual messages. -->
        <action android:name="com.google.android.c2dm.intent.RECEIVE"/>
        <category android:name="io.boxcar.push.demo"/>
    </intent-filter>
</receiver>
~~~

* **Step 6**: If you expect to use UDID (Universal Device ID), register the service on your Android manifest file. Services are specified in XML as children of &lt;application&gt; element:

~~~ xml
<service android:name="org.openudid.android.OpenUDIDService">
    <intent-filter>
        <action android:name="org.openudid.android.GETUDID" />
    </intent-filter>
</service>
~~~

* **Step 7**: (Optional) Add GET_ACCOUNTS permissions to your Android Manifest file. This permission allows to inspect if this device has a Google / Amazon account associated with it. This allows a finer grain check to know if push is supported or not. This permission is optional. If not set, Boxcar Android Push SDK will skip this specific check. Permissions are specified in XML as children of &lt;manifest&gt; element.

~~~ xml
<uses-permission android:name="android.permission.GET_ACCOUNTS" />
~~~

**Note**: you can see a working example of the *AndroidManifest.xml* file within the push demos released along with the core SDK library. There is currently a common manifest file for all demos on *src/main/AndroidManifest.xml* which is then merged by the *Gradle* build manager with the platform specific parts. For instance, the specific elements for Google Android are located at *src/google/AndroidManifest.xml*.

#### Source code

### Usage

Usage is shown in *io.boxcar.push.demo.DemoApplication* and *io.boxcar.push.demo.NormalNotificationActivity*.

*Boxcar* is the only point of interaction, aside from the callbacks to handle incoming notifications.

* When your application starts you would normally initialize the framework pasing the delegate instance, as explained on subsection ["Configure and start the Boxcar framework"](#configure).

After that point, you should subscribe on each application component (typically Activities) to receive the events produced by the Push library:

~~~ java
Boxcar.registerSubscriber(Object subscriber);
~~~

Push library relies on [EventBus](https://github.com/greenrobot/EventBus) to loosely couple your artifacts with the library. Your artifacts do not need to listen to all events, just those it is really interested in. Because you would normally expect to update the UI upon the reception of a push or any other event, you will define one method named 'onEventMainThread' for each different event type. Example:

~~~ java
public void onEventMainThread(RegistrationSuccessEvent event) {
    renderLabels();
}
~~~

this method will be called every time a success registration is performed by the client, as long as your artifact was previously registered through the use of *Boxcar.registerSubscriber(Object subscriber)* method.

**You should always** register your artifact in its *onResume()* method and also unregister it in the *onPause()* method, like this:

~~~ java
protected void onPause() {
    super.onPause();
    Boxcar.unregisterSubscriber(this);
}

protected void onResume() {
    super.onResume();
    Boxcar.registerSubscriber(this);
}
~~~

See the complete [list of supported events](#events).

**Note**: previously, an additional argument, *applicationId*, was required by #start method to identify your application version. This is now handled internally by the framework. However is still important to define it on your Android manifest file.

* Unregister from push services. Please note that after this invocation your application won't receive any new push notification unless a new registration is performed:

~~~ java
Boxcar.unregister(Context context);
~~~

**Note**: this method throws throws *io.boxcar.push.registration.BXCPendingOperationException* if there is a pending registration or unregistration in process. You should always receive a corresponding callback telling when the current operation has finished (either successfully or with error). In other words, do not call this method until your previous call (either 'register' or 'unregister') hasn't received a confirmation via *BXCCallback*.

* Register device. Registers this device on GCM gateway and Universal Push Notification Platform using the provided credentials.

* Register specifying the initial set of tags or channels and a user name:

~~~ java
Boxcar.register(Context context, List<String> tags, String username);
~~~

**Note**: check that your tag list doesn't contain *deprectated* tags, if any. You should check if your push project has tags that were marked as deprecated. If you register to some deprecated tag, you will be informed in the corresponding *RegistrationSuccessEvent* event, through *getDeprecatedTags()* method.

* Register to the channel used for the last time (if any):

~~~ java
Boxcar.register(Context context, String username);
~~~

When registration is success, your application should receive an event with class RegistrationSuccessEvent. That instance contains the list of tags your device is now subscribed to **and also the list of tags you tried to subscribe but are marked as deprecated for the Boxcar project to which your application belongs**. These tags should be cleaned up by your application. In the following example taken from the demo application you can see how to extract the list of accepted and deprecated tags from the registration success event:

~~~ java
public void onEventMainThread(RegistrationSuccessEvent event) {

    String[] deprecatedTags =  event.getDeprecatedTags();
    if (deprecatedTags.length > 0) {
        StringBuffer message = new StringBuffer("Deprecated tags: ");
        for (int i = 0; i < deprecatedTags.length - 1; i++) {
            message.append(deprecatedTags[i]);
            message.append(", ");
        }
        message.append(deprecatedTags[deprecatedTags.length - 1]);
        Log.w(TAG, message.toString());
    }

    renderLabels();
}
~~~


* Register into Universal Push Notification Platform with the tags and username used the last time:

~~~ java
Boxcar.register(Context context);
~~~

**Note**: this method throws *io.boxcar.push.registration.BXCPendingOperationException* if there is a pending registration or unregistration in process. You should always receive a corresponding callback telling when the current operation has finished (either successfully or with error). In other words, do not call this method until your previous call (either 'register' or 'unregister') hasn't received a confirmation via via onEvent*(RegistrationSuccessEvent), onEvent*(RegistrationFailedEvent), onEvent*(UnregisterSuccessEvent event) or onEvent*(UnregisterFailedEvent event).

* Get available tags. This operation can be performed without previous authentication. The result will be informed through onEvent(*GetTagsSuccessEvent event*):

~~~ java
Boxcar.getTags(Context context);
~~~

* Subscribers registration/unregistration:

~~~ java
Boxcar.registerSubscriber(Object subscriber);
Boxcar.unregisterSubscriber(Object subscriber);
~~~

* Track an incoming notification:

~~~ java
Boxcar.trackNotification(Context context, BXCNotification notification,
       BXCTrackNotification.State.active);
~~~

* Track when application was opened or is being used by the final user

~~~ java
Boxcar.appBecameActive(Context context);
~~~

This method is similar to *Boxcar#trackNotification(...)* in the sense it is used for statistical purposes, allowing to know how much your application is being used. You would normally call this method on places like your activity *onResume()* method.

* Get the set of tags to which this instance was registered since the last registration attempt. It won't include tags that were marked as *deprecated* for the current push project:

~~~ java
List<String> getSubscribedTags(Context context)
~~~

* Get current status of Boxcar Framework. Either one of *registered*, *unregistered*, *registering* or *unregistering*:

~~~ java
Boxcar.getState(Context context);
~~~

* Clean Android Notification Center and reset the number of unread notifications on Universal Push Notification Platform:

~~~ java
Boxcar.cleanNotifications(Context context);
Boxcar.cleanBadge(Context context);
~~~

* Check if push is supported on this device. It returns an enumerated type containing the error code or 'supported' when push is natively supported on current device. Push must be natively supported as a pre-requisite of Boxcar Push SDK to work.

~~~ java
PushSupportStatus status = Boxcar.getPushSupportStatus(this);
if (!status.equals(PushSupportStatus.supported))
    Log.e(TAG, "Push not supported. Reason: " + status);
~~~
    
Possible values an its meaning:

    *supported*: native push is supported on this device (GCM on Android, ADM on Amazon, etc).
	*missingNativePushAccount*: this device is not linked to the native provider (Google / Amazon account).
	*missingNativePushDependencies*: current operating system doesn't provide the required dependencies. Usually this is because operating system version is old.
	*missingNativePushManifest*: AndroidManifest doesn't provide all the required permissions and services required to support native push.
        
You are not required to explicitly check push support status. It will be performed automatically on *Boxcar.register(...)* call and and exception thrown if native push is not supported. Remember that if you do not allow the application to access account information (*android.permission.GET_ACCOUNTS*), the SDK won't be able to check if the device is linked to an authorized provider account (Gmail account on standard Android devices, Amazon on Kindle devices).
        
### Troubleshooting

#### Device seems registered but no pushes are received

1. check that your device is on the list of registered devices, of for the corresponding project under Boxcar Push Service administration console.
2. ensure that the *clientKey* and *clientSecret* set on *io.boxcar.push.BXCConfig* are the same as defined on your Universal Push Notification Platform project for the corresponding Android client application.
3. similarly, check that *senderId* (excluding Amazon Kindle apps) value defined on *io.boxcar.push.BXCConfig* is associated with the *API Key* set on Boxcar Push Service for the corresponding Android client application. If your device had a wrong *senderId* previously, please make sure your device is unregistered by calling *Boxcar.unregister()*. This step forces your application to request a new token to GCM/Nokia cloud.

## Boxcar Framework Demo for Amazon Kindle devices

The demo project is an example on how to integrate with Boxcar Push Service on Amazon devices based on Amazon Device Messaging (ADM). It is integrated into the same project structure than the standard Android version (same directory and files) but the *AndroidManifest.xml* file is dynamically generated by Gradle build framework.

### Boxcar SDK integration

#### Setup your project
Before setting up your project, please ensure you have:

1. created an account on Amazon, following the instructions here: [https://developer.amazon.com/sdk/adm/credentials.html](https://developer.amazon.com/sdk/adm/credentials.html)
2. set your API Key. On the demo project the API key has a placeholder value on the file *src/kindle/assets/api_key.txt*. You must set the one generated by you, based on your Amazon account and the signature used to sign your application. All these details are documented on the step 5 of the link above.
3. enabled ADM feature on your Amazon account. Read step 6 on the link above.
4. ensure each Amazon Kindle device is registered into the Amazon network.

* **Step 1**: Create a new Android project. See ["Prepare your project to use the Boxcar Android SDK"](#prepare-your-project-to-use-the-boxcar-android-sdk) section for more information.

* **Step 2**: Add ADM permissions to your Android Manifest file. Permissions are specified in XML as children of &lt;manifest&gt; element. In this example our application package (as defined on 'package' attribte of &lt;manifest&gt; element) is *"io.boxcar.push.demo"*:

~~~ xml
<!-- This permission ensures that no other application can intercept your ADM messages. -->
<permission android:name="io.boxcar.push.demo.permission.RECEIVE_ADM_MESSAGE" android:protectionLevel="signature" />
<uses-permission android:name="io.boxcar.push.demo.permission.RECEIVE_ADM_MESSAGE" />
<!-- This permission allows your app access to receive push notifications from ADM. -->
<uses-permission android:name="com.amazon.device.messaging.permission.RECEIVE" />
<!-- ADM uses WAKE_LOCK to keep the processor from sleeping when a message is received. -->
<uses-permission android:name="android.permission.WAKE_LOCK" />
~~~

* **Step 3**: Register the ADM service on your Android manifest file. Services are specified in XML as children of &lt;application&gt; element.

~~~ xml
<service android:name="io.boxcar.push.ADMIntentService" android:enabled="true" android:exported="false" />
~~~

* **Step 4**: Configure the corresponding received for ADM incoming notifications. Receivers are specified in XML as children of &lt;application&gt; element.

~~~ xml
<!-- This permission ensures that only ADM can send your app registration broadcasts. -->
<receiver android:name="io.boxcar.push.ADMReceiver" android:enabled="true" android:permission="com.amazon.device.messaging.permission.SEND" >
    <!-- To interact with ADM, your app must listen for the following intents. -->
    <intent-filter>
        <action android:name="com.amazon.device.messaging.intent.REGISTRATION" />
        <action android:name="com.amazon.device.messaging.intent.RECEIVE" />
        <!-- You must replace the name in the category tag with your app's package name. -->
        <category android:name="io.boxcar.push.demo" />
    </intent-filter>
</receiver>
~~~

* **Step 5**: If you expect to use UDID (Universal Device ID), register the service on your Android manifest file. Services are specified in XML as children of &lt;application&gt; element:

~~~ xml
<service android:name="org.openudid.android.OpenUDIDService">
    <intent-filter>
        <action android:name="org.openudid.android.GETUDID" />
    </intent-filter>
</service>
~~~

* **Step 6**: You must explicitly enable ADM. To do so, add the following element, specified in XML as children of &lt;application&gt; element.

~~~ xml
<amazon:enable-feature android:name="com.amazon.device.messaging" android:required="true" />
~~~

**Note**: you can see a working example of the *AndroidManifest.xml* file within the push demos released along with the core SDK library. There is currently a common manifest file for all demos on *src/main/AndroidManifest.xml* which is then merged by the *Gradle* build manager with the platform specific parts. For instance, the specific elements for Amazon Kindle are located at *src/kindle/AndroidManifest.xml*.

#### Source code

Source code for the Kindle demo application is the same than the standard Android app explained above.

**Important:*** do not include Amazon ADM SDK library into your APK file. This library is already provided by the Amazon environment already. If you include the library in your packaged application you will experience errors during the registration phase. 

## Boxcar Framework Demo for Nokia NM devices

The Nokia X project is an example on how to integrate with Boxcar Push Service on Nokia NM devices based on Nokia Notification API (NNA).

### Boxcar SDK integration

#### Setup your project
Before setting up your project, please ensure you have:

1. created a Nokia Developer Account to define the senderId for your project and get the API Key generated by Notifications API Developer Console.

2. set your senderId on *src/nokia/res/values/config.xml*.

* **Step 1**: Create a new Android project. See ["Prepare your project to use the Boxcar Android SDK"](#prepare-your-project-to-use-the-boxcar-android-sdk) section for more information.
* **Step 2**: Add Nokia permissions to your Android Manifest file. Permissions are specified in XML as children of &lt;manifest&gt; element. In this example our application package (as defined on 'package' attribte of &lt;manifest&gt; element) is *"io.boxcar.push.demo"*:

~~~ xml
<permission android:name="io.boxcar.push.demo.permission.C2D_MESSAGE" android:protectionLevel="signature" />
<uses-permission android:name="io.boxcar.push.demo.permission.C2D_MESSAGE" />
<!-- This permission allows your app access to receive push notifications from Nokia Notifications. -->
<uses-permission android:name="com.nokia.pushnotifications.permission.RECEIVE" />
<uses-permission android:name="android.permission.WAKE_LOCK" />
<uses-permission android:name="android.permission.INTERNET" />
<uses-permission android:name="android.permission.GET_ACCOUNTS" />
<uses-permission android:name="android.permission.VIBRATE" />
~~~

* **Step 3**: Register the Nokia NNA service on your Android manifest file. Services are specified in XML as children of &lt;application&gt; element.

~~~ xml
<service android:name="io.boxcar.push.NNAIntentService" android:enabled="true" android:exported="false" />
~~~

* **Step 4**: Configure the corresponding received for NNA incoming notifications. Receivers are specified in XML as children of &lt;application&gt; element.

~~~ xml
<receiver android:name="io.boxcar.push.NNAReceiver" android:enabled="true" android:permission="com.nokia.pushnotifications.permission.SEND">
    <intent-filter>
        <!-- Receives the actual messages. -->
        <action android:name="com.nokia.pushnotifications.intent.RECEIVE" />
        <!-- Receives the registration id. -->
        <action android:name="com.nokia.pushnotifications.intent.REGISTRATION" />
        <!-- You must replace the name in the category tag with your app's package name. -->
        <category android:name="io.boxcar.push.demo" />
    </intent-filter>
</receiver>
~~~

* **Step 5**: If you expect to use UDID (Universal Device ID), register the service on your Android manifest file. Services are specified in XML as children of &lt;application&gt; element:

~~~ xml
<service android:name="org.openudid.android.OpenUDIDService">
    <intent-filter>
        <action android:name="org.openudid.android.GETUDID" />
    </intent-filter>
</service>
~~~

**Note**: you can see a working example of the *AndroidManifest.xml* file within the push demos released along with the core SDK library. There is currently a common manifest file for all demos on *src/main/AndroidManifest.xml* which is then merged by the *Gradle* build manager with the platform specific parts. For instance, the specific elements for Nokia X are located at *src/nokia/AndroidManifest.xml*.

#### Source code

Source code for the Nokia demo application is the same than the standard Android app explained above.








[1]:	mailto:support@boxcar.uservoice.com
[2]:	http://developer.android.com/google/gcm/gs.html#create-proj
[3]:	http://developer.android.com/google/gcm/gs.html#gcm-service
[4]:	http://developer.android.com/google/gcm/gs.html#access-key
[5]:	https://console.developers.google.com
[6]:	https://account.nnapi.ovi.com/cm/Web/services_direct.jsp
[7]:	http://docs.aws.amazon.com/sns/latest/dg/mobile-push-adm.html
[8]:	http://www.gradle.org/downloads

[image-1]:	/images/android/phone-device-big-picture.png
[image-2]:	/images/android/wear-device-big-picture.png
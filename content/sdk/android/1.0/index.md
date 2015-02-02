---
title: Android SDK Introduction | Boxcar Push Service
version: 1.1.0.14
---

# Android SDK v1 introduction

Latest SDK Version: <%= @item[:version] %>

* TOC
{:toc}

## Package

The SDK package contains:

* **bxc-android-core.jar**: This is the Boxcar Push Client SDK library that isolates the interaction with the Universal Push Notification Platform and handles all incoming notifications for your application.
* **bxc-android-core-jar-with-dependencies.jar**: This is the same library, but includes required compile dependencies.
* **Boxcar SDK Developer's Guide** document: This is the document you are currently reading.
* **Javadoc documentation**: This is a set of HTML files documenting the objects and method available in the API.
* **bxc-demo-android**: This is a minimal Android example application showing the usage of the core library. It includes the standalone Eclipse project with source code and a precompiled .apk application.
* **bxc-demo-kindle**: This is a minimal Amazon Kindle example application showing the usage of the core library. It's the same application than *bxc-android-demo*, except that it includes a manifest file according to the requirements of Amazon ADM service, and the corresponding notification JAR library provided by Amazon SDK. It also includes the standalone Eclipse project with source code and a precompiled .apk application.
* **bxc-demo-nokia**: This is a minimal Nokia NM example application showing the usage of the core library. It's the same application than *bxc-android-demo*, except that it includes a manifest file according to the requirements of Nokia NNA service, and the corresponding notification JAR library provided by Nokia. It also includes the standalone Eclipse project with source code and a precompiled .apk application.

### Dependencies

In order to integrate, compile and run your application with Boxcar Push Client Framework you will need the following dependencies:

* Boxcar Push Client SDK library for Android v<%= @item[:version] %>
* Android support-v4 v19
    * Artifact available at Maven Central as:

            <groupId>com.google.android</groupId>
            <artifactId>support-v4</artifactId>
            <version>19</version>

* Apache IO Commons v2.4
    * Artifact available at Maven Central as:

            <groupId>commons-io</groupId>
            <artifactId>commons-io</artifactId>
            <version>2.4</version>

* Android Volley library 1.0.6
    * Source code available at [https://github.com/mcxiaoke/android-volley](https://github.com/mcxiaoke/android-volley)
	* Artifact available at Maven Central as:

            <groupId>com.mcxiaoke.volley</groupId>
            <artifactId>library</artifactId>
            <version>1.0.6</version>

* Third party cloud messaging API client:
    * For standard Android devices with API level >= 8: GCM Client v1.0.2
    * For Android X devices: Nokia Notification API (NNA) push library v1.0.0
    * For Kindle Fire devices >= 2nd generation: Amazon Device Messaging client library v1.0.1
* Android SDK
    * For standard Android devices, SDK 2.2 or greater
    * For Android X devices, SDK 4.1 or greater
    * For Kindle Fire devices, SDK **NOT** greater than 4.0.*


## Creating a new push client from scratch with core API

### Framework overview

The Boxcar Push library includes basic objects to build clients quickly. Among important classes you have:

* **Boxcar**: This is the main entry point to interact with Universal Push Notification Platform. It offers methods to start the framework, register and unregister from the remote service; retrieve push tags and track notifications. Boxcar also uses a persistent cache based on SQLite database to store incoming notifications and track its internal state (unread / read).
* **BXCConfig**: this is the configuration class to tweak the SDK. For example, the way notifications should be displayed and how they should refer to your application.
* **BXCCallback**: callback interface. Every artifact from your application, like activities, fragments or services that needs to listen for incoming events from Universal Push Notification Platform must implement this interface according to the desired behaviour on each case.
* **BXCNotification**: represents an incoming notification from P1Push service.

**Boxcar Push Library assumes your application does initialize the framework every time it is started.** This means your application should override the method *Application#onCreate()* and initialize it there. If you do not initialize the framework at this stage you could have errors while dealing with incoming notifications from GCM (or any other provider). The first step your O.S. accomplishes when a notification has arrived for your application is to start it (if it wasn't running), that's why the framework should be initialized at that stage and not later:

	@Override
	public void onCreate() {
		super.onCreate();
		startBXCPushFramework();
	}

The following is the implementation of *startBXCPushFramework()* taken from the demo app:

	private void startBXCPushFramework() {
		
		// build notification strategy
		int icon = R.drawable.ic_stat_gcm;
		String title = getString(R.string.app_name);
		BaseUINotificationStrategy notificationStrategy =
				new ExtendedUINotificationStrategy(icon, title,
						NormalNotificationActivity.class, WebViewActivity.class,
						InboxActivity.class);

		String pushScheme = getResources().getString(R.string.pushScheme);
		String pushHost = getResources().getString(R.string.pushHost);
		int pushPort = 443;
		try {
			pushPort = Integer.parseInt(getResources()
							  .getString(R.string.pushPort));
		} catch (Exception e) {}
		
		String clientKey = getResources().getString(R.string.clientKey);
		String clientSecret = getResources().getString(R.string.clientSecret);
		String senderId = getResources().getString(R.string.gcmSenderId);
		
		config = new BXCConfig(pushScheme, pushHost, pushPort,
				clientKey, clientSecret, notificationStrategy, senderId);
		config.setRichPushBaseUrl(getResources().getString(R.string.richPushBaseURL));

        String deviceName = android.os.Build.MANUFACTURER + " " +
        					android.os.Build.PRODUCT;

		Boxcar.start(this, callback, config, deviceName);
		
		try {
			PushSupportStatus status = Boxcar.getPushSupportStatus(this);
			if (!status.equals(PushSupportStatus.supported)) {
				Log.e(TAG, "Push not supported. Reason: " + status);
				Toast.makeText(getApplicationContext(), "Push not supported",
						Toast.LENGTH_LONG).show();
				return;
			}
			Log.v(TAG, "Registering on p1push service");
			Boxcar.register(DemoApplication.this);
        } catch (BXCPendingOperationException e) {
            Log.e(TAG, "There is a pending operation. We must wait to the callback.", e);
		} catch (BXCException e) {
			Log.e(TAG, "Error registering on remote service", e);
		}
	}

In brief, we do two important operations on application startup:

1. Initialize the framework

        Boxcar.start(this, callback, config, deviceName);
    
2. Register on Universal Push Notification Platform

        Boxcar.register(DemoApplication.this);

Note this is the brief method to register. It registers using the tags and alias set on your previous selection. If there is none, then it registers without selecting any special channel or *tag* and without a user name. If you want to change the *tags*, you do so by registering again, this time passing the list of tags the user is interested in (assuming those tags exist on your Boxcar Universal Push Notification Platform project):

    List<String> selectedTags = new ArrayList<String>();
    selectedTags.add("music");

	Boxcar.register(context, selectedTags);

If you also want to register using a user identifier or alias, you have an alternative registration method. A user name is optional and serves as an indirection to allow pushing without knowing the registration id of the user. Typically your application would set a username previously validated.

	Boxcar.register(context, selectedTags, "John Doe");

To receive the feedback from the remote service (whether if your app was able to register, or if an incoming notification was received) you would implement an instance of *io.boxcar.push.BXCCallback*.

**Note**: check that your tag list doesn't contain *deprectated* tags, if any. You should check if your push project has tags that were marked as deprecated. If you register to some deprecated tag, you will be informed through *registrationSuccess(String[] subscribedTags, String[] deprecatedTags)* callback method.

## Configuration

### Registering
To register on Universal Push Notification Platform we need:

#### For Android Devices:
A target device with Android API Level 8 or greater. This is required to support GCM mechanism.

#### For Kindle Devices:
A target device with Android API Level 15 or greater. This is required to support ADM mechanism.

#### For Nokia X Devices:
A target device with Android API Level 16 or greater. This is required to support Nokia Notification API (NNA) mechanism.

### Parameters

To register devices against your Universal Push Notification Platform account, you need to set the credentials associated with your Client as it was defined on your project. If the project you own within the Universal Push Notification Platform is called "Foobar" and you have created two clients for it, namely "Foobar for Android" and "Foobar for Kindle", then you will have two sets of credentials, one for the Android version and another set for the Kindle flavor.

* *clientKey* and *clientSecret*. These are two strings embedded in your client application. They allow your application to register a device and notify the server about an opened notification.
* *senderId*. This is a mandatory parameter for GCM and NNA based applications. For GCM based apps, it comes from Google and is actually the project numeric id. In the [Google API Console](https://console.developers.google.com), look at the URL of your project:

		https://code.google.com/apis/console/#project:xxxxxxxxxxx

    * The xxxxxxxxx is the project ID, which is the sender ID.

    * For Nokia devices you first need a Nokia Developer Account. Once logged in, you should access [https://account.nnapi.ovi.com/cm/Web/services_direct.jsp](https://account.nnapi.ovi.com/cm/Web/services_direct.jsp). After possible queries, you will see My NNA 2.0 services tab on the developer console main page. Enter the desired service identification name to the Sender ID text box and click Create.

    * **Note**: senderId is NOT used at all by Amazon ADM based applications.
	
* URL of the Universal Push Notification Platform.
    * *pushScheme*: https
    * *pushHost*: console.boxcar.io
    * *pushPort*: 443

Connection details like *clientKey*, *clientSecret*, *pushScheme*, *pushHost*, *pushPort* and *senderId* are set on the configuration class *io.boxcar.push.BXCConfig*.

You should also set an algorithm to handle incoming notifications. It must implement the interface defined by *io.boxcar.push.ui.BXCNotificationStrategy* Currently the framework offers three different approaches:

* io.boxcar.push.ui.AutomaticUINotificationStrategy
* io.boxcar.push.ui.MultipleUINotificationStrategy
* io.boxcar.push.ui.ExtendedUINotificationStrategy
* io.boxcar.push.ui.DummyNotificationStrategy

with the exception of *DummyNotificationStrategy*, all of them extend from io.boxcar.push.ui.BaseUINotificationStrategy, which implements *io.boxcar.push.ui.BXCNotificationStrategy* but also offers some base methods that allow to customize some options.

#### <a name="automaticUI"></a>AutomaticUINotificationStrategy

Renders notifications on the Android Notification Center and updates it if there were pending notifications on it. In other words, it groups notifications and keeps a pending intent for the last event received. To extract the notification from the intent triggered after the user tapped on it, you should get the "notification" argument. Example:

    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);
        BXCNotification notification = intent.getParcelableExtra("notification");
        if (notification != null) {
            // handle notification
        }
    }

#### MultipleUINotificationStrategy

Renders a new notification on the Android Notification Center and a new notification on the Notification Area. Each notification is unique and keeps the alert information specific to it. So, if application receives three alerts, it will render three different notification icons on the notification area. The way to handle intents generated by notifications built by this strategy is the same than [*AutomaticUINotificationStrategy*](#automaticUI).

#### ExtendedUINotificationStrategy

Renders a single notification showing the last three received alerts since the last time *Boxcar#cleanNotificationsAndBadge(Context context)* was called. The action to be performed when user taps on it depends on the amount of pending notifications and type. Strategy constructor allows up to three Activities to be opened, depending on the case type, which could be:

1. there is a *single* unread *plain notification*
2. there is a *single* unread *rich notification*
3. there are two or more unread notifications

##### ExtendedUINotificationStrategy instantiation example

The following example shows a possible instantiation using three different activities:

1. NormalNotificationActivity (manages a single plain notification)
2. WebViewActivitiy (manages a single rich notification)
3. InboxActivity (manages a collection of notifications, either plain or rich)

        BXCNotificationStrategy notificationStrategy =
            new ExtendedUINotificationStrategy(icon, title, NormalNotificationActivity.class,
                    WebViewActivity.class, InboxActivity.class);

For the first and second case (single notification), the BXCNotification instance representing the incoming alert
should be extracted from the notification intent within the extra argument *"notification"*. The following example
shows how to handle an incoming intent from a single rich push:

    private void handleIntent(Intent intent) {
        String url = intent.getStringExtra("url");
        BXCNotification notification = intent.getParcelableExtra("notification");
        // ... do your processing here
    }

The following example shows how to handle incoming intents containing several unread notifications. Please note that
we now extract a list of incoming notifications from the extra argument *"notifications"*:

    private void handleIntent(Intent intent) {
        String url = intent.getStringExtra("url");
        List<BXCNotification> incomingNotifications = intent.getParcelableArrayListExtra("notifications");
        // ... do your processing here
    }

#### DummyNotificationStrategy
This is a direct implementation of the *io.boxcar.push.ui.BXCNotificationStrategy* interface which does nothing when a notification is received.

You should take this class into account if you want to take complete control of the notification process. If you set an instance of this class as the notification strategy and you also listen to notification events through *BXCCallback#notificationReceived(BXCNotification notification)* you will have complete control about what to do on your application every time a push is received. 

The dummy notifcation strategy should be instantiated with the empty constructor:

    BXCNotificationStrategy notificationStrategy = new DummyNotificationStrategy();

**Note**: In the demo applicaton all parameters are externalized into *res/values/config.xml*.

##### Custom notification strategies
You can also define your own strategy implementing the interface *io.boxcar.push.ui.BXCNotificationStrategy*. There is also a base class with template methods based on the previous interface.

The most important thing to remember is that every time a new push arrives to the device, the SDK will call the following method: 

~~~ java
public void handleNotification(Context context, BXCNotification notification) {
	// your custom implementation goes here
}
~~~

Below is a source example of the *MultipleUINotificationStrategy* implementation, which extends from the aforementioned base class *BaseUINotificationStrategy*. It is a good starting point to implement your own if the provided strategies don't fit your usage scenario.
 
~~~ java
package io.boxcar.push.ui;

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
 * Intents targets are different depending on whether this is a simple
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
    public void handleNotification(Context context,
                                   BXCNotification notification) {
        generateNotification(context, notification);
    }
    
    /**
     * Issues a notification to inform the user that server has sent a message.
     */
    protected void generateNotification(Context context,
                                        BXCNotification notification) {
    
        Intent notificationIntent;
        String url = extractURL(notification);        
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
    
        int notificationId = getNotificationIdFor(notification);
        
        PendingIntent intent = PendingIntent.getActivity(context,
                notificationId,
                notificationIntent, 0);
        long when = System.currentTimeMillis();
        
        // The following statement calls a hook method defined in
        // BaseUINotificationStrategy that instantiates a notification
        // builder based on the attributes set above.
        NotificationCompat.Builder notifyBuilder =
                prepareNotificationBuilder(context, notification, when, intent);
    
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

    BaseUINotificationStrategy.setDefaultSound(Context context, int resourceId);
    
For example:

    notificationStrategy.setDefaultSound(this, R.raw.beep);

#### Configuring notification actions
The Boxcar Universal Push Notification Platform allows to define *categories* for the pushes to send. This is an optional attribute you can set on the push which helps the SDK to distinguish between different kind of notifications.

You can for example configure you application to add one or more buttons (actions) to the notification widget when the category is *'sports_schedule'*.

Let's say you want to add the button 'Favorite' for pushes belonging to that category. In that case you should create a mapping between the *sports_schedule* category and the action(s) you want for that notification.

Create the mapping and add it to your *BXCConfig* configuration instance:

    ...
    NotificationCategoryMapping mapping = buildCategoryMapping();
    config.addNotificationCategoryMapping(mapping);
    ...
    
    private NotificationCategoryMapping buildCategoryMapping() {
        NotificationCategoryMapping mapping = new NotificationCategoryMapping("sports_schedule");
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
		        
                return PendingIntent.getService(context, -1, intent, PendingIntent.FLAG_CANCEL_CURRENT);
            }
        });
            
        return mapping;
    }

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

The bxcframework-demo project is an example on how to integrate with Boxcar Push Service on Android devices based on Google Cloud Messaging (GCM).

### Boxcar SDK integration

#### Setup your project

* **Step 1**: Create a new Android project and add the provided jar file (bxc-android-core.jar or bxc-android-core-jar-with-dependencies.jar) to your project classpath. Project must be targetted for Android 2.2 or greater.
* **Step 2**: Add GCM permissions to your Android Manifest file. Permissions are specified in XML as children of &lt;manifest&gt; element. In this example our application package (as defined on 'package' attribte of &lt;manifest&gt; element) is *"io.boxcar.push.demo"*:

        <uses-permission android:name="com.google.android.c2dm.permission.RECEIVE" />
        <permission android:name="io.boxcar.push.demo.permission.C2D_MESSAGE" android:protectionLevel="signature" />
        <uses-permission android:name="io.boxcar.push.demo.permission.C2D_MESSAGE" />

* **Step 3**: Register the GCM service on your Android manifest file. Services are specified in XML as children of &lt;application&gt; element.

        <service android:name="io.boxcar.push.GCMIntentService" android:enabled="true"/>

* **Step 4**: Configure the corresponding received for GCM incoming notifications. Receivers are specified in XML as children of &lt;application&gt; element.

        <receiver android:name="io.boxcar.push.GCMReceiver" android:enabled="true" android:permission="com.google.android.c2dm.permission.SEND" >
            <intent-filter>
                <!-- Receives the actual messages. -->
                <action android:name="com.google.android.c2dm.intent.RECEIVE" />
                <!-- Receives the registration id. -->
                <action android:name="com.google.android.c2dm.intent.REGISTRATION" />
                <category android:name="io.boxcar.push.demo" />
            </intent-filter>
        </receiver>


* **Step 5**: If you expect to use UDID (Universal Device ID), register the service on your Android manifest file. Services are specified in XML as children of &lt;application&gt; element:

	    <service android:name="org.openudid.android.OpenUDIDService">
	        <intent-filter>
	            <action android:name="org.openudid.android.GETUDID" />
	        </intent-filter>
	    </service>

* **Step 6**: (Optional) You can use an enhanced REST implementation adding the following Intent Service in your Android manifest file. Services are specified in XML as children of &lt;application&gt; element:

        <service android:name="io.boxcar.push.rest.RESTIntentService" android:enabled="true" />

    **Note**: this step is not needed anymore since version 1.0.12 of Boxcar Push Framework.

* **Step 7**: (Optional) Add GET_ACCOUNTS permissions to your Android Manifest file. This permission allows to inspect if this device has a Google / Amazon account associated with it. This allows a finer grain check to know if push is supported or not. This permission is optional. If not set, Boxcar Android Push SDK will skip this specific check. Permissions are specified in XML as children of &lt;manifest&gt; element.

	    <uses-permission android:name="android.permission.GET_ACCOUNTS" />

**Note**: you can see a working example of the *AndroidManifest* file within the push demos released along with the core SDK library. There is currently one demo (basically one *AndroidManifest.xml* file) for each supported platform.

#### Source code

### Usage

Usage is shown in *io.boxcar.push.demo.DemoApplication* and *io.boxcar.push.demo.NormalNotificationActivity*.

*Boxcar* is the only point of interaction, aside from the callbacks to handle incoming notifications.

* When your application starts you would normally start the framework pasing the initial callback (you can add more callbacks later), the choosen user name and a configuration:

        Boxcar.start(Context context, BXCCallback callback, BXCConfig config);

You can alternatively pass an additional *Handler* instance in case you don't want to receive callbacks from the same thread used to invocate the *BXCCallback.start(...)* method.

**Note**: previously, an additional argument, *applicationId*, was required by the aforementioned method to identify your application version. This is now handled internally by the framework. However is still important to define it on your Android manifest file.

* Unregister from push services. Please note that after this invocation your application won't receive any new push notification unless a new registration is performed:

        Boxcar.unregister();

**Note**: this method throws throws *io.boxcar.push.registration.BXCPendingOperationException* if there is a pending registration or unregistration in process. You should always receive a corresponding callback telling when the current operation has finished (either successfully or with error). In other words, do not call this method until your previous call (either 'register' or 'unregister') hasn't received a confirmation via *BXCCallback*.

* Register device. Registers this device on GCM gateway and P1Push service using the provided credentials.

        Boxcar.register(Context context, List<String> tags, String username); or Boxcar.register(Context context);

**Note**: this method throws *io.boxcar.push.registration.BXCPendingOperationException* if there is a pending registration or unregistration in process. You should always receive a corresponding callback telling when the current operation has finished (either successfully or with error). In other words, do not call this method until your previous call (either 'register' or 'unregister') hasn't received a confirmation via *BXCCallback*.

When registration is success, your application should receive a callback event with two string array arguments. The list of tags your device is now subscribed to **and also the list of tags you tried to subscribe but are marked as deprecated for the Boxcar project to which your application belongs**. These tags should be cleaned up by your application.

* Get available tags. This operation can be performed without previous authentication. The result will be informed through the registered callbacks (*BXCCallback#getTagsSuccess(List<String> tags)*):

        Boxcar.getTags();

* Additional callback registration/unregistration:

        Boxcar.registerCallback(BXCCallback callback);
        Boxcar.unregisterCallback(BXCCallback callback);

* Track an incoming notification:

        Boxcar.trackNotification(BXCNotification notification, BXCTrackNotification.State.active);

* Track when application was opened or is being used by the final user

        Boxcar.appBecameActive(Context context);
        
This method is similar to *Boxcar#trackNotification(...)* in the sense it is used for statistical purposes, allowing to know how much your application is being used. You would normally call this method on places like your activity *onResume()* method.

* Get the set of tags to which this instance was registered since the last registration attempt. It won't include tags that were marked as *deprecated* for the current push project:

        List<String> getSubscribedTags()

* Get current status of Boxcar Framework. Either one of *registered*, *unregistered*, *registering* or *unregistering*:

        Boxcar.getState();

* Clean Android Notification Center and reset the number of unread notifications on P1 Push Service:

        Boxcar.cleanNotifications(Context context);
        Boxcar.cleanBadge();

* Check if push is supported on this device. It returns an enumerated type containing the error code or 'supported' when push is natively supported on current device. Push must be natively supported as a pre-requisite of Boxcar Push SDK to work.

        PushSupportStatus status = Boxcar.getPushSupportStatus(this);
        if (!status.equals(PushSupportStatus.supported))
            Log.e(TAG, "Push not supported. Reason: " + status);
    
Possible values an its meaning:

    *supported*: native push is supported on this device (GCM on Android, ADM on Amazon, etc).
	*missingNativePushAccount*: this device is not linked to the native provider (Google / Amazon account).
	*missingNativePushDependencies*: current operating system doesn't provide the required dependencies. Usually this is because operating system version is old.
	*missingNativePushManifest*: AndroidManifest doesn't provide all the required permissions and services required to support native push.
        
You are not required to explicitly check push support status. It will be performed automatically on *Boxcar.register(...)* call and and exception thrown if native push is not supported. Remember that if you do not allow the application to access account information (*android.permission.GET_ACCOUNTS*), the SDK won't be able to check if the device is linked to an authorized provider account (Gmail account on standard Android devices, Amazon on Kindle devices).

Additionally you need to implement BXCCallback methods to associate specific behaviour for each possible asynchronous event:

* notificationReceived(BXCNotification notification);
* registrationSuccess(String[] subscribedTags, String[] deprecatedTags);
* registrationFailed(Throwable t);
* trackNotificationSuccess(BXCNotification notification);
* trackNotificationFailed(BXCNotification notification, Throwable t);
* badgeResetSuccess();
* badgeResetFailed(Throwable t);
* unregisterSuccess();
* unregisterFailed(Throwable t);
* getTagsFailed(Throwable t);
* getTagsSuccess(List<String> tags);

### Troubleshooting

#### Device seems registered but no pushes are received

1. check that your device is on the list of registered devices, of for the corresponding project under Boxcar Push Service administration console.
2. ensure that the *clientKey* and *clientSecret* set on *io.boxcar.push.BXCConfig* are the same as defined on your Universal Push Notification Platform project for the corresponding Android client application.
3. similarly, check that *senderId* (excluding Amazon Kindle apps) value defined on *io.boxcar.push.BXCConfig* is associated with the *API Key* set on Boxcar Push Service for the corresponding Android client application. If your device had a wrong *senderId* previously, please make sure your device is unregistered by calling *Boxcar.unregister()*. This step forces your application to request a new token to GCM/Nokia cloud.

## Boxcar Framework Demo for Amazon Kindle devices

The bxcframework-demo-kindle project is an example on how to integrate with Boxcar Push Service on Amazon devices based on Amazon Device Messaging (ADM).

### Boxcar SDK integration

#### Setup your project
Before setting up your project, please ensure you have:

1. created an account on Amazon, following the instructions here: [https://developer.amazon.com/sdk/adm/credentials.html](https://developer.amazon.com/sdk/adm/credentials.html)
2. set your API Key. On the demo project the API key has a placeholder value on the file *assets/api_key.txt*. You must set the one generated by you, based on your Amazon account and the signature used to sign your application. All these details are documented on the step 5 of the link above.
3. enabled ADM feature on your Amazon account. Read step 6 on the link above.
4. ensure each Amazon Kindle device is registered into the Amazon network.

* **Step 1**: Create a new Android project and add the provided jar file (bxc-android-core.jar or bxc-android-core-jar-with-dependencies.jar) to your project classpath. Project must be targetted for Android 4.0.3 (API 15) or greater.
* **Step 2**: Add ADM permissions to your Android Manifest file. Permissions are specified in XML as children of &lt;manifest&gt; element. In this example our application package (as defined on 'package' attribte of &lt;manifest&gt; element) is *"io.boxcar.push.demo"*:

        <!-- This permission ensures that no other application can intercept your ADM messages. -->
        <permission android:name="io.boxcar.push.demo.permission.RECEIVE_ADM_MESSAGE" android:protectionLevel="signature" />
        <uses-permission android:name="io.boxcar.push.demo.permission.RECEIVE_ADM_MESSAGE" />
        <!-- This permission allows your app access to receive push notifications from ADM. -->
        <uses-permission android:name="com.amazon.device.messaging.permission.RECEIVE" />
        <!-- ADM uses WAKE_LOCK to keep the processor from sleeping when a message is received. -->
        <uses-permission android:name="android.permission.WAKE_LOCK" />

* **Step 3**: Register the ADM service on your Android manifest file. Services are specified in XML as children of &lt;application&gt; element.

        <service android:name="io.boxcar.push.ADMIntentService" android:enabled="true" android:exported="false" />

* **Step 4**: Configure the corresponding received for ADM incoming notifications. Receivers are specified in XML as children of &lt;application&gt; element.

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

* **Step 5**: If you expect to use UDID (Universal Device ID), register the service on your Android manifest file. Services are specified in XML as children of &lt;application&gt; element:

	    <service android:name="org.openudid.android.OpenUDIDService">
	        <intent-filter>
	            <action android:name="org.openudid.android.GETUDID" />
	        </intent-filter>
	    </service>

* **Step 6**: You must explicitly enable ADM. To do so, add the following element, specified in XML as children of &lt;application&gt; element.

        <amazon:enable-feature android:name="com.amazon.device.messaging" android:required="true" />

#### Source code

Source code for the Kindle demo application is the same than the standard Android app explained above.

**Important:*** do not include Amazon ADM SDK library (*messaging.jar* in our demo app) into your APK file. This library is already provided by the Amazon environment already. If you include the library in your packaged application you will experience errors during the registration phase. 

## Boxcar Framework Demo for Nokia NM devices

The bxcframework-demo-nokia project is an example on how to integrate with Boxcar Push Service on Nokia NM devices based on Nokia Notification API (NNA).

### Boxcar SDK integration

#### Setup your project
Before setting up your project, please ensure you have:

1. created a Nokia Developer Account to define the senderId for your project and get the API Key generated by Notifications API Developer Console.

2. set your senderId on *res/values/config.xml*.

* **Step 1**: Create a new Android project and add the provided jar file (bxc-android-core.jar or bxc-android-core-jar-with-dependencies.jar) to your project classpath. Project must be targetted for API 16 or greater.
* **Step 2**: Add Nokia permissions to your Android Manifest file. Permissions are specified in XML as children of &lt;manifest&gt; element. In this example our application package (as defined on 'package' attribte of &lt;manifest&gt; element) is *"io.boxcar.push.demo"*:

        <permission android:name="io.boxcar.push.demo.permission.C2D_MESSAGE" android:protectionLevel="signature" />
        <uses-permission android:name="io.boxcar.push.demo.permission.C2D_MESSAGE" />
        <!-- This permission allows your app access to receive push notifications from Nokia Notifications. -->
        <uses-permission android:name="com.nokia.pushnotifications.permission.RECEIVE" />
        <uses-permission android:name="android.permission.WAKE_LOCK" />
        <uses-permission android:name="android.permission.INTERNET" />
        <uses-permission android:name="android.permission.GET_ACCOUNTS" />
        <uses-permission android:name="android.permission.VIBRATE" />

* **Step 3**: Register the Nokia NNA service on your Android manifest file. Services are specified in XML as children of &lt;application&gt; element.

        <service android:name="io.boxcar.push.NNAIntentService" android:enabled="true" android:exported="false" />

* **Step 4**: Configure the corresponding received for NNA incoming notifications. Receivers are specified in XML as children of &lt;application&gt; element.

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

* **Step 5**: If you expect to use UDID (Universal Device ID), register the service on your Android manifest file. Services are specified in XML as children of &lt;application&gt; element:

        <service android:name="org.openudid.android.OpenUDIDService">
            <intent-filter>
	        <action android:name="org.openudid.android.GETUDID" />
            </intent-filter>
        </service>

#### Source code

Source code for the Nokia demo application is the same than the standard Android app explained above.

[image-1]:	/images/android/phone-device-big-picture.png
[image-2]:	/images/android/wear-device-big-picture.png
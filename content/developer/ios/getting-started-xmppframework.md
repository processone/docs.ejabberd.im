---
title: Getting Started with XMPPFramework
---

# Getting Started with XMPPFramework

XMPPFramework is a large framework relying on several
dependencies. The easiest way to get started is to use Cocoapods to
integrate XMPPFramework in your own project. It will take care of
adding all dependencies and perform all the required configuration
steps.

Here are the steps needed to get started:

1. Create a new iOS project in Xcode, if you do not have one.

2. If you do not yet have a `Podfile`, create it if `pod init` command
   from the project root directory.
   
3. Edit your `Podfile` to use XMPPFramework as a target. It may looks like:

   ~~~ ruby
   platform :ios, '6.0'
   use_frameworks!

   target 'projectname' do
      pod 'XMPPFramework'
   end
   ~~~

4. Run `pod install` command. It should download, install and
   configure three pods.

5. Open your XCode project with the newly created workspace file
   instead of the project file. This is required by Cocoapods so that
   you can use the installed Pods.
   
6. At this stage, you should be able to build your project
   successfully with the XMPP framework setup.


# GrijjyCloudLogger, *remote logging for Windows, iOS, Android, macOS and Linux*
GrijjyCloudLogger is a remote logging tool that allows you to send log messages over the Intranet or Internet from Windows, Linux, iOS, Android and macOS devices to a viewer running on Windows.  Besides sending messages along with any data, it has numerous features including custom live watches, remote live views of objects, tracking live memory usage, object allocations, growth leaks and more.	 

Here at Grijjy we use this tool on a daily basis to help us diagnose run-time related issues with our applications running on various platforms.  Our logger helps us easily examine the run-time state of our application running on iOS and Android devices, which can be difficult using the debugger or mobile platform specific logging features.  We developed this utility because we needed a high-performance remote logger that worked on all platforms and operating systems and we wanted unified, run-time debug related capabilities like memory and object tracking from these respective platforms.

Being able to analyze the run-time of your application using your Windows desktop, where your IDE already resides, is a powerful tool in your arsenal as a developer.  We hope you find it useful.

![](http://i.imgur.com/1toU6Pl.png)

The GrijjyCloudLogger is built upon our [ZeroMQ Majordomo implementation](https://blog.grijjy.com/2017/05/02/roll-your-own-lightweight-scalable-backend-using-zeromq/) that allows you to create powerful, lightweight, distributed applications that can route messages over any network, including the Internet.  It is extremely fast over the network and can handle numerous connected developers simultaneously.  It also uses our [Google Protocol Buffers implementation](https://blog.grijjy.com/2017/04/25/binary-serialization-with-google-protocol-buffers/) that allows us to encapsulate extensible and arbitrary data and transport the data using efficient payloads.

# Getting Started
To use GrijjyCloudLogging in your Delphi project all you need to do is include the `Grijjy.CloudLogging` unit in your uses list.  You should also run the `GrijjyLogBroker.exe` to route messages and the `GrijjyLogViewer.exe` to view messages.  These [binaries are included in our GitHub repository](https://github.com/grijjy/GrijjyCloudLogger/tree/master/Bin).

```Delphi
uses Grijjy.CloudLogging;
```
To send messages simply use one of the many provided `GrijjyLog.Send()` methods.
```Delphi
GrijjyLog.Send('String value', 'Foo');
```
## Sending data using Grijjy Cloud Logger

The Grijjy Cloud Logger provides various overloaded `Send()` methods to remotely send data over the network to the GrijjyLogViewer.  Besides the basic data types, the Grijjy Cloud Logger support sending of advanced data types including `TStrings`, Memory Pointers, `TBytes` and even `TObject`s.

Each of the various `Send()` methods has common parameters, namely `AMsg` parameter which is always a string that is displayed in the Log Viewer Messages frame.  The `AValue` is the actual data of various overloaded types.  You can also send an optional `TgoLogLevel` parameter indicating the severity of the message.  Lastly an optional parameter called the Service name.  The Service name indicates who is sending the message so the Log Broker can properly route the message to the intended Log Viewer (more on this topic later).

#### Common types
Most of the basic data types are supported by passing the data directly to the `Send()` method. 
```Delphi
GrijjyLog.Send('String value', 'Foo'); // strings
GrijjyLog.Send('Integer value', 42); // integer
GrijjyLog.Send('Boolean value', True); // boolean
GrijjyLog.Send('Float value', Pi); // extended
```
#### TStrings
`TStrings` can be sent over the cloud logger as well by simply constructing a `TStringList` and calling the overloaded `Send()` method.
```Delphi
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Add('Foo');
    S.Add('With Spaces');
    S.Add('With, Commas');
    S.Add('With "Quotes"');
    S.Add('With ''Quotes''');
    S.Add('Width , "every", ''thing''');
    GrijjyLog.Send('TStrings value', S, TgoLogLevel.Warning);
  finally
    S.Free;
  end;
end;
```
#### TBytes
Grijjy Cloud Logger provides several methods to display blocks of memory.  In this example we send a `TBytes` object to the remote viewer.
```Delphi
var
  Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes
    ('The Quick Brown Fox Jumps Over The Lazy Dog');
  GrijjyLog.Send('TBytes value', Bytes, TgoLogLevel.Warning);
end;
```
#### Data Pointers
The second approach to sending blocks of memory over the cloud logger is to use the `Send()` method with a Pointer to a memory location and a Size in bytes. 
```Delphi
var
  Bytes: TBytes;
  I: Integer;
begin
  SetLength(Bytes, 997);
  for I := 0 to Length(Bytes) - 1 do
    Bytes[I] := Random(256);
  Bytes[10] := 0;
  GrijjyLog.Send('Memory value', @Bytes[0], Length(Bytes), TgoLogLevel.Warning);
end;
```
#### TObject
`TObject`s can be sent along with all of the objects fields and properties.  You can choose whether only public and published fields are shown or all protected and private fields as well.  Optionally you can also choose how many subfield levels are sent for the various fields and properties.
```Delphi
GrijjyLog.Send('Object value', Self, mvPublic, 4, TgoLogLevel.Warning);
```

![](http://i.imgur.com/WYDpCMv.png)

> Note that this can potentially be a slow and bandwidth-intensive call since RTTI is used to query the object, and it may result in large data loads depending on the `AMinVisibility` and `AMaxNesting` parameters.

#### Nested methods
The GrijjyCloudLogger contains method helpers that allow you to more easily view when methods are entered and exited.  `EnterMethod` logs the start of a method block. Subsequent `Send()` calls to Log will be treated as part of this method, until `ExitMethod` is called.
```Delphi
var
  Foo: TSampleFoo;
begin
  Foo := TSampleFoo.Create;
  try
    GrijjyLog.EnterMethod(Self, 'ButtonMethodClick');
    GrijjyLog.Send('Inside TFormMain.ButtonMethodClick', TgoLogLevel.Info);
    Foo.SomeMethod;
    GrijjyLog.ExitMethod(Self, 'ButtonMethodClick');
  finally
    Foo.Free;
  end;
end;
```
![](http://i.imgur.com/wSZAb6b.png)
#### Information, warning and error messages
All the `Send()` methods support an optional `TgoLogLevel` parameter indicating the severity of the message.  Additionally there are messages specifically for sending `TgoLogLevel` notifications.
```Delphi
  GrijjyLog.Send('Sample Error Message', TgoLogLevel.Error);
  GrijjyLog.Send('Sample Info Message', TgoLogLevel.Info);
  GrijjyLog.Send('Sample Warning Message', TgoLogLevel.Warning);
```
A special method called `SetLogLevel()` allows you to control which messages are displayed in the GrijjyLogViewer based upon the current log level  The default log level is `Info` in DEBUG mode and `Warning` in RELEASE mode.  With `Info` level, all messages are logged. With `Warning` level, only warning and error messages are logged and with `Error` level, only error messages are logged.

# Memory and object tracking
Besides the cross-platform remote logging capabilities, the GrijjyCloudLogger contains integrated features that help you understand the state of your application at run-time.  The first of these features is the memory and object tracker. 

To enable the object tracker, you need to add the `Grijjy.CloudLogging.InstanceTracker` unit to the uses clause of your project (.dpr) file, preferably as the very first unit:

```Delphi
program MyProgram;

uses
  Grijjy.CloudLogging.InstanceTracker,
  ...other units...
```

By adding this unit, your application will keep track of most allocated objects (using techniques described in our [Cross-Platform Code Hooking](https://blog.grijjy.com/2017/07/26/cross-platform-code-hooking/) blog article), so it can provide this information to the log viewer. Since this adds some overhead, this functionality is only enabled in DEBUG builds. In RELEASE builds, the InstanceTracker unit will act as an empty unit.

Each time you click Update in the GrijjyLogViewer, it will provide a real-time snapshot of the allocated objects from your running application and the memory consumed by those objects.  This is very useful in determining if particular actions in your run-time application are actually allocating and deallocating objects as you expect.  The tracker also displays the Delta between each Update and the Max or peak values.

![](http://i.imgur.com/4mXc5Jl.png)

# Live watches
The Live watches feature of the GrijjyCloudLogger allows you to create custom data that can be sent over the network to the GrijjyLogViewer at run-time.

To create a live watch, you only need to subscribe a `TMessage` in your application and add your custom data to the `TgoLiveWatchesMessage`.  The GrijjyCloudLogger will automatically handle the data transport, routing and viewing.

To start sending live watches to the remote log viewer, simply subscribe the `TgoLiveWatchesMessage` message. 
```Delphi
TMessageManager.DefaultManager.SubscribeToMessage(TgoLiveWatchesMessage, HandleLiveWatches);
```
In your `TgoLiveWatchesMessage` listener you populate the `TgoLiveWatchesMessage` with one or more message and data elements.

```Delphi
procedure TMyClass.HandleLiveWatches(const Sender: TObject;
  const M: TMessage);
var
  Msg: TgoLiveWatchesMessage absolute M;
begin
  Assert(M is TgoLiveWatchesMessage);
  Msg.Add('Custom Watch', TrackBar.Value, 1);
end;
```
Then in the GrijjyLogViewer you click Update to retrieve the current state of any live watches in your remote app.
![](http://i.imgur.com/USaxySB.png)

# Grijjy Log Broker
The GrijjyLogBroker routes log messages between senders and viewers.  By default the Broker is configured to listen on `tcp://localhost:7337`.  When you use `Grijjy.CloudLogging` in your project you need to call the `GrijjyLog.Connect()` method and provide the address of the Broker.  You also can provide an optional service name.  If you intend to share the Broker across your development organization, each developer should use a unique service name when connecting to the Broker.  This service name is provided to both the `GrijjyLog.Connect` when sending and receiving messages and to the GrijjyLogViewer in the connection settings.

# Grijjy Log Viewer
The GrijjyLogViewer displays messages that are sent from your app.  The Service name in the Connection Settings of the GrijjyLogViewer should match the same Service name you provided to the Grijjy.CloudLogger when calling the `Connect()` method.  Using this technique, all developers in your organization can share the same GrijjyLogBroker.

![](http://i.imgur.com/pLa92Yw.png)

# Installing the GrijjyCloudLogger
GrijjyCloudLogger is available in the Embarcadero GetIt Package Manager in RAD Studio, otherwise to install the GrijjyCloudLogger you need to install a few repositories and add some Delphi library search paths.

#### 1. Download the following GitHub repositories,
- GrijjyCloudLogger - https://github.com/grijjy/GrijjyCloudLogger
- GrijjyFoundation - https://github.com/grijjy/GrijjyFoundation
- DelphiZeroMQ - https://github.com/grijjy/DelphiZeroMQ

#### 2. Add the GrijjyFoundation source folder to your Delphi library path for all platforms.
#### 3. Add the DelphiZeroMQ source folder to your Delphi library path for all platforms.

#### 4. Run the GrijjyLogBroker.exe and the GrijjyLogViewer.exe located under the `/GrijjyCloudLogger/Bin` folder.

> If your app is running on Windows, macOS and Linux you must deploy the ZeroMQ library for those respective platforms with your app.  Those library binaries are pre-built and are located under the folder `/DelphiZeroMQ/Lib`.  For Android and iOS we use pre-built static libraries that are automatically linked into your application. 

We intend to work with Embarcadero in the future to simplify the install steps and get the GrijjyCloudLogger hosted by GetIt. 

# Firemonkey Example
We included an example that demonstrates how to `Send()` GrijjyCloudLogger messages from all platforms supported by FireMonkey currently, including Windows, Android, iOS and macOS.  The [ExampleLogClient.FMX is provided in our GitHub repository](https://github.com/grijjy/GrijjyCloudLogger/tree/master/ExampleLogClient.FMX) for the GrijjyCloudLogger.

![](http://i.imgur.com/pLq2rIr.png)

# Console Example for Windows and Linux Services 
For Console oriented platforms like Linux and server-side applications we also include an [ExampleLogClient.Console](https://github.com/grijjy/GrijjyCloudLogger/tree/master/ExampleLogClient.Console) in our GitHub repository for the GrijjyCloudLogger.

# Using GrijjyCloudLogger with development teams
While it is perfectly acceptable to run the GrijjyLogBroker, GrijjyLogViewer and your app on the same Windows computer as a single developer, you may want to consider sharing your installation with the entire development team.  There are many reasons why this may be beneficial.

First off all, the [ZeroMQ Majordomo protocol](https://github.com/grijjy/DelphiZeroMQ) that the GrijjyCloudLogger is based upon, takes care of routing issues for you automatically.  All sender apps using the Grijjy.CloudLogging unit and all log viewer using GrijjyLogViewer connect to the Broker directly.  If you run the Broker on your network, everyone simply routes the same TCP/IP address and port, they just use different Service names.  However, your network manager could create a network port mapping to the GrijjyLogBroker so that your entire team could use the same Broker whether you were local to the network or remote and your messages would route from all your apps to any of your viewers automatically with a single firewall network mapping.

Another approach is to simply install the GrijjyLogBroker on a computer on the Internet in the cloud.  Everyone on the local network or those connecting over the Internet would route the same TCP/IP address and port and no firewall changes are required.

# ZeroMQ and the Majordomo Protocol
As we mentioned earlier, the GrijjyCloudLogger is built using our implementation of the [ZeroMQ Majordomo protocol](https://github.com/grijjy/DelphiZeroMQ).  It demonstrates one of many possible solutions you can create with the powerful ZeroMQ distributed framework.  

Another really nice feature of ZeroMQ, is that it is mostly protocol agnostic.  In the example we connect using `tcp://localhost:7337`.  However ZeroMQ supports many other protocols including ipc:// (interprocess communications), inproc:// (intraprocess) and more.  

ZeroMQ is an excellent choice for many iOT and performance oriented messaging models.  If you are interested in this topic, please see our [related article on ZeroMQ](https://blog.grijjy.com/2017/05/02/roll-your-own-lightweight-scalable-backend-using-zeromq/).

# Conclusion
We hope you find our remote logger useful in your everyday development efforts.  The Grijjy team is considering adding other features for the future including showing full stack traces of each object allocation on most platforms, trapping and remote logging of exceptions on various platforms and more.

The example contained here depends upon part of our [Grijjy Foundation library](https://github.com/grijjy/GrijjyFoundation).

The source code and related example repository are hosted on GitHub at [https://github.com/grijjy/GrijjyCloudLogger](https://github.com/grijjy/GrijjyCloudLogger).

# License
GrijjyCloudLogger, Grijjy.CloudLogging, GrijjyLogBroker, GrijjyLogViewer and related classes along with the example programs are licensed under the Simplified BSD License. See License.txt for details.
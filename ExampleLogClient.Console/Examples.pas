unit Examples;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  System.Messaging,
  System.Generics.Collections;

type
  TExamples = class
  private
    FObjects: TObjectList<TObject>;
    FMemoryBlocks: TArray<TBytes>;
    FStartTime: TDateTime;
    FCustomWatchValue: String;
  private
    { Entry points }
    procedure Connect;
    function MainMenu: Boolean;
  private
    { Logging examples }
    procedure LogBasic;
    procedure LogDataTypes;
    procedure LogTracking;
    procedure LogLiveWatches;
    procedure LogOther;
  private
    { Memory tracking actions }
    procedure TrackAllocateObjects;
    procedure TrackDeallocateObjects;
    procedure TrackAllocateMemory;
    procedure TrackDeallocateMemory;
  private
    { Message handler }
    procedure HandleLiveWatches(const Sender: TObject; const M: TMessage);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Run;
  end;

implementation

uses
  System.Classes,
  System.TypInfo,
  System.DateUtils,
  Grijjy.SysUtils,
  Grijjy.CloudLogging,
  SampleClasses;

{ TExamples }

procedure TExamples.Connect;
var
  Broker, Service: String;
begin
  WriteLn;
  Write('Enter broker address (default = "', GrijjyLog.DEFAULT_BROKER, '"): ');
  ReadLn(Broker);
  if (Broker.Trim = '') then
    Broker := GrijjyLog.DEFAULT_BROKER;

  Write('Enter service (default = "', GrijjyLog.DEFAULT_SERVICE, '"): ');
  ReadLn(Service);
  if (Service.Trim = '') then
    Service := GrijjyLog.DEFAULT_SERVICE;

  GrijjyLog.Connect(Broker, Service);
  WriteLn('Connected to broker "', Broker, '" using service "', Service, '".');
end;

constructor TExamples.Create;
begin
  inherited;
  FObjects := TObjectList<TObject>.Create;

  { Enable all log levels }
  GrijjyLog.SetLogLevel(TgoLogLevel.Info);

  { Subscribe to the Live Watches message to provide the Grijjy Log Viewer with
    a list of watches.
    Besides this form, TFrameWatches also subscribes to this message. }
  TMessageManager.DefaultManager.SubscribeToMessage(TgoLiveWatchesMessage,
    HandleLiveWatches);
  FStartTime := Now;
end;

destructor TExamples.Destroy;
begin
  FObjects.Free;
  inherited;
end;

procedure TExamples.HandleLiveWatches(const Sender: TObject; const M: TMessage);
var
  Msg: TgoLiveWatchesMessage absolute M;
  ElapsedMs: Integer;
begin
  Assert(M is TgoLiveWatchesMessage);
  ElapsedMs := MilliSecondsBetween(Now, FStartTime);
  Msg.Add('Elapsed Seconds', ElapsedMs / 1000, 1);
  Msg.Add('Custom Watch', FCustomWatchValue);
end;

procedure TExamples.LogBasic;
var
  Foo: TSampleFoo;
begin
  WriteLn;

  // Send Enter Method message, with nested submessages
  GrijjyLog.EnterMethod(Self, 'Basic');

  // Send Info message
  GrijjyLog.Send('Sample Info Message', TgoLogLevel.Info);

  // Send Warning message
  GrijjyLog.Send('Sample Warning Message', TgoLogLevel.Warning);

  // Send Error message
  GrijjyLog.Send('Sample Error Message', TgoLogLevel.Error);

  // Send messages from TSampleFoo object
  Foo := TSampleFoo.Create;
  try
    Foo.SomeMethod;
  finally
    Foo.Free;
  end;

  // Exit the method
  GrijjyLog.ExitMethod(Self, 'Basic');

  WriteLn('Sent basic log messages to the Grijjy Log Viewer');
end;

procedure TExamples.LogDataTypes;
var
  SL: TStringList;
  Bytes: TBytes;
  I: Integer;
begin
  WriteLn;

  // String value
  GrijjyLog.Send('String value', 'Foo');

  // Floating-point value
  GrijjyLog.Send('Float value', Pi);

  // Integer value
  GrijjyLog.Send('Integer value', 42);

  // TStrings value
  SL := TStringList.Create;
  try
    SL.Add('Foo');
    SL.Add('With Spaces');
    SL.Add('With, Commas');
    SL.Add('With "Quotes"');
    SL.Add('With ''Quotes''');
    SL.Add('Width , "every", ''thing''');
    GrijjyLog.Send('TStrings value', SL, TgoLogLevel.Warning);

    // All fields and properties of the stringlist
    GrijjyLog.Send('Object value', SL, mvPrivate, 4);
  finally
    SL.Free;
  end;

  // TBytes value
  Bytes := TEncoding.UTF8.GetBytes('The Quick Brown Fox Jumps Over The Lazy Dog');
  GrijjyLog.Send('TBytes value', Bytes);

  // Raw memory
  SetLength(Bytes, 997);
  for I := 0 to Length(Bytes) - 1 do
    Bytes[I] := Random(256);
  Bytes[10] := 0;
  GrijjyLog.Send('Memory value', @Bytes[0], Length(Bytes));

  WriteLn('Sent data of different types to the Grijjy Log Viewer');
end;

procedure TExamples.LogLiveWatches;
var
  S: String;
begin
  WriteLn;
  WriteLn('The Grijjy Log Viewer can show "Live Watches" of data in your app.');
  WriteLn('This sample app adds a watch with the number of seconds that have');
  WriteLn('elapsed since the app started. You can also customize another watch below.');;
  WriteLn;
  WriteLn('In the Log Viewer, make sure you have "View | Live Watches" enabled.');
  WriteLn('You can then get a snapshot of live watches by clicking the "Update"');
  WriteLn('button in the "Watches" panel. Or check "Auto-update" to refresh the');
  WriteLn('watches every second.');

  while True do
  begin
    WriteLn;
    WriteLn('Enter any custom watch value you want to add, or just press [Enter]');
    Write('to return to the menu: ');
    ReadLn(S);
    if (S.Trim = '') then
      Break
    else
      FCustomWatchValue := S;
  end;
end;

procedure TExamples.LogOther;
var
  I: Integer;
begin
  WriteLn;
  WriteLn('Logging 10,000 messages...');

  for I := 0 to 9999 do
    GrijjyLog.Send('Message ' + I.ToString, TgoLogLevel(I mod 3));

  WriteLn('Finished logging messages.');
end;

procedure TExamples.LogTracking;
var
  S: String;
  I: Integer;
begin
  WriteLn;
  WriteLn('You can allocate and deallocate memory and object here, ');
  WriteLn('and watch for changes in the Grijjy Log Viewer.');
  WriteLn;
  WriteLn('In the Log Viewer, make sure you have "View | Live Watches" enabled.');
  WriteLn('You can then get a snapshot of live objects and memory by clicking');
  WriteLn('the "Update" button in the "Memory Usage" panel.');
  WriteLn;
  WriteLn('In particular, look for changes in instance counts for the TSampleFoo');
  WriteLn('and TSampleBar classes.');

  while True do
  begin
    WriteLn;
    WriteLn('Choose action to perform:');
    WriteLn('1) Allocate some objects');
    if (FObjects.Count > 0) then
      WriteLn('2) Deallocate some objects');
    WriteLn('3) Allocate 10MB');
    if (FMemoryBlocks <> nil) then
      WriteLn('4) Deallocate 10MB');
    WriteLn('5) Back to menu');

    Write('Enter option (1-5): ');
    ReadLn(S);
    I := StrToIntDef(S, 0);
    case I of
      1: TrackAllocateObjects;
      2: TrackDeallocateObjects;
      3: TrackAllocateMemory;
      4: TrackDeallocateMemory;
      5: Exit;
    else
      WriteLn('Invalid option number entered');
    end;
  end;
end;

function TExamples.MainMenu: Boolean;
var
  S: String;
  I: Integer;
begin
  while True do
  begin
    WriteLn;
    WriteLn('Choose example to run:');
    WriteLn('1) Basic logging');
    WriteLn('2) Logging different data types');
    WriteLn('3) Track memory usage and object allocations');
    WriteLn('4) Live Watches');
    WriteLn('5) Other examples');
    WriteLn('6) Change Connection');
    WriteLn('7) Exit');
    Write('Enter option (1-7): ');
    ReadLn(S);
    I := StrToIntDef(S.Trim, 0);
    if (I < 1) or (I > 7) then
      WriteLn('Invalid example number entered')
    else
    begin
      case I of
        1: LogBasic;
        2: LogDataTypes;
        3: LogTracking;
        4: LogLiveWatches;
        5: LogOther;
        6: Connect;
        7: Exit(False);
      end;
      Exit(True);
    end;
  end;
end;

procedure TExamples.Run;
begin
  WriteLn('Grijjy Example Logging Client - Console Version');
  WriteLn('===============================================');
  Connect;

  while (MainMenu) do ;
end;

procedure TExamples.TrackAllocateMemory;
var
  Block: TBytes;
begin
  SetLength(Block, 10 * 1024 * 1024);
  FMemoryBlocks := FMemoryBlocks + [Block];
end;

procedure TExamples.TrackAllocateObjects;
var
  I, Count: Integer;
  Obj: TObject;
begin
  if (FObjects.Count = 0) then
  begin
    FObjects.Add(TSampleFoo.Create);
    FObjects.Add(TSampleBar.Create);
  end;

  Count := Random(10) + 2;
  for I := 0 to Count - 1 do
  begin
    if (Random(2) = 0) then
      Obj := TSampleFoo.Create
    else
      Obj := TSampleBar.Create;
    FObjects.Add(Obj)
  end;
end;

procedure TExamples.TrackDeallocateMemory;
begin
  if (Length(FMemoryBlocks) > 0) then
    SetLength(FMemoryBlocks, Length(FMemoryBlocks) - 1);
end;

procedure TExamples.TrackDeallocateObjects;
var
  Count: Integer;
begin
  Count := FObjects.Count div 2;
  if (Count = 0) then
    Count := FObjects.Count;

  while (Count > 0) do
  begin
    FObjects.Delete(0);
    Dec(Count);
  end;
end;

end.

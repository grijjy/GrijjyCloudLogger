unit Processes;

{$INCLUDE 'Grijjy.inc'}

interface

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Defaults,
  System.Generics.Collections,
  Grijjy.CloudLogging.Protocol,
  LogMessages,
  Usage,
  PascalZMQ,
  ZMQ.WorkerProtocol;

const
  { The maximum number of messages that will be shown per process. Once the
    number of messages exceeds this threshold, the oldest messages will be
    removed. }
  MAX_MESSAGES_PER_PROCESS = 10000;

type
  TgoProcess = class;

  { Implement this to listen to certain events from TgoProcess }
  IgoProcessListener = interface
  ['{8E55B113-80A9-439B-BEF9-64C5DE86CD22}']
    { Called when memory usage information for the process is retrieved.

      Parameters:
        AProcess: the process for which memory usage is available. Call its
          UpdateMemoryUsage method to request this information through the
          ClassUsage and TotalObjects properties. }
    procedure MemoryUsageAvailable(const AProcess: TgoProcess);

    { Called when a list of live watches for the process is retrieved.

      Parameters:
        AProcess: the process for which the watches are available. Call its
          GetLiveWatches method to request the watches. }
    procedure LiveWatchesAvailable(const AProcess: TgoProcess);
  end;

  { Represents all (logging) information associated with a single process }
  TgoProcess = class(TObject)
  {$REGION 'Internal Declarations'}
  private class var
    FInstanceComparer: IComparer<TgoInstanceUsageItem>;
  private
    { These fields are accessed from the ZMQ communication thread: }
    FProcessId: Cardinal;
    FAppName: String;
    FRoutingFrame: PZFrame;
    FNewMessages: TgoLogMessages;
    FNewMessagesLock: TCriticalSection;
    FFirstMessageTime: TDateTime;
    FCurrentIndent: Integer;
    FLogProtocol: TZMQWorkerProtocol;
    FMemoryUsage: TBytes;
    FMemoryUsageLock: TCriticalSection;
    FLiveWatches: TBytes;
    FLiveWatchesLock: TCriticalSection;
    FListener: IgoProcessListener;
  private
    { These fields may only be modified from the main UI thread: }
    FMessages: TgoLogMessages;
    FSelectedMessageIndex: Integer;
    FSelectedTabIndex: Integer;
    FClassUsage: TStringList;
    FTotalObjects: TgoClassUsageItem;
  private
    procedure SendCommand(const ACommand: Integer;
      const AArgs: TBytes = nil);
  {$ENDREGION 'Internal Declarations'}
  public
    (*********************************************
     * General
     *********************************************)

    constructor Create(const AProcessId: Cardinal; const AAppName: String;
      const ALogProtocol: TZMQWorkerProtocol; const ARoutingFrame: PZFrame);
    destructor Destroy; override;

    { The ID of the process }
    property ProcessId: Cardinal read FProcessId;

    { The name of the process/application }
    property AppName: String read FAppName;

    { The routing frame used to send messages to this process }
    property RoutingFrame: PZFrame read FRoutingFrame;

    { The listener that is notified of certain process events }
    property Listener: IgoProcessListener read FListener write FListener;

    { Index of the currently selected tab ("Messages" or "Circular References") }
    property SelectedTabIndex: Integer read FSelectedTabIndex write FSelectedTabIndex;
  public
    (*********************************************
     * Log Messages
     *********************************************)

    { Adds a log message for this process.
      This process becomes owner of the message. }
    procedure AddLogMessage(const ALogMessage: TgoLogMessage);

    { Checks whether new messages have been added since the last call of
      HasNewMessages. If so, the Messages list will be updated with the new
      messages. Sets AOldMessagesPurged to True if the maximum message limit
      was reach and some oldest messages got deleted. }
    function HasNewMessages(out AOldMessagesPurged: Boolean): Boolean;

    { Clears the list of messages }
    procedure ClearLogMessages;

    { The log messages for this process. This list is only updated after
      you call HasNewMessages.
      NOTE: It is safe to access this property from the UI thread. }
    property Messages: TgoLogMessages read FMessages;

    { The time of the first message }
    property FirstMessageTime: TDateTime read FFirstMessageTime;

    { Index of the currently selected message }
    property SelectedMessageIndex: Integer read FSelectedMessageIndex write FSelectedMessageIndex;
  public
    (*********************************************
     * Memory Usage
     *********************************************)

    { Sends a message to the client to request its memory usage.

      Parameters:
        AClasses: (optional) array of class handles for which to receive
          details (individual instances) }
    procedure RequestMemoryUsage(const AClasses: TArray<THandle> = nil);

    { Sets the memory usage of this process to the data retrieved from a
      RequestMemoryUsage request. }
    procedure SetMemoryUsage(const AData: TBytes);

    { Updates the ClassUsage and TotalObjects properties with the latest
      information. Call this in response to the
      IgoProcessListener.MemoryUsageAvailable event.

      Returns:
        Number of items in the ClassUsage list. }
    function UpdateMemoryUsage: Integer;

    { The current object usage for this process. This list is only updated
      after you call UpdateMemoryUsage.

      Each string in the list is the name of the class, and its associated
      object is an instance of TgoClassUsageItem.

      The list is sorted by class name. }
    property ClassUsage: TStringList read FClassUsage;

    { The object usage item that represents the totals of all items in the
      ClassUsage list. This item is only updated after you call
      UpdateMemoryUsage. }
    property TotalObjects: TgoClassUsageItem read FTotalObjects;
  public
    (*********************************************
     * Live Watches
     *********************************************)

    { Sends a message to the client to request a list of live watches }
    procedure RequestLiveWatches;

    { Sets the live watches of this process to the data retrieved
      from a RequestLiveWatches request. }
    procedure SetLiveWatches(const AData: TBytes);

    { Gets the list of live watches for this process. Call this in
      response to the IgoProcessListener.LiveWatchesAvailable event.

      Returns:
        The the live watches. }
    function GetLiveWatches: TArray<TgoLiveWatch>;
  end;

implementation

uses
  System.Math,
  Winapi.Windows,
  Grijjy.ProtocolBuffers,
  ZMQ.Shared;

type
  TInstanceComparer = class(TInterfacedObject, IComparer<TgoInstanceUsageItem>)
  public
    { IComparer<TgoInstanceUsageItem> }
    function Compare(const Left, Right: TgoInstanceUsageItem): Integer;
  end;

{ TInstanceComparer }

function TInstanceComparer.Compare(const Left,
  Right: TgoInstanceUsageItem): Integer;
begin
  Result := CompareText(Left.Caption, Right.Caption);
end;

{ TgoProcess }

procedure TgoProcess.AddLogMessage(const ALogMessage: TgoLogMessage);
begin
  { This method is called from the ZMQ thread. We store the message in the
    FNewMessages list until the UI requests an update. }
  Assert(Assigned(ALogMessage));
  if (ALogMessage.Level = TgoLogLevelEx.ExitMethod) and (FCurrentIndent > 0) then
    Dec(FCurrentIndent);
  ALogMessage.Indent := FCurrentIndent;
  if (ALogMessage.Level = TgoLogLevelEx.EnterMethod) then
    Inc(FCurrentIndent);

  FNewMessagesLock.Enter;
  try
    FNewMessages.Add(ALogMessage)
  finally
    FNewMessagesLock.Leave;
  end;
end;

procedure TgoProcess.ClearLogMessages;
begin
  FNewMessagesLock.Enter;
  try
    FNewMessages.Clear;
    FMessages.Clear;
  finally
    FNewMessagesLock.Leave;
  end;
end;

constructor TgoProcess.Create(const AProcessId: Cardinal;
  const AAppName: String; const ALogProtocol: TZMQWorkerProtocol;
  const ARoutingFrame: PZFrame);
begin
  Assert(Assigned(ALogProtocol));
  Assert(Assigned(ARoutingFrame));
  inherited Create;
  FProcessId := AProcessId;
  FAppName := AAppName;
  FLogProtocol := ALogProtocol;
  FRoutingFrame := ARoutingFrame.Clone;
  FMessages := TgoLogMessages.Create;
  FNewMessages := TgoLogMessages.Create;
  FNewMessagesLock := TCriticalSection.Create;
  FSelectedMessageIndex := -1;
  FMemoryUsageLock := TCriticalSection.Create;
  FClassUsage := TStringList.Create;
  FClassUsage.Duplicates := dupError;
  FClassUsage.Sorted := True;
  FClassUsage.OwnsObjects := True;
  FTotalObjects := TgoClassUsageItem.Create('[Total objects]', 0);
  FLiveWatchesLock := TCriticalSection.Create;

  if (FInstanceComparer = nil) then
    FInstanceComparer := TInstanceComparer.Create;
end;

destructor TgoProcess.Destroy;
begin
  FListener := nil;
  FRoutingFrame.Free;
  FLiveWatchesLock.Free;
  FTotalObjects.Free;
  FClassUsage.Free;
  FMemoryUsageLock.Free;
  FNewMessagesLock.Free;
  FNewMessages.Free;
  FMessages.Free;
  inherited;
end;

function TgoProcess.GetLiveWatches: TArray<TgoLiveWatch>;
var
  Protocol: TgoLogLiveWatchesProtocol;
begin
  FLiveWatchesLock.Enter;
  try
    TgoProtocolBuffer.Deserialize(Protocol, FLiveWatches);
    Result := Protocol.Watches;
  finally
    FLiveWatchesLock.Leave;
  end;
end;

function TgoProcess.HasNewMessages(out AOldMessagesPurged: Boolean): Boolean;
begin
  { This method is (usually) called from the UI thread. Check for new messages
    and move these over to the main list, so the UI can safely access them
    while the ZMQ thread keeps adding messages. }
  AOldMessagesPurged := False;
  FNewMessagesLock.Enter;
  try
    Result := (FNewMessages.Count > 0);
    if (Result) then
    begin
      if (FMessages.Count = 0) then
        FFirstMessageTime := FNewMessages[0].TimeStamp;

      FMessages.AddRange(FNewMessages);

      { We can clear the FNewMessages list now, but that should NOT free its
        objects. }
      FNewMessages.OwnsObjects := False;
      try
        FNewMessages.Clear;
      finally
        FNewMessages.OwnsObjects := True;
      end;

      { If the number of messages exceeds the threshold, we remove 10% of the
        oldest messages. }
      if (FMessages.Count > MAX_MESSAGES_PER_PROCESS) then
      begin
        FMessages.DeleteRange(0, Max(MAX_MESSAGES_PER_PROCESS div 10, FMessages.Count - MAX_MESSAGES_PER_PROCESS));
        AOldMessagesPurged := True;
      end;
    end;
  finally
    FNewMessagesLock.Leave;
  end;
end;

procedure TgoProcess.RequestMemoryUsage(const AClasses: TArray<THandle> = nil);
var
  Args: TBytes;
  Handles: TgoHandleArray;
begin
  Args := nil;
  if Assigned(AClasses) then
  begin
    Handles.Handles := AClasses;
    Args := TgoProtocolBuffer.Serialize(Handles);
  end;
  SendCommand(LOG_FORMAT_MEMORY_USAGE, Args);
end;

procedure TgoProcess.RequestLiveWatches;
begin
  SendCommand(LOG_FORMAT_LIVE_WATCHES);
end;

procedure TgoProcess.SendCommand(const ACommand: Integer; const AArgs: TBytes);
var
  Protocol: TgoLogCommandProtocol;
  Msg: PZMessage;
  RoutingFrame: PZFrame;
begin
  Assert(Assigned(FLogProtocol));
  Protocol.Command := ACommand;
  Protocol.Args := AArgs;
  Msg := TZMessage.Create;
  try
    Msg.PushProtocolBuffer(Protocol);
    RoutingFrame := FRoutingFrame.Clone;
    FLogProtocol.Send(Msg, RoutingFrame);
  finally
    Msg.Free;
  end;
end;

procedure TgoProcess.SetMemoryUsage(const AData: TBytes);
begin
  { This method is called from the ZMQ thread. }
  FMemoryUsageLock.Enter;
  try
    FMemoryUsage := AData;
  finally
    FMemoryUsageLock.Leave;
  end;

  TThread.Synchronize(nil,
    procedure
    begin
      if Assigned(FListener) then
        FListener.MemoryUsageAvailable(Self);
    end);
end;

procedure TgoProcess.SetLiveWatches(const AData: TBytes);
begin
  { This method is called from the ZMQ thread. }
  FLiveWatchesLock.Enter;
  try
    FLiveWatches := AData;
  finally
    FLiveWatchesLock.Leave;
  end;

  TThread.Synchronize(nil,
    procedure
    begin
      if Assigned(FListener) then
        FListener.LiveWatchesAvailable(Self);
    end);
end;

function TgoProcess.UpdateMemoryUsage: Integer;
const
  PSEUDO_ALLOCATED_MEMORY = '@Allocated Memory';
var
  Protocol: TgoLogMemoryUsageProtocol;
  Entry: TgoLogMemoryUsageProtocol.PEntry;
  I, J, Count, Total: Integer;
  UsageItem: TgoClassUsageItem;
  Instances: TArray<TgoInstanceUsageItem>;
  Name: String;
  Now: Cardinal;
begin
  FMemoryUsageLock.Enter;
  try
    TgoProtocolBuffer.Deserialize(Protocol, FMemoryUsage);
  finally
    FMemoryUsageLock.Leave;
  end;

  Total := 0;

  { Mark all previous items as unchecked (unused) }
  for I := 0 to FClassUsage.Count - 1 do
  begin
    UsageItem := TgoClassUsageItem(FClassUsage.Objects[I]);
    UsageItem.Checked := False;
  end;

  { Create pseudo usage items }
  J := FClassUsage.IndexOf(PSEUDO_ALLOCATED_MEMORY);
  if (J < 0) then
  begin
    UsageItem := TgoClassUsageItem.Create(PSEUDO_ALLOCATED_MEMORY, 0);
    FClassUsage.AddObject(PSEUDO_ALLOCATED_MEMORY, UsageItem)
  end
  else
    UsageItem := TgoClassUsageItem(FClassUsage.Objects[J]);
  UsageItem.Count := Protocol.AllocatedBytes;
  UsageItem.Checked := True;

  { Walk through all new items and update and check them as used }
  for I := 0 to Length(Protocol.Entries) - 1 do
  begin
    Entry := @Protocol.Entries[I];
    Name := Entry.ClassName;
    Count := Entry.InstanceCount;

    J := FClassUsage.IndexOf(Name);
    if (J < 0) then
    begin
      UsageItem := TgoClassUsageItem.Create(Name, Entry.ClassHandle);
      FClassUsage.AddObject(Name, UsageItem)
    end
    else
      UsageItem := TgoClassUsageItem(FClassUsage.Objects[J]);

    UsageItem.Count := Count;
    UsageItem.Checked := True;

    if Assigned(Entry.Instances) then
    begin
      SetLength(Instances, Length(Entry.Instances));
      for J := 0 to Length(Instances) - 1 do
        Instances[J] := TgoInstanceUsageItem.Create(Entry.Instances[J].Caption);
      TArray.Sort<TgoInstanceUsageItem>(Instances, FInstanceComparer);
      UsageItem.Instances := Instances;
    end
    else
      UsageItem.Instances := nil;

    Inc(Total, Count);
  end;

  FTotalObjects.Count := Total;

  { Remove items that have not have any instances for 10 seconds }
  Now := GetTickCount;
  for I := FClassUsage.Count - 1 downto 0 do
  begin
    UsageItem := TgoClassUsageItem(FClassUsage.Objects[I]);
    if (not UsageItem.Checked) then
    begin
      UsageItem.Count := 0;
      UsageItem.Instances := nil;
    end;

    if (UsageItem.ZeroTime <> 0) and (((Now - UsageItem.ZeroTime) >= 10000) or (Now < UsageItem.ZeroTime)) then
      FClassUsage.Delete(I);
  end;

  Result := FClassUsage.Count;
end;

end.

unit LogProtocol;

{$INCLUDE 'Grijjy.inc'}

interface

uses
  System.SyncObjs,
  System.Generics.Collections,
  ZMQ.WorkerProtocol,
  ZMQ.Shared,
  PascalZMQ,
  Processes;

type
  { Implement this to listen to certain events from TgoLogProtocol }
  IgoLogProtocolListener = interface
  ['{2FC80273-1EAF-4125-B8B3-2E5EA065F114}']
    { Called when the log protocol receives a message from a new process }
    procedure ProcessAdded(const AProcess: TgoProcess);
  end;

type
  { Our ZMQ worker protocol for logging }
  TgoLogProtocol = class(TZMQWorkerProtocol)
  {$REGION 'Internal Declarations'}
  private
    FListener: IgoLogProtocolListener;
    FProcesses: TObjectDictionary<Cardinal, TgoProcess>;
    FProcessesLock: TCriticalSection;
  protected
    procedure DoRecv(const ACommand: TZMQCommand; var AMsg: PZMessage;
      var ASentFrom: PZFrame); override;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const AListener: IgoLogProtocolListener);
    destructor Destroy; override;

    { Removes the given process }
    procedure RemoveProcess(const AProcess: TgoProcess);

    { Removes all processes }
    procedure RemoveAllProcesses;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  Grijjy.CloudLogging.Protocol,
  Grijjy.ProtocolBuffers,
  LogMessages;

{ TgoLogProtocol }

constructor TgoLogProtocol.Create(const AListener: IgoLogProtocolListener);
begin
  Assert(Assigned(AListener));
  inherited Create(True, True);
  FListener := AListener;
  FProcesses := TObjectDictionary<Cardinal, TgoProcess>.Create([doOwnsValues]);
  FProcessesLock := TCriticalSection.Create;
end;

destructor TgoLogProtocol.Destroy;
begin
  inherited;
  FProcessesLock.Free;
  FProcesses.Free;
end;

procedure TgoLogProtocol.DoRecv(const ACommand: TZMQCommand; var AMsg: PZMessage;
  var ASentFrom: PZFrame);
var
  Protocol: TgoLogMessageProtocol;
  IsNewProcess: Boolean;
  Process: TgoProcess;
  LogMsg: TgoLogMessage;
begin
  AMsg.PopProtocolBuffer(Protocol);

  { Keep track of each process. }
  FProcessesLock.Enter;
  try
    IsNewProcess := (not FProcesses.TryGetValue(Protocol.ProcessId, Process));
    if (IsNewProcess) then
    begin
      Process := TgoProcess.Create(Protocol.ProcessId, Protocol.AppName, Self, ASentFrom);
      FProcesses.Add(Protocol.ProcessId, Process);
    end;
  finally
    FProcessesLock.Leave;
  end;

  if (Protocol.DataFormat < 0) then
  begin
    { Reponse from request to client }
    case Protocol.DataFormat of
      LOG_FORMAT_MEMORY_USAGE:
        Process.SetMemoryUsage(Protocol.Data);

      LOG_FORMAT_LIVE_WATCHES:
        Process.SetLiveWatches(Protocol.Data);
    end;
  end
  else
  begin
    { "Regular" log message }
    LogMsg := TgoLogMessage.Create(Protocol.Level, Protocol.MessageText,
      Protocol.TimeStamp, Protocol.ProcessId, Protocol.ThreadId,
      Protocol.DataFormat, Protocol.Data);
    Process.AddLogMessage(LogMsg);
  end;

  if (IsNewProcess) then
    TThread.Synchronize(nil,
      procedure
      begin
        FListener.ProcessAdded(Process);
      end);

  inherited;
end;

procedure TgoLogProtocol.RemoveAllProcesses;
begin
  FProcessesLock.Enter;
  try
    FProcesses.Clear;
  finally
    FProcessesLock.Leave;
  end;
end;

procedure TgoLogProtocol.RemoveProcess(const AProcess: TgoProcess);
begin
  if (AProcess = nil) then
    Exit;

  FProcessesLock.Enter;
  try
    FProcesses.Remove(AProcess.ProcessId);
  finally
    FProcessesLock.Leave;
  end;
end;

end.

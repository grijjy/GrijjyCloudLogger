program GrijjyLogBroker;
{ Logging broker using the ZMQ Broker Protocol class }

{$I Grijjy.inc}

{$APPTYPE CONSOLE}

uses
  Grijjy.Console,
  System.SysUtils,
  System.Messaging,
  ZMQ.Shared,
  ZMQ.BrokerProtocol,
  PascalZMQ;

type
  TGrijjyBroker = class(TZMQBrokerProtocol)
  private
    procedure LogMessageListener(const Sender: TObject; const M: TMessage);
  private
    procedure DoRecvFromClient(const AService: String; const ASentFromId: String;
      const ASentFrom: PZFrame; var AMsg: PZMessage; var AAction: TZMQAction; var ASendToId: String); override;
    procedure DoRecvFromWorker(const AService: String; const ASentFromId: String;
      const ASentFrom: PZFrame; var AMsg: PZMessage; var AAction: TZMQAction); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  Broker: TGrijjyBroker;

constructor TGrijjyBroker.Create;
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TZMQLogMessage, LogMessageListener);
end;

destructor TGrijjyBroker.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TZMQLogMessage, LogMessageListener);
  inherited;
end;

{ Out internal messages to the console }
procedure TGrijjyBroker.LogMessageListener(const Sender: TObject;
  const M: TMessage);
var
  LogMessage: TZMQLogMessage;
begin
  LogMessage := M as TZMQLogMessage;
  Writeln(LogMessage.Text);
end;

{ This event is fired whenever a new message is received from a client }
procedure TGrijjyBroker.DoRecvFromClient(const AService: String; const ASentFromId: String;
  const ASentFrom: PZFrame; var AMsg: PZMessage; var AAction: TZMQAction; var ASendToId: String);
begin
  AAction := TZMQAction.Forward;
end;

{ This event is fired whenever a new message is received from a worker }
procedure TGrijjyBroker.DoRecvFromWorker(const AService: String; const ASentFromId: String;
  const ASentFrom: PZFrame; var AMsg: PZMessage; var AAction: TZMQAction);
begin
  AAction := TZMQAction.Forward;
end;

begin
  try
    Broker := TGrijjyBroker.Create;
    try
      if Broker.Bind('tcp://*:7337') then
        WaitForCtrlC;
    finally
      Broker.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

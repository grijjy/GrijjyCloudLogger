unit LogMessages;

{$INCLUDE 'Grijjy.inc'}

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TgoLogLevelEx = (Info, Warning, Error, EnterMethod, ExitMethod);

type
  { Represents a single log message }
  TgoLogMessage = class(TObject)
  {$REGION 'Internal Declarations'}
  private
    FLevel: TgoLogLevelEx;
    FMessage: String;
    FDataFormat: Integer;
    FData: TBytes;
    FTimeStamp: TDateTime;
    FProcessId: Cardinal;
    FThreadId: Cardinal;
    FIndent: Integer;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const ALevel: Integer; const AMessage: String;
      const ATimeStamp: TDateTime; const AProcessId, AThreadId: Cardinal;
      const ADataFormat: Integer; const AData: TBytes);

    { Log level }
    property Level: TgoLogLevelEx read FLevel;

    { Message text }
    property Message: String read FMessage;

    { Format of the optional data in the Data property. }
    property DataFormat: Integer read FDataFormat;

    { Optional data. The format of this data depends on DataFormat:
      * LOG_FORMAT_NONE: should be nil.
      * LOG_FORMAT_TSTRINGS: contains a TStrings.CommaText values, encoded as UTF-8.
      * LOG_FORMAT_MEMORY: contains raw binary data.
      * LOG_FORMAT_OBJECT: contains the fields and properties of an object,
        serialized to JSON format and compressed with ZLib.

      Otherwise, its contents is user defined. }
    property Data: TBytes read FData;

    { The time the message was sent. }
    property TimeStamp: TDateTime read FTimeStamp;

    { ID of the process that send the message }
    property ProcessId: Cardinal read FProcessId;

    { ID of the thread that send the message }
    property ThreadId: Cardinal read FThreadId;

    { Indentation level. Used when message is inside EnterMethod/ExitMethod
      block }
    property Indent: Integer read FIndent write FIndent;
  end;

type
  TgoLogMessages = TObjectList<TgoLogMessage>;

implementation

{ TgoLogMessage }

constructor TgoLogMessage.Create(const ALevel: Integer;
  const AMessage: String; const ATimeStamp: TDateTime; const AProcessId,
  AThreadId: Cardinal; const ADataFormat: Integer; const AData: TBytes);
begin
  inherited Create;
  FLevel := TgoLogLevelEx(ALevel);
  FMessage := AMessage;
  FTimeStamp := ATimeStamp;
  FProcessId := AProcessId;
  FThreadId := AThreadId;
  FDataFormat := ADataFormat;
  FData := AData;
end;
end.

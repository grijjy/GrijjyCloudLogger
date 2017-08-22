unit Frame.DataTypes;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TFrameDataTypes = class(TFrame)
    ButtonObject: TButton;
    ButtonMemory: TButton;
    ButtonTBytes: TButton;
    ButtonTStrings: TButton;
    ButtonFloat: TButton;
    ButtonInteger: TButton;
    ButtonString: TButton;
    procedure ButtonStringClick(Sender: TObject);
    procedure ButtonIntegerClick(Sender: TObject);
    procedure ButtonFloatClick(Sender: TObject);
    procedure ButtonTStringsClick(Sender: TObject);
    procedure ButtonTBytesClick(Sender: TObject);
    procedure ButtonMemoryClick(Sender: TObject);
    procedure ButtonObjectClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  System.TypInfo,
  Grijjy.SysUtils,
  Grijjy.CloudLogging;

procedure TFrameDataTypes.ButtonFloatClick(Sender: TObject);
begin
  GrijjyLog.Send('Float value', Pi, TgoLogLevel.Warning);
end;

procedure TFrameDataTypes.ButtonIntegerClick(Sender: TObject);
begin
  GrijjyLog.Send('Integer value', 42, TgoLogLevel.Warning);
end;

procedure TFrameDataTypes.ButtonMemoryClick(Sender: TObject);
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

procedure TFrameDataTypes.ButtonObjectClick(Sender: TObject);
begin
  GrijjyLog.Send('Object value', Self, mvPublic, 4, TgoLogLevel.Warning);
end;

procedure TFrameDataTypes.ButtonStringClick(Sender: TObject);
begin
  GrijjyLog.Send('String value', 'Foo', TgoLogLevel.Warning);
end;

procedure TFrameDataTypes.ButtonTBytesClick(Sender: TObject);
var
  Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes
    ('The Quick Brown Fox Jumps Over The Lazy Dog');
  GrijjyLog.Send('TBytes value', Bytes, TgoLogLevel.Warning);
end;

procedure TFrameDataTypes.ButtonTStringsClick(Sender: TObject);
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

end.

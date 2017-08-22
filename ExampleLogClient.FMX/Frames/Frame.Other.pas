unit Frame.Other;

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
  TFrameOther = class(TFrame)
    Button10000: TButton;
    procedure Button10000Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  Grijjy.CloudLogging;

procedure TFrameOther.Button10000Click(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to 9999 do
    GrijjyLog.Send('Message ' + I.ToString, TgoLogLevel(I mod 3));
end;

end.

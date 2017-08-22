unit Frame.Basic;

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
  TFrameBasic = class(TFrame)
    ButtonError: TButton;
    ButtonInfo: TButton;
    ButtonEnterExitMethod: TButton;
    ButtonWarning: TButton;
    procedure ButtonInfoClick(Sender: TObject);
    procedure ButtonWarningClick(Sender: TObject);
    procedure ButtonErrorClick(Sender: TObject);
    procedure ButtonEnterExitMethodClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  Grijjy.CloudLogging,
  SampleClasses;

procedure TFrameBasic.ButtonErrorClick(Sender: TObject);
begin
  GrijjyLog.Send('Sample Error Message', TgoLogLevel.Error);
end;

procedure TFrameBasic.ButtonInfoClick(Sender: TObject);
begin
  GrijjyLog.Send('Sample Info Message', TgoLogLevel.Info);
end;

procedure TFrameBasic.ButtonEnterExitMethodClick(Sender: TObject);
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

procedure TFrameBasic.ButtonWarningClick(Sender: TObject);
begin
  GrijjyLog.Send('Sample Warning Message', TgoLogLevel.Warning);
end;

end.

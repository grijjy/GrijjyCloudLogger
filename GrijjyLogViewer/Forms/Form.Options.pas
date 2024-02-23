unit Form.Options;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Settings;

type
  TFormOptions = class(TForm)
    PanelButtons: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    RadioGroupTimeColumn: TRadioGroup;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FSettings: TgoSettings;
  public
    { Public declarations }
    constructor Create(const ASettings: TgoSettings); reintroduce;
    class function Execute(const ASettings: TgoSettings): Boolean; static;
  end;

implementation

{$R *.dfm}

{ TFormOptions }

constructor TFormOptions.Create(const ASettings: TgoSettings);
begin
  Assert(Assigned(ASettings));
  inherited Create(Application);
  FSettings := ASettings;
  RadioGroupTimeColumn.ItemIndex := Ord(FSettings.TimeDisplay);
end;

class function TFormOptions.Execute(const ASettings: TgoSettings): Boolean;
var
  Form: TFormOptions;
begin
  Form := TFormOptions.Create(ASettings);
  try
    Result := (Form.ShowModal = mrOk);
  finally
    Form.Release;
  end;
end;

procedure TFormOptions.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (ModalResult = mrOk) then
    FSettings.TimeDisplay := TgoTimeDisplay(RadioGroupTimeColumn.ItemIndex);
end;

end.

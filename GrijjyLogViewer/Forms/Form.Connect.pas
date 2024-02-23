unit Form.Connect;

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
  TFormConnect = class(TForm)
    GroupBoxSettings: TGroupBox;
    LabelDispatcher: TLabel;
    EditDispatcher: TEdit;
    LabelService: TLabel;
    EditService: TEdit;
    PanelButtons: TPanel;
    ButtonConnect: TButton;
    ButtonCancel: TButton;
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

{ TFormConnect }

constructor TFormConnect.Create(const ASettings: TgoSettings);
begin
  Assert(Assigned(ASettings));
  inherited Create(Application);
  FSettings := ASettings;
  EditDispatcher.Text := ASettings.Dispatcher;
  EditService.Text := ASettings.Service;
end;

class function TFormConnect.Execute(const ASettings: TgoSettings): Boolean;
var
  Form: TFormConnect;
begin
  Form := TFormConnect.Create(ASettings);
  try
    Result := (Form.ShowModal = mrOk);
  finally
    Form.Release;
  end;
end;

procedure TFormConnect.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (ModalResult = mrOk) then
  begin
    FSettings.Dispatcher := EditDispatcher.Text;
    FSettings.Service := EditService.Text;
  end;
end;

end.

unit Form.Main;

{$I 'Grijjy.inc'}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,
  System.SyncObjs,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.ImgList,
  Vcl.Menus,
  Vcl.ActnList,
  Vcl.ToolWin,
  Vcl.Tabs,
  Frame.Messages,
  Frame.Memory,
  Frame.Watches,
  LogProtocol,
  Processes,
  Settings;

type
  TFormMain = class(TForm, IgoLogProtocolListener, IgoProcessListener)
    MainMenu: TMainMenu;
    ActionList: TActionList;
    ActionConnect: TAction;
    ActionExit: TAction;
    MenuFile: TMenuItem;
    MenuConnect1: TMenuItem;
    N1: TMenuItem;
    MenuExit: TMenuItem;
    ToolBar: TToolBar;
    ToolButtonConnect: TToolButton;
    ActionClear: TAction;
    MenuEdit: TMenuItem;
    MenuClear: TMenuItem;
    ToolButtonClear: TToolButton;
    ActionOptions: TAction;
    OptionsMenu: TMenuItem;
    ToolButtonOptions: TToolButton;
    TabControlProcesses: TTabControl;
    PopupMenuTabs: TPopupMenu;
    ActionCloseTab: TAction;
    MenuCloseTab: TMenuItem;
    ActionReset: TAction;
    MenuReset: TMenuItem;
    ToolButtonReset: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    PanelMemoryAndWatches: TPanel;
    SplitterMain: TSplitter;
    PanelWatches: TPanel;
    SplitterMemoryAndWatches: TSplitter;
    PanelMain: TPanel;
    ToolButtonWatches: TToolButton;
    ToolButton4: TToolButton;
    ActionViewWatches: TAction;
    MenuView: TMenuItem;
    MenuViewWatches: TMenuItem;
    ImageListMain: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionConnectExecute(Sender: TObject);
    procedure ActionClearExecute(Sender: TObject);
    procedure ActionOptionsExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TabControlProcessesChange(Sender: TObject);
    procedure TabControlProcessesMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ActionCloseTabExecute(Sender: TObject);
    procedure ActionResetExecute(Sender: TObject);
    procedure ActionViewWatchesExecute(Sender: TObject);
  private
    { IgoLogProtocolListener }
    procedure ProcessAdded(const AProcess: TgoProcess);
  private
    { IgoProcessListener }
    procedure MemoryUsageAvailable(const AProcess: TgoProcess);
    procedure LiveWatchesAvailable(const AProcess: TgoProcess);
  private
    FLogProtocol: TgoLogProtocol;
    FSettings: TgoSettings;
    FCurrentProcess: TgoProcess;
    FFrameMessages: TFrameMessages;
    FFrameMemory: TFrameMemory;
    FFrameWatches: TFrameWatches;
    procedure Connect;
    procedure ApplySettings;
    procedure SetCurrentProcess(const AProcess: TgoProcess);
    procedure LoadLayout;
    procedure SaveLayout;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  PascalZMQ,
  Form.Connect,
  Form.Options;

{ TFormMain }

procedure TFormMain.Connect;
begin
  if (FSettings.Dispatcher <> '') then
    FLogProtocol.Connect(FSettings.Dispatcher, '', TZSocketType.Dealer, FSettings.Service);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  FFrameMessages := TFrameMessages.Create(Self);
  FFrameMessages.ButtonSave.Images := ImageListMain;
  FFrameMessages.Align := alClient;
  FFrameMessages.Parent := PanelMain;

  FFrameWatches := TFrameWatches.Create(Self);
  FFrameWatches.ButtonRequest.Images := ImageListMain;
  FFrameWatches.Align := alClient;
  FFrameWatches.Parent := PanelWatches;

  FFrameMemory := TFrameMemory.Create(Self);
  FFrameMemory.ButtonRequest.Images := ImageListMain;
  FFrameMemory.Align := alClient;
  FFrameMemory.Parent := PanelMemoryAndWatches;

  FSettings := TgoSettings.Create;
  LoadLayout;

  FLogProtocol := TgoLogProtocol.Create(Self);
  ApplySettings;
  Connect;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  SetCurrentProcess(nil);
  SaveLayout;
  FreeAndNil(FFrameMessages);
  FreeAndNil(FFrameMemory);
  FreeAndNil(FFrameWatches);
  FreeAndNil(FSettings);
  FreeAndNil(FLogProtocol);
end;

procedure TFormMain.LoadLayout;
begin
  if (FSettings = nil) then
    Exit;

  if (FSettings.MainFormBounds.Right <> 0) then
    BoundsRect := FSettings.MainFormBounds;
  if (FSettings.MainFormMaximized) then
    WindowState := TWindowState.wsMaximized;

  if (FSettings.InspectorWidth > 0) then
    FFrameMessages.PanelInspector.Width := FSettings.InspectorWidth;
  ActionViewWatches.Checked := FSettings.ViewWatches;
  ActionViewWatchesExecute(ActionViewWatches);
  if (FSettings.WatchesWidth > 0) then
    PanelMemoryAndWatches.Width := FSettings.WatchesWidth;
  if (FSettings.WatchesHeight > 0) then
    PanelWatches.Height := FSettings.WatchesHeight;
end;

procedure TFormMain.MemoryUsageAvailable(const AProcess: TgoProcess);
begin
  Assert(Assigned(FFrameMemory));
  FFrameMemory.MemoryUsageAvailable(AProcess);
end;

procedure TFormMain.LiveWatchesAvailable(const AProcess: TgoProcess);
begin
  Assert(Assigned(FFrameWatches));
  FFrameWatches.LiveWatchesAvailable(AProcess);
end;

procedure TFormMain.ProcessAdded(const AProcess: TgoProcess);
begin
  Assert(Assigned(AProcess));
  AProcess.Listener := Self;
  TabControlProcesses.Tabs.AddObject(Format('%s (%d)', [AProcess.AppName, AProcess.ProcessId]), AProcess);
  TabControlProcesses.TabIndex := TabControlProcesses.Tabs.Count - 1;
  TabControlProcesses.Visible := True;
  SetCurrentProcess(AProcess);
end;

procedure TFormMain.SaveLayout;
begin
  if (FSettings = nil) then
    Exit;

  FSettings.MainFormMaximized := (WindowState = wsMaximized);
  FSettings.MainFormBounds := BoundsRect;
  if Assigned(FFrameMessages) then
    FSettings.InspectorWidth := FFrameMessages.PanelInspector.Width;
  FSettings.ViewWatches := PanelMemoryAndWatches.Visible;
  FSettings.WatchesWidth := PanelMemoryAndWatches.Width;
  FSettings.WatchesHeight := PanelWatches.Height;
end;

procedure TFormMain.SetCurrentProcess(const AProcess: TgoProcess);
begin
  FCurrentProcess := AProcess;
  if Assigned(FFrameMessages) then
    FFrameMessages.SetCurrentProcess(AProcess);
  if Assigned(FFrameMemory) then
    FFrameMemory.SetCurrentProcess(AProcess);
  if Assigned(FFrameWatches) then
    FFrameWatches.SetCurrentProcess(AProcess);
end;

procedure TFormMain.TabControlProcessesChange(Sender: TObject);
var
  Process: TgoProcess;
begin
  if (TabControlProcesses.TabIndex >= 0) then
  begin
    Process := TgoProcess(TabControlProcesses.Tabs.Objects[TabControlProcesses.TabIndex]);
    Assert(Assigned(Process))
  end
  else
    Process := nil;

  SetCurrentProcess(Process);
end;

procedure TFormMain.TabControlProcessesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  if (Button = mbRight) then
  begin
    Index := TabControlProcesses.IndexOfTabAt(X, Y);
    if (Index >= 0) then
    begin
      TabControlProcesses.TabIndex := Index;
      TabControlProcessesChange(TabControlProcesses);
      TabControlProcesses.PopupMenu := PopupMenuTabs;
    end
    else
      TabControlProcesses.PopupMenu := nil;
  end;
end;

procedure TFormMain.ActionClearExecute(Sender: TObject);
begin
  Assert(Assigned(FFrameMessages));
  FFrameMessages.Clear;
end;

procedure TFormMain.ActionCloseTabExecute(Sender: TObject);
var
  Index: Integer;
  Process: TgoProcess;
begin
  Index := TabControlProcesses.TabIndex;
  if (Index >= 0) then
  begin
    Process := TgoProcess(TabControlProcesses.Tabs.Objects[TabControlProcesses.TabIndex]);
    FLogProtocol.RemoveProcess(Process);

    TabControlProcesses.Tabs.Delete(Index);
    if (Index >= TabControlProcesses.Tabs.Count) then
      Index := TabControlProcesses.Tabs.Count - 1;
    if (Index < 0) then
      TabControlProcesses.Visible := False
    else
      TabControlProcesses.TabIndex := Index;
    TabControlProcessesChange(TabControlProcesses);
  end;
end;

procedure TFormMain.ActionConnectExecute(Sender: TObject);
begin
  if TFormConnect.Execute(FSettings) then
    Connect;
end;

procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ActionOptionsExecute(Sender: TObject);
begin
  if TFormOptions.Execute(FSettings) then
    ApplySettings;
end;

procedure TFormMain.ActionResetExecute(Sender: TObject);
begin
  TabControlProcesses.Tabs.Clear;
  TabControlProcesses.Visible := False;
  TabControlProcessesChange(TabControlProcesses);
  FLogProtocol.RemoveAllProcesses;
end;

procedure TFormMain.ActionViewWatchesExecute(Sender: TObject);
begin
  PanelMemoryAndWatches.Visible := ActionViewWatches.Checked;
  SplitterMain.Visible := ActionViewWatches.Checked;
  if (SplitterMain.Visible) then
    SplitterMain.Left := PanelMemoryAndWatches.Left - SplitterMain.Width - 1;
end;

procedure TFormMain.ApplySettings;
begin
  Assert(Assigned(FFrameMessages));
  FFrameMessages.ApplySettings(FSettings);
end;

end.

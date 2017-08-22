unit Form.Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Messaging,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.MultiView,
  FMX.ListBox,
  FMX.TabControl,
  FMX.Edit,
  Settings,
  Frame.Basic,
  Frame.DataTypes,
  Frame.Tracking,
  Frame.Other,
  Frame.Watches;

type
  TFormMain = class(TForm)
    MultiView: TMultiView;
    ToolBarMain: TToolBar;
    LabelMain: TLabel;
    LayoutDetail: TLayout;
    ToolBarDetail: TToolBar;
    LabelDetail: TLabel;
    MasterButton: TSpeedButton;
    ListBoxExamples: TListBox;
    ListBoxItemConnect: TListBoxItem;
    TabControlExamples: TTabControl;
    TabItemConnect: TTabItem;
    ListBoxItemBasic: TListBoxItem;
    LayoutBroker: TLayout;
    LabelBroker: TLabel;
    EditBroker: TEdit;
    LayoutService: TLayout;
    LabelService: TLabel;
    EditService: TEdit;
    LayoutConnect: TLayout;
    ButtonConnect: TButton;
    TabItemBasic: TTabItem;
    FrameBasic: TFrameBasic;
    ListBoxItemDataTypes: TListBoxItem;
    TabItemDataTypes: TTabItem;
    FrameDataTypes: TFrameDataTypes;
    ListBoxItemTracking: TListBoxItem;
    TabItemTracking: TTabItem;
    FrameTracking: TFrameTracking;
    ListBoxItemOther: TListBoxItem;
    TabItemOther: TTabItem;
    FrameOther: TFrameOther;
    TabItem1: TTabItem;
    ListBoxItemWatches: TListBoxItem;
    FrameWatches: TFrameWatches;
    procedure ListBoxExamplesItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditConnectChangeTracking(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
  private
    { Private declarations }
    FSettings: TSettings;
    FStartTime: TDateTime;
    procedure ShowSelectedPage;
    procedure EnableExamples(const AEnable: Boolean);
    procedure HandleLiveWatches(const Sender: TObject; const M: TMessage);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.DateUtils,
  Grijjy.CloudLogging;

procedure TFormMain.ButtonConnectClick(Sender: TObject);
begin
  FSettings.Broker := EditBroker.Text.Trim;
  FSettings.Service := EditService.Text.Trim;
  FSettings.Save;
  GrijjyLog.Connect(FSettings.Broker, FSettings.Service);
  EnableExamples(True);
  ListBoxExamples.ItemIndex := ListBoxItemBasic.Index;
  ShowSelectedPage;
end;

procedure TFormMain.EditConnectChangeTracking(Sender: TObject);
begin
  ButtonConnect.Enabled := (EditBroker.Text.Trim <> '') and (EditService.Text.Trim <> '');
end;

procedure TFormMain.EnableExamples(const AEnable: Boolean);
begin
  ListBoxItemBasic.Visible := AEnable;
  ListBoxItemDataTypes.Visible := AEnable;
  ListBoxItemTracking.Visible := AEnable;
  ListBoxItemWatches.Visible := AEnable;
  ListBoxItemOther.Visible := AEnable;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FSettings := TSettings.Create;
  TabControlExamples.TabPosition := TTabPosition.None;
  MasterButton.Visible := False;
  EnableExamples(False);

  EditBroker.Text := FSettings.Broker;
  EditService.Text := FSettings.Service;

  { Enable all log levels }
  GrijjyLog.SetLogLevel(TgoLogLevel.Info);

  { Subscribe to the Live Watches message to provide the Grijjy Log Viewer with
    a list of watches.
    Besides this form, TFrameWatches also subscribes to this message. }
  TMessageManager.DefaultManager.SubscribeToMessage(TgoLiveWatchesMessage,
    HandleLiveWatches);
  FStartTime := Now;

  ShowSelectedPage;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  TMessageManager.DefaultManager.Unsubscribe(TgoLiveWatchesMessage,
    HandleLiveWatches);

  FSettings.Free;
end;

procedure TFormMain.HandleLiveWatches(const Sender: TObject; const M: TMessage);
var
  Msg: TgoLiveWatchesMessage absolute M;
  ElapsedMs: Integer;
begin
  Assert(M is TgoLiveWatchesMessage);
  ElapsedMs := MilliSecondsBetween(Now, FStartTime);
  Msg.Add('Elapsed Seconds', ElapsedMs / 1000, 1);
end;

procedure TFormMain.ListBoxExamplesItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  ShowSelectedPage;
  Item.IsSelected := False;
  MultiView.HideMaster;
end;

procedure TFormMain.ShowSelectedPage;
var
  Item: TListBoxItem;
begin
  Item := ListBoxExamples.Selected;
  if (Item = nil) or (Item is TListBoxSeparatorItem) then
    Exit;

  LabelDetail.Text := Item.Text;
  TabControlExamples.TabIndex := Item.Tag;
end;

end.

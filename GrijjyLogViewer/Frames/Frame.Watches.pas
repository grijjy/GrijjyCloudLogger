unit Frame.Watches;

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
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Grijjy.CloudLogging.Protocol,
  Processes;

type
  TFrameWatches = class(TFrame)
    TimerUpdate: TTimer;
    ListViewWatches: TListView;
    PanelHeader: TPanel;
    CheckBoxAutoRequest: TCheckBox;
    ButtonRequest: TButton;
    procedure CheckBoxAutoRequestClick(Sender: TObject);
    procedure ButtonRequestClick(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure ListViewWatchesCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    FCurrentProcess: TgoProcess;
    FWatches: TArray<TgoLiveWatch>;
  private
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  private
    procedure RequestLiveWatches;
    procedure ShowLiveWatches;
  public
    procedure SetCurrentProcess(const AProcess: TgoProcess);
    procedure LiveWatchesAvailable(const AProcess: TgoProcess);
  end;

implementation

{$R *.dfm}

uses
  Utils;

{ TFrameWatches }

procedure TFrameWatches.ButtonRequestClick(Sender: TObject);
begin
  RequestLiveWatches;
end;

procedure TFrameWatches.CheckBoxAutoRequestClick(Sender: TObject);
begin
  TimerUpdate.Enabled := CheckBoxAutoRequest.Checked;
  ButtonRequest.Enabled := not CheckBoxAutoRequest.Checked;
end;

procedure TFrameWatches.CMVisibleChanged(var Message: TMessage);
begin
  if Assigned(TimerUpdate) then
    TimerUpdate.Enabled := Visible and (CheckBoxAutoRequest.Checked);
end;

procedure TFrameWatches.ListViewWatchesCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
const
  ALIGN: array [TgoWatchAlign] of TTextFormats = (tfLeft, tfCenter, tfRight);
var
  Canvas: TCanvas;
  Index: Integer;
  R: TRect;
  S: String;
begin
  if (FCurrentProcess = nil) then
    Exit;

  Canvas := ListViewWatches.Canvas;
  Index := Item.Index;

  { Item background (either selection bar or alternating background color) }
  R := Item.DisplayRect(drSelectBounds);
  if (Item.Selected) then
    Canvas.Brush.Color := clSkyBlue
  else if (Odd(Index)) then
    Canvas.Brush.Color := clBtnFace
  else
    Canvas.Brush.Color := clWhite;
  Canvas.FillRect(R);

  if (Index < Length(FWatches)) then
  begin
    { Paint name }
    R := Item.DisplayRect(drLabel);
    S := FWatches[Index].Name;
    Canvas.TextRect(R, S, TEXT_FORMAT);

    { Paint value }
    SetTextColor(Canvas.Handle, VALUE_COLOR);
    R := Item.SubItemDisplayRect(0, drLabel);
    S := FWatches[Index].Value;
    Canvas.TextRect(R, S, TEXT_FORMAT + [ALIGN[FWatches[Index].ValueAlign]]);
  end;

  DefaultDraw := False;
end;

procedure TFrameWatches.LiveWatchesAvailable(const AProcess: TgoProcess);
begin
  if (AProcess = FCurrentProcess) then
    ShowLiveWatches;
end;

procedure TFrameWatches.RequestLiveWatches;
begin
  if Assigned(FCurrentProcess) then
    FCurrentProcess.RequestLiveWatches;
end;

procedure TFrameWatches.SetCurrentProcess(const AProcess: TgoProcess);
begin
  FCurrentProcess := AProcess;
  ShowLiveWatches;
end;

procedure TFrameWatches.ShowLiveWatches;
begin
  if Assigned(FCurrentProcess) then
  begin
    FWatches := FCurrentProcess.GetLiveWatches;
    ListViewWatches.Items.Count := Length(FWatches);
    ListViewWatches.Repaint;
  end;
end;

procedure TFrameWatches.TimerUpdateTimer(Sender: TObject);
begin
  RequestLiveWatches;
end;

end.

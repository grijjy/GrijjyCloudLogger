unit Frame.Memory;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.CommCtrl,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.ImageList,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ImgList,
  Grijjy.Collections,
  Processes,
  Usage;

type
  TListView = class(Vcl.ComCtrls.TListView)
  public
    procedure SetItemCount(const ACount: Integer);
  end;

type
  TFrameMemory = class(TFrame)
    TimerUpdate: TTimer;
    PanelHeader: TPanel;
    CheckBoxAutoRequest: TCheckBox;
    ButtonRequest: TButton;
    ImageListMemory: TImageList;
    ListViewMemory: TListView;
    procedure ButtonRequestClick(Sender: TObject);
    procedure CheckBoxAutoRequestClick(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure ListViewMemoryClick(Sender: TObject);
    procedure ListViewMemoryDblClick(Sender: TObject);
    procedure ListViewMemoryAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
  private
    FCurrentProcess: TgoProcess;
    FExpandedClasses: TgoSet<THandle>;
    FItems: TList<TgoUsageItem>;
  private
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  private
    procedure RequestMemoryUsage;
    procedure ShowMemoryUsage;
    procedure ToggleNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetCurrentProcess(const AProcess: TgoProcess);
    procedure MemoryUsageAvailable(const AProcess: TgoProcess);
  end;

implementation

{$R *.dfm}

uses
  Grijjy.SysUtils,
  Grijjy.CloudLogging.Protocol,
  Utils;

const
  ICON_HEIGHT  = 12;
  NODE_INDENT  = 16;
  II_COLLAPSED = 0;
  II_EXPANDED  = 1;

procedure TFrameMemory.ButtonRequestClick(Sender: TObject);
begin
  RequestMemoryUsage;
end;

procedure TFrameMemory.CheckBoxAutoRequestClick(Sender: TObject);
begin
  TimerUpdate.Enabled := CheckBoxAutoRequest.Checked;
  ButtonRequest.Enabled := not CheckBoxAutoRequest.Checked;
end;

procedure TFrameMemory.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(TimerUpdate) then
    TimerUpdate.Enabled := Visible and (CheckBoxAutoRequest.Checked);
end;

constructor TFrameMemory.Create(AOwner: TComponent);
begin
  inherited;
  FExpandedClasses := TgoSet<THandle>.Create;
  FItems := TList<TgoUsageItem>.Create;
end;

destructor TFrameMemory.Destroy;
begin
  FItems.Free;
  FExpandedClasses.Free;
  inherited;
end;

procedure TFrameMemory.ListViewMemoryAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  Canvas: TCanvas;
  Index, Delta: Integer;
  UsageItem: TgoUsageItem;
  ClassItem: TgoClassUsageItem absolute UsageItem;
  InstanceItem: TgoInstanceUsageItem absolute UsageItem;
  I: Integer;
  R: TRect;
  S: String;
  ValueFormat: (Int, ByteCount);

  function ValueToString(const AValue: Integer): String;
  begin
    case ValueFormat of
      Int:
        Result := IntToStr(AValue);

      ByteCount:
        Result := goByteCountToString(AValue);
    end;
  end;

begin
  DefaultDraw := False;
  Index := Item.Index;
  if (FCurrentProcess = nil) or (Index >= FItems.Count) or (Stage <> cdPrePaint) then
    Exit;

  Canvas := ListViewMemory.Canvas;

  { Item background (either selection bar or alternating background color) }
  R := Item.DisplayRect(drSelectBounds);
  if (Item.Selected) then
    Canvas.Brush.Color := clSkyBlue
  else if (Odd(Index)) then
    Canvas.Brush.Color := clBtnFace
  else
    Canvas.Brush.Color := clWhite;
  Canvas.FillRect(R);

  UsageItem := FItems[Index];
  if (UsageItem.UsageType = TgoUsageType.Clazz) then
  begin
    R := Item.DisplayRect(drLabel);

    if (ClassItem.ClassHandle = 0) then
      { Not a class. Paint in Bold. }
      Canvas.Font.Style := [fsBold]
    else
    begin
      { Paint tree node icon }
      if (ClassItem.Count = 0) then
        Index := -1
      else if (ClassItem.Instances = nil) then
        Index := II_COLLAPSED
      else
        Index := II_EXPANDED;

      if (Index >= 0) then
        ImageListMemory.Draw(Canvas, R.Left, R.Top + (R.Height - ICON_HEIGHT) div 2, Index);
    end;

    Inc(R.Left, NODE_INDENT);

    { Paint class name }
    S := ClassItem.Caption;
    ValueFormat := Int;
    if (ClassItem.IsPseudoClass) then
    begin
      S := S.Substring(1);
      Canvas.Font.Style := [fsBold];
      ValueFormat := ByteCount;
    end;
    Canvas.TextRect(R, S, TEXT_FORMAT);

    { Paint metrics }
    SetTextColor(Canvas.Handle, VALUE_COLOR);
    Delta := ClassItem.Count - ClassItem.Previous;
    for I := 0 to 3 do
    begin
      R := Item.SubItemDisplayRect(I, drLabel);
      case I of
        0: S := ValueToString(ClassItem.Count);
        1: S := ValueToString(ClassItem.Previous);
        2: if (Delta > 0) then
             S := '+' + ValueToString(Delta)
           else
             S := ValueToString(Delta);
        3: S := ValueToString(ClassItem.Max);
      end;
      Canvas.TextRect(R, S, TEXT_FORMAT + [tfRight]);
    end;
  end
  else
  begin
    R := Item.DisplayRect(drBounds);
    Inc(R.Left, NODE_INDENT * 2);
    S := InstanceItem.Caption;
    SetTextColor(Canvas.Handle, INSTANCE_COLOR);
    Canvas.TextRect(R, S, TEXT_FORMAT);
  end;
end;

procedure TFrameMemory.ListViewMemoryClick(Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ListViewMemory.ScreenToClient(P);
  if (P.X <= NODE_INDENT) then
    ToggleNode;
end;

procedure TFrameMemory.ListViewMemoryDblClick(Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ListViewMemory.ScreenToClient(P);
  if (P.X > NODE_INDENT) then
    ToggleNode;
end;

procedure TFrameMemory.MemoryUsageAvailable(const AProcess: TgoProcess);
begin
  if (AProcess = FCurrentProcess) then
    ShowMemoryUsage;
end;

procedure TFrameMemory.RequestMemoryUsage;
begin
  if Assigned(FCurrentProcess) then
    FCurrentProcess.RequestMemoryUsage(FExpandedClasses.ToArray);
end;

procedure TFrameMemory.SetCurrentProcess(const AProcess: TgoProcess);
begin
  FCurrentProcess := AProcess;
  FExpandedClasses.Clear;
  FItems.Clear;
  ShowMemoryUsage;
end;

procedure TFrameMemory.ShowMemoryUsage;
var
  ClassUsage: TStringList;
  UsageItem: TgoClassUsageItem;
  I, J: Integer;
begin
  if Assigned(FCurrentProcess) then
  begin
    FCurrentProcess.UpdateMemoryUsage;
    FItems.Clear;
    ClassUsage := FCurrentProcess.ClassUsage;
    for I := 0 to ClassUsage.Count - 1 do
    begin
      UsageItem := TgoClassUsageItem(ClassUsage.Objects[I]);
      FItems.Add(UsageItem);
      for J := 0 to Length(UsageItem.Instances) - 1 do
        FItems.Add(UsageItem.Instances[J]);
    end;
    FItems.Add(FCurrentProcess.TotalObjects);
    ListViewMemory.SetItemCount(FItems.Count);
    { Force column size recalculation }
//    ListViewMemory.Width := ListViewMemory.Width + 1;
  end;
end;

procedure TFrameMemory.TimerUpdateTimer(Sender: TObject);
begin
  RequestMemoryUsage;
end;

procedure TFrameMemory.ToggleNode;
var
  Item: TListItem;
  Index, EndIndex: Integer;
  UsageItem: TgoUsageItem;
  ClassItem: TgoClassUsageItem absolute UsageItem;
begin
  Item := ListViewMemory.Selected;
  if (Item = nil) then
    Exit;

  Index := Item.Index;
  if (Index >= FItems.Count) then
    Exit;

  UsageItem := FItems[Index];
  if (UsageItem.UsageType <> TgoUsageType.Clazz) or (ClassItem.ClassHandle = 0) then
    Exit;

  if (ClassItem.Instances = nil) then
  begin
    { Node is collapsed. Expand it. }
    if (ClassItem.Count > 0) then
    begin
      FExpandedClasses.AddOrSet(ClassItem.ClassHandle);
      RequestMemoryUsage;
    end;
  end
  else
  begin
    { Node is expanded. Collapse it. }
    FExpandedClasses.Remove(ClassItem.ClassHandle);

    { Delete child "nodes" }
    Inc(Index);
    EndIndex := Index;
    while (EndIndex < FItems.Count) and (FItems[EndIndex].UsageType = TgoUsageType.Instance) do
      Inc(EndIndex);
    ClassItem.Instances := nil;
    if (EndIndex <> Index) then
    begin
      FItems.DeleteRange(Index, EndIndex - Index);
      ListViewMemory.SetItemCount(FItems.Count);
    end;
  end;
end;

{ TListView }

procedure TListView.SetItemCount(const ACount: Integer);
begin
  { We cannot use "Items.Count := ACount", since this will update the scroll
    position if the count changes. So we do it manually. }
  if (ACount <> 0) then
    ListView_SetItemCountEx(Handle, ACount, LVSICF_NOINVALIDATEALL or LVSICF_NOSCROLL)
  else
    ListView_SetItemCountEx(Handle, ACount, 0);
  Repaint;
end;

end.

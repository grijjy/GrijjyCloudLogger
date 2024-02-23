unit Frame.Messages;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ImgList,
  Vcl.Buttons,
  Grijjy.Bson,
  LogMessages,
  Processes,
  Settings;

type
  TListView = class(Vcl.ComCtrls.TListView)
  private
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  end;

type
  TFrameMessages = class(TFrame)
    PanelInspector: TPanel;
    MemoInspector: TMemo;
    PanelObjectInspector: TPanel;
    HeaderControlInspector: THeaderControl;
    TreeViewInspector: TTreeView;
    SplitterMessages: TSplitter;
    ListViewMessages: TListView;
    ImageListMessages: TImageList;
    TimerUpdate: TTimer;
    PanelHeader: TPanel;
    ButtonSave: TButton;
    SaveDialog: TSaveDialog;
    SpeedButtonScrollLock: TSpeedButton;
    procedure HeaderControlInspectorSectionResize(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure ListViewMessagesCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListViewMessagesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure TreeViewInspectorAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure TreeViewInspectorDeletion(Sender: TObject; Node: TTreeNode);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure SpeedButtonScrollLockClick(Sender: TObject);
  private
    FCurrentDocument: TgoBsonDocument;
    FCurrentProcess: TgoProcess;
    FCurrentMessages: TgoLogMessages;
    FCurrentMessage: TgoLogMessage;
    FTimeDisplay: TgoTimeDisplay;
    FNameWidth: Integer;
    FListViewScrollbarVisible: Boolean;
  private
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  private
    procedure ClearInspectors;
    procedure UpdateUIFromProcess(const AProcess: TgoProcess);
    procedure ShowMessage(const AIndex: Integer);
    procedure ShowMemo;
    procedure ShowObjectInspector;
    procedure ShowText(const AString: String);
    procedure ShowTStrings(const AData: TBytes);
    procedure ShowMemory(const AData: TBytes);
    procedure ShowObject(const AData: TBytes);
    procedure ShowMember(const AParentNode: TTreeNode;
      const ADoc: TgoBsonDocument; const ALevel: Integer);
    procedure SaveMemory(const AData: TBytes; const AFilename: String);
  public
    destructor Destroy; override;
    procedure ApplySettings(const ASettings: TgoSettings);
    procedure SetCurrentProcess(const AProcess: TgoProcess);
    procedure Clear;
  end;

implementation

{$R *.dfm}

uses
  System.Math,
  System.Types,
  System.Character,
  System.ZLib,
  System.Generics.Collections,
  Grijjy.CloudLogging.Protocol,
  Utils;

const
  ICON_HEIGHT  = 12;
  II_COLLAPSED = 9;
  II_EXPANDED  = 10;
  II_FOCUSED   = 11;

type
  TNodeData = class
    Name: String;
    Value: TgoBsonValue;
  end;

{ TFrameMessages }

procedure TFrameMessages.ApplySettings(const ASettings: TgoSettings);
begin
  FTimeDisplay := ASettings.TimeDisplay;
  case FTimeDisplay of
    TgoTimeDisplay.Difference:
      ListViewMessages.Columns[1].Caption := 'Time Diff';

    TgoTimeDisplay.Offset:
      ListViewMessages.Columns[1].Caption := 'Time Offset';
  else
    ListViewMessages.Columns[1].Caption := 'Time Stamp';
  end;
  ListViewMessages.Repaint;
end;

procedure TFrameMessages.ButtonSaveClick(Sender: TObject);
begin
  if (FCurrentMessage = nil) then
    Exit;

  case FCurrentMessage.DataFormat of
    LOG_FORMAT_NONE,
    LOG_FORMAT_TSTRINGS:
      begin
        SaveDialog.Filter := 'Text Files|*.txt';
        SaveDialog.DefaultExt := 'txt';
      end;

    LOG_FORMAT_MEMORY:
      begin
        SaveDialog.Filter := 'Binary Files|*.bin';
        SaveDialog.DefaultExt := 'bin';
      end;

    LOG_FORMAT_OBJECT:
      begin
        SaveDialog.Filter := 'JSON Files|*.json';
        SaveDialog.DefaultExt := 'json';
      end;
  else
    Exit;
  end;

  SaveDialog.FileName := '';
  if (not SaveDialog.Execute) then
    Exit;

  case FCurrentMessage.DataFormat of
    LOG_FORMAT_NONE,
    LOG_FORMAT_TSTRINGS:
      MemoInspector.Lines.SaveToFile(SaveDialog.FileName);

    LOG_FORMAT_MEMORY:
      SaveMemory(FCurrentMessage.Data, SaveDialog.FileName);

    LOG_FORMAT_OBJECT:
      if (not FCurrentDocument.IsNil) then
        FCurrentDocument.SaveToJsonFile(SaveDialog.FileName, TgoJsonWriterSettings.Pretty);
  end;
end;

procedure TFrameMessages.Clear;
begin
  if Assigned(FCurrentProcess) then
    FCurrentProcess.ClearLogMessages;

  ClearInspectors;
  ListViewMessages.Clear;
end;

procedure TFrameMessages.ClearInspectors;
begin
  MemoInspector.Clear;
  TreeViewInspector.Items.Clear;
  FCurrentDocument.SetNil;
end;

procedure TFrameMessages.CMVisibleChanged(var Message: TMessage);
begin
  TimerUpdate.Enabled := Visible;
end;

destructor TFrameMessages.Destroy;
begin
  inherited;
end;

procedure TFrameMessages.HeaderControlInspectorSectionResize(
  HeaderControl: THeaderControl; Section: THeaderSection);
begin
  FNameWidth := HeaderControl.Sections[0].Width - 1;
  TreeViewInspector.Repaint;
end;

procedure TFrameMessages.ListViewMessagesCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Index: Integer;
  LogMsg: TgoLogMessage;
  Canvas: TCanvas;
  Time: TDateTime;
  R: TRect;
  S: String;
begin
  Assert(Assigned(Item));
  if (FCurrentMessages = nil) then
    Exit;

  Index := Item.Index;
  if (Index < 0) or (Index >= FCurrentMessages.Count) then
    Exit;

  LogMsg := FCurrentMessages[Index];
  Canvas := ListViewMessages.Canvas;

  { Item background (either selection bar or alternating background color) }
  R := Item.DisplayRect(drSelectBounds);
  if (Item.Selected) then
    Canvas.Brush.Color := clSkyBlue
  else if (Odd(Index)) then
    Canvas.Brush.Color := clBtnFace
  else
    Canvas.Brush.Color := clWhite;
  Canvas.FillRect(R);

  { Paint message }
  R := Item.DisplayRect(drLabel);
  Inc(R.Left, LogMsg.Indent shl 4);
  ImageListMessages.Draw(Canvas, R.Left, R.Top + (R.Height - ICON_HEIGHT) div 2, Ord(LogMsg.Level));
  Inc(R.Left, 20);
  if (LogMsg.Level <= TgoLogLevelEx.Error) then
  begin
    ImageListMessages.Draw(Canvas, R.Left, R.Top + (R.Height - ICON_HEIGHT) div 2, 5 + Ord(LogMsg.DataFormat));
    Inc(R.Left, 20);
  end;
  S := LogMsg.Message;
  Canvas.TextRect(R, S, TEXT_FORMAT);

  { Paint timestamp }
  R := Item.SubItemDisplayRect(0, drLabel);
  Dec(R.Right, 4);
  Time := LogMsg.TimeStamp;
  case FTimeDisplay of
    TgoTimeDisplay.Difference:
      begin
        if (Index = 0) then
          Time := 0
        else if Assigned(FCurrentMessages) then
          Time := Time - FCurrentMessages[Index - 1].TimeStamp;
      end;

    TgoTimeDisplay.Offset:
      if Assigned(FCurrentProcess) then
        Time := Time - FCurrentProcess.FirstMessageTime;
  end;
  S := FormatDateTime('h:nn:ss.zzz', Time);
  Canvas.TextRect(R, S, TEXT_FORMAT + [tfRight]);

  { Paint focus }
  if (cdsFocused in State) then
  begin
    R := Item.DisplayRect(drSelectBounds);
    Canvas.DrawFocusRect(R);
  end;

  DefaultDraw := False;
end;

procedure TFrameMessages.ListViewMessagesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Assigned(Item) and (Selected) then
    ShowMessage(Item.Index);
end;

procedure TFrameMessages.SaveMemory(const AData: TBytes;
  const AFilename: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFilename, fmCreate);
  try
    Stream.WriteBuffer(AData[0], Length(AData));
  finally
    Stream.Free;
  end;
end;

procedure TFrameMessages.SetCurrentProcess(const AProcess: TgoProcess);
var
  Item: TListItem;
begin
  ButtonSave.Enabled := False;
  if (AProcess <> nil) then
  begin
    UpdateUIFromProcess(AProcess);
    ClearInspectors;
    ListViewMessages.ClearSelection;
    if (AProcess.SelectedMessageIndex >= 0) and (AProcess.SelectedMessageIndex < ListViewMessages.Items.Count) then
    begin
      Item := ListViewMessages.Items[AProcess.SelectedMessageIndex];
      ListViewMessages.ItemFocused := Item;
      ListViewMessages.Selected := Item;
    end
    else
      ListViewMessages.ItemFocused := nil;
    ListViewMessages.Repaint;
  end
  else
  begin
    FCurrentProcess := nil;
    FCurrentMessages := nil;
  end;
end;

procedure TFrameMessages.ShowMember(const AParentNode: TTreeNode;
  const ADoc: TgoBsonDocument; const ALevel: Integer);
var
  I: Integer;
  Element: TgoBsonElement;
  Node: TTreeNode;
  Data: TNodeData;
begin
  for I := 0 to ADoc.Count - 1 do
  begin
    Element := ADoc.Elements[I];
    if (AParentNode <> nil) and (Element.Name = '@Class') then
      Continue;

    Node := TreeViewInspector.Items.AddChild(AParentNode, '');

    { We abuse ImageIndex for indentation level.
      This is faster than using the Level property. }
    Node.ImageIndex := ALevel;
    Data := TNodeData.Create;
    Data.Name := Element.Name;
    Data.Value := Element.Value;
    Node.Data := Data;
    if (Element.Value.IsBsonDocument) then
      ShowMember(Node, Element.Value.AsBsonDocument, ALevel + 1);
  end;
end;

procedure TFrameMessages.ShowMemo;
begin
  PanelObjectInspector.Visible := False;
  MemoInspector.Align := alClient;
  MemoInspector.Visible := True;
end;

procedure TFrameMessages.ShowMemory(const AData: TBytes);
const
  HEX_CHARS: array [0..15] of Char = '0123456789ABCDEF';
var
  Offset: Cardinal;
  Index, Size, Count, I, HexIndex, CharIndex: Integer;
  S: String;
  B: Byte;
  C: Char;
begin
  ShowMemo;
  Offset := 0;
  Size := Length(AData);
  Index := 0;
  { For performance, we allocate S once and fill it in-place. Example line:
        00000000: 54 68 65 20 51 75 69 63  | The Quic }
  S := '        :                          |         ';
  MemoInspector.Lines.BeginUpdate;
  try
    while (Size > 0) do
    begin
      { Offset (for performance, don't use InToHex because we want to fill S
        in-place. }
      S[1] := HEX_CHARS[Offset shr 28];
      S[2] := HEX_CHARS[(Offset shr 24) and $0F];
      S[3] := HEX_CHARS[(Offset shr 20) and $0F];
      S[4] := HEX_CHARS[(Offset shr 16) and $0F];
      S[5] := HEX_CHARS[(Offset shr 12) and $0F];
      S[6] := HEX_CHARS[(Offset shr 8) and $0F];
      S[7] := HEX_CHARS[(Offset shr 4) and $0F];
      S[8] := HEX_CHARS[Offset and $0F];

      Count := Min(Size, 8);
      HexIndex := 11;
      CharIndex := 38;

      for I := 0 to Count - 1 do
      begin
        B := AData[Index];
        Inc(Index);

        S[HexIndex] := HEX_CHARS[B shr 4];
        S[HexIndex + 1] := HEX_CHARS[B and $0F];
        Inc(HexIndex, 3);

        C := Char(B);
        if (C.IsControl) then
          S[CharIndex] := '.'
        else
          S[CharIndex] := C;
        Inc(CharIndex);
      end;

      for I := Count to 7 do
      begin
        S[HexIndex] := ' ';
        S[HexIndex + 1] := ' ';
        Inc(HexIndex, 3);

        S[CharIndex] := ' ';
        Inc(CharIndex);
      end;

      MemoInspector.Lines.Add(S);

      Dec(Size, 8);
      Inc(Offset, 8);
    end;
  finally
    MemoInspector.Lines.EndUpdate;
  end;
end;

procedure TFrameMessages.ShowMessage(const AIndex: Integer);
var
  Msg: TgoLogMessage;
begin
  if Assigned(FCurrentMessages) and Assigned(FCurrentProcess) and
    (AIndex >= 0) and (AIndex < FCurrentMessages.Count) then
  begin
    FCurrentProcess.SelectedMessageIndex := AIndex;
    Msg := FCurrentMessages[AIndex];
    if (Msg <> FCurrentMessage) then
    begin
      FCurrentMessage := Msg;
      MemoInspector.Clear;
      TreeViewInspector.Items.Clear;
      FCurrentDocument.SetNil;
      case Msg.DataFormat of
        LOG_FORMAT_NONE:
          ShowText(Msg.Message);

        LOG_FORMAT_TSTRINGS:
          ShowTStrings(Msg.Data);

        LOG_FORMAT_MEMORY:
          ShowMemory(Msg.Data);

        LOG_FORMAT_OBJECT:
          ShowObject(Msg.Data);
      else
        Exit;
      end;
      ButtonSave.Enabled := True;
    end;
  end;
end;

procedure TFrameMessages.ShowObject(const AData: TBytes);
var
  Uncompressed: TBytes;
  Json: String;
begin
  TreeViewInspector.Items.BeginUpdate;
  try
    ZDecompress(AData, Uncompressed);
    Json := TEncoding.UTF8.GetString(Uncompressed);
    FCurrentDocument := TgoBsonDocument.Parse(Json);
    TreeViewInspector.Items.Clear;
    ShowMember(nil, FCurrentDocument, 0);
  finally
    TreeViewInspector.Items.EndUpdate;
  end;
  ShowObjectInspector;
end;

procedure TFrameMessages.ShowObjectInspector;
begin
  MemoInspector.Visible := False;
  PanelObjectInspector.Align := alClient;
  PanelObjectInspector.Visible := True;
end;

procedure TFrameMessages.ShowText(const AString: String);
begin
  ShowMemo;
  MemoInspector.Text := AString;
end;

procedure TFrameMessages.ShowTStrings(const AData: TBytes);
var
  S: String;
begin
  ShowMemo;
  S := TEncoding.UTF8.GetString(AData);
  MemoInspector.Lines.BeginUpdate;
  try
    MemoInspector.Lines.CommaText := S;
  finally
    MemoInspector.Lines.EndUpdate;
  end;
end;

procedure TFrameMessages.SpeedButtonScrollLockClick(Sender: TObject);
begin
  if (SpeedButtonScrollLock.Down) and (ListViewMessages.Items.Count > 0) then
    ListViewMessages.Items[ListViewMessages.Items.Count - 1].MakeVisible(False);
end;

procedure TFrameMessages.TimerUpdateTimer(Sender: TObject);
var
  OldMessagesPurged: Boolean;
begin
  if Assigned(FCurrentProcess) and (FCurrentProcess.HasNewMessages(OldMessagesPurged)) then
  begin
    UpdateUIFromProcess(FCurrentProcess);
    if (OldMessagesPurged) then
      ListViewMessages.Invalidate;
  end;
end;

procedure TFrameMessages.TreeViewInspectorAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
const
  GRID_COLOR  = clBtnFace;
var
  Level, Indent, Index: Integer;
  NodeRect, R: TRect;
  S: String;
  Canvas: TCanvas;
  Data: TNodeData;
begin
  if (HeaderControlInspector = nil) or (Stage <> cdPrePaint) then
    Exit;

  Data := Node.Data;
  if (Data = nil) then
    Exit;

  { We abuse ImageIndex for indentation level.
    This is faster than using the Level property. }
  Level := Node.ImageIndex;
  Indent := Level shl 4;

  if (FNameWidth = 0) then
    FNameWidth := HeaderControlInspector.Sections[0].Width - 1;

  Canvas := TreeViewInspector.Canvas;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsSolid;

  { Paint gutter }
  NodeRect := Node.DisplayRect(False);
  R := NodeRect;
  R.Right := 16;
  Canvas.Brush.Color := GRID_COLOR;
  Canvas.FillRect(R);

  { Paint grid lines }
  Canvas.Pen.Color := GRID_COLOR;
  Canvas.MoveTo(FNameWidth, NodeRect.Top);
  Canvas.LineTo(FNameWidth, NodeRect.Bottom);
  Canvas.MoveTo(NodeRect.Left, NodeRect.Bottom - 1);
  Canvas.LineTo(NodeRect.Right, NodeRect.Bottom - 1);

  { Paint focus bar }
  if (cdsFocused in State) then
  begin
    Canvas.Brush.Color := GRID_COLOR;
    Canvas.FillRect(NodeRect);

    { Default focus text is white. We need black }
    Canvas.Font.Color := clWindowText;
  end;
  Inc(NodeRect.Left, Indent + 18);

  { Paint collapse/expand/focused icon }
  if (Node.HasChildren) then
  begin
    if (Node.Expanded) then
      Index := II_EXPANDED
    else
      Index := II_COLLAPSED
  end
  else if (cdsFocused in State) then
    Index := II_FOCUSED
  else
    Index := -1;

  if (Index >= 0) then
    { Paint focus indicator }
    ImageListMessages.Draw(Canvas, R.Left + Indent, R.Top + (R.Height - ICON_HEIGHT) div 2, Index);

  { Paint name }
  R := NodeRect;
  R.Right := FNameWidth - 4;
  S := Data.Name;
  SetBkMode(Canvas.Handle, TRANSPARENT);
  Canvas.TextRect(R, S, TEXT_FORMAT);

  { Paint value }
  NodeRect.Left := FNameWidth + 4;
  if (Data.Value.IsBsonDocument) then
    S := Data.Value.AsBsonDocument['@Class']
  else
    S := Data.Value.ToString;
  SetTextColor(Canvas.Handle, VALUE_COLOR);
  Canvas.TextRect(NodeRect, S, TEXT_FORMAT);

  DefaultDraw := False;
end;

procedure TFrameMessages.TreeViewInspectorDeletion(Sender: TObject;
  Node: TTreeNode);
var
  Obj: TObject;
begin
  Obj := Node.Data;
  Node.Data := nil;
  Obj.Free;
end;

procedure TFrameMessages.UpdateUIFromProcess(const AProcess: TgoProcess);
var
  ScollBarVisible: Boolean;
  Count: Integer;
begin
  FCurrentMessage := nil;
  if Assigned(AProcess) then
  begin
    FCurrentProcess := AProcess;
    FCurrentMessages := AProcess.Messages;
    Assert(Assigned(FCurrentMessages));
    Count := FCurrentMessages.Count;
    ListViewMessages.Items.Count := Count;
    if (Count > 0) and (SpeedButtonScrollLock.Down) then
      ListViewMessages.Items[Count - 1].MakeVisible(False);

    ScollBarVisible := ListViewMessages.IsVScrollBarVisible;
    if (ScollBarVisible <> FListViewScrollbarVisible) then
    begin
      FListViewScrollbarVisible := ScollBarVisible;
      ListViewMessages.AutoSizeColumns;
    end;
  end
  else
    FCurrentMessages := nil;
end;

{ TListView }

procedure TListView.WMVScroll(var Message: TWMVScroll);
var
  Before, After: Integer;
  Info: TScrollInfo;
begin
  Before := GetScrollPos(Handle, SB_VERT);
  inherited;
  After := GetScrollPos(Handle, SB_VERT);
  if (Before <> After) and Assigned(Owner) and (Owner is TFrameMessages) then
  begin
    Info.cbSize := SizeOf(Info);
    Info.fMask := SIF_PAGE or SIF_RANGE;
    GetScrollInfo(Handle, SB_VERT, Info);
    TFrameMessages(Owner).SpeedButtonScrollLock.Down := ((After + Integer(Info.nPage)) > Info.nMax);
  end;
end;

end.

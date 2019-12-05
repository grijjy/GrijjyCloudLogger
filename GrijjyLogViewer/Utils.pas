unit Utils;

interface

uses
  System.Types,
  Vcl.ComCtrls,
  Vcl.Graphics;

type
  { Adds some methods that Embarcadero should have added to TListItem }
  TListItemHelper = class helper for TListItem
  public
    function SubItemDisplayRect(const ASubItemIndex: Integer;
      const ACode: TDisplayCode): TRect;
  end;

type
  { Adds some methods to TListView }
  TListViewHelper = class helper for TListView
  public
    function IsVScrollBarVisible: Boolean;
    procedure AutoSizeColumns;
  end;

const
  TEXT_FORMAT    = [tfVerticalCenter, tfSingleLine, tfEndEllipsis];
  VALUE_COLOR    = $800000;
  INSTANCE_COLOR = $000080;

implementation

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.CommCtrl;

{ TListItemHelper }

function TListItemHelper.SubItemDisplayRect(const ASubItemIndex: Integer;
  const ACode: TDisplayCode): TRect;
const
  CODES: array [TDisplayCode] of Longint = (LVIR_BOUNDS, LVIR_ICON, LVIR_LABEL,
    LVIR_SELECTBOUNDS);
begin
  Assert(Assigned(Owner));
  Assert(Assigned(Owner.Owner));
  Assert(Owner.Owner.HandleAllocated);
  ListView_GetSubItemRect(Owner.Owner.Handle, Index, ASubItemIndex + 1,
    Codes[ACode], @Result);
end;

{ TListViewHelper }

procedure TListViewHelper.AutoSizeColumns;
var
  WP: TWindowPos;
begin
  { When adding items to the list view, a scrollbar may appear. If that happens,
    the columns need to be resized. This is done in TCustomListView.DoAutoSize.
    Unfortunately, that method is private, but it is called in response to a
    WM_WINDOWPOSCHANGED message... }
  FillChar(WP, SizeOf(WP), 0);
  Perform(WM_WINDOWPOSCHANGED, 0, NativeInt(@WP));
end;

function TListViewHelper.IsVScrollBarVisible: Boolean;
begin
  Result := ((GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL) <> 0);
end;

end.

unit Usage;

{$INCLUDE 'Grijjy.inc'}

interface

type
  TgoUsageType = (Clazz, Instance);

type
  TgoUsageItem = class abstract(TObject)
  {$REGION 'Internal Declarations'}
  private
    FCaption: String;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const ACaption: String);

    function UsageType: TgoUsageType; virtual; abstract;

    property Caption: String read FCaption;
  end;

type
  TgoInstanceUsageItem = class(TgoUsageItem)
  public
    function UsageType: TgoUsageType; override;
  end;

type
  TgoClassUsageItem = class(TgoUsageItem)
  {$REGION 'Internal Declarations'}
  private
    FClassHandle: THandle;
    FCount: Integer;
    FPrevious: Integer;
    FMax: Integer;
    FZeroTime: Cardinal;
    FInstances: TArray<TgoInstanceUsageItem>;
    FChecked: Boolean;
    FIsPseudoClass: Boolean;
    procedure SetCount(const Value: Integer);
    procedure SetInstances(const Value: TArray<TgoInstanceUsageItem>);
  private
    procedure FreeInstances;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const ACaption: String; const AClassHandle: THandle);
    destructor Destroy; override;

    function UsageType: TgoUsageType; override;

    procedure Clear;

    property ClassHandle: THandle read FClassHandle;
    property Count: Integer read FCount write SetCount;
    property Previous: Integer read FPrevious;
    property Max: Integer read FMax;
    property ZeroTime: Cardinal read FZeroTime;
    property Instances: TArray<TgoInstanceUsageItem> read FInstances write SetInstances;
    property Checked: Boolean read FChecked write FChecked;
    property IsPseudoClass: Boolean read FIsPseudoClass;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils;

{ TgoUsageItem }

constructor TgoUsageItem.Create(const ACaption: String);
begin
  inherited Create;
  FCaption := ACaption;
end;

{ TgoInstanceUsageItem }

function TgoInstanceUsageItem.UsageType: TgoUsageType;
begin
  Result := TgoUsageType.Instance;
end;

{ TgoClassUsageItem }

procedure TgoClassUsageItem.Clear;
begin
  FCount := 0;
  FPrevious := 0;
  FMax := 0;
  FZeroTime := 0;
  FreeInstances;
end;

constructor TgoClassUsageItem.Create(const ACaption: String;
  const AClassHandle: THandle);
begin
  inherited Create(ACaption);
  FClassHandle := AClassHandle;
  FIsPseudoClass := Caption.StartsWith('@');
end;

destructor TgoClassUsageItem.Destroy;
begin
  FreeInstances;
  inherited;
end;

procedure TgoClassUsageItem.FreeInstances;
var
  I: Integer;
begin
  for I := 0 to Length(FInstances) - 1 do
    FInstances[I].Free;
  FInstances := nil;
end;

procedure TgoClassUsageItem.SetCount(const Value: Integer);
begin
  if (Value <> FCount) then
  begin
    if (Value = 0) then
      FZeroTime := GetTickCount
    else
      FZeroTime := 0;
  end;

  FPrevious := FCount;
  FCount := Value;
  if (Value > FMax) then
    FMax := Value;
end;

procedure TgoClassUsageItem.SetInstances(
  const Value: TArray<TgoInstanceUsageItem>);
begin
  if (Value <> FInstances) then
  begin
    FreeInstances;
    FInstances := Value;
  end;
end;

function TgoClassUsageItem.UsageType: TgoUsageType;
begin
  Result := TgoUsageType.Clazz;
end;

end.

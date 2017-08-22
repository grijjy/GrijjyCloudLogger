unit Frame.Tracking;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TFrameTracking = class(TFrame)
    ButtonDeallocateMem: TButton;
    ButtonAllocateMem: TButton;
    ButtonDeallocateObjects: TButton;
    ButtonAllocateObjects: TButton;
    LabelInstructions1: TLabel;
    LabelInstructions2: TLabel;
    LabelInstructions3: TLabel;
    procedure ButtonAllocateObjectsClick(Sender: TObject);
    procedure ButtonDeallocateObjectsClick(Sender: TObject);
    procedure ButtonAllocateMemClick(Sender: TObject);
    procedure ButtonDeallocateMemClick(Sender: TObject);
  private
    { Private declarations }
    FObjects: TObjectList<TObject>;
    FMemoryBlocks: TArray<TBytes>;
    procedure UpdateControls;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses
  SampleClasses;

procedure TFrameTracking.ButtonAllocateMemClick(Sender: TObject);
var
  Block: TBytes;
begin
  SetLength(Block, 10 * 1024 * 1024);
  FMemoryBlocks := FMemoryBlocks + [Block];
  UpdateControls;
end;

procedure TFrameTracking.ButtonAllocateObjectsClick(Sender: TObject);
var
  I, Count: Integer;
  Obj: TObject;
begin
  if (FObjects.Count = 0) then
  begin
    FObjects.Add(TSampleFoo.Create);
    FObjects.Add(TSampleBar.Create);
  end;

  Count := Random(10) + 2;
  for I := 0 to Count - 1 do
  begin
    if (Random(2) = 0) then
      Obj := TSampleFoo.Create
    else
      Obj := TSampleBar.Create;
    FObjects.Add(Obj)
  end;
  UpdateControls;
end;

procedure TFrameTracking.ButtonDeallocateMemClick(Sender: TObject);
begin
  if (Length(FMemoryBlocks) > 0) then
    SetLength(FMemoryBlocks, Length(FMemoryBlocks) - 1);
  UpdateControls;
end;

procedure TFrameTracking.ButtonDeallocateObjectsClick(Sender: TObject);
var
  Count: Integer;
begin
  Count := FObjects.Count div 2;
  if (Count = 0) then
    Count := FObjects.Count;

  while (Count > 0) do
  begin
    FObjects.Delete(0);
    Dec(Count);
  end;

  UpdateControls;
end;

constructor TFrameTracking.Create(AOwner: TComponent);
begin
  inherited;
  FObjects := TObjectList<TObject>.Create;
end;

destructor TFrameTracking.Destroy;
begin
  FObjects.Free;
  inherited;
end;

procedure TFrameTracking.UpdateControls;
begin
  ButtonDeallocateObjects.Enabled := (FObjects.Count > 0);
  ButtonDeallocateMem.Enabled := (FMemoryBlocks <> nil);
end;

end.

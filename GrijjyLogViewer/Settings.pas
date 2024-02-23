unit Settings;

{$INCLUDE 'Grijjy.inc'}

interface

uses
  System.Types;

type
  TgoTimeDisplay = (Stamp, Difference, Offset);

type
  TgoSettings = class(TObject)
  {$REGION 'Internal Declarations'}
  private
    FFilename: String;
    FDispatcher: String;
    FService: String;
    FTimeDisplay: TgoTimeDisplay;
    FMainFormBounds: TRect;
    FMainFormMaximized: Boolean;
    FInspectorWidth: Integer;
    FViewWatches: Boolean;
    FWatchesWidth: Integer;
    FWatchesHeight: Integer;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load;
    procedure Save;

    property Dispatcher: String read FDispatcher write FDispatcher;
    property Service: String read FService write FService;
    property TimeDisplay: TgoTimeDisplay read FTimeDisplay write FTimeDisplay;
    property MainFormBounds: TRect read FMainFormBounds write FMainFormBounds;
    property MainFormMaximized: Boolean read FMainFormMaximized write FMainFormMaximized;
    property InspectorWidth: Integer read FInspectorWidth write FInspectorWidth;
    property ViewWatches: Boolean read FViewWatches write FViewWatches;
    property WatchesWidth: Integer read FWatchesWidth write FWatchesWidth;
    property WatchesHeight: Integer read FWatchesHeight write FWatchesHeight;
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils,
  Grijjy.Bson;

const
  DEFAULT_DISPATCHER = 'tcp://localhost:7337';
  DEFAULT_SERVICE    = 'Default';

const
  KEY_DISPATCHER          = 'Dispatcher';
  KEY_SERVICE             = 'Service';
  KEY_TIME_DISPLAY        = 'TimeDisplay';
  KEY_MAIN_FORM_BOUNDS    = 'MainFormBounds';
  KEY_MAIN_FORM_MAXIMIZED = 'MainFormMaximized';
  KEY_INSPECTOR_WIDTH     = 'InspectorWidth';
  KEY_VIEW_WATCHES        = 'ViewWatches';
  KEY_WATCHES_WIDTH       = 'WatchesWidth';
  KEY_WATCHES_HEIGHT      = 'WatchesHeight';
  KEY_LEFT                = 'Left';
  KEY_TOP                 = 'Top';
  KEY_RIGHT               = 'Right';
  KEY_BOTTOM              = 'Bottom';

{ TgoSettings }

constructor TgoSettings.Create;
begin
  inherited Create;
  FFilename := TPath.Combine(TPath.GetHomePath, 'Grijjy\GrijjyLogViewer\Settings.json');
  ForceDirectories(TPath.GetDirectoryName(FFilename));
  FDispatcher := DEFAULT_DISPATCHER;
  FService := DEFAULT_SERVICE;
  Load;
end;

destructor TgoSettings.Destroy;
begin
  Save;
  inherited;
end;

procedure TgoSettings.Load;
var
  Doc,Rect: TgoBsonDocument;
  Value: TgoBsonValue;
  I: Integer;
begin
  if (not TFile.Exists(FFilename)) then
    Exit;

  Doc := TgoBsonDocument.LoadFromJsonFile(FFilename);

  FDispatcher := Doc.Get(KEY_DISPATCHER, DEFAULT_DISPATCHER);
  FService :=  Doc.Get(KEY_SERVICE, DEFAULT_SERVICE);
  I := Doc[KEY_TIME_DISPLAY];
  FTimeDisplay := TgoTimeDisplay(I);

  Value := Doc[KEY_MAIN_FORM_BOUNDS];
  if (Value.IsBsonDocument) then
  begin
    Rect := Value.AsBsonDocument;
    FMainFormBounds.Left := Rect[KEY_LEFT];
    FMainFormBounds.Top := Rect[KEY_TOP];
    FMainFormBounds.Right := Rect[KEY_RIGHT];
    FMainFormBounds.Bottom := Rect[KEY_BOTTOM];
  end;

  FMainFormMaximized := Doc[KEY_MAIN_FORM_MAXIMIZED];
  FInspectorWidth := Doc[KEY_INSPECTOR_WIDTH];
  FViewWatches := Doc[KEY_VIEW_WATCHES];
  FWatchesWidth := Doc[KEY_WATCHES_WIDTH];
  FWatchesHeight := Doc[KEY_WATCHES_HEIGHT];
end;

procedure TgoSettings.Save;
var
  Doc, Rect: TgoBsonDocument;
begin
  Doc := TgoBsonDocument.Create;
  Doc[KEY_DISPATCHER] := FDispatcher;
  Doc[KEY_SERVICE] := FService;
  Doc[KEY_TIME_DISPLAY] := Ord(FTimeDisplay);

  Rect := TgoBsonDocument.Create;
  Doc[KEY_MAIN_FORM_BOUNDS] := Rect;
  Rect[KEY_LEFT] := FMainFormBounds.Left;
  Rect[KEY_TOP] := FMainFormBounds.Top;
  Rect[KEY_RIGHT] := FMainFormBounds.Right;
  Rect[KEY_BOTTOM] := FMainFormBounds.Bottom;

  Doc[KEY_MAIN_FORM_MAXIMIZED] := FMainFormMaximized;
  Doc[KEY_INSPECTOR_WIDTH] := FInspectorWidth;
  Doc[KEY_VIEW_WATCHES] := FViewWatches;
  Doc[KEY_WATCHES_WIDTH] := FWatchesWidth;
  Doc[KEY_WATCHES_HEIGHT] := FWatchesHeight;

  Doc.SaveToJsonFile(FFilename, TgoJsonWriterSettings.Pretty);
end;

end.

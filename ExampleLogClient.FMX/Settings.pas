unit Settings;

interface

type
  TSettings = class(TObject)
  {$REGION 'Internal Declarations'}
  private const
    KEY_BROKER  = 'Broker';
    KEY_SERVICE = 'Service';
  private
    FFilename: String;
    FBroker: String;
    FService: String;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load;
    procedure Save;

    property Broker: String read FBroker write FBroker;
    property Service: String read FService write FService;
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils,
  Grijjy.Bson,
  Grijjy.CloudLogging;

{ TSettings }

constructor TSettings.Create;
begin
  inherited Create;
  {$IF Defined(IOS) or Defined(ANDROID)}
  FFilename := TPath.Combine(TPath.GetDocumentsPath, 'Settings.json');
  {$ELSE}
  FFilename := TPath.Combine(TPath.GetHomePath, string.Join(TPath.DirectorySeparatorChar,['Grijjy','LogClientFMX','Settings.json']));
  ForceDirectories(TPath.GetDirectoryName(FFilename));
  {$ENDIF}

  FBroker := GrijjyLog.DEFAULT_BROKER;
  FService := GrijjyLog.DEFAULT_SERVICE;
  Load;
end;

destructor TSettings.Destroy;
begin
  Save;
  inherited;
end;

procedure TSettings.Load;
var
  Doc: TgoBsonDocument;
begin
  if (TFile.Exists(FFilename)) then
  begin
    Doc := TgoBsonDocument.LoadFromJsonFile(FFilename);
    FBroker := Doc.Get(KEY_BROKER, GrijjyLog.DEFAULT_BROKER);
    FService := Doc.Get(KEY_SERVICE, GrijjyLog.DEFAULT_SERVICE);
  end;
end;

procedure TSettings.Save;
var
  Doc: TgoBsonDocument;
begin
  Doc := TgoBsonDocument.Create;
  Doc[KEY_BROKER] := FBroker;
  Doc[KEY_SERVICE] := FService;
  Doc.SaveToJsonFile(FFilename, TgoJsonWriterSettings.Pretty);
end;

end.

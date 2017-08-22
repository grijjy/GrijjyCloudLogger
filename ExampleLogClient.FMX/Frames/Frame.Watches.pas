unit Frame.Watches;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Messaging,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TFrameWatches = class(TFrame)
    LabelInstructions1: TLabel;
    LabelInstructions2: TLabel;
    LabelCustomWatch: TLabel;
    TrackBar: TTrackBar;
    procedure TrackBarChange(Sender: TObject);
  private
    { Private declarations }
    procedure HandleLiveWatches(const Sender: TObject; const M: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses
  Grijjy.CloudLogging;

constructor TFrameWatches.Create(AOwner: TComponent);
begin
  inherited;
  { Subscribe to the Live Watches message to provide the Grijjy Log Viewer with
    a list of watches.
    Besides this frame, TFormMain also subscribes to this message. }
  TMessageManager.DefaultManager.SubscribeToMessage(TgoLiveWatchesMessage,
    HandleLiveWatches);
end;

destructor TFrameWatches.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TgoLiveWatchesMessage,
    HandleLiveWatches);
  inherited;
end;

procedure TFrameWatches.HandleLiveWatches(const Sender: TObject;
  const M: TMessage);
var
  Msg: TgoLiveWatchesMessage absolute M;
begin
  Assert(M is TgoLiveWatchesMessage);
  Msg.Add('Custom Watch', TrackBar.Value, 1);
end;

procedure TFrameWatches.TrackBarChange(Sender: TObject);
begin
  LabelCustomWatch.Text := Format('Custom watch: %.1f', [TrackBar.Value]);
end;

end.

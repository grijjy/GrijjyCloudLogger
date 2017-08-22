program LogClientFMX;

uses
  Grijjy.CloudLogging.InstanceTracker,
  System.StartUpCopy,
  FMX.Forms,
  SampleClasses in '..\ExampleLogClient.Shared\SampleClasses.pas',
  Settings in 'Settings.pas',
  Frame.Basic in 'Frames\Frame.Basic.pas' {FrameBasic: TFrame},
  Frame.DataTypes in 'Frames\Frame.DataTypes.pas' {FrameDataTypes: TFrame},
  Frame.Other in 'Frames\Frame.Other.pas' {FrameOther: TFrame},
  Frame.Tracking in 'Frames\Frame.Tracking.pas' {FrameTracking: TFrame},
  Frame.Watches in 'Frames\Frame.Watches.pas' {FrameWatches: TFrame},
  Form.Main in 'Forms\Form.Main.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

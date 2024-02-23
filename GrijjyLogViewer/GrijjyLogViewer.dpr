program GrijjyLogViewer;

uses
  Vcl.Forms,
  Form.Main in 'Forms\Form.Main.pas' {FormMain},
  LogMessages in 'LogMessages.pas',
  Utils in 'Utils.pas',
  Form.Connect in 'Forms\Form.Connect.pas' {FormConnect},
  Settings in 'Settings.pas',
  Form.Options in 'Forms\Form.Options.pas' {FormOptions},
  Processes in 'Processes.pas',
  LogProtocol in 'LogProtocol.pas',
  Frame.Messages in 'Frames\Frame.Messages.pas' {FrameMessages: TFrame},
  Frame.Memory in 'Frames\Frame.Memory.pas' {FrameMemory: TFrame},
  Usage in 'Usage.pas',
  Frame.Watches in 'Frames\Frame.Watches.pas' {FrameWatches: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

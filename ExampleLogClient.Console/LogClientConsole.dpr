program LogClientConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Grijjy.CloudLogging.InstanceTracker,
  System.SysUtils,
  SampleClasses in '..\ExampleLogClient.Shared\SampleClasses.pas',
  Examples in 'Examples.pas';

procedure Run;
var
  Examples: TExamples;
begin
  Examples := TExamples.Create;
  try
    Examples.Run;
  finally
    Examples.Free;
  end;
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

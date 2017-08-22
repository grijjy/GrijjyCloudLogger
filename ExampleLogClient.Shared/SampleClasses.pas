unit SampleClasses;

interface

type
  TSampleFoo = class(TObject)
  private class var
    FNextId: Integer;
  private
    FId: Integer;
  public
    constructor Create;
    function ToString: String; override;

    procedure SomeMethod;
  end;

  TSampleBar = class(TSampleFoo)
  public
    function ToString: String; override;
  end;

procedure SomeProcedure;

implementation

uses
  System.SysUtils,
  Grijjy.CloudLogging;

procedure SomeProcedure;
begin
  GrijjyLog.EnterMethod('SomeProcedure');
  GrijjyLog.Send('Inside SomeProcedure', TgoLogLevel.Error);
  GrijjyLog.ExitMethod('SomeProcedure');
end;

{ TSampleFoo }

constructor TSampleFoo.Create;
begin
  inherited;
  FId := FNextId;
  Inc(FNextId);
end;

procedure TSampleFoo.SomeMethod;
begin
  GrijjyLog.EnterMethod(Self, 'SomeMethod');
  GrijjyLog.Send('Inside TSampleFoo.SomeMethod', TgoLogLevel.Warning);
  SomeProcedure;
  GrijjyLog.ExitMethod(Self, 'SomeMethod');
end;

function TSampleFoo.ToString: String;
begin
  Result := Format('Foo #%d', [FId]);
end;

{ TSampleBar }

function TSampleBar.ToString: String;
begin
  Result := Format('Bar #%d', [FId]);
end;

end.

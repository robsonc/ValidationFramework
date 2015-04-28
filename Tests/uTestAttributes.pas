unit uTestAttributes;

interface
uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestValidationAttributes = class(TObject) 
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  end;

implementation

procedure TTestValidationAttributes.Setup;
begin
end;

procedure TTestValidationAttributes.TearDown;
begin
end;


initialization
  TDUnitX.RegisterTestFixture(TTestValidationAttributes);
end.

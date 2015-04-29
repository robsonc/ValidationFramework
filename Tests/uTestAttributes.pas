unit uTestAttributes;

interface
uses
  DUnitX.TestFramework, uValidationFramework;

type
  TNotBlankClass = class
  private
    FblankString: String;
    procedure SetblankString(const Value: String);
  public
    [NotBlank]
    property blankString: String read FblankString write SetblankString;
  end;

  TNotNullClass = class
  private
    Fobj: TObject;
    procedure Setobj(const Value: TObject);
  public
    [NotNull]
    property obj: TObject read Fobj write Setobj;
  end;

  TNullClass = class
  private
    Fobj: TObject;
    procedure Setobj(const Value: TObject);
  public
    [Null]
    property obj: TObject read Fobj write Setobj;
  end;

  [TestFixture]
  TTestValidationAttributes = class(TObject)
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  end;

  [TextFixture]
  TTestNotBlankAttribute = class(TObject)
  private
    FValidator: IValidator;
  public
    [Setup]
    procedure Setup;
    [Test]
    procedure testBlankString;
    [Test]
    procedure testNotBlankString;
    [Test]
    procedure testEmptyString;
  end;

  [TestFixture]
  TTestNotNullAttribute = class
  private
    FValidator: IValidator;
  public
    [Setup]
    procedure Setup;
    [Test]
    procedure TestNullObjectIsInvalid;
    [Test]
    procedure TestNotNullObjectIsValid;
  end;

  [TestFixture]
  TTestNullAttribute = class
  private
    FValidator: IValidator;
  public
    [Setup]
    procedure Setup;
    [Test]
    procedure TestNullObjectIsValid;
    [Test]
    procedure TestNotNullObjectIsInvalid;
  end;

implementation

procedure TTestValidationAttributes.Setup;
begin
end;

procedure TTestValidationAttributes.TearDown;
begin
end;


{ TTestNotBlankAttribute }

procedure TTestNotBlankAttribute.Setup;
begin
  FValidator := TValidator.Create;
end;

procedure TTestNotBlankAttribute.testNotBlankString;
var
  blankObj: TNotBlankClass;
  isValid: Boolean;
begin
  blankObj := TNotBlankClass.Create;
  blankObj.FblankString := 'string';

  isValid := FValidator.validate(blankObj);

  Assert.AreEqual(0, FValidator.getErrorMessages().Count);
  Assert.IsTrue(isValid);
end;

procedure TTestNotBlankAttribute.testBlankString;
var
  blankObj: TNotBlankClass;
  isValid: Boolean;
begin
  blankObj := TNotBlankClass.Create;
  blankObj.FblankString := '      ';

  isValid := FValidator.validate(blankObj);

  Assert.AreEqual('Não pode estar em branco.', FValidator.getErrorMessages()[0].Messages[0]);
  Assert.IsFalse(isValid);
end;

procedure TTestNotBlankAttribute.testEmptyString;
var
  blankObj: TNotBlankClass;
  isValid: Boolean;
begin
  blankObj := TNotBlankClass.Create;

  isValid := FValidator.validate(blankObj);

  Assert.AreEqual('Não pode estar em branco.', FValidator.getErrorMessages()[0].Messages[0]);
  Assert.IsFalse(isValid);
end;

{ TBlankClass }

procedure TNotBlankClass.SetblankString(const Value: String);
begin
  FblankString := Value;
end;

{ TNotNullClass }

procedure TNotNullClass.Setobj(const Value: TObject);
begin
  Fobj := Value;
end;

{ TTestNotNullAttribute }

procedure TTestNotNullAttribute.Setup;
begin
  FValidator := TValidator.Create;
end;

procedure TTestNotNullAttribute.TestNotNullObjectIsValid;
var
  notNullObj: TNotNullClass;
  isValid: Boolean;
begin
  notNullObj := TNotNullClass.Create;
  notNullObj.obj := TObject.Create;
  isValid := FValidator.validate(notNullObj);

  Assert.AreEqual(0, FValidator.getErrorMessages().Count);
  Assert.IsTrue(isValid);
end;

procedure TTestNotNullAttribute.TestNullObjectIsInvalid;
var
  notNullObj: TNotNullClass;
  isValid: Boolean;
begin
  notNullObj := TNotNullClass.Create;
  isValid := FValidator.validate(notNullObj);

  Assert.AreEqual('Objeto não pode ser nulo.', FValidator.getErrorMessages()[0].Messages[0]);
  Assert.IsFalse(isValid);
end;

{ TTestNullAttribute }

procedure TTestNullAttribute.Setup;
begin
  FValidator := TValidator.Create;
end;

procedure TTestNullAttribute.TestNotNullObjectIsInvalid;
var
  nullObj: TNullClass;
  isValid: Boolean;
begin
  nullObj := TNullClass.Create;
  nullObj.obj := TObject.Create;
  isValid := FValidator.validate(nullObj);

  Assert.AreEqual('Objeto deve ser nulo.', FValidator.getErrorMessages()[0].Messages[0]);
  Assert.IsFalse(isValid);
end;

procedure TTestNullAttribute.TestNullObjectIsValid;
var
  nullObj: TNullClass;
  isValid: Boolean;
begin
  nullObj := TNullClass.Create;
  isValid := FValidator.validate(nullObj);

  Assert.AreEqual(0, FValidator.getErrorMessages().Count);
  Assert.IsTrue(isValid);
end;

{ TNullClass }

procedure TNullClass.Setobj(const Value: TObject);
begin
  Fobj := Value;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestValidationAttributes);
  TDUnitX.RegisterTestFixture(TTestNotBlankAttribute);
  TDUnitX.RegisterTestFixture(TTestNotNullAttribute);

end.

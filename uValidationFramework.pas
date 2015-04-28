unit uValidationFramework;

interface

uses System.Rtti, System.TypInfo, System.SysUtils,
  System.Generics.Collections, RegularExpressions, System.Math, System.Types;

type
  IValidator = interface;
  TValidator = class;
  TErrorMessage = class;

  TValidationAttribute = class abstract(TCustomAttribute)
  protected
    FValid: Boolean;
    FErrorMessage: String;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); virtual; abstract;
  public
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean;
    function isValid(): Boolean;
    function getErrorMessage(): String;
  end;

  Required = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
  end;

  Min = class(TValidationAttribute)
  private
    FValueDouble: Double;
    FValue: Integer;
  public
    constructor Create(value: Double); overload;
    constructor Create(value: Integer); overload;
    constructor Create(value: Integer; errorMessage: String); overload;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    property Value: Integer read FValue write FValue;
  end;

  Max = class(TValidationAttribute)
  private
    FValueDouble: Double;
    FValue: Integer;
  public
    constructor Create(value: Double); overload;
    constructor Create(value: Integer); overload;
    constructor Create(value: Integer; errorMessage: String); overload;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    property Value: Integer read FValue write FValue;
  end;

  Pattern = class(TValidationAttribute)
  private
    FValue: String;
  public
    constructor Create(regex: String); overload;
    constructor Create(regex: String; errorMessage: String); overload;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    property Value: String read FValue write FValue;
  end;

  AssertTrue = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
  end;

  AssertFalse = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
  end;

  Size = class(TValidationAttribute)
  private
    FMin: Integer;
    FMax: Integer;
  public
    constructor Create; overload;
    constructor Create(min: Integer); overload;
    constructor Create(min: Integer; errorMessage: String); overload;
    constructor Create(min: Integer; max: Integer); overload;
    constructor Create(min: Integer; max: Integer; errorMessage: String); overload;
    property Min: Integer read FMin write FMin;
    property Max: Integer read FMax write FMax;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
  end;

  Future = class(TValidationAttribute)
  public
    constructor Create(errorMessage: String); overload;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
  end;

  Past = class(TValidationAttribute)
  public
    constructor Create(errorMessage: String); overload;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
  end;

  NotNull = class(TValidationAttribute)
  public
    constructor Create(errorMessage: String); overload;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
  end;

  Null = class(TValidationAttribute)
  public
    constructor Create(errorMessage: String); overload;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
  end;

  Valid = class(TValidationAttribute)
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
  end;

  NotBlank = class(TValidationAttribute)
  public
    constructor Create;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
  end;

  //digits
  //DecimalMin
  //DecimalMax

  TErrorMessage = class
  private
    FFieldName: String;
    FMessages: TList<String>;
    procedure SetFieldName(const Value: String);
    procedure SetMessages(const Value: TList<String>);
  public
    constructor Create;
    destructor Destroy; override;
    property FieldName: String read FFieldName write SetFieldName;
    property Messages: TList<String> read FMessages write SetMessages;
  end;

  IValidator = interface(IInvokable)
  ['{DB120293-9B40-44BF-88BE-816FF0C67EBE}']
    function validate(obj: TObject): Boolean;
    function getErrorMessages(): TList<TErrorMessage>;
    procedure clear();
  end;

  TValidator = class(TInterfacedObject, IValidator)
  private
    FValidatedObjects: TDictionary<Integer, TObject>;
    FListConstraintViolations: TList<TErrorMessage>;
    procedure executeValidation(member: TRttiMember;
      errorMessage: TErrorMessage; obj: TObject);
    procedure addErrorMessages(errorMessage: TErrorMessage);
  public
    constructor Create;
    destructor Destroy; override;
    function validate(obj: TObject): Boolean;
    function getErrorMessages(): TList<TErrorMessage>;
    procedure clear();
  end;

implementation

{ TErrorMessage }

constructor TErrorMessage.Create;
begin
  FMessages := TList<String>.Create;
end;

destructor TErrorMessage.Destroy;
begin
  FMessages.Free;
  inherited;
end;

procedure TErrorMessage.SetFieldName(const Value: String);
begin
  FFieldName := Value;
end;

procedure TErrorMessage.SetMessages(const Value: TList<String>);
begin
  FMessages := Value;
end;

{ TValidation }

procedure TValidator.clear;
begin
  FListConstraintViolations.Clear;
end;

constructor TValidator.Create;
begin
  FValidatedObjects := TDictionary<Integer, TObject>.Create;
  FListConstraintViolations := TObjectList<TErrorMessage>.Create;
end;

destructor TValidator.Destroy;
begin
  FValidatedObjects.Free;
  FListConstraintViolations.Free;
  inherited;
end;

function TValidator.getErrorMessages: TList<TErrorMessage>;
begin
  Result := FListConstraintViolations;
end;

function TValidator.validate(obj: TObject): Boolean;
var
  context: TRttiContext;
  rType: TRttiType;
  rField: TRttiField;
  rAttr: TCustomAttribute;
  rProperty: TRttiProperty;
  errorMessage: TErrorMessage;
  rMember: TRttiMember;
  rParameter: TRttiParameter;
begin
  if obj = nil then
  begin
    raise EArgumentNilException.Create('Objeto n�o pode ser nulo.');
  end;

  if FValidatedObjects.ContainsKey(obj.GetHashCode()) then
  begin
    Exit;
  end else
  begin
    FValidatedObjects.Add(obj.GetHashCode, obj);
  end;

  Result := true;

  rType := context.GetType(obj.ClassInfo);

  for rField in rType.GetFields do
  begin
    errorMessage := TErrorMessage.Create;
    errorMessage.FieldName := rField.Name;

    executeValidation(rField, errorMessage, obj);
    addErrorMessages(errorMessage);
  end;

 for rProperty in rType.GetProperties do
  begin
    errorMessage := TErrorMessage.Create;
    errorMessage.FieldName := rType.Name + '.' + rProperty.Name;

    executeValidation(rProperty, errorMessage, obj);
    addErrorMessages(errorMessage);
  end;

  if FListConstraintViolations.Count > 0 then
    Result := false;
end;

procedure TValidator.executeValidation(member: TRttiMember;
  errorMessage: TErrorMessage; obj: TObject);
var
  rAttr: TCustomAttribute;
begin
  for rAttr in member.GetAttributes do
  begin
    if rAttr is TValidationAttribute then
    begin
      with TValidationAttribute(rAttr) do
      begin
        execute(member, obj, Self);
        if not isValid() then
        begin
          errorMessage.Messages.Add(getErrorMessage());
        end;
      end;
    end;
  end;
end;

procedure TValidator.addErrorMessages(errorMessage: TErrorMessage);
begin
  if errorMessage.Messages.Count > 0 then
  begin
    FListConstraintViolations.Add(errorMessage);
  end else
  begin
    errorMessage.Free;
  end;
end;

{ Required }

constructor Required.Create;
begin
  FErrorMessage := 'Campo requerido.';
end;

constructor Required.Create(errorMessage: String);
begin
  Self.Create;
  FErrorMessage := errorMessage;
end;

procedure Required.doValidation(rType: TRttiType; rTypeName: String;
  value: TValue; validator: IValidator);
begin
  case rType.TypeKind of
//    tkUnknown: ;
//    tkInteger: ;
//    tkChar: ;
//    tkEnumeration: ;
//    tkFloat: ;
    tkString, tkUString, tkWString:
    begin
      if not (value.AsString <> '') then
      begin
        FValid := false;
      end;
    end;
//    tkSet: ;
//    tkClass: ;
//    tkMethod: ;
//    tkWChar: ;
//    tkLString: ;
//    tkWString: ;
//    tkVariant: ;
//    tkArray: ;
//    tkRecord: ;
//    tkInterface: ;
//    tkInt64: ;
//    tkDynArray: ;
//    tkUString: ;
//    tkClassRef: ;
//    tkPointer: ;
//    tkProcedure: ;
  end;
end;

{ Min }

constructor Min.Create(value: Integer);
begin
  FErrorMessage := 'Valor m�nimo inv�lido.';
  FValue := value;
end;

constructor Min.Create(value: Integer; errorMessage: String);
begin
  Self.Create(value);
  FErrorMessage := errorMessage;
end;

constructor Min.Create(value: Double);
begin
  FErrorMessage := 'Valor n�o pode ser menor especificado';
  FValueDouble := value;
end;

procedure Min.doValidation(rType: TRttiType; rTypeName: String; value: TValue;
  validator: IValidator);
begin
  if rType.TypeKind = tkInteger then
  begin
    if value.AsInteger < Self.FValue then
    begin
      FValid := false;
    end;
  end;

  if rType.TypeKind = tkFloat then
  begin
    if CompareValue(value.AsType<Double>, Self.FValueDouble) = LessThanValue then
    begin
      FValid := False;
    end;
  end;
end;

{ Max }

constructor Max.Create(value: Integer);
begin
  FValue := value;
  FErrorMessage := 'Valor m�ximo inv�lido.';
end;

constructor Max.Create(value: Integer; errorMessage: String);
begin
  Self.Create(value);
  FErrorMessage := errorMessage;
end;

constructor Max.Create(value: Double);
begin
  FValueDouble := value;
  FErrorMessage := 'Valor n�o pode ser maior que o especificado';
end;

procedure Max.doValidation(rType: TRttiType; rTypeName: String; value: TValue;
  validator: IValidator);
begin
  if rType.TypeKind = tkInteger then
  begin
    if value.AsInteger > Self.FValue then
    begin
      FValid := false;
    end;
  end;

  if rType.TypeKind = tkFloat then
  begin
    if CompareValue(value.AsType<Double>, Self.FValueDouble) = GreaterThanValue then
    begin
      FValid := False;
    end;
  end;
end;

{ Pattern }

constructor Pattern.Create(regex: String);
begin
  FErrorMessage := 'Valor inv�lido.';
  FValue := regex;
end;

constructor Pattern.Create(regex, errorMessage: String);
begin
  Self.Create(regex);
  FErrorMessage := errorMessage;
end;

procedure Pattern.doValidation(rType: TRttiType; rTypeName: String;
  value: TValue; validator: IValidator);
var
  regex: TRegEx;
begin
  if rType.TypeKind in [tkString, tkWString, tkUString] then
  begin
    regex := TRegEx.Create(FValue);
    if not regex.IsMatch(value.AsString) then
    begin
      FValid := false;
    end;
  end;
end;

{ AssertTrue }

constructor AssertTrue.Create;
begin
  FErrorMessage := 'Valor inv�lido. O valor precisa ser verdadeiro.';
end;

constructor AssertTrue.Create(errorMessage: String);
begin
  Self.Create;
  FErrorMessage := errorMessage;
end;

procedure AssertTrue.doValidation(rType: TRttiType; rTypeName: String;
  value: TValue; validator: IValidator);
begin
  if not value.AsBoolean then
  begin
    FValid := false;
  end;
end;

{ AssertFalse }

constructor AssertFalse.Create;
begin
  FErrorMessage := 'Valor inv�lido. O valor precisa ser falso.';
end;

constructor AssertFalse.Create(errorMessage: String);
begin
  Self.Create;
  FErrorMessage := errorMessage;
end;

procedure AssertFalse.doValidation(rType: TRttiType; rTypeName: String;
  value: TValue; validator: IValidator);
begin
  if value.AsBoolean then
  begin
    FValid := false;
  end;
end;

{ TValidationAttribute }

function TValidationAttribute.execute(member: TRttiMember; obj: TObject;
  validator: TValidator): Boolean;
var
  rType: TRttiType;
  value: TValue;
  rTypeName: string;
begin
  FValid := true;

  if member is TRttiField then
  begin
    rType := TRttiField(member).FieldType;
    value := TRttiField(member).GetValue(obj);
    rTypeName := TRttiField(member).FieldType.Name;
  end else if member is TRttiProperty then
  begin
    rType := TRttiProperty(member).PropertyType;
    value := TRttiProperty(member).GetValue(obj);
    rTypeName := TRttiProperty(member).PropertyType.Name;
  end;

  Self.doValidation(rType, rTypeName, value, validator);
end;

function TValidationAttribute.getErrorMessage: String;
begin
  Result := FErrorMessage;
end;

function TValidationAttribute.isValid: Boolean;
begin
  Result := FValid;
end;

{ Size }

constructor Size.Create;
begin
  raise Exception.Create('Voc� deve especificar os valores min ou max.');
end;

constructor Size.Create(min: Integer);
begin
  FMin := min;
end;

constructor Size.Create(min: Integer; errorMessage: String);
begin
  Self.Create(min);
  FErrorMessage := errorMessage;
end;

constructor Size.Create(min: Integer; max: Integer);
begin
  Self.Create(min);
  FMax := max;
end;

constructor Size.Create(min: Integer; max: Integer; errorMessage: String);
begin
  Self.Create(min, max);
  FErrorMessage := errorMessage;
end;

procedure Size.doValidation(rType: TRttiType; rTypeName: String; value: TValue;
  validator: IValidator);
var
  fieldLength: Integer;
begin
  if rType.TypeKind in [tkString, tkWString, tkUString] then
  begin
    fieldLength := Length(value.AsString);

    if FMax = 0 then
    begin
      FValid := fieldLength >= FMin;
      Exit;
    end;

    if (fieldLength > FMax) or (fieldLength < FMin) then
      FValid := false;
  end;
end;

{ Future }

constructor Future.Create(errorMessage: String);
begin
  Self.Create;
  FErrorMessage := errorMessage;
end;

procedure Future.doValidation(rType: TRttiType; rTypeName: String;
  value: TValue; validator: IValidator);
begin
  if (rTypeName = 'TDate') or (rTypeName = 'TDateTime') then
  begin
    if value.AsType<TDate> <= Date then
      FValid := false;
  end;
end;

{ Past }

constructor Past.Create(errorMessage: String);
begin
  Self.Create;
  FErrorMessage := errorMessage;
end;

procedure Past.doValidation(rType: TRttiType; rTypeName: String; value: TValue;
  validator: IValidator);
begin
  if (rTypeName = 'TDate') or (rTypeName = 'TDateTime') then
  begin
    if value.AsType<TDate> >= Date then
      FValid := false;
  end;
end;

{ NotNull }

constructor NotNull.Create(errorMessage: String);
begin
  FErrorMessage := errorMessage;
end;

procedure NotNull.doValidation(rType: TRttiType; rTypeName: String;
  value: TValue; validator: IValidator);
begin
  if rType.TypeKind = tkClass then
  begin
    if value.AsObject = nil then
    begin
      FValid := false;
    end;
  end;
end;

{ Null }

constructor Null.Create(errorMessage: String);
begin
  FErrorMessage := errorMessage;
end;

procedure Null.doValidation(rType: TRttiType; rTypeName: String; value: TValue;
  validator: IValidator);
begin
  if rType.TypeKind = tkClass then
  begin
    if not (value.AsObject = nil) then
    begin
      FValid := false;
    end;
  end;
end;

{ Valid }

procedure Valid.doValidation(rType: TRttiType; rTypeName: String; value: TValue;
  validator: IValidator);
begin
  FValid := True;
  if rType.TypeKind = tkClass then
  begin
    if not (value.AsObject = nil) then
    begin
      validator.validate(value.AsObject)
    end;
  end;
end;

{ NotBlank }

constructor NotBlank.Create;
begin
  FErrorMessage := 'N�o pode estar em branco.';
end;

procedure NotBlank.doValidation(rType: TRttiType; rTypeName: String;
  value: TValue; validator: IValidator);
begin
  if rType.TypeKind in [tkString, tkWString, tkUString] then
  begin
    if Length(Trim(value.AsString)) = 0 then
    begin
      FValid := False;
    end;
  end;
end;

end.

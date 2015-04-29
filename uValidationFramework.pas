unit uValidationFramework;

interface

uses System.Rtti, System.TypInfo, System.SysUtils,
  System.Generics.Collections, RegularExpressions, System.Math, System.Types;

type
  TValidationAttribute = class;
  IValidator = interface;
  TValidator = class;
  TValidationViewHelper = class;

  Required = class;
  Min = class;
  Max = class;
  Pattern = class;
  AssertTrue = class;
  AssertFalse = class;
  Size = class;
  Future = class;
  Past = class;
  NotNull = class;
  Null = class;
  Valid = class;
  NotBlank = class;

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
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
  end;

  Null = class(TValidationAttribute)
  public
    constructor Create; overload;
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
  //NotEmpty
  //Range
  //Length
  //URL
  //CreditCardNumber
  //EAN13
  //CPF
  //CNPJ

  IValidator = interface(IInvokable)
  ['{DB120293-9B40-44BF-88BE-816FF0C67EBE}']
    function validate(obj: TObject): Boolean;
    function getErrorMessages(): TObjectDictionary<string, TList<string>>;
    function getFirstErrorMessage(memberId: String): String;
    function hasErrorMessages(memberId: String): Boolean;
    procedure clear();
  end;

  TValidator = class(TInterfacedObject, IValidator)
  private
    FValidatedObjects: TDictionary<Integer, TObject>;
    FListConstraintViolations: TObjectDictionary<string, TList<string>>;
    procedure doValidation(member: TRttiMember; obj: TObject; memberId: String);
  public
    constructor Create;
    destructor Destroy; override;
    function validate(obj: TObject): Boolean;
    function getErrorMessages(): TObjectDictionary<string, TList<string>>;
    function getFirstErrorMessage(memberId: String): String;
    function hasErrorMessages(memberId: String): Boolean;
    procedure clear();
  end;

  IValidationViewHelper = interface
  ['{AF90779E-D4A8-4729-8008-9D51255B6E4A}']
    function showValidationFor(fieldName: String): TValidationViewHelper;
    procedure withLabel(fieldLabel: String);
    function result(): String;
  end;

  TValidationViewHelper = class(TInterfacedObject, IValidationViewHelper)
  strict private
    FCurrentField: String;
    FMessages: TDictionary<String, String>;
    FValidator: IValidator;
    FStringBuilder: TStringBuilder;
  public
    constructor Create(validator: IValidator);
    destructor Destroy; override;
    function showValidationFor(fieldName: String): TValidationViewHelper;
    procedure withLabel(fieldLabel: String);
    function result(): String;
  end;

implementation

{ TValidation }

procedure TValidator.doValidation(member: TRttiMember; obj: TObject; memberId: String);
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
          if not FListConstraintViolations.ContainsKey(memberId) then
            FListConstraintViolations.Add(memberId, TList<String>.Create);
          FListConstraintViolations.Items[memberId].Add(getErrorMessage());
        end;
      end;
    end;
  end;
end;

procedure TValidator.clear;
begin
  FListConstraintViolations.Clear;
end;

constructor TValidator.Create;
begin
  FValidatedObjects := TDictionary<Integer, TObject>.Create;
  FListConstraintViolations := TObjectDictionary<string, TList<string>>.Create([doOwnsValues]);
end;

destructor TValidator.Destroy;
begin
  FValidatedObjects.Free;
  FListConstraintViolations.Free;
  inherited;
end;

function TValidator.getErrorMessages: TObjectDictionary<string, TList<string>>;
begin
  Result := FListConstraintViolations;
end;

function TValidator.getFirstErrorMessage(memberId: String): String;
begin
  if FListConstraintViolations.ContainsKey(memberId) then
    Result := FListConstraintViolations.Items[memberId][0];
end;

function TValidator.hasErrorMessages(memberId: String): Boolean;
begin
  Result := False;
  if FListConstraintViolations.ContainsKey(memberId) then
    Result := True;
end;

function TValidator.validate(obj: TObject): Boolean;
var
  context: TRttiContext;
  rType: TRttiType;
  rField: TRttiField;
  rAttr: TCustomAttribute;
  rProperty: TRttiProperty;
  rMember: TRttiMember;
  rParameter: TRttiParameter;
  memberId: String;
begin
  if obj = nil then
  begin
    raise EArgumentNilException.Create('Objeto não pode ser nulo.');
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
    memberId := rType.Name + '.' + rField.Name;
    Self.doValidation(rField, obj, memberId);
  end;

  for rProperty in rType.GetProperties do
  begin
    memberId := rType.Name + '.' + rProperty.Name;
    Self.doValidation(rProperty, obj, memberId);
  end;

  if FListConstraintViolations.Count > 0 then
    Result := false;
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
  FErrorMessage := 'Valor mínimo inválido.';
  FValue := value;
end;

constructor Min.Create(value: Integer; errorMessage: String);
begin
  Self.Create(value);
  FErrorMessage := errorMessage;
end;

constructor Min.Create(value: Double);
begin
  FErrorMessage := 'Valor não pode ser menor especificado';
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
  FErrorMessage := 'Valor máximo inválido.';
end;

constructor Max.Create(value: Integer; errorMessage: String);
begin
  Self.Create(value);
  FErrorMessage := errorMessage;
end;

constructor Max.Create(value: Double);
begin
  FValueDouble := value;
  FErrorMessage := 'Valor não pode ser maior que o especificado';
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
  FErrorMessage := 'Valor inválido.';
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
  FErrorMessage := 'Valor inválido. O valor precisa ser verdadeiro.';
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
  FErrorMessage := 'Valor inválido. O valor precisa ser falso.';
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
  raise Exception.Create('Você deve especificar os valores min ou max.');
end;

constructor Size.Create(min: Integer);
begin
  FErrorMessage := 'Tamanho inválido.';
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

constructor NotNull.Create;
begin
  FErrorMessage := 'Objeto não pode ser nulo.';
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

constructor Null.Create;
begin
  FErrorMessage := 'Objeto deve ser nulo.';
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
  FErrorMessage := 'Não pode estar em branco.';
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

{ TValidationViewHelper }

constructor TValidationViewHelper.Create(validator: IValidator);
begin
  FMessages := TDictionary<String, String>.Create;
  FValidator := validator;
  FStringBuilder := TStringBuilder.Create;
end;

destructor TValidationViewHelper.Destroy;
begin
  FMessages.Free;
  FStringBuilder.Free;
  inherited;
end;

function TValidationViewHelper.showValidationFor(fieldName: String): TValidationViewHelper;
begin
  FCurrentField := fieldName;
  FMessages.Add(fieldName, String.Empty);
  result := Self;
end;

procedure TValidationViewHelper.withLabel(fieldLabel: String);
begin
  FMessages.Items[FCurrentField] := fieldLabel;
end;

function TValidationViewHelper.result(): String;
var
  key: String;
begin
  for key in FMessages.Keys do
  begin
    if FValidator.hasErrorMessages(key) then
      FStringBuilder.AppendLine(FMessages.Items[key] + ': ' + FValidator.getFirstErrorMessage(key));
  end;

  Result := FStringBuilder.ToString;
end;

end.

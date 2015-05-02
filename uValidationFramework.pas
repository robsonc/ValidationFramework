unit uValidationFramework;

interface

uses System.Rtti, System.TypInfo, System.SysUtils,
  System.Generics.Collections, RegularExpressions, System.Math, System.Types,
  uValidation.Framework.Attributes;

type
  IValidator = interface;
  TValidationAttribute = class;
  TValidator = class;
  TValidationViewHelper = class;

  TValidationAttribute = class abstract
  protected
    FValid: Boolean;
    FErrorMessage: String;
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); virtual; abstract;
  public
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean;
    function isValid(): Boolean;
    function getErrorMessage(): String;
    procedure visit(vAttribute: ValidationAttribute); virtual; abstract;
  end;

  TRequired = class(TValidationAttribute)
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    procedure visit(vAttribute: ValidationAttribute); override;
  end;

  TMin = class(TValidationAttribute)
  strict private
    FValueDouble: Double;
    FValue: Integer;
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    procedure visit(vAttribute: ValidationAttribute); override;
  end;

  TMax = class(TValidationAttribute)
  strict private
    FValueDouble: Double;
    FValue: Integer;
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    procedure visit(vAttribute: ValidationAttribute); override;
  end;

  TPattern = class(TValidationAttribute)
  strict private
    FValue: String;
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    procedure visit(vAttribute: ValidationAttribute); override;
  end;

  TAssertTrue = class(TValidationAttribute)
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    procedure visit(vAttribute: ValidationAttribute); override;
  end;

  TAssertFalse = class(TValidationAttribute)
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    procedure visit(vAttribute: ValidationAttribute); override;
  end;

  TSize = class(TValidationAttribute)
  strict private
    FMin: Integer;
    FMax: Integer;
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    procedure visit(vAttribute: ValidationAttribute); override;
  end;

  TFuture = class(TValidationAttribute)
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    procedure visit(vAttribute: ValidationAttribute); override;
  end;

  TPast = class(TValidationAttribute)
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    procedure visit(vAttribute: ValidationAttribute); override;
  end;

  TNotNull = class(TValidationAttribute)
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    procedure visit(vAttribute: ValidationAttribute); override;
  end;

  TNull = class(TValidationAttribute)
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    procedure visit(vAttribute: ValidationAttribute); override;
  end;

  TValid = class(TValidationAttribute)
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    procedure visit(vAttribute: ValidationAttribute); override;
  end;

  TNotBlank = class(TValidationAttribute)
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    procedure visit(vAttribute: ValidationAttribute); override;
  end;

  TValidEmail = class(TValidationAttribute)
  public
    procedure doValidation(rType: TRttiType; rTypeName: String; value: TValue;
      validator: IValidator); override;
    procedure visit(vAttribute: ValidationAttribute); override;
  end;

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

  TValidatorManager = class abstract
  private
    class var FRegisteredValidators: TObjectDictionary<String, TValidationAttribute>;
  public
    class procedure registerValidator(validationAttribute: TClass;
      validationExecutor: TValidationAttribute); static;
    class procedure unregisterValidators; static;
    class function getExecutor(validationAttribute: String): TValidationAttribute; static;
  end;

implementation

{ TValidation }

procedure TValidator.doValidation(member: TRttiMember; obj: TObject; memberId: String);
var
  rAttr: TCustomAttribute;
  executor: TValidationAttribute;
  vAttribute: ValidationAttribute;
begin
  for rAttr in member.GetAttributes do
  begin
    if rAttr is ValidationAttribute then
    begin
      vAttribute := rAttr as ValidationAttribute;

      executor := TValidatorManager.getExecutor(vAttribute.ClassName);
      executor.visit(vAttribute);
      executor.execute(member, obj, Self);

      if not executor.isValid() then
      begin
        if not FListConstraintViolations.ContainsKey(memberId) then
          FListConstraintViolations.Add(memberId, TList<String>.Create);
        FListConstraintViolations.Items[memberId].Add(vAttribute.ErrorMessage);
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

{ TRequired }

procedure TRequired.doValidation(rType: TRttiType; rTypeName: String;
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

procedure TRequired.visit(vAttribute: ValidationAttribute);
begin

end;

{ TMin }

procedure TMin.visit(vAttribute: ValidationAttribute);
begin
  FValue := Min(vAttribute).Value;
  FValueDouble := Min(vAttribute).ValueDouble;
end;

procedure TMin.doValidation(rType: TRttiType; rTypeName: String; value: TValue;
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

{ TMax }

procedure TMax.visit(vAttribute: ValidationAttribute);
begin
  FValue := Max(vAttribute).Value;
  FValueDouble := Max(vAttribute).ValueDouble;
end;

procedure TMax.doValidation(rType: TRttiType; rTypeName: String; value: TValue;
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

{ TPattern }

procedure TPattern.visit(vAttribute: ValidationAttribute);
begin
  FValue := Pattern(vAttribute).Value;
end;

procedure TPattern.doValidation(rType: TRttiType; rTypeName: String;
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

{ TAssertTrue }

procedure TAssertTrue.visit(vAttribute: ValidationAttribute);
begin

end;

procedure TAssertTrue.doValidation(rType: TRttiType; rTypeName: String;
  value: TValue; validator: IValidator);
begin
  if not value.AsBoolean then
  begin
    FValid := false;
  end;
end;

{ TAssertFalse }

procedure TAssertFalse.visit(vAttribute: ValidationAttribute);
begin

end;

procedure TAssertFalse.doValidation(rType: TRttiType; rTypeName: String;
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

{ TSize }

procedure TSize.visit(vAttribute: ValidationAttribute);
begin
  FMin := Size(vAttribute).Min;
  FMax := Size(vAttribute).Max;
end;

procedure TSize.doValidation(rType: TRttiType; rTypeName: String; value: TValue;
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

{ TFuture }

procedure TFuture.visit(vAttribute: ValidationAttribute);
begin

end;

procedure TFuture.doValidation(rType: TRttiType; rTypeName: String;
  value: TValue; validator: IValidator);
begin
  if (rTypeName = 'TDate') or (rTypeName = 'TDateTime') then
  begin
    if value.AsType<TDate> <= Date then
      FValid := false;
  end;
end;

{ TPast }

procedure TPast.visit(vAttribute: ValidationAttribute);
begin

end;

procedure TPast.doValidation(rType: TRttiType; rTypeName: String; value: TValue;
  validator: IValidator);
begin
  if (rTypeName = 'TDate') or (rTypeName = 'TDateTime') then
  begin
    if value.AsType<TDate> >= Date then
      FValid := false;
  end;
end;

{ TNotNull }

procedure TNotNull.visit(vAttribute: ValidationAttribute);
begin

end;

procedure TNotNull.doValidation(rType: TRttiType; rTypeName: String;
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

{ TNull }

procedure TNull.visit(vAttribute: ValidationAttribute);
begin

end;

procedure TNull.doValidation(rType: TRttiType; rTypeName: String; value: TValue;
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

{ TValid }

procedure TValid.doValidation(rType: TRttiType; rTypeName: String; value: TValue;
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

{ TValid }

procedure TValid.visit(vAttribute: ValidationAttribute);
begin

end;

{ TNotBlank }

procedure TNotBlank.visit(vAttribute: ValidationAttribute);
begin

end;

procedure TNotBlank.doValidation(rType: TRttiType; rTypeName: String;
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

{ TValidEmail }

procedure TValidEmail.visit(vAttribute: ValidationAttribute);
begin

end;

procedure TValidEmail.doValidation(rType: TRttiType; rTypeName: String;
  value: TValue; validator: IValidator);
var
  regex: TRegEx;
begin
  if rType.TypeKind in [tkString, tkWString, tkUString] then
  begin
    regex := TRegEx.Create('^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,4}$');
    if not regex.IsMatch(value.AsString) then
      FValid := false;
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

class procedure TValidatorManager.registerValidator(validationAttribute: TClass;
      validationExecutor: TValidationAttribute);
begin
  if TValidatorManager.FRegisteredValidators = nil then
  begin
    TValidatorManager.FRegisteredValidators := TObjectDictionary<String, TValidationAttribute>.Create([doOwnsValues]);
  end;

  if not TValidatorManager.FRegisteredValidators.ContainsKey(validationAttribute.ClassName) then
  begin
    TValidatorManager.FRegisteredValidators
      .Add(validationAttribute.ClassName, validationExecutor);
  end;
end;

class procedure TValidatorManager.unregisterValidators();
begin
  TValidatorManager.FRegisteredValidators.Free;
end;

class function TValidatorManager.getExecutor(validationAttribute: String): TValidationAttribute;
begin
  Result := FRegisteredValidators[validationAttribute];
end;

initialization
  TValidatorManager.registerValidator(Required, TRequired.Create);
  TValidatorManager.registerValidator(Min, TMin.Create);
  TValidatorManager.registerValidator(Max, TMax.Create);
  TValidatorManager.registerValidator(Pattern, TPattern.Create);
  TValidatorManager.registerValidator(AssertTrue, TAssertTrue.Create);
  TValidatorManager.registerValidator(AssertFalse, TAssertFalse.Create);
  TValidatorManager.registerValidator(Size, TSize.Create);
  TValidatorManager.registerValidator(Future, TFuture.Create);
  TValidatorManager.registerValidator(Past, TPast.Create);
  TValidatorManager.registerValidator(NotNull, TNotNull.Create);
  TValidatorManager.registerValidator(Null, TNull.Create);
  TValidatorManager.registerValidator(Valid, TValid.Create);
  TValidatorManager.registerValidator(NotBlank, TNotBlank.Create);
  TValidatorManager.registerValidator(ValidEmail, TValidEmail.Create);

finalization
  TValidatorManager.unregisterValidators();

end.

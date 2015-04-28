unit uValidationFramework;

interface

uses System.Rtti, System.TypInfo, System.SysUtils,
  System.Generics.Collections, RegularExpressions;

type
  TValidator = class;

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

  TValidationAttribute = class abstract(TCustomAttribute)
  protected
    FValid: Boolean;
    FErrorMessage: String;
  public
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean; virtual; abstract;
    function isValid(): Boolean;
    function getErrorMessage(): String;
  end;

  Required = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean; override;
  end;

  Min = class(TValidationAttribute)
  private
    FValue: Integer;
  public
    constructor Create(value: Integer); overload;
    constructor Create(value: Integer; errorMessage: String); overload;
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean; override;
    property Value: Integer read FValue write FValue;
  end;

  Max = class(TValidationAttribute)
  private
    FValue: Integer;
  public
    constructor Create(value: Integer); overload;
    constructor Create(value: Integer; errorMessage: String); overload;
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean; override;
    property Value: Integer read FValue write FValue;
  end;

  Pattern = class(TValidationAttribute)
  private
    FValue: String;
  public
    constructor Create(regex: String); overload;
    constructor Create(regex: String; errorMessage: String); overload;
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean; override;
    property Value: String read FValue write FValue;
  end;

  AssertTrue = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean; override;
  end;

  AssertFalse = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean; override;
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
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean; override;
  end;

  Future = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean; override;
  end;

  Past = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean; override;
  end;

  NotNull = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean; override;
  end;

  Null = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean; override;
  end;

  Valid = class(TValidationAttribute)
  public
    constructor Create;
    function execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean; override;
  end;

  //digits
  //DecimalMin
  //DecimalMax

  IValidator = interface(IInvokable)
  ['{DB120293-9B40-44BF-88BE-816FF0C67EBE}']
    function validate(obj: TObject): Boolean;
    function getErrorMessages(): TList<TErrorMessage>;
    procedure clear();
  end;

  TValidator = class(TInterfacedObject, IValidator)
  private
    FErrorMessages: TList<TErrorMessage>;
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

{ TValidation }

procedure TValidator.clear;
begin
  FErrorMessages.Clear;
end;

constructor TValidator.Create;
begin
  FErrorMessages := TObjectList<TErrorMessage>.Create;
end;

destructor TValidator.Destroy;
begin
  FErrorMessages.Free;
  inherited;
end;

function TValidator.getErrorMessages: TList<TErrorMessage>;
begin
  Result := FErrorMessages;
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
    raise EArgumentNilException.Create('Objeto não pode ser nulo.');
  end;

  Result := true;
  //FErrorMessages.Clear;

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
    errorMessage.FieldName := rProperty.Name;

    executeValidation(rProperty, errorMessage, obj);
    addErrorMessages(errorMessage);
  end;

  if FErrorMessages.Count > 0 then
    Result := false;
end;

procedure TValidator.executeValidation(member: TRttiMember;
  errorMessage: TErrorMessage; obj: TObject);
var
  localrAttr: TCustomAttribute;
begin
  for localrAttr in member.GetAttributes do
  begin
    if localrAttr is TValidationAttribute then
    begin
      TValidationAttribute(localrAttr).execute(member, obj, Self);
      if not TValidationAttribute(localrAttr).isValid then
      begin
        errorMessage.Messages.Add(TValidationAttribute(localrAttr).getErrorMessage);
      end;
    end;
  end;
end;

procedure TValidator.addErrorMessages(errorMessage: TErrorMessage);
begin
  if errorMessage.Messages.Count > 0 then
  begin
    FErrorMessages.Add(errorMessage);
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

function Required.execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean;
var
  rType: TRttiType;
  rMember: TRttiMember;
  value: TValue;
begin
  FValid := true;

  if member is TRttiField then
  begin
    rType := TRttiField(member).FieldType;
    value := TRttiField(member).GetValue(obj);
  end else if member is TRttiProperty then
  begin
    rType := TRttiProperty(member).PropertyType;
    value := TRttiProperty(member).GetValue(obj);
  end;

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

function Min.execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean;
var
  rType: TRttiType;
  value: TValue;
begin
  FValid := true;

  if member is TRttiField then
  begin
    rType := TRttiField(member).FieldType;
    value := TRttiField(member).GetValue(obj);
  end else if member is TRttiProperty then
  begin
    rType := TRttiProperty(member).PropertyType;
    value := TRttiProperty(member).GetValue(obj);
  end;

  if rType.TypeKind = tkInteger then
  begin
    if value.AsInteger < Self.FValue then
    begin
      FValid := false;
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

function Max.execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean;
var
  rType: TRttiType;
  value: TValue;
begin
  FValid := true;

  if member is TRttiField then
  begin
    rType := TRttiField(member).FieldType;
    value := TRttiField(member).GetValue(obj);
  end else if member is TRttiProperty then
  begin
    rType := TRttiProperty(member).PropertyType;
    value := TRttiProperty(member).GetValue(obj);
  end;

  if rType.TypeKind = tkInteger then
  begin
    if value.AsInteger > Self.FValue then
    begin
      FValid := false;
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

function Pattern.execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean;
var
  rType: TRttiType;
  regex: TRegEx;
  value: TValue;
begin
  FValid := true;

  if member is TRttiField then
  begin
    rType := TRttiField(member).FieldType;
    value := TRttiField(member).GetValue(obj);
  end else if member is TRttiProperty then
  begin
    rType := TRttiProperty(member).PropertyType;
    value := TRttiProperty(member).GetValue(obj);
  end;

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

function AssertTrue.execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean;
var
  rType: TRttiType;
  value: TValue;
begin
  if member is TRttiField then
  begin
    rType := TRttiField(member).FieldType;
    value := TRttiField(member).GetValue(obj);
  end else if member is TRttiProperty then
  begin
    rType := TRttiProperty(member).PropertyType;
    value := TRttiProperty(member).GetValue(obj);
  end;

  FValid := true;
  if not value.AsBoolean then
    FValid := false;
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

function AssertFalse.execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean;
var
  rType: TRttiType;
  value: TValue;
begin
  if member is TRttiField then
  begin
    rType := TRttiField(member).FieldType;
    value := TRttiField(member).GetValue(obj);
  end else if member is TRttiProperty then
  begin
    rType := TRttiProperty(member).PropertyType;
    value := TRttiProperty(member).GetValue(obj);
  end;

  FValid := true;
  if value.AsBoolean then
    FValid := false;
end;

{ TValidationAttribute }

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

end;

constructor Size.Create(min: Integer);
begin
  Self.Create;
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

function Size.execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean;
var
  rFieldType: TRttiType;
  fieldLength: Integer;
  rType: TRttiType;
  value: TValue;
begin
  FValid := true;

  if member is TRttiField then
  begin
    rType := TRttiField(member).FieldType;
    value := TRttiField(member).GetValue(obj);
  end else if member is TRttiProperty then
  begin
    rType := TRttiProperty(member).PropertyType;
    value := TRttiProperty(member).GetValue(obj);
  end;

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

{ Future }

constructor Future.Create;
begin

end;

constructor Future.Create(errorMessage: String);
begin
  Self.Create;
  FErrorMessage := errorMessage;
end;

function Future.execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean;
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

  if (rTypeName = 'TDate') or (rTypeName = 'TDateTime') then
  begin
    if value.AsType<TDate> <= Date then
      FValid := false;
  end;
end;

{ Past }

constructor Past.Create;
begin

end;

constructor Past.Create(errorMessage: String);
begin
  Self.Create;
  FErrorMessage := errorMessage;
end;

function Past.execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean;
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

  if (rTypeName = 'TDate') or (rTypeName = 'TDateTime') then
  begin
    if value.AsType<TDate> >= Date then
      FValid := false;
  end;
end;

{ NotNull }

constructor NotNull.Create;
begin

end;

constructor NotNull.Create(errorMessage: String);
begin
  FErrorMessage := errorMessage;
end;

function NotNull.execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean;
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

  if rType.TypeKind = tkClass then
  begin
    if value.AsObject = nil then
    begin
      FValid := false;
    end;
  end;

end;

{ Null }

constructor Null.Create;
begin

end;

constructor Null.Create(errorMessage: String);
begin
  FErrorMessage := errorMessage;
end;

function Null.execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean;
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

  if rType.TypeKind = tkClass then
  begin
    if not (value.AsObject = nil) then
    begin
      FValid := false;
    end;
  end;
end;

{ Valid }

constructor Valid.Create;
begin

end;

function Valid.execute(member: TRttiMember; obj: TObject; validator: TValidator): Boolean;
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

  if rType.TypeKind = tkClass then
  begin
    if not validator.validate(value.AsObject) then
    begin
      FValid := false;
    end;
  end;
end;

end.

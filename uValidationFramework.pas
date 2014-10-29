unit uValidationFramework;

interface

uses System.Rtti, CodeSiteLogging, System.TypInfo, System.SysUtils, 
  System.Generics.Collections, RegularExpressions;

type
  TErrorMessage = class
  private
    FFieldName: String;
    FMessages: TList<String>;
    procedure SetFieldName(const Value: String);
    procedure SetMessages(const Value: TList<String>);
  public
    constructor Create;
    property FieldName: String read FFieldName write SetFieldName;
    property Messages: TList<String> read FMessages write SetMessages;
  end;

  TValidationAttribute = class abstract(TCustomAttribute)
  protected
    FValid: Boolean;
    FErrorMessage: String;
  public
    function execute(field: TRttiField; obj: TObject): Boolean; virtual; abstract;
    function isValid(): Boolean;
    function getErrorMessage(): String;
  end;

  Required = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    function execute(field: TRttiField; obj: TObject): Boolean; override;
  end;

  Min = class(TValidationAttribute)
  private
    FValue: Integer;
  public
    constructor Create(value: Integer); overload;
    constructor Create(value: Integer; errorMessage: String); overload;
    function execute(field: TRttiField; obj: TObject): Boolean; override;
    property Value: Integer read FValue write FValue;
  end;

  Max = class(TValidationAttribute)
  private
    FValue: Integer;
  public
    constructor Create(value: Integer); overload;
    constructor Create(value: Integer; errorMessage: String); overload;
    function execute(field: TRttiField; obj: TObject): Boolean; override;
    property Value: Integer read FValue write FValue;
  end;

  Pattern = class(TValidationAttribute)
  private
    FValue: String;
  public
    constructor Create(regex: String); overload;
    constructor Create(regex: String; errorMessage: String); overload;
    function execute(field: TRttiField; obj: TObject): Boolean; override;
    property Value: String read FValue write FValue;
  end;

  AssertTrue = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    function execute(field: TRttiField; obj: TObject): Boolean; override;
  end;

  AssertFalse = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    function execute(field: TRttiField; obj: TObject): Boolean; override;
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
    function execute(field: TRttiField; obj: TObject): Boolean; override;
  end;

  Future = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    function execute(field: TRttiField; obj: TObject): Boolean; override;
  end;

  Past = class(TValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    function execute(field: TRttiField; obj: TObject): Boolean; override;
  end;

  //notnull
  //null
  //digits

  TValidator = class
  private
    FErrorMessages: TList<TErrorMessage>;
  public
    constructor Create;
    function validate(obj: TObject): Boolean;
    function getErrorMessages(): TList<TErrorMessage>;
  end;

implementation

{ TValidation }

uses uModel;

constructor TValidator.Create;
begin
  FErrorMessages := TList<TErrorMessage>.Create;
end;

function TValidator.getErrorMessages: TList<TErrorMessage>;
begin
  Result := FErrorMessages;
end;

function TValidator.validate(obj: TObject): Boolean;
var
  context: TRttiContext;
  rType, rFieldType: TRttiType;
  rField: TRttiField;
  rAttr: TCustomAttribute;
  errorMessage: TErrorMessage;
begin
  Result := true;
  FErrorMessages.Clear;

  rType := context.GetType(TCliente.ClassInfo);
  for rField in rType.GetFields do
  begin
    errorMessage := TErrorMessage.Create;
    errorMessage.FieldName := rField.Name;

    for rAttr in rField.GetAttributes do
    begin
      if rAttr is TValidationAttribute then
      begin
        TValidationAttribute(rAttr).execute(rField, obj);
        if not TValidationAttribute(rAttr).isValid() then
        begin
          errorMessage.Messages.Add(TValidationAttribute(rAttr).getErrorMessage());
        end;
      end;
    end;

    if errorMessage.Messages.Count > 0 then
      FErrorMessages.Add(errorMessage);
  end;

  if FErrorMessages.Count > 0 then
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

function Required.execute(field: TRttiField; obj: TObject): Boolean;
var
  rFieldType: TRttiType;
begin
  FValid := true;
  rFieldType := field.FieldType;
  case rFieldType.TypeKind of
//    tkUnknown: ;
//    tkInteger: ;
//    tkChar: ;
//    tkEnumeration: ;
//    tkFloat: ;
    tkString, tkUString, tkWString:
    begin
      CodeSite.Send(rFieldType.ToString);
      if not (field.GetValue(obj).AsString <> '') then
        FValid := false;
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

function Min.execute(field: TRttiField; obj: TObject): Boolean;
var
  rFieldType: TRttiType;
begin
  FValid := true;
  rFieldType := field.FieldType;
  if rFieldType.TypeKind = tkInteger then
  begin
    if field.GetValue(obj).AsInteger < Self.FValue then
      FValid := false;
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

function Max.execute(field: TRttiField; obj: TObject): Boolean;
var
  rFieldType: TRttiType;
begin
  FValid := true;
  rFieldType := field.FieldType;
  if rFieldType.TypeKind = tkInteger then
  begin
    if field.GetValue(obj).AsInteger > Self.FValue then
      FValid := false;
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

function Pattern.execute(field: TRttiField; obj: TObject): Boolean;
var
  rFieldType: TRttiType;
  regex: TRegEx;
begin
  FValid := true;
  rFieldType := field.FieldType;
  if rFieldType.TypeKind in [tkString, tkWString, tkUString] then
  begin
    regex := TRegEx.Create(FValue);
    if not regex.IsMatch(field.GetValue(obj).AsString) then
      FValid := false;
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

function AssertTrue.execute(field: TRttiField; obj: TObject): Boolean;
begin
  FValid := true;
  if not field.GetValue(obj).AsBoolean then
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

function AssertFalse.execute(field: TRttiField; obj: TObject): Boolean;
begin
  FValid := true;
  if field.GetValue(obj).AsBoolean then
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

function Size.execute(field: TRttiField; obj: TObject): Boolean;
var
  rFieldType: TRttiType;
  fieldLength: Integer;
begin
  FValid := true;
  rFieldType := field.FieldType;
  if rFieldType.TypeKind in [tkString, tkWString, tkUString] then
  begin
    fieldLength := Length(field.GetValue(obj).AsString);

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

function Future.execute(field: TRttiField; obj: TObject): Boolean;
begin
  FValid := true;
  if (field.FieldType.Name = 'TDate') or (field.FieldType.Name = 'TDateTime') then
  begin
    if field.GetValue(obj).AsType<TDate> <= Date then
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

function Past.execute(field: TRttiField; obj: TObject): Boolean;
begin
  FValid := true;
  if (field.FieldType.Name = 'TDate') or (field.FieldType.Name = 'TDateTime') then
  begin
    if field.GetValue(obj).AsType<TDate> >= Date then
      FValid := false;
  end;
end;

end.

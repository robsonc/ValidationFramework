unit uValidation.Framework.Attributes;

interface

uses System.SysUtils;

type
  ValidationAttribute = class(TCustomAttribute)
  protected
    FErrorMessage: String;
    procedure SetErrorMessage(const Value: String);
  public
    property ErrorMessage: String read FErrorMessage write SetErrorMessage;
  end;

  Min = class(ValidationAttribute)
  private
    FValueDouble: Double;
    FValue: Integer;
    procedure SetValue(const Value: Integer);
    procedure SetValueDouble(const Value: Double);
  public
    constructor Create(value: Double); overload;
    constructor Create(value: Integer); overload;
    constructor Create(value: Integer; errorMessage: String); overload;
    property Value: Integer read FValue write SetValue;
    property ValueDouble: Double read FValueDouble write SetValueDouble;
  end;

  Max = class(ValidationAttribute)
  private
    FValueDouble: Double;
    FValue: Integer;
    procedure SetValue(const Value: Integer);
    procedure SetValueDouble(const Value: Double);
  public
    constructor Create(value: Double); overload;
    constructor Create(value: Integer); overload;
    constructor Create(value: Integer; errorMessage: String); overload;
    property Value: Integer read FValue write SetValue;
    property ValueDouble: Double read FValueDouble write SetValueDouble;
  end;

  Pattern = class(ValidationAttribute)
  private
    FValue: String;
    procedure SetValue(const Value: String);
  public
    constructor Create(regex: String); overload;
    constructor Create(regex: String; errorMessage: String); overload;
    property Value: String read FValue write SetValue;
  end;

  AssertTrue = class(ValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
  end;

  AssertFalse = class(ValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
  end;

  Size = class(ValidationAttribute)
  private
    FMin: Integer;
    FMax: Integer;
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
  public
    constructor Create; overload;
    constructor Create(min: Integer); overload;
    constructor Create(min: Integer; errorMessage: String); overload;
    constructor Create(min: Integer; max: Integer); overload;
    constructor Create(min: Integer; max: Integer; errorMessage: String); overload;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
  end;

  Future = class(ValidationAttribute)
  public
    constructor Create(errorMessage: String); overload;
  end;

  Past = class(ValidationAttribute)
  public
    constructor Create(errorMessage: String); overload;
  end;

  NotNull = class(ValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
  end;

  Null = class(ValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
  end;

  Valid = class(ValidationAttribute)

  end;

  NotBlank = class(ValidationAttribute)
  public
    constructor Create;
  end;

  ValidEmail = class(ValidationAttribute)
  public
    //^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
  end;

  NotEmpty = class(ValidationAttribute)
  public
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
  end;

implementation

{ ValidationAttribute }

procedure ValidationAttribute.SetErrorMessage(const Value: String);
begin
  FErrorMessage := Value;
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

procedure Min.SetValue(const Value: Integer);
begin
  FValue := Value;
end;

procedure Min.SetValueDouble(const Value: Double);
begin
  FValueDouble := Value;
end;

constructor Min.Create(value: Double);
begin
  FErrorMessage := 'Valor não pode ser menor especificado';
  FValueDouble := value;
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

procedure Max.SetValue(const Value: Integer);
begin
  FValue := Value;
end;

procedure Max.SetValueDouble(const Value: Double);
begin
  FValueDouble := Value;
end;

constructor Max.Create(value: Double);
begin
  FValueDouble := value;
  FErrorMessage := 'Valor não pode ser maior que o especificado';
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

procedure Pattern.SetValue(const Value: String);
begin
  FValue := Value;
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

procedure Size.SetMax(const Value: Integer);
begin
  FMax := Value;
end;

procedure Size.SetMin(const Value: Integer);
begin
  FMin := Value;
end;

{ Future }

constructor Future.Create(errorMessage: String);
begin
  Self.Create;
  FErrorMessage := errorMessage;
end;

{ Past }

constructor Past.Create(errorMessage: String);
begin
  FErrorMessage := errorMessage;
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

{ Null }

constructor Null.Create(errorMessage: String);
begin
  FErrorMessage := errorMessage;
end;

constructor Null.Create;
begin
  FErrorMessage := 'Objeto deve ser nulo.';
end;

{ NotBlank }

constructor NotBlank.Create;
begin
  FErrorMessage := 'Não pode estar em branco.';
end;

{ ValidEmail }

constructor ValidEmail.Create;
begin
  FErrorMessage := 'E-mail inválido';
end;

constructor ValidEmail.Create(errorMessage: String);
begin
  Self.Create;
  FErrorMessage := errorMessage;
end;

{ NotEmpty }

constructor NotEmpty.Create;
begin
  FErrorMessage := 'Não pode ser vazio.';
end;

constructor NotEmpty.Create(errorMessage: String);
begin
  FErrorMessage := errorMessage;
end;

end.

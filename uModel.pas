unit uModel;

interface

uses uValidationFramework, System.Rtti, RegularExpressions, System.TypInfo;

type
  ValidEmail = class(TValidationAttribute)
  public
    //^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$
    constructor Create; overload;
    constructor Create(errorMessage: String); overload;
    function execute(field: TRttiField; obj: TObject): Boolean; override;
  end;

  TCliente = class
  private
    [Required('Este campo é obrigatório e não pode estar vazio.')]
    [Pattern('^Robson$', 'Nome inválido. O nome do indivíduo deve ser "Robson".')]
    [Size(6, 10, 'O nome de usuário deve ter no mínino 6 caracteres.')]
    FNome: String;
    [Min(18, 'Idade abaixo da idade mínima exigida de 18 anos.')]
    [Max(50)]
    Fidade: Integer;
    [Max(3)]
    FFilhos: Integer;
    [AssertTrue]
    FisCasado: Boolean;
    [Past('Data precisa ser no passado.')]
    FdataAtual: TDate;
    [ValidEmail('E-mail inválido.')]
    FEmail: String;
    procedure SetNome(const Value: String);
    procedure Setidade(const Value: Integer);
    procedure SetFilhos(const Value: Integer);
    procedure SetisCasado(const Value: Boolean);
    procedure SetdataAtual(const Value: TDate);
    procedure SetEmail(const Value: String);
  public
    property Nome: String read FNome write SetNome;
    property idade: Integer read Fidade write Setidade;
    property Filhos: Integer read FFilhos write SetFilhos;
    property isCasado: Boolean read FisCasado write SetisCasado;
    property dataAtual: TDate read FdataAtual write SetdataAtual;
    property Email: String read FEmail write SetEmail;
  end;

implementation

{ TCliente }

procedure TCliente.SetdataAtual(const Value: TDate);
begin
  FdataAtual := Value;
end;

procedure TCliente.SetEmail(const Value: String);
begin
  FEmail := Value;
end;

procedure TCliente.SetFilhos(const Value: Integer);
begin
  FFilhos := Value;
end;

procedure TCliente.Setidade(const Value: Integer);
begin
  Fidade := Value;
end;

procedure TCliente.SetisCasado(const Value: Boolean);
begin
  FisCasado := Value;
end;

procedure TCliente.SetNome(const Value: String);
begin
  FNome := Value;
end;

{ ValidEmail }

constructor ValidEmail.Create;
begin
  FValid := true;
end;

constructor ValidEmail.Create(errorMessage: String);
begin
  Self.Create;
  FErrorMessage := errorMessage;
end;

function ValidEmail.execute(field: TRttiField; obj: TObject): Boolean;
var
  regex: TRegEx;
begin
  if field.FieldType.TypeKind in [tkString, tkWString, tkUString] then
  begin
    regex := TRegEx.Create('^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,4}$');
    if not regex.IsMatch(field.GetValue(obj).AsString) then
      FValid := false;
  end;
end;

end.

unit uModel;

interface

uses uValidation.Framework.Attributes;

type
  TCliente = class;
  TTelefone = class;

  TTelefone = class
  private
    FDDD: String;
    FNumero: String;
    FCliente: TCliente;
    procedure SetDDD(const Value: String);
    procedure SetNumero(const Value: String);
    procedure SetCliente(const Value: TCliente);
  public
    [Valid]
    property Cliente: TCliente read FCliente write SetCliente;
    property DDD: String read FDDD write SetDDD;
    [NotEmpty('N�mero do Telefone � obrigat�rio.')]
    property Numero: String read FNumero write SetNumero;
  end;

  TCliente = class
  private
    FNome: String;
    Fidade: Integer;
    FFilhos: Integer;
    FisCasado: Boolean;
    FdataAtual: TDate;
    FEmail: String;
    FTelefone: TTelefone;
    FValor: Double;
    procedure SetNome(const Value: String);
    procedure Setidade(const Value: Integer);
    procedure SetFilhos(const Value: Integer);
    procedure SetisCasado(const Value: Boolean);
    procedure SetdataAtual(const Value: TDate);
    procedure SetEmail(const Value: String);
    procedure SetTelefone(const Value: TTelefone);
    procedure SetValor(const Value: Double);
  public
    //[Min(10.00)]
    property Valor: Double read FValor write SetValor;
    [NotEmpty('Este campo � obrigat�rio e n�o pode estar vazio.')]
    //[Pattern('^Robson$', 'Nome inv�lido. O nome do indiv�duo deve ser "Robson".')]
    [Size(6, 'O nome de usu�rio deve ter no m�nino 6 caracteres.')]
    //[Size]
    property Nome: String read FNome write SetNome;
    [Min(18, 'Idade abaixo da idade m�nima exigida de 18 anos.')]
    [Max(50, 'Idade acima da idade m�xima de 50 anos.')]
    property idade: Integer read Fidade write Setidade;
    [Required]
    [Min(1)]
    [Max(3, 'N�mero de filhos excede o limite de 3 filhos.')]
    property Filhos: Integer read FFilhos write SetFilhos;
    [AssertTrue('� obrigat�rio estar casado(a).')]
    property isCasado: Boolean read FisCasado write SetisCasado;
    //[Past('Data precisa ser no passado.')]
    property dataAtual: TDate read FdataAtual write SetdataAtual;
    [ValidEmail('E-mail inv�lido.')]
    property Email: String read FEmail write SetEmail;
    [NotNull('Telefone n�o pode ser nulo.')]
    [Valid]
    property Telefone: TTelefone read FTelefone write SetTelefone;
    destructor Destroy; override;
  end;

  TAssociado = class(TCliente)
  public
    [NotBlank]
    property Nome: String read FNome write SetNome;
  end;

implementation

{ TCliente }

destructor TCliente.Destroy;
begin
  if FTelefone <> nil then
  begin
    FTelefone.Free;
  end;

  inherited;
end;

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

procedure TCliente.SetTelefone(const Value: TTelefone);
begin
  FTelefone := Value;
end;

procedure TCliente.SetValor(const Value: Double);
begin
  FValor := Value;
end;

{ TTelefone }

procedure TTelefone.SetCliente(const Value: TCliente);
begin
  FCliente := Value;
end;

procedure TTelefone.SetDDD(const Value: String);
begin
  FDDD := Value;
end;

procedure TTelefone.SetNumero(const Value: String);
begin
  FNumero := Value;
end;

end.

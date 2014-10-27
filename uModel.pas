unit uModel;

interface

uses uValidationFramework;

type
  TCliente = class
  private
    [Required('Este campo é obrigatório e não pode estar vazio.')]
    [Pattern('^Robson$', 'Nome inválido. O nome do indivíduo deve ser "Robson".')]
    [Size(6, 6, 'O tamanho do nome deve ser de exatos 6 caracteres.')]
    FNome: String;
    [Min(18, 'Idade abaixo da idade mínima exigida de 18 anos.')]
    [Max(50)]
    Fidade: Integer;
    [Max(3)]
    FFilhos: Integer;
    [AssertTrue] [Test]
    FisCasado: Boolean;
    procedure SetNome(const Value: String);
    procedure Setidade(const Value: Integer);
    procedure SetFilhos(const Value: Integer);
    procedure SetisCasado(const Value: Boolean);
  public
    property Nome: String read FNome write SetNome;
    property idade: Integer read Fidade write Setidade;
    property Filhos: Integer read FFilhos write SetFilhos;
    property isCasado: Boolean read FisCasado write SetisCasado;
  end;

implementation

{ TCliente }

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

end.

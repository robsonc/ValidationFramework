unit uFrmPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uModel,
  uValidationFramework, Vcl.Bind.GenData, System.Rtti, System.Bindings.Outputs,
  Vcl.Bind.Editors, Data.Bind.EngExt, Vcl.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.ObjectScope, RegularExpressions, System.TypInfo, System.Generics.Collections;

type
  TForm1 = class(TForm)
    editNome: TEdit;
    lbNome: TLabel;
    btnSalvar: TButton;
    lbErrorMessage: TLabel;
    editIdade: TEdit;
    Label1: TLabel;
    lbIdadeError: TLabel;
    lbFilhos: TLabel;
    editFilhos: TEdit;
    lbFilhosError: TLabel;
    ckCasado: TCheckBox;
    lbCasadoError: TLabel;
    lbEmail: TLabel;
    editEmail: TEdit;
    lbEmailError: TLabel;
    BalloonHint1: TBalloonHint;
    Label2: TLabel;
    editTelefone: TEdit;
    lblTelefoneError: TLabel;
    Memo1: TMemo;
    procedure btnSalvarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure initControls;
    procedure exibeErros(validator: IValidator); overload;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnSalvarClick(Sender: TObject);
var
  cliente: TCliente;
  validator: IValidator;
  telefone: TTelefone;
begin
  initControls();

  validator := TValidator.Create;

  cliente := TAssociado.Create;
  try
    cliente.Nome := editNome.Text;
    cliente.idade := StrToInt(editIdade.Text);
    cliente.Filhos := StrToInt(editFilhos.Text);
    cliente.isCasado := ckCasado.Checked;
    cliente.dataAtual := Date;
    cliente.Email := editEmail.Text;
    cliente.Valor := 5;

    telefone := TTelefone.Create;
    telefone.Numero := editTelefone.Text;

    cliente.Telefone := telefone;
    telefone.Cliente := cliente;

    Memo1.Lines.Clear;
    if not validator.validate(cliente) then
    begin
      exibeErros(validator);
    end else
    begin
      ShowMessage('Cliente salvo com sucesso!');
    end;
  finally
    cliente.Free;
  end;
end;

procedure TForm1.exibeErros(validator: IValidator);
var
  showErrorMessage: TProc<String>;
begin
  showErrorMessage := procedure(mensagem: String)
  begin
    Application.MessageBox(PChar(mensagem), 'Atenção', MB_ICONERROR);
  end;

  if validator.hasErrorMessages('TAssociado.Nome') then
    showErrorMessage('Campo Nome: ' + validator.getFirstErrorMessage('TAssociado.Nome'));
  if validator.hasErrorMessages('TAssociado.idade') then
    showErrorMessage('Campo Idade: ' + validator.getFirstErrorMessage('TAssociado.idade'));
  if validator.hasErrorMessages('TAssociado.Filhos') then
    showErrorMessage('Campo Filhos: ' + validator.getFirstErrorMessage('TAssociado.Filhos'));
  if validator.hasErrorMessages('TAssociado.isCasado') then
    showErrorMessage('Campo Casado: ' + validator.getFirstErrorMessage('TAssociado.isCasado'));
  if validator.hasErrorMessages('TAssociado.Email') then
    showErrorMessage('Campo Email: ' + validator.getFirstErrorMessage('TAssociado.Email'));
  if validator.hasErrorMessages('TTelefone.Numero') then
    showErrorMessage('Campo Telefone: ' + validator.getFirstErrorMessage('TTelefone.Numero'));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  editIdade.Text := '0';
  editFilhos.Text := '0';
end;

procedure TForm1.initControls;
var
  i: Integer;
begin
  for i := 0 to Self.ControlCount - 1 do
  begin
    if Self.Controls[i] is TEdit then
    begin
      TEdit(Self.Controls[i]).Color := clWindow;
    end;
  end;

  lbErrorMessage.Visible := false;
  lbIdadeError.Visible := false;
  lbFilhosError.Visible := false;
  lbCasadoError.Visible := false;
  lbEmailError.Visible := false;
  lblTelefoneError.Visible := False;
end;

end.

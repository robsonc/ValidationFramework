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
    procedure showErrorMessage(field: String; msg: TErrorMessage; errorLabel: TLabel; controle: TObject);
    procedure initControls;
    procedure exibeErros(errorMessages: System.Generics.Collections.TList<TErrorMessage>);
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
  errorMessages: TList<TErrorMessage>;
begin
  initControls();

  validator := TValidator.Create;

  cliente := TCliente.Create;
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
      errorMessages := validator.getErrorMessages();
      exibeErros(errorMessages);
      validator.clear();
    end else
    begin
      ShowMessage('Cliente salvo com sucesso!');
    end;
  finally
    cliente.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  editIdade.Text := '0';
  editFilhos.Text := '0';
end;

procedure TForm1.showErrorMessage(field: String; msg: TErrorMessage; errorLabel: TLabel; controle: TObject);
begin
  if msg.FieldName = field then
  begin
    if controle is TEdit then
    begin
      TEdit(controle).Color := RGB(250, 157, 162);
    end;

    if controle is TCheckBox then
    begin
      TCheckBox(controle).Font.Color := RGB(250, 157, 162);
    end;

    TControl(controle).Hint := msg.Messages[0];
    TControl(controle).ShowHint := True;
//
//    errorLabel.Caption := msg.Messages[0];
//    errorLabel.Visible := true;
  end;
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

procedure TForm1.exibeErros(errorMessages: System.Generics.Collections.TList<TErrorMessage>);
var
  msg: TErrorMessage;
  mensagem: string;
begin
  for msg in errorMessages do
  begin
    showErrorMessage('TCliente.Nome', msg, lbErrorMessage, editNome);
    showErrorMessage('TCliente.idade', msg, lbIdadeError, editIdade);
    showErrorMessage('TCliente.Filhos', msg, lbFilhosError, editFilhos);
    showErrorMessage('TCliente.isCasado', msg, lbCasadoError, ckCasado);
    showErrorMessage('TCliente.Email', msg, lbEmailError, editEmail);
    showErrorMessage('TCliente.Telefone', msg, lblTelefoneError, editTelefone);
    showErrorMessage('TTelefone.Numero', msg, lblTelefoneError, editTelefone);
    Memo1.Lines.Append(msg.FieldName);
    for mensagem in msg.Messages do
    begin
      Memo1.Lines.Append('-----> ' + mensagem);
    end;
  end;
end;

end.

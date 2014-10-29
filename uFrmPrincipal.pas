unit uFrmPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uModel,
  uValidationFramework, Vcl.Bind.GenData, System.Rtti, System.Bindings.Outputs,
  Vcl.Bind.Editors, Data.Bind.EngExt, Vcl.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.ObjectScope, RegularExpressions, System.TypInfo;

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
    procedure btnSalvarClick(Sender: TObject);
    procedure bsClienteCreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
  private
    procedure showErrorMessage(field: String; msg: TErrorMessage; errorLabel: TLabel);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.bsClienteCreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  //
end;

procedure TForm1.btnSalvarClick(Sender: TObject);
var
  cliente: TCliente;
  msg: TErrorMessage;
  validator: TValidator;
begin
  lbErrorMessage.Visible := false;
  lbIdadeError.Visible := false;
  lbFilhosError.Visible := false;
  lbCasadoError.Visible := false;
  lbEmailError.Visible := false;

  validator := TValidator.Create;
  try
    cliente := TCliente.Create;
    try
      cliente.Nome := editNome.Text;
      cliente.idade := StrToInt(editIdade.Text);
      cliente.Filhos := StrToInt(editFilhos.Text);
      cliente.isCasado := ckCasado.Checked;
      cliente.dataAtual := Date;
      cliente.Email := editEmail.Text;

      if not validator.validate(cliente) then
      begin
        for msg in validator.getErrorMessages do
        begin
          showErrorMessage('FNome', msg, lbErrorMessage);
          showErrorMessage('Fidade', msg, lbIdadeError);
          showErrorMessage('FFilhos', msg, lbFilhosError);
          showErrorMessage('FisCasado', msg, lbCasadoError);
          showErrorMessage('FEmail', msg, lbEmailError);
        end;
      end;
    finally
      cliente.Free;
    end;
  finally
    validator.Free;
  end;
end;

procedure TForm1.showErrorMessage(field: String; msg: TErrorMessage; errorLabel: TLabel);
begin
  if msg.FieldName = field then
  begin
    errorLabel.Caption := msg.Messages[0];
    errorLabel.Visible := true;
  end;
end;

end.

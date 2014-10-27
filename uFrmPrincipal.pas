unit uFrmPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uModel,
  uValidationFramework, Vcl.Bind.GenData, System.Rtti, System.Bindings.Outputs,
  Vcl.Bind.Editors, Data.Bind.EngExt, Vcl.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.ObjectScope;

type
  TForm1 = class(TForm)
    editNome: TEdit;
    lbNome: TLabel;
    btnSalvar: TButton;
    lbErrorMessage: TLabel;
    bsCliente: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkControlToField1: TLinkControlToField;
    bsErrors: TPrototypeBindSource;
    procedure btnSalvarClick(Sender: TObject);
    procedure bsClienteCreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
  private
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

  validator := TValidator.Create;

  cliente := TCliente.Create;
  cliente.Nome := editNome.Text;
  cliente.idade := 12;
  cliente.Filhos := 3;
  cliente.isCasado := true;

  if not validator.validate(cliente) then
  begin
    for msg in validator.getErrorMessages do
    begin
      if msg.FieldName = 'FNome' then
      begin
        lbErrorMessage.Caption := msg.Messages[0];
        lbErrorMessage.Visible := true;
      end;
    end;
  end;
end;

end.

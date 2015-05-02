program PrjValidation;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  Vcl.Forms,
  uFrmPrincipal in 'uFrmPrincipal.pas' {Form1},
  uModel in 'uModel.pas',
  uValidationFramework in 'uValidationFramework.pas',
  uValidation.Framework.Attributes in 'uValidation.Framework.Attributes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

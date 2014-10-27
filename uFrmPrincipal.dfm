object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 316
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbNome: TLabel
    Left = 20
    Top = 11
    Width = 32
    Height = 13
    Caption = 'Nome'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbErrorMessage: TLabel
    Left = 21
    Top = 50
    Width = 69
    Height = 13
    Caption = 'Error Message'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Visible = False
  end
  object editNome: TEdit
    Left = 20
    Top = 27
    Width = 345
    Height = 21
    TabOrder = 0
    Text = 'Bitmap28'
  end
  object btnSalvar: TButton
    Left = 290
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Salvar'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = btnSalvarClick
  end
  object bsCliente: TPrototypeBindSource
    AutoActivate = True
    AutoPost = False
    FieldDefs = <
      item
        Name = 'Nome'
        Generator = 'BitmapNames'
        ReadOnly = False
      end>
    ScopeMappings = <>
    OnCreateAdapter = bsClienteCreateAdapter
    Left = 296
    Top = 136
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 300
    Top = 197
    object LinkControlToField1: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = bsCliente
      FieldName = 'Nome'
      Control = editNome
      Track = True
    end
  end
  object bsErrors: TPrototypeBindSource
    AutoActivate = True
    AutoPost = False
    FieldDefs = <
      item
        Name = 'ErrorMessage'
        Generator = 'BitmapNames'
        ReadOnly = False
      end>
    ScopeMappings = <>
    Left = 216
    Top = 136
  end
end

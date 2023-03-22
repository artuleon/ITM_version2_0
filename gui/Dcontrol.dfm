object ControlDataForm: TControlDataForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Control Editor'
  ClientHeight = 340
  ClientWidth = 251
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 94
    Width = 69
    Height = 15
    Caption = 'Initial Setting'
  end
  object Label2: TLabel
    Left = 16
    Top = 174
    Width = 59
    Height = 15
    Caption = 'Time Series'
  end
  object Label3: TLabel
    Left = 16
    Top = 214
    Width = 72
    Height = 15
    Caption = 'Control Node'
  end
  object Label4: TLabel
    Left = 16
    Top = 254
    Width = 74
    Height = 15
    Caption = 'Control Curve'
  end
  object Label5: TLabel
    Left = 16
    Top = 131
    Width = 72
    Height = 15
    Caption = 'Time to Close'
  end
  object Label6: TLabel
    Left = 192
    Top = 131
    Width = 43
    Height = 15
    Caption = 'minutes'
  end
  object Label7: TLabel
    Left = 192
    Top = 94
    Width = 40
    Height = 15
    Caption = 'percent'
  end
  object ControlTypeGroup: TRadioGroup
    Left = 16
    Top = 16
    Width = 213
    Height = 57
    Caption = 'Type of Control'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Time'
      'Depth')
    TabOrder = 0
  end
  object InitSettingEdit: TSpinEdit
    Left = 128
    Top = 91
    Width = 49
    Height = 24
    MaxValue = 100
    MinValue = 0
    TabOrder = 1
    Value = 100
  end
  object CloseTimeEdit: TSpinEdit
    Left = 128
    Top = 128
    Width = 49
    Height = 24
    MaxValue = 120
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object TseriesCombo: TComboBox
    Left = 128
    Top = 171
    Width = 101
    Height = 23
    TabOrder = 3
  end
  object NodeCombo: TComboBox
    Left = 128
    Top = 211
    Width = 101
    Height = 23
    Style = csDropDownList
    TabOrder = 4
  end
  object CurveCombo: TComboBox
    Left = 128
    Top = 251
    Width = 101
    Height = 23
    TabOrder = 5
  end
  object Button1: TButton
    Left = 16
    Top = 292
    Width = 65
    Height = 25
    Caption = 'Ok'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 92
    Top = 292
    Width = 65
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object Button3: TButton
    Left = 169
    Top = 292
    Width = 65
    Height = 25
    Caption = 'Help'
    TabOrder = 8
  end
end

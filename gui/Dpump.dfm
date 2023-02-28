object PumpDataForm: TPumpDataForm
  Left = 0
  Top = 0
  Caption = 'Pump Data'
  ClientHeight = 246
  ClientWidth = 574
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 24
    Width = 66
    Height = 15
    Caption = 'Pump Curve'
  end
  object Label2: TLabel
    Left = 16
    Top = 89
    Width = 76
    Height = 15
    Caption = 'Friction Factor'
  end
  object Label3: TLabel
    Left = 16
    Top = 129
    Width = 69
    Height = 15
    Caption = 'Initial Setting'
  end
  object Label4: TLabel
    Left = 288
    Top = 24
    Width = 85
    Height = 15
    Caption = 'Control Method'
  end
  object Label5: TLabel
    Left = 288
    Top = 68
    Width = 102
    Height = 15
    Caption = 'Control Time Series'
  end
  object Label6: TLabel
    Left = 288
    Top = 112
    Width = 72
    Height = 15
    Caption = 'Control Node'
  end
  object Label7: TLabel
    Left = 288
    Top = 152
    Width = 74
    Height = 15
    Caption = 'Control Curve'
  end
  object ComboBox1: TComboBox
    Left = 112
    Top = 21
    Width = 153
    Height = 23
    TabOrder = 0
  end
  object NumEdit1: TNumEdit
    Left = 112
    Top = 86
    Width = 73
    Height = 23
    TabOrder = 1
    Text = '0.04'
    Style = esPosNumber
    Modified = False
    SelLength = 0
    SelStart = 0
  end
  object NumEdit2: TNumEdit
    Left = 112
    Top = 126
    Width = 73
    Height = 23
    TabOrder = 2
    Text = '100'
    Style = esPosNumber
    Modified = False
    SelLength = 0
    SelStart = 0
  end
  object ComboBox2: TComboBox
    Left = 408
    Top = 21
    Width = 145
    Height = 22
    Style = csOwnerDrawFixed
    TabOrder = 3
  end
  object ComboBox3: TComboBox
    Left = 408
    Top = 65
    Width = 145
    Height = 23
    TabOrder = 4
  end
  object ComboBox4: TComboBox
    Left = 408
    Top = 109
    Width = 145
    Height = 22
    Style = csOwnerDrawFixed
    TabOrder = 5
  end
  object ComboBox5: TComboBox
    Left = 408
    Top = 149
    Width = 145
    Height = 23
    TabOrder = 6
  end
  object OkBtn: TButton
    Left = 155
    Top = 200
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 7
  end
  object CancelBtn: TButton
    Left = 250
    Top = 200
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 8
  end
  object HelpBtn: TButton
    Left = 345
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 9
  end
  object Panel1: TPanel
    Left = 112
    Top = 49
    Width = 153
    Height = 23
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Leave blank for no pump'
    Color = clInfoBk
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 10
  end
end

object LegendForm: TLegendForm
  Left = 218
  Top = 116
  BorderStyle = bsDialog
  Caption = 'Legend Editor'
  ClientHeight = 326
  ClientWidth = 245
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefault
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Box0: TShape
    Left = 14
    Top = 16
    Width = 11
    Height = 27
  end
  object Box1: TShape
    Tag = 1
    Left = 14
    Top = 42
    Width = 11
    Height = 28
  end
  object Box2: TShape
    Tag = 2
    Left = 14
    Top = 69
    Width = 11
    Height = 28
  end
  object Box3: TShape
    Tag = 3
    Left = 14
    Top = 96
    Width = 11
    Height = 28
  end
  object Box4: TShape
    Tag = 4
    Left = 14
    Top = 123
    Width = 11
    Height = 28
  end
  object NameLabel: TLabel
    Left = 32
    Top = 8
    Width = 60
    Height = 15
    Caption = 'NameLabel'
  end
  object Bevel1: TBevel
    Left = 122
    Top = 172
    Width = 105
    Height = 4
    Shape = bsBottomLine
  end
  object Box5: TShape
    Tag = 4
    Left = 14
    Top = 150
    Width = 11
    Height = 28
  end
  object Box6: TShape
    Tag = 4
    Left = 14
    Top = 177
    Width = 11
    Height = 28
  end
  object Box7: TShape
    Tag = 4
    Left = 14
    Top = 203
    Width = 11
    Height = 28
  end
  object Box8: TShape
    Tag = 4
    Left = 14
    Top = 230
    Width = 11
    Height = 28
  end
  object Box9: TShape
    Tag = 4
    Left = 14
    Top = 257
    Width = 11
    Height = 28
  end
  object Box10: TShape
    Tag = 4
    Left = 14
    Top = 284
    Width = 11
    Height = 28
  end
  object Edit1: TEdit
    Left = 31
    Top = 33
    Width = 71
    Height = 23
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 31
    Top = 60
    Width = 71
    Height = 23
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 31
    Top = 87
    Width = 71
    Height = 23
    TabOrder = 2
  end
  object Edit4: TEdit
    Left = 31
    Top = 114
    Width = 71
    Height = 23
    TabOrder = 3
  end
  object BtnAutoScale: TButton
    Left = 122
    Top = 33
    Width = 105
    Height = 25
    Caption = '&Auto-Scale'
    TabOrder = 10
    OnClick = BtnAutoScaleClick
  end
  object BtnColorRamp: TButton
    Left = 122
    Top = 69
    Width = 105
    Height = 25
    Caption = '&Color Ramp ...'
    TabOrder = 11
    OnClick = BtnColorRampClick
  end
  object BtnReverse: TButton
    Left = 122
    Top = 105
    Width = 105
    Height = 25
    Caption = '&Reverse Colors'
    TabOrder = 12
    OnClick = BtnReverseClick
  end
  object BtnOK: TButton
    Left = 122
    Top = 194
    Width = 105
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 14
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 122
    Top = 234
    Width = 105
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 15
    OnClick = BtnCancelClick
  end
  object BtnHelp: TButton
    Left = 122
    Top = 274
    Width = 105
    Height = 25
    Caption = '&Help'
    TabOrder = 16
    OnClick = BtnHelpClick
  end
  object CheckFramed: TCheckBox
    Left = 137
    Top = 142
    Width = 81
    Height = 17
    Caption = 'Framed'
    TabOrder = 13
    OnClick = CheckFramedClick
  end
  object Edit5: TEdit
    Left = 31
    Top = 141
    Width = 71
    Height = 23
    TabOrder = 4
  end
  object Edit6: TEdit
    Left = 31
    Top = 168
    Width = 71
    Height = 23
    TabOrder = 5
  end
  object Edit7: TEdit
    Left = 31
    Top = 195
    Width = 71
    Height = 23
    TabOrder = 6
  end
  object Edit8: TEdit
    Left = 31
    Top = 222
    Width = 71
    Height = 23
    TabOrder = 7
  end
  object Edit9: TEdit
    Left = 31
    Top = 249
    Width = 71
    Height = 23
    TabOrder = 8
  end
  object Edit10: TEdit
    Left = 31
    Top = 276
    Width = 71
    Height = 23
    TabOrder = 9
  end
end

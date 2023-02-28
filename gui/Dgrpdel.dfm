object GroupDeleteForm: TGroupDeleteForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Group Deletion'
  ClientHeight = 188
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 281
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Caption = 'Delete which objects in the selected region? '
    WordWrap = True
  end
  object NodeCheckBox: TCheckBox
    Left = 16
    Top = 46
    Width = 257
    Height = 21
    Caption = 'All Nodes (and attached Links)'
    TabOrder = 0
    WordWrap = True
  end
  object LabelCheckBox: TCheckBox
    Left = 16
    Top = 110
    Width = 121
    Height = 21
    Caption = 'All Map Labels'
    TabOrder = 3
  end
  object OkBtn: TButton
    Left = 67
    Top = 148
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 155
    Top = 148
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object NodeTagEdit: TEdit
    Left = 160
    Top = 70
    Width = 121
    Height = 23
    TabOrder = 2
  end
  object NodeTagCheck: TCheckBox
    Left = 16
    Top = 70
    Width = 137
    Height = 21
    Caption = 'with Tag equal to '
    TabOrder = 1
  end
end

object ColorRampForm: TColorRampForm
  Left = 200
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Color Ramp Selector'
  ClientHeight = 113
  ClientWidth = 215
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Shape0: TShape
    Left = 17
    Top = 8
    Width = 17
    Height = 17
    Brush.Color = 2342134
  end
  object Shape1: TShape
    Left = 33
    Top = 8
    Width = 17
    Height = 17
    Brush.Color = 2994658
  end
  object Shape2: TShape
    Left = 49
    Top = 8
    Width = 17
    Height = 17
    Brush.Color = 3843019
  end
  object Shape3: TShape
    Left = 65
    Top = 8
    Width = 17
    Height = 17
    Brush.Color = 4626100
  end
  object Shape4: TShape
    Left = 81
    Top = 8
    Width = 17
    Height = 17
    Brush.Color = 5408924
  end
  object Shape5: TShape
    Left = 97
    Top = 8
    Width = 17
    Height = 17
    Brush.Color = 6257540
  end
  object Shape6: TShape
    Left = 113
    Top = 8
    Width = 17
    Height = 17
    Brush.Color = 7040365
  end
  object Shape7: TShape
    Left = 129
    Top = 8
    Width = 17
    Height = 17
    Brush.Color = 7823446
  end
  object Shape8: TShape
    Left = 145
    Top = 8
    Width = 17
    Height = 17
    Brush.Color = 8671806
  end
  object Shape9: TShape
    Left = 161
    Top = 8
    Width = 17
    Height = 17
    Brush.Color = 9454886
  end
  object Shape10: TShape
    Left = 177
    Top = 8
    Width = 17
    Height = 17
    Brush.Color = 10303246
  end
  object ComboBox1: TComboBox
    Left = 17
    Top = 40
    Width = 177
    Height = 21
    Style = csDropDownList
    DropDownCount = 9
    TabOrder = 0
    OnChange = ComboBox1Change
  end
  object Button1: TButton
    Left = 23
    Top = 80
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 113
    Top = 80
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end

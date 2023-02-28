object InflowsForm: TInflowsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'InflowsForm'
  ClientHeight = 223
  ClientWidth = 301
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
    Left = 24
    Top = 8
    Width = 254
    Height = 15
    Caption = 'Inflow = Baseline + (Time Series) x (Scale Factor)'
  end
  object Label2: TLabel
    Left = 24
    Top = 40
    Width = 43
    Height = 15
    Caption = 'Baseline'
  end
  object Label3: TLabel
    Left = 24
    Top = 80
    Width = 59
    Height = 15
    Caption = 'Time Series'
  end
  object Label4: TLabel
    Left = 24
    Top = 118
    Width = 63
    Height = 15
    Caption = 'Scale Factor'
  end
  object Label5: TLabel
    Left = 16
    Top = 153
    Width = 267
    Height = 15
    Caption = 'Leave Baseline and Time Series blank for no inflow.'
  end
  object NumEdit1: TNumEdit
    Left = 112
    Top = 37
    Width = 105
    Height = 23
    TabOrder = 0
    OnChange = NumEdit1Change
    Style = esNumber
    Modified = False
    SelLength = 0
    SelStart = 0
  end
  object ComboBox1: TComboBox
    Left = 112
    Top = 77
    Width = 105
    Height = 23
    TabOrder = 1
    OnChange = NumEdit1Change
  end
  object NumEdit2: TNumEdit
    Left = 112
    Top = 115
    Width = 105
    Height = 23
    TabOrder = 2
    Text = '1.0'
    OnChange = NumEdit1Change
    Style = esPosNumber
    Modified = False
    SelLength = 0
    SelStart = 0
  end
  object BitBtn1: TBitBtn
    Left = 223
    Top = 37
    Width = 24
    Height = 24
    Hint = 'Remove Value'
    ImageIndex = 13
    ImageName = 'Delete'
    Images = MainForm.ProjectImageList
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 223
    Top = 77
    Width = 24
    Height = 24
    Hint = 'Edit Series'
    ImageIndex = 2
    ImageName = 'edit'
    Images = MainForm.ProjectImageList
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 253
    Top = 77
    Width = 24
    Height = 24
    Hint = 'Remove Series'
    ImageIndex = 13
    ImageName = 'Delete'
    Images = MainForm.ProjectImageList
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = BitBtn3Click
  end
  object Button1: TButton
    Left = 21
    Top = 182
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object Button2: TButton
    Left = 113
    Top = 182
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object Button3: TButton
    Left = 205
    Top = 182
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 8
    OnClick = Button3Click
  end
end

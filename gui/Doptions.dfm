object AnalysisOptionsForm: TAnalysisOptionsForm
  Left = 465
  Top = 171
  BorderStyle = bsDialog
  Caption = 'Simulation Options'
  ClientHeight = 464
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 403
    Height = 401
    ActivePage = TabSheet2
    TabOrder = 0
    OnChange = PageControl1Change
    OnChanging = PageControl1Changing
    object TabSheet2: TTabSheet
      Caption = 'Dates/Time Steps'
      ImageIndex = 1
      object Label17: TLabel
        Left = 40
        Top = 46
        Width = 87
        Height = 15
        Caption = 'Start Analysis on'
      end
      object Label18: TLabel
        Left = 40
        Top = 86
        Width = 96
        Height = 15
        Caption = 'Start Reporting on'
      end
      object Label1: TLabel
        Left = 40
        Top = 126
        Width = 83
        Height = 15
        Caption = 'End Analysis on'
      end
      object Label3: TLabel
        Left = 160
        Top = 16
        Width = 71
        Height = 15
        Caption = 'Date (M/D/Y)'
      end
      object Label4: TLabel
        Left = 272
        Top = 16
        Width = 60
        Height = 15
        Caption = 'Time (H:M)'
      end
      object Label12: TLabel
        Left = 40
        Top = 217
        Width = 161
        Height = 15
        Caption = 'Reporting Time Step (seconds)'
      end
      object Label7: TLabel
        Left = 40
        Top = 270
        Width = 138
        Height = 15
        Caption = 'Maximum Computational'
      end
      object Bevel1: TBevel
        Left = 40
        Top = 156
        Width = 297
        Height = 17
        Shape = bsBottomLine
      end
      object Label2: TLabel
        Left = 40
        Top = 291
        Width = 106
        Height = 15
        Caption = 'Time Step (seconds)'
      end
      object StartDatePicker: TDateTimePicker
        Left = 160
        Top = 40
        Width = 97
        Height = 23
        Date = 37914.000000000000000000
        Time = 0.027740509249269960
        DateMode = dmUpDown
        TabOrder = 0
        OnChange = EditChange
      end
      object RptDatePicker: TDateTimePicker
        Left = 160
        Top = 80
        Width = 97
        Height = 23
        Date = 37914.000000000000000000
        Time = 0.027740509249269960
        DateMode = dmUpDown
        TabOrder = 2
        OnChange = EditChange
      end
      object EndDatePicker: TDateTimePicker
        Left = 160
        Top = 120
        Width = 97
        Height = 23
        Date = 37914.000000000000000000
        Time = 0.027740509249269960
        DateMode = dmUpDown
        TabOrder = 4
        OnChange = EditChange
      end
      object StartTimePicker: TDateTimePicker
        Left = 272
        Top = 40
        Width = 65
        Height = 23
        Date = 37914.000000000000000000
        Format = 'HH:mm'
        Time = 37914.000000000000000000
        Kind = dtkTime
        TabOrder = 1
        OnChange = EditChange
      end
      object RptTimePicker: TDateTimePicker
        Left = 272
        Top = 80
        Width = 65
        Height = 23
        Date = 37914.000000000000000000
        Format = 'HH:mm'
        Time = 37914.000000000000000000
        Kind = dtkTime
        TabOrder = 3
        OnChange = EditChange
      end
      object EndTimePicker: TDateTimePicker
        Left = 272
        Top = 120
        Width = 65
        Height = 23
        Date = 37914.000000000000000000
        Format = 'HH:mm'
        Time = 37914.000000000000000000
        Kind = dtkTime
        TabOrder = 5
        OnChange = EditChange
      end
      object MaxTimeStepEdit: TNumEdit
        Left = 226
        Top = 278
        Width = 67
        Height = 23
        TabOrder = 7
        Text = '30.0'
        OnChange = EditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object ReportStepEdit: TNumEdit
        Left = 226
        Top = 214
        Width = 67
        Height = 23
        TabOrder = 6
        Text = '30.0'
        OnChange = EditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'ITM Options'
      ImageIndex = 4
      object Label8: TLabel
        Left = 36
        Top = 18
        Width = 130
        Height = 15
        Caption = 'ITM Simulation Options'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold, fsUnderline]
        ParentFont = False
      end
      object Label10: TLabel
        Left = 36
        Top = 56
        Width = 144
        Height = 15
        Caption = 'Maximum Number of Cells'
      end
      object Label11: TLabel
        Left = 36
        Top = 94
        Width = 156
        Height = 15
        Caption = 'Maximum Plot Cells per  Pipe'
      end
      object Label14: TLabel
        Left = 36
        Top = 132
        Width = 138
        Height = 15
        Caption = 'Reference Depth Ratio (%)'
      end
      object Label15: TLabel
        Left = 36
        Top = 170
        Width = 151
        Height = 15
        Caption = 'Pressure Wave Celerity (m/s)'
      end
      object Label16: TLabel
        Left = 36
        Top = 208
        Width = 173
        Height = 15
        Caption = 'Global Initial Water Elevation (m)'
      end
      object ApplyDefaultsLabel: TLabel
        Left = 276
        Top = 18
        Width = 77
        Height = 15
        Cursor = crHandPoint
        Caption = 'Apply Defaults'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = ApplyDefaultsLabelClick
      end
      inline MaxCells: TUpDnEditBox
        Left = 276
        Top = 52
        Width = 67
        Height = 23
        AutoSize = True
        TabOrder = 0
        ExplicitLeft = 276
        ExplicitTop = 52
        ExplicitWidth = 67
        ExplicitHeight = 23
        inherited Spinner: TUpDown
          Left = 51
          Height = 23
          Min = 100
          Max = 100000
          Increment = 100
          Position = 100
          TabOrder = 1
          ExplicitLeft = 51
          ExplicitHeight = 23
        end
        inherited EditBox: TEdit
          Width = 51
          Height = 23
          TabOrder = 0
          Text = '100'
          OnChange = EditChange
          ExplicitWidth = 51
          ExplicitHeight = 23
        end
      end
      inline MaxPlotCells: TUpDnEditBox
        Left = 276
        Top = 91
        Width = 67
        Height = 23
        AutoSize = True
        TabOrder = 1
        ExplicitLeft = 276
        ExplicitTop = 91
        ExplicitWidth = 67
        ExplicitHeight = 23
        inherited Spinner: TUpDown
          Left = 51
          Height = 23
          Min = 1
          Position = 100
          TabOrder = 1
          ExplicitLeft = 51
          ExplicitHeight = 23
        end
        inherited EditBox: TEdit
          Width = 51
          Height = 23
          TabOrder = 0
          Text = '100'
          OnChange = EditChange
          ExplicitWidth = 51
          ExplicitHeight = 23
        end
      end
      object WaveCelerityEdit: TNumEdit
        Left = 276
        Top = 167
        Width = 67
        Height = 23
        TabOrder = 3
        Text = '200'
        OnChange = EditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      object InitElevEdit: TNumEdit
        Left = 276
        Top = 205
        Width = 67
        Height = 23
        TabOrder = 4
        OnChange = EditChange
        Style = esPosNumber
        Modified = False
        SelLength = 0
        SelStart = 0
      end
      inline RefDepth: TUpDnEditBox
        Left = 276
        Top = 128
        Width = 67
        Height = 23
        AutoSize = True
        TabOrder = 2
        ExplicitLeft = 276
        ExplicitTop = 128
        ExplicitWidth = 67
        ExplicitHeight = 23
        inherited Spinner: TUpDown
          Left = 51
          Height = 23
          Min = 80
          Max = 99
          Position = 95
          TabOrder = 1
          ExplicitLeft = 51
          ExplicitHeight = 23
        end
        inherited EditBox: TEdit
          Width = 51
          Height = 23
          TabOrder = 0
          Text = '95'
          OnChange = EditChange
          ExplicitWidth = 51
          ExplicitHeight = 23
        end
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Hot Start Files'
      ImageIndex = 2
      object Label9: TLabel
        Left = 32
        Top = 40
        Width = 285
        Height = 15
        Caption = 'Name of hot start file to use (leave blank if none used)'
      end
      object Label13: TLabel
        Left = 32
        Top = 120
        Width = 290
        Height = 15
        Caption = 'Name of hot start file to save (leave blank if none used)'
      end
      object UseHotstartEdit: TEdit
        Left = 32
        Top = 72
        Width = 280
        Height = 23
        TabOrder = 0
      end
      object SaveHotstartEdit: TEdit
        Left = 32
        Top = 152
        Width = 280
        Height = 23
        TabOrder = 2
      end
      object UseHotstartBtn: TBitBtn
        Tag = 1
        Left = 318
        Top = 73
        Width = 23
        Height = 22
        Hint = 'Edit'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = UseHotstartBtnClick
      end
      object SaveHotstartBtn: TBitBtn
        Tag = 1
        Left = 318
        Top = 153
        Width = 23
        Height = 22
        Hint = 'Edit'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = SaveHotstartBtnClick
      end
    end
  end
  object OKBtn: TButton
    Left = 83
    Top = 424
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 170
    Top = 424
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpBtn: TButton
    Left = 258
    Top = 424
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpBtnClick
  end
end

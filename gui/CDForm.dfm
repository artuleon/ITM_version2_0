object ChartOptionsForm: TChartOptionsForm
  Left = 410
  Top = 182
  BorderStyle = bsDialog
  Caption = 'Chart Options'
  ClientHeight = 376
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 353
    Height = 313
    ActivePage = SeriesPage
    TabOrder = 0
    OnChange = PageControl1Change
    object GeneralPage: TTabSheet
      Caption = 'General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 8
        Top = 24
        Width = 54
        Height = 13
        Caption = 'Panel Color'
      end
      object Label2: TLabel
        Left = 8
        Top = 72
        Width = 84
        Height = 13
        Caption = 'Background Color'
      end
      object Label3: TLabel
        Left = 8
        Top = 148
        Width = 49
        Height = 13
        Caption = 'View in 3D'
      end
      object Label4: TLabel
        Left = 8
        Top = 184
        Width = 85
        Height = 13
        Caption = '3D Effect Percent'
      end
      object Label5: TLabel
        Left = 8
        Top = 224
        Width = 45
        Height = 13
        Caption = 'Main Title'
      end
      object Label8: TLabel
        Left = 8
        Top = 112
        Width = 100
        Height = 13
        Caption = 'Background Gradient'
      end
      object PanelColorBox: TColorBox
        Left = 132
        Top = 16
        Width = 117
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
        TabOrder = 0
      end
      object BackColorBox: TColorBox
        Left = 132
        Top = 64
        Width = 117
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
        TabOrder = 1
      end
      object View3DBox: TCheckBox
        Left = 132
        Top = 143
        Width = 25
        Height = 25
        TabOrder = 3
      end
      object GraphTitleBox: TEdit
        Left = 8
        Top = 240
        Width = 263
        Height = 21
        TabOrder = 6
      end
      object GraphTitleFontBtn: TButton
        Left = 277
        Top = 238
        Width = 49
        Height = 25
        Caption = 'Font...'
        TabOrder = 7
        OnClick = GraphTitleFontBtnClick
      end
      object Pct3DUpDown: TUpDown
        Left = 181
        Top = 181
        Width = 16
        Height = 21
        Associate = Pct3DEdit
        Min = 1
        Increment = 10
        Position = 15
        TabOrder = 5
        TabStop = True
      end
      object Pct3DEdit: TEdit
        Left = 132
        Top = 181
        Width = 49
        Height = 21
        NumbersOnly = True
        TabOrder = 4
        Text = '15'
      end
      object BackGradientBox: TCheckBox
        Left = 132
        Top = 107
        Width = 25
        Height = 25
        TabOrder = 2
      end
    end
    object XaxisPage: TTabSheet
      Caption = 'Horizontal Axis'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label6: TLabel
        Left = 8
        Top = 24
        Width = 40
        Height = 13
        Caption = 'Minimum'
      end
      object Label7: TLabel
        Left = 8
        Top = 64
        Width = 44
        Height = 13
        Caption = 'Maximum'
      end
      object XIncrementLabel: TLabel
        Left = 8
        Top = 104
        Width = 49
        Height = 13
        Caption = 'Increment'
      end
      object Label11: TLabel
        Left = 8
        Top = 216
        Width = 43
        Height = 13
        Caption = 'Axis Title'
      end
      object XDataMinLabel: TLabel
        Left = 198
        Top = 20
        Width = 70
        Height = 13
        Caption = 'XDataMinLabel'
      end
      object XDataMaxLabel: TLabel
        Left = 198
        Top = 60
        Width = 74
        Height = 13
        Caption = 'XDataMaxLabel'
      end
      object XFormatLabel: TLabel
        Left = 8
        Top = 88
        Width = 34
        Height = 13
        Caption = 'Format'
      end
      object Xmin: TEdit
        Left = 96
        Top = 16
        Width = 87
        Height = 21
        TabOrder = 0
      end
      object Xmax: TEdit
        Left = 96
        Top = 56
        Width = 87
        Height = 21
        TabOrder = 1
      end
      object Xinc: TEdit
        Left = 96
        Top = 96
        Width = 87
        Height = 21
        TabOrder = 2
      end
      object Xauto: TCheckBox
        Left = 8
        Top = 136
        Width = 101
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Auto Scale'
        TabOrder = 4
      end
      object Xtitle: TEdit
        Left = 8
        Top = 232
        Width = 265
        Height = 21
        TabOrder = 6
      end
      object XFontBtn: TButton
        Left = 279
        Top = 230
        Width = 49
        Height = 25
        Caption = 'Font...'
        TabOrder = 7
        OnClick = XFontBtnClick
      end
      object Xgrid: TCheckBox
        Left = 8
        Top = 176
        Width = 101
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Grid Lines'
        TabOrder = 5
      end
      object DateFmtCombo: TComboBox
        Left = 200
        Top = 96
        Width = 105
        Height = 21
        Style = csDropDownList
        TabOrder = 3
      end
    end
    object YaxisPage: TTabSheet
      Caption = 'Vertical Axis'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label9: TLabel
        Left = 8
        Top = 24
        Width = 40
        Height = 13
        Caption = 'Minimum'
      end
      object Label12: TLabel
        Left = 8
        Top = 64
        Width = 44
        Height = 13
        Caption = 'Maximum'
      end
      object Label13: TLabel
        Left = 8
        Top = 104
        Width = 49
        Height = 13
        Caption = 'Increment'
      end
      object Label15: TLabel
        Left = 8
        Top = 216
        Width = 43
        Height = 13
        Caption = 'Axis Title'
      end
      object YDataMinLabel: TLabel
        Left = 198
        Top = 20
        Width = 70
        Height = 13
        Caption = 'YDataMinLabel'
      end
      object YDataMaxLabel: TLabel
        Left = 198
        Top = 60
        Width = 74
        Height = 13
        Caption = 'YDataMaxLabel'
      end
      object Ymin: TEdit
        Left = 96
        Top = 16
        Width = 87
        Height = 21
        TabOrder = 0
      end
      object Ymax: TEdit
        Left = 96
        Top = 56
        Width = 87
        Height = 21
        TabOrder = 1
      end
      object Yinc: TEdit
        Left = 96
        Top = 96
        Width = 87
        Height = 21
        TabOrder = 2
      end
      object Yauto: TCheckBox
        Left = 8
        Top = 136
        Width = 101
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Auto Scale'
        TabOrder = 3
      end
      object Ytitle: TEdit
        Left = 8
        Top = 232
        Width = 265
        Height = 21
        TabOrder = 5
      end
      object YFontBtn: TButton
        Left = 279
        Top = 230
        Width = 49
        Height = 25
        Caption = 'Font...'
        TabOrder = 6
        OnClick = YFontBtnClick
      end
      object Ygrid: TCheckBox
        Left = 8
        Top = 176
        Width = 101
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Grid Lines'
        TabOrder = 4
      end
    end
    object LegendPage: TTabSheet
      Caption = 'Legend'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label18: TLabel
        Left = 8
        Top = 40
        Width = 37
        Height = 13
        Caption = 'Position'
      end
      object Label19: TLabel
        Left = 8
        Top = 80
        Width = 25
        Height = 13
        Caption = 'Color'
      end
      object LegendFrameBox: TCheckBox
        Left = 8
        Top = 152
        Width = 121
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Framed'
        TabOrder = 2
      end
      object LegendVisibleBox: TCheckBox
        Left = 8
        Top = 216
        Width = 121
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Visible'
        TabOrder = 3
      end
      object LegendPosBox: TComboBox
        Left = 116
        Top = 32
        Width = 117
        Height = 21
        Style = csDropDownList
        TabOrder = 0
      end
      object LegendColorBox: TColorBox
        Left = 116
        Top = 72
        Width = 117
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
        TabOrder = 1
      end
      object LegendCheckBox: TCheckBox
        Left = 8
        Top = 120
        Width = 121
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Check Boxes'
        TabOrder = 4
      end
      object LegendShadowBox: TCheckBox
        Left = 8
        Top = 184
        Width = 121
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Shadowed'
        TabOrder = 5
      end
    end
    object SeriesPage: TTabSheet
      Caption = 'Series'
      ImageIndex = 4
      OnExit = SeriesPageExit
      object Label21: TLabel
        Left = 8
        Top = 24
        Width = 29
        Height = 13
        Caption = 'Series'
      end
      object Label22: TLabel
        Left = 8
        Top = 64
        Width = 58
        Height = 13
        Caption = 'Legend Title'
      end
      object SeriesListBox: TComboBox
        Left = 96
        Top = 16
        Width = 92
        Height = 21
        Style = csDropDownList
        TabOrder = 0
        OnClick = SeriesListBoxClick
      end
      object SeriesTitle: TEdit
        Left = 96
        Top = 56
        Width = 177
        Height = 21
        TabOrder = 1
      end
      object LegendFontBtn: TButton
        Left = 279
        Top = 54
        Width = 49
        Height = 25
        Caption = 'Font...'
        TabOrder = 2
        OnClick = LegendFontBtnClick
      end
      object PageControl2: TPageControl
        Left = 48
        Top = 96
        Width = 233
        Height = 185
        ActivePage = PieOptionsSheet
        TabOrder = 3
        object LineOptionsSheet: TTabSheet
          Caption = 'Lines'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label23: TLabel
            Left = 16
            Top = 16
            Width = 24
            Height = 13
            Caption = 'Style'
          end
          object Label24: TLabel
            Left = 16
            Top = 56
            Width = 25
            Height = 13
            Caption = 'Color'
          end
          object Label25: TLabel
            Left = 16
            Top = 96
            Width = 19
            Height = 13
            Caption = 'Size'
          end
          object LineStyleBox: TComboBox
            Left = 72
            Top = 11
            Width = 118
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            OnChange = LineStyleBoxChange
          end
          object LineColorBox: TColorBox
            Left = 72
            Top = 48
            Width = 118
            Height = 22
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
            TabOrder = 1
          end
          object LineVisibleBox: TCheckBox
            Left = 16
            Top = 128
            Width = 69
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Visible'
            TabOrder = 3
          end
          object LineSizeEdit: TEdit
            Left = 72
            Top = 92
            Width = 33
            Height = 21
            NumbersOnly = True
            TabOrder = 2
            Text = '1'
          end
          object LineSizeUpDown: TUpDown
            Left = 105
            Top = 92
            Width = 16
            Height = 21
            Associate = LineSizeEdit
            Min = 1
            Max = 10
            Position = 1
            TabOrder = 4
          end
        end
        object MarkOptionsSheet: TTabSheet
          Caption = 'Markers'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label26: TLabel
            Left = 16
            Top = 16
            Width = 24
            Height = 13
            Caption = 'Style'
          end
          object Label27: TLabel
            Left = 16
            Top = 56
            Width = 25
            Height = 13
            Caption = 'Color'
          end
          object Label28: TLabel
            Left = 16
            Top = 96
            Width = 19
            Height = 13
            Caption = 'Size'
          end
          object MarkVisibleBox: TCheckBox
            Left = 16
            Top = 128
            Width = 69
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Visible'
            TabOrder = 4
          end
          object MarkStyleBox: TComboBox
            Left = 72
            Top = 11
            Width = 118
            Height = 21
            Style = csDropDownList
            TabOrder = 0
          end
          object MarkColorBox: TColorBox
            Left = 72
            Top = 48
            Width = 118
            Height = 22
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
            TabOrder = 1
          end
          object MarkSizeEdit: TEdit
            Left = 72
            Top = 88
            Width = 33
            Height = 21
            NumbersOnly = True
            TabOrder = 2
            Text = '1'
          end
          object MarkSizeUpDown: TUpDown
            Left = 105
            Top = 88
            Width = 16
            Height = 21
            Associate = MarkSizeEdit
            Min = 1
            Max = 10
            Position = 1
            TabOrder = 3
          end
        end
        object AreaOptionsSheet: TTabSheet
          Caption = 'Patterns'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label29: TLabel
            Left = 16
            Top = 16
            Width = 24
            Height = 13
            Caption = 'Style'
          end
          object Label30: TLabel
            Left = 16
            Top = 56
            Width = 25
            Height = 13
            Caption = 'Color'
          end
          object Label31: TLabel
            Left = 16
            Top = 96
            Width = 40
            Height = 13
            Caption = 'Stacking'
          end
          object AreaFillStyleBox: TComboBox
            Left = 88
            Top = 11
            Width = 118
            Height = 21
            Style = csDropDownList
            TabOrder = 0
          end
          object AreaColorBox: TColorBox
            Left = 88
            Top = 48
            Width = 118
            Height = 22
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
            TabOrder = 1
          end
          object StackStyleBox: TComboBox
            Left = 88
            Top = 88
            Width = 118
            Height = 21
            Style = csDropDownList
            TabOrder = 2
          end
        end
        object PieOptionsSheet: TTabSheet
          Caption = 'Pie Options'
          ImageIndex = 3
          object Label32: TLabel
            Left = 24
            Top = 96
            Width = 71
            Height = 13
            Caption = 'Rotation Angle'
          end
          object PieCircledBox: TCheckBox
            Left = 24
            Top = 24
            Width = 105
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Circular'
            TabOrder = 0
          end
          object PiePatternBox: TCheckBox
            Left = 24
            Top = 56
            Width = 105
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Use Patterns'
            TabOrder = 1
          end
          object PieRotateEdit: TEdit
            Left = 115
            Top = 92
            Width = 33
            Height = 21
            NumbersOnly = True
            TabOrder = 2
            Text = '0'
          end
          object PieRotateUpDown: TUpDown
            Left = 148
            Top = 92
            Width = 16
            Height = 21
            Associate = PieRotateEdit
            Max = 360
            Increment = 10
            TabOrder = 3
          end
        end
        object LabelsOptionsSheet: TTabSheet
          Caption = 'Labels'
          ImageIndex = 4
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label33: TLabel
            Left = 16
            Top = 16
            Width = 24
            Height = 13
            Caption = 'Style'
          end
          object Label34: TLabel
            Left = 16
            Top = 56
            Width = 25
            Height = 13
            Caption = 'Color'
          end
          object LabelsStyleBox: TComboBox
            Left = 96
            Top = 11
            Width = 110
            Height = 21
            Style = csDropDownList
            TabOrder = 0
          end
          object LabelsBackColorBox: TColorBox
            Left = 96
            Top = 48
            Width = 110
            Height = 22
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
            TabOrder = 1
          end
          object LabelsTransparentBox: TCheckBox
            Left = 16
            Top = 80
            Width = 97
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Transparent'
            TabOrder = 2
          end
          object LabelsArrowsBox: TCheckBox
            Left = 16
            Top = 104
            Width = 97
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Show Arrows'
            TabOrder = 3
          end
          object LabelsVisibleBox: TCheckBox
            Left = 16
            Top = 128
            Width = 97
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Visible'
            TabOrder = 4
          end
        end
      end
    end
  end
  object DefaultBox: TCheckBox
    Left = 8
    Top = 336
    Width = 73
    Height = 17
    Caption = 'Default'
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 120
    Top = 334
    Width = 75
    Height = 25
    DoubleBuffered = True
    Kind = bkOK
    ParentDoubleBuffered = False
    TabOrder = 2
    OnClick = BitBtn3Click
  end
  object BitBtn2: TBitBtn
    Left = 203
    Top = 334
    Width = 75
    Height = 25
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 3
  end
  object BitBtn3: TBitBtn
    Left = 286
    Top = 334
    Width = 75
    Height = 25
    Caption = '&Help'
    DoubleBuffered = True
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333336633
      3333333333333FF3333333330000333333364463333333333333388F33333333
      00003333333E66433333333333338F38F3333333000033333333E66333333333
      33338FF8F3333333000033333333333333333333333338833333333300003333
      3333446333333333333333FF3333333300003333333666433333333333333888
      F333333300003333333E66433333333333338F38F333333300003333333E6664
      3333333333338F38F3333333000033333333E6664333333333338F338F333333
      0000333333333E6664333333333338F338F3333300003333344333E666433333
      333F338F338F3333000033336664333E664333333388F338F338F33300003333
      E66644466643333338F38FFF8338F333000033333E6666666663333338F33888
      3338F3330000333333EE666666333333338FF33333383333000033333333EEEE
      E333333333388FFFFF8333330000333333333333333333333333388888333333
      0000}
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 4
    OnClick = BitBtn3Click
  end
  object ColorBox3: TColorBox
    Left = 328
    Top = 376
    Width = 145
    Height = 22
    TabOrder = 5
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 72
    Top = 344
  end
end

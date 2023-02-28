object ProjectSummaryForm: TProjectSummaryForm
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Project Summary'
  ClientHeight = 372
  ClientWidth = 336
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Padding.Left = 6
  Padding.Top = 6
  Padding.Right = 6
  Padding.Bottom = 6
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object StringGrid1: TStringGrid
    Left = 6
    Top = 6
    Width = 324
    Height = 360
    Align = alClient
    ColCount = 2
    DefaultColWidth = 180
    RowCount = 15
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    ScrollBars = ssNone
    TabOrder = 0
  end
end

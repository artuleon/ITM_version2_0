object PropEditForm: TPropEditForm
  Left = 489
  Top = 145
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Property Editor'
  ClientHeight = 331
  ClientWidth = 267
  Color = clWindow
  Ctl3D = False
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 0
    Top = 287
    Width = 267
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    Color = clBtnShadow
    ParentColor = False
    ExplicitWidth = 200
  end
  object Panel1: TPanel
    Left = 0
    Top = 291
    Width = 267
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 1
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 0
    object HintLabel: TLabel
      Left = 1
      Top = 1
      Width = 265
      Height = 38
      Align = alClient
      AutoSize = False
      Color = clBtnFace
      ParentColor = False
      Transparent = True
      WordWrap = True
      ExplicitWidth = 198
    end
  end
end

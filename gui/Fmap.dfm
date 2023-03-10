object MapForm: TMapForm
  Left = 242
  Top = 132
  Caption = 'Study Area Map'
  ClientHeight = 328
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000007000000700000000700000070000000AAA0000DDD000077A
    AA7777DDD770000AAA0000DDD000000070000007000000007000000700000000
    7000000700000000700000070000000EEE0000BBB000077EEE7777BBB770000E
    EE0000BBB000000070000007000000007000000700000000000000000000FFFF
    0000F7EF0000F7EF0000E3C7000080010000E3C70000F7EF0000F7EF0000F7EF
    0000F7EF0000E3C7000080010000E3C70000F7EF0000F7EF0000FFFF0000}
  KeyPreview = True
  OldCreateOrder = True
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDblClick = FormDblClick
  OnDestroy = FormDestroy
  OnDragDrop = FormDragDrop
  OnDragOver = FormDragOver
  OnKeyDown = FormKeyDown
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnMouseWheel = FormMouseWheel
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 15
  object NodeLegendPanel: TPanel
    Left = 53
    Top = 50
    Width = 100
    Height = 180
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    Visible = False
    object NodeLegendBox: TPaintBox
      Left = 0
      Top = 0
      Width = 100
      Height = 180
      Cursor = crDrag
      Align = alClient
      OnDblClick = NodeLegendBoxDblClick
      OnMouseDown = DragLegend
      OnPaint = NodeLegendBoxPaint
    end
  end
  object LinkLegendPanel: TPanel
    Left = 172
    Top = 50
    Width = 100
    Height = 180
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 1
    Visible = False
    object LinkLegendBox: TPaintBox
      Left = 0
      Top = 0
      Width = 100
      Height = 180
      Cursor = crDrag
      Align = alClient
      OnDblClick = LinkLegendBoxDblClick
      OnMouseDown = DragLegend
      OnPaint = LinkLegendBoxPaint
    end
  end
  object TimeLegendBox: TScrollBox
    Left = 256
    Top = 8
    Width = 161
    Height = 25
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    BorderStyle = bsNone
    Color = clBlack
    Ctl3D = False
    ParentColor = False
    ParentCtl3D = False
    TabOrder = 2
    object TimeLegendPanel: TPanel
      Left = 0
      Top = 0
      Width = 161
      Height = 25
      BevelInner = bvRaised
      BevelOuter = bvLowered
      Caption = 'TimeLegendPanel'
      Color = clBlack
      Ctl3D = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clLime
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
      OnDblClick = TimeLegendPanelDblClick
      OnMouseDown = TimeLegendPanelMouseDown
    end
  end
  object HintPanel: TPanel
    Left = 40
    Top = 272
    Width = 49
    Height = 17
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clInfoBk
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 3
    object HintLabel: TLabel
      Left = 0
      Top = 0
      Width = 47
      Height = 15
      Align = alClient
      Caption = 'HintLabel'
      ExplicitWidth = 51
    end
  end
  object PopupMenu1: TPopupMenu
    AutoPopup = False
    OnPopup = PopupMenu1Popup
    Left = 258
    Top = 241
    object PopupCopy: TMenuItem
      Caption = 'Copy'
      OnClick = PopupCopyClick
    end
    object PopupPaste: TMenuItem
      Caption = 'Paste'
      OnClick = PopupPasteClick
    end
    object PopupDelete: TMenuItem
      Caption = 'Delete'
      OnClick = PopupDeleteClick
    end
    object PopupReverse: TMenuItem
      Caption = 'Reverse'
      OnClick = PopupReverseClick
    end
    object PopupConvert: TMenuItem
      Caption = 'Convert to ...'
      object ConvertToType1: TMenuItem
        Tag = 1
        OnClick = ConvertToTypeClick
      end
      object ConvertToType2: TMenuItem
        Tag = 2
        OnClick = ConvertToTypeClick
      end
      object ConvertToType3: TMenuItem
        Tag = 3
        OnClick = ConvertToTypeClick
      end
      object ConvertToType4: TMenuItem
        Tag = 4
        OnClick = ConvertToTypeClick
      end
      object ConvertToType5: TMenuItem
        Tag = 5
        OnClick = ConvertToTypeClick
      end
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object PopupVertices: TMenuItem
      Caption = 'Vertices'
      OnClick = PopupVerticesClick
    end
    object PopupProperties: TMenuItem
      Caption = 'Properties'
      OnClick = PopupPropertiesClick
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 108
    Top = 241
  end
  object PopupMenu2: TPopupMenu
    Left = 328
    Top = 242
    object PopupObjects: TMenuItem
      Caption = 'L&ayers'
      OnClick = PopupObjectsClick
      object PopupShowNodes: TMenuItem
        Caption = '&Nodes'
        OnClick = PopupShowObjectsClick
      end
      object PopupShowLinks: TMenuItem
        Caption = '&Links'
        OnClick = PopupShowObjectsClick
      end
      object PopupShowLabels: TMenuItem
        Caption = 'Lab&els'
        OnClick = PopupShowObjectsClick
      end
      object PopupShowBackdrop: TMenuItem
        Caption = '&Backdrop'
        OnClick = PopupShowBackdropClick
      end
    end
    object PopupLegends: TMenuItem
      Caption = '&Legends'
      OnClick = PopupLegendsClick
      object PopupNodeLegend: TMenuItem
        Caption = '&Node'
        OnClick = PopupNodeLegendClick
      end
      object PopupLinkLegend: TMenuItem
        Caption = '&Link'
        OnClick = PopupLinkLegendClick
      end
      object PopupTimeLegend: TMenuItem
        Caption = '&Time'
        OnClick = PopupTimeLegendClick
      end
    end
    object PopupOptions: TMenuItem
      Caption = 'O&ptions...'
      OnClick = PopupOptionsClick
    end
  end
  object PopupMenu3: TPopupMenu
    OnPopup = PopupMenu3Popup
    Left = 328
    Top = 283
    object PopupAddVertex: TMenuItem
      Caption = 'Add Vertex'
      OnClick = PopupAddVertexClick
    end
    object PopupDeleteVertex: TMenuItem
      Caption = 'Delete Vertex'
      OnClick = PopupDeleteVertexClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object PopupQuitEditing: TMenuItem
      Caption = 'Quit Editing'
      OnClick = PopupQuitEditingClick
    end
  end
  object Timer2: TTimer
    Interval = 500
    OnTimer = Timer2Timer
    Left = 152
    Top = 241
  end
  object Timer3: TTimer
    Interval = 50
    OnTimer = Timer3Timer
    Left = 200
    Top = 240
  end
end

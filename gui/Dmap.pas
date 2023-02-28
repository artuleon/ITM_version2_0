unit Dmap;

{-------------------------------------------------------------------}
{                    Unit:    Dmap.pas                              }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/22/22                              }
{                                                                   }
{   Dialog form unit for changing Study Area Map display options.   }
{                                                                   }
{   The form consists of a listbox on the left and a notebook       }
{   next to it. The notebook has a page for each category of        }
{   map display options, which are selected from the listbox.       }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ExtCtrls, ComCtrls, Math, UpDnEdit, Grids, CheckLst,
  Vcl.Themes, Uglobals, Umap;

type
  TMapOptionsForm = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    Panel1: TPanel;
    Notebook1: TNotebook;
    NodesBySize: TCheckBox;
    NodeBorder: TCheckBox;
    GroupBox2: TGroupBox;
    NodeShape: TShape;
    LinksBySize: TCheckBox;
    GroupBox3: TGroupBox;
    LinkShape: TShape;
    Label4: TLabel;
    Label7: TLabel;
    NotationTransparent: TCheckBox;
    Label3: TLabel;
    LinkSymbols: TCheckBox;
    Label6: TLabel;
    Label8: TLabel;
    LinkArrows: TRadioGroup;
    ListBox1: TListBox;
    LabelsTransparent: TCheckBox;
    Label1: TLabel;
    LinkBorder: TCheckBox;
    NodeSymbols: TCheckBox;
    NotationListBox: TCheckListBox;
    NodeSpin: TUpDnEditBox;
    LinkSpin: TUpDnEditBox;
    ZoomForLabels: TUpDnEditBox;
    NotationFontSize: TUpDnEditBox;
    ZoomForNotation: TUpDnEditBox;
    ZoomForSymbols: TUpDnEditBox;
    ArrowSpin: TUpDnEditBox;
    ZoomForArrows: TUpDnEditBox;
    Label9: TLabel;
    Label5: TLabel;
    ColorListBox1: TColorListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure NodeSpinChange(Sender: TObject);
    procedure LinkSpinChange(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure NodeBorderClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ColorListBox1GetColors(Sender: TCustomColorListBox;
      Items: TStrings);
  private
    { Private declarations }
    procedure ResizeNodeShape;
    procedure ResizeLinkShape;
    procedure UpdateBoolean(var Value: Boolean; const NewValue: Boolean);
    procedure UpdateInteger(var Value: Integer; const NewValue: Integer);
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure GetActivePage(var aPage: Integer);
    procedure SetOptions(const Options: TMapOptions);
    procedure SetActivePage(aPage: Integer);
    procedure GetOptions(var Options: TMapOptions);
  end;

var
  MapOptionsForm: TMapOptionsForm;

implementation

{$R *.DFM}

procedure TMapOptionsForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler for form.
//----------------------------------------------------------------------------
begin
  with ListBox1 do
  begin
    ItemHeight := (ClientHeight) div Items.Count;
    ItemIndex := 0;
  end;
  ListBox1Click(Sender);
end;

procedure TMapOptionsForm.ColorListBox1GetColors(Sender: TCustomColorListBox;
  Items: TStrings);
begin
  Items.Clear;
  Items.AddObject('White', TObject(Uglobals.MapBackColor[1]));
  Items.AddObject('Yellow', TObject(Uglobals.MapBackColor[2]));
  Items.AddObject('Blue', TObject(Uglobals.MapBackColor[3]));
  Items.AddObject('Panel', TObject(Uglobals.MapBackColor[4]));
  Items.AddObject('Black', TObject(Uglobals.MapBackColor[5]));
  Items.AddObject('Cyan', TObject(Uglobals.MapBackColor[6]));
  Items.AddObject('Green', TObject(Uglobals.MapBackColor[7]));
  Items.AddObject('Pink', TObject(Uglobals.MapBackColor[8]));
end;

procedure TMapOptionsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
// OnKeyDown handler for form (KeyPreview was set to True).
// Calls OnClick handler for Help button when F1 is pressed.
//-----------------------------------------------------------------------------
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

procedure TMapOptionsForm.SetOptions(const Options: TMapOptions);
//-----------------------------------------------------------------------------
// Loads current map display options into the form.
//-----------------------------------------------------------------------------
begin
  with Options do
  begin
    NodeSpin.Spinner.Position := NodeSize;
    NodesBySize.Checked := ShowNodesBySize;
    NodeBorder.Checked := ShowNodeBorder;

    LinkSpin.Spinner.Position := LinkSize;
    LinksBySize.Checked := ShowLinksBySize;
    LinkBorder.Checked := ShowLinkBorder;

    NodeSymbols.Checked := ShowNodeSymbols;
    LinkSymbols.Checked := ShowLinkSymbols;

    LinkArrows.ItemIndex := Ord(ArrowStyle);
    ArrowSpin.Spinner.Position := ArrowSize;

    LabelsTransparent.Checked := LabelsTranspar;

    with NotationListBox do
    begin
      Checked[0] := ShowNodeIDs;
      Checked[1] := ShowLinkIDs;
      Checked[2] := ShowNodeValues;
      Checked[3] := ShowLinkValues;
    end;

    NotationTransparent.Checked := NotationTranspar;
    NotationFontSize.Spinner.Position := NotationSize;

    ZoomForLabels.Spinner.Position := LabelZoom;
    ZoomForSymbols.Spinner.Position := SymbolZoom;
    ZoomForArrows.Spinner.Position := ArrowZoom;
    ZoomForNotation.Spinner.Position := NotationZoom;

    ColorListBox1.ItemIndex := ColorIndex - 1;
  end;
  ResizeNodeShape;
  ResizeLinkShape;
  NodeBorderClick(self);
  NodeSymbols.Enabled := not QueryFlag;
  HasChanged := False;
end;

procedure TMapOptionsForm.GetOptions(var Options: TMapOptions);
//-----------------------------------------------------------------------------
// Unloads contents of form into map display options.
//-----------------------------------------------------------------------------
var
  Astyle: TArrowStyle;
begin
  with Options do
  begin
    UpdateInteger(NodeSize, NodeSpin.Spinner.Position);
    UpdateBoolean(ShowNodesBySize, NodesBySize.Checked);
    UpdateBoolean(ShowNodeBorder,NodeBorder.Checked);

    UpdateInteger(LinkSize, LinkSpin.Spinner.Position);
    UpdateBoolean(ShowLinksBySize, LinksBySize.Checked);
    UpdateBoolean(ShowLinkBorder, LinkBorder.Checked);

    UpdateBoolean(ShowNodeSymbols, NodeSymbols.Checked);
    UpdateBoolean(ShowLinkSymbols, LinkSymbols.Checked);

    Astyle := TArrowStyle(LinkArrows.ItemIndex);
    if Astyle <> ArrowStyle then HasChanged := true;
    ArrowStyle := Astyle;
    UpdateInteger(ArrowSize, ArrowSpin.Spinner.Position);

    UpdateBoolean(LabelsTranspar, LabelsTransparent.Checked);

    with NotationListBox do
    begin
      ShowNodeIDs := Checked[0];
      ShowLinkIDs := Checked[1];
      ShowNodeValues := Checked[2];
      ShowLinkValues := Checked[3];
    end;
    UpdateBoolean(NotationTranspar, NotationTransparent.Checked);
    UpdateInteger(NotationSize, NotationFontSize.Spinner.Position);

    LabelZoom := ZoomForLabels.Spinner.Position;
    SymbolZoom := ZoomForSymbols.Spinner.Position;
    ArrowZoom := ZoomForArrows.Spinner.Position;
    NotationZoom := ZoomForNotation.Spinner.Position;

    ColorIndex := ColorListBox1.ItemIndex + 1;
  end;
end;

procedure TMapOptionsForm.UpdateBoolean(var Value: Boolean;
  const NewValue: Boolean);
//-----------------------------------------------------------------------------
// Utility function which changes the value of a boolean variable.
//-----------------------------------------------------------------------------
begin
  if Value <> NewValue then HasChanged := true;
  Value := NewValue;
end;

procedure TMapOptionsForm.UpdateInteger(var Value: Integer;
  const NewValue: Integer);
//-----------------------------------------------------------------------------
// Utility function which changes the value of an integer variable.
//-----------------------------------------------------------------------------
begin
  if Value <> NewValue then HasChanged := true;
  Value := NewValue;
end;

procedure TMapOptionsForm.GetActivePage(var aPage: Integer);
//-----------------------------------------------------------------------------
// Retrieves index of notebook page of options currently displayed.
//-----------------------------------------------------------------------------
begin
  aPage := ListBox1.ItemIndex;
end;

procedure TMapOptionsForm.SetActivePage(aPage: Integer);
//-----------------------------------------------------------------------------
// Displays a specific page of map options.
//-----------------------------------------------------------------------------
begin
  with ListBox1 do
  begin
    if aPage < Items.Count then
    begin
      ItemIndex := aPage;
      ListBox1Click(Self);
    end;
  end;
end;

procedure TMapOptionsForm.ListBox1Click(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the option category listbox.
//-----------------------------------------------------------------------------
begin
  NoteBook1.PageIndex := ListBox1.ItemIndex;
end;

procedure TMapOptionsForm.ListBox1DrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
//-----------------------------------------------------------------------------
// OnDrawItem handler for the item category listbox.
// Centers the text within the item's drawing rectangle.
//-----------------------------------------------------------------------------
var
  ht: Integer;
  dy: Integer;
  s:  String;
begin
  with Control as TListBox do
  begin
    s := Items[Index];
    ht := Canvas.TextHeight(s);
    Canvas.FillRect(Rect);
    dy := (Rect.Bottom - Rect.Top - ht) div 2;
    Canvas.TextOut(0,Rect.Top+dy,s);
  end;
end;

procedure TMapOptionsForm.NodeSpinChange(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for Node Size SpinEdit control.
//-----------------------------------------------------------------------------
begin
  ResizeNodeShape;
end;

procedure TMapOptionsForm.ResizeNodeShape;
//-----------------------------------------------------------------------------
// Resizes the NodeShape control.
//-----------------------------------------------------------------------------
var
  newsize : Integer;
  aRect   : TRect;
begin
  newsize := 3*NodeSpin.Spinner.Position+2;
  aRect := NodeShape.BoundsRect;
  aRect.Top := NodeSpin.Top + (NodeSpin.Height div 2)
               - (newsize div 2);
  aRect.Bottom := aRect.Top + newsize;// + 1;
  aRect.Right := aRect.Left + newsize;// + 1;
  NodeShape.BoundsRect := aRect;
end;

procedure TMapOptionsForm.LinkSpinChange(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for Link Size SpinEdit control.
//-----------------------------------------------------------------------------
begin
  ResizeLinkShape;
end;

procedure TMapOptionsForm.ResizeLinkShape;
//-----------------------------------------------------------------------------
// Resizes the LinkShape control.
//-----------------------------------------------------------------------------
begin
  LinkShape.Top := LinkSpin.Top + (LinkSpin.Height -
                                   LinkSpin.Spinner.Position) div 2;
  LinkShape.Pen.Width := LinkSpin.Spinner.Position;
  LinkShape.Height := LinkSpin.Spinner.Position;
end;

procedure TMapOptionsForm.NodeBorderClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for NodeBorder checkbox. Causes the node shape symbol
// to be drawn with/without a border.
//-----------------------------------------------------------------------------
begin
  if NodeBorder.Checked then
    NodeShape.Pen.Color := clBlack
  else
    NodeShape.Pen.Color := clRed;
end;

procedure TMapOptionsForm.BtnHelpClick(Sender: TObject);
begin
  case NoteBook1.PageIndex of
    0: Application.HelpCommand(HELP_CONTEXT, 211130);
    1: Application.HelpCommand(HELP_CONTEXT, 211140);
    2: Application.HelpCommand(HELP_CONTEXT, 211150);
    3: Application.HelpCommand(HELP_CONTEXT, 211160);
    4: Application.HelpCommand(HELP_CONTEXT, 211170);
    5: Application.HelpCommand(HELP_CONTEXT, 211180);
    6: Application.HelpCommand(HELP_CONTEXT, 211190);
  end;
end;

end.

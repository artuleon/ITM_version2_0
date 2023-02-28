unit Dquery;

{-------------------------------------------------------------------}
{                    Unit:    Dquery.pas                            }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/22/22                              }
{                                                                   }
{  Stay on top form unit that performs a Map Query (such as         }
{  locate all nodes with flooding > 0)                              }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Uglobals, Uutils, ExtCtrls, Buttons;

type
  TQueryForm = class(TForm)
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    Panel1: TPanel;
    Button1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
  private
    { Private declarations }
    OldLinkVar: Integer;
    OldNodeVar: Integer;
    OldShowNodes: Boolean;
    OldShowLinks: Boolean;
  public
    { Public declarations }
    procedure Clear;
    procedure UpdateVariables;
    procedure UpdateQueryCaption;
  end;

var
  QueryForm: TQueryForm;

implementation

{$R *.DFM}

uses
  Fmain, Fmap, Ubrowser, Uoutput, Uproject;

const
  TXT_NODES_WITH = 'Nodes';
  TXT_LINKS_WITH = 'Links';
  TXT_ITEMS_FOUND = ' items found';

procedure TQueryForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler for form.
//-----------------------------------------------------------------------------
var
  I: Integer;
  P: TPoint;
begin
  // Load items into Node/Link selection combo box
  ComboBox1.Items.Add(TXT_NODES_WITH);
  ComboBox1.Items.Add(TXT_LINKS_WITH);
  ComboBox1.ItemIndex := 0;

  // Load the choices of relations to query on (below, equals, above)
  for I := Low(FilterRelation) to High(FilterRelation) do
    ComboBox3.Items.Add(FilterRelation[I]);
  ComboBox3.ItemIndex := 0;
  OldNodeVar := -1;
  OldLinkVar := -1;

  // Position form at top left of Main form
  with MainForm do
  begin
    P.x := Left + (Width - ClientWidth) - 2;
    P.Y := Top + (Height-ClientHeight) - 2;
  end;
  Top := P.Y;
  Left := P.X;
end;

procedure TQueryForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
// OnShow handler for the form.
//-----------------------------------------------------------------------------
begin
  // Set form's font
  Panel1.Font.Color := clRed;

  // Save show settings for each class of object
  with MapForm.Map.Options do
  begin
    OldShowNodes := ShowNodes;
    OldShowLinks := ShowLinks;
  end;
  ComboBox1Change(self);
end;

procedure TQueryForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
// OnClose handler for form. Restores the map display to the state it had
// before last query was made.
//-----------------------------------------------------------------------------
begin
  Panel1.Caption := '';
  if (QueryFlag) then
  begin
    QueryFlag := False;
    CurrentNodeVar := OldNodeVar;
    CurrentLinkVar := OldLinkVar;
    if not RunFlag then
    begin
      if CurrentNodeVar >= NODEOUTVAR1 then CurrentNodeVar := NOVIEW;
      if CurrentLinkVar >= LINKOUTVAR1 then CurrentLinkVar := NOVIEW;
    end;
    OldNodeVar := -1;
    OldLinkVar := -1;
    Uoutput.SetNodeColors;
    Uoutput.SetLinkColors;
    MapForm.Map.Options.ShowNodes := OldShowNodes;
    MapForm.Map.Options.ShowLinks := OldShowLinks;
    MapForm.RedrawMap;
    MapForm.DrawNodeLegend;
    MapForm.DrawLinkLegend;
  end;

  // Enable selection of map view theme from main form's Browser panel
  MainForm.NodeViewBox.Enabled := True;
  MainForm.LinkViewBox.Enabled := True;
  Action := caHide;
end;

procedure TQueryForm.Clear;
//-----------------------------------------------------------------------------
// Clears the Query Value and Results fields of the form.
//-----------------------------------------------------------------------------
begin
  Edit1.Text := '';
  Panel1.Caption := '';
end;

procedure TQueryForm.UpdateVariables;
//-----------------------------------------------------------------------------
// Changes the names of the view variables listed in the Variable combo box.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  with ComboBox2 do
  begin
    I := ItemIndex;
    Clear;
    case ComboBox1.ItemIndex of
    0: Items.Assign(MainForm.NodeViewBox.Items);
    1: Items.Assign(MainForm.LinkViewBox.Items);
    end;
    Items.Delete(0);
    if I >= Items.Count then ItemIndex := 0 else ItemIndex := I;
  end;
end;

procedure TQueryForm.ComboBox1Change(Sender: TObject);
//-----------------------------------------------------------------------------
// OnChange handler for the Node/Link combo box. Changes list of map
// view variables to choose from when user switches between Node & Link.
//-----------------------------------------------------------------------------
begin
  UpdateVariables;
  ComboBox2.ItemIndex := 0;
  Edit1.Visible := ComboBox3.Visible;
  Panel1.Caption := '';
end;

procedure TQueryForm.ComboBox2Change(Sender: TObject);
begin
  Panel1.Caption := '';
end;

procedure TQueryForm.Button1Click(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick handler for the Go button that executes the query.
//-----------------------------------------------------------------------------
begin
  // Check for valid numerical entry for query comparison value
  if not Edit1.Visible
  or Uutils.IsValidNumber(Edit1.Text,QueryValue) then
  begin

    // Set a flag & save query relation type
    QueryFlag := True;
    QueryRelation := TRelationType(ComboBox3.ItemIndex);

    // Disable selection of map view theme from Main form Browser panel
    MainForm.NodeViewBox.Enabled := False;
    MainForm.LinkViewBox.Enabled := False;

    // If this is first query since form was last shown then save
    // current status of map view
    if (OldNodeVar < 0) then OldNodeVar := CurrentNodeVar;
    if (OldLinkVar < 0) then OldLinkVar := CurrentLinkVar;

  // Select the query variable for viewing on the map
    CurrentLinkVar := NOVIEW;
    CurrentNodeVar := NOVIEW;
    if (ComboBox1.ItemIndex = 0) then
    begin
      CurrentNodeVar := ComboBox2.ItemIndex+1;
      Uoutput.SetNodeColors;
    end
    else
    begin
      CurrentLinkVar := ComboBox2.ItemIndex+1;
      Uoutput.SetLinkColors;
    end;

  // Display number of items matching the query
    UpdateQueryCaption;

  // Redraw the map
    MapForm.RedrawMap;
    MapForm.DrawNodeLegend;
    MapForm.DrawLinkLegend;
  end
  else Edit1.SetFocus;
end;

procedure TQueryForm.Edit1KeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress handler for the Edit box where user enters a value to compare
// against. Causes the query to be submitted if the user hits the Enter key.
//-----------------------------------------------------------------------------
begin
  if Key = #13 then
  begin
    Button1Click(Sender);
    Key := #0;
  end;
end;

procedure TQueryForm.FormKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------------------------------------
// OnKeyPress handler for the form. Closes form when user hits Esc.
//-----------------------------------------------------------------------------
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;
end;

procedure TQueryForm.UpdateQueryCaption;
//-----------------------------------------------------------------------------
// Updates the display of number of items matching the query.
//-----------------------------------------------------------------------------
var
  I,J,N: Integer;
begin
  Panel1.Caption := '';
  N := 0;
  if True then

  if (CurrentNodeVar <> NOVIEW) then
  begin
      for I := 0 to MAXCLASS do
      begin
        if Project.IsNode(I) then
          for J := 0 to Project.Lists[I].Count-1 do
            if Project.GetNode(I, J).ColorIndex > 0 then Inc(N);
      end;
  end
  else if CurrentLinkVar <> NOVIEW then
  begin
      for I := 0 to MAXCLASS do
      begin
        if Project.IsLink(I) then
          for J := 0 to Project.Lists[I].Count-1 do
            if Project.GetLink(I, J).ColorIndex > 0 then Inc(N);
      end;
  end;
  Panel1.Caption := IntToStr(N) + TXT_ITEMS_FOUND;
end;

end.


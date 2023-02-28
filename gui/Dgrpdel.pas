unit Dgrpdel;

{-------------------------------------------------------------------}
{                    Unit:    Dgrpdel.pas                           }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:   10/21/22                               }
{                                                                   }
{   Dialog form for deleting a group of objects bounded by a user-  }
{   drawn fenceline.                                                }
{                                                                   }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Uproject, Uglobals, ExtCtrls;

type
  TGroupDeleteForm = class(TForm)
    NodeCheckBox: TCheckBox;
    LabelCheckBox: TCheckBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    NodeTagEdit: TEdit;
    Label1: TLabel;
    NodeTagCheck: TCheckBox;
    procedure OkBtnClick(Sender: TObject);
  private
    { Private declarations }
    theRegion: HRgn;
    NumDeletions: Integer;
    procedure GroupDelete;
    procedure DeleteNodes;
    procedure DeleteLabels;
  public
    { Public declarations }
  end;

//var
//  GroupDeleteForm: TGroupDeleteForm;

implementation

{$R *.dfm}

uses Fmain, Fmap, Ubrowser;

procedure TGroupDeleteForm.OkBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  GroupDelete;
end;

procedure TGroupDeleteForm.GroupDelete;
//-----------------------------------------------------------------------------
//  Deletes all objects of a selected type that lie within the user-drawn
//  fenceline.
//-----------------------------------------------------------------------------
begin
  //Create a GDI region from user's fenceline region
  theRegion := CreatePolygonRgn(MapForm.Fenceline, MapForm.NumFencePts - 1,
                                WINDING);
  try
    // Delete selected classes of objects
    if NodeCheckBox.Checked then DeleteNodes;
    if LabelCheckBox.Checked then DeleteLabels;

    // Update browser item list box and map
    if NumDeletions > 0 then with MainForm do
    begin
      SetChangeFlags;
      ItemListBox.Count := Project.Lists[CurrentList].Count;
      ItemListBox.Refresh;
      Ubrowser.BrowserUpdate(CurrentList, Project.CurrentItem[CurrentList]);
      MapForm.NumFencePts := 0;
      MapForm.RedrawMap;
    end;
  finally
    DeleteObject(theRegion);
  end;
end;

procedure TGroupDeleteForm.DeleteNodes;
//-----------------------------------------------------------------------------
//  Deletes all nodes and their connecting links that lie within the
//  deletion polygon area.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
  P: TPoint;
  N: TNode;
  Tag: String;
  Checked: Boolean;
begin
  Checked := NodeTagCheck.Checked;
  Tag := NodeTagEdit.Text;
  for I := 0 to MAXCLASS do
  begin
    if Project.IsNode(I) then
    begin
      // Must delete nodes from highest to lowest list index
      for J := Project.Lists[I].Count-1 downto 0 do
      begin
        N := Project.GetNode(I, J);
        P := MapForm.Map.GetNodePoint(N);
        if (PtInRegion(theRegion, P.X, P.Y)) then
        begin
          if Checked and not SameText(Tag, N.Data[TAG_INDEX]) then continue;
          Inc(NumDeletions);

          // Delete any references to node as a label's anchor point
          Project.DeleteLabelAnchors(N);

          // Delete all links adjacent to deleted node
          MapForm.Map.GetAdjacencyRect(I, J, True);
          Project.DeleteItem(I, J);
        end;
      end;
    end;
  end;
end;

procedure TGroupDeleteForm.DeleteLabels;
//-----------------------------------------------------------------------------
//  Deletes all map labels that lie within the deletion polygon area.
//-----------------------------------------------------------------------------
var
  J: Integer;
  Xp: Integer;
  Yp: Integer;
begin
  for J := Project.Lists[MAPLABEL].Count-1 downto 0 do
  begin
    Xp := MapForm.Map.GetXpix(Project.GetMapLabel(J).X);
    Yp := MapForm.Map.GetYpix(Project.GetMapLabel(J).Y);
    if (PtInRegion(theRegion, Xp, Yp)) then
    begin
      Inc(NumDeletions);
      Project.DeleteItem(MAPLABEL, J);
    end;
  end;
end;

end.

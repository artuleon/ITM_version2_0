unit Uclipbrd;

{-------------------------------------------------------------------}
{                    Unit:    Uclipbrd.pas                          }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/20/22                              }
{                                                                   }
{   Delphi Pascal unit containing procedures that copy and paste    }
{   data to and from the program's internal clipboard.              }
{-------------------------------------------------------------------}

interface

uses SysUtils, Windows, Forms, Messages, Classes, Graphics,
     Controls, Dialogs, System.UITypes, Uglobals, Uproject, Uutils, Uvertex;

procedure CopyLabel(const Index: Integer);
procedure CopyLink(const Ltype: Integer; const Index: Integer);
procedure CopyNode(const Ntype: Integer; const Index: Integer);
procedure PasteLabel(const Index: Integer);
procedure PasteLink(const Ltype: Integer; const Index: Integer);
procedure PasteNode(const Ntype: Integer; const Index: Integer);

implementation

uses Fmain, Fmap, Uedit, Uupdate;


function GetDataIndexes(const ObjType: Integer;
  var First, Last: Integer):Boolean;
//-----------------------------------------------------------------------------
//  Gets the first and last indexes, depending on object type, of data
//  items that are copied/pasted from the program's internal clipboard.
//-----------------------------------------------------------------------------
begin
  Result := False;
  case ObjType of
    JUNCTION:
    begin
      First := NODE_INVERT_INDEX;
      Last := JUNCTION_AREA_INDEX;
    end;
    BOUNDARY:
    begin
      First := NODE_INVERT_INDEX;
      Last := BOUNDARY_VENTILATED_INDEX;
    end;
    GATE:
    begin
      First := NODE_INVERT_INDEX;
      Last := GATE_CONTROL_CURVE_INDEX;
    end;
    WEIR:
    begin
      First := NODE_INVERT_INDEX;
      Last := WEIR_RATING_CURVE_INDEX;
    end;
    STORAGE:
    begin
      First := NODE_INVERT_INDEX;
      Last := STORAGE_OUTFLOW_INDEX;
    end;
    CONDUIT:
    begin
      First := CONDUIT_DIAMETER_INDEX;
      Last := CONDUIT_INIT_FLOW_INDEX;
    end;
{    PUMP:
    begin
      First := PUMP_CURVE_INDEX;
      Last := PUMP_CONTROL_CURVE_INDEX;
    end;}
    else Exit;
  end;
  Result := True;
end;


procedure CopyNode(const Ntype: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
// Copies a node's data to the clipboard.
//-----------------------------------------------------------------------------
var
  I      : Integer;
  First  : Integer;
  Last   : Integer;
  aNode  : TNode;
begin
  with Project.Clipboard do
  begin
    Clear;
    if not GetDataIndexes(Ntype, First, Last) then Exit;
    aNode := Project.GetNode(Ntype, Index);
    for I := First to Last do Data.Add(aNode.Data[I]);
    ExInflow := aNode.ExInflow;
    ObjType := Ntype;
  end;
end;


procedure CopyLink(const Ltype: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
// Copies a link's data to the clipboard.
//-----------------------------------------------------------------------------
var
  I      : Integer;
  First  : Integer;
  Last   : Integer;
  aLink  : TLink;
begin
  with Project.Clipboard do
  begin
    Clear;
    if not GetDataIndexes(Ltype, First, Last) then Exit;
    aLink := Project.GetLink(Ltype, Index);
    for I := First to Last do Data.Add(aLink.Data[I]);
    ObjType := Ltype;
  end;
end;


procedure CopyLabel(const Index: Integer);
//-----------------------------------------------------------------------------
// Copies a map label's data to the clipboard.
//-----------------------------------------------------------------------------
var
  aLabel: TMapLabel;
begin
  with Project.Clipboard do
  begin
    Clear;
    ObjType := MAPLABEL;
    aLabel := Project.GetMapLabel(Index);
    with Font do
    begin
      Name := aLabel.FontName;
      Size := aLabel.FontSize;
      Style := [];
      if aLabel.FontBold then Style := Style + [fsBold];
      if aLabel.FontItalic then Style := Style + [fsItalic];
    end;
  end;
end;


procedure PasteLabel(const Index: Integer);
//-----------------------------------------------------------------------------
// Pastes data from the clipboard to a map label.
//-----------------------------------------------------------------------------
var
  aLabel: TMapLabel;
begin
  with Project.Clipboard do
  begin
    aLabel := Project.GetMapLabel(Index);
    with Font do
    begin
      aLabel.FontName := Name;
      aLabel.FontSize := Size;
      if (fsBold in Style) then aLabel.FontBold := True
      else aLabel.FontBold := False;
      if (fsItalic in Style) then aLabel.FontItalic := True
      else aLabel.FontItalic := False;
    end;
  end;
  MapForm.EraseLabel(Index);
  MapForm.DrawObject(MAPLABEL, Index);
  Uedit.UpdateEditor(MAPLABEL, Index);
  HasChanged := True;
end;


procedure PasteLink(const Ltype: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
// Pastes data from the clipboard to a link.
//-----------------------------------------------------------------------------
var
  I:     Integer;
  First: Integer;
  Last:  Integer;
  aLink: TLink;
begin
  if not GetDataIndexes(Ltype, First, Last) then Exit;
  aLink := Project.GetLink(Ltype, Index);
  with Project.Clipboard do
  begin
    for I := First to Last do aLink.Data[I] := Data[I-First];
  end;
  Uedit.UpdateEditor(Ltype, Index);
  if Uglobals.CurrentLinkVar < LINKOUTVAR1
  then Uupdate.UpdateLinkColor(Ltype, Index,
    LinkVariable[CurrentLinkVar].SourceIndex);
  MainForm.SetChangeFlags;
end;


procedure PasteNode(const Ntype: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
// Pastes data from the clipboard to a node.
//-----------------------------------------------------------------------------
var
  I:     Integer;
  First: Integer;
  Last:  Integer;
  aNode: TNode;
begin
  if not GetDataIndexes(Ntype, First, Last) then Exit;
  aNode := Project.GetNode(Ntype, Index);
  with Project.Clipboard do
  begin
    for I := First to Last do aNode.Data[I] := Data[I-First];
    aNode.ExInflow := ExInflow;
  end;
  Uedit.UpdateEditor(Ntype, Index);
  if Uglobals.CurrentNodeVar < NODEOUTVAR1
  then Uupdate.UpdateNodeColor(Ntype, Index,
    NodeVariable[CurrentNodeVar].SourceIndex);
  MainForm.SetChangeFlags;
end;

end.

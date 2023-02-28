unit Uvalidate;

{-------------------------------------------------------------------}
{                    Unit:    Uvalidate.pas                         }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/20/22                              }
{                                                                   }
{   Delphi Pascal unit that validates new values for edited         }
{   properties of ITM objects.                                      }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Forms, Messages, Classes, Uglobals,   Dialogs,
  Uproject, Uutils, Uvertex;

function  ValidateEditor(I: Integer; var S: String; var E: String): Boolean;

implementation

uses
  Fmain, Fmap, Fproped, Uupdate;

const
  MSG_DUPLICATE_ID = 'ID name is blank or already in use.';
  MSG_NO_DATA      = 'This data field cannot be blank.';
  MSG_ZERO_VALUE   = 'This property cannot be 0.';

var
  Errmsg: String;


function  ValidateData(const I: Integer; const S: String): Boolean;
//-----------------------------------------------------------------------------
//  General validation function for an object property I with string
//  value S. The global variable EditorObject determines the type of
//  object being edited.
//-----------------------------------------------------------------------------

  function IsNotBlank(const S: String): Boolean;
  begin
    if Length(Trim(S)) > 0 then Result := True else Result := False;
  end;

  function IsZero(const S: String): Boolean;
  var
    X: Single;
  begin
    if Uutils.GetSingle(S, X) and (X = 0.0)
    then Result := True
    else Result := False;
  end;

begin
  Result := True;
  case EditorObject of

  JUNCTION:
    if I in [NODE_INVERT_INDEX,
             JUNCTION_MAX_DEPTH_INDEX, JUNCTION_INIT_DEPTH_INDEX,
             JUNCTION_AREA_INDEX] then
      Result := IsNotBlank(S);

  BOUNDARY:
    if I in [NODE_INVERT_INDEX, BOUNDARY_VALUE_INDEX] then
      Result := IsNotBlank(S);

  GATE:
    if I in [NODE_INVERT_INDEX, GATE_OPEN_RATE_INDEX] then
      Result := IsNotBlank(S);

  WEIR:
    if I = NODE_INVERT_INDEX then Result := IsNotBlank(S);

  STORAGE:
    if I in [NODE_INVERT_INDEX, STORAGE_MAX_DEPTH_INDEX,
             STORAGE_INIT_DEPTH_INDEX, STORAGE_OUTFLOW_INDEX] then
      Result := IsNotBlank(S);

  CONDUIT:
    if (I in [CONDUIT_DIAMETER_INDEX..CONDUIT_ROUGHNESS_INDEX]) and IsZero(S)
    then begin
      Result := False;
      ErrMsg := MSG_ZERO_VALUE;
      Exit;
    end
    else if I in [CONDUIT_DIAMETER_INDEX..CONDUIT_EXIT_LOSS_INDEX,
                  CONDUIT_DEPTH_VALUE_INDEX, CONDUIT_INIT_FLOW_INDEX]
    then Result := IsNotBlank(S);
  end;

  if not Result then ErrMsg := MSG_NO_DATA;
end;


procedure ReplaceID(const NewName: String; const ObjType: Integer;
  const Index: Integer);
//-----------------------------------------------------------------------------
//  Replaces the ID name of the object of type ObjType and index Index
//  with NewName.
//-----------------------------------------------------------------------------
var
  OldName: String;
begin
  with Project.Lists[ObjType] do
  begin

    // Replace the ID name in the project database
    OldName := Strings[Index];
    Strings[Index] := NewName;

    // Replace the ID name that appears in the MainForm's ItemListBox
    MainForm.ItemListBox.Refresh;

    if Project.IsNode(ObjType) then
    begin
      Project.GetNode(ObjType, Index).ID := PChar(Strings[Index]);
      Uupdate.UpdateNodeName(Oldname, NewName);
    end
    else if Project.IsLink(ObjType) then
    begin
      Project.GetLink(ObjType, Index).ID := PChar(Strings[Index]);
    end
    else if ObjType = MAPLABEL then
      Project.GetMapLabel(Index).Text := PChar(Strings[Index]);

    // Replace the ID name shown on the Property Editor's title bar
    if (ObjType = EditorObject) and (ObjType <> MAPLABEL) then
      PropEditForm.Caption := ObjectLabels[EditorObject] + ' ' + NewName;
  end;

  // Redraw the ID name on the map if these are currently displayed
  if Project.IsNode(ObjType) then
  begin
    if MapForm.Map.Options.ShowNodeIDs
    then MapForm.DrawObject(ObjType, Index);
  end
  else if Project.IsLink(ObjType) then
  begin
    if MapForm.Map.Options.ShowLinkIDs
    then MapForm.DrawObject(ObjType, Index);
  end;
end;


function ValidateNode(I: Integer; var S: String): Boolean;
//-----------------------------------------------------------------------------
// Validates data for a system node.
//-----------------------------------------------------------------------------
var
  Last: Integer;
  N   : TNode;
  V   : Extended;
  X, Y: Extended;
begin
  Result := True;
  N := Project.GetNode(EditorObject, EditorIndex);
  case EditorObject of
    JUNCTION:   Last := High(JunctionProps);
    BOUNDARY:   Last := High(BoundaryProps);
    GATE:       Last := High(GateProps);
    WEIR:       Last := High(WeirProps);
    STORAGE:    Last := High(StorageProps);
    else        Last := -1;
  end;

  // For ID name, make sure its not a duplicate
  if (I = ID_INDEX) then
  begin
    Result := not Project.DupID(S, EditorObject, EditorIndex);
    if   Result = True
    then ReplaceID(S, EditorObject, EditorIndex)
    else ErrMsg := MSG_DUPLICATE_ID;
  end

  // For X or Y coordinate, move node to new position
  else if I in [X_INDEX..Y_INDEX] then
  begin
    if Uutils.GetExtended(S, V) then
    begin
      X := N.X;
      Y := N.Y;
      case I of
        X_INDEX: X := V;
        Y_INDEX: Y := V;
      end;
      MapForm.HiliteOff;
      MapForm.MoveNode(EditorObject, EditorIndex, X, Y);
      MapForm.HiliteOn;
    end;
  end

  // For all other properties
  else if (I >= COMMENT_INDEX) and (I <= Last) then
  begin
    if ValidateData(I, S) then
    begin
      N.Data[I] := S;
      Uupdate.UpdateNodeColor(EditorObject, EditorIndex, I);
      if (EditorObject = GATE) and (I = GATE_CONTROL_METHOD_INDEX) then
        Uupdate.EditGate;
    end
    else Result := False;
  end;
end;


function ValidateLink(I: Integer; var S: String): Boolean;
//-----------------------------------------------------------------------------
// Validates data for a drainage system link.
//-----------------------------------------------------------------------------
var
  Last: Integer;
  L   : TLink;

  OldHasPump : Boolean;
begin
  L := Project.GetLink(EditorObject, EditorIndex);
  Last := High(ConduitProps);
  Result := True;

  // For ID name, make sure its not a duplicate
  if (I = ID_INDEX) then
  begin
    Result := not Project.DupID(S, EditorObject, EditorIndex);
    if   Result = True
    then ReplaceID(S, EditorObject, EditorIndex)
    else ErrMsg := MSG_DUPLICATE_ID;
  end

  // For start/end node:
  else if (I = UP_INDEX) or (I = DN_INDEX) then
  begin
    Result := Uupdate.UpdateLinkNode(EditorObject, EditorIndex, I, S, Errmsg);
    if Result then L.Data[I] := S;
  end

  // For all other properties
  else if (I >= COMMENT_INDEX) and (I <= Last) then
  begin
    if ValidateData(I, S) then
    begin
      L.Data[I] := S;
      Uupdate.UpdateLinkColor(EditorObject, EditorIndex, I);

      if I = CONDUIT_HAS_PUMP_INDEX then
      begin
        OldHasPump := L.HasPump;
        L.hasPump := SameText(S, 'YES');
        if L.HasPump <> OldHasPump then
          MapForm.DrawObject(CONDUIT, EditorIndex);
      end;

      if (I = CONDUIT_HAS_PUMP_INDEX) or (I = PUMP_CONTROL_METHOD_INDEX) then
        Uupdate.EditConduit;
    end
    else Result := False;
  end;
end;


function ValidateLabel(I: Integer; var S: String): Boolean;
//-----------------------------------------------------------------------------
// Validation function for map label data.
//-----------------------------------------------------------------------------
var
  V: Extended;
  L: TMapLabel;
begin
  Result := True;
  L := Project.GetMapLabel(EditorIndex);

  // Replace old text with new
  if I = LABEL_TEXT_INDEX then
  begin
    MapForm.ReplaceLabel(EditorIndex, S);
    ReplaceID(S, EditorObject, EditorIndex);
  end

  // Move label to new position
  else if I in [X_INDEX..Y_INDEX] then
  begin
    if Uutils.GetExtended(S, V) then
    begin
      MapForm.HiliteOff;
      MapForm.EraseLabel(EditorIndex);
      case I of
        X_INDEX: L.X := V;
        Y_INDEX: L.Y := V;
      end;
      MapForm.DrawObject(MAPLABEL, EditorIndex);
      MapForm.HiliteOn;
    end;
  end

  //Update connection to an anchor node
  else if I = ANCHOR_NODE_INDEX
  then Result := Uupdate.UpdateLabelAnchor(L, S, Errmsg);

  if Result = True then HasChanged := True;
end;


function ValidateEditor(I: Integer; var S: String; var E: String): Boolean;
//-----------------------------------------------------------------------------
//  Validates data for the current object being edited. I is the index of
//  the property being edited, S is the string value of the property, and
//  E is the text of any error message generated.
//-----------------------------------------------------------------------------
begin
  E := '';
  case EditorObject of
    JUNCTION..STORAGE:  Result := ValidateNode(I, S);
    CONDUIT:            Result := ValidateLink(I, S);
    MAPLABEL:           Result := ValidateLabel(I, S);
    else                Result := False;
  end;
  if Result = False then E := ErrMsg
  else MainForm.SetChangeFlags;
end;

end.

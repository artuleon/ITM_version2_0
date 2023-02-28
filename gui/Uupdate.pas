unit Uupdate;

{-------------------------------------------------------------------}
{                    Unit:    Uupdate.pas                           }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/20/22                              }
{                                                                   }
{   Delphi Pascal unit that updates references to names of data     }
{   objects that change during an editing session.                  }
{                                                                   }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Forms, Messages, Classes, Math, Controls, Dialogs,
  StrUtils, Uglobals, Uproject, Uutils, Uvertex;

procedure EditGate;
procedure EditConduit;

procedure RemoveName(const ObjType: Integer; const OldName: String);

procedure UpdateAllLengths;

procedure UpdateCurveName(const OldName: String; const NewName: String);
procedure UpdateDefOptions;

function  UpdateLabelAnchor(L: TMapLabel; const S: String;
          var Errmsg: String): Boolean;
procedure UpdateLinkColor(const Ltype: Integer; const Index: Integer;
          K: Integer);
procedure UpdateLinkHints;
function  UpdateLinkNode(const Ltype: Integer; const Index: Integer;
          const I: LongInt; const S: String; var Errmsg: String): Boolean;

procedure UpdateMapUnits;
procedure UpdateOffsets;
procedure UpdateObjectLength(const ObjType: Integer; const Index: Integer);

procedure UpdateNodeColor(const Ntype: Integer; const Index: Integer;
          K: Integer);
procedure UpdateNodeName(const OldName: String; const NewName: String);

procedure UpdateTseriesName(const OldName: String; const NewName: String);
procedure UpdateUnits;

implementation

uses
  Fmain, Fmap, Uedit, Umap, Uoutput;

const
  MSG_NO_NODE_NAMED = 'There is no node named ';
  MSG_BAD_CONNECTION = 'Node cannot be connected to itself.';


procedure EditGate;
//-----------------------------------------------------------------------------
//  Re-edits a Gate node after its Control Method property was changed
//-----------------------------------------------------------------------------
begin
  Uedit.EditObject(GATE);
end;

procedure EditConduit;
//-----------------------------------------------------------------------------
//  Re-edits a Pump link after its Control Method property was changed
//-----------------------------------------------------------------------------
begin
  Uedit.EditObject(CONDUIT);
end;

procedure RemoveName(const ObjType: Integer; const OldName: String);
//-----------------------------------------------------------------------------
//  Removes all references to Object of type ObjType with name OldName
//-----------------------------------------------------------------------------
begin
  if Project.IsNode(ObjType) then
  begin
    UpdateNodeName(OldName, '');
  end;
end;


procedure UpdateTseriesName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Timeseries OldName with NewName
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  aNode: TNode;
begin
  for I := JUNCTION to STORAGE do
  begin
    for J := 0 to Project.Lists[I].Count - 1 do
    begin
      aNode := Project.GetNode(I, J);
      if (I = JUNCTION) and SameText(aNode.ExInflow.Tseries, OldName) then
        aNode.ExInflow.Tseries := NewName;
      if (I = GATE) and SameText(aNode.Data[GATE_TIME_SERIES_INDEX], OldName) then
        aNode.Data[GATE_TIME_SERIES_INDEX] := NewName;

    end;
  end;
end;


procedure UpdateCurveName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Curve OldName with NewName
//-----------------------------------------------------------------------------
var
  J: Integer;
begin
  // Gate headloss curves
  for J := 0 to Project.Lists[GATE].Count - 1 do
  begin
    with Project.GetNode(GATE, J) do
      if SameText(Data[GATE_HLOSS_CURVE_INDEX], OldName) then
        Data[GATE_HLOSS_CURVE_INDEX] := NewName;
  end;

  // Weir rating curves
  for J := 0 to Project.Lists[WEIR].Count - 1 do
  begin
    with Project.GetnODE(WEIR, J) do
      if SameText(Data[WEIR_RATING_CURVE_INDEX], OldName) then
        Data[WEIR_RATING_CURVE_INDEX] := NewName;
  end;

  // Storage area curves
  for J := 0 to Project.Lists[STORAGE].Count - 1 do
  begin
    with Project.GetNode(STORAGE, J) do
      if SameText(Data[STORAGE_SHAPE_TABLE_INDEX], OldName) then
        Data[STORAGE_SHAPE_TABLE_INDEX] := NewName;
  end;

  // Pump curves
  for J := 0 to Project.Lists[CONDUIT].Count - 1 do
  begin
    with Project.GetLink(CONDUIT, J) do
      if SameText(Data[PUMP_CURVE_INDEX], OldName) then
        Data[PUMP_CURVE_INDEX] := NewName;
  end;

end;


procedure UpdateNodeName(const OldName: String; const NewName: String);
//-----------------------------------------------------------------------------
//  Replaces all references to Node OldName with NewName
//-----------------------------------------------------------------------------
var
  J: Integer;
  S: String;
begin
  if Length(NewName) = 0 then S := '*' else S := NewName;
  for J := 0 to Project.Lists[GATE].Count - 1 do
  begin
    with Project.GetNode(GATE, J) do
    begin
      if SameText(Data[GATE_CONTROL_NODE_INDEX], OldName)
      then Data[GATE_CONTROL_NODE_INDEX] := S;
    end;
  end;
end;


function UpdateLinkNode(const Ltype: Integer; const Index: Integer;
  const I: LongInt; const S: String; var Errmsg: String): Boolean;
//-----------------------------------------------------------------------------
//  Updates a link's node I (UP or DOWN) to node S & redraws link.
//-----------------------------------------------------------------------------
var
  Ntype  : Integer;
  Index2 : Integer;
  L      : TLink;
  N      : TNode;
  R      : TRect;

begin
  // New node must be in project's database
  if not Project.FindNode(S, Ntype, Index2) then
  begin
    Errmsg := MSG_NO_NODE_NAMED + S;
    Result := False;
    Exit;
  end;

  // Both end nodes cannot be the same
  L := Project.GetLink(Ltype, Index);
  N := Project.GetNode(Ntype, Index2);
  if ((I = UP_INDEX) and (N = L.Node2))
  or  (N = L.Node1) then
  begin
    ErrMsg := MSG_BAD_CONNECTION;
    Result := False;
    Exit;
  end;

  // Replace old end node with new one.
  // Get bounding rectangle of current link connection.
  MapForm.HiliteOff;
  R := MapForm.Map.GetAdjacencyRect(Ltype, Index, False);
  if I = UP_INDEX then L.Node1 := N
  else                 L.Node2 := N;

  // Union new bounding rectangle with old one & redraw map.
  if UnionRect(R, R, MapForm.Map.GetAdjacencyRect(Ltype, Index, False))
  then MapForm.InvalidateMap(R);
  MapForm.HiliteOn;

  // Update conduit length if AutoLength is on
  if AutoLength and (Ltype = CONDUIT)
  then Uupdate.UpdateObjectLength(Ltype, Index);
  Result := True;
end;


procedure UpdateNodeColor(const Ntype: Integer; const Index: Integer;
  K: Integer);
//-----------------------------------------------------------------------------
//  Updates color of node on map if view theme is an input variable.
//-----------------------------------------------------------------------------
begin
  if Uglobals.CurrentNodeVar < NODEOUTVAR1 then
  with NodeVariable[Uglobals.CurrentNodeVar] do
  begin
    if SourceIndex = K then
    begin
      Uoutput.SetNodeColor(Project.GetNode(Ntype, Index), K);
      MapForm.DrawObject(Ntype, Index);
    end;
  end;
end;


procedure UpdateLinkColor(const Ltype: Integer; const Index: Integer;
  K: Integer);
//-----------------------------------------------------------------------------
//  Updates color of link on map if view theme is an input variable.
//-----------------------------------------------------------------------------
begin
  if CurrentLinkVar < LINKOUTVAR1 then with LinkVariable[CurrentLinkVar] do
  begin
    if SourceIndex = K then
    begin
      Uoutput.SetLinkColor(Project.GetLink(Ltype, Index), K);
      MapForm.DrawObject(Ltype, Index);
    end;
  end;
end;


function  UpdateLabelAnchor(L: TMapLabel; const S: String;
  var Errmsg: String): Boolean;
//-----------------------------------------------------------------------------
//  Makes node with ID name S be the anchor node for map label L.
//-----------------------------------------------------------------------------
var
  Ntype: Integer;
  Index: Integer;
begin
  // Erase current label from map
  MapForm.HiliteOff;
  MapForm.EraseLabel(EditorIndex);

  // No anchor node if S is blank
  Result := True;
  if Length(Trim(S)) = 0 then L.Anchor := nil

  // Otherwise make sure S is in database
  else
  begin
    if not Project.FindNode(S, Ntype, Index) then
    begin
      ErrMsg := MSG_NO_NODE_NAMED + S;
      Result := False;
    end
    else L.Anchor := Project.GetNode(Ntype, Index);
  end;

  // Redraw label on map
  MapForm.DrawObject(MAPLABEL, EditorIndex);
  MapForm.HiliteOn;
end;


procedure UpdateLinkHints;
//-----------------------------------------------------------------------------
//  Updates the hints associated with Link properties.
//-----------------------------------------------------------------------------
const
  DEFAULT_OFFSET_TXT1 = ' (can enter * if same as node invert)';
begin
  if SameText(Project.Options.Data[LINK_OFFSETS_INDEX], 'ELEVATION') then
  begin
    ConduitHint[CONDUIT_INLET_HT_INDEX] :=
      'Elevation of conduit invert at inlet end' + DEFAULT_OFFSET_TXT1;
    ConduitHint[CONDUIT_OUTLET_HT_INDEX] :=
      'Elevation of conduit invert at outlet end' + DEFAULT_OFFSET_TXT1;
    Project.DefProp[CONDUIT].Data[CONDUIT_INLET_HT_INDEX] := '*';
    Project.DefProp[CONDUIT].Data[CONDUIT_OUTLET_HT_INDEX] := '*';
    MainForm.OffsetsBtn.Caption := 'Offsets: Elevation';
  end
  else
  begin
    ConduitHint[CONDUIT_INLET_HT_INDEX] :=
      'Depth of conduit invert above node invert at inlet end';
    ConduitHint[CONDUIT_OUTLET_HT_INDEX] :=
      'Depth of conduit invert above node invert at outlet end';
    Project.DefProp[CONDUIT].Data[CONDUIT_INLET_HT_INDEX] := '0';
    Project.DefProp[CONDUIT].Data[CONDUIT_OUTLET_HT_INDEX] := '0';
    MainForm.OffsetsBtn.Caption := 'Offsets: Depth';
  end;
end;


procedure UpdateUnits;
//-----------------------------------------------------------------------------
//  Updates flow units and the unit system for all input design variables.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Uglobals.FlowUnits := Project.Options.Data[FLOW_UNITS_INDEX];
  Project.DefProp[OPTION].Data[FLOW_UNITS_INDEX] := Uglobals.FlowUnits;
  MainForm.FlowUnitsBtn.Caption := 'Flow Units: ' + Uglobals.FlowUnits;
  if Uutils.FindKeyWord(Uglobals.FlowUnits, SIFlowUnits, 3) >= 0
  then Uglobals.UnitSystem := usSI
  else Uglobals.UnitSystem := usUS;
  for I := 0 to NODEOUTVAR1 - 1 do
    NodeUnits[I].Units := BaseNodeUnits[I, UnitSystem];
  for I := 0 to LINKOUTVAR1 - 1 do
    LinkUnits[I].Units := BaseLinkUnits[I, UnitSystem];
  UpdateMapUnits;
end;


procedure UpdateMapUnits;
//------------------------------------------------------------
//  Updates length units conversion factor
//  (conversion constants are defined in Uglobals.pas).
//------------------------------------------------------------
begin
  with MapForm.Map.Dimensions do
  begin
    LengthUCF := 1.0;

    if (UnitSystem = usUS) then
    begin
      AreaUCF := ACRESperFOOT2;
      if (Units = muMeters) or (Units = muDegrees) then
      begin
        LengthUCF := FEETperMETER;
        AreaUCF := ACRESperMETER2;
      end;
    end;

    if (UnitSystem = usSI) then
    begin
      AreaUCF := HECTARESperMETER2;
      if (Units = muFeet) then
      begin
        LengthUCF := METERSperFOOT;
        AreaUCF := HECTARESperFOOT2;
      end;
    end;

    Digits := Umap.DefMapDimensions.Digits;
    if Units = muDegrees then
    begin
      Digits := MAXDEGDIGITS;
      YperDeg := XperDeg*Cos(DegToRad((LowerLeft.Y + UpperRight.Y)/2));
    end;
  end;
end;


procedure UpdateObjectLength(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Updates a conduit's length.
//-----------------------------------------------------------------------------
var
  L: TLink;
begin
  if ObjType = CONDUIT then
  begin
    L := Project.GetLink(CONDUIT, Index);
    L.Data[CONDUIT_LENGTH_INDEX] := MapForm.Map.GetLinkLengthStr(CONDUIT, Index);
    Uedit.UpdateEditor(ObjType, Index);
  end;
end;


procedure UpdateAllLengths;
//-----------------------------------------------------------------------------
//  Updates all conduit lengths.
//-----------------------------------------------------------------------------
var
  I: Integer;
  L: TLink;
begin
  for I := 0 to Project.Lists[CONDUIT].Count-1 do
  begin
    L := Project.GetLink(CONDUIT, I);
    L.Data[CONDUIT_LENGTH_INDEX] := MapForm.Map.GetLinkLengthStr(CONDUIT, I);
  end;
end;


procedure UpdateDefOptions;
//-----------------------------------------------------------------------------
//  Updates project default options when actual options are changed.
//-----------------------------------------------------------------------------
begin
  Project.DefProp[OPTION].Data[FLOW_UNITS_INDEX] :=
    Project.Options.Data[FLOW_UNITS_INDEX];
  Project.DefProp[OPTION].Data[LINK_OFFSETS_INDEX] :=
    Project.Options.Data[LINK_OFFSETS_INDEX];
end;


function GetOffsetDepth(L: TLink; N: TNode; I: Integer): String;
//-----------------------------------------------------------------------------
//  Converts a link's Elevation offset to a Depth offset.
//-----------------------------------------------------------------------------
var
  Z: Single;
  E: Single;
begin
  if SameText(Trim(L.Data[I]), '*') then Result := '0'
  else
  begin
    Uutils.GetSingle(L.Data[I], Z);
    Uutils.GetSingle(N.Data[NODE_INVERT_INDEX], E);
    Z := Z - E;
    if Z <= 0 then Result := '0'
    else Result := Format('%.3f', [Z]);
  end;
end;


function GetOffsetElevation(L: TLink; N: TNode; I: Integer): String;
//-----------------------------------------------------------------------------
//  Converts a link's Depth offset to an Elevation offset.
//-----------------------------------------------------------------------------
var
  Z: Single;
  E: Single;
begin
  Uutils.GetSingle(L.Data[I], Z);
  if Z = 0 then Result := N.Data[NODE_INVERT_INDEX]
  else
  begin
    Uutils.GetSingle(N.Data[NODE_INVERT_INDEX], E);
    Z := Z + E;
    Result := Format('%.3f', [Z]);
  end;
end;


procedure ComputeDepthOffsets;
//-----------------------------------------------------------------------------
//  Converts Elevation offsets to Depth offsets for all links.
//-----------------------------------------------------------------------------
var
  J: Integer;
  L: TLink;
begin
  with Project.Lists[CONDUIT] do
    for J := 0 to Count-1 do
    begin
      L := TLink(Objects[J]);
      L.Data[CONDUIT_INLET_HT_INDEX] :=
        GetOffsetDepth(L, L.Node1, CONDUIT_INLET_HT_INDEX);
      L.Data[CONDUIT_OUTLET_HT_INDEX] :=
        GetOffsetDepth(L, L.Node2, CONDUIT_OUTLET_HT_INDEX);
    end;
end;


procedure ComputeElevationOffsets;
//-----------------------------------------------------------------------------
//  Converts Depth offsets to Elevation offsets for all links.
//-----------------------------------------------------------------------------
var
  J: Integer;
  L: TLink;
begin
  with Project.Lists[CONDUIT] do
    for J := 0 to Count-1 do
    begin
      L := TLink(Objects[J]);
      L.Data[CONDUIT_INLET_HT_INDEX] :=
        GetOffsetElevation(L, L.Node1, CONDUIT_INLET_HT_INDEX);
      L.Data[CONDUIT_OUTLET_HT_INDEX] :=
        GetOffsetElevation(L, L.Node2, CONDUIT_OUTLET_HT_INDEX);
    end;
end;


procedure UpdateOffsets;
const
  Msg1 = 'You have switched from using Depth Offsets to Elevation Offsets.' +
         #13 + 'Should all link offsets be converted to elevations now?';
  Msg2 = 'You have switched form using Elevation Offsets to Depth Offsets.' +
         #13 + 'Should all link offsets be converted to depths now?';
begin
  if SameText(Project.Options.Data[LINK_OFFSETS_INDEX], 'ELEVATION') then
  begin
    if Project.GetLinkCount > 0 then
    begin
      if Uutils.MsgDlg(Msg1, mtConfirmation, [mbYes, mbNo]) = mrYes
      then ComputeElevationOffsets;
    end;
  end
  else
  begin
    if Project.GetLinkCount > 0 then
    begin
      if Uutils.MsgDlg(Msg2, mtConfirmation, [mbYes, mbNo]) = mrYes
      then ComputeDepthOffsets;
    end;
  end;
  UpdateEditor(Uglobals.EditorObject, Uglobals.EditorIndex);
end;

end.

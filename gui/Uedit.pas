unit Uedit;
        
{-------------------------------------------------------------------}
{                    Unit:    Uedit.pas                             }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/20/22                              }
{                                                                   }
{   Delphi Pascal unit that adds and edits objects in an ITM        }
{   project's database.                                             }
{                                                                   }
{-------------------------------------------------------------------}

interface

uses SysUtils, Windows, Forms, Messages, Classes, Graphics,
     Controls, Dialogs, System.UITypes, Uglobals, Uproject, Uutils, Uvertex;

procedure AddLabel(const X: Extended; const Y: Extended; const S: String);
procedure AddLink(const Ltype: Integer; Node1, Node2: TNode;
          PointList: array of TPoint; N: Integer);
procedure AddNode(const Ntype: Integer; const X: Extended; const Y: Extended);
procedure AddObject(const ObjType: Integer);
procedure EditComment(const Title: string; var S: String; var Modified: Boolean);
procedure EditNodalInflow(const ObjType: Integer; const Index: Integer;
          var S: String; var Modified: Boolean);
function  EditCurve(const ObjType: Integer; const Index: Integer): String;
procedure EditLabelFont(const Index: Integer; var Modified: Boolean);
procedure EditObject(const ObjType: Integer);
function  EditTimeseries(const Index: Integer): String;
procedure UpdateEditor(const ObjType: Integer; const Index: Integer);


implementation

uses
  Fmain, Fmap, Fproped, Doptions, Dtseries, Dcurve, Dnotes, Dinflow,
  Ubrowser, Uoutput, Uupdate, Uvalidate, PropEdit;

const
  TXT_VALUE = 'Value';
  TXT_LABEL_EDITOR = 'Label Editor';

//=============================================================================
//                      Object Addition Routines
//=============================================================================

procedure AddLink(const Ltype: Integer; Node1, Node2: TNode;
  PointList: array of TPoint; N: Integer);
//-----------------------------------------------------------------------------
// Adds a new link to the database.
//-----------------------------------------------------------------------------
var
  L  : TLink;
  I  : Integer;
  Vx,
  Vy: Extended;
  ID : String;
begin
  // Create the link object and assign default properties to it
  L := TLink.Create;
  Uutils.CopyStringArray(Project.DefProp[Ltype].Data, L.Data);
  L.Ltype := Ltype;
  if Ltype = CONDUIT then
    L.HasPump := False;

  // Assign end nodes to the link
  if (Node1 <> nil) and (Node2 <> nil) then
  begin
    L.Node1 := Node1;
    L.Node2 := Node2;
  end;

  // Save the coordinates of the link's vertex points
  for I := 1 to N do
  begin
    Vx := MapForm.Map.GetX(PointList[I].X);
    Vy := MapForm.Map.GetY(PointList[I].Y);
    L.Vlist.Add(Vx, Vy);
  end;

  // Initialize output result index and ID label
  L.Zindex := -1;
  ID := Project.GetNextID(Ltype);

  // Add the link to the list of the project's objects
  Project.Lists[Ltype].AddObject(ID, L);
  I := Project.Lists[Ltype].Count - 1;
  L.ID := PChar(Project.Lists[Ltype].Strings[I]);

  // Compute the link's length if AutoLength is on
  if (Ltype = CONDUIT) and Uglobals.AutoLength then
    L.Data[CONDUIT_LENGTH_INDEX] := MapForm.Map.GetLinkLengthStr(Ltype, I);

  // Assign the link a map color
  if (Uglobals.CurrentLinkVar = NOVIEW)
  or (Uglobals.CurrentLinkVar >= LINKOUTVAR1)
  then L.ColorIndex := -1
  else Uoutput.SetLinkColor(L,
    LinkVariable[Uglobals.CurrentLinkVar].SourceIndex);

  // Draw the link on the map and update the Data Browser
  // (must update map before Browser)
  MapForm.DrawObject(Ltype, I);
  Ubrowser.BrowserAddItem(Ltype, I);
end;


procedure AddNode(const Ntype: Integer; const X: Extended; const Y: Extended);
//-----------------------------------------------------------------------------
// Adds a new node to the database.
//-----------------------------------------------------------------------------
var
  I  : Integer;
  N  : TNode;
  ID : String;
begin
  // Get a default ID label for the node
  ID := Project.GetNextID(Ntype);

  // Create a Node with default properties
  N := TNode.Create;
  N.Ntype := Ntype;
  N.X := X;
  N.Y := Y;
  N.Zindex := -1;
  Uutils.CopyStringArray(Project.DefProp[Ntype].Data, N.Data);
  N.ExInflow.FlowType := '';

  // Add the node to the list of the project's objects
  Project.Lists[Ntype].AddObject(ID, N);
  I := Project.Lists[Ntype].Count - 1;
  N.ID := PChar(Project.Lists[Ntype].Strings[I]);

  // Assign the node a map color
  N.ColorIndex := -1;
  if (Uglobals.CurrentNodeVar = NOVIEW)
  or (Uglobals.CurrentNodeVar >= NODEOUTVAR1)
  then N.ColorIndex := -1
  else Uoutput.SetNodeColor(N,
    NodeVariable[Uglobals.CurrentNodeVar].SourceIndex);

  // Draw the node on map and update the Data Browser
  // (must update map before Browser)
  MapForm.DrawObject(Ntype, I);
  Ubrowser.BrowserAddItem(Ntype, I);
end;


procedure AddLabel(const X: Extended; const Y: Extended; const S: String);
//-----------------------------------------------------------------------------
// Adds a new map label to the database.
//-----------------------------------------------------------------------------
var
  I : Integer;
  L : TMapLabel;
begin
  L := TMapLabel.Create;
  L.X := X;
  L.Y := Y;
  Project.Lists[MAPLABEL].AddObject(S, L);
  I := Project.Lists[MAPLABEL].Count - 1;
  L.Text := PChar(Project.Lists[MAPLABEL].Strings[I]);
  MapForm.DrawObject(MAPLABEL, I);
  Ubrowser.BrowserAddItem(MAPLABEL, I);
end;


procedure AddObject(const ObjType: Integer);
//-----------------------------------------------------------------------------
// Adds a new object of type ObjType to the database.
//-----------------------------------------------------------------------------
begin
  case ObjType of
    // For non-visual objects, call its editor with no reference
    // to any existing item
    GATECURVE..
    CONTROLCURVE:  EditCurve(ObjType, -1);
    TIMESERIES:    EditTimeseries(-1);
  end;
end;


//=============================================================================
//                        Editing Routines
//=============================================================================

procedure EditLabel(const Index: Integer);
//-----------------------------------------------------------------------------
//  Edits a map label with position Index in list of labels.
//-----------------------------------------------------------------------------
var
  L : TMapLabel;
begin
  // Load current properties into the PropList stringlist
  with Project do
  begin
    L := GetMapLabel(Index);
    PropList.Clear;
    PropList.Add(GetID(MAPLABEL, Index));
    PropList.Add(Format('%.3f', [L.X]));
    PropList.Add(Format('%.3f', [L.Y]));
    if L.Anchor = nil then PropList.Add('')
    else PropList.Add(L.Anchor.ID);
    PropList.Add(L.FontName);
  end;

  // Update the Property Editor form
  PropEditForm.Caption := TXT_LABEL_EDITOR;
  PropEditForm.Editor.SetProps(LabelProps, Project.PropList);
end;


procedure GetLabelFont(const Index: Integer; F: TFont);
//-----------------------------------------------------------------------------
//  Assigns the font properties of a map label to a font object.
//-----------------------------------------------------------------------------
begin
  with Project.GetMapLabel(Index) do
  begin
    F.Name := FontName;
    F.Size := FontSize;
    F.Style := [];
    if FontBold then F.Style := F.Style + [fsBold];
    if FontItalic then F.Style := F.Style + [fsItalic];
  end;
end;


procedure EditLabelFont(const Index: Integer; var Modified: Boolean);
//-----------------------------------------------------------------------------
//  Edits the font for a map label using the MainForm's FontDialog control.
//-----------------------------------------------------------------------------
begin
  Modified := False;
  with MainForm.FontDialog do
  begin
    GetLabelFont(Index, Font);
    if Execute then
    begin
      MapForm.EraseLabel(Index);
      with Project.GetMapLabel(Index) do
      begin
        FontName := Font.Name;
        FontSize := Font.Size;
        if (fsBold in Font.Style) then FontBold := True
        else FontBold := False;
        if (fsItalic in Font.Style) then FontItalic := True
        else FontItalic := False;
      end;
      MapForm.DrawObject(MAPLABEL, Index);
      UpdateEditor(MAPLABEL, Project.CurrentItem[MAPLABEL]);
      Modified := True;
    end;
  end;
end;


procedure EditNode(const Ntype: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Edits a node of type Ntype at position Index in the list of nodes.
//-----------------------------------------------------------------------------
var
  I         : Integer;
  LastIndex : Integer;
  N         : TNode;
  S         : String;
begin
  // Set the caption of the Property Editor window
  PropEditForm.Caption := ObjectLabels[Ntype] + ' ' +
    Project.GetID(Ntype, Index);

  // Get a pointer to the node object
  N := Project.GetNode(Ntype, Index);

  // Place current input data values in the PropList stringlist
  with Project do
  begin
    PropList.Clear;
    PropList.Add(N.ID);
    if N.X = MISSING then PropList.Add('')
    else PropList.Add(Format('%.3f', [N.X]));
    if N.Y = MISSING then PropList.Add('')
    else PropList.Add(Format('%.3f', [N.Y]));

    case Ntype of
      JUNCTION:   LastIndex := High(JunctionProps);
      BOUNDARY:   LastIndex := High(BoundaryProps);
      GATE:       LastIndex := High(GateProps);
      WEIR:       LastIndex := High(WeirProps);
      STORAGE:    LastIndex := High(StorageProps);
      else        LastIndex := -1;
    end;
    for I := COMMENT_INDEX to LastIndex do PropList.Add(N.Data[I]);
  end;

  // Update the Property Editor window
  case Ntype of
    JUNCTION:
      begin
        PropEditForm.Editor.SetProps(JunctionProps, Project.PropList);
      end;
    BOUNDARY:
      begin
        PropEditForm.Editor.SetProps(BoundaryProps,  Project.PropList);
      end;
    GATE:
      begin
        GateProps[GATE_HLOSS_CURVE_INDEX].List := Project.Lists[GATECURVE].Text;

        // Initially set all gate control property styles to read-only
        for I := GATE_TIME_SERIES_INDEX to GATE_CONTROL_CURVE_INDEX do
          GateProps[I].Style := esReadOnly;

        // Change control property styles depending on Control Method
        S := Project.PropList[GATE_CONTROL_METHOD_INDEX];
        // Time Series control method
        if SameText(S, ControlMethods[1]) then
        begin
          GateProps[GATE_TIME_SERIES_INDEX].Style := esComboEdit;
          GateProps[GATE_TIME_SERIES_INDEX].List := Project.Lists[TIMESERIES].Text;
          Project.PropList[GATE_CONTROL_NODE_INDEX] := '';
          Project.PropList[GATE_CONTROL_CURVE_INDEX] := '';
        end
        // Node Depth control method
        else if SameText(S, ControlMethods[2]) then
        begin
          GateProps[GATE_CONTROL_NODE_INDEX].Style := esEdit;
          GateProps[GATE_CONTROL_CURVE_INDEX].Style := esComboEdit;
          GateProps[GATE_CONTROL_CURVE_INDEX].List := Project.Lists[CONTROLCURVE].Text;
          Project.PropList[GATE_TIME_SERIES_INDEX] := '';
        end
        // No control method
        else for I := GATE_TIME_SERIES_INDEX to GATE_CONTROL_CURVE_INDEX do
          Project.PropList[I] := '';

        PropEditForm.Editor.SetProps(GateProps,  Project.PropList);
      end;
    WEIR:
      begin
        WeirProps[WEIR_RATING_CURVE_INDEX].List := Project.Lists[RATINGCURVE].Text;
        PropEditForm.Editor.SetProps(WeirProps,  Project.PropList);
      end;
    STORAGE:
      begin
        StorageProps[STORAGE_SHAPE_TABLE_INDEX].List := Project.Lists[STORAGECURVE].Text;
        PropEditForm.Editor.SetProps(StorageProps,  Project.PropList);
      end;
  end;
end;


procedure EditNodalInflow(const ObjType: Integer; const Index: Integer;
          var S: String; var Modified: Boolean);
//-----------------------------------------------------------------------------
//  Edits the external inflow to a specific node.
//-----------------------------------------------------------------------------
var
  aNode: TNode;
  InflowsForm: TInflowsForm;
begin
  Modified := False;
  InflowsForm := TInflowsForm.Create(Application);
  with InflowsForm do
  try
    aNode := Project.GetNode(ObjType, Index);
    SetData(aNode);
    if ShowModal = mrOK then
    begin
      GetData(aNode);
      if Length(aNode.ExInflow.FlowType) > 0 then S := 'YES'
      else S := 'NO';
      if ObjType = JUNCTION then
        aNode.Data[JUNCTION_INFLOWS_INDEX] := S;
      Modified := HasChanged;
    end;
  finally
    Free;
  end;
end;


procedure EditLink(const Ltype: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Edits a link of type Ltype at position Index in the list of links.
//-----------------------------------------------------------------------------
var
  I         : Integer;
  LastIndex : Integer;
  S         : String;
  L         : TLink;
begin
  // Set the caption of the Property Editor window
  S := Project.GetID(Ltype, Index);
  PropEditForm.Caption := ObjectLabels[Ltype] + ' ' + S;

  // Get a pointer to the link object
  L := Project.GetLink(Ltype, Index);

  // Place current data into the PropList stringlist
  with Project do
  begin
    PropList.Clear;
    PropList.Add(S);
    if L.Node1 = nil then PropList.Add('')
    else PropList.Add(L.Node1.ID);
    if L.Node2 = nil then PropList.Add('')
    else PropList.Add(L.Node2.ID);
    LastIndex := High(ConduitProps);
    for I := 3 to LastIndex do PropList.Add(L.Data[I]);
  end;

  // Update the Property Editor form
  PropEditForm.Editor.SetProps(ConduitProps, Project.PropList);

  // Initially set all pump control property styles to read-only
  for I := PUMP_CURVE_INDEX to PUMP_CONTROL_CURVE_INDEX do
  begin
    ConduitProps[I].Style := esReadOnly;
    Project.PropList[I] := '';
  end;

  // Activate pump property settings if conduit contains a pump
    S := Project.PropList[CONDUIT_HAS_PUMP_INDEX];
    if SameText(S, 'YES') then
    begin
      ConduitProps[PUMP_CURVE_INDEX].Style := esComboEdit;
      ConduitProps[PUMP_CURVE_INDEX].List := Project.Lists[PUMPCURVE].Text;
      ConduitProps[PUMP_LOSS_COEFF_INDEX].Style := esEdit;
      ConduitProps[PUMP_FRICTION_INDEX].Style := esEdit;
      ConduitProps[PUMP_INIT_SETTING_INDEX].Style := esEdit;
      ConduitProps[PUMP_CONTROL_METHOD_INDEX].Style := esComboList;
      Project.PropList[PUMP_CURVE_INDEX] := L.Data[PUMP_CURVE_INDEX];
      Project.PropList[PUMP_LOSS_COEFF_INDEX] := L.Data[PUMP_LOSS_COEFF_INDEX];
      Project.PropList[PUMP_FRICTION_INDEX] := L.Data[PUMP_FRICTION_INDEX];
      Project.PropList[PUMP_INIT_SETTING_INDEX] := L.Data[PUMP_INIT_SETTING_INDEX];
      Project.PropList[PUMP_CONTROL_METHOD_INDEX] := L.Data[PUMP_CONTROL_METHOD_INDEX];

      // Change control property styles depending on Control Method
      S := Project.PropList[PUMP_CONTROL_METHOD_INDEX];
      // Time Series control method
      if SameText(S, ControlMethods[1]) then
      begin
        ConduitProps[PUMP_TIME_SERIES_INDEX].Style := esComboEdit;
        ConduitProps[PUMP_TIME_SERIES_INDEX].List := Project.Lists[TIMESERIES].Text;
        Project.PropList[PUMP_TIME_SERIES_INDEX] := L.Data[PUMP_TIME_SERIES_INDEX];
      end
      // Node Depth control method
      else if SameText(S, ControlMethods[2]) then
      begin
        ConduitProps[PUMP_CONTROL_NODE_INDEX].Style := esEdit;
        ConduitProps[PUMP_CONTROL_CURVE_INDEX].Style := esComboEdit;
        ConduitProps[PUMP_CONTROL_CURVE_INDEX].List := Project.Lists[CONTROLCURVE].Text;
        Project.PropList[PUMP_CONTROL_NODE_INDEX] := L.Data[PUMP_CONTROL_NODE_INDEX];
        Project.PropList[PUMP_CONTROL_CURVE_INDEX] := L.Data[PUMP_CONTROL_CURVE_INDEX];
      end;
      // No control method
     // else for I := PUMP_TIME_SERIES_INDEX to PUMP_CONTROL_CURVE_INDEX do
     //   Project.PropList[I] := '';
    end;

  PropEditForm.Editor.SetProps(ConduitProps, Project.PropList);

end;

function EditTimeseries(const Index: Integer): String;
//-----------------------------------------------------------------------------
//  Edits data for a Time Series object.
//-----------------------------------------------------------------------------
var
  OldName : String;
  NewName : String;
  Series : TTimeseries;
  TimeseriesForm: TTimeseriesForm;
begin
  // If index >= 0 get corresponding time series object
  Result := '';
  if Index >= 0 then with Project.Lists[TIMESERIES] do
  begin
    Series := TTimeseries(Objects[Index]);
    OldName := Strings[Index];
  end

  // Otherwise create a new time series object
  else
  begin
    Series := TTimeseries.Create;
    OldName := '';
  end;

  TimeseriesForm := TTimeseriesForm.Create(Application);
  with TimeseriesForm do
  try
    SetData(Index, OldName, Series);
    if ShowModal = mrOK then
    begin
      GetData(Newname, Series);
      Result := NewName;

      // For new time series, add it to data base
      if Index < 0 then
      begin
        Project.Lists[TIMESERIES].AddObject(NewName, Series);
        Ubrowser.BrowserAddItem(TIMESERIES, Project.Lists[TIMESERIES].Count-1);
      end

      // For existing time series, update the Browser
      else
      begin
        // If name changed, then change all references to name in other objects
        if not SameText(OldName, NewName) then
        begin
          Uupdate.UpdateTseriesName(OldName, NewName);
          Project.Lists[TIMESERIES].Strings[Index] := NewName;
          MainForm.ItemListBox.Refresh;
        end;
      end;
      if Modified then MainForm.SetChangeFlags;
    end

    // If editing cancelled, then delete new timeseries
    else if Index < 0 then Series.Free;
  finally
    Free;
  end;
end;


function EditCurve(const ObjType: Integer; const Index: Integer): String;
//-----------------------------------------------------------------------------
//  Edits data for a Curve object.
//-----------------------------------------------------------------------------
var
  OldName : String;
  NewName : String;
  C : TCurve;
  CurveDataForm: TCurveDataForm;
begin
  // If index >= 0 get corresponding Curve object
  Result := '';
  if Index >= 0 then with Project.Lists[ObjType] do
  begin
    C := TCurve(Objects[Index]);
    OldName := Strings[Index];
  end

  // Otherwise create a new Curve object
  else
  begin
    C := TCurve.Create;
    OldName := '';
  end;

  CurveDataForm := TCurveDataForm.Create(Application);
  with CurveDataForm do
  try
    SetData(ObjType, Index, OldName, C);
    if ShowModal = mrOK then
    begin
      GetData(NewName, C);
      Result := NewName;

      // For new curve, add it to data base
      if Index < 0 then
      begin
        Project.Lists[ObjType].AddObject(NewName, C);
        Ubrowser.BrowserAddItem(ObjType, Project.Lists[ObjType].Count-1);
      end

      // For existing curve, update the Browser
      else
      begin
        // If name changed, then change all references to name in other objects
        if not SameText(OldName, NewName) then
        begin
          Uupdate.UpdateCurveName(OldName, NewName);
          Project.Lists[ObjType].Strings[Index] := NewName;
          MainForm.ItemListBox.Refresh;
        end;
      end;
      if Modified then MainForm.SetChangeFlags;
    end

    // If editing cancelled, then delete new curve
    else if Index < 0 then C.Free;
  finally
    Free;
  end;
end;


procedure EditOptions;
//-----------------------------------------------------------------------------
//  Edits analysis options.
//-----------------------------------------------------------------------------
var
  I: Integer;
  Updated: Boolean;
  AnalysisOptionsForm: TAnalysisOptionsForm;
begin
  Updated := False;
  I := Project.CurrentItem[OPTION];

  if I < 0 then I := 0;
  AnalysisOptionsForm := TAnalysisOptionsForm.Create(Application);
  with AnalysisOptionsForm do
  try
    SetOptions(I);
    if ShowModal = mrOK then
    begin
      GetOptions;
      Updated := HasChanged;
    end;
  finally
    Free;
  end;
  if Updated then
  begin
    MainForm.SetChangeFlags;
  end;
end;


procedure EditNotes;
//-----------------------------------------------------------------------------
//  Edits project title/notes.
//-----------------------------------------------------------------------------
var
  NotesEditorForm: TNotesEditorForm;
begin
  NotesEditorForm := TNotesEditorForm.Create(Application);
  with NotesEditorForm do
  try
    setProjectNotes(Project.Lists[NOTES]);
    if (ShowModal = mrOK) and HasChanged then
    begin
      getProjectNotes(Project.Lists[NOTES]);
      Project.Title := '';
      Project.HasItems[NOTES] := False;
      if Project.Lists[NOTES].Count > 0 then
      begin
        Project.HasItems[NOTES] := True;
        Project.Title := Project.Lists[NOTES].Strings[0];
      end;
      Uglobals.HasChanged := True;
      with MainForm do
      begin
        with PageSetupDialog.Header do
          if Uglobals.TitleAsHeader
          then Text := Project.Title
          else Text := '';
        PageSetup;
      end;
      CurrentList := OPTION;
      Ubrowser.BrowserUpdate(NOTES, Project.CurrentItem[NOTES]);
    end;
  finally
    Free;
  end;
end;


procedure EditComment(const Title: string; var S: String; var Modified: Boolean);
//-----------------------------------------------------------------------------
//  Edits an object's Comment property.
//-----------------------------------------------------------------------------
var
  NotesEditorForm: TNotesEditorForm;
begin
  NotesEditorForm := TNotesEditorForm.Create(Application);
  with NotesEditorForm do
  try
    SetComment(Title, S);
    if ShowModal = mrOK then
    begin
      GetComment(S);
      Modified := HasChanged;
    end;
  finally
    Free;
  end;
end;


procedure EditObject(const ObjType: Integer);
//-----------------------------------------------------------------------------
//  Edits data for object of type ObjType.
//-----------------------------------------------------------------------------
begin
  // Save current object class index and item index in global variables
  EditorObject := ObjType;
  EditorIndex := Project.CurrentItem[ObjType];

  // Hide the Property Editor form if not applicable
  if not Project.IsVisual(ObjType) then PropEditForm.Hide;

  // Use appropriate editor for selected item
  case ObjType of

    // Use the Property Editor form for visual objects
    JUNCTION..MAPLABEL:
    begin
      PropEditForm.Show;
      UpdateEditor(EditorObject, EditorIndex);
      PropEditForm.BringToFront;
      PropEditForm.Editor.Edit;
    end;

    // Use specific dialog form editor for other objects
    NOTES:       EditNotes;
    GATECURVE..
    CONTROLCURVE: EditCurve(ObjType, EditorIndex);
    TIMESERIES:  EditTimeseries(EditorIndex);
    OPTION:      EditOptions;
  end;
end;

procedure UpdateEditor(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Updates the contents of the Property Editor form for a specific object.
//-----------------------------------------------------------------------------
begin
  EditorObject := ObjType;
  EditorIndex := Index;
  with PropEditForm do
  begin
    if Visible then
    begin
      Editor.ColHeading2 := TXT_VALUE;
      case ObjType of
        JUNCTION..STORAGE: EditNode(ObjType,Index);
        CONDUIT:           EditLink(ObjType,Index);
        MAPLABEL:          EditLabel(Index);
      end;
    end;
  end;
end;

end.

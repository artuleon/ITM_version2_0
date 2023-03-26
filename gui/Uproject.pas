unit Uproject;

{-------------------------------------------------------------------}
{                    Unit:    Uproject.pas                          }
{                    Project: ITM                                   }
{                    Version: 2.0                                   }
{                    Date:    03/02/23                              }
{                                                                   }
{   Delphi Pascal unit that defines the Project class used to       }
{   represent a project's data objects and their properties.        }
{                                                                   }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Forms, Messages, Classes, Graphics,
  Controls, Dialogs, Math, System.UITypes, Propedit, Uutils, Uvertex;

const
  MISSING          = -1.0e10;  //Missing value
  MAXNODEPROPS     = 10;       //Max. index for node data array
  MAXLINKPROPS     = 21;       //Max. index for link data array
  MAXOPTIONS       = 19;       //Max. index for options array
  MAXPROPS         = 21;       //Max. index of all data arrays; must be as
                               //large as largest of all preceeding constants
  MAXCLASS         = 15;       //Max. index for object classes

//----------------------
// Object category codes
//----------------------
  NOTES        = 0;
  OPTION       = 1;
  JUNCTION     = 2;
  BOUNDARY     = 3;
  STORAGE      = 4;
  CONDUIT      = 5;
  PUMP         = 6;
  ORIFICE      = 7;
  WEIR         = 8;
  OUTLET       = 9;
  MAPLABEL     = 10;
  STORAGECURVE = 11;
  PUMPCURVE    = 12;
  RATINGCURVE  = 13;
  CONTROLCURVE = 14;
  TIMESERIES   = 15;

//----------------------
// Shared property codes
//----------------------
  ID_INDEX            = 0;  //ID index
  X_INDEX             = 1;  //Node's X-coord.
  Y_INDEX             = 2;  //Node's Y-coord.
  UP_INDEX            = 1;  //Link's upstream node
  DN_INDEX            = 2;  //Link's downstream node
  COMMENT_INDEX       = 3;  //Comment index
  TAG_INDEX           = 4;  //Tag index
  PROP_INDEX_OFFSET   = 4;  //Offset in Property Editor
  NODE_INVERT_INDEX   = 5;  //Node invert

//------------------------
// Junction property codes
//------------------------
  JUNCTION_MAX_DEPTH_INDEX  = 6;
  JUNCTION_INIT_DEPTH_INDEX = 7;
  JUNCTION_INFLOWS_INDEX    = 8;
  JUNCTION_DROPSHAFT_INDEX  = 9;
  JUNCTION_AREA_INDEX       = 10;

//------------------------
// Boundary property codes
//------------------------
  BOUNDARY_TYPE_INDEX       = 6;
  BOUNDARY_VALUE_INDEX      = 7;
  BOUNDARY_VENTILATED_INDEX = 8;

//----------------------------
// Storage unit property codes
//----------------------------
  STORAGE_MAX_DEPTH_INDEX    = 6;
  STORAGE_INIT_DEPTH_INDEX   = 7;
  STORAGE_SHAPE_TABLE_INDEX  = 8;
  STORAGE_OUTFLOW_INDEX      = 9;

//-----------------------
// Conduit property codes
//-----------------------
  CONDUIT_DIAMETER_INDEX    = 5;
  CONDUIT_LENGTH_INDEX      = 6;
  CONDUIT_ROUGHNESS_INDEX   = 7;
  CONDUIT_INLET_HT_INDEX    = 8;
  CONDUIT_OUTLET_HT_INDEX   = 9;
  CONDUIT_ENTRY_LOSS_INDEX  = 10;
  CONDUIT_EXIT_LOSS_INDEX   = 11;
  CONDUIT_DEPTH_TYPE_INDEX  = 12;
  CONDUIT_DEPTH_VALUE_INDEX = 13;
  CONDUIT_INIT_FLOW_INDEX   = 14;

  CONDUIT_SLOPE_INDEX       = 120;

//--------------------
// Pump property codes
//--------------------
  PUMP_CURVE_INDEX          = 5;
  PUMP_DIAM_INDEX           = 6;
  PUMP_LENGTH_INDEX         = 7;
  PUMP_FRICTION_INDEX       = 8;
  PUMP_LOSS_COEFF_INDEX     = 9;
  PUMP_INIT_SETTING_INDEX   = 10;
  PUMP_CONTROL_TYPE_INDEX   = 11;
  PUMP_CONTROL_TIMES_INDEX  = 12;
  PUMP_CONTROL_NODE_INDEX   = 13;
  PUMP_CONTROL_CURVE_INDEX  = 14;

//--------------------
// Orifice property codes
//--------------------
  ORIFICE_TYPE_INDEX          = 5;
  ORIFICE_SHAPE_INDEX         = 6;
  ORIFICE_HEIGHT_INDEX        = 7;
  ORIFICE_WIDTH_INDEX         = 8;
  ORIFICE_BOTTOM_HT_INDEX     = 9;
  ORIFICE_COEFF_INDEX         = 10;
  ORIFICE_FLAPGATE_INDEX      = 11;
  ORIFICE_INIT_SETTING_INDEX  = 12;
  ORIFICE_CLOSE_RATE_INDEX    = 13;
  ORIFICE_CONTROL_TYPE_INDEX  = 14;
  ORIFICE_CONTROL_TIMES_INDEX = 15;
  ORIFICE_CONTROL_NODE_INDEX  = 16;
  ORIFICE_CONTROL_CURVE_INDEX = 17;

//--------------------
// Weir property codes
//--------------------
  WEIR_TYPE_INDEX          = 5;
  WEIR_HEIGHT_INDEX        = 6;
  WEIR_WIDTH_INDEX         = 7;
//WEIR_SLOPE_INDEX         = 8;
  WEIR_CREST_INDEX         = 8;
  WEIR_COEFF_INDEX         = 9;
//WEIR_FLAPGATE_INDEX      = 11;
  WEIR_CONTRACT_INDEX      = 10;
//WEIR_END_COEFF_INDEX     = 13;
  WEIR_SURCHARGE_INDEX     = 11;
  WEIR_INIT_SETTING_INDEX  = 12;
  WEIR_CLOSE_RATE_INDEX    = 13;
  WEIR_CONTROL_TYPE_INDEX  = 14;
  WEIR_CONTROL_TIMES_INDEX = 15;
  WEIR_CONTROL_NODE_INDEX  = 16;
  WEIR_CONTROL_CURVE_INDEX = 17;

  WEIR_SHAPE_INDEX         = 21;

//----------------------
// Outlet property codes
//----------------------
  OUTLET_OFFSET_INDEX       = 5;
  OUTLET_FLAPGATE_INDEX     = 6;
  OUTLET_CURVE_INDEX        = 7;

//-----------------
// Curve type codes
//-----------------
  STORAGE_CURVE  = 0;
  PUMP_CURVE     = 1;
  RATING_CURVE   = 2;
  CONTROL_CURVE  = 3;

//-------------------------
// Map Label property codes
//-------------------------
  LABEL_TEXT_INDEX    = 0;
  ANCHOR_NODE_INDEX   = 3;
  METER_TYPE_INDEX    = 4;
  METER_ID_INDEX      = 5;

//---------------------------
// Analysis option type codes
//---------------------------
  FLOW_UNITS_INDEX          = 0;

  START_DATE_INDEX          = 1;  //Date/Time format
  START_TIME_INDEX          = 2;
  END_DATE_INDEX            = 3;
  END_TIME_INDEX            = 4;
  REPORT_START_DATE_INDEX   = 5;
  REPORT_START_TIME_INDEX   = 6;

  MAX_NUM_CELLS_INDEX       = 7;
  MAX_NUM_PLOT_CELLS_INDEX  = 8;

  REF_DEPTH_FRACTION_INDEX  = 9;  //Float values
  REPORT_STEP_INDEX         = 10;
  MAX_TIME_STEP_INDEX       = 11;
  PRESS_WAVE_CELERITY_INDEX = 12;
  INIT_WATER_ELEV_INDEX     = 13;

  USE_HOTSTART_FILE_INDEX   = 14;  //Strings
  SAVE_HOTSTART_FILE_INDEX  = 15;

// NOT USED
  MIN_NUM_CELLS_INDEX       = 16;
  MAX_NUM_ITER_INDEX        = 17;
  ITM_FLOW_TYPE_INDEX       = 18;
  LINK_OFFSETS_INDEX        = 19;


//---------------------
// Interface file codes
//---------------------
  AS_INPUT       = 0;
  AS_OUTPUT      = 1;
  RAINFALL_FILE  = 0;
  RUNOFF_FILE    = 1;
  HOTSTART_FILE  = 2;
  RDII_FILE      = 3;
  INFLOWS_FILE   = 4;
  OUTFLOWS_FILE  = 5;
  ROUTING_FILE   = 6;

{$I objprops.txt}  //File containing object property definitions

type

//------------------------
// X,Y coordinate position
//------------------------
  TSinglePoint = record
    X : Single;
    Y : Single;
  end;

  TExtendedPoint = record
    X : Extended;
    Y : Extended;
  end;

  TExInflow = record
    FlowType : String;
    Baseline : String;
    Tseries  : String;
    ScaleFactor : String;
  end;

//------------------------
// Default property values
//------------------------
  TDefProp = record
    Data: array[0..MAXPROPS] of String;
  end;

//-----------
// Node class
//-----------
  TNode = class(TObject)
    ID: PChar;                  // Pointer to ID label
    Ntype: Integer;             // Node type
    X, Y : Extended;            // X,Y map coordinates
    Zindex: Integer;            // Index in array of computed results
    ColorIndex: Integer;        // Index in array of map display colors
    Data : array [0..MAXNODEPROPS] of String; // Node-specific data
    ExInflow: TExInflow;        // External inflow data
    AdjList: TList;             // Link adjacency list
    PathLen: Integer;           // Path length to node

    constructor Create;
    destructor  Destroy; override;
   end;

//-----------
// Link class
//-----------
  TLink = class(TObject)
    ID: PChar;                  // Pointer to ID label
    Ltype: Integer;             // Link type
    Node1,Node2: TNode;         // Start & end nodes
    Vlist: TVertexList;         // List of vertex points
    Zindex: Integer;            // Index in array of computed results
    ITMindex: Integer;          // Index in ITM results
    ColorIndex: Integer;        // Index in array of map display colors
    Marked: Boolean;
    Data : array [0..MAXLINKPROPS] of String; // Link-specific data
    constructor Create;
    destructor  Destroy; override;
    function    AddVertex: PVertex;
    procedure   ReverseNodes;
  end;

//------------------
// Time series class
//------------------
  TTimeseries = class(TObject)
    Comment : String;
    Filename: String;
    Dates   : TStringlist;
    Times   : TStringlist;
    Values  : TStringlist;
    constructor Create;
    destructor  Destroy; override;
  end;

//------------
// Curve class
//------------
  TCurve = class(TObject)
    Comment  : String;
    CurveType: String;
    CurveCode: Integer;
    Xdata    : TStringList;
    Ydata    : TStringList;
    constructor Create;
    destructor  Destroy; override;
  end;

//----------------
// Map label class
//----------------
  TMapLabel = class(Tobject)
    Text      : PChar;       // Pointer to label text
    X         : Extended;    // X,Y coordinates
    Y         : Extended;
    Anchor    : TNode;       // Anchor node
    FontName  : String;      // Font properties
    FontSize  : Integer;
    FontBold  : Boolean;
    FontItalic: Boolean;
    constructor Create;
    procedure getFont(aFont: TFont);
  end;

//-----------------------
// Analysis options class
//-----------------------
  TOptions = class(TObject)
    Data  : array[0..MAXOPTIONS] of String;  // Analysis options
    Report: TStringlist;                     // Unused reporting options
    constructor  Create;
    destructor   Destroy; override;
  end;

//------------------------
// Project clipboard class
//------------------------
  TProjectClipboard = class
    ObjType    : Integer;
    Data       : TStringlist;
    ExInflow   : TExInflow;
    Font       : TFont;
    constructor  Create;
    destructor   Destroy; override;
    procedure Clear;
  end;

//--------------
// Project class
//--------------
  TProject = class(TObject)
  public
    Title        : String;
    Lists        : array [0..MAXCLASS] of TStringList;
    DefProp      : array [0..MAXCLASS] of TDefProp;
    HasItems     : array [0..MAXCLASS] of Boolean;
    CurrentItem  : array [0..MAXCLASS] of Integer;
    NextID       : array [0..MAXCLASS] of Integer;
    IDPrefix     : array [0..MAXCLASS] of String;
    IDincrement  : Integer;
    PropList     : TStringList;       // Property list (for Property Editor)
    ProfileNames : TStringList;       // Names of saved profile plots
    ProfileLinks : TStringList;       // Link sequences of saved profile plots
    IfaceFiles   : TStringList;       // Interface file specs
    Events       : TStringList;       // Routing event periods
    Options      : TOptions;          // Analysis options
    Clipboard    : TProjectClipboard; // Project clipboard

    constructor Create;
    destructor  Destroy; override;

    procedure   Clear;
    procedure   ClearList(const ObjType: Integer);
    procedure   ClearMapData;

    procedure   DeleteItem(const ObjType: Integer; const Index: Integer);
    procedure   DeleteItemOnly(const ObjType: Integer; const Index: Integer);
    procedure   DeleteLabelAnchors(aNode: TNode);
    function    DupID(const ID: String; const Obj: Integer;
                const Indx: Integer): Boolean;

    function    FindCurve(const S: String; var Ctype: Integer;
                  var Index: Integer): Boolean;
    function    FindLink(const S: String; var Ltype: Integer;
                  var Index: Integer): Boolean;
    function    FindNode(const S: String; var Ntype: Integer;
                  var Index: Integer): Boolean;

    function    GetCurveCount: Integer;
    function    GetID(const ObjType: Integer; const Index: Integer): String;
    function    GetLastNodes(var N1, N2: TNode): Boolean;
    function    GetLink(const Ltype: Integer; const Index: Integer): TLink;
    function    GetLinkCount: Integer;
    function    GetMapLabel(const Index: Integer): TMapLabel;
    function    GetNextID(const ObjType : Integer): String;
    function    GetNode(const Ntype: Integer; const Index: Integer):TNode;
    function    GetNodeCount: Integer;

    function    HasResults(const ObjType: Integer; const Index: Integer): Boolean;

    procedure   InitCurrentItems;
    function    IsCurve(const ObjType: Integer): Boolean;
    function    IsNode(const ObjType: Integer): Boolean;
    function    IsLink(const ObjType: Integer): Boolean;
    function    IsSortable(const ObjType: Integer): Boolean;
    function    IsVisual(const ObjType: Integer): Boolean;

  private
    function    IDExists(const ObjType: Integer; const ID: string): Boolean;
  end;

implementation

uses
  Uupdate;

//-----------------------------------------------------------------------------
// Project class methods
//-----------------------------------------------------------------------------

constructor TProject.Create;
var
  I: Integer;
begin
  inherited Create;
  for I := 0 to MAXCLASS do
  begin
    Lists[I] := TStringList.Create;
  end;
  PropList     := TStringList.Create;
  ProfileNames := TStringlist.Create;
  ProfileLinks := TStringlist.Create;
  IfaceFiles   := TStringlist.Create;
  Events       := TStringList.Create;
  Options      := TOptions.Create;
  Clipboard    := TProjectClipboard.Create;
end;


destructor TProject.Destroy;
var
  I : Integer;
begin
  for I := 0 to MAXCLASS do
  begin
    Lists[I].Free;
    CurrentItem[I] := -1;
  end;
  PropList.Free;
  ProfileNames.Free;
  ProfileLinks.Free;
  IfaceFiles.Free;
  Events.Free;
  Options.Free;
  Clipboard.Free;
  inherited Destroy;
end;


procedure TProject.DeleteItem(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Deletes a specific object from the project.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  if (Lists[ObjType].Count > 0) then
  begin
    S := Lists[ObjType].Strings[Index];

    // Free the object & delete it from its object list
    Lists[ObjType].Objects[Index].Free;
    Lists[ObjType].Delete(Index);

    // Remove any references to object's name in other objects
    Uupdate.RemoveName(ObjType, S);

    // Update the CurrentItem pointer for the object class
    if Index >= Lists[ObjType].Count
    then CurrentItem[ObjType] := Lists[ObjType].Count - 1
    else CurrentItem[ObjType] := Index;

  end;
  if Lists[ObjType].Count = 0 then HasItems[ObjType] := False;
end;


procedure TProject.DeleteItemOnly(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Deletes a specific object from the project without removing references
//  to object's name in other objects
//-----------------------------------------------------------------------------
begin
  Lists[ObjType].Objects[Index].Free;
  Lists[ObjType].Delete(Index);
  if Index >= Lists[ObjType].Count
  then CurrentItem[ObjType] := Lists[ObjType].Count - 1
  else CurrentItem[ObjType] := Index;
  if Lists[ObjType].Count = 0 then HasItems[ObjType] := False;
end;


procedure TProject.DeleteLabelAnchors(aNode: TNode);
//-----------------------------------------------------------------------------
//  Deletes references to node aNode in the Anchor property of all map labels.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  for I := 0 to Lists[MAPLABEL].Count - 1 do
  begin
    with GetMapLabel(I) do
      if Anchor = aNode then Anchor := nil;
  end;
end;

procedure TProject.ClearMapData;
//-----------------------------------------------------------------------------
//  Clears the display of all objects on the study area map.
//-----------------------------------------------------------------------------
var
  I, J     : Integer;
  aNode    : TNode;
  aLink    : TLink;
  aLabel   : TMapLabel;
begin
  for I := 0 to MAXCLASS do
  begin
    if IsNode(I) then
    begin
      for J := 0 to Lists[I].Count-1 do
      begin
        aNode := GetNode(I,J);
        aNode.X := MISSING;
        aNode.Y := MISSING;
      end;
    end
    else if IsLink(I) then
    begin
      for J := 0 to Lists[I].Count-1 do
      begin
        aLink := GetLink(I, J);
        aLink.Vlist.Free;
        aLink.Vlist := TVertexList.Create;
      end;
    end
    else if I = MAPLABEL then
    begin
      for J := 0 to Lists[MAPLABEL].Count-1 do
      begin
        aLabel := GetMapLabel(J);
        aLabel.X := MISSING;
        aLabel.Y := MISSING;
      end;
    end;
  end;
end;


procedure TProject.ClearList(const ObjType: Integer);
//-----------------------------------------------------------------------------
//  Deletes all objects of a given type from the project.
//-----------------------------------------------------------------------------
var
  J: Integer;
begin
  with Lists[ObjType] do
  begin
    for J := 0 to Count - 1 do Objects[J].Free;
    Clear;
  end;
end;


procedure TProject.Clear;
//-----------------------------------------------------------------------------
//  Deletes all objects from the project.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Delete links first ( otherwise their end nodes will point nowhere)
  for I := 0 to MAXCLASS do
    if IsLink(I) then ClearList(I);

  // Then delete all other objects
  for I := 0 to MAXCLASS do
    if not IsLink(I) then ClearList(I);

  // Reset the current item and next ID number for each class of object
  for I := 0 to MAXCLASS do
  begin
    CurrentItem[I] := -1;
    NextID[I] := IDIncrement;
  end;

  // Clear all remaining project data
  Title := '';
  ProfileNames.Clear;
  ProfileLinks.Clear;
  IfaceFiles.Clear;
  Events.Clear;
  Options.Report.Clear;
end;


function TProject.GetCurveCount: Integer;
//-----------------------------------------------------------------------------
//  Counts the number of curve objects in the project.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to MAXCLASS do
    if IsCurve(I) then Result := Result + Lists[I].Count;
end;


function TProject.GetNodeCount: Integer;
//-----------------------------------------------------------------------------
//  Counts the number of node objects in the project.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to MAXCLASS do
    if IsNode(I) then Result := Result + Lists[I].Count;
end;


function TProject.GetLastNodes(var N1, N2: TNode): Boolean;
//-----------------------------------------------------------------------------
// Retrieves last two nodes added to the database
// (used to supply default end nodes for a new link).
//-----------------------------------------------------------------------------
var
  I, J: Integer;
begin
  N1 := nil;
  N2 := nil;
  for I := 0 to MAXCLASS do
  begin
    if not IsNode(I) then continue;
    for J := Lists[I].Count-1 downto 0 do
    begin
      if N2 = nil then N2 := GetNode(I, J)
      else
      begin
        N1 := GetNode(I, J);
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;


function TProject.GetLinkCount: Integer;
//-----------------------------------------------------------------------------
//  Counts the number of link objects in the project.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to MAXCLASS do
    if IsLink(I) then Result := Result + Lists[I].Count;
end;


procedure TProject.InitCurrentItems;
//-----------------------------------------------------------------------------
//  Initializes the CurrentItem pointer for each list of object type.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  Lists[OPTION].Add('Dates/Time Steps');
  Lists[OPTION].Add('ITM Options');
  Lists[OPTION].Add('Hot Start Files');

  for I := 0 to MAXCLASS do
  begin
    if Lists[I].Count = 0 then
    begin
      CurrentItem[I] := -1;
      HasItems[I] := False;
    end
    else
    begin
      CurrentItem[I] := 0;
      HasItems[I] := True;
    end;
    HasItems[OPTION] := True;
    NextID[I] := IDIncrement;
  end;
  Uupdate.UpdateLinkHints;
end;

//-----------------------------------------------------------------------------
//  The following functions test what category an object belongs to.
//-----------------------------------------------------------------------------

function TProject.IsNode(const ObjType: Integer): Boolean;
begin
  if ObjType in [JUNCTION..STORAGE] then Result := True
  else Result := False;
end;

function TProject.IsLink(const ObjType: Integer): Boolean;
begin
  if ObjType in [CONDUIT..OUTLET] then Result := True
  else Result := False;
end;

function TProject.IsCurve(const ObjType: Integer): Boolean;
begin
  if ObjType in [STORAGECURVE..CONTROLCURVE]
  then Result := True
  else Result := False;
end;

function TProject.IsVisual(const ObjType: Integer): Boolean;
begin
  if ObjType in [JUNCTION..MAPLABEL] then Result := True
  else Result := False;
end;

function TProject.IsSortable(const ObjType: Integer): Boolean;
begin
  if ObjType in [JUNCTION..TIMESERIES] then Result := True
  else Result := False;
end;

//-----------------------------------------------------------------------------
//  The following functions return a pointer to the object of a given type.
//-----------------------------------------------------------------------------

function TProject.GetNode(const Ntype: Integer; const Index: Integer): TNode;
begin
  Result := TNode(Lists[Ntype].Objects[Index]);
end;

function TProject.GetLink(const Ltype: Integer; const Index: Integer): TLink;
begin
  Result := TLink(Lists[Ltype].Objects[Index]);
end;

function TProject.GetMapLabel(const Index: Integer): TMapLabel;
begin
  Result := TMapLabel(Lists[MAPLABEL].Objects[Index]);
end;

function TProject.GetID(const ObjType: Integer; const Index: Integer): String;
//-----------------------------------------------------------------------------
// Returns ID label of object of type ObjType and index Index.
//-----------------------------------------------------------------------------
begin
  Result := Lists[ObjType].Strings[Index];
end;

function TProject.HasResults(const ObjType: Integer;
  const Index: Integer): Boolean;
//-----------------------------------------------------------------------------
// Determines if computed results exist for a given object.
//-----------------------------------------------------------------------------
begin
  if IsNode(ObjType) and (GetNode(ObjType, Index).Zindex >= 0)
  then Result := True
  else if IsLink(ObjType) and (GetLink(ObjType, Index).Zindex >= 0)
  then Result := True
  else Result := False;
end;

//-----------------------------------------------------------------------------
//  The following functions locate an object in the data base given its ID name.
//-----------------------------------------------------------------------------

function TProject.FindNode(const S: String; var Ntype: Integer;
  var Index: Integer): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to MAXCLASS do
  begin
    if not IsNode(I) then continue;
    Ntype := I;
    Index := Lists[Ntype].IndexOf(S);
    if Index >= 0 then Exit;
  end;
  Result := False;
end;

function TProject.FindLink(const S: String; var Ltype: Integer;
  var Index: Integer): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to MAXCLASS do
  begin
    if not IsLink(I) then continue;
    Ltype := I;
    Index := Lists[Ltype].IndexOf(S);
    if Index >= 0 then Exit;
  end;
  Result := False;
end;

function TProject.FindCurve(const S: String; var Ctype: Integer;
  var Index: Integer): Boolean;
var
  I: Integer;
begin
  Result := True;
  Index := -1;
  for I := 0 to MAXCLASS do
  begin
    if not IsCurve(I) then continue;
    Ctype := I;
    Index := Lists[Ctype].IndexOf(S);
    if Index >= 0 then Exit;
  end;
  Result := False;
end;

//=============================================================================
//                    Object ID management functions
//=============================================================================

function TProject.DupID(const ID: String; const Obj: Integer;
  const Indx: Integer): Boolean;
//-----------------------------------------------------------------------------
// Checks if ID for object Indx of type Obj already exists.
//-----------------------------------------------------------------------------
var
  I : Integer;
  J : Integer;
  S : String;
  P : PChar;
begin
  // Temporarily blank out ID
  Result := False;
  S := GetID(Obj, Indx);
  Lists[Obj].Strings[Indx] := '';

  // Check if ID already in use by another object of same type
  // NOTE: subcatchments and nodes cannot share same IDs
  if IsNode(Obj) then
  begin
    if FindNode(ID, I, J) then Result := True;
  end
  else if IsLink(Obj) then
  begin
    if FindLink(ID, I, J) then Result := True;
  end
  else if isCurve(Obj) then
  begin
    if FindCurve(ID, I, J) then Result := True;
  end
  else
  begin
   J := Lists[Obj].IndexOf(ID);
   if J >= 0 then Result := True;
  end;

  // Restore ID property for objects that have one
  Lists[Obj].Strings[Indx] := S;
  P := PChar(Lists[Obj].Strings[Indx]);
  if IsNode(Obj) then GetNode(Obj, Indx).ID := P
  else if IsLink(Obj) then GetLink(Obj, Indx).ID := P;
end;

function TProject.IDExists(const ObjType: Integer; const ID: string): Boolean;
//-----------------------------------------------------------------------------
// Determines if object already has ID (used by GetNextID function).
//-----------------------------------------------------------------------------
var
  I, J: Integer;
begin
  Result := False;

  // Object is a node
  if IsNode(ObjType) then
  begin
    if FindNode(ID, I, J) then Result := True;
  end

  // Object is a link
  else if IsLink(ObjType) then
  begin
    if FindLink(ID, I, J) then Result := True;
  end

  // Object is a curve
  else if IsCurve(ObjType) then
  begin
    if FindCurve(ID, I, J) then Result := True;
  end

  // Object is anything else
  else if Lists[ObjType].IndexOf(ID) >= 0 then
    Result := True;
end;

function TProject.GetNextID(const ObjType : Integer): String;
//-----------------------------------------------------------------------------
// Returns next available default ID tag for network object.
//-----------------------------------------------------------------------------
var
  N : LongInt;
  S : String;
  V : LongInt;
  Code: Integer;
begin
  // Get ID prefix and next ID number.
  S := IDPrefix[ObjType];
  Val(S, V, Code);
  N := NextID[ObjType];

  // Keep incrementing ID number until a unique ID is created.
  // If prefix is a number, then ID = Prefix + Next ID Number.
  if (Code = 0) then
  begin
    while IDExists(ObjType, IntToStr(V+N)) do Inc(N, IDIncrement);
    Result := IntToStr(V+N);
  end

  // Otherwise ID = concatonation of Prefix & ID number.
  else
  begin
    while IDExists(ObjType, S + IntToStr(N)) do Inc(N, IDIncrement);
    Result := S + IntToStr(N);
  end;

  // Save last ID number used.
  NextID[ObjType] := N;
end;

//-----------------------------------------------------------------------------
// Node class methods
//-----------------------------------------------------------------------------
constructor TNode.Create;
begin
  inherited Create;
  AdjList := TList.Create;
end;

destructor TNode.Destroy;
begin
  AdjList.Clear;
  AdjList.Free;
  inherited Destroy;
end;


//-----------------------------------------------------------------------------
// Link class methods
//-----------------------------------------------------------------------------
constructor TLink.Create;
begin
  inherited Create;
  Vlist := TVertexList.Create;
end;

destructor TLink.Destroy;
begin
  Vlist.Free;
  inherited Destroy;
end;

function TLink.AddVertex: PVertex;
//
// Adds a new vertex to a link's polyline.
//
var
  Vx, Vy : Extended;
begin
  with Vlist do
  begin
    if Vcurrent = nil then
    begin
      Vx := (Node1.X + Node2.X)/2;
      Vy := (Node1.Y + Node2.Y)/2;
    end
    else if Vcurrent^.Next = nil then
    begin
      Vx := (Vcurrent^.X + Node2.X)/2;
      Vy := (Vcurrent^.Y + Node2.Y)/2;
    end
    else
    begin
      Vx := (Vcurrent^.X + (Vcurrent^.Next)^.X)/2;
      Vy := (Vcurrent^.Y + (Vcurrent^.Next)^.Y)/2;
    end;
    Result := Add(Vx,Vy);
  end;
end;

procedure TLink.ReverseNodes;
//
// Reverses the upstream and downstream nodes of a link.
//
var
  Ntemp: TNode;
  S: String;
begin
  Ntemp := Node2;
  Node2 := Node1;
  Node1 := Ntemp;
  if Ltype = CONDUIT then
  begin
    S := Data[CONDUIT_OUTLET_HT_INDEX];
    Data[CONDUIT_OUTLET_HT_INDEX] := Data[CONDUIT_INLET_HT_INDEX];
    Data[CONDUIT_INLET_HT_INDEX] := S;
  end;
end;


//-----------------------------------------------------------------------------
// MapLabel class methods
//-----------------------------------------------------------------------------
constructor TMapLabel.Create;
begin
  inherited Create;
  X := MISSING;
  Y := MISSING;
  Anchor := nil;
  FontName := 'Arial';
  FontSize := 10;
  FontBold := False;
  FontItalic := False;
end;

procedure TMapLabel.GetFont(aFont: TFont);
//
// Places the font properties of a map label into aFont.
//
begin
  aFont.Name := FontName;
  aFont.Size := FontSize;
  aFont.Style := [];
  if FontBold then aFont.Style := aFont.Style + [fsBold];
  if FontItalic then aFont.Style := aFont.Style + [fsItalic];
end;


//-----------------------------------------------------------------------------
// Timeseries class methods
//-----------------------------------------------------------------------------
constructor TTimeseries.Create;
begin
  inherited Create;
  Filename := '';
  Dates := TStringlist.Create;
  Times := TStringlist.Create;
  Values := TStringlist.Create;
end;

destructor TTimeseries.Destroy;
begin
  Dates.Free;
  Times.Free;
  Values.Free;
  inherited Destroy;
end;


//-----------------------------------------------------------------------------
// Curve class methods
//-----------------------------------------------------------------------------
constructor TCurve.Create;
begin
  inherited Create;
  CurveType := '';
  CurveCode := 0;
  Xdata := TStringList.Create;
  Ydata := TStringList.Create;
end;

destructor TCurve.Destroy;
begin
  Xdata.Free;
  Ydata.Free;
  inherited Destroy;
end;


//------------------------------------------------------------------------------
// Options class methods
//------------------------------------------------------------------------------
constructor TOptions.Create;
begin
  inherited Create;
  Report := TStringlist.Create;
end;

destructor TOptions.Destroy;
begin
  Report.Free;
  inherited Destroy;
end;


//-----------------------------------------------------------------------------
// ProjectClipboard class methods
//-----------------------------------------------------------------------------
constructor TProjectClipboard.Create;
begin
  inherited Create;
  Data := TStringlist.Create;
  Font := TFont.Create;
  ObjType := -1;
end;

destructor TProjectClipboard.Destroy;
begin
  Data.Free;
  Font.Free;
  inherited Destroy;
end;

procedure TProjectClipboard.Clear;
begin
  Data.Clear;
  ObjType := -1;
end;

end.

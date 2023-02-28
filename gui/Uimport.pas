unit Uimport;

{-------------------------------------------------------------------}
{                    Unit:    Uimport.pas                           }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/23/22                              }
{                                                                   }
{   Delphi Pascal unit that imports an ITM project's data from a    }
{   a formatted text file.                                          }
{-------------------------------------------------------------------}

interface

uses
  Classes, Forms, Controls, Dialogs, SysUtils, Windows, Math, StrUtils,
  Uutils, Uglobals;

const
  ITEMS_ERR    = 1;
  KEYWORD_ERR  = 2;
  NODE_ERR     = 3;
  LINK_ERR     = 4;
  NUMBER_ERR   = 5;
  TIMESTEP_ERR = 6;
  DATE_ERR     = 7;
  MAX_ERRORS   = 50;

type
  TFileType = (ftInput, ftImport);

// These routines can be called from other units
function  ErrMsg(const ErrCode: Integer; const Name: String): Integer;
function  OpenProject(const Fname: String): TInputFileType;
function  ReadInpFile(const Fname: String):Boolean;
procedure SetDefaultDates;

implementation

uses
  Fmain, Fmap, Fstatus, Uexport, Uinifile, Uproject, Umap,
  Ucoords, Uvertex, Uupdate;

const
  MSG_READING_PROJECT_DATA = 'Reading project data... ';
  TXT_ERROR = 'Error ';
  TXT_AT_LINE = ' at line ';
  TXT_MORE_ERRORS = ' more errors found in file.';
  TXT_ERROR_REPORT = 'Error Report for File ';

  SectionWords : array[0..23] of PChar =
    ('[TITLE',                    //0
     '[OPTION',                   //1
     '[JUNCTION',                 //2
     '[CONSTBOUND',               //3
     '[GATE',                     //4
     '[RATINGUNIT',               //5
     '[STORAGE',                  //6
     '[CONDUIT',                  //7
     '[PUMP',                     //8
     '[XSECTION',                 //9
     '[LOSS',                     //10
     '[INFLOW',                   //11
     '[CURVE',                    //12
     '[TIMESERIES',               //13
     '[FILE',                     //14
     '[MAP',                      //15
     '[COORDINATES',              //16
     '[VERTICES',                 //17
     '[LABELS',                   //18
     '[BACKDROP',                 //19
     '[PROFILE',                  //20
     '[TAG',                      //21
     '[BOUNDAR',                  //22
     '[WEIR');                    //23

var
  FileType     : TFileType;
  InpFile      : String;
  ErrList      : TStringlist;
  NodeList     : TStringlist;
  LinkList     : TStringlist;
  TokList      : TStringlist;
  Ntoks        : Integer;
  Section      : Integer;
  LineCount    : LongInt;
  ErrCount     : LongInt;
  Line         : String;
  Comment      : String;
  PrevID       : String;
  PrevIndex    : Integer;
  CurveType    : Integer;
  MapExtentSet : Boolean;

  // These are deprecated attributes of a backdrop image
  BackdropX    : Extended;
  BackdropY    : Extended;
  BackdropW    : Extended;
  BackdropH    : Extended;


function ErrMsg(const ErrCode: Integer; const Name: String): Integer;
//-----------------------------------------------------------------------------
//  Adds an error message for a specific error code to the list
//  of errors encountered when reading an input file.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  if ErrCount <= MAX_ERRORS then
  begin
    case ErrCode of
      ITEMS_ERR:    S := 'Too few items ';
      KEYWORD_ERR:  S := 'Unrecognized keyword (' + Name + ') ';
      NODE_ERR:     S := 'Undefined Node (' + Name + ') referenced ';
      LINK_ERR:     S := 'Undefined Link (' + Name + ') referenced ';
      NUMBER_ERR:   S := 'Illegal numeric value (' + Name + ') ';
      TIMESTEP_ERR: S := 'Illegal time step value ';
      DATE_ERR:     S := 'Illegal date/time value ';
      else          S := 'Unknown error ';
    end;
    S := S + 'at line ' + IntToStr(LineCount) + ':';
    ErrList.Add(S);
    if Section >= 0 then ErrList.Add(SectionWords[Section] + ']');
    ErrList.Add(Line);
    ErrList.Add('');
  end;
  Result := ErrCode;
end;

function FindNode(const ID: String): TNode;
//-----------------------------------------------------------------------------
// Finds a Node object given its ID name.
//-----------------------------------------------------------------------------
var
  Index: Integer;
  Ntype: Integer;
begin
  Result := nil;
  if (FileType = ftInput) then
  begin
    if NodeList.Find(ID, Index)
    then Result := TNode(NodeList.Objects[Index]);
  end
  else
  begin
    if (Project.FindNode(ID, Ntype, Index))
    then Result := Project.GetNode(Ntype, Index);
  end;
end;

function FindLink(const ID: String): TLink;
//-----------------------------------------------------------------------------
// Finds a Link object given its ID name.
//-----------------------------------------------------------------------------
var
  Index : Integer;
  Ltype : Integer;
begin
  Result := nil;
  if (FileType = ftInput) then
  begin
    if LinkList.Find(ID, Index)
    then Result := TLink(LinkList.Objects[Index]);
  end
  else
  begin
    if (Project.FindLink(ID, Ltype, Index))
    then Result := Project.GetLink(Ltype, Index);
  end;
end;

function ReadTitleData(Line: String): Integer;
//-----------------------------------------------------------------------------
// Adds Line of text to a project's title and notes.
//-----------------------------------------------------------------------------
begin
  if Project.Lists[NOTES].Count = 0 then Project.Title := Line;
  Project.Lists[NOTES].Add(Line);
  Project.HasItems[NOTES] := True;
  Result := 0;
end;

function ReadJunctionData: Integer;
//-----------------------------------------------------------------------------
//  Reads junction data from a line of input.
//-----------------------------------------------------------------------------
var
  aNode: TNode;
  ID   : String;
begin
  if Ntoks < 6
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    aNode := TNode.Create;
    NodeList.AddObject(ID, aNode);
    aNode.Ntype := JUNCTION;
    aNode.X := MISSING;
    aNode.Y := MISSING;
    aNode.Zindex := -1;
    Uutils.CopyStringArray(Project.DefProp[JUNCTION].Data, aNode.Data);
    aNode.Data[NODE_INVERT_INDEX] := TokList[1];
    aNode.Data[JUNCTION_MAX_DEPTH_INDEX]  := TokList[2];
    aNode.Data[JUNCTION_INIT_DEPTH_INDEX] := TokList[3];
    aNode.Data[JUNCTION_AREA_INDEX]       := TokList[4];
    if SameText(TokList[5], 'DROPSHAFT') then
      aNode.Data[JUNCTION_DROPSHAFT_INDEX] := 'YES'
    else
      aNode.Data[JUNCTION_DROPSHAFT_INDEX] := 'NO';
    aNode.Data[COMMENT_INDEX ] := Comment;
    Project.Lists[JUNCTION].AddObject(ID, aNode);
    Project.HasItems[JUNCTION] := True;
    Result := 0;
  end;
end;

function ReadBoundaryData: Integer;
//-----------------------------------------------------------------------------
//  Reads Boundary node data from a line of input.
//-----------------------------------------------------------------------------
var
  aNode: TNode;
  ID   : String;
begin
  if Ntoks < 5
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    aNode := TNode.Create;
    NodeList.AddObject(ID, aNode);
    aNode.Ntype := BOUNDARY;
    aNode.X := MISSING;
    aNode.Y := MISSING;
    aNode.Zindex := -1;
    Uutils.CopyStringArray(Project.DefProp[BOUNDARY].Data, aNode.Data);
    aNode.Data[NODE_INVERT_INDEX] := TokList[1];
    aNode.Data[BOUNDARY_TYPE_INDEX] := TokList[2];
    aNode.Data[BOUNDARY_VENTILATED_INDEX] := TokList[3];
    aNode.Data[BOUNDARY_VALUE_INDEX] := TokList[4];
    aNode.Data[COMMENT_INDEX] := Comment;
    Project.Lists[BOUNDARY].AddObject(ID, aNode);
    Project.HasItems[BOUNDARY] := True;
    Result := 0;
  end;
end;

function ReadStorageData: Integer;
//-----------------------------------------------------------------------------
//  Reads storage unit data from a line of input.
//-----------------------------------------------------------------------------
var
  aNode: TNode;
  ID   : String;
begin
  Result := 0;
  if Ntoks < 6
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    aNode := TNode.Create;
    NodeList.AddObject(ID, aNode);
    aNode.Ntype := STORAGE;
    aNode.X := MISSING;
    aNode.Y := MISSING;
    aNode.Zindex := -1;
    Uutils.CopyStringArray(Project.DefProp[STORAGE].Data, aNode.Data);
    Project.Lists[STORAGE].AddObject(ID, aNode);
    aNode.Data[NODE_INVERT_INDEX]        := TokList[1];
    aNode.Data[STORAGE_MAX_DEPTH_INDEX]  := TokList[2];
    aNode.Data[STORAGE_INIT_DEPTH_INDEX] := TokList[3];
    aNode.Data[STORAGE_SHAPE_TABLE_INDEX] := TokList[5];
    if Ntoks > 6 then
      aNode.Data[STORAGE_OUTFLOW_INDEX] := TokList[6];
    aNode.Data[COMMENT_INDEX ] := Comment;
    Project.HasItems[STORAGE] := True;
  end;
end;

function ReadGateData: Integer;
//-----------------------------------------------------------------------------
//  Reads Gate node data from a line of input.
//-----------------------------------------------------------------------------
var
  aNode: TNode;
  ID   : String;
begin
  if Ntoks < 3
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    Result := 0;
    ID := TokList[0];
    aNode := TNode.Create;
    NodeList.AddObject(ID, aNode);
    aNode.Ntype := GATE;
    aNode.X := MISSING;
    aNode.Y := MISSING;
    aNode.Zindex := -1;
    Uutils.CopyStringArray(Project.DefProp[GATE].Data, aNode.Data);
    Project.Lists[GATE].AddObject(ID, aNode);
    Project.HasItems[GATE] := True;
    aNode.Data[COMMENT_INDEX ] := Comment;
    aNode.Data[NODE_INVERT_INDEX] := TokList[1];
    aNode.Data[GATE_HLOSS_CURVE_INDEX] := TokList[2];
    if Ntoks > 3 then
    begin
      if not SameText(TokList[3], '*') then
        aNode.Data[GATE_TIME_SERIES_INDEX] := TokList[3];
    end;
    if Ntoks > 4 then
      aNode.Data[GATE_INIT_OPENING_INDEX] := TokList[4];
    if Ntoks > 5 then
      aNode.Data[GATE_OPEN_RATE_INDEX] := TokList[5];
    if Ntoks > 7 then
    begin
      aNode.Data[GATE_CONTROL_NODE_INDEX] := TokList[6];
      aNode.Data[GATE_CONTROL_CURVE_INDEX] := TokList[7];
    end;

    if Length(aNode.Data[GATE_TIME_SERIES_INDEX]) > 0 then
      aNode.Data[GATE_CONTROL_METHOD_INDEX] := ControlMethods[1]
    else if Length(aNode.Data[GATE_CONTROL_CURVE_INDEX]) > 0 then
      aNode.Data[GATE_CONTROL_METHOD_INDEX] := ControlMethods[2]
    else
      aNode.Data[GATE_CONTROL_METHOD_INDEX] := ControlMethods[0];
  end;
end;

function ReadWeirData: Integer;
//-----------------------------------------------------------------------------
//  Reads Weir node data from a line of input.
//-----------------------------------------------------------------------------
var
  aNode: TNode;
  ID   : String;
begin
  if Ntoks < 4
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    Result := 0;
    ID := TokList[0];
    aNode := TNode.Create;
    NodeList.AddObject(ID, aNode);
    aNode.Ntype := WEIR;
    aNode.X := MISSING;
    aNode.Y := MISSING;
    aNode.Zindex := -1;
    Uutils.CopyStringArray(Project.DefProp[WEIR].Data, aNode.Data);
    Project.Lists[WEIR].AddObject(ID, aNode);
    Project.HasItems[WEIR] := True;
    aNode.Data[COMMENT_INDEX ] := Comment;
    aNode.Data[NODE_INVERT_INDEX] := TokList[1];
    aNode.Data[WEIR_CREST_ELEV_INDEX] := TokList[2];
    aNode.Data[WEIR_RATING_CURVE_INDEX] := TokList[3];
  end;
end;

function ReadConduitData: Integer;
//-----------------------------------------------------------------------------
//  Reads Conduit data from a line of input.
//-----------------------------------------------------------------------------
var
  aLink : TLink;
  aNode1: TNode;
  aNode2: TNode;
  ID    : String;
begin
  if Ntoks < 7 then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    aNode1 := FindNode(TokList[1]);
    aNode2 := FindNode(TokList[2]);
    if (aNode1 = nil) then Result := ErrMsg(NODE_ERR, TokList[1])
    else if (aNode2 = nil) then Result := ErrMsg(NODE_ERR, TokList[2])
    else begin
      aLink := TLink.Create;
      LinkList.AddObject(ID, aLink);
      aLink.Ltype := CONDUIT;
      aLink.Node1 := aNode1;
      aLink.Node2 := aNode2;
      aLink.Zindex := -1;
      Uutils.CopyStringArray(Project.DefProp[CONDUIT].Data, aLink.Data);
      Project.Lists[CONDUIT].AddObject(ID, aLink);
      Project.HasItems[CONDUIT] := True;
      aLink.Data[CONDUIT_LENGTH_INDEX]     := TokList[3];
      aLink.Data[CONDUIT_ROUGHNESS_INDEX]  := TokList[4];
      aLink.Data[CONDUIT_INLET_HT_INDEX]   := TokList[5];
      aLink.Data[CONDUIT_OUTLET_HT_INDEX]  := TokList[6];
      if Ntoks > 7 then aLink.Data[CONDUIT_INIT_FLOW_INDEX] := TokList[7];
      if Ntoks > 8 then aLink.Data[CONDUIT_DEPTH_TYPE_INDEX] := TokList[8];
      if Ntoks > 9 then aLink.Data[CONDUIT_DEPTH_VALUE_INDEX] := TokList[9];
      aLink.Data[COMMENT_INDEX ] := Comment;
      aLink.HasPump := False;
      Result := 0;
    end;
  end;
end;

function ReadPumpData: Integer;
//-----------------------------------------------------------------------------
//  Reads pump data from a line of input.
//-----------------------------------------------------------------------------
var
  aLink : TLink;
  Ltype : Integer;
  Lindex: Integer;
begin
  if nToks < 2 then
    Result := ErrMsg(ITEMS_ERR, '')
  else if Project.FindLink(TokList[0], Ltype, Lindex) = False then
    Result := ErrMsg(LINK_ERR, TokList[0])
  else
  begin
    aLink := Project.GetLink(Ltype, Lindex);
    aLink.Data[CONDUIT_HAS_PUMP_INDEX] := 'YES';
    aLink.HasPump := True;
    aLink.Data[PUMP_CURVE_INDEX] := TokList[1];
    if nToks > 2 then
      aLink.Data[PUMP_LOSS_COEFF_INDEX] := TokList[2];
    if nToks > 3 then
      aLink.Data[PUMP_FRICTION_INDEX] := TokList[3];
    if nToks > 4 then aLink.Data[PUMP_INIT_SETTING_INDEX] := TokList[4];
    if Ntoks > 5 then
    begin
      if not SameText(TokList[5], '*') then
        aLink.Data[PUMP_TIME_SERIES_INDEX] := TokList[5];
    end;
    if Ntoks > 7 then
    begin
      aLink.Data[PUMP_CONTROL_NODE_INDEX] := TokList[6];
      aLink.Data[PUMP_CONTROL_CURVE_INDEX] := TokList[7];
    end;
    if Length(aLink.Data[PUMP_TIME_SERIES_INDEX]) > 0 then
      aLink.Data[PUMP_CONTROL_METHOD_INDEX] := ControlMethods[1]
    else if Length(aLink.Data[PUMP_CONTROL_CURVE_INDEX]) > 0 then
      aLink.Data[PUMP_CONTROL_METHOD_INDEX] := ControlMethods[2]
    else
      aLink.Data[PUMP_CONTROL_METHOD_INDEX] := ControlMethods[0];
    Result := 0;
  end;
end;

function ReadXsectionData: Integer;
//-----------------------------------------------------------------------------
//  Reads cross section data from a line of input.
//-----------------------------------------------------------------------------
var
  ID    : String;
  aLink : TLink;
begin
  Result := 0;
  if nToks < 3
  then Result := ErrMsg(ITEMS_ERR, '')
  else
  begin
    ID := TokList[0];
    aLink := FindLink(ID);
    if (aLink = nil)
    then Result := ErrMsg(LINK_ERR, TokList[0])
    else if aLink.Ltype = CONDUIT then
      aLink.Data[CONDUIT_DIAMETER_INDEX] := TokList[2];
  end;
end;

function ReadLossData: Integer;
//-----------------------------------------------------------------------------
//  Reads conduit loss data from a line of input.
//-----------------------------------------------------------------------------
var
  ID    : String;
  aLink : TLink;
begin
  if nToks < 3
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    ID := TokList[0];
    aLink := FindLink(ID);
    if (aLink = nil) then Result := ErrMsg(LINK_ERR, ID)
    else if (aLink.Ltype <> CONDUIT) then Result := 0
    else begin
      aLink.Data[CONDUIT_ENTRY_LOSS_INDEX] := TokList[1];
      aLink.Data[CONDUIT_EXIT_LOSS_INDEX] := TokList[2];
      Result := 0;
    end;
  end;
end;

function ReadExInflowData: Integer;
//-----------------------------------------------------------------------------
//  Reads external inflow data from a line of input.
//-----------------------------------------------------------------------------
var
  aNode : TNode;
  TSname: String;
  Sfactor: String;
  BaseFlow: String;
  X: Single;
begin
  Result := 0;
  if Ntoks < 3 then
  begin
   Result := ErrMsg(ITEMS_ERR, '');
   exit;
  end;

  aNode := FindNode(TokList[0]);
  if (aNode = nil) then
  begin
    Result := ErrMsg(NODE_ERR, TokList[0]);
    exit;
  end;

  if (aNode.Ntype <> JUNCTION) then exit;
  TSname := TokList[2];
  if SameText(TSname, '*') then TSname := '';
  Sfactor := '1';
  BaseFlow := '0';
  if Ntoks > 5 then Sfactor := TokList[5];
  if Ntoks > 6 then BaseFlow := TokList[6];

  if not Uutils.GetSingle(Sfactor, X) then
  begin
    Result := ErrMsg(NUMBER_ERR, TokList[5]);
    exit;
  end;
  if not Uutils.GetSingle(BaseFlow, X) then
  begin
    Result := ErrMsg(NUMBER_ERR, TokList[6]);
    exit;
  end;

  with aNode.ExInflow do
  begin
    FlowType := 'FLOW';
    Baseline := BaseFlow;
    Tseries := TSname;
    ScaleFactor := Sfactor;
  end;
  aNode.Data[JUNCTION_INFLOWS_INDEX] := 'YES';
end;

function ReadCurveData: Integer;
//-----------------------------------------------------------------------------
//  Reads curve data from a line of input.
//-----------------------------------------------------------------------------
var
  Index   : Integer;
  K       : Integer;
  L       : Integer;
  M       : Integer;
  ObjType : Integer;
  ID      : String;
  Tok1    : String;
  aCurve  : TCurve;
begin
  // Check for too few tokens
  if Ntoks < 3
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin

    // Check if curve ID is same as for previous line
    ID := TokList[0];
    if (ID = PrevID) then
    begin
      Index := PrevIndex;
      ObjType := CurveType;
    end
    else Project.FindCurve(ID, ObjType, Index);

    // Create new curve if ID not in data base
    K := 2;
    if Index < 0 then
    begin

      // Check for valid curve type keyword
      M := -1;
      Tok1 := AnsiUpperCase(TokList[1]);
      if Pos('PUMP', Tok1) = 1 then M := PUMP_CURVE
      else for L := 0 to High(CurveTypeOptions) do
      begin
        if SameText(TokList[1], CurveTypeOptions[L]) then
        begin
          M := L;
          break;
        end;
      end;
      if M < 0 then
      begin
        Result := ErrMsg(KEYWORD_ERR, TokList[1]);
        Exit;
      end;

      // Convert curve type keyword index to a curve object category
      case M of
      0: ObjType := GATECURVE;
      1: ObjType := RATINGCURVE;
      2: ObjType := STORAGECURVE;
      3: ObjType := PUMPCURVE;
      4: ObjType := CONTROLCURVE;
      end;

      // Create a new curve object
      aCurve := TCurve.Create;
      aCurve.Comment := Comment;
      aCurve.CurveType := TokList[1];
      Project.Lists[ObjType].AddObject(ID, aCurve);
      Project.HasItems[ObjType] := True;
      Index := Project.Lists[ObjType].Count - 1;
      PrevID := ID;
      PrevIndex := Index;
      CurveType := ObjType;
      K := 3;
    end;

    // Add x,y values to the list maintained by the curve
    aCurve := TCurve(Project.Lists[ObjType].Objects[Index]);
    while K <= nToks-1 do
    begin
      aCurve.Xdata.Add(TokList[K-1]);
      aCurve.Ydata.Add(TokList[K]);
      K := K + 2;
    end;
    Result := 0;
  end;
end;

function ReadTimeseriesData: Integer;
//-----------------------------------------------------------------------------
//  Reads time series data from a line of input.
//-----------------------------------------------------------------------------
const
  NEEDS_DATE = 1;
  NEEDS_TIME = 2;
  NEEDS_VALUE = 3;
var
  Index     : Integer;
  State     : Integer;
  K         : Integer;
  ID        : String;
  StrDate   : String;
  aTseries  : TTimeseries;
begin
  // Check for too few tokens
  Result := -1;
  if Ntoks < 3 then Result := ErrMsg(ITEMS_ERR, '');

  // Check if series ID is same as for previous line
  ID := TokList[0];
  if (ID = PrevID) then Index := PrevIndex
  else Index := Project.Lists[TIMESERIES].IndexOf(ID);

  // If starting input for a new series then create it
  if Index < 0 then
  begin
    aTseries := TTimeseries.Create;
    aTseries.Comment := Comment;
    Project.Lists[TIMESERIES].AddObject(ID,aTseries);
    Project.HasItems[TIMESERIES] := True;
    Index := Project.Lists[TIMESERIES].Count - 1;
    PrevID := ID;
    PrevIndex := Index;
  end;
  aTseries := TTimeseries(Project.Lists[TIMESERIES].Objects[Index]);

  // Check if external file name used
  if SameText(TokList[1], 'FILE') then
  begin
    aTseries.Filename := TokList[2];
    Result := 0;
    Exit;
  end;

  // Add values to the list maintained by the timeseries
  State := NEEDS_DATE;
  K := 1;
  while K < nToks do
  begin
    case State of

    NEEDS_DATE:
      begin
        try
          StrDate := Uutils.ConvertDate(TokList[K]);
          StrToDate(StrDate, MyFormatSettings);
          aTseries.Dates.Add(StrDate);
          Inc(K);
          if K >= nToks then break;
        except
          On EconvertError do aTseries.Dates.Add('');
        end;
        State := NEEDS_TIME;
      end;

    NEEDS_TIME:
      begin
        aTseries.Times.Add(TokList[K]);
        Inc(K);
        if K >= nToks then break;
        State := NEEDS_VALUE;
      end;

    NEEDS_VALUE:
      begin
        aTseries.Values.Add(TokList[K]);
        Result := 0;
        State := NEEDS_DATE;
        Inc(K);
      end;
    end;
  end;
  if Result = -1 then Result := ErrMsg(ITEMS_ERR, '');
end;

function ReadFileData: Integer;
//-----------------------------------------------------------------------------
//  Reads interface file usage from a line of input.
//  (Only recognizes hot start interface files.)
//-----------------------------------------------------------------------------
var
  Fname: String;
begin
  if Ntoks < 3
  then Result := ErrMsg(ITEMS_ERR, '')
  else if not SameText(TokList[0], 'USE') and
    not SameText(TokList[0], 'SAVE') then
      Result := ErrMsg(KEYWORD_ERR, TokList[0])
  else if not SameText(TokList[1], 'HOTSTART') then
    Result := ErrMsg(KEYWORD_ERR, TokList[0])
  else
  begin
    Fname := TokList[2];
    if ExtractFilePath(Fname) = ''
    then Fname := ExtractFilePath(Uglobals.InputFileName) + Fname;
    Project.IfaceFiles.Add(TokList[0] + ' ' + TokList[1] + ' ' +
     '"' + Fname + '"');
    Result := 0;
  end;
end;

function ReadOptionData: Integer;
//-----------------------------------------------------------------------------
//  Reads an analysis option from a line of input.
//-----------------------------------------------------------------------------
var
  Index : Integer;
  Keyword : String;
  S : String;
  I : Integer;
  T : Extended;
begin
  // Check which keyword applies
  Result := 0;
  if Ntoks < 2 then exit;
  Keyword := TokList[0];
  if SameText(Keyword, 'TEMPDIR') then exit;
  Index := Uutils.FindKeyWord(Keyword, OptionLabels, 17);
  if Index < 0 then
  begin
    Result := ErrMsg(KEYWORD_ERR, Keyword);
  end

  else case Index of
    START_DATE_INDEX, REPORT_START_DATE_INDEX, END_DATE_INDEX:
    begin
      S := TokList[1];
      try
        StrToDate(S, MyFormatSettings);
        TokList[1] := S;
      except
        on EConvertError do Result := ErrMsg(DATE_ERR, '');
      end;
    end;


    START_TIME_INDEX, REPORT_START_TIME_INDEX, END_TIME_INDEX:
    begin
      S := TokList[1];
      try
        StrToTime(S, MyFormatSettings);
        TokList[1] := S;
      except
        on EConvertError do Result := ErrMsg(DATE_ERR, '');
      end;
    end;

    MAX_NUM_CELLS_INDEX,
    MAX_NUM_PLOT_CELLS_INDEX, MIN_NUM_CELLS_INDEX, MAX_NUM_ITER_INDEX:
    begin
      S := TokList[1];
      try
        I := StrToInt(S);
        TokList[1] := S;
      except
        on EConvertError do Result := ErrMsg(DATE_ERR, 'NUMBER_ERR');
      end;
    end;

    REF_DEPTH_FRACTION_INDEX:
    begin
      S := TokList[1];
      try
        T := StrToFloat(S);
        if T < 0.8 then T := 0.8;
        if T > 0.99  then T := 0.99;
        // Convert from fraction to percent
        I := Trunc(T * 100.0);
        TokList[1] := IntToStr(I);
      except
        on EConvertError do Result := ErrMsg(DATE_ERR, 'NUMBER_ERR');
      end;
    end;

    REPORT_STEP_INDEX .. INIT_WATER_ELEV_INDEX:
    begin
      S := TokList[1];
      try
        T := StrToFloat(S);
        TokList[1] := S;
      except
        on EConvertError do Result := ErrMsg(DATE_ERR, 'NUMBER_ERR');
      end;
    end;

    USE_HOTSTART_FILE_INDEX,
    SAVE_HOTSTART_FILE_INDEX:
    begin
      if SameText(TokList[1], '*') then
        TokList[1] := '';
    end;

    ITM_FLOW_TYPE_INDEX:
    begin
      S := TokList[1];
      if Uutils.FindKeyword(S, ItmFlowTypes, 4) = -1 then
        Result := ErrMsg(DATE_ERR, 'KEYWORD_ERR')
    end;

    LINK_OFFSETS_INDEX:
    begin
      S := TokList[1];
      if Uutils.FindKeyword(S, LinkOffsetsOptions, 4) = -1 then
        Result := ErrMsg(DATE_ERR, 'KEYWORD_ERR')
    end;

  end;
  if Result = 0 then Project.Options.Data[Index] := TokList[1];
end;

function ReadTagData: Integer;
//-----------------------------------------------------------------------------
//  Reads in tag data from a line of input.
//-----------------------------------------------------------------------------
const
  TagTypes: array[0..1] of PChar = ('Node', 'Link');
var
  I    : Integer;
  aNode: TNode;
  aLink: TLink;
begin
  Result := 0;
  if Ntoks < 3
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    I := Uutils.FindKeyWord(TokList[0], TagTypes, 4);
    case I of

    -1: Result := ErrMsg(KEYWORD_ERR, TokList[0]);

     0: begin
          aNode := FindNode(TokList[1]);
          if (aNode <> nil) then aNode.Data[TAG_INDEX] := TokList[2];
        end;

     1: begin
          aLink := FindLink(TokList[1]);
          if (aLink <> nil) then aLink.Data[TAG_INDEX] := TokList[2];
        end;
     end;
  end;
end;

function ReadMapData: Integer;
//-----------------------------------------------------------------------------
//  Reads map dimensions data from a line of input.
//-----------------------------------------------------------------------------
var
  I       : Integer;
  Index   : Integer;
  Keyword : String;
  X       : array[1..4] of Extended;
begin
  // Check which keyword applies
  Result := 0;
  Keyword := TokList[0];
  Index := Uutils.FindKeyWord(Keyword, MapWords, 4);
  case Index of

    0:  // Map dimensions
    begin
      if Ntoks < 5 then Result := ErrMsg(ITEMS_ERR, '')
      else begin
        for I := 1 to 4 do
          if not Uutils.GetExtended(TokList[I], X[I]) then
            Result := ErrMsg(NUMBER_ERR, TokList[I]);
        if Result = 0 then with MapForm.Map.Dimensions do
        begin
          LowerLeft.X := X[1];
          LowerLeft.Y := X[2];
          UpperRight.X := X[3];
          UpperRight.Y := X[4];
          MapExtentSet := True;
        end;
      end;
    end;

    1:  //Map units
    if Ntoks > 1 then
    begin
      I := Uutils.FindKeyWord(Copy(TokList[1], 1, 1), MapUnits, 1);
      if I < 0 then Result := ErrMsg(KEYWORD_ERR, TokList[1])
      else MapForm.Map.Dimensions.Units := TMapUnits(I);
      with MapForm.Map.Dimensions do
      begin
        if Units = muDegrees then Digits := MAXDEGDIGITS
        else Digits := Umap.DefMapDimensions.Digits;
      end;
    end;

    else Result := ErrMsg(KEYWORD_ERR, Keyword);
  end;
end;

function ReadCoordData: Integer;
//-----------------------------------------------------------------------------
//  Reads node coordinate data from a line of input.
//-----------------------------------------------------------------------------
var
  X, Y  : Extended;
  aNode : TNode;
begin
  Result := 0;
  if (Ntoks < 3)
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin

    // Locate the node ID in the database
    aNode := FindNode(TokList[0]);

    // If node exists then assign it X & Y coordinates
    if (aNode <> nil) then
    begin
      if not Uutils.GetExtended(TokList[1], X) then
        Result := ErrMsg(NUMBER_ERR, TokList[1])
      else if not Uutils.GetExtended(TokList[2], Y) then
        Result := ErrMsg(NUMBER_ERR, TokList[2])
      else
      begin
        aNode.X := X;
        aNode.Y := Y;
      end;
    end;
  end;
end;

function ReadVertexData: Integer;
//-----------------------------------------------------------------------------
//  Reads link vertex coordinate data from a line of input.
//-----------------------------------------------------------------------------
var
  X, Y  : Extended;
  aLink : TLink;

begin
  Result := 0;
  if (Ntoks < 3)
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin

    // Locate the link ID in the database
    aLink := FindLink(TokList[0]);;

    // If link exists then assign it X & Y coordinates
    if (aLink <> nil) then
    begin
      if not Uutils.GetExtended(TokList[1], X) then
        Result := ErrMsg(NUMBER_ERR, TokList[1])
      else if not Uutils.GetExtended(TokList[2], Y) then
        Result := ErrMsg(NUMBER_ERR, TokList[2])
      else  aLink.Vlist.Add(X, Y);
    end;
  end;
end;

function ReadLabelData: Integer;
//-----------------------------------------------------------------------------
//  Reads map label data from a line of input.
//-----------------------------------------------------------------------------
var
  Ntype    : Integer;
  Index    : Integer;
  X, Y     : Extended;
  S        : String;
  aMapLabel: TMapLabel;
begin
  if (Ntoks < 3)
  then Result := ErrMsg(ITEMS_ERR, '')
  else begin
    if not Uutils.GetExtended(TokList[0], X) then
        Result := ErrMsg(NUMBER_ERR, TokList[0])
    else if not Uutils.GetExtended(TokList[1], Y) then
        Result := ErrMsg(NUMBER_ERR, TokList[1])
    else begin
      S := TokList[2];
      aMapLabel := TMapLabel.Create;
      aMapLabel.X := X;
      aMapLabel.Y := Y;
      Project.Lists[MAPLABEL].AddObject(S, aMapLabel);
      Index := Project.Lists[MAPLABEL].Count - 1;
      aMapLabel.Text := PChar(Project.Lists[MAPLABEL].Strings[Index]);
      Project.HasItems[MAPLABEL] := True;
      if Ntoks >= 4 then
      begin
        if (Length(TokList[3]) > 0) and
          Project.FindNode(TokList[3], Ntype, Index) then
            aMapLabel.Anchor := Project.GetNode(Ntype, Index);
      end;
      if Ntoks >= 5 then aMapLabel.FontName := TokList[4];
      if Ntoks >= 6 then aMapLabel.FontSize := StrToInt(TokList[5]);
      if Ntoks >= 7 then
        if StrToInt(TokList[6]) = 1 then aMapLabel.FontBold := True;
      if Ntoks >= 8 then
        if StrToInt(TokList[7]) = 1 then aMapLabel.FontItalic := True;
      Result := 0;
    end;
  end;
end;

function ReadBackdropData: Integer;
//-----------------------------------------------------------------------------
//  Reads map backdrop image information from a line of input.
//-----------------------------------------------------------------------------
var
  Index   : Integer;
  Keyword : String;
  I       : Integer;
  X       : array[1..4] of Extended;
begin
  // Check which keyword applies
  Result := 0;
  Keyword := TokList[0];
  Index := Uutils.FindKeyWord(Keyword, BackdropWords, 4);
  case Index of

    0:  //Backdrop file
    if Ntoks > 1 then MapForm.Map.Backdrop.Filename := TokList[1];

    1:  // Backdrop dimensions
    begin
      if Ntoks < 5 then Result := ErrMsg(ITEMS_ERR, '')
      else begin
        for i := 1 to 4 do
          if not Uutils.GetExtended(TokList[I], X[I]) then
            Result := ErrMsg(NUMBER_ERR, TokList[I]);
        if Result = 0 then with MapForm.Map.Backdrop do
        begin
          LowerLeft.X := X[1];
          LowerLeft.Y := X[2];
          UpperRight.X := X[3];
          UpperRight.Y := X[4];
        end;
      end;
    end;

    2:  //Map units - deprecated
    if Ntoks > 1 then
    begin
      i := Uutils.FindKeyWord(Copy(TokList[1], 1, 1), MapUnits, 1);
      if I < 0 then Result := ErrMsg(KEYWORD_ERR, TokList[1])
      else MapForm.Map.Dimensions.Units := TMapUnits(I);
    end;

    3, 4:  //Backdrop offset or scaling -- deprecated
    begin
      if Ntoks < 3 then Result := ErrMsg(ITEMS_ERR, '')
      else if not Uutils.GetExtended(TokList[1], X[1]) then
        Result := ErrMsg(NUMBER_ERR, TokList[1])
      else if not Uutils.GetExtended(TokList[2], X[2]) then
        Result := ErrMsg(NUMBER_ERR, TokList[2])
      else
      begin
        if Index = 3 then
        begin
          BackdropX := X[1];
          BackdropY := X[2];
        end
        else
        begin
          BackdropW := X[1];
          BackdropH := X[2];
        end;
      end;
    end;

    else Result := ErrMsg(KEYWORD_ERR, Keyword);
  end;
end;

function ReadProfileData: Integer;
//-----------------------------------------------------------------------------
//  Reads profile plot data from a line of input.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
  S   : String;
begin
  Result := 0;
  if (Ntoks < 2) then Exit;

  // Locate the profile name in the database
  I := Project.ProfileNames.IndexOf(TokList[0]);

  // If profile does not exist then create it
  if (I < 0) then
  begin
    Project.ProfileNames.Add(TokList[0]);
    I := Project.ProfileNames.Count-1;
    S := '';
    Project.ProfileLinks.Add(S);
  end
  else S := Project.ProfileLinks[I] + #13;

  // Add each remaining token to the list of links in the profile
  S := S + TokList[1];
  for J := 2 to Ntoks-1 do
    S := S + #13 + TokList[J];
  Project.ProfileLinks[I] := S;
end;

procedure SetMapDimensions;
//-----------------------------------------------------------------------------
// Determines map dimensions based on range of object coordinates.
//-----------------------------------------------------------------------------
begin
  with MapForm.Map do
  begin
    // Dimensions not provided in [MAP] section
    if not MapExtentSet then
    begin

      // Dimensions were provided in [BACKDROP] section
      if (Backdrop.LowerLeft.X <> Backdrop.UpperRight.X) then
      begin

        // Interpret these as map dimensions
        Dimensions.LowerLeft  := Backdrop.LowerLeft;
        Dimensions.UpperRight := Backdrop.UpperRight;

        // Compute backdrop dimensions from Offset and Scale values
        Backdrop.LowerLeft.X  := BackdropX;
        Backdrop.LowerLeft.Y  := BackdropY - BackdropH;
        Backdrop.UpperRight.X := BackdropX + BackdropW;
        Backdrop.UpperRight.Y := BackdropY;
      end

      // No dimensions of any kind provided
      else with Dimensions do
        Ucoords.GetCoordExtents(LowerLeft.X, LowerLeft.Y,
                                UpperRight.X, UpperRight.Y);
    end;
  end;
end;

function ParseInpLine(S: String): Integer;
//-----------------------------------------------------------------------------
//  Parses current input line depending on current section of input file.
//-----------------------------------------------------------------------------
begin
  case Section of
    1:    Result := ReadOptionData;
    2:    Result := ReadJunctionData;
    3:    Result := ReadBoundaryData;
    4:    Result := ReadGateData;
    5:    Result := ReadWeirData;
    6:    Result := ReadStorageData;
    7:    Result := ReadConduitData;
    8:    Result := ReadPumpData;
    9:    Result := ReadXsectionData;
    10:   Result := ReadLossData;
    11:   Result := ReadExInflowData;
    12:   Result := ReadCurveData;
    13:   Result := ReadTimeseriesData;
    14:   Result := ReadFileData;
    15:   Result := ReadMapData;
    16:   Result := ReadCoordData;
    17:   Result := ReadVertexdata;
    18:   Result := ReadLabelData;
    19:   Result := ReadBackdropData;
    20:   Result := ReadProfileData;
    21:   Result := ReadTagData;
    22:   Result := ReadBoundaryData;
    23:   Result := ReadWeirData;
    else  Result := 0;
  end;
end;

procedure StripComment(const Line: String; var S: String);
//-----------------------------------------------------------------------------
//  Strips comment (text following a ';') from a line of input.
//  Ignores comment if it begins with ';;'.
//-----------------------------------------------------------------------------
var
  P: Integer;
  N: Integer;
  C: String;
begin
  C := '';
  S := Trim(Line);
  P := Pos(';', S);
  if P > 0 then
  begin
    N := Length(S);
    C := Copy(S, P+1, N);
    if (Length(C) >= 1) and (C[1] <> ';') then
    begin
      if Length(Comment) > 0 then Comment := Comment + #13;
      Comment := Comment + C;
    end;
    Delete(S, P, N);
  end;
end;

function FindNewSection(const S: String): Integer;
//-----------------------------------------------------------------------------
//  Checks if S matches any of the section heading key words.
//-----------------------------------------------------------------------------
var
  K: Integer;
begin
  for K := 0 to High(SectionWords) do
  begin
    if Pos(SectionWords[K], S) = 1 then
    begin
      Result := K;
      Exit;
    end;
  end;
  Result := -1;
end;

function StartNewSection(S: String): Integer;
//-----------------------------------------------------------------------------
//  Begins reading a new section of the input file.
//-----------------------------------------------------------------------------
var
  K : Integer;

begin
  // Determine which new section to begin
  K := FindNewSection(UpperCase(S));
  if (K >= 0) then
  begin
    //Update section code
    Section := K;
    PrevID := '';
    Result := 0;
  end
  else Result := ErrMsg(KEYWORD_ERR, S);
  Comment := '';
end;

function ReadFile(var F: Textfile; const Fsize: Int64):Boolean;
//-----------------------------------------------------------------------------
//  Reads each line of a SWMM input file.
//-----------------------------------------------------------------------------
var
  Err         : Integer;
  ByteCount   : Integer;
  StepCount   : Integer;
  StepSize    : Integer;
  S           : String;
begin
  Result := True;
  ErrCount := 0;
  LineCount := 0;
  Comment := '';

  // Initialize progress meter settings
  StepCount := MainForm.ProgressBar.Max div MainForm.ProgressBar.Step;
  StepSize := Fsize div StepCount;
  if StepSize < 1000 then StepSize := 0;
  ByteCount := 0;

  // Read each line of input file
  Reset(F);
  while not Eof(F) do
  begin
    Err := 0;
    Readln(F, Line);
    Inc(LineCount);
    if StepSize > 0 then
    begin
      Inc(ByteCount, Length(Line));
      MainForm.UpdateProgressBar(ByteCount, StepSize);
    end;

    // Strip out trailing spaces, control characters & comment
    Line := TrimRight(Line);
    StripComment(Line, S);

    // Check if line begins a new input section
    if (Pos('[', S) = 1) then Err := StartNewSection(S)
    else
    begin
      // Check if line contains project title/notes
      if (Section = 0) and (Length(Line) > 0) then
      begin
        if LeftStr(Line,2) <> ';;' then Err := ReadTitleData(Line);
      end

      // If in some section, then process the input line
      else
      begin
        // Break line into string tokens and parse their contents
        Uutils.Tokenize(S, TokList, Ntoks);
        if (Ntoks > 0) and (Section >= 0) then
        begin
          Err := ParseInpLine(S);
          Comment := '';
        end

        // No current section -- file was probably not an ITM file
        else if (Ntoks > 0) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end;

    // Increment error count
    if Err > 0 then Inc(ErrCount);
  end;  //End of file.

  if ErrCount > MAX_ERRORS then ErrList.Add(
    IntToStr(ErrCount-MAX_ERRORS) + TXT_MORE_ERRORS);
end;

procedure DisplayInpErrForm(const Fname: String);
//-----------------------------------------------------------------------------
//  Displays Status Report form that lists any error messages.
//-----------------------------------------------------------------------------
begin
  SysUtils.DeleteFile((TempReportFile));
  TempReportFile := Uutils.GetTempFile(TempDir,'itm');
  ErrList.Insert(0, TXT_ERROR_REPORT + Fname + #13);
  ErrList.SaveToFile(TempReportFile);
  MainForm.MnuReportStatusClick(MainForm.MnuReportSummary);
end;

procedure ReverseVertexLists;
//-----------------------------------------------------------------------------
//  Reverses list of vertex points for each link in the project.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
begin
  for I := 0 to MAXCLASS do
  begin
    if not Project.IsLink(I) then continue;
    for J := 0 to Project.Lists[I].Count-1 do
      if Project.GetLink(I, J).Vlist <> nil then
        Project.GetLink(I, J).Vlist.Reverse;
  end;
end;

procedure SetIDPtrs;
//-----------------------------------------------------------------------------
//  Makes pointers to ID strings the ID property of objects.
//-----------------------------------------------------------------------------
var
  I, J : Integer;
begin
  for I := 0 to MAXCLASS do
  begin
    if Project.IsNode(I) then with Project.Lists[I] do
    begin
      for J := 0 to Count-1 do TNode(Objects[J]).ID := PChar(Strings[J]);
    end
    else if Project.IsLink(I) then with Project.Lists[I] do
    begin
      for J := 0 to Count-1 do TLink(Objects[J]).ID := PChar(Strings[J]);
    end
    else continue;
  end;
end;

function ReadInpFile(const Fname: String):Boolean;
//-----------------------------------------------------------------------------
//  Reads SWMM input data from a text file.
//-----------------------------------------------------------------------------
var
  F : Textfile;
begin
  // Try to open the file
  Result := False;
  AssignFile(F,Fname);
  {$I-}
  Reset(F);
  {$I+}
  if (IOResult = 0) then
  begin

    // Create stringlists
    Screen.Cursor := crHourGlass;
    MapExtentSet := False;
    ErrList := TStringList.Create;
    TokList := TStringList.Create;
    NodeList := TStringList.Create;
    LinkList := TStringList.Create;
    FileType := ftInput;
    InpFile := Fname;
    try

      // Read the file
      MainForm.ShowProgressBar(MSG_READING_PROJECT_DATA);
      NodeList.Sorted := True;
      LinkList.Sorted := True;
      Section := -1;
      Result := ReadFile(F, Uutils.GetFileSize(Fname));
      if (Result = True) then
      begin
        // Establish pointers to ID names
        SetIDPtrs;
      end;

    finally
      // Free the stringlists
      NodeList.Free;
      LinkList.Free;
      TokList.Free;
      MainForm.PageSetupDialog.Header.Text := Project.Title;
      MainForm.HideProgressBar;
      Screen.Cursor := crDefault;

      // Display errors if found & set map dimensions
      if Result = True then
      begin
        if ErrList.Count > 0 then DisplayInpErrForm(Fname);
        SetMapDimensions;
      end;
      ErrList.Free;
    end;
  end;

  // Close the input file
  CloseFile(F);
end;

procedure ClearDefaultDates;
//-----------------------------------------------------------------------------
//  Clears project's date/time settings.
//-----------------------------------------------------------------------------
begin
  with Project.Options do
  begin
    Data[START_DATE_INDEX]        := '';
    Data[START_TIME_INDEX]        := '';
    Data[REPORT_START_DATE_INDEX] := '';
    Data[REPORT_START_TIME_INDEX] := '';
    Data[END_DATE_INDEX]          := '';
    Data[END_TIME_INDEX]          := '';
  end;
end;

procedure SetDefaultDates;
//-----------------------------------------------------------------------------
//  Sets default values for project's date/time settings.
//-----------------------------------------------------------------------------
var
  StartTime: TDateTime;
  StartDate: TDateTime;
  T: TDateTime;
  D: TDateTime;
begin
  with Project.Options do
  begin

  // Process starting date/time
    try
      StartDate := StrToDate(Data[START_DATE_INDEX], MyFormatSettings);
    except
      On EConvertError do StartDate := Date;
    end;
    StartTime := Uutils.StrHoursToTime(Data[START_TIME_INDEX]);
    if StartTime < 0 then StartTime := 0;
    D := StartDate + StartTime;
    Data[START_DATE_INDEX] := DateToStr(D, MyFormatSettings);
    Data[START_TIME_INDEX] := TimeToStr(D, MyFormatSettings);

  // Process reporting start date/time
    try
      D := StrToDate(Data[REPORT_START_DATE_INDEX], MyFormatSettings);
    except
      On EConvertError do D := StartDate;
    end;
    T := Uutils.StrHoursToTime(Data[REPORT_START_TIME_INDEX]);
    if T < 0 then T := StartTime;
    D := D + T;
    Data[REPORT_START_DATE_INDEX] := DateToStr(D, MyFormatSettings);
    Data[REPORT_START_TIME_INDEX] := TimeToStr(D, MyFormatSettings);

  // Process ending date/time
    try
      D := StrToDate(Data[END_DATE_INDEX], MyFormatSettings);
    except
      On EConvertError do D := StartDate;
    end;
    T := Uutils.StrHoursToTime(Data[END_TIME_INDEX]);
    if T < 0 then T := StartTime;
    D := D + T;
    Data[END_DATE_INDEX] := DateToStr(D, MyFormatSettings);
    Data[END_TIME_INDEX] := TimeToStr(D, MyFormatSettings);
  end;
end;

function OpenProject(const Fname: String): TInputFileType;
//-----------------------------------------------------------------------------
//  Reads in project data from a file.
//-----------------------------------------------------------------------------
begin
  // Show progress meter
  ClearDefaultDates;
  Screen.Cursor := crHourGlass;
  MainForm.ShowProgressBar(MSG_READING_PROJECT_DATA);

  // Use default map dimensions and backdrop settings
  MapForm.Map.Dimensions := DefMapDimensions;
  MapForm.Map.Backdrop := DefMapBackdrop;

  // Do the following for non-temporary input files
  if not SameText(Fname, Uglobals.TempInputFile) then
  begin

    // Create a backup file
    if AutoBackup
    then CopyFile(PChar(Fname), PChar(ChangeFileExt(Fname, '.bak')), FALSE);

    // Retrieve project defaults from .INI file
    if CompareText(ExtractFileExt(Fname), '.ini') <> 0
    then Uinifile.ReadProjIniFile(ChangeFileExt(Fname, '.ini'));
  end;

  // Read and parse each line of input file
  Result := iftNone;
  if ReadInpFile(Fname) then Result := iftINP;

  // Finish processing the input data
  if Result <> iftNone then
  begin
    SetDefaultDates;                   // set any missing analysis dates
    Uupdate.UpdateUnits;               // update choice of unit system
    Uupdate.UpdateDefOptions;          // update default analysis options
    Uupdate.UpdateLinkHints;           // update hints used for offsets
  end;

  // Hide progress meter.
  MainForm.HideProgressBar;
  Screen.Cursor := crDefault;
end;

end.

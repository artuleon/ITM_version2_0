unit Uexport;

{-------------------------------------------------------------------}
{                    Unit:    Uexport.pas                           }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/24/22                              }
{                                                                   }
{   Delphi Pascal unit that exports current project data to a       }
{   formatted text file.                                            }
{-------------------------------------------------------------------}

interface

uses SysUtils, Windows, Messages, Classes, Math, Dialogs, StrUtils,
  Uglobals, Uutils, Uproject, Uvertex;

procedure ExportMap(S: TStringlist);
procedure ExportProfiles(S: TStringlist);
procedure ExportProject(S: TStringlist);
procedure ExportTags(S: TStringlist);
procedure ExportTempDir(S: TStringlist);
procedure SaveProject(Fname: String);
procedure SaveResults(Fname: String);

implementation

uses
  Fmap, Uinifile, Uoutput;

var
  XflowCount : Integer;      // # nodes w/ external inflow
  Tab        : String;       // tab or space character
  SaveToPath : String;       // path of saved project file
  Hdr1, Hdr2, Hdr3: String;  // header strings

procedure ExportComment(S: TStringlist; Comment: String);
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  if Length(Trim(Comment)) = 0 then exit;
  with Tstringlist.Create do
  try
    Clear;
    SetText(PChar(Comment));
    for I := 0 to Count-1 do S.Add(';' + Strings[I]);
  finally
    Free;
  end;
end;

procedure ExportTitle(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  with Project do
  begin
    S.Add('[TITLE]');
    S.Add(';;Project Title/Notes');
    S.Add(';;-------------------');
    for I := 0 to Lists[NOTES].Count-1 do
      S.Add(Lists[NOTES].Strings[I]);
  end;
end;

procedure ExportOptions(S: TStringlist);
//-----------------------------------------------------------------------------
var
   I: Integer;
   Line: String;
   TmpOptions: array[0..MAXOPTIONS] of String;
begin
  S.Add('');
  S.Add('[OPTIONS]');
  S.Add(';;Option                 ' + Tab + 'Value');
  S.Add(';;-----------------------' + Tab + '----------');

  with Project.Options do
  begin
    // Copy current options to a temporary  array
    // (so that blank defaults can be filled in)
    for I := 0 to MAXOPTIONS do TmpOptions[I] := Data[I];

    // Fill in blank defaults
    if Length(Trim(TmpOptions[REPORT_START_DATE_INDEX])) = 0 then
       TmpOptions[REPORT_START_DATE_INDEX] := TmpOptions[START_DATE_INDEX];
    if Length(Trim(TmpOptions[REPORT_START_TIME_INDEX])) = 0 then
       TmpOptions[REPORT_START_TIME_INDEX] := TmpOptions[START_TIME_INDEX];
    if Length(Trim(TmpOptions[END_DATE_INDEX])) = 0 then
      TmpOptions[END_DATE_INDEX] := TmpOptions[START_DATE_INDEX];
    if Length(Trim(TmpOptions[END_TIME_INDEX])) = 0 then
      TmpOptions[END_TIME_INDEX] := TmpOptions[START_TIME_INDEX];
    if Length(Trim(TmpOptions[USE_HOTSTART_FILE_INDEX])) = 0 then
      TmpOptions[USE_HOTSTART_FILE_INDEX] := '*';
    if Length(Trim(TmpOptions[SAVE_HOTSTART_FILE_INDEX])) = 0 then
      TmpOptions[SAVE_HOTSTART_FILE_INDEX] := '*';

    // Write options to string list S
    for I := FLOW_UNITS_INDEX to SAVE_HOTSTART_FILE_INDEX do
    begin
      if Length(Trim(TmpOptions[I])) = 0 then continue;
      if I = REF_DEPTH_FRACTION_INDEX then
        Line := Format('%-25s%s%.2f',[OptionLabels[I], Tab,
          StrToFloat(TmpOptions[I])/100])
      else
        Line := Format('%-25s', [OptionLabels[I]]) + Tab + TmpOptions[I];
      S.Add(Line);
    end;
    S.Add('');
  end;
end;

procedure ExportJunctions(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I     : Integer;
  N     : TNode;
  Line  : String;
begin
  if Project.Lists[JUNCTION].Count = 0 then exit;
  S.Add('');
  S.Add('[JUNCTIONS]');

Hdr1 := ';;              ' + Tab + 'Invert    ' + Tab + 'Max.      ' + Tab + 'Init.     ' + '          ' + Tab + 'Dropshaft';
Hdr2 := ';;Name          ' + Tab + 'Elev.     ' + Tab + 'Depth     ' + Tab + 'Depth     ' + 'Area      ' + Tab + '(Ventilated)';
Hdr3 := ';;--------------' + Tab + '----------' + Tab + '----------' + Tab + '----------' + '----------' + Tab + '----------';

  S.Add(Hdr1);
  S.Add(Hdr2);
  S.Add(Hdr3);
  
  with Project.Lists[JUNCTION] do
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      ExportComment(S, N.Data[COMMENT_INDEX]);
      if Length(N.ExInflow.FlowType) > 0 then Inc(XflowCount);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-10s', [N.Data[NODE_INVERT_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[JUNCTION_MAX_DEPTH_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[JUNCTION_INIT_DEPTH_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[JUNCTION_AREA_INDEX]]);
      if SameText(N.Data[JUNCTION_DROPSHAFT_INDEX], 'YES') then
        Line := Line + Tab + 'DROPSHAFT'
      else
        Line := Line + Tab + 'JUNCTION';
      S.Add(Line);
    end;
end;

procedure ExportBoundaries(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I      : Integer;
  Line   : String;
  N      : TNode;
begin
  if Project.Lists[BOUNDARY].Count = 0 then exit;
  S.Add('');
  S.Add('[CONSTBOUNDS]');
  Hdr1 := ';;              ' + Tab + 'Invert    ' + Tab + 'Boundary      ' + Tab + 'Dropshaft    ' + Tab + 'Boundary   ';
  Hdr2 := ';;Name          ' + Tab + 'Elev.     ' + Tab + 'Condition     ' + Tab + '(Ventilated)?' + Tab + 'Value      ';
  Hdr3 := ';;--------------' + Tab + '----------' + Tab + '--------------' + Tab + '-------------' + Tab + '-----------';
  S.Add(Hdr1);
  S.Add(Hdr2);
  S.Add(Hdr3);
  
  with Project.Lists[BOUNDARY] do
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      ExportComment(S, N.Data[COMMENT_INDEX]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-10s', [N.Data[NODE_INVERT_INDEX]]);
      Line := Line + Tab + Format('%-14s', [N.Data[BOUNDARY_TYPE_INDEX]]);
      Line := Line + Tab + Format('%-13s', [N.Data[BOUNDARY_VENTILATED_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[BOUNDARY_VALUE_INDEX]]);
      S.Add(Line);
    end;
end;

procedure ExportGates(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I         : Integer;
  Line      : String;
  N         : TNode;
  CtrlType  : String;
  CtrlSeries: String;
  CtrlNode  : String;
  CtrlCurve : String;
begin
  if Project.Lists[GATE].Count = 0 then exit;
  S.Add('');
  S.Add('[GATES]');
  Hdr1 := ';;              ' + Tab + 'Invert  ' + Tab + 'Head Loss       ' + Tab + 'Control         ' + Tab;
  Hdr2 := ';;Name          ' + Tab + 'Elev.   ' + Tab + 'Coeff. Curve    ' + Tab + 'Time Series     ' + Tab;
  Hdr3 := ';;--------------' + Tab + '--------' + Tab + '----------------' + Tab + '----------------' + Tab;

  Hdr1 := Hdr1 + 'Initial ' + Tab + 'Time To ' + Tab + 'Control         ' + Tab + 'Control         ';
  Hdr2 := Hdr2 + 'Opening ' + Tab + 'Open    ' + Tab + 'Node            ' + Tab + 'Curve           ';
  Hdr3 := Hdr3 + '--------' + Tab + '--------' + Tab + '----------------' + Tab + '----------------';

  S.Add(Hdr1);
  S.Add(Hdr2);
  S.Add(Hdr3);

  with Project.Lists[GATE] do
  begin
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      ExportComment(S, N.Data[COMMENT_INDEX]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-8s', [N.Data[NODE_INVERT_INDEX]]);
      Line := Line + Tab + Format('%-16s', [N.Data[GATE_HLOSS_CURVE_INDEX]]);

      CtrlType := N.Data[GATE_CONTROL_METHOD_INDEX];
      CtrlSeries := '*';
      CtrlNode := '';
      CtrlCurve := '';
      if SameText(CtrlType, ControlMethods[1]) then
      begin
        CtrlSeries := Trim(N.Data[GATE_TIME_SERIES_INDEX]);
        if Length(CtrlSeries) = 0 then CtrlSeries := '*';
      end
      else if SameText(CtrlType, ControlMethods[2]) then
      begin
        CtrlNode := Trim(N.Data[GATE_CONTROL_NODE_INDEX]);
        CtrlCurve := Trim(N.Data[GATE_CONTROL_CURVE_INDEX]);
      end;

      Line := Line + Tab + Format('%-16s', [CtrlSeries]);
      Line := Line + Tab + Format('%-8s', [N.Data[GATE_INIT_OPENING_INDEX]]);
      Line := Line + Tab + Format('%-8s', [N.Data[GATE_OPEN_RATE_INDEX]]);
      Line := Line + Tab + Format('%-16s', [CtrlNode]);
      Line := Line + Tab + Format('%-16s', [CtrlCurve]);
      S.Add(Line);
    end;
  end;
end;

procedure ExportWeirs(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I    : Integer;
  Line : String;
  N    : TNode;
begin
  if Project.Lists[WEIR].Count = 0 then exit;
  S.Add('');
  S.Add('[RATINGUNIT]');
  Hdr1 := ';;              ' + Tab + 'Invert  ' + Tab + 'Crest   ' + Tab + 'Rating          ';
  Hdr2 := ';;Name          ' + Tab + 'Elev.   ' + Tab + 'Elev.   ' + Tab + 'Curve           ';
  Hdr3 := ';;--------------' + Tab + '--------' + Tab + '--------' + Tab + '----------------';
  S.Add(Hdr1);
  S.Add(Hdr2);
  S.Add(Hdr3);

  with Project.Lists[WEIR] do
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      ExportComment(S, N.Data[COMMENT_INDEX]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-8s', [N.Data[NODE_INVERT_INDEX]]);
      Line := Line + Tab + Format('%-8s', [N.Data[WEIR_CREST_ELEV_INDEX]]);
      Line := Line + Tab + Format('%-16s', [N.Data[WEIR_RATING_CURVE_INDEX]]);
      S.Add(Line);
    end;
end;

procedure ExportStorage(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I    : Integer;
  Line : String;
  N    : TNode;
begin
  if Project.Lists[STORAGE].Count = 0 then exit;
  S.Add('');
  S.Add('[STORAGE]');

Hdr1 := ';;              ' + Tab + 'Invert    ' + Tab + 'Max.      ' + Tab + 'Init.     ' + Tab + 'Shape                   ' + Tab + 'Constant  ';
Hdr2 := ';;Name          ' + Tab + 'Elev.     ' + Tab + 'Depth     ' + Tab + 'Depth     ' + Tab + 'Curve                   ' + Tab + 'Outflow   ';
Hdr3 := ';;--------------' + Tab + '----------' + Tab + '----------' + Tab + '----------' + Tab + '------------------------' + Tab + '----------';
  S.Add(Hdr1);
  S.Add(Hdr2);
  S.Add(Hdr3);

  with Project.Lists[STORAGE] do
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      ExportComment(S, N.Data[COMMENT_INDEX]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-10s', [N.Data[NODE_INVERT_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[STORAGE_MAX_DEPTH_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[STORAGE_INIT_DEPTH_INDEX]]);
      Line := Line + Tab + Format('TABULAR %-16s', [N.Data[STORAGE_SHAPE_TABLE_INDEX]]);
      Line := Line + Tab + Format('%-10s', [N.Data[STORAGE_OUTFLOW_INDEX]]);
      S.Add(Line);
    end;
end;

procedure ExportConduits(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I     : Integer;
  Line  : String;
  L     : TLink;
begin
  if Project.Lists[CONDUIT].Count = 0 then exit;
  S.Add('');
  S.Add('[CONDUITS]');
  
Hdr1 := ';;              ' + Tab + 'Inlet           ' + Tab + 'Outlet          ' + Tab + '          ' + Tab + 'Manning   ' + Tab + 'Inlet     ' + Tab + 'Outlet    ' + Tab + 'Init.     ' + Tab + '--Initial Depth--';
Hdr2 := ';;Name          ' + Tab + 'Node            ' + Tab + 'Node            ' + Tab + 'Length    ' + Tab + 'N         ' + Tab + 'Offset    ' + Tab + 'Offset    ' + Tab + 'Flow      ' + Tab + 'Type     Value   ';
Hdr3 := ';;--------------' + Tab + '----------------' + Tab + '----------------' + Tab + '----------' + Tab + '----------' + Tab + '----------' + Tab + '----------' + Tab + '----------' + Tab + '-------- --------';
  S.Add(Hdr1);
  S.Add(Hdr2);
  S.Add(Hdr3);
  
  with Project.Lists[CONDUIT] do
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      ExportComment(S, L.Data[COMMENT_INDEX]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + Format('%-16s', [L.Node1.ID]);
      Line := Line + Tab + Format('%-16s', [L.Node2.ID]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_LENGTH_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_ROUGHNESS_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_INLET_HT_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_OUTLET_HT_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_INIT_FLOW_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_DEPTH_TYPE_INDEX]]);
      Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_DEPTH_VALUE_INDEX]]);
      S.Add(Line);
    end;
end;

procedure ExportPumps(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I     : Integer;
  Line  : String;
  L     : TLink;
  CtrlType  : String;
  CtrlSeries: String;
  CtrlNode  : String;
  CtrlCurve : String;
begin
  if Project.GetPumpCount = 0 then exit;
  S.Add('');
  S.Add('[PUMPS]');

  Hdr1 := ';;              ' + Tab + 'Pump            ' + Tab + 'Loss    ' + Tab;
  Hdr2 := ';;Conduit       ' + Tab + 'Curve           ' + Tab + 'Coeff.  ' + Tab;
  Hdr3 := ';;--------------' + Tab + '----------------' + Tab + '--------' + Tab;

  Hdr1 := Hdr1 + 'Friction' + Tab + 'Initial ' + Tab + 'Control         ' + Tab;
  Hdr2 := Hdr2 + 'Factor  ' + Tab + 'Setting ' + Tab + 'Time Series     ' + Tab;
  Hdr3 := Hdr3 + '--------' + Tab + '--------' + Tab + '----------------' + Tab;

  Hdr1 := Hdr1 + 'Control         ' + Tab + 'Control         ';
  Hdr2 := Hdr2 + 'Node            ' + Tab + 'Curve           ';
  Hdr3 := Hdr3 + '----------------' + Tab + '----------------';

  S.Add(Hdr1);
  S.Add(Hdr2);
  S.Add(Hdr3);

  with Project.Lists[CONDUIT] do
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      if L.HasPump = False then continue;
      Line := Format('%-16s', [Strings[I]]);
      if Length(L.Data[PUMP_CURVE_INDEX]) = 0 then
        Line := Line + Tab + '*               '
      else
        Line := Line + Tab + Format('%-16s', [L.Data[PUMP_CURVE_INDEX]]);
      Line := Line + Tab + Format('%-8s', [L.Data[PUMP_LOSS_COEFF_INDEX]]);
      Line := Line + Tab + Format('%-8s', [L.Data[PUMP_FRICTION_INDEX]]);
      Line := Line + Tab + Format('%-8s', [L.Data[PUMP_INIT_SETTING_INDEX]]);

      CtrlType := L.Data[PUMP_CONTROL_METHOD_INDEX];
      CtrlSeries := '*';
      CtrlNode := '';
      CtrlCurve := '';
      if SameText(CtrlType, ControlMethods[1]) then
      begin
        CtrlSeries := Trim(L.Data[PUMP_TIME_SERIES_INDEX]);
        if Length(CtrlSeries) = 0 then CtrlSeries := '*';
      end
      else if SameText(CtrlType, ControlMethods[2]) then
      begin
        CtrlNode := Trim(L.Data[PUMP_CONTROL_NODE_INDEX]);
        CtrlCurve := Trim(L.Data[PUMP_CONTROL_CURVE_INDEX]);
      end;
      Line := Line + Tab + Format('%-16s', [CtrlSeries]);
      Line := Line + Tab + Format('%-16s', [CtrlNode]);
      Line := Line + Tab + Format('%-16s', [CtrlCurve]);

      S.Add(Line);
    end;
end;

procedure ExportXsections(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I     : Integer;
  Line  : String;
  L     : TLink;
begin
  if Project.Lists[CONDUIT].Count = 0 then exit;
  S.Add('');
  S.Add('[XSECTIONS]');
  Line := ';;Link          ' + Tab + 'Shape       ' + Tab + 'Diameter        ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '------------' + Tab + '----------------';
  S.Add(Line);
  with Project.Lists[CONDUIT] do
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      Line := Format('%-16s', [Strings[I]]);
      Line := Line + Tab + 'CIRCULAR    ';
      Line := Line + Tab + Format('%-16s', [L.Data[CONDUIT_DIAMETER_INDEX]]);
      S.Add(Line);
    end;
end;

procedure ExportLosses(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I     : Integer;
  Line  : String;
  L     : TLink;
begin
  if Project.Lists[CONDUIT].Count = 0 then exit;
  S.Add('');
  S.Add('[LOSSES]');
  Line := ';;Link          ' + Tab + 'Inlet     ' + Tab + 'Outlet    ';
   S.Add(Line);
  Line := ';;--------------' + Tab + '----------' + Tab + '----------';
  S.Add(Line);
  with Project.Lists[CONDUIT] do
  for I := 0 to Count-1 do
  begin
    L := TLink(Objects[I]);
    if  SameText(L.Data[CONDUIT_ENTRY_LOSS_INDEX], '0')
    and SameText(L.Data[CONDUIT_EXIT_LOSS_INDEX], '0')
    then continue;
    Line := Format('%-16s', [Strings[I]]);
    Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_ENTRY_LOSS_INDEX]]);
    Line := Line + Tab + Format('%-10s', [L.Data[CONDUIT_EXIT_LOSS_INDEX]]);
    S.Add(Line);
  end;
end;

function GetInflowLine(aNode: TNode): String;
//-----------------------------------------------------------------------------
var
  TS : String;
  BL : String;
  SF : String;
begin
  with aNode.ExInflow do
  begin
    TS := Tseries;
    if Length(TS) = 0 then TS := '*';
    BL := Baseline;
    if Length(BL) = 0 then BL := '0';
    SF := ScaleFactor;
    if Length(SF) = 0  then SF := '1';
  end;
  Result := Format('%-16s', [aNode.ID]);
  Result := Result + Tab + 'FLOW      ';
  Result := Result + Tab + Format('%-16s',[TS]);
  Result := Result + Tab + 'FLOW    ' + Tab + '1.0     ';
  Result := Result + Tab + Format('%-8s', [SF]);
  Result := Result + Tab + Format('%-8s', [BL]);
end;

procedure ExportInflows(S: TStringlist);
//-----------------------------------------------------------------------------
var
  J   : Integer;
  aNode  : TNode;
begin
  if XflowCount = 0 then exit;
  S.Add('');
  S.Add('[INFLOWS]');
  
Hdr1 := ';;              ' + Tab + '          ' + Tab + '                ' + Tab + 'Param   ' + Tab + 'Units   ' + Tab + 'Scale   ' + Tab + 'Baseline';
Hdr2 := ';;Node          ' + Tab + 'Parameter ' + Tab + 'Time Series     ' + Tab + 'Type    ' + Tab + 'Factor  ' + Tab + 'Factor  ' + Tab + 'Value   ';
Hdr3 := ';;--------------' + Tab + ' ---------' + Tab + '----------------' + Tab + '--------' + Tab + '--------' + Tab + '--------' + Tab + '--------';
  S.Add(Hdr1);
  S.Add(Hdr2);
  S.Add(Hdr3);

  for J := 0 to Project.Lists[JUNCTION].Count - 1 do
  begin
    aNode := Project.GetNode(JUNCTION, J);
    if Length(aNode.ExInflow.FlowType) > 0 then
      S.Add(GetInflowLine(aNode));
  end;
  for J := 0 to Project.Lists[STORAGE].Count - 1 do
  begin
    aNode := Project.GetNode(STORAGE, J);
    if Length(aNode.ExInflow.FlowType) > 0 then
      S.Add(GetInflowLine(aNode));
  end;
end;

procedure ExportTimeseries(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J, N : Integer;
  Name    : String;
  Line    : String;
  Tseries : TTimeseries;
begin
  if Project.Lists[TIMESERIES].Count = 0 then exit;
  S.Add('');
  S.Add('[TIMESERIES]');
  Line := ';;Name          ' + Tab + 'Date      ' + Tab + 'Time      ' + Tab + 'Value     ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------' + Tab + '----------' + Tab + '----------';
  S.Add(Line);
  with Project.Lists[TIMESERIES] do
    for I := 0 to Count-1 do
    begin
      Name := Strings[I];
      Tseries := TTimeseries(Objects[I]);
      ExportComment(S, Tseries.Comment);
      N := MinIntValue([Tseries.Times.Count, Tseries.Values.Count]);
      for J := 0 to N-1 do
      begin
        Line := Format('%-16s', [Name]);
        Line := Line + Tab + Format('%-10s', [Tseries.Dates[J]]);
        Line := Line + Tab + Format('%-10s', [Tseries.Times[J]]);
        Line := Line + Tab + Format('%-10s', [Tseries.Values[J]]);
        S.Add(Line);
      end;
      if I < Count-1 then S.Add('');
    end;
end;

procedure ExportCurves(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I,J,K,N,M : Integer;
  Line    : String;
  Name    : String;
  CurveType: String;
  aCurve  : TCurve;
begin
  if Project.GetCurveCount = 0 then exit;
  M := 0;
  S.Add('');
  S.Add('[CURVES]');
  Line := ';;Name          ' + Tab + 'Type      ' + Tab + 'X-Value   ' + Tab + 'Y-Value   ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------' + Tab + '----------' + Tab + '----------';
  S.Add(Line);
  for I := 0 to MAXCLASS do
  begin
    if Project.IsCurve(I) then with Project.Lists[I] do
    begin
      for J := 0 to Count-1 do
      begin
        Name := Strings[J];
        aCurve := TCurve(Objects[J]);
        ExportComment(S, aCurve.Comment);
        N := MinIntValue([aCurve.Xdata.Count, aCurve.Ydata.Count]);
        if SameText(aCurve.CurveType, 'PUMP') then
          CurveType := 'PUMP3'
        else
          CurveType := aCurve.CurveType;
        if N > 0 then
        begin
          if M > 0 then S.Add('') else M := 1;
          Line := Format('%-16s', [Name]) + Tab +
                  Format('%-10s', [CurveType]) + Tab +
                  Format('%-10s', [aCurve.Xdata[0]]) + Tab +
                  Format('%-10s', [aCurve.Ydata[0]]);
          S.Add(Line);
          for K := 1 to N-1 do
          begin
            Line := Format('%-16s', [Name]) + Tab + '          ' + Tab +
                    Format('%-10s', [aCurve.Xdata[K]]) + Tab +
                    Format('%-10s', [aCurve.Ydata[K]]);
            S.Add(Line);
          end;
        end;
      end;
    end;
  end;
end;

procedure ExportFiles(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, N: Integer;
  Fname: String;
  Line: String;
  TokList : TStringlist;
  Ntoks   : Integer;
begin
  N := Project.IfaceFiles.Count;
  if N = 0 then Exit;
  S.Add('');
  S.Add('[FILES]');
  S.Add(';;Hot Start Files');
  TokList := TStringList.Create;
  try
    for I := 0 to N-1 do
    begin
      Uutils.Tokenize(Project.IfaceFiles[I], TokList, Ntoks);
      if Ntoks < 3 then continue;
      Fname := TokList[2];
      if SameText(SaveToPath, ExtractFilePath(Fname))
      then Fname := ExtractFileName(Fname);
      Line := TokList[0] + Tab + TokList[1] + Tab + '"' + Fname + '"';
      S.Add(Line);
    end;
  finally
    TokList.Free;
  end;
end;

procedure ExportProfiles(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, M, N   : Integer;
  aID       : String;
  Line      : String;
  LinksList : TStringlist;
begin
  if Project.ProfileNames.Count = 0 then exit;
  if Uglobals.TabDelimited then Tab := #9 else Tab := ' ';
  S.Add('');
  S.Add('[PROFILES]');
  Line := ';;Name          ' + Tab + 'Links     ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '----------';
  S.Add(Line);
  LinksList := TStringlist.Create;
  with Project do
  try
    for I := 0 to ProfileNames.Count-1 do
    begin
      aID := ProfileNames[I];
      LinksList.Clear;
      LinksList.SetText(PChar(ProfileLinks[I]));
      N := LinksList.Count;
      M := 0;
      Line := '';
      while M < N do
      begin
        if (M mod 5) = 0 then
        begin
          if M > 0 then S.Add(Line);
          Line := Format('"%-16s"', [aID]);
        end;
        Line := Line + Tab + LinksList[M];
        Inc(M);
      end;
      S.Add(Line);
    end;
  finally
    LinksList.Free;
  end;
end;

procedure ExportTags(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I: Integer;
  J: Integer;
  Line: String;
begin
  if Uglobals.TabDelimited then Tab := #9 else Tab := ' ';
  S.Add('');
  S.Add('[TAGS]');
  Hdr1 := ';;Object  ' + Tab + 'Name            ' + Tab + 'Tag             ';
  Hdr2 := ';;--------' + Tab + '----------------' + Tab + '----------------';  
  S.Add(Hdr1);
  S.Add(Hdr2);
  for I := JUNCTION to STORAGE do
  begin
    for J := 0 to Project.Lists[I].Count-1 do
    begin
      with Project.GetNode(I, J) do
        if Length(Data[TAG_INDEX]) > 0 then
        begin
          Line := 'Node      ' + Tab + Format('%-16s', [ID]) + Tab +
                  Format('%-16s', [Data[TAG_INDEX]]);
          S.Add(Line);
        end;
    end;
  end;
  I := CONDUIT;
  for J := 0 to Project.Lists[I].Count-1 do
  begin
      with Project.GetLink(I, J) do
        if Length(Data[TAG_INDEX]) > 0 then
        begin
          Line := 'Link      ' + Tab + Format('%-16s', [ID]) + Tab +
                  Format('%-16s', [Data[TAG_INDEX]]);
          S.Add(Line);
        end;
  end;
end;

procedure ExportMap(S: TStringlist);
//-----------------------------------------------------------------------------
var
  I, J, N, D: Integer;
  aID       : String;
  Line      : String;
  Fmt       : String;
  TrueFalse : Integer;                                                          
  Slist     : TStringlist;
  aNode     : TNode;
  aVertex   : PVertex;
  aMapLabel : TMapLabel;
begin
  // Export map's dimensions
  if Uglobals.TabDelimited then Tab := #9 else Tab := ' ';
  S.Add('');
  S.Add('[MAP]');
  with MapForm.Map.Dimensions do
  begin
    D := Digits;
    Fmt := '%-18.' + IntToStr(Digits) + 'f';
    Line := 'DIMENSIONS' + Tab +
            FloatToStrF(LowerLeft.X,ffFixed,18,D) + Tab +
            FloatToStrF(LowerLeft.Y,ffFixed,18,D) + Tab +
            FloatToStrF(UpperRight.X,ffFixed,18,D) + Tab +
            FloatToStrF(UpperRight.Y,ffFixed,18,D);
    S.Add(Line);
    Line := 'Units     ' + Tab + MapUnits[Ord(Units)];
    S.Add(Line);
  end;

  // Export nodal coordinates
  S.Add('');
  S.Add('[COORDINATES]');
  Line := ';;Node          ' + Tab + 'X-Coord           ' + Tab + 'Y-Coord           ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '------------------' + Tab + '------------------';
  S.Add(Line);
  for I := JUNCTION to STORAGE do
  begin
    Slist := Project.Lists[i];
    N := Slist.Count - 1;
    for J := 0 to N do
    begin
      aNode := Project.GetNode(I, J);
      with aNode do
        if (X <> MISSING) and (Y <> MISSING) then
        begin
          Line := Format('%-16s', [Slist[J]]) + Tab +
                  Format(Fmt, [X]) + Tab +
                  Format(Fmt, [Y]);
          S.Add(Line);
        end;
    end;
  end;
  S.Add('');

  // Export link vertex coordinates
  S.Add('[VERTICES]');
  Line := ';;Link          ' + Tab + 'X-Coord           ' + Tab + 'Y-Coord           ';
  S.Add(Line);
  Line := ';;--------------' + Tab + '------------------' + Tab + '------------------';
  S.Add(Line);
  I := CONDUIT;
  begin
    Slist := Project.Lists[I];
    for J := 0 to Slist.Count - 1 do
    begin
      aVertex := Project.GetLink(I, J).Vlist.First;
      while aVertex <> nil do
      begin
        Line := Format('%-16s', [Slist[J]]) + Tab +
                Format(Fmt, [aVertex^.X]) + Tab +
                Format(Fmt, [aVertex^.Y]);
        S.Add(Line);
        aVertex := aVertex^.Next;
      end;
    end;
  end;
  S.Add('');

  // Export map label coordinates
  Slist := Project.Lists[MAPLABEL];
  N := Slist.Count - 1;
  if N >= 0 then
  begin
    S.Add( '[LABELS]');
    Line := ';;X-Coord         ' + Tab + 'Y-Coord           ' + Tab + 'Label           ';
    S.Add(Line);
    for J := 0 to N do
    begin
      aMapLabel := Project.GetMapLabel(J);
      with aMapLabel do
      begin
        aID := '""';
        if Anchor <> nil then aID := Anchor.ID;
        Line := Format(Fmt, [X]) + Tab +
                Format(Fmt, [Y]) + Tab + '"' + Slist[J] + '"' +
                Tab + aID + Tab + '"' + FontName + '"' + Tab +
                Format('%d', [FontSize]);
        if FontBold then TrueFalse := 1 else TrueFalse := 0;
        Line := Line + Tab + IntToStr(TrueFalse);
        if FontItalic then TrueFalse := 1 else TrueFalse := 0;
        Line := Line + Tab + IntToStr(TrueFalse);
        S.Add(Line);
      end;
    end;
    S.Add('');
  end;

  // Export backdrop image information
  with MapForm.Map.Backdrop do
  begin
    if Length(Filename) > 0 then
    begin
      S.Add('');
      S.Add('[BACKDROP]');
      S.Add('FILE      ' + Tab + '"' + Filename + '"');
      Line := 'DIMENSIONS' + Tab +
              FloatToStrF(LowerLeft.X,ffFixed,18,D) + Tab +
              FloatToStrF(LowerLeft.Y,ffFixed,18,D) + Tab +
              FloatToStrF(UpperRight.X,ffFixed,18,D) + Tab +
              FloatToStrF(UpperRight.Y,ffFixed,18,D);
      S.Add(Line);
    end;
  end;

end;

procedure ExportTempDir(S: TStringlist);
//-----------------------------------------------------------------------------
begin
  if Uglobals.TabDelimited then Tab := #9 else Tab := ' ';
  S.Add('');
  S.Add('[OPTIONS]');
  S.Add('TEMPDIR   ' + Tab + '"' + TempDir + '"');
end;

procedure ExportProject(S: TStringlist);
//-----------------------------------------------------------------------------
begin
  if Uglobals.TabDelimited then Tab := #9 else Tab := ' ';
  XflowCount := 0;

  ExportTitle(S);
  ExportOptions(S);
  ExportFiles(S);
  //*****************************************************
  // These sections must be exported in the order as shown
  //******************************************************
  ExportJunctions(S);
  ExportBoundaries(S);
  ExportGates(S);
  ExportWeirs(S);
  ExportStorage(S);
  ExportConduits(S);
  ExportPumps(S);
  //******************************************************
  ExportXsections(S);
  ExportLosses(S);
  ExportInflows(S);
  ExportCurves(S);
  ExportTimeseries(S);
end;

function SaveOutput(Fname: String): Boolean;
//-----------------------------------------------------------------------------
var
  F1: String;
  F2: String;
  F3: String;
  F4: String;
//  F5: String;
begin
  // Make sure that report & output file don't have same name as input file
  Result := False;
  F1 := ChangeFileExt(Fname, '.rpt');
  F2 := ChangeFileExt(Fname, '.out');
  F3 := ChangeFileExt(Fname, '.itm');
  F4 := ChangeFileExt(Fname, '.dbg');
//  F5 := ChangeFileExt(Fname, '.err');
  if (SameText(F1, Fname)) or (SameText(F2, Fname)
  or (SameText(F3, Fname)) or (SameText(F4, Fname))
  //or (SameText(F5, Fname))
  ) then Exit;

  // Exit if current temporary files have same names as permanent ones
  if (SameText(F1, TempReportFile) and SameText(F2, TempOutputFile)
  and SameText(F3, ITMOutputFile)) and SameText(F4, TempDebugFile)
  //and SameText(F5, TempErrorFile)
  then
  begin
    Result := True;
    Exit;
  end;

  // Rename current temporary report file
  DeleteFile(PChar(F1));
  if not RenameFile(TempReportFile, F1) then Exit;

  // Close & rename current temporary output file
  Uoutput.CloseOutputFile;
  DeleteFile(PChar(F2));
  if not RenameFile(TempOutputFile, F2) then Exit;
  DeleteFile(PChar(F3));
  if not RenameFile(ITMOutputFile, F3) then Exit;
  DeleteFile(PChar(F4));
  if not RenameFile(TempDebugFile, F4) then Exit;
//  DeleteFile(PChar(F5));
//  if not RenameFile(TempErrorFile, F5) then Exit;

  // Reopen output files & clear temporary file names
  Uoutput.OpenOutputFile(F2, F3);
  TempReportFile := '';
  TempOutputFile := '';
  ITMOutputFile := '';
  TempDebugFile := '';
  TempErrorFile := '';
  Result := True;
end;

procedure SaveProject(Fname: String);
//-----------------------------------------------------------------------------
var
  S: TStringlist;
begin
  SaveToPath := ExtractFilePath(Fname);
  S := TStringlist.Create;
  try
    ExportProject(S);
    S.AddStrings(Project.Options.Report);
    ExportTags(S);
    ExportMap(S);
    ExportProfiles(S);
    S.SaveToFile(Fname);
  finally
    S.Free;
  end;
  SaveToPath := '';
  if CompareText(ExtractFileExt(Fname), '.ini') <> 0
  then Uinifile.SaveProjIniFile(ChangeFileExt(Fname, '.ini'));
end;

procedure SaveResults(Fname: String);
//-----------------------------------------------------------------------------
begin
  Uglobals.ResultsSaved := SaveOutput(Fname);
  if CompareText(ExtractFileExt(Fname), '.ini') <> 0
  then Uinifile.SaveProjIniFile(ChangeFileExt(Fname, '.ini'));
end;

end.

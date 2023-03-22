unit itm_export;

{-------------------------------------------------------------------}
{                    Unit:    itm_export.pas                        }
{                    Project: ITM                                   }
{                    Version: 2.0                                   }
{                    Date:    03/05/23                              }
{                                                                   }
{   Delphi Pascal unit that exports current project data to an      }
{   ITM formatted input file.                                       }
{-------------------------------------------------------------------}

interface

uses SysUtils, Windows, Messages, Classes, Math, Dialogs, StrUtils,
  Uglobals, Uutils, Uproject, Uvertex;

  function  ValidateProject : Boolean;
  procedure SaveToItmFile(Fname: String);

implementation

const
  Tab = '  ';

var
  NodeStartIndex: array[JUNCTION .. STORAGE] of Integer;
  CurveStartIndex: array[STORAGECURVE .. CONTROLCURVE] of Integer;
  NumCurves: Integer;

function GetNodeIndex(NodeName: String): Integer;
var
  J, K: Integer;
begin
  if not Project.FindNode(NodeName, J, K) then Result := -1
  else
    Result := NodeStartIndex[J] + K;
end;

function GetCurveIndex(CurveType: Integer; CurveName: String): Integer;
begin
  Result := Project.Lists[CurveType].IndexOf(CurveName) + CurveStartIndex[CurveType];
end;

procedure ExportCounts(S: TStringlist);
var
  NumNodes: Integer;
//  NumLinks: Integer;
  I: Integer;
begin
  NodeStartIndex[JUNCTION] := 0;
  NumNodes := Project.Lists[JUNCTION].Count;
  for I := JUNCTION+1 to STORAGE do
  begin
    NodeStartIndex[I] := NodeStartIndex[I-1] + Project.Lists[I-1].Count;
    NumNodes := NumNodes + Project.Lists[I].Count;
  end;
//  NumLinks := Project.Lists[CONDUIT].Count;
//  for I := PUMP to OUTLET do
//    NumLinks := NumLinks + Project.Lists[I].Count;
  CurveStartIndex[STORAGECURVE] := 0;
  NumCurves := Project.Lists[STORAGECURVE].Count;
  for I := STORAGECURVE+1 to CONTROLCURVE do
  begin
    CurveStartIndex[I] := CurveStartIndex[I-1] + Project.Lists[I-1].Count;
    NumCurves := NumCurves + Project.Lists[I].Count;
  end;

  S.Add(IntToStr(NumNodes));
  for I := CONDUIT to OUTLET do
    S.Add(IntToStr(Project.Lists[I].Count));
 // S.Add(IntToStr(NumLinks));
  S.Add(IntToStr(NumCurves));
  S.Add(IntToStr(Project.Lists[TIMESERIES].Count));
end;

procedure ExportJunctions(S: TStringlist);
var
  I: Integer;
  Line: String;
  N     : TNode;
begin
  with Project.Lists[JUNCTION] do
  begin
    S.Add(IntToStr(Count));
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      Line := Strings[I];
      Line := Line + Tab + N.Data[NODE_INVERT_INDEX];
      Line := Line + Tab + N.Data[JUNCTION_MAX_DEPTH_INDEX];
      Line := Line + Tab + N.Data[JUNCTION_INIT_DEPTH_INDEX];
      Line := Line + Tab + N.Data[JUNCTION_AREA_INDEX];
      if SameText(N.Data[JUNCTION_DROPSHAFT_INDEX], 'YES') then
        Line := Line + Tab + '0'
      else
        Line := Line + Tab + '1';
      S.Add(Line);
    end;
  end;
end;

procedure ExportBoundaries(S: TStringlist);
var
  I: Integer;
  Line: String;
  N     : TNode;
begin
  with Project.Lists[BOUNDARY] do
  begin
    S.Add(IntToStr(Count));
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      Line := Strings[I];
      Line := Line + Tab + N.Data[NODE_INVERT_INDEX];
      if SameText(N.Data[BOUNDARY_TYPE_INDEX], 'CONST/FLOW') then
        Line := Line + Tab + '10'
      else
        Line := Line + Tab + '11';
      Line := Line + Tab + N.Data[BOUNDARY_VALUE_INDEX];
      if SameText(N.Data[BOUNDARY_VENTILATED_INDEX], 'YES') then
        Line := Line + Tab + '0'
      else
        Line := Line + Tab + '1';
      S.Add(Line);
    end;
  end;
end;

procedure ExportStorage(S: TStringlist);
var
  I: Integer;
  J: Integer;
  N: TNode;
  Line: String;
begin
  with Project.Lists[STORAGE] do
  begin
    S.Add(IntToStr(Count));
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      Line := Strings[I];
      Line := Line + Tab + N.Data[NODE_INVERT_INDEX];
      Line := Line + Tab + N.Data[STORAGE_MAX_DEPTH_INDEX];
      Line := Line + Tab + N.Data[STORAGE_INIT_DEPTH_INDEX];
      J := GetCurveIndex(STORAGECURVE, N.Data[STORAGE_SHAPE_TABLE_INDEX]);
      Line := Line + Tab + IntToStr(J+1);
      Line := Line + Tab + N.Data[STORAGE_OUTFLOW_INDEX];
      S.Add(Line);
    end;
  end;
end;

procedure ExportConduits(S: TStringlist);
var
  I     : Integer;
  J     : Integer;
  K     : Integer;
  Line  : String;
  Dtype : String;
  L     : TLink;
begin
  with Project.Lists[CONDUIT] do
  begin
    S.Add(IntToStr(Count));
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      Line := Strings[I];
      K := GetNodeIndex(L.Node1.ID);
      Line := Line + Tab + IntToStr(K+1);
      K := GetNodeIndex(L.Node2.ID);
      Line := Line + Tab + IntToStr(K+1);
      Line := Line + Tab + L.Data[CONDUIT_DIAMETER_INDEX];
      Line := Line + Tab + L.Data[CONDUIT_LENGTH_INDEX];
      Line := Line + Tab + L.Data[CONDUIT_ROUGHNESS_INDEX];
      Line := Line + Tab + L.Data[CONDUIT_INLET_HT_INDEX];
      Line := Line + Tab + L.Data[CONDUIT_OUTLET_HT_INDEX];
      Line := Line + Tab + L.Data[CONDUIT_ENTRY_LOSS_INDEX];
      Line := Line + Tab + L.Data[CONDUIT_EXIT_LOSS_INDEX];
      Line := Line + Tab + L.Data[CONDUIT_INIT_FLOW_INDEX];
      Line := Line + Tab + L.Data[CONDUIT_DEPTH_VALUE_INDEX];
      Dtype := L.Data[CONDUIT_DEPTH_TYPE_INDEX];
      if SameText(Dtype, 'CONSTANT') then J := 1
      else if SameText(Dtype, 'CRITICAL') then J := 2
      else if SameText(Dtype, 'NORMAL') then J := 3
      else J := -1;
      Line := Line + Tab + IntToStr(J);
      S.Add(Line);
    end;
  end;
end;

function GetControlLine(L: TLink; CtrlIndex: Integer): String;
var
  J: Integer;
begin
  if SameText(L.Data[CtrlIndex], 'TIME') then
  begin
    Result := '1 ' + L.Data[CtrlIndex + CTRL_INIT_SETTING_INDEX] + ' ' +
      L.Data[CtrlIndex + CTRL_CLOSE_RATE_INDEX];
    J := Project.Lists[TIMESERIES].IndexOf(L.Data[CtrlIndex + CTRL_TIME_SERIES_INDEX]);
    Result := Result + ' ' + IntToStr(J+1) + ' 0 0';
  end

  else if SameText(L.Data[CtrlIndex], 'DEPTH') then
  begin
    Result := '2 ' + L.Data[CtrlIndex + CTRL_INIT_SETTING_INDEX] + ' ' +
      L.Data[CtrlIndex + CTRL_CLOSE_RATE_INDEX] + ' 0 ';
    J := GetNodeIndex(L.Data[CtrlIndex + CTRL_CONTROL_NODE_INDEX]);
    Result := Result + ' ' + IntToStr(J+1);
    J := GetCurveIndex(CONTROLCURVE, L.Data[CtrlIndex + CTRL_CONTROL_CURVE_INDEX]);
    Result := Result + ' ' + IntToStr(J+1);
  end

  else Result := '0 100 0 0 0 0';
end;

procedure ExportPumps(S: TStringlist);
var
  I   : Integer;
  K   : Integer;
  N   : Integer;
  L   : TLink;
  Line: String;
begin
  N := Project.Lists[PUMP].Count;
  S.Add(IntToStr(N));
  if N > 0 then with Project.Lists[PUMP] do
  begin
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      Line := Strings[I];
      K := GetNodeIndex(L.Node1.ID);
      Line := Line + Tab + IntToStr(K+1);
      K := GetNodeIndex(L.Node2.ID);
      Line := Line + Tab + IntToStr(K+1);
      K := GetCurveIndex(PUMPCURVE, L.Data[PUMP_CURVE_INDEX]);
      Line := Line + Tab + IntToStr(K+1);

      Line := Line + Tab + L.Data[PUMP_DIAM_INDEX];
      Line := Line + Tab + L.Data[PUMP_LENGTH_INDEX];
      Line := Line + Tab + L.Data[PUMP_FRICTION_INDEX];
      Line := Line + Tab + L.Data[PUMP_LOSS_COEFF_INDEX];
      S.Add(Line);
      S.Add(GetControlLine(L, PUMP_CONTROL_INDEX));
    end;
  end;
end;

procedure ExportOrifices(S: TStringlist);
var
  I   : Integer;
  K   : Integer;
  N   : Integer;
  L   : TLink;
  Line: String;
begin
  N := Project.Lists[ORIFICE].Count;
  S.Add(IntToStr(N));
  if N > 0 then with Project.Lists[ORIFICE] do
  begin
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      Line := Strings[I];
      K := GetNodeIndex(L.Node1.ID);
      Line := Line + Tab + IntToStr(K+1);
      K := GetNodeIndex(L.Node2.ID);
      Line := Line + Tab + IntToStr(K+1);
      K := 0;
      if SameText(L.Data[ORIFICE_TYPE_INDEX], 'BOTTOM') then K := 1;
      Line := Line + Tab + IntToStr(K);
      K := 0;
      if SameText(L.Data[ORIFICE_SHAPE_INDEX], 'RECTANGULAR') then K := 1;
      Line := Line + Tab + IntToStr(K);
      Line := Line + Tab + L.Data[ORIFICE_HEIGHT_INDEX];
      Line := Line + Tab + L.Data[ORIFICE_WIDTH_INDEX];
      Line := Line + Tab + L.Data[ORIFICE_BOTTOM_HT_INDEX];
      Line := Line + Tab + L.Data[ORIFICE_COEFF_INDEX];
      K := 0;
      if SameText(L.Data[ORIFICE_FLAPGATE_INDEX], 'YES') then K := 1;
      Line := Line + Tab + IntToStr(K);
      S.Add(Line);
      S.Add(GetControlLine(L, ORIFICE_CONTROL_INDEX));
    end;
  end;
end;

procedure ExportWeirs(S: TStringlist);
var
  I   : Integer;
  K   : Integer;
  N   : Integer;
  L   : TLink;
  Line: String;
begin
  N := Project.Lists[WEIR].Count;
  S.Add(IntToStr(N));
  if N > 0 then with Project.Lists[WEIR] do
  begin
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      Line := Strings[I];
      K := GetNodeIndex(L.Node1.ID);
      Line := Line + Tab + IntToStr(K+1);
      K := GetNodeIndex(L.Node2.ID);
      Line := Line + Tab + IntToStr(K+1);
      K := Uutils.FindKeyWord(L.Data[WEIR_TYPE_INDEX], WeirTypes, 3);
      if K < 0 then K := 0;
      Line := Line + Tab + IntToStr(K);
      Line := Line + Tab + L.Data[WEIR_HEIGHT_INDEX];
      Line := Line + Tab + L.Data[WEIR_WIDTH_INDEX];
//      Line := Line + Tab + L.Data[WEIR_SLOPE_INDEX];
      Line := Line + Tab + L.Data[WEIR_CREST_INDEX];
      Line := Line + Tab + L.Data[WEIR_COEFF_INDEX];
      K := 0;
//      if SameText(L.Data[WEIR_FLAPGATE_INDEX], 'YES') then K := 1;
//      Line := Line + Tab + IntToStr(K);
      Line := Line + Tab + L.Data[WEIR_CONTRACT_INDEX];
//      Line := Line + Tab + L.Data[WEIR_END_COEFF_INDEX];
      K := 0;
      if SameText(L.Data[WEIR_SURCHARGE_INDEX], 'YES') then K := 1;
      Line := Line + Tab + IntToStr(K);
      S.Add(Line);
      S.Add(GetControlLine(L, WEIR_CONTROL_INDEX));
    end;
  end;
end;

procedure ExportOutlets(S: TStringlist);
var
  I   : Integer;
  K   : Integer;
  N   : Integer;
  L   : TLink;
  Line: String;
begin
  N := Project.Lists[OUTLET].Count;
  S.Add(IntToStr(N));
  if N > 0 then with Project.Lists[OUTLET] do
  begin
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      Line := Strings[I];
      K := GetNodeIndex(L.Node1.ID);
      Line := Line + Tab + IntToStr(K+1);
      K := GetNodeIndex(L.Node2.ID);
      Line := Line + Tab + IntToStr(K+1);
      Line := Line + Tab + L.Data[OUTLET_OFFSET_INDEX];
      K := 0;
      if SameText(L.Data[OUTLET_FLAPGATE_INDEX], 'YES') then K := 1;
      Line := Line + Tab + IntToStr(K);
      K := GetCurveIndex(RATINGCURVE, L.Data[OUTLET_CURVE_INDEX]);
      Line := Line + Tab + IntToStr(K+1);
      S.Add(Line);
    end;
  end;
end;

procedure ExportInflows(S: TStringlist);
var
  I: Integer;
  N: TNode;
  TS: String;
  SF: String;
  BL: String;
  Line: String;
begin
  with Project.Lists[JUNCTION] do
  begin
    S.Add(IntToStr(Count));
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      TS := '0';
      SF := '0';
      BL := '0';
      if (Length(N.ExInflow.Tseries) > 0) then
        TS := IntToStr(Project.Lists[TIMESERIES].IndexOf(N.ExInflow.Tseries) + 1);
      if Length(N.ExInflow.ScaleFactor) > 0 then
        SF := N.ExInflow.ScaleFactor;
      if Length(N.ExInflow.Baseline) > 0 then
        BL := N.ExInflow.Baseline;
      Line := IntToStr(I+1) + Tab + TS + Tab + SF + Tab + BL;
      S.Add(Line);
    end;
  end;
end;

procedure ExportCurves(S: TStringlist);
var
  I, J, K, N, M: Integer;
  C: TCurve;
begin
  S.Add(IntToStr(NumCurves));
  if NumCurves = 0 then exit;

  // Loop through each type of curve
  for M := StorageCurve to ControlCurve do
  begin
    with Project.Lists[M] do
    begin
      // N = number of curves of this type
      N := Count;
      // Loop through each curve
      if N > 0 then for I := 0 to N-1 do
      begin
        // Write number of data points in curve
        C := TCurve(Objects[I]);
        K := MinIntValue([C.Xdata.Count, C.Ydata.Count]);
        S.Add(IntToStr(K));
        // Write each data point
        {if M = PUMPCURVE then
        begin
          for J := K-1 downto 0 do
            S.Add(C.Ydata[J] + Tab + C.Xdata[J]);
        end
        else}
        begin
          for J := 0 to K-1 do
            S.Add(C.Xdata[J] + Tab + C.Ydata[J]);
        end;
      end;
    end;
  end;
end;

function GetDateTime(S: String): Double;
var
  D: TDateTime;
begin
  if TryStrToDateTime(S, D, MyFormatSettings) then
    Result := D
  else
    Result := 0;
end;

function GetElapsedSeconds(aDate: String; aTime: String; UseDates: Boolean;
  FirstDate: TDateTime; var LastDate: TDateTime): Double;
var
  T: TDateTime;   // a time in decimal days
begin
  // Convert aTime to decimal days
  T := Uutils.StrHoursToTime(aTime);
  if T < 0 then
  begin
    Result := MISSING;
    exit;
  end;

  // If dates are being used
  if UseDates then
  begin
    // Update the time series' current date
    if Length(aDate) > 0 then
    begin
      if not TryStrToDate(aDate, LastDate, MyFormatSettings) then
      begin
        Result := MISSING;
        exit;
      end;
    end;
    // Find days since FirstDate
    T := LastDate + T - FirstDate;
  end;

  // Convert elapsed days to seconds
  Result := T * 86400;
end;

procedure ExportTimeseries(S: TStringlist);
var
  I: Integer;
  J: Integer;
  N: Integer;
  Tseries:  TTimeseries;
  ElapsedSeconds: Double;
  LastElapsedSeconds: Double;
  FirstDate: TDateTime;
  LastDate: TDateTime;
  UseDates: Boolean;
begin
  // Find project's starting Date/Time
  with Project.Options do
    FirstDate := GetDateTime(Data[START_DATE_INDEX] + ' ' + Data[START_TIME_INDEX]);

  with Project.Lists[TIMESERIES] do
  begin
    // Add number of time series to input string
    S.Add(IntToStr(Count));

    // Evaluate each time series
    for I := 0 to Count-1 do
    begin

      // Add number of entries in series to input string
      Tseries := TTimeseries(Objects[I]);
      N := MinIntValue([Tseries.Times.Count, Tseries.Values.Count]);
      S.Add(IntToStr(N));

      // See if time series uses dates or not
      UseDates := Length(Tseries.Dates[0]) > 0;
      LastDate := 0;
      LastElapsedSeconds := MISSING;

      // Loop through each series entry
      for J := 0 to N-1 do
      begin

       // Determine entry's elapsed seconds from project starting date/time
       ElapsedSeconds := GetElapsedSeconds(Tseries.Dates[J], Tseries.Times[J],
         UseDates, FirstDate, LastDate);

       // If elapsed seconds invalid then just use previous value
       if (ElapsedSeconds <= LastElapsedSeconds) then
         ElapsedSeconds := LastElapsedSeconds
       else
         LastElapsedSeconds := ElapsedSeconds;

       // Add elapsed time and series value to input string
       S.Add(Format('%.4f  %s', [ElapsedSeconds, Tseries.Values[J]]));
      end;
    end;
  end;
end;

procedure ExportOptions(S: TStringlist);
var
  StartDateTime: TDateTime;
  EndDateTime: TDateTime;
  ReportStart: TDateTime;
  Duration: TDateTime;
  ReportStep: Double;
  RefDepthFrac: Double;
  InitWaterElev: Double;
  HotstartFile: String;
begin
  with Project.Options do
  begin
    StartDateTime := GetDateTime(Data[START_DATE_INDEX] + ' ' + Data[START_TIME_INDEX]);
    ReportStart := (GetDateTime(Data[REPORT_START_DATE_INDEX] + ' ' +
                   Data[REPORT_START_TIME_INDEX]) - StartDateTime) * 86400.;
    EndDateTime := GetDateTime(Data[END_DATE_INDEX] + Data[END_TIME_INDEX]);
    Duration := (EndDateTime - StartDateTime) * 86400.;
    ReportStep := StrToFloat(Data[REPORT_STEP_INDEX]);
    RefDepthFrac := StrToFloat(Data[REF_DEPTH_FRACTION_INDEX]) / 100.;
    if Length(Data[INIT_WATER_ELEV_INDEX]) = 0 then
      InitWaterElev := -99999.5
    else
      InitWaterElev := StrToFloat(Data[INIT_WATER_ELEV_INDEX]);

    S.Add(Data[MAX_NUM_CELLS_INDEX]);
    S.Add(Data[PRESS_WAVE_CELERITY_INDEX]);
    S.Add(Format('%0.3f', [RefDepthFrac]));
    S.Add(Format('%.6f', [Duration]));
    S.Add(Data[MAX_TIME_STEP_INDEX]);
    S.Add(Format('%.6f', [ReportStep]));
    S.Add(Format('%.6f', [ReportStart]));
    S.Add(Data[MAX_NUM_PLOT_CELLS_INDEX]);
    S.Add(Format('%.2f', [InitWaterElev]));
    HotstartFile := Data[USE_HOTSTART_FILE_INDEX];
    if Length(Trim(HotstartFile)) = 0 then
      HotstartFile := '*';
    S.Add(HotstartFile);
    HotstartFile := Data[SAVE_HOTSTART_FILE_INDEX];
    if Length(Trim(HotstartFile)) = 0 then
      HotstartFile := '*';
    S.Add(HotstartFile);
  end;
end;

procedure SaveToItmFile(Fname: String);
//-----------------------------------------------------------------------------
var
  S: TStringlist;
  Title: String;
begin
  Title := ' ';
  if Project.Lists[NOTES].Count > 0  then
    Title := Project.Lists[NOTES].Strings[0];
  S := TStringlist.Create;
  try
    S.Add(Title);
    ExportCounts(S);
    ExportJunctions(S);
    ExportBoundaries(S);
    ExportStorage(S);
    ExportConduits(S);
    ExportPumps(S);
    ExportOrifices(S);
    ExportWeirs(S);
    ExportOutlets(S);
    ExportInflows(S);
    ExportCurves(S);
    ExportTimeseries(S);
    ExportOptions(S);
    S.SaveToFile(Fname);
  finally
    S.Free;
  end;
end;

procedure ValidateControls(LinkType: Integer; CtrlIndex: Integer; ErrList: TStringlist);
var
  I : Integer;
  L : TLink;
  S : String;
begin
  with Project.Lists[LinkType] do
  begin
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      if SameText(L.Data[CtrlIndex], 'TIME') then
      begin
        S := L.Data[CtrlIndex + CTRL_TIME_SERIES_INDEX];
        if Project.Lists[TIMESERIES].IndexOf(S) < 0 then
          ErrList.Add('- Control Time Series ' + S +
            ' used by Link ' + L.ID + ' does not exist.');
      end
      else if SameText(L.Data[CtrlIndex], 'DEPTH') then
      begin
        S := L.Data[CtrlIndex + CTRL_CONTROL_NODE_INDEX];
        if GetNodeIndex(S) < 0 then
          ErrList.Add('- Control Node ' + S +
            ' used by Link ' + L.ID + ' does not exist.');
        S := L.Data[CtrlIndex + CTRL_CONTROL_CURVE_INDEX];
        if GetCurveIndex(CONTROLCURVE, S) < 0 then
          ErrList.Add('- Control Curve ' + S +
            ' used by Link ' + L.ID + ' does not exist.');
      end;
    end;
  end;
end;

function ValidateProject : Boolean;
var
  I : Integer;
  N : TNode;
  L : TLink;
  ErrList : TStringList;
  StartDateTime: TDateTime;
  EndDateTime: TDateTime;
  ReportStart: TDateTime;
begin
  Result := True;
  ErrList := TStringList.Create;
  try

    // Validate Storage nodes
    with Project.Lists[STORAGE] do
    begin
      for I := 0 to Count-1 do
      begin
        N := TNode(Objects[I]);
        if GetCurveIndex(STORAGECURVE, N.Data[STORAGE_SHAPE_TABLE_INDEX]) < 0 then
          ErrList.Add('- Storage Curve ' + N.Data[STORAGE_SHAPE_TABLE_INDEX] +
              ' used by Storage node ' + N.ID + ' does not exist.');
      end;
    end;

    // Validate Pump links
    with Project.Lists[PUMP] do
    begin
      for I := 0 to Count-1 do
      begin
        L := TLink(Objects[I]);
        if GetCurveIndex(PUMPCURVE, L.Data[PUMP_CURVE_INDEX]) < 0 then
          ErrList.Add('- Pump Curve ' + L.Data[PUMP_CURVE_INDEX] +
              ' used by Pump link ' + L.ID + ' does not exist.');
      end;
    end;

    // Validate Outlet links
    with Project.Lists[OUTLET] do
    begin
      for I := 0 to Count-1 do
      begin
        L := TLink(Objects[I]);
        if GetCurveIndex(RATINGCURVE, L.Data[OUTLET_CURVE_INDEX]) < 0 then
          ErrList.Add('- Rating Curve ' + L.Data[OUTLET_CURVE_INDEX] +
              ' used by Outlet link ' + L.ID + ' does not exist.');
      end;
    end;

    // Validate link controls
    ValidateControls(PUMP, PUMP_CONTROL_INDEX, ErrList);
    ValidateControls(ORIFICE, ORIFICE_CONTROL_INDEX, ErrList);
    ValidateControls(WEIR, WEIR_CONTROL_INDEX, ErrList);

    // Validate inflow time series
    with Project.Lists[JUNCTION] do
    begin
      for I := 0 to Count-1 do
      begin
        N := TNode(Objects[I]);
        if (Length(N.ExInflow.Tseries) > 0) and
          (Project.Lists[TIMESERIES].IndexOf(N.ExInflow.Tseries) < 0) then
            ErrList.Add('- Inflow Time Series ' + N.ExInflow.Tseries +
              ' used by Junction ' + N.ID + ' does not exist.');
      end;
    end;

    // Validate date options
    with Project.Options do
    begin
      StartDateTime := GetDateTime(Data[START_DATE_INDEX] + ' ' + Data[START_TIME_INDEX]);
      ReportStart := GetDateTime(Data[REPORT_START_DATE_INDEX] + ' ' +
                     Data[REPORT_START_TIME_INDEX]);
      EndDateTime := GetDateTime(Data[END_DATE_INDEX] + ' ' + Data[END_TIME_INDEX]);
      if EndDateTime <= StartDateTime then
        ErrList.Add('- Simulation starting date occurs after ending date.');
      if ReportStart < StartdateTime then
        ErrList.Add('- Report starting date occurs before simulation start date.');
    end;

    // Save error list to temporary report file
    if ErrList.Count > 0 then
    begin
      Result := False;
      SysUtils.DeleteFile((TempReportFile));
      TempReportFile := Uutils.GetTempFile(TempDir,'itm');
      ErrList.Insert(0, 'Run canceled due to following errors:');
      ErrList.Insert(1, '');
      ErrList.SaveToFile(TempReportFile);
    end;

  finally
    ErrList.Free;
  end;
end;

end.

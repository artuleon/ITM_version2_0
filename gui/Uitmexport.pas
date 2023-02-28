unit Uitmexport;

{-------------------------------------------------------------------}
{                    Unit:    Uitmexport.pas                        }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    12/01/22                              }
{                                                                   }
{   Delphi Pascal unit that exports current project data to an      }
{   ITM formatted input file.                                       }
{-------------------------------------------------------------------}

interface

uses SysUtils, Windows, Messages, Classes, Math, Dialogs, StrUtils,
  Uglobals, Uutils, Uproject, Uvertex;

  procedure ExportProject(Fname: String);

implementation

const
  Tab = '  ';

var
  CurveStartIndex: array[GATECURVE .. CONTROLCURVE] of Integer;

function GetCurveIndex(CurveType: Integer; CurveName: String): Integer;
begin
  Result := Project.Lists[CurveType].IndexOf(CurveName) + CurveStartIndex[CurveType];
end;

procedure ExportCounts(S: TStringlist);
var
  NumNodes: Integer;
  NumLinks: Integer;
  I: Integer;
begin
  NumNodes := 0;
  for I := JUNCTION to STORAGE do
    NumNodes := NumNodes + Project.Lists[I].Count;
  NumLinks := Project.Lists[CONDUIT].Count + Project.Lists[PUMP].Count;
  S.Add(IntToStr(NumNodes));
  S.Add(IntToStr(NumLinks));
  for I := JUNCTION to PUMP do
    S.Add(IntToStr(Project.Lists[I].Count));
  CurveStartIndex[GATECURVE] := 0;
  for I := GATECURVE+1 to CONTROLCURVE do
    CurveStartIndex[I] := CurveStartIndex[I-1] + Project.Lists[I].Count;
end;

procedure ExportJunctions(S: TStringlist);
var
  I: Integer;
  Line: String;
  N     : TNode;
begin
  with Project.Lists[JUNCTION] do
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

procedure ExportBoundaries(S: TStringlist);
var
  I: Integer;
  Line: String;
  N     : TNode;
begin
  with Project.Lists[BOUNDARY] do
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

procedure ExportGates(S: TStringlist);
var
  I: Integer;
  J: Integer;
  N: TNode;
  Line: String;
  CtrlType  : String;
  CtrlSeries: String;
  CtrlNode  : String;
  CtrlCurve : String;
begin
  with Project.Lists[GATE] do
  begin
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      Line := Strings[I];
      Line := Line + Tab + N.Data[NODE_INVERT_INDEX];
      Line := Line + Tab + N.Data[GATE_INIT_OPENING_INDEX];
      Line := Line + Tab + N.Data[GATE_OPEN_RATE_INDEX];

      J := GetCurveIndex(GATECURVE, N.Data[GATE_HLOSS_CURVE_INDEX]);
      Line := Line + Tab + IntToStr(J+1);

      CtrlType := N.Data[GATE_CONTROL_METHOD_INDEX];
      CtrlSeries := '0';
      CtrlNode := '0';
      CtrlCurve := '0';

      if SameText(CtrlType, ControlMethods[1]) then
      begin
        J := Project.Lists[GATECURVE].IndexOf(N.Data[GATE_TIME_SERIES_INDEX]);
        CtrlSeries := IntToStr(J+1);
      end
      else if SameText(CtrlType, ControlMethods[2]) then
      begin
        J := Project.Lists[TIMESERIES].IndexOf(N.Data[GATE_CONTROL_NODE_INDEX]);
        CtrlNode := IntToStr(J+1);
        J := GetCurveIndex(CONTROLCURVE, N.Data[GATE_CONTROL_CURVE_INDEX]);
        CtrlCurve := IntToStr(J+1);
      end;

      Line := Line + Tab + CtrlSeries + Tab + CtrlNode + Tab + CtrlCurve;
      S.Add(Line);
    end;
  end;
end;

procedure ExportWeirs(S: TStringlist);
var
  I: Integer;
  J: Integer;
  N: TNode;
  Line: String;
begin
  with Project.Lists[WEIR] do
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      Line := Strings[I];
      Line := Line + Tab + N.Data[NODE_INVERT_INDEX];
      J := GetCurveIndex(RATINGCURVE, N.Data[WEIR_RATING_CURVE_INDEX]);
      Line := Line + Tab + IntToStr(J+1);
      S.Add(Line);
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
    for I := 0 to Count-1 do
    begin
      L := TLink(Objects[I]);
      Line := Strings[I];
      if not Project.FindNode(L.Node1.ID, J, K) then K := -1;
      Line := Line + Tab + IntToStr(K+1);
      if not Project.FindNode(L.Node2.ID, J, K) then K := -1;
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

procedure ExportPumps(S: TStringlist);
begin
//
end;

procedure ExportInflows(S: TStringlist);
var
  I: Integer;
  J: Integer;
  N: TNode;
  Line: String;
begin
  with Project.Lists[JUNCTION] do
    for I := 0 to Count-1 do
    begin
      N := TNode(Objects[I]);
      if (Length(N.ExInflow.Tseries) > 0) or (Length(N.ExInflow.Baseline) > 0) then
      begin
        J := Project.Lists[TIMESERIES].IndexOf(N.ExInflow.Tseries);
        Line := IntToStr(I+1) + Tab + IntToStr(J+1) + Tab + N.ExInflow.ScaleFactor + Tab;
        if Length(N.ExInflow.Baseline) = 0 then
          Line := Line + '0'
        else
          Line := Line + N.ExInflow.Baseline;
      end;
    end;
end;

procedure ExportCurves(S: TStringlist);
var
  I, J, K, N, M: Integer;
  C: TCurve;
begin
  // Loop through each type of curve
  for M := GateCurve to ControlCurve do
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
        for J := 0 to K-1 do
          S.Add(C.Xdata[J] + Tab + C.Ydata[J]);
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

function GetElapsedSeconds(aDate: String; aTime: String; FirstDate: TDateTime;
  var LastDate: TDateTime): Double;
var
  T: TDateTime;   // a time in decimal days
begin
  // Convert the time series Time entry to a DateTime value
  T := Uutils.StrHoursToTime(aTime);
  if T < 0 then
  begin
    Result := -1;
    exit;
  end;

  // If dates are being used
  if StartDateTime > 0 then
  begin
    // Update the time series' current date
    if Length(aDate) > 0 then
    begin
      if not TryStrToDate(aDate, LastDate, MyFormatSettings) then
      begin
        Result := -1;
        exit;
      end;
    end;
    // Find days since StartDateTime
    T := LastDate + T - StartDateTime;
  end;

  // Convert elapsed time to seconds
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
  StartDateTime: TDateTime;
  FirstDate: TDateTime;
  LastDate: TDateTime;
begin
  with Project.Options do
    StartDateTime := GetDateTime(Data[START_DATE_INDEX] + ' ' + Data[START_TIME_INDEX]);
  with Project.Lists[TIMESERIES] do
    for I := 0 to Count-1 do
    begin
      Tseries := TTimeseries(Objects[I]);
      N := MinIntValue([Tseries.Times.Count, Tseries.Values.Count]);
      S.Add(IntToStr(N));
      if (StartDateTime > 0) and (Length(Tseries.Dates[0]) > 0) then
        FirstDate := StartDateTime
      else
        FirstDate := 0;
      LastElapsedSeconds := 0;
      for J := 0 to N-1 do
      begin
       ElapsedSeconds := GetElapsedSeconds(Tseries.Dates[J], Tseries.Times[J],
         FirstDate, LastDate);
       if ElapsedSeconds > 0 then
         LastElapsedSeconds := ElapsedSeconds
       else
         ElapsedSeconds := LastElapsedSeconds;
       S.Add(Format('%.4f  %s', [ElapsedSeconds, Tseries.Values[J]]));
      end;
    end;
end;

procedure ExportOptions(S: TStringlist);
var
  StartDateTime: TDateTime;
  EndDateTime: TDateTime;
  Duration: TDateTime;
begin
  with Project.Options do
  begin
    StartDateTime := GetDateTime(Data[START_DATE_INDEX] + ' ' + Data[START_TIME_INDEX]);
    EndDateTime := GetDateTime(Data[END_DATE_INDEX] + Data[END_TIME_INDEX]);
    Duration := (EndDateTime - StartDateTime) * 24;
    S.Add(Format('%.6f', [StartDateTime]));
    S.Add(Format('%.6f', [Duration]));
    S.Add(Data[REPORT_STEP_INDEX]);
    S.Add(Data[FRAC_REPORT_STEP_INDEX]);
    S.Add(Data[MAX_TIME_STEP_INDEX]);
    S.Add(Data[MAX_NUM_CELLS_INDEX]);
    S.Add(Data[MAX_NUM_PLOT_CELLS_INDEX]);
    S.Add(Data[REF_DEPTH_FRACTION_INDEX]);
    S.Add(Data[PRESS_WAVE_CELERITY_INDEX]);
    S.Add(Data[INIT_WATER_ELEV_INDEX]);
  end;
end;

procedure ExportProject(Fname: String);
//-----------------------------------------------------------------------------
var
  S: TStringlist;
begin
  S := TStringlist.Create;
  try
    ExportCounts(S);
    ExportJunctions(S);
    ExportBoundaries(S);
    ExportGates(S);
    ExportWeirs(S);
    ExportStorage(S);
    ExportConduits(S);
    ExportPumps(S);
    ExportInflows(S);
    ExportCurves(S);
    ExportTimeseries(S);
    ExportOptions(S);
    S.SaveToFile(Fname);
  finally
    S.Free;
  end;
end;

end.

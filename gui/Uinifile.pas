unit Uinifile;

{-------------------------------------------------------------------}
{                    Unit:    Uinifile.pas                          }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/23/22                              }
{                                                                   }
{   Delphi Pascal unit that reads and writes initialization data    }
{   to the ITM INI file (ITM.ini) and to the current project's      }
{   INI file.                                                       }
{-------------------------------------------------------------------}

interface

uses
  Dialogs, Classes, SysUtils, Forms, Controls, Windows, IniFiles,
  Graphics, PgSetup, Uglobals, Uproject, Uutils;

procedure ReadStyleName;
procedure ReadIniFile;
procedure SaveIniFile;
procedure ReadDefaults;
procedure SaveDefaults;
procedure ReadMainFormSize;
procedure SaveMainFormSize;
procedure ReadProjIniFile(const Fname: String);
procedure SaveProjIniFile(const Fname: String);

implementation

uses
  Fmain, Fmap, Fproped, Uupdate;

procedure ExtractValues(S: String; const N1: Integer; const N2: Integer;
                        var X: array of Single); forward;

procedure InitMapLegends;
//-----------------------------------------------------------------------------
//  Assigns factory defaults to map legends.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
begin
  // Assign default values to legend colors
  for I := 0 to MAXINTERVALS do
  begin
    MapNodeColor[I] := DefLegendColor[I];
    MapLinkColor[I] := DefLegendColor[I];
  end;

  // Assign defaults to legend intervals for node variables
  for I := 0 to NODEVIEWS do
  begin
    with NodeLegend[I] do
    begin
      Nintervals := MAXINTERVALS;
      ViewVar := I;
      LType := NODES;
      for J := 1 to Nintervals do
        Intervals[J] := NodeVariable[I].DefIntervals[J];
    end;
  end;

  // Assign defaults to legend intervals for link variables
  for I := 0 to LINKVIEWS do
  begin
    with LinkLegend[I] do
    begin
      Nintervals := MAXINTERVALS;
      ViewVar := I;
      LType := LINKS;
      for J := 1 to Nintervals do
        Intervals[J] := LinkVariable[I].DefIntervals[J];
    end;
  end;
end;


procedure CheckGraphSeriesOptions(const I: Integer);
//-----------------------------------------------------------------------------
//  Checks for valid graph options for data series I
//-----------------------------------------------------------------------------
begin
  with GraphOptions do
  begin
    if (not LineVisible[I]) and (not PointVisible[I])
    then LineVisible[I] := True;
    if (LineStyle[I] < 0) or (LineStyle[I] > 4)
    then LineStyle[I] := DefGraphOptions.LineStyle[I];
    if LineWidth[I] < 1 then
    LineWidth[I] := DefGraphOptions.LineWidth[I];
    if (PointStyle[I] < 0) or (PointStyle[I] > 8)
    then PointStyle[I] := DefGraphOptions.PointStyle[I];
    if PointSize[I] < 1
    then PointSize[I] := DefGraphOptions.PointSize[I];
  end;
end;


procedure ReadStyleName;
begin
  // Initialize UI style
  Uglobals.StyleName := 'Windows';

  // Create the .INI file object
  with TIniFile.Create(IniFileDir + INIFILE) do
  try
    Uglobals.StyleName := ReadString('Preferences', 'StyleName', 'Windows');

  // Free the .INI file object
  finally
    Free;
  end;
end;


procedure ReadIniFile;
//-----------------------------------------------------------------------------
//  Reads map settings, program preferences, current directories,
//  and most-recently-used file list from the ITM.ini file.
//-----------------------------------------------------------------------------
var
  I : Integer;
  S : String;
begin
  // Initialize graph options with factory settings
  GraphOptions := DefGraphOptions;
  ProfileOptions := DefProfileOptions;

  // Create the .INI file object
  with TIniFile.Create(IniFileDir + INIFILE) do
  try

    // Retrieve Graph Options
    with GraphOptions do
    begin
      View3D := ReadBool('Graph', 'View3D', View3D);
      Percent3D := ReadInteger('Graph', 'Percent3D', Percent3D);
      PanelColor := ReadInteger('Graph', 'PanelColor', PanelColor);
      BackColor := ReadInteger('Graph', 'BackColor', BackColor);
      BackGradColor := ReadInteger('Graph', 'BackGradColor', BackGradColor);
      LegendPosition := ReadInteger('Graph', 'LegendPosition', LegendPosition);
      LegendColor := ReadInteger('Graph', 'LegendColor', LegendColor);
      LegendWidth := ReadInteger('Graph', 'LegendWidth', LegendWidth);
      LegendFramed := ReadBool('Graph', 'LegendFramed', LegendFramed);
      LegendTransparent := ReadBool('Graph', 'LegendTransparent', LegendTransparent);
      LegendVisible := ReadBool('Graph', 'LegendVisible', LegendVisible);
      AxisGridStyle[0] := ReadInteger('Graph', 'X-AxisGrid', AxisGridStyle[0]);
      AxisGridStyle[1] := ReadInteger('Graph', 'Y-AxisGrid', AxisGridStyle[1]);
      for I := 0 to MAXSERIES do
      begin
        S := IntToStr(I);
        LineVisible[I] := ReadBool('Graph', 'LineVisible' + S, LineVisible[I]);
        LineStyle[I] := ReadInteger('Graph', 'LineStyle' + S, LineStyle[I]);
        LineColor[I] := ReadInteger('Graph', 'LineColor' + S, LineColor[I]);
        LineWidth[I] := ReadInteger('Graph', 'LineWidth' + S, LineWidth[I]);
        PointVisible[I] := ReadBool('Graph', 'PointVisible' + S, PointVisible[I]);
        PointStyle[I] := ReadInteger('Graph', 'PointStyle' + S, PointStyle[I]);
        PointColor[I] := ReadInteger('Graph', 'PointColor' + S, PointColor[I]);
        PointSize[I] := ReadInteger('Graph', 'PointSize' + S, PointSize[I]);
        CheckGraphSeriesOptions(I);
      end;
      TitleFontColor := ReadInteger('Graph', 'TitleFontColor', TitleFontColor);
      TitleFontName := ReadString('Graph', 'TitleFontName', TitleFontName);
      TitleFontSize := ReadInteger('Graph', 'TitleFontSize', TitleFontSize);
      TitleFontBold := ReadBool('Graph', 'TitleFontBold', TitleFontBold);
      TitleFontItalic := ReadBool('Graph', 'TitleFontItalic', TitleFontItalic);
      AxisFontName := ReadString('Graph', 'AxisFontName', AxisFontName);
      AxisFontSize := ReadInteger('Graph', 'AxisFontSize', AxisFontSize);
      AxisFontBold := ReadBool('Graph', 'AxisFontBold', AxisFontBold);
      AxisFontItalic := ReadBool('Graph', 'AxisFontItalic', AxisFontItalic);
      AreaFillColor := ReadInteger('Graph', 'AreaFillColor', AreaFillColor);
      AreaFillStyle := TBrushStyle(ReadInteger('Graph',
                          'AreaFillStyle', Ord(AreaFillStyle)));
      LabelsVisible := ReadBool('Graph', 'LabelsVisible', LabelsVisible);
      LabelsTransparent := ReadBool('Graph', 'LabelsTransparent', LabelsTransparent);
      LabelsArrows := ReadBool('Graph', 'LabelsArrows', LabelsArrows);
      LabelsBackColor := ReadInteger('Graph', 'LabelsBackColor', LabelsBackColor);
    end;

    // Retrieve Profile Plot options
    with ProfileOptions do
    begin
      ConduitColor := ReadInteger('ProfilePlot', 'ConduitColor', ConduitColor);
      WaterColor := ReadInteger('ProfilePlot', 'WaterColor', WaterColor);
      LabelsOnAxis := ReadBool('ProfilePlot', 'LabelsOnAxis', LabelsOnAxis);
      LabelsOnPlot := ReadBool('ProfilePlot', 'LabelsOnPlot', LabelsOnPlot);
      LabelsArrowLength := ReadInteger('ProfilePlot', 'LabelsArrowLength',
                           LabelsArrowLength);
      LineWidth := ReadInteger('ProfilePlot', 'LineWidth', LineWidth);
    end;

    // Retrieve directory names
    S := ReadString('Directories', 'DataDir', ProjectDir);
    if (DirectoryExists(S)) then
    begin
      ProjectDir := S;
      SetCurrentDir(S);
    end;
{
    S := ReadString('Directories', 'TempDir', TempDir);
    if S[Length(S)] <> '\' then S := S + '\';
    if (DirectoryExists(S)) then TempDir := S;
}
    // Retrieve general preferences
    Blinking := ReadBool('Preferences', 'Blinking', True);
    FlyOvers := ReadBool('Preferences', 'FlyOvers', True);
    AutoBackup := ReadBool('Preferences', 'AutoBackup', False);
    ConfirmDelete := ReadBool('Preferences', 'ConfirmDelete', True);
    AutoSave := ReadBool('Preferences', 'AutoSave', False);
    RptElapsedTime := ReadBool('Preferences', 'RptElapsedTime', True);
    TabDelimited := ReadBool('Preferences', 'TabDelimited', False);

    // Retrieve MRU file names
    MainForm.MRUList.Clear;
    for I := 0 to Uglobals.MAXMRUINDEX do
      MainForm.MRUList.Add(ReadString('MRU', IntToStr(I), ''));

    // Retrieve placement parameters for the Property Editor form
{    with PropEditForm do
    begin
      Top := (Screen.Height - Height) div 2;
      Left := (Screen.Width - Width) div 2;
      Left := ReadInteger('Property Editor', 'Left', Left);
      Top  := ReadInteger('Property Editor', 'Top', Top);
      Width := ReadInteger('Property Editor', 'Width', Width);
      Height := ReadInteger('Property Editor', 'Height', Height);
      if Width >= Screen.Width then Width := Screen.Width div 2;
      if Height >= Screen.Height then Height := Screen.Height;
      if Left + Width > Screen.Width
      then Left := Screen.Width - Width;
      if Top + Height > Screen.Height
      then Top := Screen.Height - Height;
      Editor.HeaderSplit := ReadInteger('Property Editor', 'HeaderSplit',
                            Editor.HeaderSplit);
    end; }

    // Retrieve output variable display precision
    for I := NODEOUTVAR1 to NODEVIEWS do
      NodeUnits[I].Digits := ReadInteger('Display Precision',
                                         NodeVariable[I].Name, 2);
    for I := LINKOUTVAR1 to LINKVIEWS do
      LinkUnits[I].Digits := ReadInteger('Display Precision',
                                         LinkVariable[I].Name, 2);

  // Free the .INI file object
  finally
    Free;
  end;
end;


procedure SaveIniFile;
//-----------------------------------------------------------------------------
// Saves map settings, program preferences, current directories,
// and most-recently-used file list to the ITM.ini file.
//-----------------------------------------------------------------------------
var
  I : Integer;
  S : String;
begin
  // Create the .INI file object
  with TIniFile.Create(IniFileDir + INIFILE) do
  try

    // Save Graph options
    with GraphOptions do
    begin
      WriteBool('Graph', 'View3D', View3D);
      WriteInteger('Graph', 'Percent3D', Percent3D);
      WriteInteger('Graph', 'PanelColor', PanelColor);
      WriteInteger('Graph', 'BackColor', BackColor);
      WriteInteger('Graph', 'BackGradColor', BackGradColor);
      WriteInteger('Graph', 'LegendPosition', LegendPosition);
      WriteInteger('Graph', 'LegendColor', LegendColor);
      WriteInteger('Graph', 'LegendWidth', LegendWidth);
      WriteBool('Graph', 'LegendFramed', LegendFramed);
      WriteBool('Graph', 'LegendTransparent', LegendTransparent);
      WriteBool('Graph', 'LegendVisible', LegendVisible);
      WriteInteger('Graph', 'X-AxisGrid', AxisGridStyle[0]);
      WriteInteger('Graph', 'Y-AxisGrid', AxisGridStyle[1]);
      for I := 0 to MAXSERIES do
      begin
        S := IntToStr(I);
        WriteBool('Graph', 'LineVisible' + S, LineVisible[I]);
        WriteInteger('Graph', 'LineStyle' + S, LineStyle[I]);
        WriteInteger('Graph', 'LineColor' + S, LineColor[I]);
        WriteInteger('Graph', 'LineWidth' + S, LineWidth[I]);
        WriteBool('Graph', 'PointVisible' + S, PointVisible[I]);
        WriteInteger('Graph', 'PointStyle' + S, PointStyle[I]);
        WriteInteger('Graph', 'PointColor' + S, PointColor[I]);
        WriteInteger('Graph', 'PointSize' + S, PointSize[I]);
      end;
      WriteInteger('Graph', 'TitleFontColor',TitleFontColor);
      WriteString('Graph', 'TitleFontName', TitleFontName);
      WriteInteger('Graph', 'TitleFontSize', TitleFontSize);
      WriteBool('Graph', 'TitleFontBold', TitleFontBold);
      WriteBool('Graph', 'TitleFontItalic', TitleFontItalic);
      WriteString('Graph', 'AxisFontName', AxisFontName);
      WriteInteger('Graph', 'AxisFontSize', AxisFontSize);
      WriteBool('Graph', 'AxisFontBold', AxisFontBold);
      WriteBool('Graph', 'AxisFontItalic', AxisFontItalic);
      WriteInteger('Graph', 'AreaFillColor', AreaFillColor);
      WriteInteger('Graph', 'AreaFillStyle', Ord(AreaFillStyle));
      WriteBool('Graph', 'LabelsVisible', LabelsVisible);
      WriteBool('Graph', 'LabelsTransparent', LabelsTransparent);
      WriteBool('Graph', 'LabelsArrows', LabelsArrows);
      WriteInteger('Graph', 'LabelsBackColor', LabelsBackColor);
    end;

    // Save Profile Plot options
    with ProfileOptions do
    begin
      WriteInteger('ProfilePlot', 'ConduitColor', ConduitColor);
      WriteInteger('ProfilePlot', 'WaterColor', WaterColor);
      WriteBool('ProfilePlot', 'LabelsOnAxis', LabelsOnAxis);
      WriteBool('ProfilePlot', 'LabelsOnPlot', LabelsOnPlot);
      WriteInteger('ProfilePlot', 'LabelsArrowLength', LabelsArrowLength);
      WriteInteger('ProfilePlot', 'LineWidth', LineWidth);
    end;

    // Save directory names
    WriteString('Directories', 'DataDir', ProjectDir);
//    WriteString('Directories', 'TempDir', TempDir);

    // Save general program preferences
    WriteString('Preferences', 'StyleName', StyleName);
    WriteBool('Preferences', 'Blinking', Blinking);
    WriteBool('Preferences', 'FlyOvers', FlyOvers);
    WriteBool('Preferences', 'AutoBackup', AutoBackup);
    WriteBool('Preferences', 'ConfirmDelete', ConfirmDelete);
    WriteBool('Preferences', 'AutoSave', AutoSave);
    WriteBool('Preferences', 'RptElapsedTime', RptElapsedTime);
    WriteBool('Preferences', 'TabDelimited', TabDelimited);

    // Save MRU file names
    for I := 0 to MainForm.MRUList.Count-1 do
      WriteString('MRU', IntToStr(I), MainForm.MRUList[I]);

    // Save Property Editor form's position
{    with PropEditForm do
    begin
      WriteInteger('Property Editor', 'Left', Left);
      WriteInteger('Property Editor', 'Top', Top);
      WriteInteger('Property Editor', 'Width', Width);
      WriteInteger('Property Editor', 'Height', Height);
      WriteInteger('Property Editor', 'HeaderSplit', Editor.HeaderSplit);
    end; }

    // Save output variable display precision
    for I := NODEOUTVAR1 to NODEVIEWS do
       WriteInteger('Display Precision', NodeVariable[I].Name,
                   NodeUnits[I].Digits);
    for I := LINKOUTVAR1 to LINKVIEWS do
       WriteInteger('Display Precision', LinkVariable[I].Name,
                    LinkUnits[I].Digits);
  // Free the .INI file object
  finally
    Free;
  end;
end;


procedure LoadDefaultsFromFile(theIniFile: TInifile);
//-----------------------------------------------------------------------------
//  Reads default property values from theIniFile, which can be either
//  the ITM.ini file or a project ini file.
//-----------------------------------------------------------------------------
var
  I, J : Integer;
  S : String;
begin
  with theIniFile do
  try
    // Retrieve default ID labeling prefixes & increment
    Project.IDIncrement := ReadInteger('Labels', 'Increment', Project.IDIncrement);
    for I := 0 to MAXCLASS do
    begin
      if Project.IsVisual(I) then Project.IDPrefix[I] :=
        ReadString('Labels', ObjectLabels[I], Project.IDPrefix[I]);
    end;

    // Retrieve default properties
    with Project do
    begin

      S := ReadString('Defaults', 'NODE_INVERT',
             DefProp[JUNCTION].Data[NODE_INVERT_INDEX]);
      for J := JUNCTION to STORAGE do
        DefProp[J].Data[NODE_INVERT_INDEX] := S;

      S := ReadString('Defaults', 'NODE_DEPTH',
             DefProp[JUNCTION].Data[JUNCTION_MAX_DEPTH_INDEX]);

      DefProp[CONDUIT].Data[CONDUIT_LENGTH_INDEX] :=
          ReadString('Defaults', 'CONDUIT_LENGTH',
            DefProp[CONDUIT].Data[CONDUIT_LENGTH_INDEX]);
      DefProp[CONDUIT].Data[CONDUIT_DIAMETER_INDEX] :=
          ReadString('Defaults', 'CONDUIT_DIAMETER',
            DefProp[CONDUIT].Data[CONDUIT_DIAMETER_INDEX]);
      DefProp[CONDUIT].Data[CONDUIT_ROUGHNESS_INDEX] :=
          ReadString('Defaults', 'CONDUIT_ROUGHNESS',
            DefProp[CONDUIT].Data[CONDUIT_ROUGHNESS_INDEX]);

      DefProp[OPTION].Data[FLOW_UNITS_INDEX] :=
          ReadString('Defaults', 'FLOW_UNITS',
            DefProp[OPTION].Data[FLOW_UNITS_INDEX]);
      DefProp[OPTION].Data[LINK_OFFSETS_INDEX] :=
          ReadString('Defaults', 'LINK_OFFSETS',
            DefProp[OPTION].Data[LINK_OFFSETS_INDEX]);

      with Project.Options do
      begin
        Data[FLOW_UNITS_INDEX]    := DefProp[OPTION].Data[FLOW_UNITS_INDEX];
        Data[LINK_OFFSETS_INDEX]   := DefProp[OPTION].Data[LINK_OFFSETS_INDEX];
      end;

    end;
  finally
  end;
end;


procedure SaveDefaultsToFile(theIniFile: TInifile);
//-----------------------------------------------------------------------------
//  Saves default property values to theIniFile, which can be either
//  the ITM.ini file or a project ini file.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  with theIniFile do
  try
    // Save default ID labeling prefixes & increment
    WriteInteger('Labels', 'Increment', Project.IDIncrement);
    for I := 0 to MAXCLASS do
    begin
      if Project.IsVisual(I) then
        WriteString('Labels', ObjectLabels[I], Project.IDPrefix[I]);
    end;

    // Save default properties
    with Project do
    begin

      WriteString('Defaults', 'NODE_INVERT',
        DefProp[JUNCTION].Data[NODE_INVERT_INDEX]);
      WriteString('Defaults', 'NODE_DEPTH',
        DefProp[JUNCTION].Data[JUNCTION_MAX_DEPTH_INDEX]);

      WriteString('Defaults', 'CONDUIT_LENGTH',
        DefProp[CONDUIT].Data[CONDUIT_LENGTH_INDEX]);
      WriteString('Defaults', 'CONDUIT_DIAMETER',
        DefProp[CONDUIT].Data[CONDUIT_DIAMETER_INDEX]);
      WriteString('Defaults', 'CONDUIT_ROUGHNESS',
        DefProp[CONDUIT].Data[CONDUIT_ROUGHNESS_INDEX]);

      WriteString('Defaults', 'FLOW_UNITS',
        DefProp[OPTION].Data[FLOW_UNITS_INDEX]);
      WriteString('Defaults', 'LINK_OFFSETS',
        DefProp[OPTION].Data[LINK_OFFSETS_INDEX]);
    end;
  finally
  end;
end;


procedure ReadDefaults;
//-----------------------------------------------------------------------------
//  Initializes default object properties and reads in previously saved
//  default properties from the ITM.ini file.
//-----------------------------------------------------------------------------
var
  I: Integer;
  theIniFile: TInifile;
begin
  // Use factory settings for map legends
  InitMapLegends;

  // Copy factory defaults (e.g., DefJunction)
  // to current defaults (e.g., DefProp[JUNCTION].Data)
  with Project do
  begin
    Uutils.CopyStringArray(DefJunction,   DefProp[JUNCTION].Data);
    Uutils.CopyStringArray(DefBoundary,   DefProp[BOUNDARY].Data);
    Uutils.CopyStringArray(DefGate,       DefProp[GATE].Data);
    Uutils.CopyStringArray(DefWeir,       DefProp[WEIR].Data);
    Uutils.CopyStringArray(DefStorage,    DefProp[STORAGE].Data);
    Uutils.CopyStringArray(DefConduit,    DefProp[CONDUIT].Data);
    Uutils.CopyStringArray(DefOptions,    DefProp[OPTION].Data);
    Uutils.CopyStringArray(DefProp[OPTION].Data, Options.Data);
  end;

  // Assign factory defaults to ID label prefixes & ID increment
  Project.IDIncrement := 1;
  for I := 0 to MAXCLASS do Project.IDPrefix[I] := '';

  // Read defaults from the INI file
  theIniFile := TIniFile.Create(IniFileDir + INIFILE);
  with theIniFile do
  try
    LoadDefaultsFromFile(theIniFile);
  finally
    Free;
  end;
  ResultsSaved := False;
  UpdateFlag := False;
  Uupdate.UpdateUnits;
end;


procedure SaveDefaults;
//-----------------------------------------------------------------------------
//  Saves default properties to the ITM.ini file.
//-----------------------------------------------------------------------------
var
  theIniFile: TInifile;
begin
  theIniFile := TIniFile.Create(IniFileDir + INIFILE);
  with theIniFile do
  try
    SaveDefaultsToFile(theIniFile);
  finally
    Free;
  end;
end;


procedure ReadMainFormSize;
//-----------------------------------------------------------------------------
//  Reads main form's position and size from the EPASWMM5.INI file.
//-----------------------------------------------------------------------------
var
  L, T, W, H: Integer;
begin
  with TIniFile.Create(IniFileDir + INIFILE) do
  try
    with MainForm do
    begin
      Top := (Screen.Height - Height) div 2;
      Left := (Screen.Width - Width) div 2;
      T := ReadInteger('MainForm', 'Top', Top);
      L := ReadInteger('MainForm', 'Left', Left);
      W := ReadInteger('MainForm', 'Width', Width);
      H := ReadInteger('MainForm', 'Height', Height);

      if T + H > Screen.Height then
      begin
        T := 0;
        if H > Screen.Height then H := Screen.Height;
      end;
      if L + W > Screen.Width then
      begin
        L := 0;
        if W > Screen.Width then W := Screen.Width;
      end;
      SetBounds(L, T, W, H);
      ItemsPanel.Height := (BrowserPageControl.Height div 2) +
        BrowserToolBar.Height;
{
      BrowserPageControl.Width :=
        ReadInteger('BrowserPanel', 'Width', BrowserPageControl.Width);
      H := ReadInteger('BrowserPanel', 'Split', ItemsPanel.Height);
      if (H < MulDiv(BrowserPageControl.Height, 9, 10))
      and (H > BrowserPageControl.Height div 10)
      then ItemsPanel.Height := H;
}
    end;
  finally
    Free;
  end;
end;


procedure SaveMainFormSize;
//-----------------------------------------------------------------------------
// Saves main form's position and size to the EPASWMM5.INI file.
//-----------------------------------------------------------------------------
begin
  with TIniFile.Create(IniFileDir + INIFILE) do
  try
    with MainForm do
    begin
      WriteInteger('MainForm', 'Left', Left);
      WriteInteger('MainForm', 'Top', Top);
      WriteInteger('MainForm', 'Width', Width);
      WriteInteger('MainForm', 'Height', Height);
      WriteInteger('BrowserPanel', 'Width', BrowserPageControl.Width);
      WriteInteger('BrowserPanel', 'Split', ItemsPanel.Height);
    end;
  finally
    Free;
  end;
end;


procedure ReadProjIniFile(const Fname: String);
//-----------------------------------------------------------------------------
//  Reads previously saved options from the project's INI file.
//-----------------------------------------------------------------------------
var
  S : String;
  I : Integer;
  N : Integer;
  theIniFile: TInifile;

begin
  theIniFile := TIniFile.Create(Fname);
  with theIniFile do
  try
    // Read map display options
    with MapForm.Map.Options do
    begin
      ShowNodeIDs := ReadBool('Map', 'ShowNodeIDs', ShowNodeIDs);
      ShowNodeValues := ReadBool('Map', 'ShowNodeValues', ShowNodeValues);
      ShowNodesBySize := ReadBool('Map', 'ShowNodesBySize', ShowNodesBySize);
      ShowNodeBorder := ReadBool('Map', 'ShowNodeBorder', ShowNodeBorder);

      ShowLinkIDs := ReadBool('Map', 'ShowLinkIDs', ShowLinkIDs);
      ShowLinkValues := ReadBool('Map', 'ShowLinkValues', ShowLinkValues);
      ShowLinksBySize := ReadBool('Map', 'ShowLinksBySize', ShowLinksBySize);
      ShowLinkBorder := ReadBool('Map', 'ShowLinkBorder', ShowLinkBorder);

      ShowNodes := ReadBool('Map', 'ShowNodes', ShowNodes);
      ShowLinks := ReadBool('Map', 'ShowLinks', ShowLinks);
      ShowNodeSymbols := ReadBool('Map', 'ShowNodeSymbols', ShowNodeSymbols);
      ShowLinkSymbols := ReadBool('Map', 'ShowLinkSymbols', ShowLinkSymbols);

      ShowLabels := ReadBool('Map', 'ShowLabels', ShowLabels);
      LabelsTranspar := ReadBool('Map', 'LabelsTranspar', LabelsTranspar);
      NotationTranspar := ReadBool('Map', 'NotationTranspar', NotationTranspar);

      NodeSize := ReadInteger('Map', 'NodeSize', NodeSize);
      LinkSize := ReadInteger('Map', 'LinkSize', LinkSize);
      NotationSize := ReadInteger('Map', 'NotationSize', NotationSize);

      ArrowStyle := TArrowStyle(ReadInteger('Map', 'ArrowStyle', Ord(ArrowStyle)));
      ArrowSize := ReadInteger('Map', 'ArrowSize', ArrowSize);

      ColorIndex := ReadInteger('Map', 'ColorIndex', ColorIndex);
      NotationZoom := ReadInteger('Map', 'NotationZoom', NotationZoom);
      LabelZoom := ReadInteger('Map', 'LabelZoom', LabelZoom);
      SymbolZoom := ReadInteger('Map', 'SymbolZoom', SymbolZoom);
      ArrowZoom := ReadInteger('Map', 'ArrowZoom', ArrowZoom);
    end;

    // Read backdrop image options
    with Mapform.Map.Backdrop do
    begin
      Visible := ReadBool('Backdrop', 'Visible', Visible);
      Watermark := ReadBool('Backdrop', 'Watermark', Watermark);
    end;

    // Read legend colors & intervals
    N := ReadInteger('Legends', 'NumIntervals', MAXINTERVALS);
    for I := 0 to N do
      MapNodeColor[I] := ReadInteger('Legends', 'MapNodeColor' + IntToStr(I),
                           MapNodeColor[I]);
    for I := 0 to N do
      MapLinkColor[I] := ReadInteger('Legends', 'MapLinkColor' + IntToStr(I),
                           MapLinkColor[I]);
    for I := 1 to NODEVIEWS do
    begin
      S := ReadString('Legends', 'NodeLegend' + IntToStr(I), '');
      ExtractValues(S, 1, N, NodeLegend[I].Intervals);
    end;
    for I := 1 to LINKVIEWS do
    begin
      S := ReadString('Legends', 'LinkLegend' + IntToStr(I), '');
      ExtractValues(S, 1, N, LinkLegend[I].Intervals);
    end;

    // Retrieve default project properties from file
    LoadDefaultsFromFile(theIniFile);
    for I := 0 to MAXCLASS do
      Project.NextID[I] := ReadInteger('Labels', ObjectLabels[I] + '_NextID', 1);

    // Read printed Page Layout info
    with PageLayout do
    begin
      LMargin := StrToFloat(ReadString('Page', 'LeftMargin', FloatToStr(LMargin)));
      RMargin := StrToFloat(ReadString('Page', 'RightMargin', FloatToStr(RMargin)));
      TMargin := StrToFloat(ReadString('Page', 'TopMargin', FloatToStr(TMargin)));;
      BMargin := StrToFloat(ReadString('Page', 'BottomMargin', FloatToStr(BMargin)));
    end;
    with MainForm.PageSetupDialog do
    begin
      Header.Text      := ReadString('Page', 'HeaderText', Header.Text);
      Header.Alignment := TAlignment(ReadInteger('Page', 'HeaderAlignment',
                            Ord(Header.Alignment)));
      Header.Enabled   := ReadBool('Page', 'HeaderEnabled', Header.Enabled);
      Footer.Text      := ReadString('Page', 'FooterText', Footer.Text);
      Footer.Alignment := TAlignment(ReadInteger('Page', 'FooterAlignment',
                            Ord(Footer.Alignment)));
      Footer.Enabled   := ReadBool('Page', 'FooterEnabled', Footer.Enabled);
      PageNumbers      := TPageNumbers(ReadInteger('Page', 'PageNumbers',
                            Ord(PageNumbers)));
    end;
    TitleAsHeader := ReadBool('Page', 'TitleAsHeader', TitleAsHeader);
    Orientation   := ReadInteger('Page', 'Orientation', Orientation);

    // Read date/time format for time series graphs
    GraphOptions.DateTimeFormat := ReadString('Graph', 'DateTimeFormat', '');

    // See if past results were saved to file
    ResultsSaved := ReadBool('Results', 'Saved', ResultsSaved);
    UpdateFlag := not ReadBool('Results', 'Current', True);
  finally
    Free;
  end;
end;


procedure SaveProjIniFile(const Fname: String);
//-----------------------------------------------------------------------------
//  Saves various options to the project's INI file.
//-----------------------------------------------------------------------------
var
  I, J, N : Integer;
  S       : String;
  theIniFile: TInifile;
begin
  theIniFile := TIniFile.Create(Fname);
  with theIniFile do
  try
      // Write current version
      WriteInteger('ITM', 'Version', Uglobals.VERSIONID2);

      // Write map display options
      with MapForm.Map.Options do
      begin
        WriteBool('Map', 'ShowNodeIDs', ShowNodeIDs);
        WriteBool('Map', 'ShowNodeValues', ShowNodeValues);
        WriteBool('Map', 'ShowNodesBySize', ShowNodesBySize);
        WriteBool('Map', 'ShowNodeBorder', ShowNodeBorder);

        WriteBool('Map', 'ShowLinkIDs', ShowLinkIDs);
        WriteBool('Map', 'ShowLinkValues', ShowLinkValues);
        WriteBool('Map', 'ShowLinksBySize', ShowLinksBySize);
        WriteBool('Map', 'ShowLinkBorder', ShowLinkBorder);

        WriteBool('Map', 'ShowNodes', ShowNodes);
        WriteBool('Map', 'ShowLinks', ShowLinks);
        WriteBool('Map', 'ShowNodeSymbols', ShowNodeSymbols);
        WriteBool('Map', 'ShowLinkSymbols', ShowLinkSymbols);

        WriteBool('Map', 'ShowLabels', ShowLabels);
        WriteBool('Map', 'LabelsTranspar', LabelsTranspar);
        WriteBool('Map', 'NotationTranspar', NotationTranspar);

        WriteInteger('Map', 'NodeSize', NodeSize);
        WriteInteger('Map', 'LinkSize', LinkSize);
        WriteInteger('Map', 'NotationSize', NotationSize);

        WriteInteger('Map', 'ArrowStyle', Ord(ArrowStyle));
        WriteInteger('Map', 'ArrowSize', ArrowSize);

        WriteInteger('Map', 'ColorIndex', ColorIndex);
        WriteInteger('Map', 'NotationZoom', NotationZoom);
        WriteInteger('Map', 'LabelZoom', LabelZoom);
        WriteInteger('Map', 'SymbolZoom', SymbolZoom);
        WriteInteger('Map', 'ArrowZoom', ArrowZoom);
      end;

      // Write map backdrop options
      with Mapform.Map.Backdrop do
      begin
        WriteBool('Backdrop', 'Visible', Visible);
        WriteBool('Backdrop', 'Watermark', Watermark);
      end;

      // Write legend colors & intervals
      WriteInteger('Legends', 'NumIntervals', MAXINTERVALS);
      N := MAXINTERVALS;
      for I := 0 to N do
        WriteInteger('Legends', 'MapNodeColor' + IntToStr(I), MapNodeColor[I]);
      for I := 0 to N do
        WriteInteger('Legends', 'MapLinkColor' + IntToStr(I), MapLinkColor[I]);
      for I := 1 to NODEVIEWS do
      begin
        S := '';
        for J := 1 to N do S := S + FloatToStr(NodeLegend[I].Intervals[J]) + ',';
        WriteString('Legends', 'NodeLegend' + IntToStr(I), S);
      end;
      for I := 1 to LINKVIEWS do
      begin
        S := '';
        for J := 1 to N do S := S + FloatToStr(LinkLegend[I].Intervals[J]) + ',';
        WriteString('Legends', 'LinkLegend' + IntToStr(I), S);
      end;

      // Save default project properties
      SaveDefaultsToFile(theIniFile);
      for I := 0 to MAXCLASS do
        WriteInteger('Labels', ObjectLabels[I] + '_NextID', Project.NextID[I]);

      // Save printed Page Layout info
      with PageLayout do
      begin
        WriteString('Page', 'LeftMargin', FloatToStr(LMargin));
        WriteString('Page', 'RightMargin', FloatToStr(RMargin));
        WriteString('Page', 'TopMargin', FloatToStr(TMargin));;
        WriteString('Page', 'BottomMargin', FloatToStr(BMargin));
      end;
      with MainForm.PageSetupDialog do
      begin
        WriteString('Page', 'HeaderText', Header.Text);
        WriteInteger('Page', 'HeaderAlignment', Ord(Header.Alignment));
        WriteBool('Page', 'HeaderEnabled', Header.Enabled);
        WriteString('Page', 'FooterText', Footer.Text);
        WriteInteger('Page', 'FooterAlignment', Ord(Footer.Alignment));
        WriteBool('Page', 'FooterEnabled', Footer.Enabled);
        WriteInteger('Page', 'PageNumbers', Ord(PageNumbers));
      end;
      WriteBool('Page', 'TitleAsHeader', TitleAsHeader);
      WriteInteger('Page', 'Orientation', Orientation);

      // Write date/time format for time series graphs
      WriteString('Graph', 'DateTimeFormat', GraphOptions.DateTimeFormat);

      // Write flag for saved results
      WriteBool('Results', 'Saved', ResultsSaved);
      WriteBool('Results', 'Current', (not UpdateFlag));
  except
  end;
  theIniFile.Free;
end;


procedure ExtractValues(S: String; const N1: Integer; const N2: Integer;
  var X: array of Single);
//-----------------------------------------------------------------------------
//  Parses the string S into a sequence of numbers and places these
//  numbers in the array X at positions N1 to N2.
//-----------------------------------------------------------------------------
var
  Snew: String;
  Slist: TStringlist;
  N, J: Integer;
  Nmax: Integer;
  Y: Single;
begin
  Snew := StringReplace(S, ',', ' ', [rfReplaceAll]);
  Slist := TStringlist.Create;
  try
    Uutils.Tokenize(Snew, Slist, N);
    if N > 0 then
    begin
      Nmax := N2 - N1 + 1;
      if Nmax > N then Nmax := N;
      N := N1;
      for J := 0 to Nmax-1 do
      begin
        if Uutils.GetSingle(Slist[J], Y) then X[N] := Y;
        Inc(N);
      end;
    end;
  finally
    Slist.Free;
  end;
end;

end.


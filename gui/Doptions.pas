unit Doptions;

{-------------------------------------------------------------------}
{                    Unit:    Doptions.pas                          }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/22/22                              }
{                                                                   }
{   Dialog form unit that edits a project's simulation options.     }
{                                                                   }
{   The form consists of a PageControl component with 3 pages,      }
{   one for each category of simulation options.                    }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, System.UITypes, Mask, ComCtrls, Math,  NumEdit,
  Buttons, ImgList, Grids, Uglobals, Uutils, UpDnEdit;

type
  TAnalysisOptionsForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet5: TTabSheet;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    Label17: TLabel;
    Label18: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    StartDatePicker: TDateTimePicker;
    RptDatePicker: TDateTimePicker;
    EndDatePicker: TDateTimePicker;
    StartTimePicker: TDateTimePicker;
    RptTimePicker: TDateTimePicker;
    EndTimePicker: TDateTimePicker;
    Label12: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    MaxCells: TUpDnEditBox;
    MaxPlotCells: TUpDnEditBox;
    WaveCelerityEdit: TNumEdit;
    InitElevEdit: TNumEdit;
    RefDepth: TUpDnEditBox;
    Bevel1: TBevel;
    ApplyDefaultsLabel: TLabel;
    MaxTimeStepEdit: TNumEdit;
    Label9: TLabel;
    UseHotstartEdit: TEdit;
    SaveHotstartEdit: TEdit;
    Label13: TLabel;
    UseHotstartBtn: TBitBtn;
    SaveHotstartBtn: TBitBtn;
    ReportStepEdit: TNumEdit;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure DefaultsLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure ApplyDefaultsLabelClick(Sender: TObject);
    procedure UseHotstartBtnClick(Sender: TObject);
    procedure SaveHotstartBtnClick(Sender: TObject);
  private
    { Private declarations }
    HasChangedTrue: Boolean;

  public
    { Public declarations }
    HasChanged: Boolean;
    procedure SetOptions(Page: Integer);
    procedure GetOptions;
  end;

//var
//  AnalysisOptionsForm: TAnalysisOptionsForm;

implementation

{$R *.dfm}

uses
  Fmain, Uproject, Uupdate;

const
  ItmFlowTypes: array[0..2] of PChar =
    ('FREE-SURFACE', 'MIXED-FLOW', 'FULLY-PRESSURIZED');


procedure TAnalysisOptionsForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  DateFmt: String;
begin
  // Set date format for date picker controls
  DateFmt := 'MM' + MyFormatSettings.DateSeparator + 'dd' +
    MyFormatSettings.DateSeparator + 'yyyy';
  StartDatePicker.Format := DateFmt;
  RptDatePicker.Format := DateFmt;
  EndDatePicker.Format := DateFmt;
  with MainForm.ProjectImageList do
  begin
    GetBitmap(GetIndexByName('browse'), UseHotstartBtn.Glyph);
    GetBitmap(GetIndexByName('browse'), SaveHotstartBtn.Glyph);
  end;

end;

procedure TAnalysisOptionsForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnShow handler.
//-----------------------------------------------------------------------------
begin
  case PageControl1.TabIndex of
  0: ActiveControl := StartDatePicker;
  1: ActiveControl := MaxCells;
  2: ActiveControl := UseHotstartEdit;
  end;
  HasChanged := False;
end;

procedure TAnalysisOptionsForm.SaveHotstartBtnClick(Sender: TObject);
begin
  with MainForm.OpenTextFileDialog do
  begin
    Title := 'Select a Hot Start File';
    Filter := 'Hot start files (*.hsf)|*.hsf|All files|*.*';
    InitialDir := ProjectDir;
    Filename := '*.hsf';
    if Execute then SaveHotstartEdit.Text := Filename;
  end;
end;

procedure TAnalysisOptionsForm.SetOptions(Page: Integer);
//-----------------------------------------------------------------------------
//  Loads current simulation options into form and displays the
//  specified page of the PageControl component.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  with Project.Options do
  begin
    StartDatePicker.Date  := StrToDate(Data[START_DATE_INDEX], MyFormatSettings);
    StartTimePicker.Time  := StrToTime(Data[START_TIME_INDEX], MyFormatSettings);
    RptDatePicker.Date    := StrToDate(Data[REPORT_START_DATE_INDEX], MyFormatSettings);
    RptTimePicker.Time    := StrToTime(Data[REPORT_START_TIME_INDEX], MyFormatSettings);
    EndDatePicker.Date    := StrToDate(Data[END_DATE_INDEX], MyFormatSettings);
    EndTimePicker.Time    := StrToTime(Data[END_TIME_INDEX], MyFormatSettings);
    ReportStepEdit.Text   := Data[REPORT_STEP_INDEX];
    MaxTimeStepEdit.Text  := Data[MAX_TIME_STEP_INDEX];

    MaxCells.Spinner.Position := StrToInt(Data[MAX_NUM_CELLS_INDEX]);
    MaxPlotCells.Spinner.Position := StrToInt(Data[MAX_NUM_PLOT_CELLS_INDEX]);
    RefDepth.Spinner.Position := StrToInt(Data[REF_DEPTH_FRACTION_INDEX]);
    WaveCelerityEdit.Text := Data[PRESS_WAVE_CELERITY_INDEX];
    InitElevEdit.Text := Data[INIT_WATER_ELEV_INDEX];

    UseHotstartEdit.Text := Data[USE_HOTSTART_FILE_INDEX];
    SaveHotstartEdit.Text := Data[SAVE_HOTSTART_FILE_INDEX];
  end;

  HasChanged := False;
  if Page < 0 then Page := 0;
  if Page >= PageControl1.PageCount
  then Page := PageControl1.PageCount - 1;
  PageControl1.TabIndex := Page;
end;

procedure TAnalysisOptionsForm.UseHotstartBtnClick(Sender: TObject);
begin
  with MainForm.OpenTextFileDialog do
  begin
    Title := 'Select a Hot Start File';
    Filter := 'Hot start files (*.hsf)|*.hsf|All files|*.*';
    InitialDir := ProjectDir;
    Filename := '*.hsf';
    Options := Options + [ofFileMustExist];
    if Execute then UseHotstartEdit.Text := Filename;
  end;
end;

procedure TAnalysisOptionsForm.GetOptions;
//-----------------------------------------------------------------------------
//  Unloads current contents of form into project's simulation options.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  with Project.Options do
  begin
    Data[START_DATE_INDEX] := DateToStr(StartDatePicker.Date, MyFormatSettings);
    Data[START_TIME_INDEX] := TimeToStr(StartTimePicker.Time, MyFormatSettings);
    Data[REPORT_START_DATE_INDEX] := DateToStr(RptDatePicker.Date, MyFormatSettings);
    Data[REPORT_START_TIME_INDEX] := TimeToStr(RptTimePicker.Time, MyFormatSettings);
    Data[END_DATE_INDEX] := DateToStr(EndDatePicker.Date, MyFormatSettings);
    Data[END_TIME_INDEX] := TimeToStr(EndTimePicker.Time, MyFormatSettings);

    Data[REPORT_STEP_INDEX] := ReportStepEdit.Text;
    Data[MAX_TIME_STEP_INDEX] := MaxTimeStepEdit.Text;
    Data[MAX_NUM_CELLS_INDEX] := IntToStr(MaxCells.Spinner.Position);
    Data[MAX_NUM_PLOT_CELLS_INDEX] := IntToStr(MaxPlotCells.Spinner.Position);
    Data[REF_DEPTH_FRACTION_INDEX] := IntToStr(RefDepth.Spinner.Position);
    Data[PRESS_WAVE_CELERITY_INDEX] := WaveCelerityEdit.Text;
    Data[INIT_WATER_ELEV_INDEX] := InitElevEdit.Text;

    Data[USE_HOTSTART_FILE_INDEX] := UseHotstartEdit.Text;
    Data[SAVE_HOTSTART_FILE_INDEX] := SaveHotstartEdit.Text;
  end;
end;

procedure TAnalysisOptionsForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button.
//-----------------------------------------------------------------------------
begin
  ModalResult := mrOK;
end;

//------------------------------------------------------------------------------
//  OnChanging and OnChange handlers for the form's tabbed Page Control.
//  (Prevents the HasChanged flag from being set to True after OnChange
//  events for controls on a user-selected page fire when they get redrawn).
//------------------------------------------------------------------------------

procedure TAnalysisOptionsForm.PageControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  // Save the current value of the HasChanged flag
  HasChangedTrue := HasChanged;
end;

procedure TAnalysisOptionsForm.PageControl1Change(Sender: TObject);
begin
  // Restore the OnChange flag to its value before page was changed
  HasChanged := HasChangedTrue;
end;


procedure TAnalysisOptionsForm.EditChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for most of form's controls.
//-----------------------------------------------------------------------------
begin
  HasChanged := True;
end;

procedure TAnalysisOptionsForm.DefaultsLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
//-----------------------------------------------------------------------------
//  OnClick handler for the Apply Defaults label.
//-----------------------------------------------------------------------------
begin
//
end;

procedure TAnalysisOptionsForm.ApplyDefaultsLabelClick(Sender: TObject);
begin
    MaxCells.Spinner.Position := StrToInt(DefOptions[MAX_NUM_CELLS_INDEX]);
    MaxPlotCells.Spinner.Position := StrToInt(DefOptions[MAX_NUM_PLOT_CELLS_INDEX]);
    RefDepth.Spinner.Position := StrToInt(DefOptions[REF_DEPTH_FRACTION_INDEX]);
    WaveCelerityEdit.Text := DefOptions[PRESS_WAVE_CELERITY_INDEX];
    InitElevEdit.Text := DefOptions[INIT_WATER_ELEV_INDEX];
end;

procedure TAnalysisOptionsForm.HelpBtnClick(Sender: TObject);
begin
  with PageControl1 do
    if ActivePage = TabSheet2 then
       Application.HelpCommand(HELP_CONTEXT, 211210)
    else if ActivePage = TabSheet3 then
       Application.HelpCommand(HELP_CONTEXT, 211230)
    else if ActivePage = TabSheet5 then
       Application.HelpCommand(HELP_CONTEXT, 212080);
end;

end.

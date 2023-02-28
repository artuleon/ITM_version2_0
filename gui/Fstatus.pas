unit Fstatus;

{-------------------------------------------------------------------}
{                    Unit:    Fstatus.pas                           }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/23/22                              }
{                                                                   }
{   MDI child form that displays the status report generated        }
{   from a run of ITM.                                              }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Types, Messages, Classes, Graphics, Controls, Windows,
  Forms, Dialogs, StdCtrls, ComCtrls, Clipbrd, Menus, StrUtils, ExtCtrls,
  System.IOUtils, Xprinter, Uglobals, Uutils;

type
  TStatusForm = class(TForm)
    Memo1: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ReportType: Integer;
    ReportFile: String;
    procedure SetReportType(aType: Integer);
    procedure RefreshStatusReport;
    procedure ClearReport;
    procedure CopyTo;
    procedure Print(Destination: TDestination);
  end;

var
  StatusForm: TStatusForm;        // Do not comment out this line.

implementation

{$R *.DFM}

uses
  Dcopy, Fmain;

const
  MSG_NO_FILE = 'There are no results to view.';
  MSG_REPORT_TOO_BIG = 'Report is too big to fit in window.';

  Captions: array[STATUSREPORT..DEBUGREPORT] of String =
    ('Status Report', 'Debug Report');


procedure TStatusForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
// OnClose handler for form.
//-----------------------------------------------------------------------------
begin
  Action := caFree;
end;

procedure TStatusForm.FormCreate(Sender: TObject);
begin
  ReportType := -1;
end;

procedure TStatusForm.SetReportType(aType: Integer);
//-----------------------------------------------------------------------------
// Sets the type of report displayed on the form.
//-----------------------------------------------------------------------------
begin
  ReportType := -1;
  if not aType in [STATUSREPORT, DEBUGREPORT] then exit;
  ReportType := aType;
  case ReportType of
  STATUSREPORT:  ReportFile := TempReportFile;
  DEBUGREPORT:   ReportFile := TempDebugFile;
  end;
end;


procedure TStatusForm.RefreshStatusReport;
//-----------------------------------------------------------------------------
// Reloads the form with the contents
// of the Status Report File generated from an analysis.
//-----------------------------------------------------------------------------
var
  S: String;
  I: Integer;
begin
  // Clear report's contents
  Memo1.Clear;

  // Check that a report type has been set
  if (ReportType = -1) then
  begin
    Caption := 'Status Report';
    Memo1.Lines.Add(MSG_NO_FILE);
    exit;
  end;
  Caption := Captions[ReportType];

  // Display contents of report file
  if not FileExists(ReportFile) then
    Memo1.Lines.Add(MSG_NO_FILE)
  else
  begin
//    Memo1.Lines.LoadFromFile(ReportFile);
    S := TFile.ReadAllText(ReportFile);
    for I := 0 to Length(S)-1 do
      if S[I] = #0 then S[I] := ' ';
    Memo1.Text := S;
    Memo1.SelStart := 0;
  end;
end;

procedure TStatusForm.ClearReport;
begin
  Memo1.Clear;
end;

procedure TStatusForm.CopyTo;
//-----------------------------------------------------------------------------
// Copies contents of the FileViewer to either a file or to the Clipboard.
//-----------------------------------------------------------------------------
var
  copyToForm : TCopyToForm;
begin
  // Create the CopyTo dialog form
  copyToForm := TCopyToForm.Create(self);
  with copyToForm do
  try

    // Disable format selection (since it has to be Text)
    FormatGroup.ItemIndex := 2;
    FormatGroup.Enabled := False;

    // Show the form modally
    if ShowModal = mrOK then
    begin

      // If user supplies a file name then copy report to it
      if Length(DestFileName) > 0
      then Memo1.Lines.SaveToFile(DestFileName)

      // Otherwise copy the contents into the Clipboard
      else Memo1.CopyToClipboard;
    end;

  // Free the CopyTo dialog form
  finally
    copyToForm.Free;
  end;
end;

procedure TStatusForm.Print(Destination: TDestination);
//-----------------------------------------------------------------------------
// Prints Status Report to Destination (printer or preview form).
//-----------------------------------------------------------------------------
var
//  I    : Integer;
//  S    : array[0..1024] of WideChar;
  Line : String;
  F : TextFile;
begin
  with MainForm.thePrinter do
  begin
    BeginJob;
    try
      SetDestination(Destination);
      AssignFile(F, ReportFile);
      try
        {$I-}
        Reset(F);
        {$I+}
        while not Eof(F) do
        begin
          Readln(F, Line);
          PrintLine(Line);
        end;
      finally
        CloseFile(F);
      end;
    finally
      EndJob;
    end;
  end;
end;

end.

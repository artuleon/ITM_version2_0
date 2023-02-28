unit Fproped;

{-------------------------------------------------------------------}
{                    Unit:    Fproped.pas                           }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/25/22                              }
{                                                                   }
{   Form unit that is a container for a TPropEdit component.        }
{   This component serves as the Property  Editor for data          }
{   objects and is styled after the Delphi object inspector.        }
{   The form is created on startup and remains active until the     }
{   application closes.                                             }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs,  ExtCtrls, ComCtrls, StdCtrls,
  PropEdit, Xprinter, Uglobals, Uproject, Uutils;

type
  TPropEditForm = class(TForm)
    Panel1: TPanel;
    HintLabel: TLabel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    procedure CallHelp;
    procedure EditCurve(const ObjType: Integer; var S: String);
    procedure EditTimeSeries(var S: String);
    procedure Validate(Sender: TObject; Index: Integer; var S: String;
      var Errmsg: String; var IsValid: Boolean);
    procedure ButtonClick(Sender: TObject; Index: Integer;
      var S: String; var Modified: Boolean);
    procedure UpdateCurveLists;
    procedure UpdateTimeseriesLists;
    procedure ShowPropertyHint(Sender: TObject; aRow: LongInt);
  public
    { Public declarations }
    Editor: TPropEdit;
    procedure RefreshPropertyHint;
  end;

var
  PropEditForm: TPropEditForm;

implementation

{$R *.DFM}

uses Fmain, Ubrowser, Uedit, Uvalidate;

const
  TXT_PROPERTY = 'Property';
  TXT_VALUE = 'Value';
  TXT_DESCRIPTION = 'Description of ';

procedure TPropEditForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnCreate handler for the form. Creates a TPropEdit component to edit an
// object's properties. The event handler function for properties with an
// ellipsis button is EditorButtonClick. The event handler for validating
// editor input is EditorValidate. The event handler for selecting a new
// row (i.e., property) is ShowPropertyHint.
//-----------------------------------------------------------------------------
begin
  Editor := TPropEdit.Create(Self);
  with Editor do
  begin
    Parent := Self;
    ParentFont := True;
    ParentColor := True;
    Align := alClient;
    BorderStyle := bsNone;
    ColHeading1 := TXT_PROPERTY;
    ColHeading2 := TXT_VALUE;
    HeaderSplit := 50;
    ReadOnlyColor := clBtnFace;
    ValueColor := clNavy;
    HeadBackColor := RGB(191,205,219);  //clInactiveCaption;  //clSilver;  //
    HeadForeColor := clBlack;
    OnButtonClick := ButtonClick;
    OnValidate := Validate;
    OnRowSelect := ShowPropertyHint;
  end;
  PopupMode:=pmExplicit;
  PopupParent:=Self;
end;

procedure TPropEditForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------------------------------
// OnClose event handler for form.
//-----------------------------------------------------------------------------
begin
  Action := caHide;
end;

procedure TPropEditForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDestroy event handler for form.
//-----------------------------------------------------------------------------
begin
  Editor.Free;
end;

procedure TPropEditForm.FormDeactivate(Sender: TObject);
//-----------------------------------------------------------------------------
// OnDeactivate event handler for form. Calls the Editor's IsValid function
// to validate the value of the current property when the form looses focus.
//-----------------------------------------------------------------------------
begin
  Editor.IsValid;
end;

procedure TPropEditForm.Validate(Sender: TObject; Index: Integer;
  var S: String; var Errmsg: String; var IsValid: Boolean);
//-----------------------------------------------------------------------------
// OnValidate event handler for the TPropEdit editor component.
// Passes the string value S of property index Index to the ValidateEditor
// function in the Uvalidate.pas unit.
//-----------------------------------------------------------------------------
begin
  IsValid := Uvalidate.ValidateEditor(Index, S, Errmsg);
  if (not IsValid) and (Length(Errmsg) > 0)
  then Uutils.MsgDlg(Errmsg, mtError, [mbOK]);
end;

procedure TPropEditForm.ButtonClick(Sender: TObject; Index: Integer;
  var S: String; var Modified: Boolean);
//-----------------------------------------------------------------------------
// OnButtonClick event handler. Activated when the user clicks the ellipsis
// button for a specific property. Index = index of the property being
// edited, S = string value of the edited property, Modified = true if
// the property value changes.
//-----------------------------------------------------------------------------
begin
  // User wants to edit a Comment
  if Index = COMMENT_INDEX
  then Uedit.EditComment(TXT_DESCRIPTION + Caption, S, Modified)

  // User wants to edit a Junction's external inflow
  else if (CurrentList = JUNCTION) and (Index = JUNCTION_INFLOWS_INDEX)
  then Uedit.EditNodalInflow(CurrentList, Project.CurrentItem[CurrentList],
                               S, Modified)

  // User wants to edit a Storage Unit's storage curve
  else if (CurrentList = STORAGE) and (Index = STORAGE_SHAPE_TABLE_INDEX)
  then EditCurve(STORAGECURVE, S)

  // User wants to edit an Outfall's rating curve
  else if (CurrentList = WEIR) and (Index = WEIR_RATING_CURVE_INDEX)
  then EditCurve(RATINGCURVE, S)

  // User wants to edit a label's font
  else if (CurrentList = MAPLABEL)
  then Uedit.EditLabelFont(Project.CurrentItem[CurrentList], Modified);

  if Modified then MainForm.SetChangeFlags;
end;

procedure TPropEditForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
// OnKeyDown event handler for the form. Processes certain keystrokes
// to change which object is being edited.
//-----------------------------------------------------------------------------
begin
  case Key of

    // Shift-PgDown loads first object into editor.
    // PgDown loads prior object into editor.
    vk_PRIOR:
      begin
        if (Project.CurrentItem[CurrentList] > 0) then
        begin
          if (ssCtrl in Shift)
          then Project.CurrentItem[CurrentList] := 0
          else Dec(Project.CurrentItem[CurrentList]);
          Ubrowser.BrowserUpdate(CurrentList, Project.CurrentItem[CurrentList]);
        end;
        Key := 0;
      end;

    // Shift-PgUp loads last object into editor.
    // PgUp loads next object into editor.
    vk_NEXT:
      begin
        if (Project.CurrentItem[CurrentList] <
            Project.Lists[CurrentList].Count-1) then
        begin
          if (ssCtrl in Shift)
          then Project.CurrentItem[CurrentList] :=
                 Project.Lists[CurrentList].Count - 1
          else Inc(Project.CurrentItem[CurrentList]);
          Ubrowser.BrowserUpdate(CurrentList, Project.CurrentItem[CurrentList]);
        end;
        Key := 0;
      end;

    // Shift-Tab shifts focus to the MainForm
    vk_TAB:
      begin
        if (ssCtrl in Shift) then MainForm.SetFocus;
        Key := 0;
      end;

    // F1 brings up context sensitive Help
    vk_F1: CallHelp;

    // Escape closes the form
    vk_ESCAPE: Close;
  end;
end;

procedure TPropEditForm.EditCurve(const ObjType: Integer; var S: String);
//-----------------------------------------------------------------------------
// Edits a Curve object of type ObjType with ID name S.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S1: String;
begin
  I := Project.Lists[ObjType].IndexOf(S);
  S1 := Uedit.EditCurve(ObjType, I);
  if Length(S1) > 0 then
  begin
    S := S1;
    UpdateCurveLists;
  end;
end;

procedure TPropEditForm.EditTimeSeries(var S: String);
//-----------------------------------------------------------------------------
// Edits a TimeSeries object with ID name S.
//-----------------------------------------------------------------------------
var
  I: Integer;
  S1: String;
begin
  I := Project.Lists[TIMESERIES].IndexOf(S);
  S1 := Uedit.EditTimeseries(I);
  if Length(S1) > 0 then
  begin
    S := S1;
    UpdateTimeseriesLists;
  end;
end;

procedure TPropEditForm.UpdateCurveLists;
//-----------------------------------------------------------------------------
// Updates the list of curve names that appear in the editor.
//-----------------------------------------------------------------------------
begin
  case CurrentList of
  GATE:
    GateProps[GATE_HLOSS_CURVE_INDEX].List := Project.Lists[GATE_CURVE].Text;
  STORAGE:
    StorageProps[STORAGE_SHAPE_TABLE_INDEX].List := Project.Lists[STORAGECURVE].Text;
  WEIR:
    WeirProps[WEIR_RATING_CURVE_INDEX].List := Project.Lists[RATINGCURVE].Text;
  end;
end;

procedure TPropEditForm.UpdateTimeseriesLists;
//-----------------------------------------------------------------------------
// Updates the list of time series names that appear in the editor.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  S := Project.Lists[TIMESERIES].Text;
  case CurrentList of
  GATE:  GateProps[GATE_TIME_SERIES_INDEX].List := S;
  end;
end;

procedure TPropEditForm.ShowPropertyHint(Sender: TObject; aRow: LongInt);
//-----------------------------------------------------------------------------
// Displays hint text for the current property appearing in row aRow
// of the editor.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  case CurrentList of
  JUNCTION: S := JunctionHint[aRow] + JunctionPropUnits[aRow][Ord(UnitSystem)];
  BOUNDARY: S := BoundaryHint[aRow] + BoundaryPropUnits[aRow][Ord(UnitSystem)];
  GATE:     S := GateHint[aRow]     + GatePropUnits[aRow][Ord(UnitSystem)];
  WEIR:     S := WeirHint[aRow]     + WeirPropUnits[aRow][Ord(UnitSystem)];
  STORAGE:  S := StorageHint[aRow]  + StoragePropUnits[aRow][Ord(UnitSystem)];
  CONDUIT:  S := ConduitHint[aRow]  + ConduitPropUnits[aRow][Ord(UnitSystem)];
  MAPLABEL: S := LabelHint[aRow];
  else S := 'Press F1 for Help';
  end;
  HintLabel.Caption := S;
end;

procedure TPropEditForm.RefreshPropertyHint;
begin
  ShowPropertyHint(self, Editor.Row);
end;

procedure TPropEditForm.CallHelp;
//-----------------------------------------------------------------------------
// Determines which Help topic to display when F1 pressed.
//-----------------------------------------------------------------------------
var
  HC: Integer;
begin
  case CurrentList of
    JUNCTION:   HC := 211480;
    BOUNDARY:   HC := 211500;
    GATE:       HC := 211510;
    WEIR:       HC := 211560;
    STORAGE:    HC := 211520;
    CONDUIT:    HC := 211530;
    MAPLABEL:   HC := 211580;
    else        HC := 0;
  end;
  if HC > 0 then Application.HelpCommand(HELP_CONTEXT, HC);
end;

end.

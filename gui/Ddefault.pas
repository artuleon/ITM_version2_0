unit Ddefault;

{-------------------------------------------------------------------}
{                    Unit:    Ddefault.pas                          }
{                    Project: ITM                                   }
{                    Version: 2.0                                   }
{                    Date:    03/06/23                              }
{                                                                   }
{   Dialog form unit that selects default settings for the current  }
{   project.                                                        }
{                                                                   }
{   The form contains a Tab control with three tabs. A PropEdit     }
{   control is placed on the Tab control and is used to set         }
{   defaults for ID prefixes, nodes and links.                      }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, PropEdit, Spin, ComCtrls, Uproject, Uglobals, Uutils;

type
  TDefaultsForm = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    CheckDefault: TCheckBox;
    TabControl1: TTabControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    PropList  : array[0..1] of TStringlist;
    TmpUnitSystem: TUnitSystem;
    TmpOffsets: String;
    procedure SetDefaults(const I: Integer);
    procedure GetDefaults;
    procedure ValidateOption(Sender: TObject; Index: Integer; var S: String;
              var Errmsg: String; var IsValid: Boolean);
  public
    { Public declarations }
    PropEdit1: TPropEdit;
    Modified: Boolean;
  end;

//var
//  DefaultsForm: TDefaultsForm;

implementation

{$R *.DFM}

uses
  Uinifile, Uupdate;

const
  MAXPREFIX         = 6; //Max. chars. in an ID prefix
  TXT_OBJECT        = 'Object';
  TXT_ID_PREFIX     = 'ID Prefix';
  TXT_PROPERTY      = 'Property';
  TXT_DEF_VALUE     = 'Default Value';
  TXT_OPTION        = 'Option';

// A TPropRecord record determines how properties are displayed
// and edited in the PropEdit control (see PropEdit.pas unit).

  PrefixProps: array[0..8] of TPropRecord =
   ((Name:'Junctions';     Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Boundaries';    Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Storage Units'; Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Conduits';      Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Pumps';         Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Orifices';      Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Weirs';         Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'Outlets';       Style:esEdit;    Mask:emNoSpace;  Length:MAXPREFIX),
    (Name:'ID Increment';  Style:esEdit;    Mask:emPosNumber));

  NodeLinkProps: array[0..7] of TPropRecord =
   ((Name:'Node Invert';       Style:esEdit;      Mask:emNumber),
    (Name:'Junction Depth';    Style:esEdit;      Mask:emPosNumber),
    (Name:'Drop Shaft Area';   Style:esEdit;      Mask:emNumber),
    (Name:'Conduit Diameter';  Style:esEdit;      Mask:emPosNumber),
    (Name:'Conduit Length';    Style:esEdit;      Mask:emPosNumber),
    (Name:'Conduit Roughness'; Style:esEdit;      Mask:emPosNumber),
    (Name:'Flow Units';        Style:esReadOnly),
    (Name:'Link Offsets';      Style:esReadOnly));

procedure TDefaultsForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnCreate event handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Set font size & style
  //Uglobals.SetFont(self);

  // Create Property Editor
  PropEdit1 := TPropEdit.Create(self);
  with PropEdit1 do
  begin
    Parent := TabControl1;
    Align := alClient;
    BorderStyle := bsNone;
    ColHeading1 := TXT_OBJECT;
    ColHeading2 := TXT_ID_PREFIX;
    ValueColor := clNavy;
    ReadOnlyColor := clBtnFace;
    OnValidate := ValidateOption;
  end;

  // Load current default values into stringlists.
  for I := 0 to 1 do
  begin
    PropList[I] := TStringList.Create;
    SetDefaults(I);
  end;
  TmpUnitSystem := Uglobals.UnitSystem;
  TmpOffsets := Project.DefProp[OPTION].Data[LINK_OFFSETS_INDEX];
  Modified := False;
end;

procedure TDefaultsForm.FormShow(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnShow event handler.
//-----------------------------------------------------------------------------
begin
  TabControl1.TabIndex := 0;
  TabControl1Change(Sender);
end;

procedure TDefaultsForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnDestroy event handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  for I := 0 to 1 do PropList[I].Free;
  PropEdit1.Free;
end;

procedure TDefaultsForm.SetDefaults(const I: Integer);
//-----------------------------------------------------------------------------
// Loads current set of default values into the work array of
// stringlists (PropList).
//-----------------------------------------------------------------------------
begin
  case I of
  // ID prefixes
  0: begin
       PropList[I].Add(Project.IDPrefix[JUNCTION]);
       PropList[I].Add(Project.IDPrefix[BOUNDARY]);
       PropList[I].Add(Project.IDPrefix[STORAGE]);
       PropList[I].Add(Project.IDPrefix[CONDUIT]);
       PropList[I].Add(Project.IDPrefix[PUMP]);
       PropList[I].Add(Project.IDPrefix[ORIFICE]);
       PropList[I].Add(Project.IDPrefix[WEIR]);
       PropList[I].Add(Project.IDPrefix[OUTLET]);
       PropList[I].Add(IntToStr(Project.IDIncrement));
     end;

  // Node/link parameters
  1: begin
       PropList[I].Add(Project.DefProp[JUNCTION].Data[NODE_INVERT_INDEX]);
       PropList[I].Add(Project.DefProp[JUNCTION].Data[JUNCTION_MAX_DEPTH_INDEX]);
       PropList[I].Add(Project.DefProp[JUNCTION].Data[JUNCTION_AREA_INDEX]);
       PropList[I].Add(Project.DefProp[CONDUIT].Data[CONDUIT_DIAMETER_INDEX]);
       PropList[I].Add(Project.DefProp[CONDUIT].Data[CONDUIT_LENGTH_INDEX]);
       PropList[I].Add(Project.DefProp[CONDUIT].Data[CONDUIT_ROUGHNESS_INDEX]);
       PropList[I].Add('CMS');
       PropList[I].Add('DEPTH');
     end;
  end;
end;

procedure TDefaultsForm.GetDefaults;
//-----------------------------------------------------------------------------
// Transfers values from work array of stringlists to the project's defaults.
//-----------------------------------------------------------------------------
var
  J, Code, V: Integer;
begin
  // ID Prefixes
  Project.IDPrefix[JUNCTION] := PropList[0].Strings[0];
  Project.IDPrefix[BOUNDARY] := PropList[0].Strings[1];
  Project.IDPrefix[STORAGE]  := PropList[0].Strings[2];
  Project.IDPrefix[CONDUIT]  := PropList[0].Strings[3];
  Project.IDPrefix[PUMP]     := PropList[0].Strings[4];
  Project.IDPrefix[ORIFICE]  := PropList[0].Strings[5];
  Project.IDPrefix[WEIR]     := PropList[0].Strings[6];
  Project.IDPrefix[OUTLET]   := PropList[0].Strings[7];
  Val(PropList[0].Strings[8], V, Code);
  if Code = 0 then
  begin
    if V <= 0 then V := 1;
    Project.IDIncrement := V;
    for J := 0 to MAXCLASS do
    begin
      if (Project.NextID[J] <= V)
      or (Project.Lists[J].Count = 0) then Project.NextID[J] := V
      else Project.NextID[J] := Project.NextID[J] + V;
    end;
  end;

  // Node/Link parameters:
  // Node invert elev.
  for J := JUNCTION to STORAGE do
  begin
    Project.DefProp[J].Data[NODE_INVERT_INDEX] := PropList[1].Strings[0];
  end;

  // Node max. depth
  Project.DefProp[JUNCTION].Data[JUNCTION_MAX_DEPTH_INDEX] := PropList[1].Strings[1];

  // Conduit diameter & length
  Project.DefProp[CONDUIT].Data[CONDUIT_DIAMETER_INDEX] := PropList[1].Strings[2];
  Project.DefProp[CONDUIT].Data[CONDUIT_LENGTH_INDEX] := PropList[1].Strings[3];

  // Conduit Roughness
  Project.DefProp[CONDUIT].Data[CONDUIT_ROUGHNESS_INDEX] := PropList[1].Strings[4];

  // Project flow units
  Project.DefProp[OPTION].Data[FLOW_UNITS_INDEX] := PropList[1].Strings[5];

  // Link Offsets
  Project.DefProp[OPTION].Data[LINK_OFFSETS_INDEX] := PropList[1].Strings[6];

  // Update Project options
  Project.Options.Data[FLOW_UNITS_INDEX] :=
    Project.DefProp[OPTION].Data[FLOW_UNITS_INDEX];
  Project.Options.Data[LINK_OFFSETS_INDEX] :=
    Project.DefProp[OPTION].Data[LINK_OFFSETS_INDEX];
end;

procedure TDefaultsForm.BtnOKClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick event handler for the OK button.
//-----------------------------------------------------------------------------
begin
  // Validate last entry in the PropEdit control
  if not PropEdit1.IsValid then
  begin
    PropEdit1.Edit;
    ModalResult := mrNone;
    Exit;
  end;

  // Retrieve updated project defaults
  GetDefaults;

  // Set the global Modified flag if edits were made
  if PropEdit1.Modified then Modified := True;

  // If Default checkbox checked then save defaults to file
  Uupdate.UpdateUnits;
  Uupdate.UpdateLinkHints;
  if not SameText(tmpOffsets, Project.DefProp[OPTION].Data[LINK_OFFSETS_INDEX])
  then Uupdate.UpdateOffsets;
  if CheckDefault.Checked then Uinifile.SaveDefaults;
  ModalResult := mrOK;
end;

procedure TDefaultsForm.BtnCancelClick(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick event handler for the Cancel button.
//-----------------------------------------------------------------------------
begin
  Uglobals.UnitSystem := TmpUnitSystem;
  ModalResult := mrCancel;
end;

procedure TDefaultsForm.TabControl1Change(Sender: TObject);
//-----------------------------------------------------------------------------
// OnClick event handler for the TabControl1.
// Switches the set of default properties edited in PropEdit1.
//-----------------------------------------------------------------------------
begin
  PropEdit1.IsValid;
  case TabControl1.TabIndex of
  0:  begin
        PropEdit1.ColHeading1 := TXT_OBJECT;;
        PropEdit1.ColHeading2 := TXT_ID_PREFIX;
        PropEdit1.SetProps(PrefixProps, PropList[0]);
        PropEdit1.Edit;
      end;
  1:  begin
        PropEdit1.ColHeading1 := TXT_OPTION;
        PropEdit1.ColHeading2 := TXT_DEF_VALUE;
        PropEdit1.SetProps(NodeLinkProps, PropList[1]);
        PropEdit1.Edit;
      end;
  end;
end;

procedure TDefaultsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------------------------------
// OnKeyDown event handler for the Form (KeyPreview was set to True).
// Allows use of Ctrl-Tab keystroke to change tabs.
//-----------------------------------------------------------------------------
begin
  if (ssCtrl in Shift) and (Key = VK_TAB) then with TabControl1 do
  begin
    if TabIndex < Tabs.Count - 1 then TabIndex := TabIndex + 1
    else TabIndex := 0;
    TabControl1Change(Sender);
  end;
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

procedure TDefaultsForm.ValidateOption(Sender: TObject; Index: Integer;
  var S: String; var Errmsg: String; var IsValid: Boolean);
//-----------------------------------------------------------------------------
// OnValidate event handler for the TPropEdit editor component.
//-----------------------------------------------------------------------------
begin
  IsValid := True;
  if (TabControl1.TabIndex > 0) and (Length(Trim(S)) = 0) then
  begin
    Errmsg := 'This field cannot be left blank.';
    IsValid := False;
  end
  else if (TabControl1.TabIndex = 1) and (Index = 5) then
  begin
    if Uutils.FindKeyWord(S, SIFlowUnits, 3) >= 0
    then Uglobals.UnitSystem := usSI
    else Uglobals.UnitSystem := usUS;
  end;
end;

procedure TDefaultsForm.BtnHelpClick(Sender: TObject);
begin
  Case TabControl1.TabIndex of
    0:  Application.HelpCommand(HELP_CONTEXT, 210930);
    1:  Application.HelpCommand(HELP_CONTEXT, 210950);
  end;
end;

end.

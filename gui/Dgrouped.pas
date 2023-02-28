unit Dgrouped;

{-------------------------------------------------------------------}
{                    Unit:    Dgrouped.pas                          }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/21/22                              }
{                                                                   }
{   Dialog form unit used to edit a property of a group of objects  }
{   that lie within a fencelined region of the study area map.      }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, PropEdit, NumEdit, Uproject, Uglobals, Uutils;

type
  TEditType = (etReplace, etMultiply, etAdd);

  TGroupEditForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    ClassListbox: TComboBox;
    TagCheckBox: TCheckBox;
    PropertyListBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    PropertyNumEdit: TNumEdit;
    PropertyEditBtn: TButton;
    TagEditBox: TNumEdit;
    EditTypeListBox: TComboBox;
    ReplaceWithLabel: TLabel;
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ClassListboxChange(Sender: TObject);
    procedure TagCheckBoxClick(Sender: TObject);
    procedure PropertyListBoxChange(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure PropertyEditBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditTypeListBoxChange(Sender: TObject);
  private
    { Private declarations }
    OldClassItemIndex: Integer;
    theRegion: HRgn;
    NumObjects: Integer;
    procedure EditConduits(const newValue: String; const EditType: TEditType);
    procedure EditJunctions(const newValue: String; const EditType: TEditType);
    procedure EditStorage(const newValue: String; const EditType: TEditType);
    function  GetNewValue(var Value1: String; const Value2: String;
              const EditType: TEditType): Boolean;
    function  ObjectQualifies(const X: Extended; const Y: Extended;
              const Tag: String): Boolean;
  public
    { Public declarations }
  end;

//var
//  GroupEditForm: TGroupEditForm;

implementation

{$R *.DFM}

uses
  Fmap, Ubrowser;

const
  TXT_PROPERTY = 'Property';
  TXT_VALUE = 'Value';
  TXT_ASSIGNED = ' Assigned';
  TXT_CLICK_TO_EDIT = '<Click to Edit>';
  TXT_WERE_CHANGED = ' were changed.' + #13 + 'Continue editing?';
  MSG_NO_DATA = 'No new value was entered.';

  ClassLabels: array[0..2] of PChar =
    ('Junction', 'Storage Unit', 'Conduit');

  EditedJunctionProps: array[0..4] of Integer =
    (TAG_INDEX,  NODE_INVERT_INDEX,
     JUNCTION_MAX_DEPTH_INDEX, JUNCTION_INIT_DEPTH_INDEX,
     JUNCTION_AREA_INDEX);

  EditedStorageProps: array[0..4] of Integer =
    (TAG_INDEX,                NODE_INVERT_INDEX,
     STORAGE_MAX_DEPTH_INDEX,  STORAGE_INIT_DEPTH_INDEX,
     STORAGE_OUTFLOW_INDEX);

  EditedConduitProps: array[0..7] of Integer =
    (TAG_INDEX,
     CONDUIT_DIAMETER_INDEX,  CONDUIT_LENGTH_INDEX,
     CONDUIT_ROUGHNESS_INDEX, CONDUIT_INLET_HT_INDEX,
     CONDUIT_OUTLET_HT_INDEX, CONDUIT_ENTRY_LOSS_INDEX,
     CONDUIT_EXIT_LOSS_INDEX);

procedure TGroupEditForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnCreate handler.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Set font size & style
  //Uglobals.SetFont(self);

  // Disable TagListbox control
  TagEditbox.Enabled := False;

  // Place the ReplaceWithLabel on top of the EditTypeListBox
  ReplaceWithLabel.Left := EditTypeListBox.Left;
  ReplaceWithLabel.Visible := False;
  EditTypeListBox.Top := PropertyNumEdit.Top;

  // Populate ClassListbox with names of classes that can be edited
  for I := Low(ClassLabels) to High(ClassLabels) do
    ClassListbox.Items.Add(ClassLabels[I]);
  ClassListbox.ItemIndex := 0;
  OldClassItemIndex := -1;
  ClassListboxChange(Sender);

  //Create a GDI region from user's fenceline region of the study area map
  theRegion := CreatePolygonRgn(MapForm.Fenceline, MapForm.NumFencePts - 1,
                                WINDING);
end;

procedure TGroupEditForm.FormDestroy(Sender: TObject);
//-----------------------------------------------------------------------------
//  Form's OnDestroy handler.
//-----------------------------------------------------------------------------
begin
  DeleteObject(theRegion);
end;

procedure TGroupEditForm.ClassListboxChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  Updates the form when user selects a different class of object to edit.
//-----------------------------------------------------------------------------
var
  I, J: Integer;
begin
  if OldClassItemIndex = ClassListbox.ItemIndex then exit;
  OldClassItemIndex := ClassListbox.ItemIndex;
  PropertyListBox.Clear;
  case ClassListbox.ItemIndex of
  0:  // Junctions
    begin
      for I := 0 to High(EditedJunctionProps) do
      begin
        J := EditedJunctionProps[I];
        PropertyListbox.Items.Add(JunctionProps[J].Name);
      end;
    end;

  1:  // Storage
    begin
      for I := 0 to High(EditedStorageProps) do
      begin
        J := EditedStorageProps[I];
        PropertyListbox.Items.Add(StorageProps[J].Name);
      end;
    end;

  2:  // Conduits
    begin
      for I := 0 to High(EditedConduitProps) do
      begin
        J := EditedConduitProps[I];
        PropertyListbox.Items.Add(ConduitProps[J].Name);
      end;
    end;
  end;
  PropertyListbox.ItemIndex := 0;
  PropertyListBoxChange(Sender);
end;

procedure TGroupEditForm.TagCheckBoxClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  Enables the Tag edit box when the Tag check box is checked.
//-----------------------------------------------------------------------------
begin
  TagEditBox.Enabled := TagCheckBox.Checked;
end;

procedure TGroupEditForm.PropertyListBoxChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange method for PropertyListBox. Determines what kind of property
//  editing controls are enabled after user selects a particular property
//  to edit.
//-----------------------------------------------------------------------------
var
  K: Integer;
begin
  // Default is to enable the PropertyNumEdit control
  PropertyNumEdit.Text := '';
  PropertyNumEdit.Style := esPosNumber;
  if TEditType(EditTypeListBox.ItemIndex) = etAdd
  then PropertyNumEdit.Style := esNumber;
  PropertyNumEdit.Enabled := True;
  PropertyEditBtn.Visible := False;
  EditTypeListBox.Visible := True;
  ReplaceWithLabel.Visible := False;

  // If Junction class selected,
  if ClassListBox.ItemIndex = 0 then
  begin
    K := EditedJunctionProps[PropertyListBox.ItemIndex];
    if (K = TAG_INDEX) then
    begin
      EditTypeListBox.Visible := False;
      ReplaceWithLabel.Visible := True;
      PropertyNumEdit.Style := esNoSpace;
    end;
  end

  // If Storage class selected
  else if ClassListBox.ItemIndex = 1 then
  begin
    K := EditedStorageProps[PropertyListBox.ItemIndex];
    if (K = TAG_INDEX) then
    begin
      EditTypeListBox.Visible := False;
      ReplaceWithLabel.Visible := True;
      PropertyNumEdit.Style := esNoSpace;
    end;
  end

  // Conduit class selected
  else if ClassListBox.ItemIndex = 2 then
  begin
    K := EditedConduitProps[PropertyListBox.ItemIndex];
    if (K = TAG_INDEX) then
    begin
      EditTypeListBox.Visible := False;
      ReplaceWithLabel.Visible := True;
      PropertyNumEdit.Style := esNoSpace
    end;
  end;
end;

procedure TGroupEditForm.EditTypeListBoxChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange method for EditTypeListBox.
//-----------------------------------------------------------------------------
begin
  case TEditType(EditTypeListBox.ItemIndex) of
    etReplace,
    etMultiply:  PropertyNumEdit.Style := esPosNumber;
    etAdd:       PropertyNumEdit.Style := esNumber;
  end;
end;

procedure TGroupEditForm.PropertyEditBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  Calls a special editing function when the ellipsis button next to
//  the property's NumEdit control is clicked.
//-----------------------------------------------------------------------------
var
  WasEdited: Boolean;
begin
  WasEdited := False;

  // Simulate click of OK button
  if WasEdited then OKBtnClick(Sender);
end;

procedure TGroupEditForm.OKBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the OK button. Calls the appropriate group editing
//  function depending on the class of object being edited.
//-----------------------------------------------------------------------------
var
  EditType: TEditType;
  NewValue: String;
begin
  // Check that a property value was entered.
  if (PropertyListBox.ItemIndex > 0)
  and (Length(Trim(PropertyNumEdit.Text)) = 0) then
  begin
    Uutils.MsgDlg(MSG_NO_DATA, mtError, [mbOK]);
    if PropertyNumEdit.Enabled then ActiveControl := PropertyNumEdit;
  end

  // Call the appropriate group editing function
  else
  begin
    NumObjects := 0;
    if EditTypeListBox.Visible
    then EditType := TEditType(EditTypeListBox.ItemIndex)
    else EditType := etReplace;
    NewValue := PropertyNumEdit.Text;
    try
      case ClassListBox.ItemIndex of
        0: EditJunctions(NewValue, EditType);
        1: EditStorage(NewValue, EditType);
        2: EditConduits(NewValue, EditType);
      end;
    finally
    end;

    // See if user wants to edit some more properties.
    if NumObjects > 0 then HasChanged := True;
    if Uutils.MsgDlg(IntToStr(NumObjects) + ' ' +
       ClassLabels[ClassListBox.ItemIndex] + 's' +
       TXT_WERE_CHANGED, mtConfirmation, [mbYes, mbNo]) = mrNo
    then ModalResult := mrOK;
  end;
end;

procedure TGroupEditForm.EditConduits(const newValue: String;
  const EditType: TEditType);
//-----------------------------------------------------------------------------
//  Group edits a conduit property.
//-----------------------------------------------------------------------------
var
  J: Integer;
  K: Integer;
  L: TLink;
begin
  // Check each conduit
  for J := 0 to Project.Lists[CONDUIT].Count-1 do
  begin
    // See if conduit lies within region being edited
    L := Project.GetLink(CONDUIT, J);
    if ( ObjectQualifies(L.Node1.X, L.Node1.Y, L.Data[TAG_INDEX]) and
         ObjectQualifies(L.Node2.X, L.Node2.Y, L.Data[TAG_INDEX]) ) then
    begin
      K := EditedConduitProps[PropertyListBox.ItemIndex];
      if GetNewValue(L.Data[K], newValue, EditType)
      then Inc(NumObjects);
    end;
  end;

  // Update display of link theme values on the map
  if NumObjects > 0 then Ubrowser.ChangeMapTheme(LINKS, CurrentLinkVar);
end;

procedure TGroupEditForm.EditJunctions(const newValue: String;
  const EditType: TEditType);
//-----------------------------------------------------------------------------
//  Group edits a junction property.
//-----------------------------------------------------------------------------
var
  J: Integer;
  K: Integer;
  N: TNode;
begin
  // Check each junction
  for J := 0 to Project.Lists[JUNCTION].Count-1 do
  begin

    // See if junction lies within region being edited
    N := Project.GetNode(JUNCTION, J);
    if ObjectQualifies(N.X, N.Y, N.Data[TAG_INDEX]) then
    begin
      K := EditedJunctionProps[PropertyListBox.ItemIndex];
      if GetNewValue(N.Data[K], newValue, EditType)
      then Inc(NumObjects);
    end;
  end;

  // Update display of node theme values on the map
  if NumObjects > 0 then Ubrowser.ChangeMapTheme(NODES, CurrentNodeVar);
end;

procedure TGroupEditForm.EditStorage(const newValue: String;
  const EditType: TEditType);
//-----------------------------------------------------------------------------
//  Group edits a storage unit property.
//-----------------------------------------------------------------------------
var
  J: Integer;
  K: Integer;
  N: TNode;
begin
  // Check each storage unit node
  for J := 0 to Project.Lists[STORAGE].Count-1 do
  begin

    // See if node lies within region being edited
    N := Project.GetNode(STORAGE, J);
    if ObjectQualifies(N.X, N.Y, N.Data[TAG_INDEX]) then
    begin
      K := EditedStorageProps[PropertyListBox.ItemIndex];
      if GetNewValue(N.Data[K], newValue, EditType)
      then Inc(NumObjects);
    end;
  end;

  // Update display of node theme values on the map
  if NumObjects > 0 then Ubrowser.ChangeMapTheme(NODES, CurrentNodeVar);
end;

function TGroupEditForm.ObjectQualifies(const X: Extended; const Y: Extended;
           const Tag: String): Boolean;
//-----------------------------------------------------------------------------
//  Checks if object located at point X,Y and with given tag should be edited.
//-----------------------------------------------------------------------------
var
  Xp: Integer;
  Yp: Integer;

begin
  if (TagCheckBox.Checked) and (CompareText(Tag, TagEditBox.Text) <> 0)
  then Result := False

  else if MapForm.AllSelected
  then Result := True

  else
  begin
    Xp := MapForm.Map.GetXpix(X);
    Yp := MapForm.Map.GetYpix(Y);
    if not(PtInRegion(theRegion, Xp, Yp))
    then Result := False
    else Result := True;
  end;
end;

function TGroupEditForm.GetNewValue(var Value1: String; const Value2: String;
  const EditType: TEditType): Boolean;
//-----------------------------------------------------------------------------
//  Applies editing operation of type EditType with value Value2 to Value1.
//-----------------------------------------------------------------------------
var
  X1, X2, Z1: Extended;
begin
  Result := False;
  if EditType = etReplace then Value1 := Value2
  else
  begin
    if not Uutils.GetExtended(Value1, X1) then Exit;
    if not Uutils.GetExtended(Value2, X2) then Exit;
    if EditType = etMultiply then X1 := X2*X1;
    if EditType = etAdd then X1 := X1 + X2;
    Z1 := Abs(X1);
    if      Z1 < 0.01 then Value1 := Format('%0.6f', [X1])
    else if Z1 < 1.0  then Value1 := Format('%0.4f', [X1])
    else if Z1 < 10.0 then Value1 := Format('%.3f', [X1])
    else                   Value1 := Format('%.2f', [X1]);
  end;
  Result := True;
end;

procedure TGroupEditForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 211300);
end;

procedure TGroupEditForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then HelpBtnClick(Sender);
end;

end.

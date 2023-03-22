unit Dcontrol;

{-------------------------------------------------------------------}
{                    Unit:    Dcontrol.pas                          }
{                    Project: ITM                                   }
{                    Version: 2.0                                   }
{                    Date:    03/04/23                              }
{                                                                   }
{   Dialog form unit for editing a link's control properties.       }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  Vcl.ExtCtrls, Uglobals, Uproject, Uutils;

type
  TControlDataForm = class(TForm)
    ControlTypeGroup: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    InitSettingEdit: TSpinEdit;
    Label5: TLabel;
    CloseTimeEdit: TSpinEdit;
    Label6: TLabel;
    Label7: TLabel;
    TseriesCombo: TComboBox;
    NodeCombo: TComboBox;
    CurveCombo: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    CtrlIndex: Integer;
    theLink: TLink;
    OldData: array[0..5] of String;
    function DataValid: Boolean;
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure LoadData(const ObjType: Integer; const Index: Integer);
    function GetControlType: String;
  end;

var
  ControlDataForm: TControlDataForm;

implementation

{$R *.dfm}

procedure TControlDataForm.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  if not DataValid then exit;
  if ControlTypeGroup.ItemIndex = 0 then with theLink do
  begin
    Data[CtrlIndex] := 'NONE';
    for I := 1 to 5 do Data[CtrlIndex+I] := '';
  end
  else if ControlTypeGroup.ItemIndex = 1 then with theLink do
  begin
    Data[CtrlIndex] := 'TIME';
    Data[CtrlIndex+1] := InitSettingEdit.Text;
    Data[CtrlIndex+2] := CloseTimeEdit.Text;
    Data[CtrlIndex+3] := TseriesCombo.Text;
    Data[CtrlIndex+4] := '';
    Data[CtrlIndex+5] := '';
  end
  else if ControlTypeGroup.ItemIndex = 2 then with theLink do
  begin
    Data[CtrlIndex] := 'DEPTH';
    Data[CtrlIndex+1] := InitSettingEdit.Text;
    Data[CtrlIndex+2] := CloseTimeEdit.Text;
    Data[CtrlIndex+3] := '';
    Data[CtrlIndex+4] := NodeCombo.Text;
    Data[CtrlIndex+5] := CurveCombo.Text;
  end;
  for I := 0 to 5 do
    if not SameText(OldData[I], theLink.Data[CtrlIndex+I]) then
      HasChanged := True;
  ModalResult := mrOk;
end;

procedure TControlDataForm.FormCreate(Sender: TObject);
var
  I, J: Integer;
begin
  for I := JUNCTION to STORAGE do
  begin
    for J := 0 to Project.Lists[I].Count-1 do
      NodeCombo.Items.Add(Project.GetID(I, J));
  end;
  NodeCombo.ItemIndex := 0;
  TseriesCombo.Items.Text := Project.Lists[TIMESERIES].Text;
  CurveCombo.Items.Text := Project.Lists[CONTROLCURVE].Text;
end;

function TControlDataForm.DataValid: Boolean;
begin
  Result := False;
  if ControlTypeGroup.ItemIndex = 1 then
  begin
    if Length(Trim(TseriesCombo.Text)) = 0 then
    begin
      Uutils.MsgDlg('A time series name must be supplied.', mtError, [mbOK]);
      TseriesCombo.SetFocus;
      exit;
    end;
  end;
  if ControlTypeGroup.ItemIndex = 2 then
  begin
    if Length(Trim(CurveCombo.Text)) = 0 then
    begin
      Uutils.MsgDlg('A control curve name must be supplied.', mtError, [mbOK]);
      CurveCombo.SetFocus;
      exit;
    end;
  end;
  Result := True;
end;

function TControlDataForm.GetControlType: string;
begin
  with ControlTypeGroup do
    Result := Items[ItemIndex];
end;

procedure TControlDataForm.LoadData(const ObjType: Integer; const Index: Integer);
var
  I: Integer;
begin
  theLink := Project.GetLink(ObjType, Index);
  case ObjType of
  PUMP:    CtrlIndex := PUMP_CONTROL_INDEX;
  ORIFICE: CtrlIndex := ORIFICE_CONTROL_INDEX;
  WEIR:    CtrlIndex := WEIR_CONTROL_INDEX;
  else     exit;
  end;
  ControlTypeGroup.ItemIndex := 0;
  if SameText(theLink.Data[CtrlIndex], 'TIME') then
    ControlTypeGroup.ItemIndex := 1;
  if SameText(theLink.Data[CtrlIndex], 'DEPTH') then
    ControlTypeGroup.ItemIndex := 2;
  if Length(theLink.Data[CtrlIndex+1]) > 0 then
    InitSettingEdit.Text := theLink.Data[CtrlIndex+1];
  if Length(theLink.Data[CtrlIndex+2]) > 0 then
    CloseTimeEdit.Text := theLink.Data[CtrlIndex+2];
  TseriesCombo.Text := theLink.Data[CtrlIndex+3];
  I := NodeCombo.Items.IndexOf(theLink.Data[CtrlIndex+4]);
  if I >= 0 then NodeCombo.ItemIndex := I;
  CurveCombo.Text := theLink.Data[CtrlIndex+5];
  for I := 0 to 5 do OldData[I] := theLink.Data[CtrlIndex+I];
  HasChanged := False;
end;

end.

unit Dinflow;

{-------------------------------------------------------------------}
{                    Unit:    Dinflow.pas                           }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/20/22                              }
{                                                                   }
{   Delphi Pascal form unit that edits the properties of external   }
{   inflows to a sewer node.                                        }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, NumEdit,
  Vcl.ExtCtrls, Uproject, Uglobals;

type
  TInflowsForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    NumEdit1: TNumEdit;
    ComboBox1: TComboBox;
    NumEdit2: TNumEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label5: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure NumEdit1Change(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    HasChanged: Boolean;
    procedure SetData(aNode: TNode);
    procedure GetData(aNode: TNode);
  end;

//var
//  InflowsForm: TInflowsForm;

implementation

{$R *.dfm}

uses
  Uedit;

procedure TInflowsForm.FormCreate(Sender: TObject);
begin
  HasChanged := False;
  ComboBox1.Items := Project.Lists[TIMESERIES];
end;

procedure TInflowsForm.NumEdit1Change(Sender: TObject);
begin
  HasChanged := True;
end;

procedure TInflowsForm.BitBtn1Click(Sender: TObject);
begin
  NumEdit1.Clear;
end;

procedure TInflowsForm.BitBtn2Click(Sender: TObject);
var
  I: Integer;
  S: String;
begin
  // Extract name of time series from combo box & launch Editor
  I := Project.Lists[TIMESERIES].IndexOf(Trim(ComboBox1.Text));
  S := Uedit.EditTimeseries(I);

  // Update name of time series in the combo box
  if Length(S) > 0 then
  begin
    ComboBox1.Text := S;
    ComboBox1.Items := Project.Lists[TIMESERIES];
  end;
end;

procedure TInflowsForm.BitBtn3Click(Sender: TObject);
begin
  ComboBox1.Clear;
end;

procedure TInflowsForm.SetData(aNode: TNode);
begin
  Caption := 'Inflows for Node ' + aNode.ID;
  with aNode.ExInflow do
  begin
    NumEdit1.Text := Baseline;
    ComboBox1.Text := Tseries;
    NumEdit2.Text := ScaleFactor;
  end;
  HasChanged := False;
end;

procedure TInflowsForm.GetData(aNode: TNode);
begin
  with aNode.ExInflow do
  begin
    Baseline := Trim(NumEdit1.Text);
    Tseries := Trim(ComboBox1.Text);
    ScaleFactor := Trim(NumEdit2.Text);
    if (Length(Baseline) = 0) and (Length(Tseries) = 0) then
      FlowType := ''
    else
      FlowType := 'FLOW';
  end;

end;

procedure TInflowsForm.Button3Click(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTEXT, 213760);
end;

end.

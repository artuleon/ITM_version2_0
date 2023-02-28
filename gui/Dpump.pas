unit Dpump;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, NumEdit;

type
  TPumpDataForm = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    NumEdit1: TNumEdit;
    Label3: TLabel;
    NumEdit2: TNumEdit;
    Label4: TLabel;
    ComboBox2: TComboBox;
    Label5: TLabel;
    ComboBox3: TComboBox;
    Label6: TLabel;
    ComboBox4: TComboBox;
    Label7: TLabel;
    ComboBox5: TComboBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    Panel1: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PumpDataForm: TPumpDataForm;

implementation

{$R *.dfm}

end.

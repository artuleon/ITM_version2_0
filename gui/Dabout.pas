unit Dabout;

{-------------------------------------------------------------------}
{                    Unit:    Dabout.pas                            }
{                    Project: ITM                                   }
{                    Version: 1.5.0                                 }
{                    Date:    10/19/22                              }
{                                                                   }
{   Form unit containing the "About" dialog box for EPA SWMM.       }
{-------------------------------------------------------------------}

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls;

type
  TAboutBoxForm = class(TForm)
    Button1: TButton;
    ForeTitle: TLabel;
    Version: TLabel;
    Image1: TImage;
    Label6: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Memo1: TMemo;
    Label12: TLabel;
    Image2: TImage;
    Label13: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//var
//  AboutBoxForm: TAboutBoxForm;

implementation

{$R *.DFM}

end.

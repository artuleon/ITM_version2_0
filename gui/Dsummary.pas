unit Dsummary;

{-------------------------------------------------------------------}
{                    Unit:    Fsummary.pas                          }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/22/22                              }
{                                                                   }
{   Dialog form that lists the number of each type of ITM object    }
{   within the current project.                                     }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, Uproject, Uglobals;

type
  TProjectSummaryForm = class(TForm)
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    TSFcount: Integer;                 //Number of time series inflow nodes
    procedure UpdateNodePropertyCount(N: TNode);
    procedure GetInflowsCount;
  public
    { Public declarations }
  end;

//var
//  ProjectSummaryForm: TProjectSummaryForm;

implementation

{$R *.dfm}

const
  S: array[0..13] of String =
    (
     ' Junction Nodes         ',
     ' Boundary Nodes         ',
     ' Gate Nodes             ',
     ' Weir Nodes             ',
     ' Storage Nodes          ',
     ' Conduit Links          ',
     ' Pump Links             ',
     ' Head Loss Curves       ',
     ' Rating Curves          ',
     ' Storage Curves         ',
     ' Pump Curves            ',
     ' Control Curves         ',
     ' Time Series Inflows    ',
     ' Flow Units             '
     );


procedure TProjectSummaryForm.FormCreate(Sender: TObject);
//-----------------------------------------------------------------------------
// Form's OnCreate handler. Compiles the number of each type of object
// in the current project and displays them in a StringGrid control.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  // Display project info in the string grid
  with StringGrid1 do
  begin
//    ColWidths[1] := ClientWidth - DefaultColWidth;
    DefaultColWidth := ClientWidth div 2;
    RowCount := High(S) + 1;
    for I := Low(S) to High(S) do Cells[0,I] := S[I];
    Cells[1,0] := IntToStr(Project.Lists[JUNCTION].Count);
    Cells[1,1] := IntToStr(Project.Lists[BOUNDARY].Count);
    Cells[1,2] := IntToStr(Project.Lists[GATE].Count);
    Cells[1,3] := IntToStr(Project.Lists[WEIR].Count);
    Cells[1,4] := IntToStr(Project.Lists[STORAGE].Count);
    Cells[1,5] := IntToStr(Project.Lists[CONDUIT].Count);
    Cells[1,6] := IntToStr(Project.GetPumpCount);
    Cells[1,7] := IntToStr(Project.Lists[GATECURVE].Count);
    Cells[1,8] := IntToStr(Project.Lists[RATINGCURVE].Count);
    Cells[1,9] := IntToStr(Project.Lists[STORAGECURVE].Count);
    Cells[1,10] := IntToStr(Project.Lists[PUMPCURVE].Count);
    Cells[1,11] := IntToStr(Project.Lists[CONTROLCURVE].Count);
    GetInflowsCount();
    Cells[1,12] := IntToStr(TSFcount);
    Cells[1,13] := Project.Options.Data[FLOW_UNITS_INDEX];
  end;
end;

procedure TProjectSummaryForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TProjectSummaryForm.GetInflowsCount;
//-----------------------------------------------------------------------------
// Counts the numbers of inflows objects of various types and the
// number of treatment nodes.
//-----------------------------------------------------------------------------
var
  I: Integer;
begin
  TSFcount := 0;

  // Count numbers of nodes with various types of inflows
  with Project.Lists[JUNCTION] do
    for I := 0 to Count-1 do UpdateNodePropertyCount(TNode(Objects[I]));
  with Project.Lists[STORAGE] do
    for I := 0 to Count-1 do UpdateNodePropertyCount(TNode(Objects[I]));
end;

procedure TProjectSummaryForm.UpdateNodePropertyCount(N: TNode);
//-----------------------------------------------------------------------------
// Adds to the count of nodal inflows and treatment if the node N
// has such objects.
//-----------------------------------------------------------------------------
begin
  if Length(N.ExInflow.FlowType) > 0 then Inc(TSFcount);
end;

end.

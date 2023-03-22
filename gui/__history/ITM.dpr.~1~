program ITM;

uses
  Windows,
  Forms,
  Dialogs,
  SysUtils,
  Dabout in 'Dabout.pas' {AboutBoxForm},
  Fmap in 'Fmap.pas' {MapForm},
  Uglobals in 'Uglobals.pas',
  Uutils in 'Uutils.pas',
  Fproped in 'Fproped.pas' {PropEditForm},
  PropEdit in 'PropEdit.pas',
  Dprefers in 'Dprefers.pas' {PreferencesForm},
  Umap in 'Umap.pas',
  Dmapdim in 'Dmapdim.pas' {MapDimensionsForm},
  Dlabel in 'Dlabel.pas' {LabelForm},
  Uinifile in 'Uinifile.pas',
  Dlegend in 'Dlegend.pas' {LegendForm},
  Dcolramp in 'Dcolramp.pas' {ColorRampForm},
  Uproject in 'Uproject.pas',
  Uedit in 'Uedit.pas',
  Dcurve in 'Dcurve.pas' {CurveDataForm},
  Uexport in 'Uexport.pas',
  Dmap in 'Dmap.pas' {MapOptionsForm},
  Dtseries in 'Dtseries.pas' {TimeseriesForm},
  Uvertex in 'Uvertex.pas',
  Uoutput in 'Uoutput.pas',
  Ulegend in 'Ulegend.pas',
  Fstatus in 'Fstatus.pas' {StatusForm},
  Dreport in 'Dreport.pas' {ReportSelectForm},
  Fgraph in 'Fgraph.pas' {GraphForm},
  Dcopy in 'Dcopy.pas' {CopyToForm},
  Ubrowser in 'Ubrowser.pas',
  Fovmap in 'Fovmap.pas' {OVMapForm},
  Ddefault in 'Ddefault.pas' {DefaultsForm},
  Fmain in 'Fmain.pas' {MainForm},
  Uimport in 'Uimport.pas',
  Dfind in 'Dfind.pas' {FindForm},
  Udxf in 'Udxf.pas',
  Dgrouped in 'Dgrouped.pas' {GroupEditForm},
  Dquery in 'Dquery.pas' {QueryForm},
  Doptions in 'Doptions.pas' {AnalysisOptionsForm},
  Ftable in 'Ftable.pas' {TableForm},
  Fproplot in 'Fproplot.pas' {ProfilePlotForm},
  Dproplot in 'Dproplot.pas' {ProfilePlotOptionsForm},
  Dprofile in 'Dprofile.pas' {ProfileSelectionForm},
  Dgrpdel in 'Dgrpdel.pas' {GroupDeleteForm},
  Fsimul in 'Fsimul.pas' {SimulationForm},
  Fstats in 'Fstats.pas' {StatsReportForm},
  Ugraph in 'Ugraph.pas',
  Dprevplot in 'Dprevplot.pas' {PreviewPlotForm},
  Dstats in 'Dstats.pas' {StatsSelectForm},
  GridEdit in 'GridEdit.pas' {GridEditFrame: TFrame},
  Dsummary in 'Dsummary.pas' {ProjectSummaryForm},
  Uupdate in 'Uupdate.pas',
  Uvalidate in 'Uvalidate.pas',
  Uclipbrd in 'Uclipbrd.pas',
  Dnotes in 'Dnotes.pas' {NotesEditorForm},
  Dbackdrp in 'Dbackdrp.pas' {BackdropFileForm},
  Dbackdim in 'Dbackdim.pas' {BackdropDimensionsForm},
  Ucoords in 'Ucoords.pas',
  UpDnEdit in 'UpDnEdit.pas' {UpDnEditBox: TFrame},
  Dchart in 'Dchart.pas' {ChartOptionsDlg},
  Dproject in 'Dproject.pas' {ProjectForm},
  Dtimeplot in 'Dtimeplot.pas' {TimePlotForm},
  Dproselect in 'Dproselect.pas' {ProfileSelectForm},
  Vcl.Themes,
  Vcl.Styles,
  Animator in 'Animator.pas' {AnimatorFrame: TFrame},
  Ustats in 'Ustats.pas',
  itm_api in 'itm_api.pas',
  Dinflow in 'Dinflow.pas' {InflowsForm},
  itm_export in 'itm_export.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.MainFormOnTaskBar := True; // added for MDI app
  Application.Title := 'ITM 1.5';
  Application.HelpFile := '';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFindForm, FindForm);
  Application.CreateForm(TOVMapForm, OVMapForm);
  Application.CreateForm(TReportSelectForm, ReportSelectForm);
  Application.CreateForm(TTimePlotForm, TimePlotForm);
  Application.CreateForm(TQueryForm, QueryForm);
  Application.Run;
end.

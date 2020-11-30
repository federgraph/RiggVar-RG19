program RG19B;

uses
  Vcl.Forms,
  FrmRG19B in 'App\FrmRG19B.pas' {FormRG19B},
  FrmRegler in 'Frm\FrmRegler.pas' {CtrlDlg},
  FrmReport in 'Frm\FrmReport.pas' {ReportForm},
  FrmSelect in 'Frm\FrmSelect.pas' {MemoDlg},
  FrmOptions in 'Frm\FrmOptions.pas' {OptionForm},
  FrmPreview in 'Frm\FrmPreview.pas' {PreviewGForm},
  FrmReglerGraph in 'Frm\FrmReglerGraph.pas' {CtrlDlg1},
  FrmBiege in 'Frm\FrmBiege.pas' {BiegeUndNeigeForm},
  FrmInput in 'Frm\FrmInput.pas' {InputForm},
  FrmOutput in 'Frm\FrmOutput.pas' {OutputForm},
  FrmGrafik in 'Frm\FrmGrafik.pas' {GrafikForm},
  FrmRot in 'Frm\FrmRot.pas' {RotationForm},
  FrmConsole in 'Frm\FrmConsole.pas' {ConsoleForm},
  FrmChart in 'Frm\FrmChart.pas' {ChartForm},
  FrmAuswahl in 'Frm\FrmAuswahl.pas' {YAuswahlDlg},
  FrmProgress in 'Frm\FrmProgress.pas' {ProgressDlg},
  FrmInfo in 'Frm\FrmInfo.pas' {KreisForm},
  FrmAni in 'Frm\FrmAni.pas' {AnimationForm},
  FrmAniRot in 'Frm\FrmAniRot.pas' {AniRotationForm},
  FrmModel in 'Frm\FrmModel.pas' {RiggDialog},
  FrmCmd in 'Frm\FrmCmd.pas' {CommandForm},
  FrmAdjust in 'Frm\FrmAdjust.pas' {AdjustForm},
  FrmScale in 'Frm\FrmScale.pas' {RumpfFaktorDlg},
  FrmKreis in 'Frm\FrmKreis.pas' {Kreisform},
  RggDoc in 'Core\RggDoc.pas',
  RggFachwerk in 'Core\RggFachwerk.pas',
  RggReport in 'Core\RggReport.pas',
  RggTypes in 'Core\RggTypes.pas',
  RggModul in 'Core\RggModul.pas',
  RggTrimmTab in 'Core\RggTrimmTab.pas',
  RggSaling3Eck in 'Core\RggSaling3Eck.pas',
  RggSchnittKK in 'Core\RggSchnittKK.pas',
  RggCalc in 'Core\RggCalc.pas',
  RggScroll in 'Core\RggScroll.pas',
  RggCtrls in 'Graph\RggCtrls.pas',
  RggMatrix in 'Graph\RggMatrix.pas',
  RggGetriebeGraph in 'Graph\RggGetriebeGraph.pas',
  RggPBox in 'Graph\RggPBox.pas',
  RggHull in 'Graph\RggHull.pas',
  RggPal in 'Graph\RggPal.pas',
  RggPreview in 'Graph\RggPreview.pas',
  RggPrinter in 'Graph\RggPrinter.pas',
  RggGraph in 'Graph\RggGraph.pas',
  RggPolarKar in 'Graph\RggPolarKar.pas',
  RggBootGraph in 'Graph\RggBootGraph.pas',
  RggRaumGraph in 'Graph\RggRaumGraph.pas',
  RggRota in 'Graph\RggRota.pas',
  RggDisplay in 'Graph\RggDisplay.pas',
  RggTestData in 'Core\RggTestData.pas',
  RggStrings in 'Core\RggStrings.pas',
  RiggVar.App.Main in 'App\RiggVar.App.Main.pas',
  RiggVar.Util.AppUtils in 'Util\RiggVar.Util.AppUtils.pas',
  RiggVar.Util.InfoMemo in 'Util\RiggVar.Util.InfoMemo.pas',
  RiggVar.Util.Logger in 'Util\RiggVar.Util.Logger.pas',
  RiggVar.VM.FormMain in 'VM\RiggVar.VM.FormMain.pas',
  RiggVar.VM.FormMainB in 'VM\RiggVar.VM.FormMainB.pas',
  RiggVar.FB.Classes in 'FB\RiggVar.FB.Classes.pas',
  RiggVar.FB.ActionConst in 'FB\RiggVar.FB.ActionConst.pas',
  RiggVar.FB.Color in 'FB\RiggVar.FB.Color.pas',
  RiggVar.RG.Main in 'RG\RiggVar.RG.Main.pas',
  RiggVar.RG.Def in 'RG\RiggVar.RG.Def.pas',
  RiggVar.RG.Track in 'RG\RiggVar.RG.Track.pas',
  RiggVar.RG.Graph in 'RG\RiggVar.RG.Graph.pas',
  RiggVar.RG.Report in 'RG\RiggVar.RG.Report.pas',
  RiggVar.RG.Data in 'RG\RiggVar.RG.Data.pas',
  RggTransformer in 'Graph\RggTransformer.pas',
  RggChartModel in 'Chart\RggChartModel.pas',
  RggChartModel01 in 'Chart\RggChartModel01.pas',
  RggChartModel02 in 'Chart\RggChartModel02.pas',
  RggZug in 'Graph\RggZug.pas',
  RggZug2D in 'Graph\RggZug2D.pas',
  RggZug3D in 'Graph\RggZug3D.pas',
  RggDisplayOrder in 'Graph\RggDisplayOrder.pas',
  RggDisplayTypes in 'Graph\RggDisplayTypes.pas',
  RggTrimmTabGraph in 'Graph\RggTrimmTabGraph.pas',
  RggKraftGraph in 'Graph\RggKraftGraph.pas',
  RggMastGraph in 'Graph\RggMastGraph.pas',
  RiggVar.FD.Point in 'Graph2\RiggVar.FD.Point.pas',
  RggInter in 'Core\RggInter.pas',
  RiggVar.RG.Model in 'RG\RiggVar.RG.Model.pas',
  RiggVar.App.Model in 'App\RiggVar.App.Model.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'RG19B';
  Application.CreateForm(TFormRG19B, FormRG19B);
  Application.CreateForm(TBiegeUndNeigeForm, BiegeUndNeigeForm);
  Application.CreateForm(TFormRegler, FormRegler);
  Application.CreateForm(TFormReglerGraph, FormReglerGraph);
  Application.CreateForm(TMemoDlg, MemoDlg);
  Application.CreateForm(TOptionForm, OptionForm);
  Application.CreateForm(TPreviewGForm, PreviewGForm);
  Application.CreateForm(TYAuswahlDlg, YAuswahlDlg);
  Application.Run;
end.

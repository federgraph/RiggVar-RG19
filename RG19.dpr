program RG19;

uses
  Vcl.Forms,
  FrmMain in 'App\FrmMain.pas' {FormMain},
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
  FrmIndicator in 'Frm\FrmIndicator.pas' {IndicatorForm},
  FrmKreis in 'Frm\FrmKreis.pas' {KreisForm},
  ThreeD in 'Graph\ThreeD.pas',
  ThreeDSolid in 'Graph\ThreeDSolid.pas',
  RggDoc in 'Core\RggDoc.pas',
  RggFachwerk in 'Core\RggFachwerk.pas',
  RggModul in 'Core\RggModul.pas',
  RggReport in 'Core\RggReport.pas',
  RggUnit0 in 'Core\RggUnit0.pas',
  RggUnit1 in 'Core\RggUnit1.pas',
  RggUnit2 in 'Core\RggUnit2.pas',
  RggUnit3 in 'Core\RggUnit3.pas',
  RggUnit4 in 'Core\RggUnit4.pas',
  RggSchnittKK in 'Core\RggSchnittKK.pas',
  RggSaling3Eck in 'Core\RggSaling3Eck.pas',
  RggTrimmTab in 'Core\RggTrimmTab.pas',
  RggTypes in 'Core\RggTypes.pas',
  RggCalc in 'Core\RggCalc.pas',
  RggKraftGraph in 'Graph\RggKraftGraph.pas',
  RggCtrls in 'Graph\RggCtrls.pas',
  RggMatrix in 'Graph\RggMatrix.pas',
  RggScroll in 'Core\RggScroll.pas',
  RggGetriebeGraph in 'Graph\RggGetriebeGraph.pas',
  RggPBox in 'Graph\RggPBox.pas',
  RggHull in 'Graph\RggHull.pas',
  RggPal in 'Graph\RggPal.pas',
  RggRota in 'Graph\RggRota.pas',
  RggPrinter in 'Graph\RggPrinter.pas',
  RggPreview in 'Graph\RggPreview.pas',
  RggGraph in 'Graph\RggGraph.pas',
  RggPolarkar in 'Graph\RggPolarkar.pas',
  RggBootGraph in 'Graph\RggBootGraph.pas',
  RggRaumGraph in 'Graph\RggRaumGraph.pas',
  RggDisplay in 'Graph\RggDisplay.pas',
  RggTestData in 'Graph\RggTestData.pas',
  RggChart in 'Graph\RggChart.pas',
  RiggVar.App.Main in 'App\RiggVar.App.Main.pas',
  RiggVar.App.Main0 in 'App\RiggVar.App.Main0.pas',
  RiggVar.App.Main1 in 'App\RiggVar.App.Main1.pas',
  RiggVar.FB.Classes in 'FB\RiggVar.FB.Classes.pas',
  RiggVar.FB.Color in 'FB\RiggVar.FB.Color.pas',
  RiggVar.RG.Data in 'RG\RiggVar.RG.Data.pas',
  RiggVar.RG.Main in 'RG\RiggVar.RG.Main.pas',
  RiggVar.RG.Def in 'RG\RiggVar.RG.Def.pas',
  RiggVar.RG.Track in 'RG\RiggVar.RG.Track.pas',
  RiggVar.RG.Graph in 'RG\RiggVar.RG.Graph.pas',
  RiggVar.RG.Report in 'RG\RiggVar.RG.Report.pas',
  RiggVar.Util.AppUtils in 'Util\RiggVar.Util.AppUtils.pas',
  RiggVar.Util.InfoMemo in 'Util\RiggVar.Util.InfoMemo.pas',
  RiggVar.Util.Logger in 'Util\RiggVar.Util.Logger.pas',
  RiggVar.VM.FormMain in 'VM\RiggVar.VM.FormMain.pas',
  RiggVar.VM.FormMainD in 'VM\RiggVar.VM.FormMainD.pas',
  RggZug2D in 'Graph\RggZug2D.pas',
  RggTransformer in 'Graph\RggTransformer.pas',
  RggZug3D in 'Graph\RggZug3D.pas',
  RggZug in 'Graph\RggZug.pas',
  RggDisplayOrder in 'Graph\RggDisplayOrder.pas',
  RggDisplayTypes in 'Graph\RggDisplayTypes.pas',
  RiggVar.FB.ActionConst in 'FB\RiggVar.FB.ActionConst.pas',
  RggVector in 'Graph\RggVector.pas',
  RggChartModel01 in 'Graph\RggChartModel01.pas',
  RggChartModel02 in 'Graph\RggChartModel02.pas',
  RggStrings in 'Core\RggStrings.pas',
  RggTrimmTabGraph in 'Graph\RggTrimmTabGraph.pas',
  RggMastGraph in 'Graph\RggMastGraph.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'RG19';
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TBiegeUndNeigeForm, BiegeUndNeigeForm);
  Application.CreateForm(TFormRegler, FormRegler);
  Application.CreateForm(TFormReglerGraph, FormReglerGraph);
  Application.CreateForm(TMemoDlg, MemoDlg);
  Application.CreateForm(TOptionForm, OptionForm);
  Application.CreateForm(TPreviewGForm, PreviewGForm);
  Application.CreateForm(TYAuswahlDlg, YAuswahlDlg);
  Application.Run;
end.

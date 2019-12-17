program RG19;

uses
  Vcl.Forms,
  FrmMain in 'App\FrmMain.pas' {FormMain},
  FrmRegler in 'Frm\FrmRegler.pas' {CtrlDlg},
  FrmReport in 'Frm\FrmReport.pas' {ReportForm},
  FrmSelect in 'Frm\FrmSelect.pas' {MemoDlg},
  FrmOptions in 'Frm\FrmOptions.pas' {OptionForm},
  RggTypes in 'Core\RggTypes.pas',
  RggUnit2 in 'Core\RggUnit2.pas',
  RggUnit0 in 'Core\RggUnit0.pas',
  Rggunit1 in 'Core\Rggunit1.pas',
  Rggunit3 in 'Core\Rggunit3.pas',
  FwUnit in 'Core\FwUnit.pas',
  Rggunit4 in 'Core\Rggunit4.pas',
  RggCtrls in 'Graph\RggCtrls.pas',
  FrmPreview in 'Frm\FrmPreview.pas' {PreviewGForm},
  Iotypes in 'Core\Iotypes.pas',
  FrmReglerGraph in 'Frm\FrmReglerGraph.pas' {CtrlDlg1},
  FrmBiege in 'Frm\FrmBiege.pas' {BiegeUndNeigeForm},
  Rggmat01 in 'Graph\Rggmat01.pas',
  FrmInput in 'Frm\FrmInput.pas' {InputForm},
  FrmOutput in 'Frm\FrmOutput.pas' {OutputForm},
  FrmGrafic in 'Frm\FrmGrafic.pas' {GrafikForm},
  RiggUnit in 'Core\RiggUnit.pas',
  FrmRot in 'Frm\FrmRot.pas' {RotationForm},
  FrmConsole in 'Frm\FrmConsole.pas' {ConsoleForm},
  FrmChart in 'Frm\FrmChart.pas' {ChartForm},
  FrmAuswahl in 'Frm\FrmAuswahl.pas' {YAuswahlDlg},
  FrmProgress in 'Frm\FrmProgress.pas' {ProgressDlg},
  FrmChartRgg in 'Frm\FrmChartRgg.pas' {ChartFormGS},
  TrimmTab in 'Core\TrimmTab.pas',
  FrmInfo in 'Frm\FrmInfo.pas' {KreisForm},
  FrmMemo in 'Frm\FrmMemo.pas' {MemoFormC},
  Rggdoc in 'Core\Rggdoc.pas',
  RggGbox in 'Graph\RggGbox.pas',
  RggHull in 'Graph\RggHull.pas',
  RggPal in 'Graph\RggPal.pas',
  FrmAni in 'Frm\FrmAni.pas' {AnimationForm},
  FrmAniRot in 'Frm\FrmAniRot.pas' {AniRotationForm},
  FrmModel in 'Frm\FrmModel.pas' {RiggDialog},
  Print004 in 'Util\Print004.pas',
  FrmCmd in 'Frm\FrmCmd.pas' {CommandForm},
  FrmAdjust in 'Frm\FrmAdjust.pas' {AdjustForm},
  uRggPrinter in 'Graph\uRggPrinter.pas',
  Vector3d in 'Graph\Vector3d.pas',
  StrBox in 'Util\StrBox.pas',
  Saling3Eck in 'Core\Saling3Eck.pas',
  FrmScale in 'Frm\FrmScale.pas' {RumpfFaktorDlg},
  ThreeD in 'Graph\ThreeD.pas',
  ThreeDSolid in 'Graph\ThreeDSolid.pas',
  RggPBox in 'Graph\RggPBox.pas',
  FrmIndicator in 'Frm\FrmIndicator.pas' {IndicatorForm},
  Schnttkk in 'Core\Schnttkk.pas',
  Vcalc116 in 'Core\Vcalc116.pas',
  RggGraph in 'Graph\RggGraph.pas',
  Polarkar in 'Graph\Polarkar.pas',
  BootGraph in 'Graph\BootGraph.pas',
  RaumGraph in 'Graph\RaumGraph.pas',
  RiggVar.InfoMemo in 'Util\RiggVar.InfoMemo.pas',
  FrmKreis in 'Frm\FrmKreis.pas',
  RiggVar.App.Main in 'App\RiggVar.App.Main.pas',
  RiggVar.Util.Logger in 'Util\RiggVar.Util.Logger.pas',
  RggScroll in 'Core\RggScroll.pas',
  RiggVar.FB.Classes in 'FB\RiggVar.FB.Classes.pas',
  RiggVar.RG.Data in 'RG\RiggVar.RG.Data.pas',
  RiggVar.App.Main0 in 'App\RiggVar.App.Main0.pas',
  RiggVar.App.Main1 in 'App\RiggVar.App.Main1.pas',
  RiggVar.Util.AppUtils in 'Util\RiggVar.Util.AppUtils.pas',
  RiggVar.RG.Main in 'RG\RiggVar.RG.Main.pas',
  RiggVar.RG.Def in 'RG\RiggVar.RG.Def.pas',
  RggKraft in 'Graph\RggKraft.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Rigg';
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TBiegeUndNeigeForm, BiegeUndNeigeForm);
  Application.CreateForm(TCtrlDlg, CtrlDlg);
  Application.CreateForm(TCtrlDlg1, CtrlDlg1);
  Application.CreateForm(TMemoDlg, MemoDlg);
  Application.CreateForm(TOptionForm, OptionForm);
  Application.CreateForm(TPreviewGForm, PreviewGForm);
  Application.CreateForm(TYAuswahlDlg, YAuswahlDlg);
  Application.CreateForm(TConsoleForm, ConsoleForm);
  Application.Run;
end.

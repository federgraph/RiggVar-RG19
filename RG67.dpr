﻿program RG67;

uses
  Vcl.Forms,
  FrmMain in 'App\FrmMain.pas' {FormMain},
  FrmAction in 'App\FrmAction.pas' {FormAction},
  FrmMemo in 'App\FrmMemo.pas' {FormMemo},
  FrmDrawing in 'App\FrmDrawing.pas' {FormDrawing},
  FrmTrimmTab in 'Frm\FrmTrimmTab.pas' {FormTrimmTab},
  FrmDiagramC in 'Frm\FrmDiagramC.pas' {FormDiagramC},
  FrmAuswahl in 'Frm\FrmAuswahl.pas' {YAuswahlDlg},
  FrmDiagramQ in 'Frm\FrmDiagramQ.pas' {FormDiagramQ},
  FrmConfig in 'Frm\FrmConfig.pas' {FormConfig},
  FrmDiagramE in 'Frm\FrmDiagramE.pas' {FormDiagramE},
  FrmInfo in 'Frm\FrmInfo.pas' {FormInfo},
  FrmKreis in 'Frm\FrmKreis.pas' {KreisForm},
  RiggVar.App.Main in 'App\RiggVar.App.Main.pas',
  RiggVar.App.Model in 'App\RiggVar.App.Model.pas',
  RiggVar.App.Strings in 'App\RiggVar.App.Strings.pas',
  RiggVar.RG.Calc in 'RG\RiggVar.RG.Calc.pas',
  RiggVar.RG.Doc in 'RG\RiggVar.RG.Doc.pas',
  RiggVar.RG.Fachwerk in 'RG\RiggVar.RG.Fachwerk.pas',
  RiggVar.RG.Inter in 'RG\RiggVar.RG.Inter.pas',
  RiggVar.RG.Scroll in 'RG\RiggVar.RG.Scroll.pas',
  RiggVar.RG.TrimmTab in 'RG\RiggVar.RG.TrimmTab.pas',
  RiggVar.RG.Types in 'RG\RiggVar.RG.Types.pas',
  RiggVar.RG.Strings in 'RG\RiggVar.RG.Strings.pas',
  RiggVar.Chart.Model in 'Chart\RiggVar.Chart.Model.pas',
  RiggVar.Chart.Model01 in 'Chart\RiggVar.Chart.Model01.pas',
  RiggVar.Chart.Graph in 'Chart\RiggVar.Chart.Graph.pas',
  RiggVar.Chart.Diagram in 'Chart\RiggVar.Chart.Diagram.pas',
  RiggVar.DT.Ctrls in 'Detail\RiggVar.DT.Ctrls.pas',
  RiggVar.DT.Profile in 'Detail\RiggVar.DT.Profile.pas',
  RiggVar.DT.TrimmTabGraph in 'Detail\RiggVar.DT.TrimmTabGraph.pas',
  RggUnit0 in 'Core\RggUnit0.pas',
  RggUnit1 in 'Core\RggUnit1.pas',
  RggUnit2 in 'Core\RggUnit2.pas',
  RggUnit3 in 'Core\RggUnit3.pas',
  RggUnit4 in 'Core\RggUnit4.pas',
  RiggVar.FB.Action in 'FB\RiggVar.FB.Action.pas',
  RiggVar.FB.ActionConst in 'FB\RiggVar.FB.ActionConst.pas',
  RiggVar.FB.Classes in 'FB\RiggVar.FB.Classes.pas',
  RiggVar.FB.Color in 'FB\RiggVar.FB.Color.pas',
  RiggVar.FB.ActionGroup in 'FB\RiggVar.FB.ActionGroup.pas',
  RiggVar.FB.ActionGroups in 'FB\RiggVar.FB.ActionGroups.pas',
  RiggVar.FB.ActionKeys in 'FB\RiggVar.FB.ActionKeys.pas',
  RiggVar.FB.ActionLong in 'FB\RiggVar.FB.ActionLong.pas',
  RiggVar.FB.ActionMap in 'FB\RiggVar.FB.ActionMap.pas',
  RiggVar.FB.ActionName in 'FB\RiggVar.FB.ActionName.pas',
  RiggVar.FB.ActionShort in 'FB\RiggVar.FB.ActionShort.pas',
  RiggVar.FB.ActionTable in 'FB\RiggVar.FB.ActionTable.pas',
  RiggVar.FB.ActionTest in 'FB\RiggVar.FB.ActionTest.pas',
  RiggVar.FB.Scheme in 'FB\RiggVar.FB.Scheme.pas',
  RiggVar.FB.SpeedBar in 'FB\RiggVar.FB.SpeedBar.pas',
  RiggVar.FB.SpeedColor in 'FB\RiggVar.FB.SpeedColor.pas',
  RiggVar.FB.TextBase in 'FB\RiggVar.FB.TextBase.pas',
  RiggVar.FederModel.Action in 'Model\RiggVar.FederModel.Action.pas',
  RiggVar.FederModel.ActionList in 'Model\RiggVar.FederModel.ActionList.pas',
  RiggVar.FederModel.ActionMapPhone in 'Model\RiggVar.FederModel.ActionMapPhone.pas',
  RiggVar.FederModel.ActionMapTablet in 'Model\RiggVar.FederModel.ActionMapTablet.pas',
  RiggVar.FederModel.Binding in 'Model\RiggVar.FederModel.Binding.pas',
  RiggVar.FederModel.Circle in 'Model\RiggVar.FederModel.Circle.pas',
  RiggVar.FederModel.Keyboard01 in 'Model\RiggVar.FederModel.Keyboard01.pas',
  RiggVar.FederModel.Menu in 'Model\RiggVar.FederModel.Menu.pas',
  RiggVar.FederModel.Touch in 'Model\RiggVar.FederModel.Touch.pas',
  RiggVar.FederModel.TouchBase in 'Model\RiggVar.FederModel.TouchBase.pas',
  RiggVar.FederModel.TouchPhone in 'Model\RiggVar.FederModel.TouchPhone.pas',
  RiggVar.RG.Data in 'RG\RiggVar.RG.Data.pas',
  RiggVar.RG.Def in 'RG\RiggVar.RG.Def.pas',
  RiggVar.RG.Graph in 'RG\RiggVar.RG.Graph.pas',
  RiggVar.RG.Main in 'RG\RiggVar.RG.Main.pas',
  RiggVar.RG.Model in 'RG\RiggVar.RG.Model.pas',
  RiggVar.RG.Report in 'RG\RiggVar.RG.Report.pas',
  RiggVar.RG.Rota in 'RG\RiggVar.RG.Rota.pas',
  RiggVar.RG.Speed01 in 'RG\RiggVar.RG.Speed01.pas',
  RiggVar.RG.Speed02 in 'RG\RiggVar.RG.Speed02.pas',
  RiggVar.RG.Speed03 in 'RG\RiggVar.RG.Speed03.pas',
  RiggVar.RG.Speed04 in 'RG\RiggVar.RG.Speed04.pas',
  RiggVar.RG.Track in 'RG\RiggVar.RG.Track.pas',
  RiggVar.RG.View in 'RG\RiggVar.RG.View.pas',
  RiggVar.Graph1.DisplayList in 'Graph1\RiggVar.Graph1.DisplayList.pas',
  RiggVar.Graph1.DisplayOrder in 'Graph1\RiggVar.Graph1.DisplayOrder.pas',
  RiggVar.Graph1.DisplayTypes in 'Graph1\RiggVar.Graph1.DisplayTypes.pas',
  RiggVar.Graph1.Hull in 'Graph1\RiggVar.Graph1.Hull.pas',
  RiggVar.Graph1.Rigg in 'Graph1\RiggVar.Graph1.Rigg.pas',
  RiggVar.Graph1.Rota in 'Graph1\RiggVar.Graph1.Rota.pas',
  RiggVar.Graph1.Transform in 'Graph1\RiggVar.Graph1.Transform.pas',
  RiggVar.Util.AppUtils in 'Util\RiggVar.Util.AppUtils.pas',
  RiggVar.Util.Logger in 'Util\RiggVar.Util.Logger.pas',
  RiggVar.Util.InfoMemo in 'Util\RiggVar.Util.InfoMemo.pas',
  RiggVar.FD.Chart in 'Graph2\RiggVar.FD.Chart.pas',
  RiggVar.FD.Elements in 'Graph2\RiggVar.FD.Elements.pas',
  RiggVar.FD.Drawings in 'Graph2\RiggVar.FD.Drawings.pas',
  RiggVar.FD.Drawing00 in 'Graph2\RiggVar.FD.Drawing00.pas',
  RiggVar.FD.Rota in 'Graph2\RiggVar.FD.Rota.pas',
  RiggVar.FD.RotationHelper in 'Graph2\RiggVar.FD.RotationHelper.pas',
  RiggVar.FD.Point in 'Graph2\RiggVar.FD.Point.pas',
  RiggVar.FD.SchnittKK in 'Graph2\RiggVar.FD.SchnittKK.pas',
  RiggVar.FD.TransformHelper in 'Graph2\RiggVar.FD.TransformHelper.pas',
  RiggVar.FD.Drawing01 in 'FD\RiggVar.FD.Drawing01.pas',
  RiggVar.FD.Drawing02 in 'FD\RiggVar.FD.Drawing02.pas',
  RiggVar.FD.Drawing03 in 'FD\RiggVar.FD.Drawing03.pas',
  RiggVar.FD.Drawing04 in 'FD\RiggVar.FD.Drawing04.pas',
  RiggVar.FD.Drawing05 in 'FD\RiggVar.FD.Drawing05.pas',
  RiggVar.FD.Drawing06 in 'FD\RiggVar.FD.Drawing06.pas',
  RiggVar.FD.Drawing07 in 'FD\RiggVar.FD.Drawing07.pas',
  RiggVar.FD.Drawing08 in 'FD\RiggVar.FD.Drawing08.pas',
  RiggVar.FD.Drawing09 in 'FD\RiggVar.FD.Drawing09.pas',
  RiggVar.FD.Drawing10 in 'FD\RiggVar.FD.Drawing10.pas',
  RiggVar.FD.Drawing11 in 'FD\RiggVar.FD.Drawing11.pas',
  RiggVar.FD.Drawing12 in 'FD\RiggVar.FD.Drawing12.pas',
  RiggVar.FD.Registry in 'FD\RiggVar.FD.Registry.pas',
  RiggVar.FZ.Registry in 'FZ\RiggVar.FZ.Registry.pas',
  RiggVar.FZ.Z01_Viereck in 'FZ\RiggVar.FZ.Z01_Viereck.pas',
  RiggVar.FZ.Z02_Logo in 'FZ\RiggVar.FZ.Z02_Logo.pas',
  RiggVar.FZ.Z03_Viergelenk in 'FZ\RiggVar.FZ.Z03_Viergelenk.pas',
  RiggVar.FZ.Z04_Tetraeder in 'FZ\RiggVar.FZ.Z04_Tetraeder.pas',
  RiggVar.FZ.Z05_TestRigg in 'FZ\RiggVar.FZ.Z05_TestRigg.pas',
  RiggVar.FZ.Z06_Hoehe in 'FZ\RiggVar.FZ.Z06_Hoehe.pas',
  RiggVar.FZ.Z07_Triangle in 'FZ\RiggVar.FZ.Z07_Triangle.pas',
  RiggVar.FZ.Z08_Arc in 'FZ\RiggVar.FZ.Z08_Arc.pas',
  RiggVar.FZ.Z09_Axis in 'FZ\RiggVar.FZ.Z09_Axis.pas',
  RiggVar.FZ.Z10_Lager in 'FZ\RiggVar.FZ.Z10_Lager.pas',
  RiggVar.FZ.Z11_Above in 'FZ\RiggVar.FZ.Z11_Above.pas',
  RiggVar.FZ.Z12_Atan2 in 'FZ\RiggVar.FZ.Z12_Atan2.pas',
  RiggVar.FZ.Z13_SchnittKK in 'FZ\RiggVar.FZ.Z13_SchnittKK.pas',
  RiggVar.FZ.Z14_SplitF in 'FZ\RiggVar.FZ.Z14_SplitF.pas',
  RiggVar.FZ.Z15_SchnittGG in 'FZ\RiggVar.FZ.Z15_SchnittGG.pas',
  RiggVar.FZ.Z16_Shrink in 'FZ\RiggVar.FZ.Z16_Shrink.pas',
  RiggVar.FZ.Z17_Feder in 'FZ\RiggVar.FZ.Z17_Feder.pas',
  RiggVar.FZ.Z18_BerechneWinkel in 'FZ\RiggVar.FZ.Z18_BerechneWinkel.pas',
  RiggVar.FZ.Z19_Chart in 'FZ\RiggVar.FZ.Z19_Chart.pas',
  RiggVar.FZ.Z20_Epsilon in 'FZ\RiggVar.FZ.Z20_Epsilon.pas',
  RiggVar.FZ.Z21_Rotations in 'FZ\RiggVar.FZ.Z21_Rotations.pas',
  RiggVar.FZ.Z22_BigArc in 'FZ\RiggVar.FZ.Z22_BigArc.pas',
  RiggVar.FZ.Z23_Federgraph in 'FZ\RiggVar.FZ.Z23_Federgraph.pas',
  RiggVar.RG.LocalizedStrings in 'RG\RiggVar.RG.LocalizedStrings.pas',
  RiggVar.RG.LocalizedStrings00 in 'RG\RiggVar.RG.LocalizedStrings00.pas',
  RiggVar.RG.LocalizedStringsDE in 'RG\RiggVar.RG.LocalizedStringsDE.pas',
  RiggVar.RG.LocalizedStringsEN in 'RG\RiggVar.RG.LocalizedStringsEN.pas',
  RiggVar.FB.ActionLongDE in 'FB\RiggVar.FB.ActionLongDE.pas',
  RiggVar.FB.ActionLongEN in 'FB\RiggVar.FB.ActionLongEN.pas',
  RiggVar.FB.ActionShortDE in 'FB\RiggVar.FB.ActionShortDE.pas',
  RiggVar.FB.ActionShortEN in 'FB\RiggVar.FB.ActionShortEN.pas',
  RiggVar.FB.Equation in 'FB\RiggVar.FB.Equation.pas',
  RiggVar.FB.Formula in 'FB\RiggVar.FB.Formula.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'RG67';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

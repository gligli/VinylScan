program curfit;

{$MODE Delphi}

uses
  uwinplot,
  Forms, Interfaces,
  Main in 'main.pas' {Form1},
  GraphOpt in '..\dialogs\graphopt.pas' {GraphOptDlg},
  RegFunc in '..\dialogs\regfunc.pas' {RegFuncDlg},
  Algorith in '..\dialogs\algorith.pas' {AlgorithmDlg},
  Param in '..\dialogs\param.pas' {ParamDlg},
  Aspect in '..\dialogs\aspect.pas' {AspectDlg},
  Help in '..\dialogs\help.pas' {HelpDlg};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TGraphOptDlg, GraphOptDlg);
  Application.CreateForm(TRegFuncDlg, RegFuncDlg);
  Application.CreateForm(TAlgorithmDlg, AlgorithmDlg);
  Application.CreateForm(TParamDlg, ParamDlg);
  Application.CreateForm(TAspectDlg, AspectDlg);
  Application.CreateForm(THelpDlg, HelpDlg);
  SetMaxCurv(1);                            { A single curve will be plotted }
  GraphOptDlg.ReadConfigFile('graph.gcf');  { Read graphic configuration }

  Application.Run;
end.

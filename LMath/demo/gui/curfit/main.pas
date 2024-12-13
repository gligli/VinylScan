unit Main;

{$MODE Delphi}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Spin, Printers, Menus;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Compute1: TMenuItem;
    Graph1: TMenuItem;
    Open1: TMenuItem;
    Quit1: TMenuItem;
    SelectModel1: TMenuItem;
    FitModel1: TMenuItem;
    ViewResults1: TMenuItem;
    Options1: TMenuItem;
    PlotGraph1: TMenuItem;
    PrintGraph1: TMenuItem;
    SelectAlgorithm1: TMenuItem;
    Panel1: TPanel;
    Label1: TLabel;
//    Gauge1: TGauge;
    AxesandCurves1: TMenuItem;
    procedure Open1Click(Sender: TObject);
    procedure Quit1Click(Sender: TObject);
    procedure SelectModel1Click(Sender: TObject);
    procedure SelectAlgorithm1Click(Sender: TObject);
    procedure FitModel1Click(Sender: TObject);
    procedure ViewResults1Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure PlotGraph1Click(Sender: TObject);
    procedure PrintGraph1Click(Sender: TObject);
    procedure AxesandCurves1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  { DMath units }
  utypes, uErrors, unlfit, umodels, uinvbeta, uScaling, ustrings, uwinplot,
  { Dialogs }
  RegFunc, Algorith, GraphOpt, Param, Aspect;

const
  Alpha = 0.05;      { Significance level for statistical tests }

var
  InFName  : String;   { Name of input file }
  OutFName : String;   { Name of output file }
  Title    : String;   { Title of study }
  XName    : String;   { Name of X variable }
  YName    : String;   { Name of Y variable }
  N        : Integer;  { Number of points }
  Model    : TModel;   { Regression model }
  X, Y     : TVector;  { Point coordinates }
  S        : TVector;  { Standard dev. of Y }
  Ycalc    : TVector;  { Estimated Y values }
  Res      : TVector;  { Residuals }
  NormRes  : TVector;  { Normalized residuals }
  B        : TVector;  { Regression parameters }
  ErrCode  : Integer;  { Error code }

function ReadInputFile(InFName   : String;
                       var Title,
                           XName,
                           YName : String;
                       var N     : Integer;
                       var X, Y  : TVector) : Integer;
{ ----------------------------------------------------------------------
  Reads an input file for linear or polynomial regression.
  The input file is an ASCII file with the following structure :

    Line 1 : Title of study
    Line 2 : Number of variables (must be 2 here !)
    Line 3 : Name of variable x
    Line 4 : Name of variable y
    Line 5 : Number of points (must be > number of fitted parameters !)

    The next lines contain the coordinates (x, y) of the points (1 point
    by line). The values of x and y must be separated by spaces or tabs.
  ---------------------------------------------------------------------- }
var
  InF  : Text;     { Input file }
  Nvar : Integer;  { Number of variables }
  K    : Integer;  { Loop variable }
begin
  Assign(InF, InFName);
  Reset(InF);

  ReadLn(InF, Title);
  ReadLn(InF, Nvar);

  if Nvar <> 2 then
    begin
      MessageDlg('Data file must contain 2 variables !', mtError, [mbOK], 0);
      ReadInputFile := - 1;
      Exit;
    end;

  ReadLn(InF, XName);
  ReadLn(InF, YName);
  ReadLn(InF, N);

  DimVector(X, N);
  DimVector(Y, N);
  DimVector(S, N);
  DimVector(Ycalc, N);
  DimVector(Res, N);
  DimVector(NormRes, N);

  for K := 1 to N do
    ReadLn(InF, X[K], Y[K]);

  Close(InF);
  ErrCode := -1;
  ReadInputFile := 0;
end;

procedure WriteResults(InFName  : String;
                       FuncName : String;
                       N        : Integer;
                       X, Y, S  : TVector;
                       Ycalc    : TVector;
                       Res      : TVector;
                       NormRes  : TVector;
                       B        : TVector;
                       V        : TMatrix;
                       FirstPar : Integer;
                       LastPar  : Integer;
                       Test     : TRegTest;
                       Alpha    : Float;
                       Tc, Fc   : Float);
{ ----------------------------------------------------------------------
  Writes the result of the regression to an output file
  ---------------------------------------------------------------------- }
var
  OutF  : Text;     { Output file }
  Line1,
  Line2 : String;   { Separating lines }
  PName : String;   { Parameter name }
  Sr    : Float;    { Residual standard deviation }
  SB    : Float;    { Standard deviations of parameters }
  I     : Integer;  { Loop variable }

begin
  I := Pos('.', InFName);
  OutFName := Copy(InFName, 1, Pred(I)) + '.out';
  Assign(OutF, OutFName);
  Rewrite(OutF);

  Line1 := StrChar(73, '-');
  Line2 := StrChar(73, '=');

  Writeln(OutF, Line2);
  Writeln(OutF, 'Curve fit: ', FuncName);
  Writeln(OutF, Line1);

  Writeln(OutF, 'Parameter   Est.value         Std.dev.        ',
         (100 * (1 - Alpha)):2:0, '% Confidence Interval');

  Writeln(OutF, Line1);

  for I := FirstPar to LastPar do
    begin
      PName := ParamName(Model, I);
      SB := Sqrt(V[I,I]);
      Writeln(OutF, PName:4, B[I]:17:8, SB:17:8,
              (B[I] - Tc * SB):17:8, ';', (B[I] + Tc * SB):17:8);
    end;

  Writeln(OutF, Line1);

  Writeln(OutF, 'Number of observations            : n           = ', N:5);

  with Test do
    begin
      Sr := Sqrt(Vr);
      Writeln(OutF, 'Residual error                    : s           = ', Sr:10:4);

      if R2 <= 1.0 then
        begin
          Writeln(OutF, 'Coefficient of correlation        : r           = ', (Sqrt(R2)):10:4);
          Writeln(OutF, 'Coefficient of determination      : r2          = ', R2:10:4);
          Writeln(OutF, 'Adjusted coeff. of determination  : r2a         = ', R2a:10:4);
        end;

      Writeln(OutF, 'Variance ratio (explained/resid.) : F(', Nu1:3, ', ', Nu2:3, ') = ', F:10:4);
      Writeln(OutF, 'Critical variance ratio           : F(p = ', (1 - Alpha):4:2, ') = ', Fc:10:4);
    end;

  Writeln(OutF, Line1);
  Writeln(OutF, '  i        Y obs.       Y calc.      Residual      Std.dev.      Std.res.');
  Writeln(OutF, Line1);

  for I := 1 to N do
    Writeln(OutF, I:3, Y[I]:14:4, Ycalc[I]:14:4, Res[I]:14:4,
            S[I]:14:4, NormRes[I]:14:4);

  Writeln(OutF, Line2);
  Close(OutF);
end;

procedure SetScale(X, Y : TVector; N : Integer; XTitle, YTitle : String);
var
  XMin, XMax, XStep : Float;
  YMin, YMax, YStep : Float;
begin
  AutoScale(X, 1, N, LinScale, XMin, XMax, XStep);
  AutoScale(Y, 1, N, LinScale, YMin, YMax, YStep);

  SetOxScale(LinScale, XMin, XMax, XStep);
  SetOyScale(LinScale, YMin, YMax, YStep);

  SetOxTitle(XTitle);
  SetOyTitle(YTitle);
end;

procedure ClearGraphics;
begin
  with Form1.Image1 do
    begin
      Canvas.Brush.Color := GraphOptDlg.BorderColorShape.Brush.Color;
      Canvas.FillRect(Rect(0, 0, Width, Height));
      Canvas.Brush.Color := GraphOptDlg.GraphColorShape.Brush.Color;
      Canvas.FillRect(Rect(GraphOptDlg.XminSpinEdit.Value * Width  div 100,
                           GraphOptDlg.YminSpinEdit.Value * Height div 100,
                           GraphOptDlg.XmaxSpinEdit.Value * Width  div 100,
                           GraphOptDlg.YmaxSpinEdit.Value * Height div 100));
    end;
end;

function Func(X : Float) : Float;
{ Function to be plotted }
begin
  Func := umodels.RegFunc(Model, X, B);
end;

procedure PlotGraph(Canvas : TCanvas);
var
  Style               : TPenStyle;
  Width, Symbol, Size : Integer;
  PColor, LColor      : TColor;
  Scale               : TScale;
  CurvAspect          : TAspect;
  Xg, Yg              : TVector;
  XMin, XMax, XStep   : Float;
  XTitle, YTitle      : String;

  procedure SetXgraph(X : TVector; Title : String);
  begin
    Xg := X;
    XTitle := Title;
  end;

  procedure SetYgraph(Y : TVector; Title : String);
  begin
    Yg := Y;
    YTitle := Title;
  end;

begin
  case AspectDlg.GetVarOx of
    Ox_X     : SetXgraph(X, XName);
    Ox_Yobs  : SetXgraph(Y, YName);
    Ox_Ycalc : SetXgraph(Ycalc, YName + ' calc.');
  end;

  case AspectDlg.GetVarOy of
    Oy_Yobs    : SetYgraph(Y, YName);
    Oy_Ycalc   : SetYgraph(Ycalc, YName + ' calc.');
    Oy_Res     : SetYgraph(Res, 'Residuals');
    Oy_NormRes : SetYgraph(NormRes, 'Normalized residuals');
  end;

  if AspectDlg.GetVarChange then
    SetScale(Xg, Yg, N, XTitle, YTitle);

  Canvas.Brush.Color := GraphOptDlg.GraphColorShape.Brush.Color;
  Canvas.Pen.Color := GraphOptDlg.AxisColorShape.Brush.Color;
  Canvas.Pen.Width := GraphOptDlg.AxisWidthSpinEdit.Value;

  SetWindow(Canvas,
            GraphOptDlg.XminSpinEdit.Value,
            GraphOptDlg.XmaxSpinEdit.Value,
            GraphOptDlg.YminSpinEdit.Value,
            GraphOptDlg.YmaxSpinEdit.Value,
            GraphOptDlg.GraphBorderCheckBox.Checked);

  Canvas.Brush.Color := GraphOptDlg.BorderColorShape.Brush.Color;
  Canvas.Font := GraphOptDlg.AxesFontDialog.Font;

  if GraphOptDlg.XplotCheckBox.Checked then PlotOxAxis(Canvas);
  if GraphOptDlg.YplotCheckBox.Checked then PlotOyAxis(Canvas);

  PlotGrid(Canvas, TGrid(GraphOptDlg.GridRadioGroup.ItemIndex));

  Canvas.Font := GraphOptDlg.TitleFontDialog.Font;

  WriteGraphTitle(Canvas);

  CurvAspect := AspectDlg.GetCurvAspect;

  GetPointParam(1, Symbol, Size, PColor);
  GetLineParam(1, Style, Width, LColor);

  if CurvAspect = Curve_Obs then
    SetPointParam(1, 0, Size, PColor);

  if CurvAspect in [Pts_Only, Pts_Curve_Calc] then
    SetLineParam(1, psClear, Width, LColor);  { Don't connect points }

  if CurvAspect <> Curve_Calc then
    if AspectDlg.GetShowErrorBars and (Xg = X) and (Yg = Y) then
      PlotCurveWithErrorBars(Canvas, X, Y, S, AspectDlg.GetNSD, 1, N, 1)
    else
      PlotCurve(Canvas, Xg, Yg, 1, N, 1);

  SetPointParam(1, Symbol, Size, PColor);
  SetLineParam(1, Style, Width, LColor);

  if (ErrCode = 0) and (Xg = X) and (Yg = Y) and
     (CurvAspect in [Curve_Calc, Pts_Curve_Calc]) then
    begin
      GetOxScale(Scale, XMin, XMax, XStep);
      PlotFunc(Canvas, Func, XMin, XMax, AspectDlg.GetNptGraph, 1);
    end;
end;

{ ************************************************************************** }

procedure TForm1.Open1Click(Sender: TObject);
{ Read data file and initialize scales and titles }
begin
  if not OpenDialog1.Execute then Exit;

  InFName := OpenDialog1.FileName;

  if ReadInputFile(InFName, Title, XName, YName, N, X, Y) <> 0 then Exit;

  SetScale(X, Y, N, XName, YName);
  SetGraphTitle(Title);
end;

procedure TForm1.Quit1Click(Sender: TObject);
begin
  Form1.Close;
end;

procedure TForm1.SelectModel1Click(Sender: TObject);
begin
  RegFuncDlg.ShowModal;
  Model := RegFuncDlg.GetModel;
  B := RegFuncDlg.GetParam;
end;

procedure TForm1.SelectAlgorithm1Click(Sender: TObject);
begin
  if Model.RegType in [REG_LIN, REG_POL] then
    AlgorithmDlg.TabbedNotebook1.PageIndex := 0
  else
    AlgorithmDlg.TabbedNotebook1.PageIndex := 1;
  AlgorithmDlg.ShowModal;
end;

procedure TForm1.FitModel1Click(Sender: TObject);
var
  MaxIter : Integer;  { Max number of iterations }
  Tol     : Float;    { Tolerance for nonlinear regression }
  SVDTol  : Float;    { Tolerance for SVD }
  V       : TMatrix;  { Variance-covariance matrix of parameters }
  Test    : TRegTest; { Regression tests }
  LastPar : Integer;  { Index of last parameter }
  I       : Integer;  { Loop variable }
  Sr      : Float;    { Residual standard deviation }
  Tc      : Float;    { Critical t value }
  Fc      : Float;    { Critical F value }

begin
  if N = 0 then Exit;

  { Dimension arrays }
  LastPar := LastParam(Model);
  DimMatrix(V, LastPar, LastPar);
  DimVector(Ycalc, N);
  DimVector(S, N);

  { Get algorithm options }
  MaxIter := AlgorithmDlg.GetMaxIter;
  Tol := AlgorithmDlg.GetTol;
  SVDTol := N * AlgorithmDlg.GetSVDTol;

  Label1.Visible := True;
  { For nonlinear regression only }
  if not (Model.RegType in [REG_LIN, REG_POL]) then
    begin
      { Force initial parameter estimation if necessary }
      if AlgorithmDlg.GetEstParam then
        begin
          { Setting a parameter to zero will force initial estimation,
            while setting MaxIter to zero will skip the refinement }
          B[1] := 0.0;
          FitModel(Model, X, Y, Ycalc, nil, 1, N, 0, Tol, SVDTol, B, V, Test);
        end;

      ParamDlg.SetModel(Model);
      ParamDlg.SetParam(B);

      { Change parameter estimates and bounds if necessary }
      if AlgorithmDlg.GetShowParam or (Model.RegType = REG_EVAL) then
        ParamDlg.ShowModal;
    end;

  { Perform regression }

  FitModel(Model, X, Y, Ycalc, nil, 1, N, MaxIter, Tol, SVDTol, B, V, Test);
  ErrCode := MathErr;

  if ErrCode <> MathOK then
  begin
    MessageDlg(MathErrMessage, mtError, [mbOk], 0);
    Exit;
  end;
  { Compute Student's t and Snedecor's F }
  Tc := InvStudent(Test.Nu2, 1 - 0.5 * Alpha);
  Fc := InvSnedecor(Test.Nu1, Test.Nu2, 1 - Alpha);

  { Update standard deviations and residuals }
  Sr := Sqrt(Test.Vr);
  for I := 1 to N do
    begin
      S[I] := Sr;
      Res[I] := Y[I] - Ycalc[I];
      NormRes[I] := Res[I] / S[I];
    end;

  { Write results }
  WriteResults(InFName, FuncName(Model), N, X, Y, S, Ycalc, Res, NormRes, B, V,
               FirstParam(Model), LastParam(Model), Test, Alpha, Tc, Fc);
//  Gauge1.Progress := 100;
  MessageDlg('Results written to file ' + OutFName, mtInformation, [mbOk], 0);
  Label1.Visible := False;
//  Gauge1.Visible := False;
end;

procedure TForm1.ViewResults1Click(Sender: TObject);
begin
    Form1.Image1.Visible := False;
    Form1.Memo1.Visible := True;
    if OutFName <> '' then
      Form1.Memo1.Lines.LoadFromFile(OutFName);
end;

procedure TForm1.Options1Click(Sender: TObject);
begin
  GraphOptDlg.ShowModal;
end;

procedure TForm1.AxesandCurves1Click(Sender: TObject);
begin
  AspectDlg.ShowModal;
end;

procedure TForm1.PlotGraph1Click(Sender: TObject);
begin
  if N = 0 then Exit;

  Form1.Image1.Visible := True;
  Form1.Memo1.Visible := False;

  ClearGraphics;

  InitGraphics(Image1.Width, Image1.Height);

  PlotGraph(Image1.Canvas);
end;

procedure TForm1.PrintGraph1Click(Sender: TObject);
begin
  if N = 0 then Exit;

  Printer.BeginDoc;

  InitGraphics(Printer.PageWidth, Printer.PageHeight);
  SetWindow(Printer.Canvas, 10, 90, 10, 90, True);

  PlotGraph(Printer.Canvas);
  Printer.EndDoc;
end;

begin
  N := 0;
  ErrCode := -1;
  Model.RegType := REG_LIN;  { Select linear regression by default }
  DimVector(B, 1);
end.

{ ******************************************************************
  This program performs a nonlinear least squares fit of a rational
  fraction :
                           p0 + p1.x + p2.x^2 + ...
                       y = ------------------------
                           1 + q1.x + q2.x^2 + ...

  The following parameters are passed on the command line :

    1st parameter = Name of input file (default extension = .DAT)
                    The structure of the input file is described in
                    REGUTILS.PAS
    2nd parameter = Degree of numerator (default = 2)
    3rd parameter = Degree of denominator (default = 2)
    4th parameter = 1 if the function includes a constant term (p0)

  The fit is performed by weighted regression, assuming a constant
  relative error (standard deviation proportional to y)

  The file FRAC.DAT is an example of enzyme kinetics.
  The best fit is obtained with 2nd-degree numerator and denominator,
  with no constant term.
  ****************************************************************** }

program regfrac;

uses
{$IFDEF USE_DLL}
  dmath, regutils;
{$ELSE}
  utypes, uErrors, uScaling, unlfit, ufracfit,
  uregtest, uinvbeta, uplot, regutils;
{$ENDIF}

const
  MaxIter = 1000;    { Max. number of iterations }
  Tol     = 1.0E-4;  { Required precision }
  Alpha   = 0.05;    { Significance level }

var
  InFName    : String;    { Name of input file }
  Title      : String;    { Title of study }
  XName,
  YName      : String;    { Names of variable }
  N          : Integer;   { Number of points }
  X, Y       : TVector;   { Point coordinates }
  S          : TVector;   { Standard deviations of observations }
  Ycalc      : TVector;   { Expected Y values }
  Deg1, Deg2 : Integer;   { Degrees of numerator and denominator }
  ConsTerm   : Boolean;   { Presence of constant term }
  B          : TVector;   { Regression parameters }
  V          : TMatrix;   { Variance-covariance matrix }
  FirstPar,
  LastPar    : Integer;   { Indices of first and last parameter }
  Test       : TRegTest;  { Regression tests }
  Sr         : Float;     { Residual standard dev. }
  Tc         : Float;     { Critical t value }
  Fc         : Float;     { Critical F value }
  I          : Integer;   { Loop variable }

procedure ReadCmdLine(var InFName    : String;
                      var Deg1, Deg2 : Integer;
                      var ConsTerm   : Boolean);
{ ------------------------------------------------------------------
  Reads command line
  ------------------------------------------------------------------ }
  var
    ErrCode : Integer;
  begin
    { Name of input file }
    InFName := ParamStr(1);
    if InFName = '' then InFName := 'frac.dat';
    if Pos('.', InFName) = 0 then InFName := InFName + '.dat';

    { Degree of numerator }
    Val(ParamStr(2), Deg1, ErrCode);
    if (ErrCode <> 0) or (Deg1 < 1) then Deg1 := 2;

    { Degree of denominator }
    Val(ParamStr(3), Deg2, ErrCode);
    if (ErrCode <> 0) or (Deg2 < 1) then Deg2 := 2;

    { Presence of constant term }
    ConsTerm := (ParamStr(4) = '1');
  end;

procedure SetBounds(Deg1, Deg2            : Integer;
                    ConsTerm              : Boolean;
                    var FirstPar, LastPar : Integer);
{ ------------------------------------------------------------------
  Sets number of parameters and parameter bounds
  ------------------------------------------------------------------ }
  var
    I : Integer;
  begin
    if ConsTerm then FirstPar := 0 else FirstPar := 1;
    LastPar := Deg1 + Deg2;

    SetMaxParam(LastPar);

    for I := 0 to LastPar do
      SetParamBounds(I, -1000, 1000);
  end;

function PltFunc(X : Float) : Float;
{ ------------------------------------------------------------------
  Function to be plotted
  ------------------------------------------------------------------ }
  begin
    PltFunc := FracFit_Func(X, B);
  end;

{ *************************** Main program ************************* }

begin
  { Read command line  }
  ReadCmdLine(InFName, Deg1, Deg2, ConsTerm);

  { Read input file }
  if ReadInputFile(InFName, Title, XName, YName, N, X, Y) <> 0 then
    begin
      WriteLn('Error reading file ', InFName);
      Halt;
    end;

  { Set parameter bounds }
  SetBounds(Deg1, Deg2, ConsTerm, FirstPar, LastPar);

  { Dimension arrays }
  DimVector(B, LastPar);
  DimMatrix(V, LastPar, LastPar);
  DimVector(S, N);
  DimVector(Ycalc, N);

  { Initialize standard deviations }
  for I := 1 to N do
    S[I] := Y[I];

  { Perform regression }
  WFracFit(X, Y, S, 1, N, Deg1, Deg2, ConsTerm, MaxIter, Tol, B, V);

  if MathErr = MatOk then
    begin
      { Compute predicted Y values }
      for I := 1 to N do
        Ycalc[I] := FracFit_Func(X[I], B);

      { Update var-cov. matrix and compute statistical tests }
      WRegTest(Y, Ycalc, S, 1, N, V, FirstPar, LastPar, Test);

      { Compute Student's t and Snedecor's F }
      Tc := InvStudent(Test.Nu2, 1 - 0.5 * Alpha);
      Fc := InvSnedecor(Test.Nu1, Test.Nu2, 1 - Alpha);

      { Update standard deviations }
      Sr := Sqrt(Test.Vr);
      for I := 1 to N do
        S[I] := S[I] * Sr;

      { Write results }
      WriteResults(Title, N, X, Y, S, Ycalc,
                   B, V, FirstPar, LastPar, Test, Alpha, Tc, Fc);

      { Plot curve }
      PlotGraph(@PltFunc, Title, N, X, Y);
    end
  else
    Writeln('Unable to fit curve!');
end.

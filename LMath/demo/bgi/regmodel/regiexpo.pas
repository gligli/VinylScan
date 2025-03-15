{ ******************************************************************
  This program performs a nonlinear least squares fit of an
  increasing exponential :

     y = Ymin + A.[1 - exp(-k.x)]

  The following parameters are passed on the command line :

    1st parameter = Name of input file (default extension = .DAT)
                    The structure of the input file is described in
                    REGUTILS.PAS
    2nd parameter = 1 if the function includes a constant term (Ymin)

  The file IEXPO.DAT is an example taken from a study in enzyme
  kinetics
  ****************************************************************** }

program regiexpo;

uses
{$IFDEF USE_DLL}
  dmath, regutils;
{$ELSE}
  utypes, uErrors, uScaling, unlfit, uiexpfit,
  uregtest, uinvbeta, uplot, regutils;
{$ENDIF}

const
  MaxIter = 1000;    { Max. number of iterations }
  Tol     = 1.0E-4;  { Required precision }
  Alpha   = 0.05;    { Significance level }

var
  InFName   : String;    { Name of input file }
  Title     : String;    { Title of study }
  XName,
  YName     : String;    { Names of variable }
  N         : Integer;   { Number of points }
  X, Y      : TVector;   { Point coordinates }
  S         : TVector;   { Standard deviations of observations }
  Ycalc     : TVector;   { Expected Y values }
  ConsTerm  : Boolean;   { Presence of constant term }
  B         : TVector;   { Regression parameters }
  V         : TMatrix;   { Variance-covariance matrix }
  FirstPar,
  LastPar   : Integer;   { Indices of first and last parameter }
  Test      : TRegTest;  { Regression tests }
  Sr        : Float;     { Residual standard dev. }
  Tc        : Float;     { Critical t value }
  Fc        : Float;     { Critical F value }
  I         : Integer;   { Loop variable }

procedure ReadCmdLine(var InFName  : String;
                      var ConsTerm : Boolean);
{ ------------------------------------------------------------------
  Reads command line
  ------------------------------------------------------------------ }
  begin
    { Name of input file }
    InFName := ParamStr(1);
    if InFName = '' then
      begin
        InFName := 'iexpo.dat';
        ConsTerm := True;
        Exit;
      end;

    if Pos('.', InFName) = 0 then InFName := InFName + '.dat';

    { Presence of constant term }
    ConsTerm := (ParamStr(2) = '1');
  end;

procedure SetBounds(ConsTerm              : Boolean;
                    var FirstPar, LastPar : Integer);
{ ------------------------------------------------------------------
  Sets number of parameters and parameter bounds
  ------------------------------------------------------------------ }
  var
    I : Integer;
  begin
    if ConsTerm then FirstPar := 0 else FirstPar := 1;
    LastPar := 2;

    SetMaxParam(LastPar);

    for I := FirstPar to LastPar do
      SetParamBounds(I, -1000, 1000);
  end;

function PltFunc(X : Float) : Float;
{ ------------------------------------------------------------------
  Function to be plotted
  ------------------------------------------------------------------ }
  begin
    PltFunc := IncExpFit_Func(X, B);
  end;

{ *************************** Main program ************************* }

begin
  { Read command line  }
  ReadCmdLine(InFName, ConsTerm);

  { Read input file }
  if ReadInputFile(InFName, Title, XName, YName, N, X, Y) <> 0 then
    begin
      WriteLn('Error reading file ', InFName);
      Halt;
    end;

  { Set parameter bounds }
  SetBounds(ConsTerm, FirstPar, LastPar);

  { Dimension arrays }
  DimVector(B, LastPar);
  DimMatrix(V, LastPar, LastPar);
  DimVector(S, N);
  DimVector(Ycalc, N);

  { Perform regression }
  IncExpFit(X, Y, 1, N, ConsTerm, MaxIter, Tol, B, V);

  if MathErr <= MatOk then
    begin
      { Compute predicted Y values }
      for I := 1 to N do
        Ycalc[I] := IncExpFit_Func(X[I], B);

      { Update var-cov. matrix and compute statistical tests }
      RegTest(Y, Ycalc, 1, N, V, FirstPar, LastPar, Test);

      { Compute Student's t and Snedecor's F }
      Tc := InvStudent(Test.Nu2, 1 - 0.5 * Alpha);
      Fc := InvSnedecor(Test.Nu1, Test.Nu2, 1 - Alpha);

      { Update standard deviations }
      Sr := Sqrt(Test.Vr);
      for I := 1 to N do
        S[I] := Sr;

      { Write results }
      WriteResults(Title, N, X, Y, S, Ycalc,
                   B, V, FirstPar, LastPar, Test, Alpha, Tc, Fc);

      { Plot curve }
      PlotGraph(@PltFunc, Title, N, X, Y);
    end
  else
    Writeln('Unable to fit curve! (MathErr = ', MathErr, ')');
end.

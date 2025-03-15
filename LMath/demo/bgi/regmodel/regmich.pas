{ ******************************************************************
  This program performs a nonlinear least squares fit of the
  Michaelis equation:

                                  Ymax . x
                              y = --------
                                   Km + x

  The parameters are: B(0) = Ymax, B(1) = Km

  The name of the input file (default extension = .DAT) is passed
  on the command line. The structure of this file is described in
  REGUTILS.PAS

  The file MICHAEL.DAT is an example taken from a study in enzyme
  kinetics.
  ****************************************************************** }

program regmich;

uses
{$IFDEF USE_DLL}
  dmath, regutils;
{$ELSE}
  utypes, uErrors, uScaling, unlfit, umichfit,
  uregtest, uinvbeta, uplot, regutils;
{$ENDIF}

const
  FirstPar = 0;       { Index of first parameter }
  LastPar  = 1;       { Index of last parameter }
  MaxIter  = 1000;    { Max. number of iterations }
  Tol      = 1.0E-4;  { Required precision }
  Alpha    = 0.05;    { Significance level }

var
  InFName : String;    { Name of input file }
  Title   : String;    { Title of study }
  XName,
  YName   : String;    { Names of variable }
  N       : Integer;   { Number of points }
  X, Y    : TVector;   { Point coordinates }
  S       : TVector;   { Standard deviations of observations }
  Ycalc   : TVector;   { Expected Y values }
  B       : TVector;   { Regression parameters }
  V       : TMatrix;   { Variance-covariance matrix }
  Test    : TRegTest;  { Regression tests }
  Sr      : Float;     { Residual standard dev. }
  Tc      : Float;     { Critical t value }
  Fc      : Float;     { Critical F value }
  I       : Integer;   { Loop variable }

procedure ReadCmdLine(var InFName : String);
{ ------------------------------------------------------------------
  Reads command line
  ------------------------------------------------------------------ }
  begin
    { Name of input file }
    InFName := ParamStr(1);
    if InFName = '' then InFName := 'michael.dat';
    if Pos('.', InFName) = 0 then InFName := InFName + '.dat';
  end;

procedure SetBounds;
{ ------------------------------------------------------------------
  Sets number of parameters and parameter bounds
  ------------------------------------------------------------------ }
  begin
    SetMaxParam(LastPar);

    { Ymax and Km are assumed to be positive }

    SetParamBounds(0, 0, 100);
    SetParamBounds(1, 0, 100);
  end;

function PltFunc(X : Float) : Float;
{ ------------------------------------------------------------------
  Function to be plotted
  ------------------------------------------------------------------ }
  begin
    PltFunc := MichFit_Func(X, B);
  end;

{ *************************** Main program ************************* }

begin
  { Read command line  }
  ReadCmdLine(InFName);

  { Read input file }
  if ReadInputFile(InFName, Title, XName, YName, N, X, Y) <> 0 then
    begin
      WriteLn('Error reading file ', InFName);
      Halt;
    end;

  { Set parameter bounds }
  SetBounds;

  { Dimension arrays }
  DimVector(B, LastPar);
  DimMatrix(V, LastPar, LastPar);
  DimVector(S, N);
  DimVector(Ycalc, N);

  MichFit(X, Y, 1, N, MaxIter, Tol, B, V);

  if MathErr = MatOk then
    begin
      { Compute predicted Y values }
      for I := 1 to N do
        Ycalc[I] := MichFit_Func(X[I], B);

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
    Writeln('Unable to fit curve!');
end.

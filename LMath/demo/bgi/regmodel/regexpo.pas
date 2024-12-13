{ ******************************************************************
  This program performs a nonlinear least squares fit of a sum of
  decreasing exponentials :

     y = Ymin + A1.exp(-a1.x) + A2.exp(-a2.x) + A3.exp(-a3.x) + ...

  The following parameters are passed on the command line :

    1st parameter = Name of input file (default extension = .DAT)
                    The structure of the input file is described in
                    REGUTILS.PAS
    2nd parameter = Number of exponentials (Default = 2)
    3rd parameter = 1 if the function includes a constant term (Ymin)

  The fit is performed by weighted regression, assuming a constant
  relative error (standard deviation proportional to y)

  There are 3 examples taken from the book of Gibaldi and Perrier
  (M. GIBALDI & D. PERRIER, Pharmacokinetics, 2nd edition, Dekker 1982) :

    IV2.DAT   (2 exponentials)
    ORAL1.DAT (2 exponentials)
    ORAL2.DAT (3 exponentials)
  ****************************************************************** }

program regexpo;

uses
{$IFDEF USE_DLL}
  dmath, regutils;
{$ELSE}
  utypes, uErrors, uScaling, unlfit, uexpfit,
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
  Nexp      : Integer;   { Number of exponentials }
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
                      var Nexp     : Integer;
                      var ConsTerm : Boolean);
{ ------------------------------------------------------------------
  Reads command line
  ------------------------------------------------------------------ }
  var
    ErrCode : Integer;
  begin
    { Name of input file }
    InFName := ParamStr(1);
    if InFName = '' then InFName := 'iv2.dat';
    if Pos('.', InFName) = 0 then InFName := InFName + '.dat';

    { Number of exponentials }
    Val(ParamStr(2), Nexp, ErrCode);
    if (ErrCode <> 0) or (Nexp < 1) then Nexp := 2;

    { Presence of constant term }
    ConsTerm := (ParamStr(3) = '1');
  end;

procedure SetBounds(Nexp                  : Integer;
                    ConsTerm              : Boolean;
                    var FirstPar, LastPar : Integer);
{ ------------------------------------------------------------------
  Sets number of parameters and parameter bounds
  ------------------------------------------------------------------ }
  var
    I : Integer;
  begin
    if ConsTerm then FirstPar := 0 else FirstPar := 1;
    LastPar := 2 * Nexp;

    SetMaxParam(LastPar);

    { Ymin and A1 may be negative }
    for I := 0 to 1 do
      SetParamBounds(I, -1000, 1000);

    { All other parameters must be positive }
    for I := 2 to LastPar do
      SetParamBounds(I, 0, 1000);
  end;

function PltFunc(X : Float) : Float;
{ ------------------------------------------------------------------
  Function to be plotted
  ------------------------------------------------------------------ }
  begin
    PltFunc := ExpFit_Func(X, B);
  end;

{ *************************** Main program ************************* }

begin
  { Read command line  }
  ReadCmdLine(InFName, Nexp, ConsTerm);

  { Read input file }
  if ReadInputFile(InFName, Title, XName, YName, N, X, Y) <> 0 then
    begin
      WriteLn('Error reading file ', InFName);
      Halt;
    end;

  { Set parameter bounds }
  SetBounds(Nexp, ConsTerm, FirstPar, LastPar);

  { Dimension arrays }
  DimVector(B, LastPar);
  DimMatrix(V, LastPar, LastPar);
  DimVector(S, N);
  DimVector(Ycalc, N);

  { Initialize standard deviations }
  for I := 1 to N do
    S[I] := Y[I];

  { Perform regression }
  WExpFit(X, Y, S, 1, N, Nexp, ConsTerm, MaxIter, Tol, B, V);

  if MathErr <= MatOk then
    begin
      { Compute predicted Y values }
      for I := 1 to N do
        Ycalc[I] := ExpFit_Func(X[I], B);

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
    Writeln('Unable to fit curve! (MathErr = ', MathErr, ')');
end.

{ ******************************************************************
  This programs evaluates the regression routines in DMATH with the
  reference data sets provided by the National Institute of Standards
  and Technology (NIST). These data are available on the Internet at:
  http://www.itl.nist.gov/div898/strd/general/dataarchive.html
  
  The following data sets are used:

  Norris         : Linear
  Longley        : Multilinear, 6 indep. variables
  Pontius        : Polynomial, degree=2
  Wampler 1 to 5 : Polynomial, degree=5, increasing noise level
  Filippelli     : Polynomial, degree=10
  Kirby 2        : Fraction, degree num=2 + const term, denom=2
  Thurber        : Fraction, degree num=3 + const term, denom=3
  Hahn 1         : Fraction, degree num=3 + const term, denom=3
  MGH 17         : Sum of 2 exponentials + const term
  Lanczos 1 to 3 : Sum of 3 exponentials, increasing noise level
  Box BOD        : Increasing exponential
  Misra 1a       : Increasing exponential
  Daniel-Wood    : Power
  Ratkowsky 2    : Logistic

  For each model, the following data are stored in file NIST.DAT,
  with all the significant digits reported by the NIST:

  * The reference parameter values with their standard deviations
  * The residual standard deviation
  * For linear and polynomial models: R^2 and F

  The program reads the input file, fits the models and writes the
  results in the output file NIST.OUT. The relative deviations between
  the reference values and the program values are computed and stored,
  then plotted as precision (- log |max. relative error|) vs model index.

  According to the NIST, a good regression program should find at least
  4-5 significant digits of the reference values.

  Notes:

  1) With linear models, the best results are obtained with the
     Singular Value Decomposition algorithm.

  2) With nonlinear models, the best results are obtained by running
     Simplex or Simulated Annealing first, then switching to Marquardt.

  3) The program should be tested at the highest precision level available.
     In particular, the Filippelli test is passed only at the extended
     precision level ($DEFINE EXTENDEDREAL)

  4) The NIST does not provide reference data for the Michaelis, Hill and
     pKa models, nor for weighted regression.
  ********************************************************************** }

program nist;

uses
{$IFDEF USE_DLL}
  dmath;
{$ELSE}
  utypes, umath, uErrors, uScaling, unlfit, umodels, ustrings, uplot;
{$ENDIF} 

const
  NDATASETS = 20;  { Number of data sets }

  DATANAME : array[1..NDATASETS] of String =
  ('Norris',
   'Longley',
   'Pontius',
   'Wampler 1',
   'Wampler 2',
   'Wampler 3',
   'Wampler 4',
   'Wampler 5',
   'Filippelli',
   'Kirby 2',
   'Thurber',
   'Hahn 1',
   'MGH 17',
   'Lanczos 1',
   'Lanczos 2',
   'Lanczos 3',
   'Box BOD',
   'Misra 1a',
   'Daniel-Wood',
   'Ratkowsky 2');

   MAXITER = 10000;    { Maximal number of iterations }

var
  N : Integer = 0;  { Number of observations }

var
  Index      : Integer;  { Index of data set }
  X          : TVector;  { Vector of independent variable }
  U          : TMatrix;  { Matrix of independent variables }
  Y          : TVector;  { Vector of dependent variable }
  Model      : TModel;   { Regression model }
  FirstPar   : Integer;  { Index of first model parameter }
  LastPar    : Integer;  { Index of last model parameter }
  B_ref      : TVector;  { Reference values for regression parameters }
  S_ref      : TVector;  { Reference values for parameter SD's }
  Sr_ref     : Float;    { Reference values for residual SD }
  R2_ref     : Float;    { Reference values for R^2 }
  F_ref      : Float;    { Reference values for F }
  Tol        : Float;    { Tolerance for nonlinear regression }
  SVDTol     : Float;    { Tolerance for Singular Value Decomposition }
  B          : TVector;  { Regression parameters }
  V          : TMatrix;  { Variance-covariance matrix }
  Ycalc      : TVector;  { Estimated Y values }
  Test       : TRegTest; { Regression tests }
  MinPrec    : TVector;  { Minimal precision reached in each fit }
  OutF       : Text;     { Output file }

procedure ExtractModelData(ModelLine : String; var Model : TModel);
{ ------------------------------------------------------------------
  Extracts the model data from the ModelLine string
  ------------------------------------------------------------------ }
var
  L      : Integer;  { Length of string }
  P      : Integer;  { Position of character in string }
  C      : Char;     { Character }
  S1, S2 : String;   { Substrings }
  N1, N2 : Integer;  { Numeric parameters }
  Code   : Integer;  { Error code }
  Cst    : Boolean;  { Presence of constant term }
begin
  with Model do
    { Identify model }
    if Pos('Linear', ModelLine) > 0 then
      RegType := REG_LIN
    else if Pos('Multilinear', ModelLine) > 0 then
      RegType := REG_MULT
    else if Pos('Polynomial', ModelLine) > 0 then
      RegType := REG_POL
    else if Pos('Rational', ModelLine) > 0 then
      RegType := REG_FRAC
    else if Pos('Exponential', ModelLine) > 0 then
      RegType := REG_EXPO
    else if Pos('Increasing exponential', ModelLine) > 0 then
      RegType := REG_IEXPO
    else if Pos('Power', ModelLine) > 0 then
      RegType := REG_POWER
    else if Pos('Logistic', ModelLine) > 0 then
      RegType := REG_LOGIS;

  { Initialize substrings }
  S1 := ''; S2 := '';

  { Find the 1st comma }
  L := Length(ModelLine);
  P := 0;
  repeat
    Inc(P);
    C := ModelLine[P];
  until (C = ',') or (P = L);

  { Extract substring between 2 commas, or from comma to slash,
    or from comma to end of string. This substring corresponds
    to the first numeric parameter }
  if P < L then
    repeat
      Inc(P);
      C := ModelLine[P];
      if (C <> ',') and (C <> '/') then
        S1 := S1 + C;
    until (C = ',') or (C = '/') or (P = L);

  { Extract substring from '/' to 2nd comma. This substring
    corresponds to the second numeric parameter }
  if C = '/' then
    repeat
      Inc(P);
      C := ModelLine[P];
      if C <> ',' then
        S2 := S2 + C;
    until (C = ',') or (P = L);

  { Convert substrings to numeric parameters }
  Val(S1, N1, Code);
  Val(S2, N2, Code);

  { Check for constant term }
  if Pos('no constant term', ModelLine) > 0 then
    Cst := False
  else if Pos('constant term', ModelLine) > 0 then
    Cst := True;

  { Store model parameters }
  with Model do
    case RegType of
      REG_MULT : begin
                   Nvar := N1;
                   Mult_ConsTerm := Cst;
                 end;
      REG_POL  : Deg := N1;
      REG_FRAC : begin
                   Deg1 := N1;
                   Deg2 := N2;
                   Frac_ConsTerm := Cst;
                 end;
      REG_EXPO : begin
                   Nexp := N1;
                   Expo_ConsTerm := Cst;
                 end;
      REG_IEXPO: IExpo_ConsTerm := Cst;
      REG_LOGIS: begin
                   Logis_ConsTerm := Cst;
                   Logis_General := False;
                 end;
    end;
end;

procedure ReadInputFile(    Index                 : Integer;
                        var N                     : Integer;
                        var X, Y                  : TVector;
                        var U                     : TMatrix;
                        var Model                 : TModel;
                        var FirstPar, LastPar     : Integer;
                        var B_ref, S_ref          : TVector;
                        var Sr_ref, R2_ref, F_ref : Float);
{ ------------------------------------------------------------------
  Reads input file and initializes model
  ------------------------------------------------------------------ }
var
  InF       : Text;     { Input file }
  S         : String;   { Line of text }
  ModelLine : String;   { Line containing the model specifications }
  I, K      : Integer;  { Loop variables }
begin
  { Delete old arrays if necessary }
  if N > 0 then
    begin
    end;

  { Open input file }
  Assign(InF, 'nist.dat');
  Reset(InF);

  { Read lines until title line is found }
  repeat
    ReadLn(InF, S);
  until Pos(DATANAME[Index], S) > 0;

  { Read model line and initialize model }
  ReadLn(InF, ModelLine);
  ExtractModelData(ModelLine, Model);
  FirstPar := FirstParam(Model);
  LastPar := LastParam(Model);

  { Read number of points }
  Read(InF, N);

  { Dimension new arrays }
  DimVector(X, N);
  DimMatrix(U, N, LastPar);
  DimVector(Y, N);
  DimVector(B_ref, LastPar);
  DimVector(S_ref, LastPar);

  { Read observations }
  if Model.RegType = REG_MULT then
    for K := 1 to N do
      begin
        for I := 1 to LastPar do
          Read(InF, U[K,I]);
        Read(InF, Y[K]);
      end
  else
    for K := 1 to N do
      Read(InF, X[K], Y[K]);

  { Read reference parameters and their SD's }
  for K := FirstPar to LastPar do
    Read(InF, B_ref[K], S_ref[K]);

  { Read reference values for regression tests }
  Read(InF, Sr_ref);
  if Model.RegType in [REG_LIN, REG_MULT, REG_POL] then
    Read(InF, R2_ref, F_ref);

  { Close input file }
  Close(InF);
end;

procedure DelArrays(var B : TVector; var V : TMatrix; var Ycalc : TVector);
{ ------------------------------------------------------------------
  Deletes old arrays
  ------------------------------------------------------------------ }
begin
  B := nil;
  V := nil;
  Ycalc := nil;
end;

procedure DimArrays(N, LastPar : Integer; var B : TVector;
                    var V : TMatrix; var Ycalc : TVector);
{ ------------------------------------------------------------------
  Dimensions new arrays
  ------------------------------------------------------------------ }
begin
  DimVector(B, LastPar);
  DimMatrix(V, LastPar, LastPar);
  DimVector(Ycalc, N);
end;

procedure SetBounds(FirstPar, LastPar : Integer);
{ ------------------------------------------------------------------
  Sets parameter bounds
  ------------------------------------------------------------------ }
var
  I : Integer;
begin
  SetMaxParam(LastPar);
  for I := FirstPar to LastPar do
    SetParamBounds(I, -10, 10000);
end;

procedure WriteResults(var OutF              : Text;
                       Index                 : Integer;
                       Model                 : TModel;
                       FirstPar, LastPar     : Integer;
                       B_ref, S_ref, B       : TVector;
                       V                     : TMatrix;
                       Sr_ref, R2_ref, F_ref : Float;
                       RegTest               : TRegTest;
                       MinPrec               : TVector);
{ ------------------------------------------------------------------
  Writes results to output file
  ------------------------------------------------------------------ }
const
  HEADER    = '           Value                     Reference             Rel.error';
  LINEWIDTH = 73;

var
  Line1, Line2 : String;   { Separating lines }
  S            : Float;    { Standard deviation of a parameter }
  Sr           : Float;    { Residual error }
  I            : Integer;  { Loop variable }
  Err, Prec    : Float;    { Error and precision terms }
begin
  Line1 := StrChar(LINEWIDTH, '-');
  Line2 := StrChar(LINEWIDTH, '=');

  WriteLn(OutF, Line2);
  WriteLn(OutF, 'Data set: ', DATANAME[Index]);
  WriteLn(OutF, 'Function: ', FuncName(Model));
  WriteLn(OutF, Line1);

  case MathErr of
    OptOk :
      begin
        WriteLn(OutF, 'Param', HEADER);
        WriteLn(OutF, Line1);

        MinPrec[Index] := MaxNum;
        for I := FirstPar to LastPar do
          begin
            Err := (B[I] - B_ref[I]) / B_ref[I];
            Prec := - Log10(Abs(Err));
            if Prec < MinPrec[Index] then MinPrec[Index] := Prec;
            WriteLn(OutF, RFill(ParamName(Model, I), 7),
                    B[I]:23, '     ', B_ref[I]:23, '     ', Err:10);
          end;

        WriteLn(OutF, Line1);
        WriteLn(OutF, 'SD   ', HEADER);
        WriteLn(OutF, Line1);

        for I := FirstPar to LastPar do
          begin
            S := Sqrt(V[I,I]);
            Write(OutF, RFill(ParamName(Model, I), 7), S:23, '     ', S_ref[I]:23);
            if S_ref[I] > 0.0 then
              Write(OutF, '     ', ((S - S_ref[I]) / S_ref[I]):10);
            WriteLn(OutF);
          end;

        WriteLn(OutF, Line1);
        WriteLn(OutF, 'Test ', HEADER);
        WriteLn(OutF, Line1);

        with RegTest do
          begin
            Sr := Sqrt(Vr);
            Write(OutF, 's      ', Sr:23, '     ', Sr_ref:23);
            if Sr_ref > 0.0 then
              Write(OutF, '     ', ((Sr - Sr_ref) / Sr_ref):10);
            WriteLn(OutF);
            if Model.RegType in [REG_LIN, REG_MULT, REG_POL] then
              begin
                WriteLn(OutF, 'R^2    ', R2:23, '     ', R2_ref:23,
                        '     ', ((R2 - R2_ref) / R2_ref):10);
                if F_ref < 1.0E+38 then
                  WriteLn(OutF, 'F      ', F:23, '     ', F_ref:23,
                          '     ', ((F - F_ref) / F_ref):10);
              end;
          end;
      end;

    OptSing      : WriteLn(OutF, 'Singular matrix');
    OptBigLambda : WriteLn(OutF, 'Too high Marquardt''s parameter');
    OptNonConv   : WriteLn(OutF, 'Non - convergence');
  end;

  WriteLn(OutF, Line2);
end;

function PlotRegFunc(X : Float) : Float;
{ ------------------------------------------------------------------
  Defines the function to be plotted
  ------------------------------------------------------------------ }
begin
  PlotRegFunc := RegFunc(Model, X, B);
end;

procedure PlotGraph(Index, N : Integer; X, Y : TVector);
{ ------------------------------------------------------------------
  Plots the fitted curve
  ------------------------------------------------------------------ }
var
  Xmin, Xmax, Xstep : Float;  { Ox scale }
  Ymin, Ymax, Ystep : Float;  { Oy scale }
begin
  if not InitGraphics(9, 2, 'c:\tp\bgi') then  { 640x480 16 color }
    begin
      Writeln('Unable to set graphic mode');
      Exit;
    end;

  SetWindow(15, 85, 15, 85, True);

  AutoScale(X, 1, N, LinScale, Xmin, Xmax, Xstep); if Xmin > 0.0 then Xmin := 0.0;
  AutoScale(Y, 1, N, LinScale, Ymin, Ymax, Ystep); if Ymin > 0.0 then Ymin := 0.0;

  SetOxScale(LinScale, Xmin, Xmax, Xstep);
  SetOyScale(LinScale, Ymin, Ymax, Ystep);

  SetGraphTitle(DATANAME[Index]);
  SetOxTitle('X');
  SetOyTitle('Y');

  PlotOxAxis;
  PlotOyAxis;

  WriteGraphTitle;

  SetClipping(True);

  SetLineParam(1, 0, 0, 0);  { Don't connect points }
  PlotCurve(X, Y, 1, N, 1);

  PlotFunc(@PlotRegFunc, Xmin, Xmax, 2);

  Readln;

  LeaveGraphics;
end;

procedure PlotPrec(MinPrec : TVector);
{ ------------------------------------------------------------------
  Plots the maximum relative error for each model
  ------------------------------------------------------------------ }
var
  X                 : TVector;  { Model index }
  I                 : Integer;  { Loop variable }
  Ymin, Ymax, Ystep : Float;    { Oy scale }
begin
  if not InitGraphics(9, 2, 'c:\tp\bgi') then  { 640x480 16 color }
    begin
      Writeln('Unable to set graphic mode');
      Exit;
    end;

  SetWindow(15, 85, 15, 85, True);

  DimVector(X, NDATASETS);
  for I := 1 to NDATASETS do
    X[I] := I;

  { Determine scale automatically }
  AutoScale(MinPrec, 1, NDATASETS, LinScale, Ymin, Ymax, Ystep);

  SetOxScale(LinScale, 1, NDATASETS, 1);
  SetOyScale(LinScale, Ymin, Ymax, Ystep);

  SetOxTitle('Model');
  SetOyTitle('Precision');

  PlotOxAxis;
  PlotOyAxis;

  { Plot points and connect them by lines }
  SetLineParam(1, 1, 1, 1);
  PlotCurve(X, MinPrec, 1, NDATASETS, 1);

  Readln;

  LeaveGraphics;
end;

begin
  Assign(OutF, 'nist.out');
  Rewrite(OutF);

  DimVector(MinPrec, NDATASETS);

  for Index := 1 to NDATASETS do
    begin
      Write('Data set: ', DATANAME[Index]);

      { Read data set and dimension arrays }
      ReadInputFile(Index, N, X, Y, U, Model, FirstPar, LastPar,
                    B_ref, S_ref, Sr_ref, R2_ref, F_ref);
      DimArrays(N, LastPar, B, V, Ycalc);

      { Set tolerance for Singular Value Decomposition }
      Tol := N * MachEp; SVDTol := Tol;

      { Set parameter bounds for nonlinear regression and perform a
        preliminary fit with the Simplex method, then switch to Marquardt }
      if Model.RegType in [REG_FRAC, REG_EXPO, REG_IEXPO, REG_POWER, REG_LOGIS] then
        begin
          SetBounds(FirstPar, LastPar);
          SetOptAlgo(NL_SIMP);
          FitModel(Model, X, Y, Ycalc, U, 1, N, MAXITER, TOL, SVDTol, B, V, Test);
          SetOptAlgo(NL_MARQ);
        end;

      FitModel(Model, X, Y, Ycalc, U, 1, N, MAXITER, TOL, SVDTol, B, V, Test);

      if MathErr = 0 then WriteLn(' - OK') else WriteLn(' - Error');

      WriteResults(OutF, Index, Model, FirstPar, LastPar, B_ref, S_ref,
                   B, V, Sr_ref, R2_ref, F_ref, Test, MinPrec);

      if (Index <> 2) and (MathErr = 0) then
        PlotGraph(Index, N, X, Y);

      { Delete old arrays }
      DelArrays(B, V, Ycalc);
    end;

  PlotPrec(MinPrec);

  Close(OutF);
  WriteLn('Results written in file nist.out');
end.

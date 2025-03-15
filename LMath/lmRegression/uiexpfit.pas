{ ******************************************************************
  This unit fits the increasing exponential :

                   y = Ymin + A.[1 - exp(-k.x)]

  ****************************************************************** }

unit uiexpfit;

interface

uses
  utypes, uErrors, umath, umeansd, ulinfit, unlfit;

{ Unweighted fit of model
  Input parameters:  X, Y     = point coordinates
                     Lb, Ub   = array bounds
                     ConsTerm = flag for presence of constant term (Ymin)
                     MaxIter  = max. number of iterations
                     Tol      = tolerance on parameters
  Output parameters: B        = regression parameters
                     V        = inverse matrix }
procedure IncExpFit(X, Y     : TVector;
                    Lb, Ub   : Integer;
                    ConsTerm : Boolean;
                    MaxIter  : Integer;
                    Tol      : Float;
                    B        : TVector;
                    V        : TMatrix);

{ Weighted fit of model
  Additional input parameter:
  S = standard deviations of observations }
procedure WIncExpFit(X, Y, S  : TVector;
                     Lb, Ub   : Integer;
                     ConsTerm : Boolean;
                     MaxIter  : Integer;
                     Tol      : Float;
                     B        : TVector;
                     V        : TMatrix);

{ Returns the value of the regression function at point X }
function IncExpFit_Func(X : Float; B : TVector) : Float;

implementation

const
  LastParam = 2;

var
  gConsTerm : Boolean = False;

function FirstParam : Integer;
{ ------------------------------------------------------------------
  Returns the index of the first parameter to be fitted
  (0 if there is a constant term Ymin, 1 otherwise)
  ------------------------------------------------------------------ }
begin
  if gConsTerm then
    FirstParam := 0
  else
    FirstParam := 1;
end;

function IncExpFit_Func(X : Float; B : TVector) : Float;
{ ------------------------------------------------------------------
  Computes the regression function at point X
  B is the vector of parameters, such that :

  B[0] = Ymin     B[1] = A     B[2] = k
  ------------------------------------------------------------------ }
begin
  if gConsTerm then
    IncExpFit_Func := B[0] + B[1] * (1.0 - Expo(- B[2] * X))
  else
    IncExpFit_Func := B[1] * (1.0 - Expo(- B[2] * X));
end;

procedure IncExpFit_Deriv(X, Y : Float; B, D : TVector);
{ --------------------------------------------------------------------
  Computes the derivatives of the regression function at point (X, Y)
  with respect to the parameters B. The results are returned in D.
  D[I] contains the derivative with respect to the I-th parameter.
  -------------------------------------------------------------------- }
var
  E : Float;
begin
  E := Expo(- B[2] * X);  { exp(-k.x) }
  D[0] := 1.0;            { dy/dYmin = 1 }
  D[1] := 1.0 - E;        { dy/dA = 1 - exp(-k.x) }
  D[2] := B[1] * X * E;  { dy/dk = A.x.exp(-k.x) }
end;

procedure ApproxFit(Mode : TRegMode; X, Y, S : TVector;
                    Lb, Ub : Integer; B : TVector);
{ ------------------------------------------------------------------
  Approximate fit of the increasing exponential by linear regression
  Ln(1 - z/A) = -k.x with z = y - Ymin
  ------------------------------------------------------------------
  Input :  Mode   = OLS for unweighted regression, WLS for weighted
           X, Y   = point coordinates
           S      = weights
           Lb, Ub = array bounds
  Output : B      = estimated regression parameters
  ------------------------------------------------------------------ }
var
  Ymax : Float;
  Z    : Float;    { Ymax - y }
  Y1   : TVector;  { Transformed ordinates }
  S1   : TVector;  { Standard deviations }
  A    : TVector;  { Linear regression parameters }
  V    : TMatrix;  { Variance-covariance matrix }
  P    : Integer;  { Number of points for linear regression }
  K    : Integer;  { Loop variable }
begin
  DimVector(Y1, Ub);
  DimVector(S1, Ub);
  DimVector(A, 1);
  DimMatrix(V, 1, 1);

  { Estimation of Ymin }
  if gConsTerm then
    B[0] := Min(Y, Lb, Ub)
  else
    B[0] := 0.0;

  { Estimation of Ymax = Ymin + A }
  Ymax := Max(Y, Lb, Ub);

  { Estimation of k }
  P := Pred(Lb);
  for K := Lb to Ub do
    begin
      Z := Ymax - Y[K];
      if Z > 0.0 then
        begin
          Inc(P);
          Y1[P] := Ln(Z);
          S1[P] := 1.0 / Z;
          if Mode = WLS then S1[P] := S1[P] * S[K];
        end;
    end;

  WLinFit(X, Y1, S1, Lb, P, A, V);

  if MathErr = MatOk then
    begin
      B[1] := Exp(A[0]);
      B[2] := - A[1];
    end;

end;

procedure GenIncExpFit(Mode     : TRegMode;
                       X, Y, S  : TVector;
                       Lb, Ub   : Integer;
                       ConsTerm : Boolean;
                       MaxIter  : Integer;
                       Tol      : Float;
                       B        : TVector;
                       V        : TMatrix);
begin
  gConsTerm := ConsTerm;

  if (GetOptAlgo in [NL_MARQ, NL_BFGS, NL_SIMP])
     and NullParam(B, FirstParam, LastParam) then
       ApproxFit(Mode, X, Y, S, Lb, Ub, B);

  if MaxIter = 0 then Exit;

  case Mode of
    OLS : NLFit(@IncExpFit_Func, @IncExpFit_Deriv, X, Y, Lb, Ub,
                       MaxIter, Tol, B, FirstParam, LastParam, V);
    WLS : WNLFit(@IncExpFit_Func, @IncExpFit_Deriv, X, Y, S, Lb, Ub,
                        MaxIter, Tol, B, FirstParam, LastParam, V);
  end;
end;

procedure IncExpFit(X, Y     : TVector;
                    Lb, Ub   : Integer;
                    ConsTerm : Boolean;
                    MaxIter  : Integer;
                    Tol      : Float;
                    B        : TVector;
                    V        : TMatrix);
begin
  GenIncExpFit(OLS, X, Y, nil, Lb, Ub, ConsTerm, MaxIter, Tol, B, V);
end;

procedure WIncExpFit(X, Y, S  : TVector;
                     Lb, Ub   : Integer;
                     ConsTerm : Boolean;
                     MaxIter  : Integer;
                     Tol      : Float;
                     B        : TVector;
                     V        : TMatrix);
begin
  GenIncExpFit(WLS, X, Y, S, Lb, Ub, ConsTerm, MaxIter, Tol, B, V);
end;

end.

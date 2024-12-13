{ ******************************************************************
  This unit fits the gamma variate regression model:

                y = a * (x - b)^c * exp[ - (x - b) / d]

  ****************************************************************** }

unit ugamfit;

interface

uses
  utypes, uErrors, umulfit, unlfit;

{ Unweighted fit of model
  Input parameters:  X, Y    = point coordinates
                     Lb, Ub  = array bounds
                     MaxIter = max. number of iterations
                     Tol     = tolerance on parameters
  Output parameters: B       = regression parameters
                     V       = inverse matrix }
procedure GammaFit(X, Y    : TVector;
                   Lb, Ub  : Integer;
                   MaxIter : Integer;
                   Tol     : Float;
                   B       : TVector;
                   V       : TMatrix);

{ Weighted fit of model
  Additional input parameter:
  S = standard deviations of observations }
procedure WGammaFit(X, Y, S : TVector;
                    Lb, Ub  : Integer;
                    MaxIter : Integer;
                    Tol     : Float;
                    B       : TVector;
                    V       : TMatrix);

{ Returns the value of the regression function at point X }
function GammaFit_Func(X : Float; B : TVector) : Float;

implementation

const
  FirstParam = 1;
  LastParam  = 4;

  { ------------------------------------------------------------------
    Computes the regression function at point X
    B is the vector of parameters, such that :

    B[1] = a     B[2] = b     B[3] = c     B[4] = d
    ------------------------------------------------------------------ }
function GammaFit_Func(X : Float; B : TVector) : Float;
var
  W, Z : Float;
begin
  if X <= B[2] then
    begin
      GammaFit_Func := 0.0;
      Exit;
    end;

  W := X - B[2];                    { x - b }
  Z := B[3] * Ln(W) - W / B[4];     { c * ln(x - b) - (x - b) / d }

  if Z > MinLog then
    GammaFit_Func := B[1] * Exp(Z)  { a * (x - b)^c * exp[ - (x - b) / d] }
  else
    GammaFit_Func := 0.0;
end;

procedure GammaFit_Deriv(X, Y : Float; B, D : TVector);
{ ------------------------------------------------------------------
  Computes the derivatives of the regression function at point X
  with respect to the parameters B. The results are returned in D.
  D[I] contains the derivative with respect to the I-th parameter.
  ------------------------------------------------------------------ }
var
  W : Float;
  I : Integer;
begin
  if X <= B[2] then
    begin
      for I := FirstParam to LastParam do D[I] := 0.0;
      Exit;
    end;

  W := X - B[2];                        { w = x - b }
  D[1] := Y / B[1];                     { dy/da = y / a }
  D[2] := Y * (1.0 / B[4] - B[3] / W);  { dy/db = y * (1 / d - c / w) }
  D[3] := Y * Ln(W);                    { dy/dc = y * Ln(w) }
  D[4] := Y * W / Sqr(B[4]);            { dy/dd = y * w / d^2 }
end;

procedure ApproxFit(Mode    : TRegMode;
                    X, Y, S : TVector;
                    Lb, Ub  : Integer;
                    B       : TVector);
{ ------------------------------------------------------------------
  Computes initial estimates of the regression parameters on the
  linearized form of the function:

  ln y = ln a + c * ln w - w / d    w = x - b
  ------------------------------------------------------------------
  Input :  Mode   = OLS or WLS
           X, Y   = point coordinates
           Lb, Ub = array bounds
  Output : B      = estimated regression parameters
  ------------------------------------------------------------------ }
var
  K  : Integer;
  W  : Float;    { x - b }
  Y1 : TVector;  { ln y }
  S1 : TVector;  { Standard deviations of ln y }
  U  : TMatrix;  { Variables for linear regression }
  A  : TVector;  { Linear regression parameters }
  P  : Integer;  { Nb of points for linear regression }
  V  : TMatrix;  { Variance-covariance matrix }

begin
  DimVector(Y1, Ub);
  DimVector(S1, Ub);
  DimMatrix(U, Ub, 2);
  DimVector(A, 2);
  DimMatrix(V, 2, 2);

  { Find the lowest X having a positive Y }
  W := MaxNum;
  for K := Lb to Ub do
    if (X[K] > 0.0) and (Y[K] > 0.0) and (X[K] < W) then
      W := X[K];

  { Set b slightly below this X value }
  B[2] := 0.9 * W;

  P := Pred(Lb);
  for K := Lb to Ub do
    if Y[K] > 0.0 then
      begin
        Inc(P);
        W := X[K] - B[2];
        U[P,1] := Ln(W);
        U[P,2] := W;
        Y1[P] := Ln(Y[K]);
        S1[P] := 1.0 / Y[K];
        if Mode = WLS then S1[P] := S1[P] * S[K];
      end;

  WMulFit(U, Y1, S1, Lb, P, 2, True, A, V);

  if MathErr = MatOk then
    begin
      B[1] := Exp(A[0]);
      B[3] := A[1];
      B[4] := - 1.0 / A[2];
    end;
end;

procedure GenGammaFit(Mode    : TRegMode;
                      X, Y, S : TVector;
                      Lb, Ub  : Integer;
                      MaxIter : Integer;
                      Tol     : Float;
                      B       : TVector;
                      V       : TMatrix);
begin
  if (GetOptAlgo in [NL_MARQ, NL_BFGS, NL_SIMP])
     and NullParam(B, FirstParam, LastParam) then
       ApproxFit(Mode, X, Y, S, Lb, Ub, B);

  if MaxIter = 0 then Exit;

  case Mode of
    OLS : NLFit(@GammaFit_Func, @GammaFit_Deriv, X, Y, Lb, Ub,
                       MaxIter, Tol, B, FirstParam, LastParam, V);
    WLS : WNLFit(@GammaFit_Func, @GammaFit_Deriv, X, Y, S, Lb, Ub,
                       MaxIter, Tol, B, FirstParam, LastParam, V);
  end;
end;

procedure GammaFit(X, Y    : TVector;
                   Lb, Ub  : Integer;
                   MaxIter : Integer;
                   Tol     : Float;
                   B       : TVector;
                   V       : TMatrix);
begin
  GenGammaFit(OLS, X, Y, nil, Lb, Ub, MaxIter, Tol, B, V);
end;

procedure WGammaFit(X, Y, S : TVector;
                    Lb, Ub  : Integer;
                    MaxIter : Integer;
                    Tol     : Float;
                    B       : TVector;
                    V       : TMatrix);
begin
  GenGammaFit(WLS, X, Y, S, Lb, Ub, MaxIter, Tol, B, V);
end;

end.

{ ******************************************************************
  This unit fits a power function :

                                y = A.x^n

  ****************************************************************** }

unit upowfit;

interface

uses
  utypes, uErrors, umath, ulinfit, unlfit;

{ Unweighted fit of model
  Input parameters:  X, Y    = point coordinates
                     Lb, Ub  = array bounds
                     MaxIter = max. number of iterations
                     Tol     = tolerance on parameters
  Output parameters: B       = regression parameters
                     V       = inverse matrix }
procedure PowFit(X, Y    : TVector;
                 Lb, Ub  : Integer;
                 MaxIter : Integer;
                 Tol     : Float;
                 B       : TVector;
                 V       : TMatrix);

{ Weighted fit of model
  Additional input parameter:
  S = standard deviations of observations }
procedure WPowFit(X, Y, S : TVector;
                  Lb, Ub  : Integer;
                  MaxIter : Integer;
                  Tol     : Float;
                  B       : TVector;
                  V       : TMatrix);

{ Computes the regression function at point X.
  B is the vector of parameters, such that :

  B[0] = A     B[1] = n }
function PowFit_Func(X : Float; B : TVector) : Float;

implementation

const
  FirstParam = 0;
  LastParam  = 1;

function PowFit_Func(X : Float; B : TVector) : Float;
begin
  PowFit_Func := B[0] * Power(X, B[1]);
end;

procedure PowFit_Deriv(X, Y : Float; B, D : TVector);
{ ------------------------------------------------------------------
  Computes the derivatives of the regression function at point (X,Y)
  with respect to the parameters B. The results are returned in D.
  D[I] contains the derivative with respect to the I-th parameter.
  ------------------------------------------------------------------ }
begin
  D[0] := Y / B[0];    { dy/dA = x^n }
  D[1] := Y * Log(X);  { dy/dk = A.x^n.Ln(x) }
end;

procedure ApproxFit(Mode : TRegMode; X, Y, S : TVector;
                    Lb, Ub : Integer; B : TVector);
{ ------------------------------------------------------------------
  Approximate fit of a power function by linear regression:
  Ln(y) = Ln(A) + n.Ln(x)
  ------------------------------------------------------------------
  Input :  Mode = OLS for unweighted regression, WLS for weighted
           X, Y = point coordinates
           W    = weights
           N    = number of points
  Output : B    = estimated regression parameters
  -------------------------------------------------------------------- }
var
  X1, Y1 : TVector;  { Transformed coordinates }
  S1     : TVector;  { Standard dev. }
  A      : TVector;  { Linear regression parameters }
  V      : TMatrix;  { Variance-covariance matrix }
  P      : Integer;  { Number of points for linear regression }
  K      : Integer;  { Loop variable }
begin
  DimVector(X1, Ub);
  DimVector(Y1, Ub);
  DimVector(S1, Ub);
  DimVector(A, 1);
  DimMatrix(V, 1, 1);

  P := Pred(Lb);
  for K := Lb to Ub do
    if (X[K] > 0.0) and (Y[K] > 0.0) then
      begin
        Inc(P);
        X1[P] := Log(X[K]);
        Y1[P] := Log(Y[K]);
        S1[P] := 1.0 / Y[K];
        if Mode = WLS then S1[P] := S1[P] * S[K];
      end;

  WLinFit(X1, Y1, S1, Lb, P, A, V);

  if MathErr = MatOk then
    begin
      B[0] := Expo(A[0]);
      B[1] := A[1];
    end;

end;

procedure GenPowFit(Mode    : TRegMode;
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
    OLS : NLFit(@PowFit_Func, @PowFit_Deriv, X, Y, Lb, Ub,
                MaxIter, Tol, B, FirstParam, LastParam, V);
    WLS : WNLFit(@PowFit_Func, @PowFit_Deriv, X, Y, S, Lb, Ub,
                 MaxIter, Tol, B, FirstParam, LastParam, V);
  end;
end;

procedure PowFit(X, Y    : TVector;
                 Lb, Ub  : Integer;
                 MaxIter : Integer;
                 Tol     : Float;
                 B       : TVector;
                 V       : TMatrix);
begin
  GenPowFit(OLS, X, Y, nil, Lb, Ub, MaxIter, Tol, B, V);
end;

procedure WPowFit(X, Y, S : TVector;
                  Lb, Ub  : Integer;
                  MaxIter : Integer;
                  Tol     : Float;
                  B       : TVector;
                  V       : TMatrix);
begin
  GenPowFit(WLS, X, Y, S, Lb, Ub, MaxIter, Tol, B, V);
end;

end.

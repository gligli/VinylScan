{ ******************************************************************
  This unit fits the Michaelis equation :

                                  Ymax . x
                              y = --------
                                   Km + x

  ****************************************************************** }

unit umichfit;

interface

uses
  utypes, uErrors, ulinfit, unlfit;

{ Unweighted fit of model
  Input parameters:  X, Y    = point coordinates
                     Lb, Ub  = array bounds
                     MaxIter = max. number of iterations
                     Tol     = tolerance on parameters
  Output parameters: B       = regression parameters
                     V       = inverse matrix }
procedure MichFit(X, Y    : TVector;
                  Lb, Ub  : Integer;
                  MaxIter : Integer;
                  Tol     : Float;
                  B       : TVector;
                  V       : TMatrix);

{ Weighted fit of model
  Additional input parameter:
  S = standard deviations of observations }
procedure WMichFit(X, Y, S : TVector;
                   Lb, Ub  : Integer;
                   MaxIter : Integer;
                   Tol     : Float;
                   B       : TVector;
                   V       : TMatrix);

{ Returns the value of the regression function at point X }
function MichFit_Func(X : Float; B : TVector) : Float;

implementation

const
  FirstParam = 0;
  LastParam  = 1;

function MichFit_Func(X : Float; B : TVector) : Float;
{ ------------------------------------------------------------------
  Computes the regression function at point X
  B is the vector of parameters, such that :

  B[0] = Ymax     B[1] = Km
  ------------------------------------------------------------------ }
begin
  MichFit_Func := B[0] * X / (B[1] + X);
end;

procedure MichFit_Deriv(X, Y : Float; B, D : TVector);
{ ------------------------------------------------------------------
  Computes the derivatives of the regression function at point (X,Y)
  with respect to the parameters B. The results are returned in D.
  D[I] contains the derivative with respect to the I-th parameter
  ------------------------------------------------------------------ }
begin
  D[0] := Y / B[0];          { dy/dYmax = x / (Km + x) }
  D[1] := - Y / (B[1] + X);  { dy/dKm = - Ymax.x / (Km + x)^2 }
end;

procedure ApproxFit(Mode : TRegMode; X, Y, S : TVector;
                    Lb, Ub : Integer; B : TVector);
{ ------------------------------------------------------------------
  Approximate fit of the Michaelis equation by linear regression:
  1/y = (1/Ymax) + (Km/Ymax) * (1/x)
  ------------------------------------------------------------------
  Input :  Mode   = OLS for unweighted regression, WLS for weighted
           X, Y   = point coordinates
           S      = standard deviations of Y values
           Lb, Ub = array bounds
  Output : B      = estimated regression parameters
  ------------------------------------------------------------------ }
var
  X1, Y1 : TVector;  { Transformed coordinates }
  S1     : TVector;  { Standard dev. of transformed Y values }
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
        X1[P] := 1.0 / X[K];
        Y1[P] := 1.0 / Y[K];
        S1[P] := 1.0 / Sqr(Y[K]);
        if Mode = WLS then S1[P] := S1[P] * S[K];
      end;

  WLinFit(X1, Y1, S1, Lb, P, A, V);

  if MathErr = MatOk then
    begin
      B[0] := 1.0 / A[0];
      B[1] := A[1] / A[0];
    end;
end;

procedure GenMichFit(Mode    : TRegMode;
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
    OLS : NLFit(@MichFit_Func, @MichFit_Deriv, X, Y, Lb, Ub,
                MaxIter, Tol, B, FirstParam, LastParam, V);
    WLS : WNLFit(@MichFit_Func, @MichFit_Deriv, X, Y, S, Lb, Ub,
                 MaxIter, Tol, B, FirstParam, LastParam, V);
  end;
end;

procedure MichFit(X, Y    : TVector;
                  Lb, Ub  : Integer;
                  MaxIter : Integer;
                  Tol     : Float;
                  B       : TVector;
                  V       : TMatrix);
begin
  GenMichFit(OLS, X, Y, nil, Lb, Ub, MaxIter, Tol, B, V);
end;

procedure WMichFit(X, Y, S : TVector;
                   Lb, Ub  : Integer;
                   MaxIter : Integer;
                   Tol     : Float;
                   B       : TVector;
                   V       : TMatrix);
begin
  GenMichFit(WLS, X, Y, S, Lb, Ub, MaxIter, Tol, B, V);
end;

end.

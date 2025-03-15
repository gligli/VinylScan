{ ******************************************************************
  This unit fits the "exponential + linear" model:

                        y = A.[1 - exp(-k.x)] + B.x

  ****************************************************************** }

unit uexlfit;

interface

uses
  utypes, umath, unlfit;

{ Unweighted fit of model
  
  Input parameters:  X, Y    = point coordinates
                     Lb, Ub  = array bounds
                     MaxIter = max. number of iterations
                     Tol     = tolerance on parameters
  Output parameters: B       = regression parameters
                     V       = inverse matrix }
procedure ExpLinFit(X, Y    : TVector;
                    Lb, Ub  : Integer;
                    MaxIter : Integer;
                    Tol     : Float;
                    B       : TVector;
                    V       : TMatrix);

{ Weighted fit of model
 
  Additional input parameter:
  S = standard deviations of observations }
procedure WExpLinFit(X, Y, S : TVector;
                     Lb, Ub  : Integer;
                     MaxIter : Integer;
                     Tol     : Float;
                     B       : TVector;
                     V       : TMatrix);

{ Returns the value of the regression function at point X }
 function ExpLinFit_Func(X : Float; B : TVector) : Float;

implementation

const
  FirstParam = 0;
  LastParam  = 2;

function ExpLinFit_Func(X : Float; B : TVector) : Float;
{ ------------------------------------------------------------------
  Computes the regression function at point X
  B is the vector of parameters, such that :

  B[0] = A     B[1] = k     B[2] = B
  ------------------------------------------------------------------ }
begin
  ExpLinFit_Func := B[0] * (1.0 - Expo(- B[1] * X)) + B[2] * X;
end;

procedure ExpLinFit_Deriv(X, Y : Float; B, D : TVector);
{ ------------------------------------------------------------------
  Computes the derivatives of the regression function at point X
  with respect to the parameters B. The results are returned in D.
  D[I] contains the derivative with respect to the I-th parameter.
  ------------------------------------------------------------------ }
  var
    E : Float;
  begin
    E := Expo(- B[1] * X);  { exp(-k.x) }
    D[0] := 1.0 - E;        { dy/dA = 1 - exp(-k.x) }
    D[1] := B[0] * X * E;   { dy/dk = A.x.exp(-k.x) }
    D[2] := X;              { dy/dB = x }
  end;

procedure ApproxFit(X, Y : TVector; Lb, Ub : Integer; B : TVector);
{ ------------------------------------------------------------------
  Computes initial estimates of the regression parameters
  ------------------------------------------------------------------
  Input :  X, Y   = point coordinates
           Lb, Ub = array bounds
  Output : B      = estimated regression parameters
  ------------------------------------------------------------------ }
  var
    K : Integer;
    D : Float;
  begin
    { B is the slope of the last (linear) part of the curve }
    K := Round(0.9 * Ub);
    if K = Ub then K := Pred(Ub);
    B[2] := (Y[Ub] - Y[K]) / (X[Ub] - X[K]);

    { A is the intercept of the linear part }
    B[0] := Y[Ub] - B[2] * X[Ub];

    { Slope of the tangent at origin = B + k.A }
    K := Round(0.1 * Ub);
    if K <= Lb then K := Succ(Lb);
    D := (Y[K] - Y[Lb]) / (X[K] - X[Lb]);
    B[1] := (D - B[2]) / B[0];
  end;

procedure GenExpLinFit(Mode    : TRegMode;
                       X, Y, S : TVector;
                       Lb, Ub  : Integer;
                       MaxIter : Integer;
                       Tol     : Float;
                       B       : TVector;
                       V       : TMatrix);
begin
  if (GetOptAlgo in [NL_MARQ, NL_BFGS, NL_SIMP])
     and NullParam(B, FirstParam, LastParam) then
       ApproxFit(X, Y, Lb, Ub, B);

  if MaxIter = 0 then Exit;

  case Mode of
    OLS : NLFit(@ExpLinFit_Func, @ExpLinFit_Deriv, X, Y, Lb, Ub,
                       MaxIter, Tol, B, FirstParam, LastParam, V);
    WLS : WNLFit(@ExpLinFit_Func, @ExpLinFit_Deriv, X, Y, S, Lb, Ub,
                       MaxIter, Tol, B, FirstParam, LastParam, V);
  end;
end;

procedure ExpLinFit(X, Y    : TVector;
                    Lb, Ub  : Integer;
                    MaxIter : Integer;
                    Tol     : Float;
                    B       : TVector;
                    V       : TMatrix);
begin
  GenExpLinFit(OLS, X, Y, nil, Lb, Ub, MaxIter, Tol, B, V);
end;

procedure WExpLinFit(X, Y, S : TVector;
                     Lb, Ub  : Integer;
                     MaxIter : Integer;
                     Tol     : Float;
                     B       : TVector;
                     V       : TMatrix);
begin
  GenExpLinFit(WLS, X, Y, S, Lb, Ub, MaxIter, Tol, B, V);
end;

end.

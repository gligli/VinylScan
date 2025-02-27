{ ******************************************************************
  This unit fits a rational fraction :

                           p0 + p1.x + p2.x^2 + ...
                       y = ------------------------
                           1 + q1.x + q2.x^2 + ...

  ****************************************************************** }

unit ufracfit;

interface

uses
  utypes, upolynom, umulfit, unlfit;

{ Unweighted fit of rational fraction
  Input parameters:  X, Y       = point coordinates
                     Lb, Ub     = array bounds
                     Deg1, Deg2 = degrees of numerator and denominator
                     ConsTerm   = presence of constant term p0
                     MaxIter    = max. number of iterations
                     Tol        = tolerance on parameters
  Output parameters: B          = regression parameters
                     V          = inverse matrix }
procedure FracFit(X, Y       : TVector;
                  Lb, Ub     : Integer;
                  Deg1, Deg2 : Integer;
                  ConsTerm   : Boolean;
                  MaxIter    : Integer;
                  Tol        : Float;
                  B          : TVector;
                  V          : TMatrix);

{ Weighted fit of rational fraction
  Additional input parameter:
  S = standard deviations of observations }
procedure WFracFit(X, Y, S    : TVector;
                   Lb, Ub     : Integer;
                   Deg1, Deg2 : Integer;
                   ConsTerm   : Boolean;
                   MaxIter    : Integer;
                   Tol        : Float;
                   B          : TVector;
                   V          : TMatrix);

{ Returns the value of the regression function at point X }
function FracFit_Func(X : Float; B : TVector) : Float;

implementation

var
  gDeg1     : Integer = 1;     { Degree of numerator }
  gDeg2     : Integer = 1;     { Degree of denominator }
  gConsTerm : Boolean = True;  { Flags the presence of a constant term p0 }

  function FirstParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the first parameter to be fitted
    (0 if there is a constant term p0, 1 otherwise)
    -------------------------------------------------------------------- }
  begin
    if gConsTerm then
      FirstParam := 0
    else
      FirstParam := 1;
  end;

  function LastParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the last parameter to be fitted
    -------------------------------------------------------------------- }
  begin
    LastParam := gDeg1 + gDeg2;
  end;

  function FracFit_Func(X : Float; B : TVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X
    B is the vector of parameters, such that :

    B[0] = p0
    B[1] = p1            B[2] = p2         ...

    B[Deg1 + 1] = q1     B[Deg1 + 2] = q2  ...
    -------------------------------------------------------------------- }
  begin
    FracFit_Func := RFrac(X, B, gDeg1, gDeg2);
  end;

  procedure FracFit_Deriv(X, Y : Float; B, D : TVector);
  { --------------------------------------------------------------------
    Computes the derivatives of the regression function at point (X,Y)
    with respect to the parameters B. The results are returned in D.
    D[I] contains the derivative with respect to the I-th parameter
    -------------------------------------------------------------------- }
  var
    I   : Integer;
    Den : Float;
  begin
    { Compute denominator (1 + q1.x + q2.x^2 + ...) }
    Den := 0.0;
    for I := (gDeg1 + gDeg2) downto Succ(gDeg1) do
      Den := (Den + B[I]) * X;
    Den := 1.0 + Den;

    { dy/dp0 = 1 / (1 + q1.x + q2.x^2 + ...) }
    D[0] := 1.0 / Den;

    { dy/dpi = x^i / (1 + q1.x + q2.x^2 + ...) }
    for I := 1 to gDeg1 do
      D[I] := D[I - 1] * X;

    { dy/dq1 = -x.y / (1 + q1.x + q2.x^2 + ...) }
    D[gDeg1 + 1] := - X * Y / Den;

    { dy/dqi = -x^i.y / (1 + q1.x + q2.x^2 + ...) }
    for I := (gDeg1 + 2) to (gDeg1 + gDeg2) do
      D[I] := D[I - 1] * X;
  end;

  procedure ApproxFit(Mode    : TRegMode;
                      X, Y, S : TVector;
                      Lb, Ub  : Integer;
                      B       : TVector);
  { --------------------------------------------------------------------
    Approximate fit of a rational fraction by linear regression:
    y = p0 + p1.x + p2.x^2 + ... - q1.(x.y) - q2.(x^2.y) - ...
    --------------------------------------------------------------------
    Input :  Mode   = OLS or WLS
             X, Y   = point coordinates
             S      = standard deviations
             Lb, Ub = array bounds
    Output : B      = estimated regression parameters
    -------------------------------------------------------------------- }
  var
    I, J : Integer;  { Loop variables }
    U    : TMatrix;  { Matrix of independent variables }
    V    : TMatrix;  { Variance-covariance matrix }
  begin
    DimMatrix(U, Ub, LastParam);
    DimMatrix(V, LastParam, LastParam);

    for I := Lb to Ub do
      begin
        U[I,1] := X[I];
        for J := 2 to gDeg1 do
          U[I,J] := U[I,J - 1] * X[I];
        U[I,gDeg1 + 1] := - X[I] * Y[I];
        for J := (gDeg1 + 2) to LastParam do
          U[I,J] := U[I,J - 1] * X[I];
      end;

    case Mode of
      OLS : MulFit(U, Y, Lb, Ub, LastParam, gConsTerm, B, V);
      WLS : WMulFit(U, Y, S, Lb, Ub, LastParam, gConsTerm, B, V);
    end;

    if not gConsTerm then B[0] := 0.0;

  end;

  procedure GenFracFit(Mode       : TRegMode;
                       X, Y, S    : TVector;
                       Lb, Ub     : Integer;
                       Deg1, Deg2 : Integer;
                       ConsTerm   : Boolean;
                       MaxIter    : Integer;
                       Tol        : Float;
                       B          : TVector;
                       V          : TMatrix);
  begin
    gDeg1 := Deg1;
    gDeg2 := Deg2;
    gConsTerm := ConsTerm;

    if (GetOptAlgo in [NL_MARQ, NL_BFGS, NL_SIMP])
      and NullParam(B, FirstParam, LastParam) then
        ApproxFit(Mode, X, Y, S, Lb, Ub, B);

    if MaxIter = 0 then Exit;

    case Mode of
      OLS : NLFit(@FracFit_Func, @FracFit_Deriv, X, Y, Lb, Ub,
              MaxIter, Tol, B, FirstParam, LastParam, V);
      WLS : WNLFit(@FracFit_Func, @FracFit_Deriv, X, Y, S, Lb, Ub,
              MaxIter, Tol, B, FirstParam, LastParam, V);
    end;
  end;

  procedure FracFit(X, Y       : TVector;
                    Lb, Ub     : Integer;
                    Deg1, Deg2 : Integer;
                    ConsTerm   : Boolean;
                    MaxIter    : Integer;
                    Tol        : Float;
                    B          : TVector;
                    V          : TMatrix);
  begin
    GenFracFit(OLS, X, Y, nil, Lb, Ub, Deg1, Deg2, ConsTerm, MaxIter, Tol, B, V);
  end;

  procedure WFracFit(X, Y, S    : TVector;
                     Lb, Ub     : Integer;
                     Deg1, Deg2 : Integer;
                     ConsTerm   : Boolean;
                     MaxIter    : Integer;
                     Tol        : Float;
                     B          : TVector;
                     V          : TMatrix);
  begin
    GenFracFit(WLS, X, Y, S, Lb, Ub, Deg1, Deg2, ConsTerm, MaxIter, Tol, B, V);
  end;

end.

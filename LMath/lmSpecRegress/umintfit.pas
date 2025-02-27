{ ******************************************************************
  This unit fits the Integrated Michaelis-Menten equation:

  y = s0 - Km * W(s0 / Km * exp((s0 - kcat * e0 * t) / Km))

  y    = product concentration at time t
  s0   = initial substrate concentration
  Km   = Michaelis constant
  kcat = catalytic constant
  e0   = total enzyme concentration

  W is Lambert's function (reciprocal of x * exp(x))

  The independent variable x may be:

  *  t  ==> fitted parameters : s0 (optional), Km, Vmax = kcat * e0
  *  s0 ==> fitted parameters : Km, (Vmax * t)
  *  e0 ==> fitted parameters : s0 (optional), Km, (kcat * t),
  ****************************************************************** }

unit umintfit;

interface

uses
  utypes, umath, uErrors, ulambert, umeansd, ulinfit, unlfit;

{ Unweighted fit of model
  Input parameters:  X, Y    = point coordinates
                     Lb, Ub  = array bounds
                     MintVar = independant var. (Var_T, Var_S, Var_E)
                     Fit_S0  = indicates if s0 must be fitted
                               (for Var_T or Var_E only)
                     MaxIter = max. number of iterations
                     Tol     = tolerance on parameters
                     B[0]   = initial value of s0
  Output parameters: B       = regression parameters
                     V       = inverse matrix }
procedure MintFit(X, Y    : TVector;
                  Lb, Ub  : Integer;
                  MintVar : TMintVar;
                  Fit_S0  : Boolean;
                  MaxIter : Integer;
                  Tol     : Float;
                  B       : TVector;
                  V       : TMatrix);

{ Weighted fit of model
  Additional input parameter:
  S = standard deviations of observations }
procedure WMintFit(X, Y, S : TVector;
                   Lb, Ub  : Integer;
                   MintVar : TMintVar;
                   Fit_S0  : Boolean;
                   MaxIter : Integer;
                   Tol     : Float;
                   B       : TVector;
                   V       : TMatrix);

{Returns the value of the regression function at point X }
function MintFit_Func(X : Float; B : TVector) : Float;

implementation

const
  LastParam = 2;

var
  gMintVar : TMintVar;
  gFit_S0  : Boolean;

function FirstParam : Integer;
{ --------------------------------------------------------------------
  Returns the index of the first parameter to be fitted
  (0 if s0 is fitted, 1 otherwise)
  -------------------------------------------------------------------- }
begin
  if (gMintVar <> Var_S) and gFit_S0 then
    FirstParam := 0
  else
    FirstParam := 1;
end;

function MintFit_Func(X : Float; B : TVector) : Float;
{ ------------------------------------------------------------------
  Computes the regression function at point X
  ------------------------------------------------------------------ }
begin
  if gMintVar = Var_S then
    MintFit_Func :=
      X - B[1] *
        LambertW(X / B[1] * Expo((X - B[2]) / B[1]),
                 True, False)
  else
    MintFit_Func :=
      B[0] - B[1] *
        LambertW(B[0] / B[1] * Expo((B[0] - B[2] * X) / B[1]),
                 True, False);
end;

procedure MintFit_Deriv(X, Y : Float; B, D : TVector);
{ ------------------------------------------------------------------
  Computes the derivatives of the regression function at point X
  with respect to the parameters B. The results are returned in D.
  D[I] contains the derivative with respect to the I-th parameter.
  ------------------------------------------------------------------ }
var
  L, Q, R : Float;
begin
  if gMintVar = Var_S then
    begin
      L := (X - Y) / B[1];
      D[2] := L / (1.0 + L);
      D[1] := D[2] * (Y - B[2]) / B[1];
    end
  else
    begin
      L := (B[0] - Y) / B[1];
      Q := 1.0 / (1.0 + L);
      R := L * Q;
      if gFit_S0 then D[0] := Y * Q / B[0];
      D[1] := R * (Y - B[2] * X) / B[1];
      D[2] := R * X;
    end;
end;

procedure ApproxFit(X, Y : TVector; Lb, Ub : Integer; B : TVector);
{ ------------------------------------------------------------------
  Computes initial estimates of the regression parameters by linear
  regression:

  x = t  ==> y / x = Vmax       + Km (1 / x) Ln(1 - y / s0)
  x = s0 ==> y     = (Vmax * t) + Km Ln(1 - y / s0)
  x = e0 ==> y / x = (kcat * t) + Km (1 / x) Ln(1 - y / s0)
  ------------------------------------------------------------------
  Input :  X, Y   = point coordinates
           Lb, Ub = array bounds
  Output : B      = estimated regression parameters
  ------------------------------------------------------------------ }
  var
    I      : Integer;
    XX, YY : TVector;
    A      : TVector;
    V      : TMatrix;
  begin
    DimVector(XX, Ub);
    DimVector(YY, Ub);
    DimVector(A, 1);
    DimMatrix(V, 1, 1);
    { Compute transformed coordinates }
    if gMintVar = Var_S then
      for I := Lb to Ub do
      begin
        if (X[I] < 0) or IsZero(X[I],MachEp) then
        begin
          SetErrCode(FDomain,'Substrate concentration is not positive.');
          Exit;
        end;
        XX[I] := Ln(1.0 - Y[I] / X[I]);
        YY[I] := Y[I];
      end
    else begin
      if B[0] <= 0.0 then
        B[0] := 1.1 * Max(Y, Lb, Ub);
      for I := Lb to Ub do
      begin
        if IsZero(X[I],MachEp) then
        begin
          SetErrCode(FSing,'Singularity: time or enzyme concentration is zero');
          Exit;
        end;
        XX[I] := Ln(1.0 - Y[I] / B[0]) / X[I];
        YY[I] := Y[I] / X[I];
      end;
    end;
    { Perform linear regression }
    LinFit(XX, YY, Lb, Ub, A, V);
    { Retrieve parameters }
    B[1] := A[1];            { Km }
    B[2] := A[0];            { Vmax or (Vmax * t) or (kcat * t) }
  end;

procedure GenMintFit(Mode    : TRegMode;
                     X, Y, S : TVector;
                     Lb, Ub  : Integer;
                     MintVar : TMintVar;
                     Fit_S0  : Boolean;
                     MaxIter : Integer;
                     Tol     : Float;
                     B       : TVector;
                     V       : TMatrix);
begin
  gMintVar := MintVar;
  gFit_S0 := Fit_S0;
  if (GetOptAlgo in [NL_MARQ, NL_BFGS, NL_SIMP])
                 and NullParam(B, FirstParam, LastParam) then
    ApproxFit(X, Y, Lb, Ub, B);
  if (MathErr <> MatOK) or (MaxIter = 0) then Exit;
  case Mode of
    OLS : NLFit(@MintFit_Func, @MintFit_Deriv, X, Y, Lb, Ub,
                MaxIter, Tol, B, FirstParam, LastParam, V);
    WLS : WNLFit(@MintFit_Func, @MintFit_Deriv, X, Y, S, Lb, Ub,
                 MaxIter, Tol, B, FirstParam, LastParam, V);
  end;
end;

procedure MintFit(X, Y    : TVector;
                  Lb, Ub  : Integer;
                  MintVar : TMintVar;
                  Fit_S0  : Boolean;
                  MaxIter : Integer;
                  Tol     : Float;
                  B       : TVector;
                  V       : TMatrix);
begin
  GenMintFit(OLS, X, Y, nil, Lb, Ub, MintVar, Fit_S0, MaxIter, Tol, B, V);
end;

procedure WMintFit(X, Y, S : TVector;
                   Lb, Ub  : Integer;
                   MintVar : TMintVar;
                   Fit_S0  : Boolean;
                   MaxIter : Integer;
                   Tol     : Float;
                   B       : TVector;
                   V       : TMatrix);
begin
  GenMintFit(WLS, X, Y, S, Lb, Ub, MintVar, Fit_S0, MaxIter, Tol, B, V);
end;

end.

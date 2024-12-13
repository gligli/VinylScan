{ ******************************************************************
  This unit fits the acid/base titration function :

                                    B - A
                       y = A + ----------------
                               1 + 10^(pKa - x)

  where x   is pH
        y   is some property (e.g. absorbance) which depends on the
            ratio of the acidic and basic forms of the compound
        A   is the property for the pure acidic form
        B   is the property for the pure basic form
        pKa is the acidity constant
  ****************************************************************** }

unit upkfit;

interface

uses
  utypes, uErrors, umath, ulinfit, unlfit;

procedure PKFit(X, Y    : TVector;
                Lb, Ub  : Integer;
                MaxIter : Integer;
                Tol     : Float;
                B       : TVector;
                V       : TMatrix);

procedure WPKFit(X, Y, S : TVector;
                 Lb, Ub  : Integer;
                 MaxIter : Integer;
                 Tol     : Float;
                 B       : TVector;
                 V       : TMatrix);

{ Computes the regression function at point X
  B is the vector of parameters, such that :
  B[0] = A     B[1] = B     B[2] = pKa }
function PKFit_Func(X : Float; B : TVector) : Float;

implementation

const
  FirstParam = 0;
  LastParam  = 2;

function PKFit_Func(X : Float; B : TVector) : Float;
begin
  PKFit_Func := B[0] + (B[1] - B[0]) / (1.0 + Exp10(B[2] - X));
end;

procedure PKFit_Deriv(X, Y : Float; B, D : TVector);
{ ------------------------------------------------------------------
  Computes the derivatives of the regression function at point X
  with respect to the parameters B. The results are returned in D.
  D[I] contains the derivative with respect to the I-th parameter.
  ------------------------------------------------------------------ }
var
  Q, R : Float;
begin
  Q := Exp10(B[2] - X);  { 10^(pKa - x) }
  R := 1.0 / (1.0 + Q);  { 1/[1 + 10^(pKa - x)] }

  D[0] := 1.0 - R;       { dy/dA = 1 - 1/[1 + 10^(pKa - x)] }
  D[1] := R;             { dy/dB = 1/[1 + 10^(pKa - x)] }

  { dy/dpKa = (A-B).10^(pKa - x).Ln(10) / [1 + 10^(pKa - x)]^2 }
  D[2] := (B[0] - B[1]) * Q * Ln10 * Sqr(R);
end;

procedure ApproxFit(Mode : TRegMode; X, Y, S : TVector;
                    Lb, Ub : Integer; B : TVector);
{ ------------------------------------------------------------------
  Approximate fit of the acid-base titration curve by linear
  regression:
  Ln[(B - A)/(y - A) - 1] = (pKa - x) * Ln(10)
  ------------------------------------------------------------------
  Input :  Mode   = OLS for unweighted regression, WLS for weighted
           X, Y   = point coordinates
           S      = standard deviations
           Lb, Ub = array bounds
  Output : B      = estimated regression parameters
  ------------------------------------------------------------------ }
var
  XX   : TVector;  { Transformed X coordinates }
  YY   : TVector;  { Transformed Y coordinates }
  SS   : TVector;  { Weights }
  A    : TVector;  { Linear regression parameters }
  V    : TMatrix;  { Variance-covariance matrix }
  P    : Integer;  { Number of points for linear regression }
  K    : Integer;  { Loop variable }
  Xmin : Float;    { Minimal X coordinate }
  Xmax : Float;    { Maximal X coordinate }
  Imin : Integer;  { Index of Xmin, such that A ~ Y[Imin] }
  Imax : Integer;  { Index of Xmax, such that B ~ Y[Imax] }
  DB   : Float;    { B - A }
  DY   : Float;    { Y - A }
  Z    : Float;    { Transformed Y coordinate }
begin
  DimVector(XX, Ub);
  DimVector(YY, Ub);
  DimVector(SS, Ub);
  DimVector(A, 1);
  DimMatrix(V, 1, 1);

  Xmin := X[Lb]; Imin := Lb;
  Xmax := X[Ub]; Imax := Ub;

  for K := Lb to Ub do
    if X[K] < Xmin then
      begin
        Xmin := X[K];
        Imin := K;
      end
    else if X[K] > Xmax then
      begin
        Xmax := X[K];
        Imax := K;
      end;

  B[0] := Y[Imin];
  B[1] := Y[Imax];

  DB := B[1] - B[0];

  P := Pred(Lb);
  for K := Lb to Ub do
    if Y[K] <> B[0] then
      begin
        DY := Y[K] - B[0];
        Z := DB / DY - 1.0;
        if Z > 0.0 then
          begin
            Inc(P);
            XX[P] := X[K];
            YY[P] := Ln(Z);
            SS[P] := Abs(DB / (Z * Sqr(DY)));
            if Mode = WLS then SS[P] := SS[P] * S[K];
          end;
      end;

  WLinFit(XX, YY, SS, Lb, P, A, V);

  if MathErr = MatOk then
    B[2] := A[0] * InvLn10;

end;

procedure GenPKFit(Mode     : TRegMode;
                   X, Y, S  : TVector;
                   Lb, Ub   : Integer;
                   MaxIter  : Integer;
                   Tol      : Float;
                   B        : TVector;
                   V        : TMatrix);
begin
  if (GetOptAlgo in [NL_MARQ, NL_BFGS, NL_SIMP])
     and NullParam(B, FirstParam, LastParam) then
       ApproxFit(Mode, X, Y, S, Lb, Ub, B);

  if MaxIter = 0 then Exit;

  case Mode of
    OLS : NLFit(@PKFit_Func, @PKFit_Deriv, X, Y, Lb, Ub,
                MaxIter, Tol, B, FirstParam, LastParam, V);
    WLS : WNLFit(@PKFit_Func, @PKFit_Deriv, X, Y, S, Lb, Ub,
                 MaxIter, Tol, B, FirstParam, LastParam, V);
  end;
end;

procedure PKFit(X, Y    : TVector;
                Lb, Ub  : Integer;
                MaxIter : Integer;
                Tol     : Float;
                B       : TVector;
                V       : TMatrix);
begin
  GenPKFit(OLS, X, Y, nil, Lb, Ub, MaxIter, Tol, B, V);
end;

procedure WPKFit(X, Y, S : TVector;
                 Lb, Ub  : Integer;
                 MaxIter : Integer;
                 Tol     : Float;
                 B       : TVector;
                 V       : TMatrix);
begin
  GenPKFit(WLS, X, Y, S, Lb, Ub, MaxIter, Tol, B, V);
end;

end.

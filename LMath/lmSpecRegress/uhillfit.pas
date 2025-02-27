{ ******************************************************************
  This unit fits the Hill equation :

                          (B - A) 
              y = A +  -------------
                       1 + (K / x)^n

    n > 0 for an increasing curve
    n < 0 for a decreasing curve
  ****************************************************************** }

unit uhillfit;

interface

uses
  utypes, uErrors, umath, uminmax, ulinfit, unlfit;

{ Unweighted fit of model
  Input parameters:  X, Y     = point coordinates
                     Lb, Ub   = array bounds
                     ConsTerm = presence of constant term A
                     MaxIter  = max. number of iterations
                     Tol      = tolerance on parameters
  Output parameters: B        = regression parameters
                     V        = inverse matrix }
procedure HillFit(X, Y     : TVector;
                  Lb, Ub   : Integer;
                  ConsTerm : Boolean;
                  MaxIter  : Integer;
                  Tol      : Float;
                  B        : TVector;
                  V        : TMatrix);

{ Weighted fit of model
  Additional input parameter:
  S = standard deviations of observations }
procedure WHillFit(X, Y, S  : TVector;
                   Lb, Ub   : Integer;
                   ConsTerm : Boolean;
                   MaxIter  : Integer;
                   Tol      : Float;
                   B        : TVector;
                   V        : TMatrix);

{ Computes the regression function at point X.
  B is the vector of parameters, such that :
  B[0] = A     B[1] = B     B[2] = K     B[3] = n }
function HillFit_Func(X : Float; B : TVector) : Float;

implementation

var
  gConsTerm : Boolean = False;  { Flags the presence of a constant term A }

const
  LastParam = 3;

function FirstParam : Integer;
{ ------------------------------------------------------------------
  Returns the index of the first parameter to be fitted
  (0 if there is a constant term A, 1 otherwise)
  ------------------------------------------------------------------ }
begin
  if gConsTerm then
    FirstParam := 0
  else
    FirstParam := 1;
end;

function HillFit_Func(X : Float; B : TVector) : Float;
var
  D : Float;
begin
  if X = 0.0 then
    begin
      if gConsTerm then
        HillFit_Func := B[0]
      else if B[3] > 0.0 then
        HillFit_Func := 0.0
      else
        HillFit_Func := B[1];
      Exit;
    end;
  
  D := 1.0 + Power(B[2] / X, B[3]);

  if gConsTerm then
    HillFit_Func := B[0] + (B[1] - B[0]) / D
  else
    HillFit_Func := B[1] / D;
end;

procedure HillFit_Deriv(X, Y : Float; B, D : TVector);
{ ------------------------------------------------------------------
  Computes the derivatives of the regression function at point (X,Y)
  with respect to the parameters B. The results are returned in D.
  D[I] contains the derivative with respect to the I-th parameter
  ------------------------------------------------------------------ }
var
  Q, R, S : Float;
begin
  if X = 0.0 then
    begin
      if gConsTerm then
        if B[3] > 0.0 then D[0] := 1.0 else D[0] := 0.0;

      if B[3] > 0.0 then D[1] := 0.0 else D[1] := 1.0;

      D[2] := 0.0;
      D[3] := 0.0;

      Exit;
    end;

  Q := Power(B[2] / X, B[3]);  { (K/x)^n }
  R := 1.0 / (1.0 + Q);        { 1 / [1 + (K/x)^n] }
  S := - Y * R * Q;            { (A - B) (K/x)^n / [1 + (K/x)^n]^2 }

  if gConsTerm then D[0] := 1.0 - R;
  
  D[1] := R;

  { dy/dK = (n/K) (A - B) (K/x)^n / [1 + (K/x)^n]^2 }
  D[2] := (B[3] / B[2]) * S;

  { dy/dn = Ln(K/x) (A - B) (K/x)^n / [1 + (K/x)^n]^2 }
  D[3] := Log(B[2] / X) * S;
end;

procedure ApproxFit(Mode : TRegMode; X, Y, S : TVector;
                    Lb, Ub : Integer; B : TVector);
{ ------------------------------------------------------------------
  Approximate fit of the Hill equation by linear regression:
  Ln[(B - A)/(y - A) - 1] = n ln K - n ln x
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

  if gConsTerm then
    begin
      B[0] := Y[Imin];
      B[1] := Y[Imax];
    end  
  else
    begin
      B[0] := 0.0;
      B[1] := Max(Y[Imin], Y[Imax]);
    end;  
  
  DB := B[1] - B[0];

  P := Pred(Lb);
  for K := Lb to Ub do
    if Y[K] <> B[0] then
      begin
        DY := Y[K] - B[0];
        Z := DB / DY - 1.0;
        if (Z > 0.0) and (X[K] > 0.0) then
          begin
            Inc(P);
            XX[P] := Ln(X[K]);
            YY[P] := Ln(Z);
            SS[P] := Abs(DB / (Z * Sqr(DY)));
            if Mode = WLS then SS[P] := SS[P] * S[K];
          end;
      end;

  WLinFit(XX, YY, SS, Lb, P, A, V);

  if MathErr = MatOk then
    begin
      B[3] := - A[1];
      B[2] := Expo(A[0] / B[3]);
    end;
end;

procedure GenHillFit(Mode     : TRegMode;
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
    OLS : NLFit(@HillFit_Func, @HillFit_Deriv, X, Y, Lb, Ub,
                MaxIter, Tol, B, FirstParam, LastParam, V);
    WLS : WNLFit(@HillFit_Func, @HillFit_Deriv, X, Y, S, Lb, Ub,
                 MaxIter, Tol, B, FirstParam, LastParam, V);
  end;
end;

procedure HillFit(X, Y     : TVector;
                  Lb, Ub   : Integer;
                  ConsTerm : Boolean;
                  MaxIter  : Integer;
                  Tol      : Float;
                  B        : TVector;
                  V        : TMatrix);
begin
  GenHillFit(OLS, X, Y, nil, Lb, Ub, ConsTerm, MaxIter, Tol, B, V);
end;

procedure WHillFit(X, Y, S  : TVector;
                   Lb, Ub   : Integer;
                   ConsTerm : Boolean;
                   MaxIter  : Integer;
                   Tol      : Float;
                   B        : TVector;
                   V        : TMatrix);
begin
  GenHillFit(WLS, X, Y, S, Lb, Ub, ConsTerm, MaxIter, Tol, B, V);
end;

end.

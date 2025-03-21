{ ******************************************************************
  Principal component analysis
  ****************************************************************** }

unit upca;

interface

uses
  utypes, uErrors, ueigsym;

{ Computes the mean vector (M) from matrix X

  Input  : X[Lb..Ub, 1..Nvar] = matrix of variables
  Output : M[1..Nvar]         = mean vector }
procedure VecMean(X            : TMatrix;
                  Lb, Ub, Nvar : Integer;
                  M            : TVector);

{ Computes the vector of standard deviations (S) from matrix X

  Input  : X, Lb, Ub, Nvar, M
  Output : S[1..Nvar] }
procedure VecSD(X            : TMatrix;
                Lb, Ub, Nvar : Integer;
                M, S         : TVector);

{ Computes the variance-covariance matrix (V) from matrix X

  Input  : X, Lb, Ub, Nvar, M
  Output : V[1..Nvar, 1..Nvar] }
procedure MatVarCov(X            : TMatrix;
                    Lb, Ub, Nvar : Integer;
                    M            : TVector;
                    V            : TMatrix);

{ Computes the correlation matrix (R) from the variance-covariance
  matrix (V)

  Input  : V, Nvar
  Output : R[1..Nvar, 1..Nvar] }
procedure MatCorrel(V    : TMatrix;
                    Nvar : Integer;
                    R    : TMatrix);

{ Performs a principal component analysis of the correlation matrix R

  Input  : R[1..Nvar, 1..Nvar]  = Correlation matrix
  Output : Lambda[1..Nvar]      = Eigenvalues of the correlation matrix
                                  (in descending order)
           C[1..Nvar, 1..Nvar]  = Eigenvectors of the correlation matrix
                                  (stored as columns)
           Rc[1..Nvar, 1..Nvar] = Correlations between principal factors
                                  and variables (Rc[I,J] is the
                                  correlation coefficient between
                                  variable I and factor J)
  NB : This procedure destroys the original matrix R }
procedure PCA(R      : TMatrix;
              Nvar   : Integer;
              Lambda : TVector;
              C, Rc  : TMatrix);

{ Scales a set of variables by subtracting means and dividing by SD's

  Input  : X, Lb, Ub, Nvar, M, S
  Output : Z[Lb..Ub, 1..Nvar] = matrix of scaled variables }
procedure ScaleVar(X            : TMatrix;
                   Lb, Ub, Nvar : Integer;
                   M, S         : TVector;
                   Z            : TMatrix);

{ Computes principal factors
  Input  : Z[Lb..Ub, 1..Nvar]  = matrix of scaled variables
           C[1..Nvar, 1..Nvar] = matrix of eigenvectors from PCA
  Output : F[Lb..Ub, 1..Nvar]  = matrix of principal factors }
procedure PrinFac(Z            : TMatrix;
                  Lb, Ub, Nvar : Integer;
                  C, F         : TMatrix);

implementation

procedure VecMean(X            : TMatrix;
                  Lb, Ub, Nvar : Integer;
                  M            : TVector);
var
  I, K, Nobs : Integer;
  Sum        : Float;

begin
  Nobs := Ub - Lb + 1;
  for I := 1 to Nvar do
    begin
      Sum := 0.0;
      for K := Lb to Ub do
        Sum := Sum + X[K,I];
      M[I] := Sum / Nobs;
    end;
end;

procedure VecSD(X            : TMatrix;
                Lb, Ub, Nvar : Integer;
                M, S         : TVector);
var
  I, K, Nobs : Integer;
  Sum        : Float;
begin
  Nobs := Ub - Lb + 1;
  for I := 1 to Nvar do
    begin
      Sum := 0.0;
      for K := Lb to Ub do
        Sum := Sum + Sqr(X[K,I] - M[I]);
      S[I] := Sqrt(Sum / Nobs);
    end;
end;

procedure MatVarCov(X            : TMatrix;
                    Lb, Ub, Nvar : Integer;
                    M            : TVector;
                    V            : TMatrix);
var
  I, J, K, Nobs : Integer;
  Sum           : Float;
begin
  Nobs := Ub - Lb + 1;

  for I := 1 to Nvar do
    for J := I to Nvar do
      begin
        Sum := 0.0;
        for K := Lb to Ub do
          Sum := Sum + (X[K,I] - M[I]) * (X[K,J] - M[J]);
        V[I,J] := Sum / Nobs;
      end;

  for I := 2 to Nvar do
    for J := 1 to Pred(I) do
      V[I,J] := V[J,I];
end;

procedure MatCorrel(V    : TMatrix;
                    Nvar : Integer;
                    R    : TMatrix);
var
  I, J : Integer;
  P    : Float;
begin
  for I := 1 to Nvar do
    begin
      R[I,I] := 1.0;
      for J := Succ(I) to Nvar do
        begin
          P := V[I,I] * V[J,J];
          if P > 0.0 then
            R[I,J] := V[I,J] / Sqrt(P)
          else
            R[I,J] := 0.0;
          R[J,I] := R[I,J];
        end;
    end;
end;

procedure PCA(R      : TMatrix;
              Nvar   : Integer;
              Lambda : TVector;
              C, Rc  : TMatrix);
var
  I, J : Integer;
  Rac  : Float;
begin
  { Compute eigenvalues and eigenvectors of correlation matrix }
  EigenSym(R, 1, Nvar, Lambda, C);
  
  if MathErr <> MatOk then Exit;

  { Compute correlations between principal factors and reduced variables }
  for J := 1 to Nvar do
    begin
      Rac := Sqrt(Lambda[J]);
      for I := 1 to Nvar do
        Rc[I,J] := C[I,J] * Rac;
    end;
end;

procedure ScaleVar(X            : TMatrix;
                   Lb, Ub, Nvar : Integer;
                   M, S         : TVector;
                   Z            : TMatrix);
var
  I, J : Integer;
begin
  for I := Lb to Ub do
    for J := 1 to Nvar do
      Z[I,J] := (X[I,J] - M[J]) / S[J];
end;

procedure PrinFac(Z            : TMatrix;
                  Lb, Ub, Nvar : Integer;
                  C, F         : TMatrix);
var
  I, J, K : Integer;
begin
  for I := Lb to Ub do
    for J := 1 to Nvar do
      begin
        F[I,J] := 0.0;
        for K := 1 to Nvar do
          F[I,J] := F[I,J] + Z[I,K] * C[K,J];
      end;
end;

end.

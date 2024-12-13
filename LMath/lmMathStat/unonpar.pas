{******************************************************************
 Non-parametric tests
******************************************************************}

unit unonpar;

interface

uses
  utypes, uminmax;

{ Mann-Whitney test
  N1 and N2 are the sample sizes
  X1[1..N1] and X2[1..N2] are the two samples.
  The procedure returns Mann-Whitney's statistic in U and the
  associated normal variable in Eps }
procedure Mann_Whitney(N1, N2     : Integer;
                       X1, X2     : TVector;
                       out U, Eps : Float);

{ Wilcoxon test
  X[Lb..Ub] and Y[Lb..Ub] are the two samples.
  Output: the number of non-zero dierences in Ndiff,
  Wilcoxon's statistic in T
  and the associated normal variable in Eps }
procedure Wilcoxon(X, Y       : TVector;
                   Lb, Ub     : Integer;
                   out Ndiff  : Integer;
                   out T, Eps : Float);

{ Kruskal-Wallis test
  Ns is the number of samples,
  N[1..Ns] is the vector of sizes and
  X the sample matrix (with the samples as columns).
  Output: Kruskal-Wallis statistic in H and the number of d. o. f. in DoF. }
procedure Kruskal_Wallis(Ns      : Integer;
                         N       : TIntVector;
                         X       : TMatrix;
                         out H   : Float;
                         out DoF : Integer);

implementation

procedure Ranks(Ns       : Integer;
                N        : TIntVector;
                X        : TMatrix;
                Sr       : TVector;
                out Corr : Float);

{ ------------------------------------------------------------------
  Compute ranks for non-parametric tests
  ------------------------------------------------------------------
  Sr   = sum of ranks for each sample
  Corr = correction for ties = Sum k(k^2 - 1)  k = nb of ties
  ------------------------------------------------------------------ }

var
  I, J, K : Integer;
  Nt      : Integer;
  Obs     : TVector;
  Group   : TIntVector;
  A, R, T : Float;

label
  L1;
    
begin
  Nt := 0;
  for I := 1 to Ns do
    Nt := Nt + N[I];
    
  DimVector(Obs, Nt);
  DimVector(Group, Nt);

  { Store data into Obs and Group vectors }
    
  K := 0;
  for I := 1 to Ns do
    for J := 1 to N[I] do
      begin
        Inc(K);
        Obs[K] := X[J,I];
        Group[K] := I;
      end;     

  { Sort array (insertion sort) }
  
  for I := 1 to Pred(Nt) do
    begin
      K := I;
      A := Obs[I];
      for J := Succ(I) to Nt do
        if Obs[J] < A then
          begin
            K := J;
            A := Obs[J];
          end;
      Swap(Obs[I], Obs[K]);
      Swap(Group[I], Group[K]);
    end;

  { Replace values in array Obs by their ranks 
    (algorithm adapted from 'Numerical Recipes') }
  
  Corr := 0;
  I := 1;
  
  while I < Nt do
    begin
      if Obs[I+1] <> Obs[I] then
        begin
          Obs[I] := I;
          Inc(I);
        end
      else
        begin
          for J := Succ(I) to Nt do
            if Obs[J] <> Obs[I] then goto L1;
          J := Nt + 1;
L1:       R := 0.5 * (I + J - 1);              
          for K := I to Pred(J) do
            Obs[K] := R;
          T := J - I;
          Corr := Corr + T * (Sqr(T) - 1.0);
          I := J;
        end;
    end;
  if I = Nt then Obs[Nt] := Nt;          
  
  { Compute sums of ranks }
  
  for I := 1 to Ns do
    Sr[I] := 0.0;
    
  for I := 1 to Nt do
    begin
      K := Group[I];
      Sr[K] := Sr[K] + Obs[I];
    end;
end;

procedure Mann_Whitney(N1, N2     : Integer;
                       X1, X2     : TVector;
                       out U, Eps : Float);

var
  Nmax, I         : Integer;
  N               : TIntVector;
  X               : TMatrix;
  Sr              : TVector;
  Sum, Prod, Corr : Float;
  U1, U2, MU, VU  : Float;

begin
  if N1 > N2 then Nmax := N1 else Nmax := N2;

  DimVector(N, 2);
  DimVector(Sr, 2);
  DimMatrix(X, Nmax, 2);

  N[1] := N1;
  N[2] := N2;

  for I := 1 to N1 do     { Copy X1 into first column of X }
    X[I,1] := X1[I];

  for I := 1 to N2 do     { Copy X2 into second column of X }
    X[I,2] := X2[I];

  Ranks(2, N, X, Sr, Corr);

  Sum := N1 + N2;
  Prod := N1 * N2;

  U1 := Prod + N1 * (N1 + 1) / 2 - Sr[1];
  U2 := Prod + N2 * (N2 + 1) / 2 - Sr[2];

  if U1 > U2 then U := U2 else U := U1;

  MU := Prod / 2;
  VU := Prod * ((Sum + 1) - Corr / Sum / (Sum - 1)) / 12;

  Eps := (U - MU) / Sqrt(VU);

end;

procedure Wilcoxon(X, Y       : TVector;
                   Lb, Ub     : Integer;
                   out Ndiff  : Integer;
                   out T, Eps : Float);

var
  J, J1, J2, N       : Integer;
  Diff, MT, VT, Corr : Float;
  D                  : TMatrix;
  ND                 : TIntVector;
  Sr                 : TVector;

begin
  N := Ub - Lb + 1;

  DimMatrix(D, N, 2);
  DimVector(ND, 2);
  DimVector(Sr, 2);

  J1 := 0; J2 := 0;
  for J := Lb to Ub do
    begin
      Diff := X[J] - Y[J];
      if Diff < 0 then
        begin
          Inc(J1);
          D[J1,1] := Abs(Diff);  { Negative difference }
        end
      else if Diff > 0 then
        begin
          Inc(J2);
          D[J2,2] := Diff;       { Positive difference }
        end;
    end;

  ND[1] := J1;      { Nb of negative differences }
  ND[2] := J2;      { Nb of positive differences }
  Ndiff := J1 + J2;  { Nb of non-null differences }

  Ranks(2, ND, D, Sr, Corr);

  if Sr[1] > Sr[2] then T := Sr[2] else T := Sr[1];

  MT := N * (N + 1) / 4;
  VT := MT * (2 * N + 1) / 6 - Corr / 48;
  Eps := (T - MT) / Sqrt(VT);

end;

procedure Kruskal_Wallis(Ns      : Integer;
                         N       : TIntVector;
                         X       : TMatrix;
                         out H   : Float;
                         out DoF : Integer);

var
  I, NT   : Integer;
  S, Corr : Float;
  Sr      : TVector;

begin
  DimVector(Sr, Ns);

  Ranks(Ns, N, X, Sr, Corr);

  S := 0.0; NT := 0;
  for I := 1 to Ns do
    begin
      S := S + Sqr(Sr[I]) / N[I];
      NT := NT + N[I];
    end;

  H := 12 * S / NT / (NT + 1) - 3 * (NT + 1);
  H := H / (1 - Corr / NT / (Sqr(NT) - 1));
  DoF := Pred(Ns);

end;

end.

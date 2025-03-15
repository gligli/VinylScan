{ ******************************************************************
  Khi-2 test
  ****************************************************************** }

unit ukhi2;

interface

uses
  utypes;

{ Khi-2 test for conformity
  N cls is the number of classes
  N estim the number of estimated parameters
  Obs[1..N cls] and Calc[1..N cls] the observed and theoretical distributions.
  The statistic is returned in Khi2 and the number of d. o. f. in DoF. }
procedure Khi2_Conform(N_cls    : Integer;
                       N_estim  : Integer;
                       Obs      : TIntVector;
                       Calc     : TVector;
                       out Khi2 : Float;
                       out DoF  : Integer);

{ Khi-2 test for independence
  N_lin and N_col are the numbers of lines and columns
  Obs[1..N lin, 1..N col] is the matrix of observed distributions.
  The statistic is returned in G and the number of d. o. f. in DoF. }
procedure Khi2_Indep(N_lin    : Integer;
                     N_col    : Integer;
                     Obs      : TIntMatrix;
                     out Khi2 : Float;
                     out DoF  : Integer);

implementation

procedure Khi2_Conform(N_cls    : Integer;
                       N_estim  : Integer;
                       Obs      : TIntVector;
                       Calc     : TVector;
                       out Khi2 : Float;
                       out DoF  : Integer);

var
  I : Integer;

begin
  Khi2 := 0.0;

  for I := 1 to N_cls do
  if Calc[I] = 0 then
    Khi2 := Khi2 + Sqr(Obs[I])
  else
    Khi2 := Khi2 + Sqr(Obs[I] - Calc[I]) / Calc[I];
  DoF := N_cls - N_estim - 1;
end;

procedure Khi2_Indep(N_lin    : Integer;
                     N_col    : Integer;
                     Obs      : TIntMatrix;
                     out Khi2 : Float;
                     out DoF  : Integer);

var
  SumLin, SumCol : TIntVector;
  Sum            : Integer;
  Prob, Calc     : Float;
  I, J           : Integer;

begin
  DimVector(SumLin, N_lin);
  DimVector(SumCol, N_col);

  for I := 1 to N_lin do
    for J := 1 to N_col do
      SumLin[I] := SumLin[I] + Obs[I,J];

  for J := 1 to N_col do
    for I := 1 to N_lin do
      SumCol[J] := SumCol[J] + Obs[I,J];

  Sum := 0;
  for I := 1 to N_lin do
    Sum := Sum + SumLin[I];

  Khi2 := 0.0;
  for I := 1 to N_lin do
    begin
      Prob := SumLin[I] / Sum;
      for J := 1 to N_col do
        begin
          Calc := SumCol[J] * Prob;
          Khi2 := Khi2 + Sqr(Obs[I,J] - Calc) / Calc;
        end;
    end;

  DoF := Pred(N_lin) * Pred(N_col);
end;

end.

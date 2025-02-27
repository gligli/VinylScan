{ ******************************************************************
  Woolf test
  ****************************************************************** }

unit uwoolf;

interface

uses
  utypes, uErrors;

{ Woolf test for conformity
  N cls is the number of classes
  N estim the number of estimated parameters
  Obs[1..N cls] and Calc[1..N cls] the observed and theoretical distributions.
  The statistic is returned in G and the number of d. o. f. in DoF. }
procedure Woolf_Conform(N_cls   : Integer;
                        N_estim : Integer;
                        Obs     : TIntVector;
                        Calc    : TVector;
                        out G   : Float;
                        out DoF : Integer);

{ Woolf test for independence
  N_lin and N_col are the numbers of lines and columns
  Obs[1..N lin, 1..N col] is the matrix of observed distributions.
  The statistic is returned in G and the number of d. o. f. in DoF. }
 procedure Woolf_Indep(N_lin   : Integer;
                      N_col   : Integer;
                      Obs     : TIntMatrix;
                      out G   : Float;
                      out DoF : Integer);

implementation

procedure Woolf_Conform(N_cls   : Integer;
                        N_estim : Integer;
                        Obs     : TIntVector;
                        Calc    : TVector;
                        out G   : Float;
                        out DoF : Integer);

var
  I : Integer;

begin
  for I := 1 to N_cls do
    if (Obs[I] <= 0) or (Calc[I] <= 0.0) then
      begin
        SetErrCode(FSing);
        Exit
      end;

  SetErrCode(FOk);

  G := 0.0;
  for I := 1 to N_cls do
    G := G + Obs[I] * Ln(Obs[I] / Calc[I]);

  G := 2.0 * G;
  DoF := N_cls - N_estim - 1;
end;

procedure Woolf_Indep(N_lin   : Integer;
                      N_col   : Integer;
                      Obs     : TIntMatrix;
                      out G   : Float;
                      out DoF : Integer);

var
  SumLin, SumCol : TIntVector;
  Sum            : Integer;
  Prob, Calc     : Float;
  I, J           : Integer;

begin
  for I := 1 to N_lin do
    for J := 1 to N_col do
      if Obs[I,J] <= 0 then
        begin
          SetErrCode(FSing);
          Exit
        end;

  SetErrCode(FOk);

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

  G := 0.0;
  for I := 1 to N_lin do
    begin
      Prob := SumLin[I] / Sum;
      for J := 1 to N_col do
        begin
          Calc := SumCol[J] * Prob;
          G := G + Obs[I,J] * Ln(Obs[I,J] / Calc);
        end;
    end;

  G := 2.0 * G;
  DoF := Pred(N_lin) * Pred(N_col);
end;

end.

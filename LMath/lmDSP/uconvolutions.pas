unit uConvolutions;

{$mode objfpc}
interface

uses
  uTypes, uMinMax, uErrors, uVectorHelper;

//Convolutes Signal[Lb..Ub] with FIR[Flb..High(Fir)] in time domain.
function Convolve(constref Signal:array of Float; constref FIR:array of float; Ziel : TVector= nil):TVector;

implementation
function Convolve(constref Signal:array of float; constref FIR:array of float; Ziel : TVector= nil):TVector;
var
  I, J, LS, LR, LF, Ind, HF, Ub : integer;
  B:Float;
begin
  LS := length(Signal);
  LF := length(FIR);
  LR := LF + LS - 1; // length of Result
  HF := LF - 1;
  Ub := LS - 1;
  if Ziel <> nil then
  begin
    if length(Ziel) < LR then
    begin
      SetErrCode(MatErrDim);
      Result := nil;
      Exit;
    end;
    Ziel.Clear;
  end else
  begin
    DimVector(Ziel,LR-1);
    if Ziel = nil then
    begin
      SetErrCode(MatErrDim,'Too long array or no memory');
      Result := nil;
      Exit;
    end;
  end;

  Ind := -HF;
  I := 0;
  while I <= HF do
  begin
    for J := -Ind to min(HF,Ub-Ind) do
      Ziel[I] := Ziel[I] + Signal[Ind+J]*Fir[HF - J];
    inc(I); inc(Ind);
  end;

  while I < LR-LF do
  begin
    for J := 0 to HF do
      Ziel[I] := Ziel[I] + Signal[Ind+J]*FIR[HF - J];
    inc(I); inc(Ind);
  end;

  while I < LR do
  begin
    for J := 0 to min(HF,Ub-Ind) do
      Ziel[I] := Ziel[I] + Signal[Ind+J]*FIR[HF - J];
    inc(I); inc(Ind);
  end;

  Result := Ziel;
end;

end.


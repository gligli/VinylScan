{ ******************************************************************
  Median and quantiles
  ****************************************************************** }

unit umedian;

interface

uses
  utypes, uMinMax, uMeanSD, uVectorHelper, uErrors;
type
  // upon return from function quantiles contains min in [0]; Q1,Q2,Q3 in [1..3]
  // respectively, and Max in [4]
  TQuantiles = array[0..4] of Float;
{Returns median for vector X. This is fast algorithm, but input array is rearranged! }
function Median(X : TVector; Lb, Ub : Integer) : Float; overload;
function Median(X:array of float) : float; overload;

// Returns quantiles (minimum, Q1 (lower quartile), Q2 (median), Q3 (upper quartile)
// and maximum in corresponding elements of output array Q.
// By odd number of elements in X, median is included in both upper and lower
// halves by finding Q1 and Q3, similar to function FiveNum in "R"
procedure Quantiles(constref X:array of Float; var Q: TQuantiles);

implementation

function Median(X: TVector; Lb, Ub: integer): float;
var
   I,J,M,L, Ubb:integer;
   A,B:float;
begin
   L := Ub-Lb+1;
   Ubb := Ub;
   M := Lb + (Ub-Lb) div 2;
   while Lb <= Ub - 1 do
   begin
     A := X[M];
     I := Lb; J := Ub;
     repeat
       while X[I] < A do inc(I);
       while X[J] > A do dec(J);
       if I <= J then
       begin
         Swap(X[I],X[J]);
         inc(I); dec(J);
       end;
     until I > J;
     if J < M then Lb := I;
     if I > M then Ub := J;
   end;
   if (L mod 2) = 1 then
     Result := X[M]
   else
     Result := (X[M] + min(X, M+1, Ubb))/2;
end;

function Median(X: array of float): float;
var
   I,J,M:integer;
   A,B:float;
   Lb, Ub, Ubb : integer;
begin
   Lb := 0;
   Ub := high(X);
   Ubb := Ub;
   M := Ub div 2;
   while Lb < Ub do
   begin
     A := X[M];   // middle element
     I := Lb; J := Ub;
     repeat
       while X[I] < A do inc(I);
       while X[J] > A do dec(J);
       if I <= J then
       begin
         Swap(X[I],X[J]);
         inc(I); dec(J);
       end;
     until I > J;  // everything lesser A on the left, bigger on the right
     if J < M then Lb := I;  // more elements on the right -> median somewhere on the right.
     if I > M then Ub := J;  // more elements on the left
   end;
   if (Ubb + 1) mod 2 = 1 then
     Result := X[M]
   else
     Result := (X[M] + min(X[M+1..Ubb]))/2;
end;

procedure Quantiles(constref X: array of Float; var Q: TQuantiles);
var
  V:TVector;
  L,B,B4,I:integer;
  Even : boolean;
begin
  V.FillWithArr(0,X);
  L := length(V);
  if L = 0 then
  begin
    SetErrCode(lmStatEmptyData);
    for I := 0 to 4 do
      Q[I] := 0;
    exit;
  end
  else if L = 1 then
  begin
    for I := 0 to 4 do
      Q[I] := X[0];
    exit;
  end;
  Even := (L mod 2) = 0;
  if Even then
    B := L div 2 - 1
  else
    B := L div 2;
  B4 := B div 2;
  Q[2] := Median(V,0,L-1); // Now V is partitioned
  Q[1] := Median(V,0,B);
  if Even then
    Q[3] := Median(V,B+1,L-1)
  else
    Q[3] := Median(V,B,L-1); // and now partitioned to 4 parts
  Q[0] := min(V[0..B4]);
  Q[4] := max(V[B4+1..L-1]);
end;


end.


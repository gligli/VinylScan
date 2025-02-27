{ ******************************************************************
  Rounding functions
  Based on FreeBASIC version contributed by R. Keeling
  ****************************************************************** }

unit uround;

interface

uses
  utypes, uminmax, umath;

{ Rounds X to N decimal places }
function RoundTo(X : Float; N : Integer) : Float;

{ Ceiling function }
function Ceil(X : Float) : Integer;

{ Floor function }
function Floor(X : Float) : Integer;

implementation

function RoundTo (X : Float; N : Integer) : Float;
const
  MaxRoundPlaces = 18;
var
  ReturnAnswer, Dec_Place : Float;
  I : Integer;
begin
  if (N >= 0) and (N < MaxRoundPlaces) then I := N else I := 0;
  Dec_Place := Exp10(I);
  ReturnAnswer := Int((Abs(X) * Dec_Place) + 0.5);
  RoundTo := Sgn(X) * ReturnAnswer / Dec_Place;
end;

function Ceil(X : Float) : Integer;
var
  ReturnAnswer : Integer;
begin
  ReturnAnswer := Trunc(X);
  if ReturnAnswer < X then ReturnAnswer := ReturnAnswer + 1;
  Ceil := ReturnAnswer;
end;

function Floor(X : Float) : Integer;
var
  ReturnAnswer : Integer;
begin
   ReturnAnswer := Trunc(X);
   if ReturnAnswer > X then ReturnAnswer := ReturnAnswer - 1;
   Floor := ReturnAnswer;
end;

end.

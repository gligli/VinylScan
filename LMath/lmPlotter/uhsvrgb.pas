{ *******************************************************************
  HSV / RGB conversion
  ******************************************************************* 
  Adapted from http://www.cs.rit.edu/~ncs/color/t_convert.html
  R, G, B values are from 0 to 255
  H = [0..360), S = [0..1], V = [0..1]
  if S = 0, then H is undefined
  ******************************************************************* }

unit uhsvrgb;

interface

uses
  utypes;

procedure HSVtoRGB(H, S, V : Float; var R, G, B : Byte);

procedure RGBtoHSV(R, G, B : Byte; var H, S, V : Float);

implementation

var
  RR, GG, BB : Float;
  
procedure SetRGB(X, Y, Z : Float);
begin
  RR := X;
  GG := Y;
  BB := Z;
end;
  
procedure HSVtoRGB(H, S, V : Float; var R, G, B : Byte);
var
  I : Integer;
  Z, F, P, Q, T : Float;
begin
  if S = 0 then    { achromatic (grey) }
    begin
      I := Trunc(V * 255);
      R := I;
      G := I;
      B := I;
      Exit;
    end;

  Z := H / 60;     { sector 0 to 5 }
  I := Trunc(Z);
  F := Frac(Z);
  P := V * (1 - S);
  Q := V * (1 - S * F);
  T := V * (1 - S * (1 - F));

  case I of
    0 : SetRGB(V, T, P);
    1 : SetRGB(Q, V, P);
    2 : SetRGB(P, V, T);
    3 : SetRGB(P, Q, V);
    4 : SetRGB(T, P, V);
  else
    SetRGB(V, P, Q);
  end;

  R := Trunc(RR * 255);
  G := Trunc(GG * 255);
  B := Trunc(BB * 255);
end;

function Min3(X, Y, Z : Float) : Float;
begin
  Result := X;
  if Y < Result then Result := Y;
  if Z < Result then Result := Z;
end;

function Max3(X, Y, Z : Float) : Float;
begin
  Result := X;
  if Y > Result then Result := Y;
  if Z > Result then Result := Z;
end;

procedure RGBtoHSV(R, G, B : Byte; var H, S, V : Float);
var
  Min, Max, Delta : Float;
begin
  RR := R / 255;
  GG := G / 255;
  BB := B / 255;

  Min := Min3(RR, GG, BB);
  Max := Max3(RR, GG, BB);

  V := Max;

  if (Max = 0) or (Max = Min) then
    begin
      S := 0;
      H := 0;
      Exit;
    end;

  Delta := Max - Min;

  S := Delta / Max;

  if RR = Max then
    H := Trunc(60 * (GG - BB) / Delta + 360) mod 360  
  else if GG = Max then
    H := 60 * (BB - RR) / Delta + 120 
  else
    H := 60 * (RR - GG) / Delta + 240;
end;

end.

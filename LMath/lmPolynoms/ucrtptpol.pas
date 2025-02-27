unit ucrtptpol;
{***************************************************
 * Finding polynom derivative and critical points  *
 ***************************************************}
{$mode objfpc}{$H+}
interface

uses
  uTypes, uErrors, uMinMax, uPolynom, urootpol, uPolUtil;

{finds derivative of polynom, which is polynom of lesser degree
Coef: coefficients of polynom
Deg: degree of polynom
DCoef: coefficience of derivative polynom
DDeg: degree of derivative polynom (Deg - 1)}
procedure DerivPolynom(Coef: TVector; Deg: integer; var DCoef: TVector; out DDeg: integer);

{finds extremums of polynom
Coef: coefficients of polynom
Deg: degree of polynom
CRTPoints: Critical points; CRTPoints[i].X abscissa, CRTPoints[y] - function value at each
PointTypes: type of critical point. -1: minimum, 0: no extremum; +1: maximum}
function CriticalPoints(Coef:TVector; Deg:integer; var CrtPoints: TRealPointVector;
                                      var PointTypes: TIntVector; ResLb : integer = 1):integer;


implementation

procedure DerivPolynom(Coef: TVector; Deg: integer; var DCoef: TVector; out DDeg: integer);
var
  I:Integer;
begin
  if (Deg < 0) or ((Deg = 0) and (Coef[0] = 0)) then
  begin
    SetErrCode(FDomain);
    DDeg := 0;
    Exit;
  end;
  if length(DCoef) < Deg - 1 then
    SetLength(DCoef,max(Deg,1));
  if Deg = 0 then
  begin
    DDeg := 0;
    DCoef[0] := 0;
  end else
  begin
    DDeg := Deg - 1;
    for I := 1 to Deg do
      DCoef[I-1] := Coef[I]*I;
  end;
  SetErrCode(MatOK);
end;

function CriticalPoints(Coef: TVector; Deg: integer; var CrtPoints: TRealPointVector;
         var PointTypes: TIntVector; ResLb : integer = 1): integer;
var
  Deriv:TVector;
  DDeg:integer;
  Z:TCompVector;
  I, J, NR:integer;
  D1,D2:Float;
begin
  if Deg < 2 then
    Result := 0
  else begin
    Result := 0;
    DimVector(Deriv,Deg-1);
    DerivPolynom(Coef,Deg,Deriv,DDeg);
    DimVector(Z,DDeg);
    NR := RootPol(Deriv,DDeg,Z);
    SortRoots(DDeg,Z);
    J := ResLb;
    I := 1;
    if NR > 0 then
    begin
      if length(CrtPoints) < NR + ResLb then
         SetLength(CrtPoints,Nr + ResLb);
      if length(PointTypes) < Nr + ResLb then
         SetLength(PointTypes,Nr + ResLb);
      D1 := Poly(Z[1].X - 1,Deriv,DDeg); // value of derivative before first critical point
      while I <= NR do
      begin
        CrtPoints[J].X := Z[I].X;
        CrtPoints[J].Y := Poly(CrtPoints[J].X,Coef,Deg);
        while SameValue(Z[I].X,Z[I+1].X) and (I < Nr) do Inc(I); // if next root of derivative is equal to this one, it is skipped
        if I = NR then
          D2 := Poly(Z[I].X + 0.2,Deriv,DDeg)
        else
          D2 := Poly(Z[I].X + (Z[I+1].X - Z[I].X)/2,Deriv,DDeg); // derivative between critical points
        if Sign(D1) = Sign(D2) then
          PointTypes[J] := 0
        else
          PointTypes[J] := Sign(D1); // 1 is maximum
        D1 := D2;
        Inc(I);
        inc(J);
      end;
    end;
    Result := J - ResLb;
  end;
end;

end.


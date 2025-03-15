{ ******************************************************************
  Roots of a polynomial from the companion matrix
  ****************************************************************** }

unit urootpol;

interface

uses
  utypes, uErrors, urtpol1, urtpol2, urtpol3, urtpol4, ubalance, uhqr;
  
{ Solves the polynomial equation:
  Coef(0) + Coef(1) * Z + Coef(2) * Z^2 + ...
                        + Coef(Deg) * Z^Deg = 0 }
function RootPol(Coef : TVector; Deg : Integer; Z : TCompVector) : Integer;

implementation

function RootPolMat(Coef : TVector; Deg : Integer; Z : TCompVector) : Integer;
{ Roots of polynomial from companion matrix }

var
  Lo, Hi : Integer;  { Used by Balance }
  I, J   : Integer;  { Loop variables  }
  Nr     : Integer;  { Number of real roots }
  A      : TMatrix;  { Companion matrix }
  Scale  : TVector;  { Used by Balance }

begin
  { Dimension arrays }
  DimMatrix(A, Deg, Deg);
  DimVector(Scale, Deg);

  { Set up the companion matrix }
  for J := 1 to Deg do
    A[1,J] := - Coef[Deg - J] / Coef[Deg];

  for I := 2 to Deg do
    for J := 1 to Deg do
      if I - 1 = J then A[I,J] := 1.0 else A[I,J] := 0.0;

  { The roots of the polynomial are the
    eigenvalues of the companion matrix }
  Balance(A, 1, Deg, Lo, Hi, Scale);
  Hqr(A, 1, Deg, Lo, Hi, Z);

  if MathErr <> 0 then
    begin
      RootPolMat := MathErr;
      Exit;
    end;

  { Count real roots }
  Nr := 0;
  for I := 1 to Deg do
    if Z[I].Y = 0.0 then Nr := Nr + 1;

  RootPolMat := Nr
end;

function RootPol(Coef : TVector; Deg : Integer; Z : TCompVector) : Integer;
begin
  if Deg < 1 then
    begin
      SetErrCode(FDomain);
      Result := 0;
      Exit;
    end;
      
  case Deg of
    1 : RootPol := RootPol1(Coef[0], Coef[1], Z[1].X);
    2 : RootPol := RootPol2(Coef, Z);
    3 : RootPol := RootPol3(Coef, Z);
    4 : RootPol := RootPol4(Coef, Z);
  else
        RootPol := RootPolMat(Coef, Deg, Z);
  end;
end;

end.

{$mode objfpc}
{$MODESWITCH TYPEHELPERS}
program PolyDeriv;
{Find derivative and critical points of polynom
2*x^4+3*x^3+x-2}
uses uTypes, uVectorHelper, ucrtptpol, uVecMatPrn, uStrings;
const
  Deg = 4; // degree of the polynome
  MaxS = '       Maximum: ';
  MinS = '       Minimum: ';
  CrtS = 'Critical Point: ';
var
  Coefs, DCoefs : TVector; //0-based coefficients of input and output (derivative) polynome
  DDeg : integer;  // degree of derivative polynome
  CrtP : TRealPointVector;
  NCrtP: integer;
  PointTypes: TIntVector;
  I:integer;
begin
  vprnlb := 0; // PrintVector begins from V[0]
  Coefs.FillWithArr(0,[-2,1,0,3,2]);
  DerivPolynom(Coefs, Deg, DCoefs, DDeg);
  writeln('Degree of input array: ',Deg);
  writeln('Coefficients of input polynome: ');
  writeln('Degree of input array: ',DDeg);
  printvector(Coefs);
  writeln('Coefficients of derivative array:');
  printVector(DCoefs);
  DimVector(CrtP,Deg);
  DimVector(PointTypes,Deg);
  writeln('Look for critical points.');
  NCrtP := CriticalPoints(Coefs,Deg,CrtP,PointTypes);
  if NCrtP = 0 then
   writeln('No critical points found.')
  else
    for I := 1 to NCrtP do
    begin
      case PointTypes[I] of
        -1: write(MinS);
         0: write(CrtS);
         1: write(MaxS);
      end;
      writeln(FloatStr(CrtP[I].X),';',FloatStr(CrtP[I].Y));
    end;
 readln;
end.


program extremum;
{$mode objfpc}{$H+}
uses uTypes, ucrtptpol, uStrings;
const
  MaxS = '       Maximum: ';
  MinS = '       Minimum: ';
  CrtS = 'Critical Point: ';
var
  I, N, Deg: integer;
  Coeffs: TVector;
  CrtPoints: TRealPointVector;
  PointTypes:TIntVector;
begin
  repeat
    writeln('Enter polynom degree. Enter -1 to exit.');
    readln(Deg);
    writeln('Now input coeffinients beginning from free member to highest order:');
    writeln('One in a line.');
    DimVector(Coeffs, Deg);
    for I := 0 to Deg do
      readln(Coeffs[I]);
    N := CriticalPoints(Coeffs,Deg,CrtPoints,PointTypes,0);
    if N = 0 then
      writeln('No critical points found.')
    else
      for I := 0 to N-1 do
      begin
        case PointTypes[I] of
          -1: write(MinS);
           0: write(CrtS);
           1: write(MaxS);
        end;
        writeln(FloatStr(CrtPoints[I].X),';',FloatStr(CrtPoints[I].Y));
      end;
  until Deg = -1;
end.


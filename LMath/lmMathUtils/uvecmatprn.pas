unit uVecMatPrn;

{$mode objfpc}{$H+}

interface

uses
  UTypes, SysUtils;

const
  vprnFmtStr : string = '%8.3f';
  vprnIntFmtStr : string = '%4d';

var
  vprnLB : integer = 1;

procedure PrintVector(V:array of integer); overload;
procedure PrintVector(V:array of float); overload;

procedure PrintMatrix(A:TMatrix);

implementation

procedure PrintMatrix(A:TMatrix);
var
  I,J:integer;
begin
  for I := vprnLB to High(A) do
  begin
    for J := vprnLB to High(A[0]) do
      write(Format(vprnFmtStr,[A[i,j]]));
    writeln;
  end;
end;

procedure PrintVector(V:array of integer);
var
  J:integer;
begin
    for J := 0 to High(V) do
      write(Format(vprnIntFmtStr,[V[j]]));
    writeln;
end;

procedure PrintVector(V:array of float);
var
  J:integer;
begin
    for J := vprnLB to High(V) do
      write(Format(vprnFmtStr,[V[j]]));
    writeln;
end;

end.


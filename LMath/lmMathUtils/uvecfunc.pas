unit uVecFunc;
{$mode objfpc}
interface
uses
  uTypes;

procedure VecAbs(V : TVector; Lb, Ub : integer); overload;
procedure VecAbs(V : TIntVector; Lb, Ub : integer); overload;
procedure MatAbs(M : TMatrix; Lb1, Ub1, Lb2, Ub2 : integer); overload;
procedure MatAbs(M : TIntMatrix; Lb1, Ub1, Lb2, Ub2 : integer); overload;

procedure VecSqr(V : TVector; Lb, Ub : integer); overload;
procedure VecSqr(V : TIntVector; Lb, Ub : integer); overload;
procedure MatSqr(M : TMatrix; Lb1, Ub1, Lb2, Ub2 : integer); overload;
procedure MatSqr(M : TIntMatrix; Lb1, Ub1, Lb2, Ub2 : integer); overload;

procedure VecSqrt(V : TVector; Lb, Ub : integer);
procedure MatSqrt(M : TMatrix; Lb1, Ub1, Lb2, Ub2 : integer);

implementation

procedure VecAbs(V: TVector; Lb, Ub: integer);
var
  I:Integer;
begin
  for I := Lb to Ub do
    V[I] := Abs(V[I]);
end;

procedure VecAbs(V: TIntVector; Lb, Ub: integer);
var
  I:Integer;
begin
  for I := Lb to Ub do
    V[I] := Abs(V[I]);
end;

procedure MatAbs(M: TMatrix; Lb1, Ub1, Lb2, Ub2: integer);
var
  I,J:Integer;
begin
  for I := Lb1 to Ub1 do
    for J := Lb2 to Ub2 do
      M[I,J] := abs(M[I,J]);
end;

procedure MatAbs(M: TIntMatrix; Lb1, Ub1, Lb2, Ub2: integer);
var
  I,J:Integer;
begin
  for I := Lb1 to Ub1 do
    for J := Lb2 to Ub2 do
      M[I,J] := abs(M[I,J]);
end;

procedure VecSqr(V: TVector; Lb, Ub: integer);
var
  I:Integer;
begin
  for I := Lb to Ub do
    V[I] := Sqr(V[I]);
end;

procedure VecSqr(V: TIntVector; Lb, Ub: integer);
var
  I:Integer;
begin
  for I := Lb to Ub do
    V[I] := Sqr(V[I]);
end;

procedure MatSqr(M: TMatrix; Lb1, Ub1, Lb2, Ub2: integer);
var
  I,J:Integer;
begin
  for I := Lb1 to Ub1 do
    for J := Lb2 to Ub2 do
      M[I,J] := Sqr(M[I,J]);
end;

procedure MatSqr(M: TIntMatrix; Lb1, Ub1, Lb2, Ub2: integer);
var
  I,J:Integer;
begin
  for I := Lb1 to Ub1 do
    for J := Lb2 to Ub2 do
      M[I,J] := Sqr(M[I,J]);
end;

procedure VecSqrt(V: TVector; Lb, Ub: integer);
var
  I:Integer;
begin
  for I := Lb to Ub do
    V[I] := Sqrt(V[I]);
end;

procedure MatSqrt(M: TMatrix; Lb1, Ub1, Lb2, Ub2: integer);
var
  I,J:Integer;
begin
  for I := Lb1 to Ub1 do
    for J := Lb2 to Ub2 do
      M[I,J] := Sqrt(M[I,J]);
end;

end.


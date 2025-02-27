unit uMatrix;

{$mode objfpc}{$H+}

interface

uses uTypes, uMinMax, uVecUtils, uErrors;

operator + (const V:array of float; R:Float) Res : TVector;
operator - (const V:array of float; R:Float) Res : TVector;
operator / (const V:array of float; R:Float) Res : TVector;
operator * (const V:array of float; R:Float) Res : TVector;

operator + (const M:TMatrix; R:Float) Res : TMatrix;
operator - (const M:TMatrix; R:Float) Res : TMatrix;
operator / (const M:TMatrix; R:Float) Res : TMatrix;
operator * (const M:TMatrix; R:Float) Res : TMatrix;

operator + (const V1:array of float; const V2:array of float) Res : TVector; // element-wise
operator - (const V1:array of float; const V2:array of float) Res : TVector;

procedure VecFloatAdd  (V:array of float; R:Float; var Ziel : array of float); overload;
procedure VecFloatSubtr(V:array of float; R:Float; var Ziel : array of float); overload;
procedure VecFloatDiv  (V:array of float; R:Float; var Ziel : array of float); overload;
procedure VecFloatMul  (V:array of float; R:Float; var Ziel : array of float); overload;

{These functions use _Ziel_ array if it is not _nil_ by call. Otherwise, new array is allocated.}
function VecFloatAdd(V:TVector; R:Float; Lb, Ub : integer; 
              Ziel : TVector = nil; ResLb : integer = 1): TVector; overload;
function VecFloatSubtr(V:TVector; R:Float; Lb, Ub : integer; 
              Ziel : TVector = nil; ResLb : integer = 1): TVector; overload;
function VecFloatDiv(V:TVector; R:Float; Lb, Ub : integer; 
              Ziel : TVector = nil; ResLb : integer = 1): TVector; overload;
function VecFloatMul(V:TVector; R:Float; Lb, Ub : integer;  
              Ziel : TVector = nil; ResLb : integer = 1): TVector; overload;

//Ub1 is number of rows, Ub2 is number of columns (same as row length)
function MatFloatAdd(M:TMatrix; R:Float; Lb, Ub1, Ub2 : integer; Ziel : TMatrix = nil) : TMatrix;
function MatFloatSubtr(M:TMatrix; R:Float; Lb, Ub1, Ub2 : integer; Ziel : TMatrix = nil) : TMatrix;
function MatFloatDiv(M:TMatrix; R:Float; Lb, Ub1, Ub2 : integer; Ziel : TMatrix = nil) : TMatrix;
function MatFloatMul(M:TMatrix; R:Float; Lb, Ub1, Ub2 : integer; Ziel : TMatrix = nil) : TMatrix;

procedure VecAdd(V1,V2:array of float; var Ziel : array of float);
procedure VecSubtr(V1,V2:array of float; var Ziel : array of float);

{This function multiplies elements of one vector by elements of other.}
procedure VecElemMul(V1,V2:array of float; var Ziel : array of float);
procedure VecDiv(V1,V2:array of float; var Ziel : array of float);

function VecDotProd(V1,V2:TVector; Lb, Ub : integer) : float; overload;
function VecOuterProd(V1, V2:TVector; Lb, Ub1, Ub2 : integer; Ziel : TMatrix = nil):TMatrix; overload;
function VecCrossProd(V1, V2:TVector; Lb: integer; Ziel :TVector = nil):TVector; overload;
function VecEucLength(V:TVector; LB, Ub : integer) : float; overload;
function MatVecMul(M:TMatrix; V:TVector; LB: integer; Ziel: TVector = nil): TVector;

function VecDotProd(const V1,V2:array of float) : float; overload;
function VecOuterProd(const V1, V2:array of float; Ziel : TMatrix = nil):TMatrix; overload;
procedure VecCrossProd(V1, V2:array of float; var Ziel : array of float); overload;
function VecEucLength(const V:array of float) : float; overload;


procedure MatVecMul(M: TMatrix; V: array of float; var Ziel: array of float);

function MatMul(A, B : TMatrix; LB : integer; Ziel : TMatrix = nil) : TMatrix;

function MatTranspose(M:TMatrix; LB: integer; Ziel: TMatrix = nil): TMatrix;

// for quadratic matrix
procedure MatTransposeInPlace(M:TMatrix; Lb, Ub : integer);

implementation
type
  TBigArray = array[0..10000000] of float;
  PBigArray = ^TBigArray;

function VecFloatAdd(V:TVector; R:Float;  Lb, Ub : integer; Ziel : TVector = nil; ResLb : integer = 1): TVector;
var
  I,M,HZ:Integer;
begin
  HZ := ResLb + Ub - Lb;
  if Ziel = nil then
    DimVector(Ziel, HZ);
  M := Lb - ResLb;
  if High(Ziel) < HZ then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  for I := Lb to Ub do
    Ziel[I-M] := V[I]+R;
  Result := Ziel;
end;

function VecFloatSubtr(V:TVector; R:Float;  Lb, Ub : integer; Ziel : TVector = nil; ResLb : integer = 1): TVector;
var
  I,M,HZ:Integer;
begin
  HZ := ResLb + Ub - Lb;
  if Ziel = nil then
    DimVector(Ziel,HZ);
  M := Lb - ResLb;
  if High(Ziel) < HZ then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  for I := Lb to Ub do
    Ziel[I-M] := V[I]-R;
  Result := Ziel;
end;

function VecFloatDiv(V:TVector; R:Float;  Lb, Ub : integer; Ziel : TVector = nil; ResLb : integer = 1): TVector;
var
  I,M,HZ:Integer;
begin
  HZ := ResLb + Ub - Lb;
  if Ziel = nil then
    DimVector(Ziel,HZ);
  M := Lb - ResLb;
  if High(Ziel) < HZ then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  for I := Lb to Ub do
    Ziel[I-M] := V[I]/R;
  Result := Ziel;
end;

function VecFloatMul(V:TVector; R:Float;  Lb, Ub : integer; Ziel : TVector = nil; ResLb : integer = 1): TVector;
var
  I,M,HZ:Integer;
begin
  HZ := ResLb + Ub - Lb;
  if Ziel = nil then
    DimVector(Ziel,HZ);
  M := Lb - ResLb;
  if High(Ziel) < HZ then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  for I := Lb to Ub do
    Ziel[I-M] := V[I]*R;
  Result := Ziel;
end;

procedure VecFloatAdd(V: array of float; R: Float; var Ziel: array of float);
var
  I,H:Integer;
begin
  H := high(V);
  if High(Ziel) < H then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  for I := 0 to H do
    Ziel[I] := V[I]+R;
end;

procedure VecFloatSubtr(V: array of float; R: Float; var Ziel: array of float);
var
  I,H:Integer;
begin
  H := high(V);
  if High(Ziel) < H then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  for I := 0 to H do
    Ziel[I] := V[I]-R;
end;

procedure VecFloatDiv(V: array of float; R: Float; var Ziel: array of float);
var
  I,H:Integer;
begin
  H := high(V);
  if High(Ziel) < H then
    SetErrCode(MatErrDim);
  if IsZero(R) then
    SetErrCode(FDomain,'VecFloatDiv : division by zero');
  if not (MathErr = matOK) then
    Exit;
  for I := 0 to H do
    Ziel[I] := V[I]/R;
end;

procedure VecFloatMul(V: array of float; R: Float; var Ziel: array of float);
var
  I,H:Integer;
begin
  H := high(V);
  if High(Ziel) < H then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  for I := 0 to H do
    Ziel[I] := V[I]*R;
end;

function MatFloatAdd(M:TMatrix; R:Float; Lb, Ub1, Ub2 : integer; Ziel : TMatrix = nil) : TMatrix;
var
  I, J:Integer;
begin
  if Ziel = nil then
    DimMatrix(Ziel, Ub1, Ub2);
  for I := Lb to Ub1 do
    for J := Lb to Ub2 do
      Ziel[I,J] := M[I,J]+R;
  Result := Ziel;
end;

function MatFloatSubtr(M:TMatrix; R:Float; Lb, Ub1, Ub2 : integer; Ziel : TMatrix = nil) : TMatrix;
var
  I, J:Integer;
begin
  if Ziel = nil then
    DimMatrix(Ziel, Ub1, Ub2);
  for I := Lb to Ub1 do
    for J := Lb to Ub2 do
      Ziel[I,J] := M[I,J]-R;
  Result := Ziel;
end;

function MatFloatDiv(M:TMatrix; R:Float; Lb, Ub1, Ub2 : integer; Ziel : TMatrix = nil) : TMatrix;
var
  I, J:Integer;
begin
  if Ziel = nil then
    DimMatrix(Ziel, Ub1, Ub2);
  for I := Lb to Ub1 do
    for J := Lb to Ub2 do
      Ziel[I,J] := M[I,J]/R;
  Result := Ziel;
end;

function MatFloatMul(M:TMatrix; R:Float; Lb, Ub1, Ub2 : integer; Ziel : TMatrix = nil) : TMatrix;
var
  I, J:Integer;
begin
  if Ziel = nil then
    DimMatrix(Ziel, Ub1, Ub2);
  for I := Lb to Ub1 do
    for J := Lb to Ub2 do
      Ziel[I,J] := M[I,J]*R;
  Result := Ziel;
end;

procedure VecAdd(V1, V2: array of float; var Ziel: array of float);
var
  I,H:Integer;
begin
  H := High(V1);
  if (High(V2) = H) and (High(Ziel) = H) then
    for I := 0 to H do
      Ziel[I] := V1[I] + V2[I]
  else
    SetErrCode(MatErrDim);
  end;

procedure VecSubtr(V1, V2: array of float; var Ziel: array of float);
var
  I,H:Integer;
begin
  H := High(V1);
  if (High(V2) = H) and (High(Ziel) = H) then
    for I := 0 to H do
      Ziel[I] := V1[I] - V2[I]
  else
    SetErrCode(MatErrDim);
end;

procedure VecElemMul(V1, V2: array of float; var Ziel: array of float);
var
  I,H:Integer;
begin
   H := High(V1);
  if (High(V2) = H) and (High(Ziel) = H) then
    for I := 0 to H do
      Ziel[I] := V1[I] * V2[I]
  else
    SetErrCode(MatErrDim);
end;

procedure VecDiv(V1, V2: array of float; var Ziel: array of float);
var
  I,L,H:Integer;
begin
  H := High(V1);
  if (High(V2) <> H) or (High(Ziel) <> H) then
    SetErrCode(MatErrDim);
  if Any(V2, @IsZero) then
    SetErrCode(FDomain, 'VecDiv: division by zero');
  if not MathErr = matOK then
    Exit;
  for I := 0 to H do
    Ziel[I] := V1[I] / V2[I]
end;

function VecDotProd(V1, V2: TVector; Lb, Ub : integer): float;
var
  I:Integer;
begin
  Result := 0;
  if (High(V1) >= Ub) and (High(V2) >= Ub) then
    for I := Lb to Ub do
      Result := Result + V1[I] * V2[I]
  else
      SetErrCode(MatErrDim);
end;

function VecOuterProd(V1, V2: TVector; Lb, Ub1, Ub2: integer; Ziel: TMatrix = nil): TMatrix;
var
  I:integer;
begin
  if Ziel = nil then
    DimMatrix(Ziel,Ub1,Ub2)
  else begin
    if (High(Ziel) < Ub1) or (High(Ziel[0]) < Ub2) then
    begin
      SetErrCode(MatErrDim);
      Result := nil;
    end;
  end;
  for I := Lb to Ub1 do
    Ziel[I] := VecFloatMul(V2,V1[I],Lb,Ub2,Ziel[I],Lb);
  Result := Ziel;
end;

function VecCrossProd(V1, V2: TVector; Lb: integer; Ziel: TVector): TVector;
var
  Y,Z:integer;
begin
  Y:= Lb+1;
  Z := Lb+2;
  if Ziel = nil then
    DimVector(Ziel,Z);
  Ziel[Lb] := V1[Y]*V2[Z]  - V1[Z]*V2[Y];
  Ziel[Y]  := V1[Z]*V2[Lb] - V1[Lb]*V2[Z];
  Ziel[Z]  := V1[Lb]*V2[Y] - V1[Y]*V2[Lb];
  Result := Ziel;
end;

function VecEucLength(V:TVector; LB, Ub : integer): float;
var
  I:integer;
begin
  Result := 0;
  for I := LB to Ub do
    Result := Result + Sqr(V[I]);
  Result := Sqrt(Result);
end;

function VecDotProd(const V1, V2: array of float): float;
var
  I:Integer;
begin
  Result := 0;
  if High(V1) = High(V2) then
  for I := 0 to High(V1) do
      Result := Result + V1[I] * V2[I]
  else
      SetErrCode(MatErrDim);
end;

function VecOuterProd(const V1, V2: array of float; Ziel: TMatrix): TMatrix;
var
  I:integer;
  Ub1, Ub2:integer;
begin
  Ub1 := high(V1);
  Ub2 := high(V2);
  if Ziel = nil then
    DimMatrix(Ziel,Ub1,Ub2)
  else begin
    if (High(Ziel) < Ub1) or (High(Ziel[0]) < Ub2) then
    begin
      SetErrCode(MatErrDim);
      Result := nil;
    end;
  end;
  for I := 0 to Ub1 do
    VecFloatMul(V2,V1[I],Ziel[I]);
  Result := Ziel;
end;

procedure VecCrossProd(V1, V2: array of float; var Ziel: array of float);
begin
  if (High(V1) <> 2) or (High(V2) <> 2) or (High(Ziel) <> 2) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  Ziel[0] := V1[1]*V2[2]  - V1[2]*V2[1];
  Ziel[1]  := V1[2]*V2[0] - V1[0]*V2[2];
  Ziel[2]  := V1[0]*V2[1] - V1[1]*V2[0];
end;

function VecEucLength(const V: array of float): float;
var
  I:integer;
begin
  Result := 0;
  for I := 0 to high(V) do
    Result := Result + Sqr(V[I]);
  Result := Sqrt(Result);
end;

procedure MatVecMul(M: TMatrix; V: array of float; var Ziel: array of float);
var
  HighRow, HighCol : integer;
  I,J:integer;
  R:float;
begin
  HighRow := High(M);
  HighCol := High(M[0]);
  if (HighCol = High(V)) and (HighRow = High(Ziel)) then
    for I := 0 to HighRow do
    begin
      R := 0.0;
      for J := 0 to HighCol do
        R := R + V[J]*M[I,J];
      Ziel[I] := R;
    end
  else
    SetErrCode(MatErrDim);
end;

function MatVecMul(M:TMatrix; V:TVector; LB: integer; Ziel: TVector = nil): TVector;
var
  HighRow, HighCol : integer;
  I,J:integer;
  R:float;
begin
  HighRow := High(M);
  HighCol := High(M[0]);
  if Ziel = nil then
    DimVector(Ziel,HighRow);
  if (HighCol = High(V)) and (HighRow = High(Ziel)) then
  begin
    for I := LB to HighRow do
    begin
      R := 0.0;
      for J := LB to HighCol do
        R := R + V[J]*M[I,J];
      Ziel[I] := R;
    end;
    Result := Ziel;
  end
  else begin
    SetErrCode(MatErrDim);
    Result := nil;
  end;
end;

function MatMul(A, B : TMatrix; LB : integer; Ziel : TMatrix = nil) : TMatrix;
var
  I,J,L : integer;
  BufB, BufC : PBigArray;
  Af: Float;
  HiRow1, HiCol1, HiRow2, HiCol2 : integer;
begin
  HiRow1 := High(A);
  HiCol1 := High(A[1]);
  HiRow2 := High(B);
  HiCol2 := High(B[1]);
  if Ziel = nil then
    DimMatrix(Ziel, HiRow1, HiCol2);
  if not ((HiRow2 = HiCol1) and
    (High(Ziel) = HiRow1) and (High(Ziel[1]) = HiCol2)) then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  for I := LB to HiRow1 do
  begin
    BufC := @(Ziel[I,LB]);  // moved to next row in C
    for j := 0 to HiCol2-LB do
        BufC^[j] := 0;      // nulled it
    for L := 0 to HiCol1 - LB do
    begin
      BufB := @(B[L,LB]);   // moved to next line in B
      Af := A[I,L];
      for j := 0 to HiCol2-LB do
        BufC^[J] := BufC^[J] + Af * BufB^[j];
    end;
  end;
  Result := Ziel;
end;

function MatTranspose(M:TMatrix; LB: integer; Ziel: TMatrix = nil): TMatrix;
var
 I,J,H,W:integer;
begin
  H := High(M);
  W := High(M[0]);
  if Ziel = nil then
    DimMatrix(Ziel, W, H);
  if (High(Ziel) = W) and (High(Ziel[0]) = H) then
  begin
    for I := LB to H do
      for J := LB to W do
        Ziel[J,I] := M[I,J];
    Result := Ziel;
  end else
  begin
    Result := nil;
    SetErrCode(MatErrDim);
  end;
end;

procedure MatTransposeInPlace(M:TMatrix; Lb, Ub : integer);
var
 I,J:integer;
begin
  if (High(M) < Ub) or (High(M[Lb]) < Ub) then
    SetErrCode(MatErrDim)
  else
    for I := Lb to Ub do
      for J := I + 1 to Ub do
        Swap(M[I,J],M[J,I]);
end;

operator+(const V: array of float; R: Float)Res: TVector;
var
  I,L:integer;
  Ziel: TVector;
begin
  L := high(V);
  DimVector(Ziel, L);
  for I := 0 to L do
    Ziel[I] := V[I]+R;
  Result := Ziel;
end;

operator-(const V: array of float; R: Float)Res: TVector;
var
  I,L:integer;
  Ziel: TVector;
begin
  L := high(V);
  DimVector(Ziel, L);
  for I := 0 to L do
    Ziel[I] := V[I]-R;
  Result := Ziel;
end;

operator/(const V: array of float; R: Float)Res: TVector;
var
  I,L:integer;
  Ziel: TVector;
begin
  L := high(V);
  DimVector(Ziel, L);
  for I := 0 to L do
    Ziel[I] := V[I]/R;
  Result := Ziel;
end;

operator*(const V: array of float; R: Float)Res: TVector;
var
  I,L:integer;
  Ziel: TVector;
begin
  L := high(V);
  DimVector(Ziel, L);
  for I := 0 to L do
    Ziel[I] := V[I]*R;
  Result := Ziel;
end;

operator+(const M: TMatrix; R: Float)Res: TMatrix;
var
  I,J,Ub1,Ub2:integer;
  Ziel: TMatrix;
begin
  Ub1 := high(M);
  Ub2 := high(M[0]);
  DimMatrix(Ziel, Ub1, Ub2);
  for I := 0 to Ub1 do
    for J := 0 to Ub2 do
      Ziel[I,J] := M[I,J]+R;
  Result := Ziel;
end;

operator-(const M: TMatrix; R: Float)Res: TMatrix;
var
  I,J,Ub1,Ub2:integer;
  Ziel: TMatrix;
begin
  Ub1 := high(M);
  Ub2 := high(M[0]);
  DimMatrix(Ziel, Ub1, Ub2);
  for I := 0 to Ub1 do
    for J := 0 to Ub2 do
      Ziel[I,J] := M[I,J] - R;
  Result := Ziel;
end;

operator/(const M: TMatrix; R: Float)Res: TMatrix;
var
  I,J,Ub1,Ub2:integer;
  Ziel: TMatrix;
begin
  Ub1 := high(M);
  Ub2 := high(M[0]);
  DimMatrix(Ziel, Ub1, Ub2);
  for I := 0 to Ub1 do
    for J := 0 to Ub2 do
      Ziel[I,J] := M[I,J] / R;
  Result := Ziel;
end;

operator*(const M: TMatrix; R: Float)Res: TMatrix;
var
  I,J,Ub1,Ub2:integer;
  Ziel: TMatrix;
begin
  Ub1 := high(M);
  Ub2 := high(M[0]);
  DimMatrix(Ziel, Ub1, Ub2);
  for I := 0 to Ub1 do
    for J := 0 to Ub2 do
      Ziel[I,J] := M[I,J] * R;
  Result := Ziel;
end;

operator+(const V1: array of float; const V2: array of float)Res: TVector;
var
  I,L:integer;
  Ziel:TVector;
begin
  L := high(V1);
  if L <> High(V2) then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  DimVector(Ziel, L);
  for I := 0 to L do
    Ziel[I] := V1[I] + V2[I];
  Result := Ziel;
end;

  operator-(const V1: array of float; const V2: array of float)Res: TVector;
  var
    I,L:integer;
    Ziel:TVector;
  begin
    L := high(V1);
    if L <> High(V2) then
    begin
      SetErrCode(MatErrDim);
      Result := nil;
      Exit;
    end;
    DimVector(Ziel, L);
    for I := 0 to L do
      Ziel[I] := V1[I] - V2[I];
    Result := Ziel;
end;


end.


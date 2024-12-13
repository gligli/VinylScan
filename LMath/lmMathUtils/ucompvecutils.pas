unit uCompVecUtils;

{$mode objfpc}{$H+}

interface

uses
  uTypes, uMinMax, uErrors, uComplex;

type
  // general function for testing complex value for a condition
  // function(Val:complex):boolean
  TComplexTestFunc = function(Val:complex):boolean;

  // general complex function of complex argument
  // function(Arg:complex):Complex
  TComplexFunc = function(Arg:Complex):complex;

  //general complex function of real argument
  //function(Arg:real):Complex
  TIntComplexFunc = function(Arg:Integer):complex;

  // general function for comparison of complex
  // function(Val, Ref:complex):boolean
  // FirstElement and SelElements pass array elements to Val and
  // user-supplied Ref value to Ref
  TComplexComparator = function(Val, Ref:complex):boolean;

function ExtractReal(const CVec:array of complex):TVector;
function ExtractImaginary(const CVec:array of complex):TVector;
function CombineCompVec(const VecRe, VecIm:array of float):TCompVector;

function CMakePolar(const V:array of complex):TCompVector;
function CMakeRectangular(const V:array of complex):TCompVector;

function MaxReLoc(CVec: TCompVector; Lb, Ub: integer):integer;
function MaxImLoc(CVec: TCompVector; Lb, Ub: integer):integer;
function MinReLoc(CVec: TCompVector; Lb, Ub: integer):integer;
function MinImLoc(CVec: TCompVector; Lb, Ub: integer):integer;

procedure Apply(var V:array of Complex; Func:TComplexFunc); overload;
procedure Apply(var V:array of Complex; Func:TIntComplexFunc); overload;
procedure Apply(V:TCompVector; Mask:TIntVector; MaskLb:integer; Func:TComplexFunc); overload;

// returns True if both vectors have same length and all elements are equal to the MachEp accuracy
function CompareCompVec(const X, Xref : array of Complex; Tol : Float) : Boolean; overload;
function Any(const Vector:array of Complex; Test:TComplexTestFunc):boolean; overload;
function FirstElement(Vector: TCompVector; Lb, Ub: integer; Ref: complex; Comparator: TComplexComparator): integer; overload;
function ComplexSeq(Lb, Ub : integer; firstRe, firstIm, incrementRe, incrementIm:Float; Vector:TCompVector = nil):TCompVector;
function SelElements(Vector:TCompVector; Lb, Ub, ResLb : integer;
         Ref:complex; Comparator:TComplexComparator):TIntVector; overload;
function ExtractElements(Vector:TCompVector; Mask:TIntVector; MaskLb:integer):TCompVector; overload;

implementation

function ExtractReal(const CVec: array of complex): TVector;
var
  I:integer;
begin
  DimVector(Result, High(CVec));
  for I := 0 to High(CVec) do
    Result[I] := CVec[I].X;
end;

function ExtractImaginary(const CVec: array of complex): TVector;
var
  I:integer;
begin
  DimVector(Result, High(CVec));
  for I := 0 to High(CVec) do
    Result[I] := CVec[I].Y;
end;

function CombineCompVec(const VecRe, VecIm:array of float): TCompVector;
var
  I:integer;
  Ub: integer;
begin
  Ub := High(VecRe);
  if Ub <> High(VecIm) then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  DimVector(Result, Ub);
  for I := 0 to Ub do
  begin
    Result[I].X := VecRe[I];
    Result[I].Y := VecIm[I];
  end;
end;

function MaxReLoc(CVec: TCompVector; Lb, Ub: integer): integer;
var
  I:integer;
  MaxVal:float;
begin
  Result := -1;
  Ub := min(Ub,High(CVec));
  if Lb > Ub then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  MaxVal := CVec[Lb].X;
  Result := Lb;
  for I := Lb+1 to Ub do
    if CVec[I].X > MaxVal then
    begin
      MaxVal := CVec[I].X;
      Result := I;
    end;
end;

function MaxImLoc(CVec: TCompVector; Lb, Ub: integer): integer;
var
  I:integer;
  MaxVal:float;
begin
  Result := -1;
  Ub := min(Ub,High(CVec));
  if Lb > Ub then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  MaxVal := CVec[Lb].Y;
  Result := Lb;
  for I := Lb+1 to Ub do
    if CVec[I].X > MaxVal then
    begin
      MaxVal := CVec[I].Y;
      Result := I;
    end;
end;

function MinReLoc(CVec: TCompVector; Lb, Ub: integer): integer;
var
  I:integer;
  MinVal:float;
begin
  Result := -1;
  Ub := min(Ub,High(CVec));
  if Lb > Ub then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  MinVal := CVec[Lb].X;
  Result := Lb;
  for I := Lb+1 to Ub do
    if CVec[I].X < MinVal then
    begin
      MinVal := CVec[I].Y;
      Result := I;
    end;
end;

function MinImLoc(CVec: TCompVector; Lb, Ub: integer): integer;
var
  I:integer;
  MinVal:float;
begin
  Result := -1;
  Ub := min(Ub,High(CVec));
  if Lb > Ub then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  Result := Lb;
  MinVal := CVec[Lb].Y;
  for I := Lb+1 to Ub do
    if CVec[I].Y < MinVal then
    begin
      MinVal := CVec[I].Y;
      Result := I;
    end;
end;

procedure Apply(var V:array of Complex; Func:TComplexFunc);
var
  I:integer;
begin
  for I := 0 to High(V) do
    V[I] := Func(V[I]);
end;

procedure Apply(var V:array of Complex; Func:TIntComplexFunc);
var
  I:integer;
begin
  for I := 0 to High(V) do
    V[I] := Func(I);
end;

procedure Apply(V: TCompVector; Mask: TIntVector; MaskLb: integer; Func: TComplexFunc);
var
  I,J:integer;
begin
  for I := MaskLb to High(Mask) do
  begin
    J := Mask[I];
    V[J] := Func(V[J]);
  end;
end;

function CompareCompVec(const X, Xref: array of complex; Tol: Float): Boolean;
var
  I,H    : Integer;
  Ok   : Boolean;
  ReTol, ImTol : Float;
begin
  Ok := True;
  H := High(XRef);
  if High(X) <> H then
  begin
    Result := false;
    Exit;
  end;
  I := 0;
  repeat
    ReTol := Tol * Abs(XRef[I].X);
    ImTol := Tol * Abs(Xref[I].Y);
    if ReTol < MachEp then ReTol := MachEp;
    if ImTol < MachEp then ImTol := MachEp;
    Ok := Ok and (Abs(X[I].Y - Xref[I].Y) < ImTol) and (Abs(X[I].X - Xref[I].X) < ReTol);
    Inc(I);
  until (not Ok) or (I > H);
  Result := Ok;
end;

function Any(const Vector: array of Complex; Test: TComplexTestFunc): boolean;
var
  I:Integer;
begin
  for I := 0 to High(Vector) do
    if Test(Vector[I]) then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

function FirstElement(Vector: TCompVector; Lb, Ub: integer; Ref: complex; Comparator: TComplexComparator): integer;
var
  I:Integer;
begin
  Ub := min(Ub,High(Vector));
  if Lb > Ub then
  begin
    SetErrCode(MatErrDim);
    Result := -1;
    Exit;
  end;
  for I := Lb to Ub do
    if Comparator(Vector[I],Ref) then
    begin
      Result := I;
      Exit;
    end;
  Result := Ub + 1;
end;

function ComplexSeq(Lb, Ub: integer; firstRe, firstIm, incrementRe, incrementIm: Float; Vector: TCompVector = nil): TCompVector;
var
  I:integer;
begin
  if Lb > Ub then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  if Vector = nil then
    DimVector(Vector, Ub)
  else
    Ub := min(Ub,High(Vector));
  Vector[Lb].X := 0;
  Vector[Lb].Y := 0;
  for I := Lb+1 to Ub do
  begin                                          // 2 cycles to avoid rounding error if
    Vector[I].X := Vector[I-1].X + incrementRe;  //First is very large and increment small
    Vector[I].Y := Vector[I-1].Y + incrementIm;
  end;
  for I := Lb to Ub do
  begin
    Vector[I].X := Vector[I].X+firstRe;
    Vector[I].Y := Vector[I].Y+firstIm;
  end;
  Result := Vector;
end;

function SelElements(Vector: TCompVector; Lb, Ub, ResLb: integer; Ref: complex; Comparator: TComplexComparator
  ): TIntVector;
var
  I,N:integer;
begin
  Ub := min(high(Vector),Ub);
  if Lb > Ub then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  DimVector(Result,Ub-Lb+ResLb);
  N := ResLb-1;
  for I := Lb to Ub do
    if Comparator(Vector[I],Ref) then
    begin
      inc(N);
      Result[N] := I;
    end;
  if N >= ResLb then
    SetLength(Result,N+1)
  else
    SetLength(Result,0);
end;

function ExtractElements(Vector: TCompVector; Mask: TIntVector; MaskLb: integer): TCompVector;
var
  I:integer;
begin
  DimVector(Result,High(Mask));
  for I := MaskLb to High(Mask) do
    Result[I] := Vector[Mask[I]];
end;

function CMakePolar(const V:array of complex):TCompVector;
var
  I:integer;
begin
  DimVector(Result,High(V));
  for I := 0 to High(V) do
    Result[I] := CToPolar(V[I]);
end;

function CMakeRectangular(const V:array of Complex):TCompVector;
var
  I:integer;
begin
  DimVector(Result,High(V));
  for I := 0 to High(V) do
    Result[I] := CToRect(V[I]);
end;

end.


unit uVecUtils;

interface
uses uTypes, uErrors, uMinMax;

type
  TIntArrayFunc   = function(X:array of integer):float;
  TFloatArrayFunc = function(X:array of float):float;
  TIntArrayIntFunc   = function(X:array of integer):integer;

  TMatCoords = record
    Row, Col :integer;
  end;
  
function tmCoords(ARow,ACol:integer):TMatCoords;

// this group of Apply functions passes existing value of an array element to a function and assigns
// back the returned value
procedure Apply(var V:array of Float; Func:TFunc); overload;
procedure Apply(var V:array of Integer; Func:TIntFunc); overload;

procedure Apply(V:TVector; Lb, Ub: integer; Func:TFunc); overload;
procedure Apply(M:TMatrix; LRow, URow, LCol, UCol: integer; Func:TFunc); overload;
procedure Apply(V:TIntVector; Lb, Ub: integer; Func:TIntFunc); overload;
procedure Apply(M:TIntMatrix; LRow, URow, LCol, UCol: integer; Func:TIntFunc); overload;

procedure Apply(V:TVector; Mask:TIntVector; MaskLb:integer; Func:TFunc); overload;
procedure Apply(V:TIntVector; Mask:TIntVector; MaskLb:integer; Func:TIntFunc); overload;

//InitWithFunc function passes to the function index of an array element and assigns returned value to it
function InitWithFunc(Lb, Ub: integer; Func:TIntFloatFunc; Ziel:TVector = nil):TVector; overload;
function InitWithFunc(Lb, Ub: integer; Func:TIntFunc; Ziel:TIntVector = nil):TIntVector; overload;

//ApplyRecursive is used to initialize an array with a recursive sequence like Fibonacci numbers
function ApplyRecursive(InitValues:array of Float;
         Lb, Ub : integer; Func:TFloatArrayFunc; Ziel:TVector = nil):TVector; overload;
function ApplyRecursive(InitValues:array of Integer;
         Lb, Ub : integer; Func:TIntArrayIntFunc; Ziel:TIntVector = nil):TIntVector; overload;

{ Checks if each component of vector X is within a fraction Tol of
the corresponding component of the reference vector Xref. In this
case, the function returns True, otherwise it returns False}
function CompVec(X, Xref : TVector; Lb, Ub  : Integer; Tol : Float) : Boolean; overload;
function CompVec(constref X, Xref : array of float; Tol : Float) : Boolean; overload;

// applies Test function to every enement in [Lb..Ub] and returns true
// if for any of them Test returns true
function Any(constref Vector:array of Float; Test:TTestFunc):boolean; overload;
function Any(constref Vector:array of integer; Test:TIntTestFunc):boolean; overload;
function Any(Vector:TVector; Lb, Ub : integer; Test:TTestFunc):boolean; overload; deprecated 'Use version with open array instead.';
function Any(M:TMatrix; LRow, URow, LCol, UCol : integer; Test:TTestFunc):boolean; overload;
function Any(Vector:TIntVector; Lb, Ub : integer; Test:TIntTestFunc):boolean; overload; deprecated 'Use version with open array instead.';
function Any(M:TIntMatrix; LRow, URow, LCol, UCol : integer; Test:TIntTestFunc):boolean; overload;


//Finds a first element satisfying the condition. If nothing is found, returns -1.
function FirstElement(Vector:TVector; Lb, Ub : integer; Ref:float; Comparator:TComparator):integer; overload;
function FirstElement(M:TMatrix; LRow, URow, LCol, UCol : integer; Ref:float; Comparator:TComparator):TMatCoords; overload;

function FirstElement(Vector:TVector; Lb, Ub : integer; Test:TTestFunc):integer; overload;
function FirstElement(M:TMatrix; LRow, URow, LCol, UCol : integer; Test:TTestFunc):TMatCoords; overload;

function FirstElement(Vector:TIntVector; Lb, Ub : integer; Ref:integer; Comparator:TIntComparator):integer; overload;
function FirstElement(M:TIntMatrix; LRow, URow, LCol, UCol : integer; Ref:integer; Comparator:TIntComparator):TMatCoords; overload;

function FirstElement(Vector:TVector; Lb, Ub : integer; Ref:float; CompType:TCompOperator):integer; overload;
function FirstElement(M:TMatrix; LRow, URow, LCol, UCol : integer; Ref:float; CompType:TCompOperator):TMatCoords; overload;

function FirstElement(Vector:TIntVector; Lb, Ub : integer; Ref:integer; CompType:TCompOperator):integer; overload;
function FirstElement(M:TIntMatrix; LRow, URow, LCol, UCol : integer; Ref:integer; CompType:TCompOperator):TMatCoords; overload;

function MaxLoc(Vector:TVector; Lb, Ub:integer):integer; overload;
function MaxLoc(M:TMatrix; LRow,URow,LCol,UCol:integer):TMatCoords;overload;

function MaxLoc(Vector:TIntVector; Lb, Ub:integer):integer; overload;
function MaxLoc(M:TIntMatrix; LRow,URow,LCol,UCol:integer):TMatCoords;overload;

function MinLoc(Vector:TVector; Lb, Ub:integer):integer; overload;
function MinLoc(M:TMatrix; LRow,URow,LCol,UCol:integer):TMatCoords;overload;

function MinLoc(Vector:TIntVector; Lb, Ub:integer):integer; overload;
function MinLoc(M:TIntMatrix; LRow,URow,LCol,UCol:integer):TMatCoords;overload;

//generates arithmetic progression
function Seq(Lb, Ub : integer; first, increment:Float; Vector:TVector = nil):TVector;
function ISeq(Lb, Ub : integer; first, increment:integer; Vector:TIntVector = nil):TIntVector;

// selects elements from array which compare to Ref value as CompType prescribes.
// CompType can be LT, LE, EQ, GT, GE, NE
// Other form is Ref and Comparator where comparator is function(Val,Ref:float):boolean
// elements for which comparator returns true are selected. Element of array is V1, Ref is V2
// Indicis of selected elements are copied to Result array beginning from ResLb
function SelElements(Vector:TVector; Lb, Ub, ResLb : integer; Ref: float;
         CompType:TCompOperator):TIntVector; overload;
function SelElements(Vector:TVector; Lb, Ub, ResLb : integer;
         Ref:float; Comparator:TComparator):TIntVector; overload;
function SelElements(Vector:TVector; Lb, Ub, ResLb : integer; Test:TTestFunc):TIntVector; overload;

function SelElements(Vector:TIntVector; Lb, Ub, ResLb : integer; Ref: Integer;
         CompType:TCompOperator):TIntVector; overload;
function SelElements(Vector:TIntVector; Lb, Ub, ResLb : integer; Ref:Integer;
         Comparator:TIntComparator):TIntVector; overload;
function SelElements(Vector:TIntVector; Lb, Ub, ResLb : integer;
         Test:TIntTestFunc):TIntVector; overload;

//extracts from Vector elements whose indices are listed in Mask.
function ExtractElements(Vector:TVector; Mask:TIntVector):TVector;

implementation

function tmCoords(ARow,ACol:integer):TMatCoords;
begin
  Result.Row := ARow;
  Result.Col := ACol;
end;

procedure Apply(V: TVector; Lb, Ub: integer; Func: TFunc);
var
  I:integer;
begin
  Ub := min(High(V),Ub);
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  for I := Lb to Ub do
    V[I] := Func(V[I]);
end;

procedure Apply(var V:array of Float; Func:TFunc); overload;
var
  I:integer;
begin
  for I := 0 to high(V) do
    V[I] := Func(V[I]);
end;

procedure Apply(M: TMatrix; LRow, URow, LCol, UCol: integer; Func: TFunc);
var
  I,J:integer;
begin
  URow := min(High(M),URow);
  if (LRow < 0) or (UCol < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  UCol := min(High(M[LRow]),UCol);
  if (LRow > URow) or (LCol > UCol) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  for I := LRow to URow do
    for J := LCol to UCol do
      M[I,J] := Func(M[I,J]);
end;

procedure Apply(V: TIntVector; Lb, Ub: integer; Func: TIntFunc);
var
  I:integer;
begin
  Ub := min(High(V),Ub);
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  for I := Lb to Ub do
    V[I] := Func(V[I]);
end;

procedure Apply(var V:array of Integer; Func:TIntFunc); overload;
var
  I:integer;
begin
  for I := 0 to High(V) do
    V[I] := Func(V[I]);
end;

procedure Apply(M: TIntMatrix; LRow, URow, LCol, UCol: integer;
  Func: TIntFunc);
var
  I,J:integer;
begin
  URow := min(High(M),URow);
  if (LRow < 0) or (UCol < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  UCol := min(High(M[LRow]),UCol);
  if (LRow > URow) or (LCol > UCol) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  for I := LRow to URow do
    for J := LCol to UCol do
      M[I,J] := Func(M[I,J]);
end;

procedure Apply(V: TVector; Mask: TIntVector; MaskLb: integer;
  Func: TFunc);
var
  I,J:integer;
begin
  for I := MaskLb to High(Mask) do
  begin
    J := Mask[I];
    V[J] := Func(V[J]);
  end;
end;

procedure Apply(V: TIntVector; Mask: TIntVector;
  MaskLb: integer; Func: TIntFunc);
var
  I,J:integer;
begin
  for I := MaskLb to High(Mask) do
  begin
    J := Mask[I];
    V[J] := Func(V[J]);
  end;
end;

function ApplyRecursive(InitValues: array of Float; Lb, Ub: integer; Func: TFloatArrayFunc; Ziel: TVector): TVector;
var
  I,L:integer;
begin
  if Ziel <> nil then
    Ub := min(High(Ziel),Ub);
  if (Lb > Ub) or (High(InitValues) > (Ub-Lb)) or (High(initValues) = -1) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  if Ziel = nil then
     DimVector(Ziel,Ub);

  L := High(InitValues); // L is length of "seed" sequence
  for I := 0 to L do
    Ziel[Lb+I] := InitValues[I]; // initial part of resulting array filled with InitValues ("seed" of recursive sequence)
  for I := Lb+L+1 to High(Ziel) do    // now for each consequitive element we call function with L preceding elements
    Ziel[I] := Func(Ziel[I-L-1..I-1]);
  Result := Ziel;
end;

function ApplyRecursive(InitValues: array of Integer; Lb, Ub: integer; Func: TIntArrayIntFunc; Ziel: TIntVector): TIntVector;
var
  I,L:integer;
begin
  if Ziel <> nil then
    Ub := min(High(Ziel),Ub);
  if (Lb > Ub) or (High(InitValues) > (Ub-Lb)) or (High(initValues) = -1) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  if Ziel = nil then
    DimVector(Ziel,Ub);

  L := High(InitValues);
  for I := 0 to L do
    Ziel[Lb+I] := InitValues[I];
  for I := Lb+L+1 to High(Ziel) do
    Ziel[I] := Func(Ziel[I-L-1..I-1]);
  Result := Ziel;
end;

function InitWithFunc(Lb, Ub: integer; Func: TIntFloatFunc; Ziel:TVector):TVector;
var
  I:integer;
begin
  if Ziel <> nil then
  Ub := min(High(Ziel),Ub);
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  if Ziel = nil then
    DimVector(Ziel,Ub);
  for I := Lb to Ub do
    Ziel[I] := Func(I);
  Result := Ziel;
end;

function InitWithFunc(Lb, Ub: integer; Func: TIntFunc; Ziel:TIntVector):TIntVector;
var
  I:integer;
begin
  if Ziel <> nil then
  Ub := min(High(Ziel),Ub);
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  if Ziel = nil then
    DimVector(Ziel,Ub);
  for I := Lb to Ub do
    Ziel[I] := Func(I);
  Result := Ziel;
end;

function CompVec(X, Xref : TVector; Lb, Ub  : Integer; Tol : Float) : Boolean;
var
  I    : Integer;
  Ok   : Boolean;
  ITol : Float;
begin
  I := Lb;
  Ub := min(Ub,High(X));
  if High(XRef) <> Ub then
  begin
    Result := false;
    Exit;
  end;
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := false;
    Exit;
  end;
  Ok := True;
  repeat
    ITol := Tol * Abs(Xref[I]);
    if ITol < MachEp then ITol := MachEp;
    Ok := Ok and (Abs(X[I] - Xref[I]) < ITol);
    I := I + 1;
  until (not Ok) or (I > Ub);
  CompVec := Ok;
end;

function CompVec(constref X, Xref : array of float; Tol : Float) : Boolean; overload;
var
  I    : Integer;
  Ok   : Boolean;
  ITol : Float;
  Ub   : integer;
begin
  I := 0;
  Ub := High(X);
  if High(XRef) <> Ub then
  begin
    Result := false;
    Exit;
  end;
  Ok := True;
  repeat
    ITol := Tol * Abs(Xref[I]);
    if ITol < MachEp then ITol := MachEp;
    Ok := Ok and (Abs(X[I] - Xref[I]) < ITol);
    I := I + 1;
  until (not Ok) or (I > Ub);
  Result := Ok;
end;


// applies Test function to any enement in [Lb..Ub] and returns true
// if for any of them Test returns true
function Any(Vector:TVector; Lb, Ub : integer; Test:TTestFunc):boolean;
var
  I:Integer;
begin
  Ub := min(Ub,High(Vector));
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := false;
    Exit;
  end;
  for I := Lb to Ub do
    if Test(Vector[I]) then
    begin
      Result := true;
      Exit;
    end; 
  Result := false;
end;

function Any(constref Vector:array of Float; Test:TTestFunc):boolean;
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

function Any(M:TMatrix; LRow, URow, LCol, UCol : integer; Test:TTestFunc):boolean;
var
  I,J:integer;
begin
  URow := min(URow,High(M));
  if (LRow < 0) or (UCol < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  UCol := min(UCol,High(M[LRow]));
  if (LRow > URow) or (LCol > UCol) then
  begin
    SetErrCode(MatErrDim);
    Result := false;
    Exit;
  end;
 for I := LRow to URow do
    for J := LCol to UCol do
      if Test(M[I,J]) then
      begin
        Result := true;
        Exit;
      end; 
  Result := false;
end;

function Any(Vector:TIntVector; Lb, Ub : integer; Test:TIntTestFunc):boolean;
var
  I:Integer;
begin
  Ub := min(Ub,High(Vector));
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := false;
    Exit;
  end;
  for I := Lb to Ub do
    if Test(Vector[I]) then
    begin
      Result := true;
      Exit;
    end; 
  Result := false;
end;

function Any(constref Vector:array of integer; Test:TIntTestFunc):boolean;
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

function Any(M:TIntMatrix; LRow, URow, LCol, UCol : integer; Test:TIntTestFunc):boolean;
var
  I,J:integer;
begin
  URow := min(URow,High(M));
  if (LRow < 0) or (UCol < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  UCol := min(UCol,High(M[LRow]));
  if (LRow > URow) or (LCol > UCol) then
  begin
    SetErrCode(MatErrDim);
    Result := false;
    Exit;
  end;
  for I := LRow to URow do
    for J := LCol to UCol do
      if Test(M[I,J]) then
      begin
        Result := true;
        Exit;
      end; 
  Result := false;
end;

function FirstElement(Vector:TVector; Lb, Ub : integer; Test:TTestFunc):integer; overload;
var
  I:Integer;
begin
  Ub := min(Ub,High(Vector));
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := -1;
    Exit;
  end;
  for I := Lb to Ub do
    if Test(Vector[I]) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function FirstElement(M:TMatrix; LRow, URow, LCol, UCol : integer; Test:TTestFunc):TMatCoords; overload;
var
  I,J:integer;
begin
  URow := min(URow,High(M));
  if (LRow < 0) or (UCol < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  UCol := min(UCol,High(M[LRow]));
  if (LRow > URow) or (LCol > UCol) then
  begin
    SetErrCode(MatErrDim);
    Result := tmCoords(-1,-1);
    Exit;
  end;
  for I := LRow to URow do
    for J := LCol to UCol do
      if Test(M[I,J]) then
      begin
        Result := tmCoords(I,J);
        Exit;
      end;
  Result := tmCoords(-1,-1);
end;

function FirstElement(Vector:TVector; Lb, Ub : integer; Ref:float; Comparator:TComparator):integer;
var
  I:Integer;
begin
  Ub := min(Ub,High(Vector));
  if (Lb > Ub) or (Lb < 0) then
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
  Result := -1;
end;

function FirstElement(M:TMatrix; LRow, URow, LCol, UCol : integer; Ref:float; Comparator:TComparator):TMatCoords;
var
  I,J:integer;
begin
  URow := min(URow,High(M));
  if (LRow < 0) or (UCol < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  UCol := min(UCol,High(M[LRow]));
  if (LRow > URow) or (LCol > UCol) then
  begin
    SetErrCode(MatErrDim);
    Result := tmCoords(-1,-1);
    Exit;
  end;
  for I := LRow to URow do
    for J := LCol to UCol do
      if Comparator(M[I,J],Ref) then
      begin
        Result := tmCoords(I,J);
        Exit;
      end;
  Result := tmCoords(-1,-1);
end;

function FirstElement(Vector:TIntVector; Lb, Ub : integer; Ref:integer; Comparator:TIntComparator):integer;
var
  I:Integer;
begin
  Ub := min(Ub,High(Vector));
  if (Lb > Ub) or (Lb < 0) then
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
  Result := -1;
end;

function FirstElement(M:TIntMatrix; LRow, URow, LCol, UCol : integer; Ref:integer; Comparator:TIntComparator):TMatCoords;
var
  I,J:integer;
begin
  URow := min(URow,High(M));
  if (LRow < 0) or (UCol < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  UCol := min(UCol,High(M[LRow]));
  if (LRow > URow) or (LCol > UCol) then
  begin
    SetErrCode(MatErrDim);
    Result := tmCoords(-1,-1);
    Exit;
  end;
  for I := LRow to URow do
    for J := LCol to UCol do
      if Comparator(M[I,J],Ref) then
      begin
        Result := tmCoords(I,J);
        Exit;
      end;
  Result := tmCoords(-1,-1);
end;

function FirstElement(Vector:TVector; Lb, Ub : integer; Ref:float; CompType:TCompOperator):integer; overload;
var
  I:integer;
  CMP:boolean;
begin
  Ub := min(Ub,High(Vector));
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := -1;
    Exit;
  end;
  for I := Lb to Ub do
  begin
    case CompType of
      LT: CMP := Vector[I] < Ref;
      LE: CMP := Vector[I] <= Ref;
      EQ: CMP := SameValue(Vector[I],Ref);
      GE: CMP := Vector[I] >= Ref;
      GT: CMP := Vector[I] > Ref;
      NE: CMP := not SameValue(Vector[I],Ref);
    end;
    if CMP then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function FirstElement(M:TMatrix; LRow, URow, LCol, UCol : integer; Ref:float; CompType:TCompOperator):TMatCoords; overload;
var
  I,J:integer;
  CMP:boolean;
begin
  URow := min(URow,High(M));
  if (LRow < 0) or (UCol < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  UCol := min(UCol,High(M[LRow]));
  if (LRow > URow) or (LCol > UCol) then
  begin
    SetErrCode(MatErrDim);
    Result := tmCoords(-1,-1);
    Exit;
  end;
  for I := LRow to URow do
    for J := LCol to UCol do
    begin
      case CompType of
        LT: CMP := M[I,J] < Ref;
        LE: CMP := M[I,J] <= Ref;
        EQ: CMP := SameValue(M[I,J],Ref);
        GE: CMP := M[I,J] >= Ref;
        GT: CMP := M[I,J] > Ref;
        NE: CMP := not SameValue(M[I,J],Ref);
      end;
      if CMP then
      begin
        Result := tmCoords(I,J);
        Exit;
      end;
    end;
  Result := tmCoords(-1,-1);
end;

function FirstElement(Vector:TIntVector; Lb, Ub : integer; Ref:integer; CompType:TCompOperator):integer; overload;
var
  I:integer;
  CMP:boolean;
begin
  Ub := min(Ub,High(Vector));
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := -1;
    Exit;
  end;
  for I := Lb to Ub do
  begin
    case CompType of
      LT: CMP := Vector[I] < Ref;
      LE: CMP := Vector[I] <= Ref;
      EQ: CMP := Vector[I] = Ref;
      GE: CMP := Vector[I] >= Ref;
      GT: CMP := Vector[I] > Ref;
      NE: CMP := Vector[I] <> Ref;
    end;
    if CMP then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function FirstElement(M:TIntMatrix; LRow, URow, LCol, UCol : integer; Ref:integer; CompType:TCompOperator):TMatCoords; overload;
var
  I,J:integer;
  CMP:boolean;
begin
  URow := min(URow,High(M));
  if (LRow < 0) or (UCol < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  UCol := min(URow,High(M[LRow]));
  if (LRow > URow) or (LCol > UCol) then
  begin
    SetErrCode(MatErrDim);
    Result := tmCoords(-1,-1);
    Exit;
  end;
  for I := LRow to URow do
    for J := LCol to UCol do
    begin
      case CompType of
        LT: CMP := M[I,J] < Ref;
        LE: CMP := M[I,J] <= Ref;
        EQ: CMP := M[I,J] = Ref;
        GE: CMP := M[I,J] >= Ref;
        GT: CMP := M[I,J] > Ref;
        NE: CMP := M[I,J] <> Ref;
      end;
      if CMP then
      begin
        Result := tmCoords(I,J);
        Exit;
      end;
    end;
  Result := tmCoords(-1,-1);
end;

function MaxLoc(Vector:TVector; Lb, Ub:integer):integer; overload;
var
  I:integer;
  MaxVal:float;
begin
  Result := Lb;
  Ub := min(Ub,High(Vector));
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := -1;
    Exit;
  end;
  MaxVal := Vector[Lb];
  for I := Lb+1 to Ub do
    if Vector[I] > MaxVal then
    begin
      MaxVal := Vector[I];
      Result := I;
    end;
end;

function MaxLoc(M:TMatrix; LRow,URow,LCol,UCol:integer):TMatCoords;overload;
var
  I,J:integer;
  MaxVal:float;
begin
  URow := min(URow,High(M));
  if (LRow < 0) or (UCol < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  UCol := min(UCol,High(M[LRow]));
  if (LRow > URow) or (LCol > UCol) then
  begin
    SetErrCode(MatErrDim);
    Result := tmCoords(-1,-1);
    Exit;
  end;
  Result := tmCoords(LRow,LCol);
  MaxVal := M[LRow,LCol];
  for I := LRow to URow do
    for J := LCol to UCol do
    if M[I,J] > MaxVal then
    begin
      MaxVal := M[I,J];
      Result := tmCoords(I,J);
    end;
end;

function MaxLoc(Vector:TIntVector; Lb, Ub:integer):integer; overload;
var
  I:integer;
  MaxVal:float;
begin
  Result := Lb;
  Ub := min(Ub,High(Vector));
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := -1;
    Exit;
  end;
  MaxVal := Vector[Lb];
  for I := Lb+1 to Ub do
    if Vector[I] > MaxVal then
    begin
      MaxVal := Vector[I];
      Result := I;
    end;
end;

function MaxLoc(M:TIntMatrix; LRow,URow,LCol,UCol:integer):TMatCoords;overload;
var
  I,J:integer;
  MaxVal:float;
begin
  URow := min(URow,High(M));
  if (LRow < 0) or (UCol < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  UCol := min(UCol,High(M[LRow]));
  if (LRow > URow) or (LCol > UCol) then
  begin
    SetErrCode(MatErrDim);
    Result := tmCoords(-1,-1);
    Exit;
  end;
  Result := tmCoords(LRow,LCol);
  MaxVal := M[LRow,LCol];
  for I := LRow to URow do
    for J := LCol to UCol do
    if M[I,J] > MaxVal then
    begin
      MaxVal := M[I,J];
      Result := tmCoords(I,J);
    end;
end;

function MinLoc(Vector:TVector; Lb, Ub:integer):integer; overload;
var
  I:integer;
  MinVal:float;
begin
  Ub := min(Ub,High(Vector));
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := -1;
    Exit;
  end;
  Result := Lb;
  MinVal := Vector[Lb];
  for I := Lb+1 to Ub do
    if Vector[I] < MinVal then
    begin
      MinVal := Vector[I];
      Result := I;
    end;
end;

function MinLoc(M:TMatrix; LRow,URow,LCol,UCol:integer):TMatCoords;overload;
var
  I,J:integer;
  MinVal:float;
begin
  URow := min(URow,High(M));
  UCol := min(UCol,High(M[LRow]));
  if (LRow > URow) or (LCol > UCol) or (LCol < 0) or (LRow < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := tmCoords(-1,-1);
    Exit;
  end;
  Result := tmCoords(LRow,LCol);
  MinVal := M[LRow,LCol];
  for I := LRow to URow do
    for J := LCol to UCol do
    if M[I,J] < MinVal then
    begin
      MinVal := M[I,J];
      Result := tmCoords(I,J);
    end;
end;

function MinLoc(Vector:TIntVector; Lb, Ub:integer):integer; overload;
var
  I:integer;
  MinVal:float;
begin
  Ub := min(Ub,High(Vector));
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := -1;
    Exit;
  end;
  Result := Lb;
  MinVal := Vector[Lb];
  for I := Lb+1 to Ub do
    if Vector[I] < MinVal then
    begin
      MinVal := Vector[I];
      Result := I;
    end;
end;

function MinLoc(M:TIntMatrix; LRow,URow,LCol,UCol:integer):TMatCoords;overload;
var
  I,J:integer;
  MinVal:float;
begin
  URow := min(URow,High(M));
  UCol := min(UCol,High(M[LRow]));
  if (LRow > URow) or (LCol > UCol)  or (LCol < 0) or (LRow < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := tmCoords(-1,-1);
    Exit;
  end;
  Result := tmCoords(LRow,LCol);
  MinVal := M[LRow,LCol];
  for I := LRow to URow do
    for J := LCol to UCol do
    if M[I,J] < MinVal then
    begin
      MinVal := M[I,J];
      Result := tmCoords(I,J);
    end;
end;

function Seq(Lb, Ub : integer; first, increment:Float; Vector:TVector = nil):TVector;
var
  I:integer;
begin
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  if Vector = nil then
    DimVector(Vector, Ub)
  else
    Ub := min(Ub,High(Vector));
  Vector[Lb] := 0;
  for I := Lb+1 to Ub do                 // 2 cycles to avoid rounding error if
    Vector[I] := Vector[I-1]+increment;  //First is very large and increment small
  for I := Lb to Ub do
    Vector[I] := Vector[I]+first;
  Result := Vector;
end;

function ISeq(Lb, Ub : integer; first, increment:integer; Vector:TIntVector = nil):TIntVector;
var
  I:integer;
begin
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  if Vector = nil then
    DimVector(Vector,Ub)
  else
    Ub := min(Ub,High(Vector));
  Vector[Lb] := First;
  for I := Lb+1 to Ub do
    Vector[I] := Vector[I-1]+increment;
  Result := Vector;
end;

function SelElements(Vector:TVector; Lb, Ub, ResLb : integer; Ref: float; CompType:TCompOperator):TIntVector; overload;
var
  I,N:integer;
  Cmp:boolean;
begin
  Ub := Min(Ub,High(Vector));
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Exit;
  end;
  DimVector(Result,Ub-Lb+ResLb);
  N := ResLb-1;
  for I := Lb to Ub do
    begin
    case CompType of
      LT: CMP := Vector[I] < Ref;
      LE: CMP := Vector[I] <= Ref;
      EQ: CMP := SameValue(Vector[I],Ref);
      GE: CMP := Vector[I] >= Ref;
      GT: CMP := Vector[I] > Ref;
      NE: CMP := not SameValue(Vector[I],Ref);
    end;
    if CMP then
    begin
      inc(N);
      Result[N] := I;
    end;
  end;
  if N >= ResLb then
    SetLength(Result,N+1)
  else
    SetLength(Result,0);
end;

function SelElements(Vector:TVector; Lb, Ub, ResLb : integer; Ref:float; Comparator:TComparator):TIntVector; overload;
var
  I,N:integer;
begin
  Ub := min(high(Vector),Ub);
  if (Lb > Ub) or (Lb < 0) then
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

function SelElements(Vector: TVector; Lb, Ub, ResLb: integer; Test: TTestFunc): TIntVector;
var
  I,N:integer;
begin
  Ub := min(high(Vector),Ub);
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  DimVector(Result,Ub-Lb+ResLb);
  N := ResLb-1;
  for I := Lb to Ub do
    if Test(Vector[I]) then
    begin
      inc(N);
      Result[N] := I;
    end;
  if N >= ResLb then
    SetLength(Result,N+1)
  else
    SetLength(Result,0);
end;

function SelElements(Vector:TIntVector; Lb, Ub, ResLb : integer; Ref: Integer; CompType:TCompOperator):TIntVector;
var
  I,N:integer;
  Cmp:boolean;
begin
  Ub := min(Ub,High(Vector));
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  DimVector(Result,Ub-Lb+ResLb);
  N := ResLb-1;
  for I := Lb to Ub do
    begin
    case CompType of
      LT: CMP := Vector[I] < Ref;
      LE: CMP := Vector[I] <= Ref;
      EQ: CMP := Vector[I] = Ref;
      GE: CMP := Vector[I] >= Ref;
      GT: CMP := Vector[I] > Ref;
      NE: CMP := Vector[I] <> Ref;
    end;
    if CMP then
    begin
      inc(N);
      Result[N] := I;
    end;
  end;
  if N  >= ResLb then
    SetLength(Result,N+1)
  else
    SetLength(Result,0);
end;

function SelElements(Vector:TIntVector; Lb, Ub, ResLb : integer; Ref:Integer; Comparator:TIntComparator):TIntVector; overload;
var
  I,N:integer;
begin
  Ub := min(Ub,High(Vector));
  if (Lb > Ub) or (Lb < 0) then
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
  if N  >= ResLb then
    SetLength(Result,N+1)
  else
    SetLength(Result,0);
end;

function SelElements(Vector:TIntVector; Lb, Ub, ResLb : integer; Test:TIntTestFunc):TIntVector; overload;
var
  I,N:integer;
begin
  Ub := min(high(Vector),Ub);
  if (Lb > Ub) or (Lb < 0) then
  begin
    SetErrCode(MatErrDim);
    Result := nil;
    Exit;
  end;
  DimVector(Result,Ub-Lb+ResLb);
  N := ResLb-1;
  for I := Lb to Ub do
    if Test(Vector[I]) then
    begin
      inc(N);
      Result[N] := I;
    end;
  if N >= ResLb then
    SetLength(Result,N+1)
  else
    SetLength(Result,0);
end;

function ExtractElements(Vector: TVector; Mask: TIntVector): TVector;
var
  I,L,LM,C:integer;
  Buf:TVector;
begin
  Result := nil;
  L := High(Vector);
  LM := High(Mask);
  if (L < 0) or (LM < 0) then
    Exit;
  DimVector(Buf,LM);
  for I := 0 to LM do
  begin
    C := Mask[I];
    if (C < 0) or (C > L) then
    begin
      SetErrCode(MatErrDim);
      Exit;
    end;
    Buf[I] := Vector[Mask[I]];
  end;
  Result := Buf;
end;

end.

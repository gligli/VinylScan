{ ******************************************************************
  Types and constants - Error handling - Dynamic arrays
  ******************************************************************
  The default real type is DOUBLE (8-byte real).
  Other types may be selected by defining the symbols:

       SINGLEREAL   (Single precision, 4 bytes)
       EXTENDEDREAL (Extended precision, 10 bytes)
  ****************************************************************** }

unit utypes;

interface
{$mode ObjFPC}{$H+}
{$i types.inc}
{-------------------------------------------------------------------
Approximate equality functions
--------------------------------------------------------------------}

// Compares to zero with epsilon. If Epsilon is -1 (default),
//default value as set with SetZeroEpsilon is used.
// if SetZeroEpsilon was not used, MachEp / 8 is used
function IsZero(F:Float): boolean; overload;
function IsZero(F: Float; Epsilon: Float): Boolean; overload;

// Test if given vaule is NAN
function IsNan(F : Float): Boolean;

// Returns true if abs(A-B) < epsilon.
function SameValue(A, B: Float; epsilon:float): Boolean; overload;

// Uses DefaultZeroEpsilon for test of equality with relative epsilon.
//DefaultZeroEpsilon may be set prior to call with SetEpsilon.
//Otherwise, it is MachEp.
function SameValue(A,B:Float):boolean; overload;

{ ------------------------------------------------------------------
  Dynamic arrays
  ------------------------------------------------------------------ }
{ Creates floating point vector V[0..Ub] }
procedure DimVector(var V : TVector; Ub : Integer); overload;

{ Creates integer vector V[0..Ub] }
procedure DimVector(var V : TIntVector; Ub : Integer); overload;

{ Creates complex vector V[0..Ub] }
procedure DimVector(var V : TCompVector; Ub : Integer); overload;

{ Creates vector V[0..Ub] of TRealPoint}
procedure DimVector(var V: TRealPointVector; Ub: Integer); overload;

{ Creates boolean vector V[0..Ub] }
procedure DimVector(var V : TBoolVector; Ub : Integer); overload;

{ Creates string vector V[0..Ub] }
procedure DimVector(var V : TStrVector; Ub : Integer); overload;

{ Creates floating point matrix A[0..Ub1, 0..Ub2] }
procedure DimMatrix(var A : TMatrix; Ub1, Ub2 : Integer); overload;

{ Creates integer matrix A[0..Ub1, 0..Ub2] }
procedure DimMatrix(var A : TIntMatrix; Ub1, Ub2 : Integer); overload;

{ Creates complex matrix A[0..Ub1, 0..Ub2] }
procedure DimMatrix(var A : TCompMatrix; Ub1, Ub2 : Integer); overload;

{ Creates boolean matrix A[0..Ub1, 0..Ub2] }
procedure DimMatrix(var A : TBoolMatrix; Ub1, Ub2 : Integer); overload;

{ Creates string matrix A[0..Ub1, 0..Ub2] }
procedure DimMatrix(var A : TStrMatrix; Ub1, Ub2 : Integer); overload;

{Sets default epsilon for SameValue }
procedure SetEpsilon(AEpsilon: float);

{sets default epsilon for comparison of a number to rero (IsZero function
and to compare two numbers near zero.}
procedure SetZeroEpsilon(AZeroEpsilon: Float);

var
  DefaultZeroEpsilon: Float = MachEp / 8;
  DefaultEpsilon: Float = MachEp;

implementation

function IsZero(F: Float; Epsilon: Float): Boolean;
begin
  Result := Abs(F) < Epsilon;
end;

function IsZero(F: Float): Boolean;
begin
  Result := Abs(F) < DefaultZeroEpsilon;
end;

{$if defined(SingleReal)}
function IsNan(F: Float): Boolean;
begin
    result:=(longword(d) and $7fffffff)>$7f800000;
end;
{$endif}
{$if defined(DoubleReal)}
type
  TSplitDouble = packed record
    cards: Array[0..1] of cardinal;
  end;

function IsNaN(F:Float): boolean;
var
  fraczero, expMaximal: boolean;
begin
  expMaximal := ((TSplitDouble(F).cards[1] shr 20) and $7ff) = 2047;
  fraczero := (TSplitDouble(F).cards[1] and $fffff = 0) and
              (TSplitDouble(F).cards[0] = 0);
  Result:=expMaximal and not(fraczero);
end;
{$endif}
{$if defined(ExtendedReal)}
function IsNan(F : Float): Boolean;
type
  TSplitExtended = packed record
    case byte of
      0: (bytes: Array[0..9] of byte);
      1: (words: Array[0..4] of word);
      2: (cards: Array[0..1] of cardinal; w: word);
  end;
var
  fraczero, expMaximal: boolean;
begin
  expMaximal := (TSplitExtended(F).w and $7fff) = 32767;
  fraczero := (TSplitExtended(F).cards[0] = 0) and
                    ((TSplitExtended(F).cards[1] and $7fffffff) = 0);
  Result:=expMaximal and not(fraczero);
end;

{$endif}

function SameValue(A, B: Float; Epsilon : float): Boolean;
begin
  Result := abs(A-B) < abs(epsilon);
end;

function SameValue(A, B: Float): Boolean;
var
  C,D:float;
  APos, BPos : boolean;
begin
  APos := A > 0; // these 4 lines are needed to prevent float overflow
  BPos := B > 0; // if very large negative and very large positive values are tested
  Result := not ((APos xor BPos) and not (IsZero(A) and IsZero(B)));
  if not Result then Exit;
  if A > B then
    D := A-B
  else
    D := B-A;
  Result := D < DefaultZeroEpsilon; // this is needed for comparisons of very small numbers
  if Result then Exit;
  A := abs(A);
  B := abs(B);
  if A > B then
    C := A
  else
    C := B;
  Result := D < C*DefaultEpsilon;
end;

procedure DimVector(var V : TVector; Ub : Integer);
begin
  { Check bounds }
  if (Ub < 0) or (Ub > MaxVecLength) then
  begin
    V := nil;
    Exit;
  end;
  { Allocate vector }
  SetLength(V, Ub + 1);
end;

procedure DimVector(var V : TIntVector; Ub : Integer);
begin
  { Check bounds }
  if (Ub < 0) or (Ub > MaxIntLength) then
    begin
      V := nil;
      Exit;
    end;
  { Allocate vector }
  SetLength(V, Ub + 1);
end;

procedure DimVector(var V : TCompVector; Ub : Integer);
begin
  { Check bounds }
  if (Ub < 0) or (Ub > MaxCompLength) then
    begin
      V := nil;
      Exit;
    end;
  { Allocate vector }
  SetLength(V, Ub + 1);
end;

procedure DimVector(var V: TRealPointVector; Ub: Integer);
begin
  { Check bounds }
  if (Ub < 0) or (Ub > MaxCompLength) then
  begin
    V := nil;
    Exit;
  end;
  SetLength(V, Ub + 1);
end;

procedure DimVector(var V : TBoolVector; Ub : Integer);
begin
  { Check bounds }
  if (Ub < 0) or (Ub > MaxBoolLength) then
    begin
      V := nil;
      Exit;
    end;
  SetLength(V, Ub + 1);
end;

procedure DimVector(var V : TStrVector; Ub : Integer);
begin
  { Check bounds }
  if (Ub < 0) or (Ub > MaxIntLength) then  //because element of string array is essentially pointer
    begin
      V := nil;
      Exit;
    end;
  SetLength(V, Ub + 1);
end;

procedure DimMatrix(var A : TMatrix; Ub1, Ub2 : Integer);
begin
  if (Ub1 < 0) or (Ub2 < 0) or (Ub1 > MaxVecLength) or (Ub2 > MaxVecLength) then
    begin
      A := nil;
      Exit;
    end;
  SetLength(A, Ub1 + 1, Ub2 + 1);
end;

procedure DimMatrix(var A : TIntMatrix; Ub1, Ub2 : Integer);
begin
  { Check bounds }
  if (Ub1 < 0) or (Ub2 < 0) or (Ub1 > MaxIntLength) or (Ub2 > MaxIntLength) then
  begin
    A := nil;
    Exit;
  end;
  { Allocate matrix }
  SetLength(A, Ub1 + 1, Ub2 + 1);
end;

procedure DimMatrix(var A : TCompMatrix; Ub1, Ub2 : Integer);
begin
  { Check bounds }
  if (Ub1 < 0) or (Ub2 < 0) or (Ub1 > MaxCompLength) or (Ub2 > MaxCompLength) then
     begin
       A := nil;
       Exit;
     end;
  { Allocate matrix }
  SetLength(A, Ub1 + 1, Ub2 + 1);
end;

procedure DimMatrix(var A : TBoolMatrix; Ub1, Ub2 : Integer);
begin
  { Check bounds }
  if (Ub1 < 0) or (Ub2 < 0) or (Ub1 > MaxBoolLength) or (Ub2 > MaxBoolLength) then
    begin
      A := nil;
      Exit;
    end;
  { Allocate matrix }
  SetLength(A, Ub1 + 1, Ub2 + 1);
end;

procedure DimMatrix(var A : TStrMatrix; Ub1, Ub2 : Integer);
begin
  { Check bounds }
  if (Ub1 < 0) or (Ub2 < 0) or (Ub1 > MaxIntLength) or (Ub2 > MaxIntLength) then
    begin
      A := nil;
      Exit;
    end;
  { Allocate matrix }
  SetLength(A, Ub1 + 1, Ub2 + 1);
end;

procedure SetEpsilon(AEpsilon: float);
begin
  DefaultEpsilon := AEpsilon;
end;

procedure SetZeroEpsilon(AZeroEpsilon: Float);
begin
  DefaultZeroEpsilon := AZeroEpsilon;
end;

end.

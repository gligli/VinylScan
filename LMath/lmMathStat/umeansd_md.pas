{ ******************************************************************
  Mean and standard deviations, aware of missing data
  ****************************************************************** }
{$boolEval off}
unit umeansd_md;

interface

uses
  utypes, uMinMax, uVecUtils;

var
  MissingData: Float = NAN;

{returns true if F is NAN or Missing data}
function Undefined(F:Float):boolean; overload;

function Undefined(A: array of Float; N : integer):boolean; overload;

function Defined(F:Float):boolean; overload;

function Defined(A: array of Float; N : integer):boolean; overload;

function RemoveUndefined(X : TVector; Lb:integer = 0; Ub: integer = -1):TVector;

{set missing data code}
procedure SetMD(aMD:float);

{Finds first defined element in array}
function FirstDefined(X:TVector; Lb:integer = 0; Ub: integer = -1):integer;

function LastDefined(X:TVector; Lb:integer = 0; Ub: integer = -1):integer;

{valid (defined) number of elements in array}
function ValidN(X:TVector; Lb, Ub: integer):integer;
function ValidN(constref X:array of Float):integer;

{ Minimum of sample X }
function Min(X : TVector; Lb, Ub: integer) : Float; overload;
function Min(constref X:array of float) : float; overload;

{ Maximum of sample X }
function Max(X : TVector; Lb, Ub: integer) : Float; overload;
function Max(constref X : array of float) : Float; overload;

{Sum of sample X}
function Sum(X:TVector; Lb, Ub : integer) : Float; overload;
function Sum(constref X:array of float) : Float; overload;

{ Mean of sample X }
function Mean(X : TVector; Lb, Ub: integer) : Float; overload;
function Mean(constref X : array of float) : Float; overload;

{ Standard deviation estimated from sample X }
function StDev(X : TVector; Lb, Ub: integer) : Float; overload;
function StDev(constref X : array of float) : Float; overload;

{ Standard deviation of population }
function StDevP(X : TVector; Lb, Ub: integer) : Float; overload;
function StDevP(constref X : array of float) : Float; overload;

function StdErr(X : TVector; Lb, Ub: integer) : Float;
function StdErr(constref X : array of float) : Float;

implementation

function Undefined(F: Float): boolean;
begin
  Result := IsNAN(F) or (F = MissingData);
end;

function Undefined(A: array of Float; N: integer): boolean;
begin
  Result := (High(A) < N) or Undefined(A[N]);
end;

function Defined(F:Float):boolean;
begin
  Result := (not IsNan(F)) and (F <> MissingData);
end;

function Defined(A: array of Float; N: integer): boolean;
begin
  Result := (High(A) >= N) and Defined(A[N]);
end;

function RemoveUndefined(X : TVector; Lb:integer = 0; Ub: integer = -1):TVector;
var
  Mask:TIntVector;
begin
  if not InRange(Lb,0,High(X)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(X)) then
    Ub := High(X);
  Mask := SelElements(X, Lb, Ub, 0, @Defined);
  Result := ExtractElements(X,Mask);
end;

procedure SetMD(aMD:float);
begin
  MissingData := aMD;
end;

function FirstDefined(X:TVector; Lb:integer = 0; Ub: integer = -1):integer;
var
  I:integer;
begin
  if not InRange(Lb,0,High(X)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(X)) then
    Ub := High(X);
  I := Lb;
  while Undefined(X[I]) and (I <= Ub) do inc(I);
  if Undefined(X[I]) then
    Result := -1
  else
    Result := I;
end;

function LastDefined(X: TVector; Lb:integer = 0; Ub: integer = -1): integer;
var
  I:integer;
begin
  if not InRange(Lb,0,High(X)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(X)) then
    Ub := High(X);
  I := Ub;
  while Undefined(X[I]) and (I > Lb) do
    dec(I);
  if Undefined(X[I]) then
    Result := -1
  else
    Result := I;
end;

function ValidN(X:TVector; Lb, Ub: integer):integer;
var
  I,S:integer;
begin
  S := 0;
  if not InRange(Lb,0,High(X)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(X)) then
    Ub := High(X);
  for I := Lb to Ub do
    if Defined(X[I]) then inc(S);
  Result := S;
end;

function ValidN(constref X: array of Float): integer;
var
  I,S:integer;
begin
  S := 0;
  for I := 0 to High(X) do
    if Defined(X[I]) then inc(S);
  Result := S;
end;

function Min(X : TVector; Lb, Ub: integer) : Float;
var
  Xmin : Float;
  I,M    : Integer;
begin
  if not InRange(Lb,0,High(X)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(X)) then
    Ub := High(X);
  M := LastDefined(X,Lb,Ub); // (1) to handle the case with ValidN = 0
  if M = -1 then             // (2) to reduce unnecessary comparisons
    Result := MissingData    // Missing data in the tail are more frequent
  else begin                 // then at the beginnning
    Xmin := X[M];
    for I := Pred(M) downto Lb do
      if not Undefined(X[I]) and (X[I] < Xmin) then
        Xmin := X[I];
    Result := Xmin;
  end;
end;

function Min(constref X: array of float): float;
var
  Xmin : Float;
  I, Ub : Integer;
begin
  Ub := high(X);
  while Undefined(X[Ub]) and (Ub >= 0) do
    dec(Ub);
  if Undefined(X[Ub]) then
    Result := MissingData
  else begin
    Xmin := X[Ub];
    for I := Pred(Ub) downto 0 do
      if Defined(X[I]) and (X[I] < Xmin) then
        Xmin := X[I];
    Result := Xmin;
  end;
end;

function Max(X : TVector; Lb, Ub: integer) : Float;
var
  Xmax : Float;
  I    : Integer;
begin
  if not InRange(Lb,0,High(X)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(X)) then
    Ub := High(X);
  Ub := LastDefined(X,Lb,Ub);
  if Ub = -1 then
    Result := MissingData
  else begin
    Xmax := X[Ub];
    for I := Pred(Ub) downto Lb do
      if not Undefined(X[I]) and (X[I] > Xmax) then
        Xmax := X[I];
    Result := Xmax;
  end;
end;

function Max(constref X: array of float): float;
var
  Xmax : Float;
  I, Ub : Integer;
begin
  Result := MissingData;
  Ub := high(X);
  while Undefined(X[Ub]) and (Ub >= 0) do
    dec(Ub);
  if Defined(X[Ub]) then
  begin
    Xmax := X[Ub];
    for I := Pred(Ub) downto 0 do
      if Defined(X[I]) and (X[I] > Xmax) then
        Xmax := X[I];
    Result := Xmax;
  end;
end;

function Sum(X: TVector; Lb, Ub: integer): Float;
var
  SX : float;
  I  : integer;
begin
  if not InRange(Lb,0,High(X)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(X)) then
    Ub := High(X);
  Sx := 0;
  for I := Lb to Ub do
    if Defined(X[I]) then
      Sx := Sx + X[I];
  Result := Sx;
end;

function Sum(constref X: array of float): Float;
var
  Sx:float;
  I:integer;
begin
  Sx := 0;
  for I := 0 to High(X) do
    if Defined(X[I]) then
      Sx := Sx + X[I];
  Result := Sx;
end;

function Mean(X : TVector; Lb, Ub: integer) : Float;
var
  SX     : Float;
  I,N  : Integer;
begin
  if length(X) = 0 then
  begin
    Result := MissingData;
    Exit;
  end;
  if not InRange(Lb,0,High(X)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(X)) then
    Ub := High(X);
  Ub := LastDefined(X,Lb,Ub);
  SX := 0.0;
  if Ub = -1 then
    Result := MissingData
  else begin
    N := 0;
    for I := Ub downto Lb do
    if not Undefined(X[I]) then
    begin
      SX := SX + X[I];
      inc(N);
    end;
    Mean := SX / N;
  end;
end;

function Mean(constref X: array of float): Float;
var
  SX     : Float;
  I,N,Ub  : Integer;
begin
  if length(X) = 0 then
  begin
    Result := MissingData;
    Exit;
  end;
  Ub := high(X);
  while Undefined(X[Ub]) and (Ub >= 0) do
    dec(Ub);
  if Undefined(X[Ub]) then
    Result := MissingData
  else begin
    SX := 0.0;
    N := 0;
    for I := 0 to Ub do
      if Defined(X[I]) then
      begin
        SX := SX + X[I];
        inc(N);
      end;
    Mean := SX / N;
  end;
end;

function StDev(X : TVector; Lb, Ub: integer) : Float;
var
  D, SD, SD2, V, MeanV: Float;
  I, N, M : Integer;
begin
  if length(X) = 0 then
  begin
    Result := MissingData;
    Exit;
  end;
  if not InRange(Lb,0,High(X)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(X)) then
    Ub := High(X);
  N := ValidN(X,Lb,Ub);
  if (N = 0) or (N = 1) then
  begin
    Result := MissingData;
    Exit;
  end;
  MeanV := Mean(X,Lb,Ub);
  M := FirstDefined(X,Lb,Ub);
  SD  := 0.0;  { Sum of deviations (used to reduce roundoff error) }
  SD2 := 0.0;  { Sum of squared deviations }
  for I := M to Ub do
  if not Undefined(X[I]) then
  begin
    D := X[I] - MeanV;
    SD := SD + D;
    SD2 := SD2 + Sqr(D)
  end;
  V := (SD2 - Sqr(SD) / N) / (N - 1);  { Variance }
  StDev := Sqrt(V);
end;

function StDev(constref X: array of float): Float;
var
  D, SD, SD2, V, MeanV: Float;
  I, N, Ub : Integer;
begin
  if length(X) = 0 then
  begin
    Result := MissingData;
    Exit;
  end;
  Ub := High(X);
  N := ValidN(X);
  if (N = 0) or (N = 1) then
  begin
    Result := MissingData;
    Exit;
  end;
  SD  := 0.0;  { Sum of deviations (used to reduce roundoff error) }
  SD2 := 0.0;  { Sum of squared deviations }
  MeanV := Mean(X);
  for I := 0 to Ub do
    if Defined(X[I]) then
    begin
      D := X[I] - MeanV;
      SD := SD + D;
      SD2 := SD2 + Sqr(D)
    end;
  V := (SD2 - Sqr(SD) / N) / (N - 1);  { Variance }
  StDev := Sqrt(V);
end;

function StDevP(X : TVector; Lb, Ub: integer) : Float;
var
  D, SD, SD2, V, MeanV : Float;
  I, N, M : Integer;
begin
  if length(X) = 0 then
  begin
    Result := MissingData;
    Exit;
  end;
  if not InRange(Lb,0,High(X)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(X)) then
    Ub := High(X);
  N := ValidN(X,Lb,Ub);
  if N = 0 then
  begin
    Result := MissingData;
    Exit;
  end;
  M := FirstDefined(X,Lb,Ub);
  MeanV := Mean(X,Lb,Ub);
  SD  := 0.0;  { Sum of deviations (used to reduce roundoff error) }
  SD2 := 0.0;  { Sum of squared deviations }
  for I := M to Ub do
    if Defined(X[I]) then
    begin
      D := X[I] - MeanV;
      SD := SD + D;
      SD2 := SD2 + Sqr(D)
    end;
  V := (SD2 - Sqr(SD) / N) / N;  { Variance }
  StDevP := Sqrt(V);
end;

function StDevP(constref X: array of float): Float;
var
  D, SD, SD2, V, MeanV : Float;
  I, N : Integer;
begin
  N := ValidN(X);
  if N = 0 then
  begin
    Result := MissingData;
    Exit;
  end;
  MeanV := Mean(X);
  SD  := 0.0;  { Sum of deviations (used to reduce roundoff error) }
  SD2 := 0.0;  { Sum of squared deviations }
  for I := 0 to High(X) do
    if Defined(X[I]) then
    begin
      D := X[I] - MeanV;
      SD := SD + D;
      SD2 := SD2 + Sqr(D)
    end;
  V := (SD2 - Sqr(SD) / N) / N;  { Variance }
  StDevP := Sqrt(V);
end;

function StdErr(X: TVector; Lb, Ub: integer): Float;
var
  N:integer;
begin
  if length(X) = 0 then
  begin
    Result := MissingData;
    Exit;
  end;
  if not InRange(Lb,0,High(X)) then
    Lb := 0;
  if not InRange(Ub,Lb,High(X)) then
    Ub := High(X);
  N := ValidN(X,Lb,Ub);
  if N = 0 then
  begin
    Result := MissingData;
    Exit;
  end;
  Result := StDev(X,Lb,Ub)/Sqrt(N);
end;

function StdErr(constref X: array of float): Float;
var
  N:integer;
begin
  if length(X) = 0 then
  begin
    Result := MissingData;
    Exit;
  end;
  N := ValidN(X);
  if N = 0 then
  begin
    Result := MissingData;
    Exit;
  end;
  Result := StDev(X)/Sqrt(N);
end;

end.

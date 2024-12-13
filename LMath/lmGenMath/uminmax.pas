{ ******************************************************************
  Minimum, maximum, sign and exchange
  ****************************************************************** }

unit uminmax;

interface

uses
  utypes, uErrors;

{ Minimum of 2 reals }
function Min(X, Y : Float) : Float; overload;

{ Maximum of 2 reals }
function Max(X, Y : Float) : Float; overload;

 { Minimum of 2 integers }
function Min(X, Y : Integer) : Integer; overload;

{ Maximum of 2 integers }
function Max(X, Y : Integer) : Integer; overload;

{ Sign (returns 1 if X = 0) }
function Sgn(X : Float) : Integer; overload;

function Sgn(X : integer) : integer; overload;

{ Sign (returns 0 if X = 0) }
function Sgn0(X : Float) : Integer; overload;

function Sgn0(X : integer) : integer; overload;

{ if b negative, result is -A otherwize result is A }
function DSgn(A, B : Float) : Float;

// compatibility with Math unit
function Sign(X: Float):integer; inline;

// returns true if X is negative ( < -DefaultZeroEpsilon )
function IsNegative(X: float):boolean; overload;

function IsNegative(X: Integer):boolean; overload;

// returns true if X is positive ( > DefaultZeroEpsilon )
function IsPositive(X: float):boolean; overload;

function IsPositive(X: Integer):boolean; overload;

{ Exchange 2 reals }
procedure Swap(var X, Y : Float);   overload; inline;

{ Exchange 2 integers }
procedure Swap(var X, Y : Integer); overload; inline;

function InRange(AVal, AMin, AMax:float):boolean;

function InRange(AVal, AMin, AMax:integer):boolean;

implementation

  function Min(X, Y : Float) : Float;
  begin
    if X <= Y then Min := X else Min := Y;
  end;

  function Max(X, Y : Float) : Float;
  begin
    if X >= Y then Max := X else Max := Y;
  end;

  function Min(X, Y : Integer) : Integer;
  begin
    if X <= Y then Min := X else Min := Y;
  end;

  function Max(X, Y : Integer) : Integer;
  begin
    if X >= Y then Max := X else Max := Y;
  end;

  function Sgn(X : Float) : Integer;
  begin
    if X >= 0.0 then Sgn := 1 else Sgn := - 1;
  end;

  function Sgn(X : integer) : Integer;
  begin
    if X >= 0 then Sgn := 1 else Sgn := - 1;
  end;

  function Sgn0(X : Float) : Integer;
  begin
    if IsZero(X) then // which means that if -DefZeroEpsilon < X < defZeroEpsilon
      Sgn0 := 0       // then 0 is returned and next is safe to compare to 0
    else if X > 0.0 then  
      Sgn0 := 1
    else
      Sgn0 := - 1;
  end;

  function Sgn0(X : Integer) : Integer;
  begin
    if X > 0 then
      Sgn0 := 1
    else if X = 0 then
      Sgn0 := 0
    else
      Sgn0 := - 1;
  end;

  function DSgn(A, B : Float) : Float;                       
  begin                                                      
    if B < 0.0 then DSgn := - Abs(A) else DSgn := Abs(A)     
  end;                                                       

  function Sign(X: Float): integer;
  begin
    Result := Sgn0(X);
  end;

function IsNegative(X: float):boolean;
begin
  Result := X < -DefaultZeroEpsilon;
end;

function IsNegative(X: Integer):boolean;
begin
  Result := X < 0;
end;

function IsPositive(X: float):boolean;
begin
  Result := X > DefaultZeroEpsilon;
end;

function IsPositive(X: Integer):boolean;
begin
  Result := X > 0;
end;

procedure Swap(var X, Y : Float);
var
  Temp : Float;
begin
  Temp := X;
  X := Y;
  Y := Temp;
end;

procedure Swap(var X, Y : Integer);
var
  Temp : Integer;
begin
  Temp := X;
  X := Y;
  Y := Temp;
end;

function InRange(AVal, AMin, AMax: float): boolean;
begin
  if AMax < AMin then
  begin
    SetErrCode(FDomain,'InRange: AMax < AMin');
    Result := false;
    exit;
  end;
  Result := (AVal >= AMin) and (AVal <= AMax);
end;

function InRange(AVal, AMin, AMax: integer): boolean;
begin
  if AMax < AMin then
  begin
    SetErrCode(FDomain,'InRange: AMax < AMin');
    Result := false;
    exit;
  end;
  Result := (AVal >= AMin) and (AVal <= AMax);
end;

end.

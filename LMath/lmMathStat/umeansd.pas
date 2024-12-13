{ ******************************************************************
  Mean and standard deviations
  ****************************************************************** }
{$mode objfpc}
unit umeansd;

interface

uses
  utypes, uErrors;

{ Minimum of sample X }
function Min(X : TVector; Lb, Ub : Integer) : Float; overload;
function Min(constref X:array of float) : float; overload;

{ Maximum of sample X }
function Max(X : TVector; Lb, Ub : Integer) : Float; overload;
function Max(constref X : array of float) : Float; overload;

{Sum of sample X}
function Sum(X:TVector; Lb, Ub : integer) : Float; overload;
function Sum(constref X:array of float) : Float; overload;

{ Mean of sample X }
function Mean(X : TVector; Lb, Ub : Integer) : Float; overload;
function Mean(constref X : array of float) : Float; overload;

{ Standard deviation estimated from sample X }
function StDev(X : TVector; Lb, Ub : Integer; M : Float) : Float; overload;
function StDev(constref X : array of float; M : Float) : Float; overload;

{ Standard deviation of population }
function StDevP(X : TVector; Lb, Ub : integer; M : Float) : Float; overload;
function StDevP(constref X : array of float; M : Float) : Float; overload;


implementation

function Min(X : TVector; Lb, Ub : Integer) : Float;
var
  Xmin : Float;
  I    : Integer;
begin
  Xmin := X[Lb];
  for I := Succ(Lb) to Ub do
    if X[I] < Xmin then Xmin := X[I];
  Min := Xmin;
end;

function Min(constref X: array of float): float;
var
  Xmin : Float;
  I    : Integer;
begin
  Xmin := X[0];
  for I := 1 to High(X) do
    if X[I] < Xmin then Xmin := X[I];
  Min := Xmin;
end;

function Max(X : TVector; Lb, Ub : Integer) : Float;
var
  Xmax : Float;
  I    : Integer;
begin
  Xmax := X[Lb];
  for I := Succ(Lb) to Ub do
    if X[I] > Xmax then Xmax := X[I];
  Max := Xmax;
end;

function Max(constref X: array of float): Float;
var
  Xmax : Float;
  I    : Integer;
begin
  Xmax := X[0];
  for I := 1 to High(X) do
    if X[I] > Xmax then Xmax := X[I];
  Max := Xmax;
end;

function Sum(X: TVector; Lb, Ub: integer): Float;
var
  I:Integer;
begin
  Result := 0;
  for I := Lb to Ub do
    Result := Result + X[I];
end;

function Sum(constref X: array of float): Float;
var
  I:Integer;
begin
  Result := 0;
  for I := 0 to high(X) do
    Result := Result +X[I];
end;

function Mean(X : TVector; Lb, Ub : Integer) : Float;
var
  I  : Integer;
begin
  Result := 0.0;
  for I := Lb to Ub do
    Result := Result + X[I];
  Mean := Result / (Ub - Lb + 1);
end;

function Mean(constref X: array of float): Float;
var
  I, N  : Integer;
begin
  Result := 0.0;
  N := length(X);
  if (N = 0) then
  begin
    SetErrCode(FDomain,'Mean: empty array');
    Result := 0;
    Exit;
  end;
  for I := 0 to High(X) do
    Result := Result + X[I];
  Mean := Result / length(X);
end;

function StDev(X : TVector; Lb, Ub : Integer; M : Float) : Float;
var
  D, SD, SD2, V : Float;
  I, N          : Integer;
begin
  N := Ub - Lb + 1;

  SD  := 0.0;  { Sum of deviations (used to reduce roundoff error) }
  SD2 := 0.0;  { Sum of squared deviations }

  for I := Lb to Ub do
  begin
    D := X[I] - M;
    SD := SD + D;
    SD2 := SD2 + Sqr(D)
  end;

  V := (SD2 - Sqr(SD) / N) / (N - 1);  { Variance }
  StDev := Sqrt(V);
end;

function StDev(constref X: array of float; M: Float): Float;
var
  D, SD, SD2, V : Float;
  I, N          : Integer;
begin
  N := length(X);

  SD  := 0.0;  { Sum of deviations (used to reduce roundoff error) }
  SD2 := 0.0;  { Sum of squared deviations }

  for I := 0 to high(X) do
  begin
    D := X[I] - M;
    SD := SD + D;
    SD2 := SD2 + Sqr(D)
  end;

  V := (SD2 - Sqr(SD) / N) / (N - 1);  { Variance }
  StDev := Sqrt(V);
end;

function StDevP(constref X: array of float; M: Float): Float;
var
  D, SD, SD2, V : Float;
  I, N          : Integer;
begin
  N := length(X);
  SD  := 0.0;  { Sum of deviations (used to reduce roundoff error) }
  SD2 := 0.0;  { Sum of squared deviations }
  for I := 0 to high(X) do
  begin
    D := X[I] - M;
    SD := SD + D;
    SD2 := SD2 + Sqr(D)
  end;

  V := (SD2 - Sqr(SD) / N) / N;  { Variance }
  StDevP := Sqrt(V);
end;

function StDevP(X : TVector; Lb, Ub : Integer; M : Float) : Float;
var
  D, SD, SD2, V : Float;
  I, N          : Integer;
begin
  N := Ub - Lb + 1;

  SD  := 0.0;  { Sum of deviations (used to reduce roundoff error) }
  SD2 := 0.0;  { Sum of squared deviations }

  for I := Lb to Ub do
  begin
    D := X[I] - M;
    SD := SD + D;
    SD2 := SD2 + Sqr(D)
  end;

  V := (SD2 - Sqr(SD) / N) / N;  { Variance }
  StDevP := Sqrt(V);
end;

end.

{ ******************************************************************
  This program computes the convolution product H of two functions
  F and G by the trapezoidal rule. The result is compared with
  the analytical solution.

  The example functions are:

  F(x) = x * exp(-x)
  G(x) = exp(-2 * x)

  The analytical solution is:

  H(x) = (F * G)(x) = (x - 1) * exp(-x) + exp(-2 * x)
  ****************************************************************** }

uses
{$IFDEF USE_DLL}
  dmath;
{$ELSE}
  utypes, utrapint;
{$ENDIF}    

function F(X : Float) : Float;
begin
  F := X * Exp(-X);
end;

function G(X : Float) : Float;
begin
  G := Exp(- 2.0 * X);
end;

function H(X : Float) : Float;
var
  E : Float;
begin
  E := Exp(-X);
  H := ((X - 1.0) + E) * E;
end;

const
  N      = 250;
  Xmax   = 10.0;
  DeltaX = Xmax / N;

var
  X, Y : TVector;
  I    : Integer;

begin
  DimVector(X, N);
  DimVector(Y, N);

  X[0] := 0.0;
  Y[0] := 0.0;

  for I := 1 to N do
    X[I] := X[I - 1] + DeltaX;

  ConvTrap(@F, @G, X, Y, N);

  Writeln('     X        ConvTrap   Exact');
  Writeln('------------------------------');

  for I := 0 to N do
    Writeln(X[I]:10:4, Y[I]:10:4, H(X[I]):10:4);
  readln;
end.

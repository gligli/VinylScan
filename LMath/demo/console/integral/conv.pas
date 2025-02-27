{ ******************************************************************
  This program computes the convolution product H of two functions
  F and G by the Gauss-Legendre method. The result is compared with
  the analytical solution.

  The example functions are:

  F(x) = x * exp(-x)
  G(x) = exp(-2 * x)

  The analytical solution is:

  H(x) = (F * G)(x) = (x - 1) * exp(-x) + exp(-2 * x)
  ****************************************************************** }

program conv;

uses
{$IFDEF USE_DLL}
  dmath;
{$ELSE}
  utypes, ugausleg;
{$ENDIF}    

function F(X : Float) : Float;
begin
  F := X * Exp(-X);
end;

function G(X : Float) : Float;
begin
  G := Exp(- 2 * X);
end;

function H(X : Float) : Float;
var
  E : Float;
begin
  E := Exp(-X);
  H := ((X - 1) + E) * E;
end;

const
  N = 10;

var
  X, Y : TVector;
  I    : Integer;

begin
  DimVector(X, N);
  DimVector(Y, N);
  
  X[0] := 0.0;
  Y[0] := 0.0;

  for I := 1 to N do
    begin
      X[I] := 0.1 * I;
      Y[I] := Convol(@F, @G, X[I]);
    end;

  WriteLn('     X        Convol     Exact');
  WriteLn('------------------------------');

  for I := 0 to N do
    WriteLn(X[I]:10:4, Y[I]:10:4, H(X[I]):10:4);
  readln;
end.

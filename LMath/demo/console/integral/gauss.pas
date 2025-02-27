{ ******************************************************************
  This program computes an integral by the Gauss-Legendre method.
  The result is compared with the analytical solution.

  The example function is:

  F(x) = x * exp(-x)

  The analytical solution is:

         (x
  G(x) = |  F(t) dt = 1 - (x + 1) * exp(-x)
         )0
  ****************************************************************** }

program gauss;

uses
{$IFDEF USE_DLL}
  dmath;
{$ELSE}
  utypes, ugausleg;
{$ENDIF}    

function F(X : Float) : Float;
{ Function to integrate }
begin
  F := X * Exp(-X);
end;

function G(X : Float) : Float;
{ Integral }
begin
  G := 1 - (X + 1) * Exp(-X);
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
      X[I] := I;
      Y[I] := GausLeg0(@F, X[I]);  { or GausLeg(F, 0, X[I]) }
    end;

  WriteLn('     X       GausLeg     Exact');
  WriteLn('------------------------------');

  for I := 0 to N do
    WriteLn(X[I]:10:4, Y[I]:10:4, G(X[I]):10:4);
  readln;
end.

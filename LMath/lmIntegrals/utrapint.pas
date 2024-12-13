{ ******************************************************************
  Trapezoidal integration
  ****************************************************************** }

unit utrapint;

interface

uses
  utypes;

{ Integration by trapezoidal rule, from (X[0], Y[0]) to (X[N], Y[N]) }
function TrapInt(X, Y : TVector; N : Integer) : Float;


{  Computes the convolution product of 2 functions Func1 and Func2
  by the trapezoidal rule over an array T[0..N] of equally spaced
  abscissas, with T[0] = 0. The result is returned in Y[0..N]}
procedure ConvTrap(Func1, Func2 : TFunc; T, Y : TVector; N : Integer);

implementation

function TrapInt(X, Y : TVector; N : Integer) : Float;
  var
    Sum  : Float;
    I, J : Integer;
  begin
    Sum := 0.0;
    for I := 0 to Pred(N) do
      begin
        J := Succ(I);
        Sum := Sum + 0.5 * (X[J] - X[I]) * (Y[J] + Y[I]);
      end;
    TrapInt := Sum;
  end;

procedure ConvTrap(Func1, Func2 : TFunc; T, Y : TVector; N : Integer);
var
  F2       : TVector;
  F10, Sum : Float;
  I, J     : Integer;
begin
  DimVector(F2, N);
  F10 := Func1(0.0);

  for I := 0 to N do
    F2[I] := Func2(T[I]);

  for I := 1 to N do
    begin
      Sum := 0.5 * (Func1(T[I]) * F2[0] + F10 * F2[I]);
      for J := 1 to I - 1 do
        Sum := Sum + Func1(T[I] - T[J]) * F2[J];
      Y[I] := Sum * T[1];
    end;
end;

end.

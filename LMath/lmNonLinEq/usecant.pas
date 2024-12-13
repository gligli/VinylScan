{ ******************************************************************
  Secant method for nonlinear equation
  ****************************************************************** }

unit usecant;

interface

uses
  utypes, uErrors;

// function Func(X : Float) : Float;
//the maximum number of iterations MaxIter
//Initial values X; Y
//the maximum number of iterations MaxIter
//the tolerance Tol with which the root must be located.
procedure Secant (Func     : TFunc;
                  var X, Y : Float;
                  MaxIter  : Integer;
                  Tol      : Float;
                  out F    : Float);

implementation

procedure Secant (Func     : TFunc;
                  var X, Y : Float;
                  MaxIter  : Integer;
                  Tol      : Float;
                  out F    : Float);

var
  Iter : Integer;
  G, Z : Float;

begin
  Iter := 0;
  SetErrCode(OptOk);

  repeat
    F := Func(X);

    if MaxIter < 1 then Exit;

    G := Func(Y);

    Iter := Iter + 1;

    if (F = G) or (Iter > MaxIter) then
      begin
        SetErrCode(OptNonConv);
        Exit;
      end;

    Z := (X * G - Y * F) / (G - F);

    X := Y;
    Y := Z;
  until Abs(X - Y) < Tol * (Abs(X) + Abs(Y));

  X := 0.5 * (X + Y);
  F := Func(X);
end;

end.

{ ******************************************************************
  Logarithms, exponentials and power
  ****************************************************************** }

unit umath;

interface

uses
  utypes, uErrors, uminmax;

{ Exponential }
function Expo(X : Float) : Float;

{ 2^X }
function Exp2(X : Float) : Float;

{ 10^X }
function Exp10(X : Float) : Float;

{ Natural log }
function Log(X : Float) : Float;

{ Log, base 2 }
function Log2(X : Float) : Float;

{ Decimal log }
function Log10(X : Float) : Float;

{ Log, base A }
function LogA(X, A : Float) : Float;

{ X^N }
function IntPower(X : Float; N : Integer) : Float;

 { X^Y, X >= 0 }
function Power(X, Y : Float) : Float;

operator ** (X, Y : Float) R : Float;
operator ** (X:Float; N:Integer) R : Float;

implementation

  function Expo(X : Float) : Float;
  begin
    SetErrCode(FOk);
    if X < MinLog then
      Expo := DefaultVal(FUnderflow, 0.0)
    else if X > MaxLog then
      Expo := DefaultVal(FOverflow, MaxNum)
    else
      Expo := Exp(X);
  end;

  function Exp2(X : Float) : Float;
  var
    XLn2 : Float;
  begin
    SetErrCode(FOk);
    XLn2 := X * Ln2;
    if XLn2 < MinLog then
      Exp2 := DefaultVal(FUnderflow, 0.0)
    else if XLn2 > MaxLog then
      Exp2 := DefaultVal(FOverflow, MaxNum)
    else
      Exp2 := Exp(XLn2);
  end;

  function Exp10(X : Float) : Float;
  var
    XLn10 : Float;
  begin
    SetErrCode(FOk);
    XLn10 := X * Ln10;
    if XLn10 < MinLog then
      Exp10 := DefaultVal(FUnderflow, 0.0)
    else if XLn10 > MaxLog then
      Exp10 := DefaultVal(FOverflow, MaxNum)
    else
      Exp10 := Exp(XLn10);
  end;

  function Log(X : Float) : Float;
  begin
    SetErrCode(FOk);
    if X < 0.0 then
      Log := DefaultVal(FDomain, 0)
    else if IsZero(X,MinNum*100) then
      Log := DefaultVal(FSing, 0)
    else
      Log := Ln(X);
  end;

  function Log10(X : Float) : Float;
  begin
    SetErrCode(FOk);
    if X < 0.0 then
      Result := DefaultVal(FDomain, 0)
    else if IsZero(X,MinNum*100) then
      Result := DefaultVal(FSing, 0)
    else
      Result := Ln(X) * InvLn10;
  end;

  function Log2(X : Float) : Float;
  begin
    SetErrCode(FOk);
    if X < 0.0 then
      Log2 := DefaultVal(FDomain, 0)
    else if IsZero(X,MinNum*100) then
      Log2 := DefaultVal(FSing, 0)
    else
      Log2 := Ln(X) * InvLn2;
  end;

  function LogA(X, A : Float) : Float;
  var
    Y : Float;
  begin
    Y := Log(X);
    if MathErr = FOk then
      if SameValue(A,1.0) then
        Y := DefaultVal(FSing, 0)
      else
        Y := Y / Log(A);
    LogA := Y;
  end;

{ ----------------------------------------------------------------------
  Power functions.

  Thanks to Volker Walter <vw@metrohm.ch>
  for suggesting improvements to Power and IntPower
  ---------------------------------------------------------------------- }

  function PowerTests(X, Y : Float; out Res : Float) : Boolean;
  { Tests the cases X=0, Y=0 and Y=1. Returns X^Y in Res }
  begin
    if IsZero(X) then
      begin
        PowerTests := True;
        if IsZero(Y) then       { 0^0 = lim  X^X = 1 }
          Res := 1.0          {       X->0         }
        else if Y > 0.0 then
          Res := 0.0          { 0^Y = 0 }
        else
          Res := DefaultVal(FSing, MaxNum);
      end
    else if IsZero(Y) then
      begin
        Res := 1.0;           { X^0 = 1 }
        PowerTests := True;
      end
    else if SameValue(Y,1.0) then
      begin
        Res := X;             { X^1 = X }
        PowerTests := True;
      end
    else
      PowerTests := False;
  end;

  function IntPower(X : Float; N : Integer) : Float;
  { Computes X^N by repeated multiplications }
  const
    InverseMaxNum = 1.0 / MaxNum;
  var
    T      : Float;
    M      : Integer;
    Invert : Boolean;
  begin
    if PowerTests(X, N, T) then
      begin
        IntPower := T;
        Exit;
      end;

    Invert := (N < 0);    { Test if inverting is needed }
    if 1.0 < Abs(X) then  { Test for 0 ..|x| .. 1 }
      begin
        X := 1.0 / X;
        Invert := not Invert;
      end;

    { Legendre's algorithm for
      minimizing the number of multiplications }
    T := 1.0; M := Abs(N);
    while 0 < M do
      begin
        if Odd(M) then T := T * X;
        X := Sqr(X);
        M := M div 2;
      end;

    if Invert then
      if Abs(T) < InverseMaxNum then  { Only here overflow }
        T := DefaultVal(FOverflow, Sgn(T) * MaxNum)
      else
        T := 1.0 / T;

    IntPower := T;
  end;

  function Power(X, Y : Float) : Float;
  { Computes X^Y = Exp(Y * Ln(X)), for X > 0
    Resorts to IntPower if Y is integer }
  var
    Res  : Float;
    YLnX : Float;
  begin
    if PowerTests(X, Y, Res) then
      Power := Res
    else if (Abs(Y) < MaxInt) and SameValue(Trunc(Y),Y) then  { Integer exponent }
      Power := IntPower(X, Trunc(Y))
    else if X <= 0.0 then
      Power := DefaultVal(FDomain, 0.0)
    else
      begin
        YLnX := Y * Ln(X);
        if YLnX < MinLog then
          Power := DefaultVal(FUnderflow, 0.0)
        else if YLnX > MaxLog then
          Power := DefaultVal(FOverflow, MaxNum)
        else
          Power := Exp(YLnX);
      end;
  end;

  operator ** (X, Y: Float): Float;
  begin
    R := Power(X,Y);
  end;

  operator ** (X: Float; N: Integer): Float;
  begin
    R := IntPower(X,N);
  end;

end.

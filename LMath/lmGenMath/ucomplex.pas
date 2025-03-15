{ ******************************************************************
  Complex number library
  ******************************************************************
  Based on ComplexMath Delphi library by E. F. Glynn
  http://www.efg2.com/Lab/Mathematics/Complex/index.html
  Later ideas from uComplex unit by Pierre Müller were used
  ****************************************************************** }

unit ucomplex;

interface
{$INLINE ON}
uses
  utypes, uErrors, umath, uminmax, utrigo, uhyper;

// operators
operator := (r : Float) z : complex; inline;

operator + (z1, z2 : complex) z : complex; inline;
operator + (z1 : complex; r : Float) z : complex; inline;
operator + (r : Float; z1 : complex) z : complex; inline;

operator - (z1, z2 : complex) z : complex; inline;
operator - (z1 : complex;r : Float) z : complex; inline;
operator - (r : Float; z1 : complex) z : complex; inline;

operator * (z1, z2 : complex) z : complex; inline;
operator * (z1 : complex; r : Float) z : complex; inline;
operator * (r : Float; z1 : complex) z : complex; inline;

operator / (znum, zden : complex) z : complex; inline;
operator / (znum : complex; r : Float) z : complex; inline;
operator / (r : Float; zden : complex) z : complex; inline;

operator = (z1, z2 : complex) b : boolean; inline;
operator = (z1 : complex;r : Float) b : boolean; inline;
operator = (r : Float; z1 : complex) b : boolean; inline;

operator - (z1 : complex) z : complex; inline;

{ ------------------------------------------------------------------
  General functions
  ------------------------------------------------------------------ }

{ Returns the complex number X + iY }
function Cmplx(X, Y : Float) : Complex;

{ Returns the complex number R * [cos(Theta) + i * sin(Theta)] }
function Polar(R, Theta : Float) : Complex;

{ Returns the real part of Z }
function CReal(Z : Complex) : Float;

{ Returns the imaginary part of Z }
function CImag(Z : Complex) : Float;

{ Complex sign }
function CSgn(Z : Complex) : Integer;

{ Exchanges two complex numbers }
procedure Swap(var X, Y : Complex); overload;

{compares two compex numbers using relative epsilon}
function samevalue(z1, z2 : complex) : boolean; overload;

{Modulus of Z }
function CAbs(Z : Complex) : Float;

{ Squared modulus of Z }
function CAbs2(Z : Complex) : Float;

{ Argument of Z }
function CArg(Z : Complex) : Float;

{ Complex conjugate }
function CConj(Z : Complex) : Complex;

{ Complex square }
function CSqr(Z : Complex) : Complex;

{Complex inverse }
function CInv(Z : Complex) : Complex;

{Principal part of complex square root }
function CSqrt(Z : Complex) : Complex;

{converts complex number from rectangular to polar form}
function CToPolar(Z : Complex):complex;

{converts complex number from polar to rectangular form}
function CToRect(Z : Complex):complex;

{ ------------------------------------------------------------------
  Logarithm, Exponential, Powers
  ------------------------------------------------------------------ }

  {Principal part of complex logarithm }
function CLn(Z : Complex) : Complex;

{Complex exponential }
function CExp(Z : Complex) : Complex;

{ All N-th roots: Z^(1/N), K=0..N-1 }
function CRoot(Z : Complex; K, N : Integer) : Complex;

{Power with complex exponent }
function CPower(A, B : Complex) : Complex;

{Power with integer exponent }
function CIntPower(A : Complex; N : Integer) : Complex;

{Power with Float exponent }
function CRealPower(A : Complex; X : Float) : Complex;

{ ------------------------------------------------------------------
  Polynomial 
  ------------------------------------------------------------------ }

  //evaluate polynom with compex argument
  function CPoly(Z : Complex; Coef : TVector; Deg : Integer) : Complex;

{ ------------------------------------------------------------------
  Trigonometric functions
  ------------------------------------------------------------------ }

  {Complex sine }
function CSin(Z : Complex) : Complex;

{ Complex cosine }
function CCos(Z : Complex) : Complex;

{ Complex sine and cosine }
procedure CSinCos(Z : Complex; out SinZ, CosZ : Complex);

{ Complex tangent }
function CTan(Z : Complex) : Complex;

{ Complex arc sine }
function CArcSin(Z : Complex) : Complex;

{ Complex arc cosine }
function CArcCos(Z : Complex) : Complex;

{ Complex arc tangent }
function CArcTan(Z : Complex) : Complex;

{ ------------------------------------------------------------------
  Hyperbolic functions
  ------------------------------------------------------------------ }

  {  Complex hyperbolic sine }
function CSinh(Z : Complex) : Complex;

{  Complex hyperbolic cosine }
function CCosh(Z : Complex) : Complex;

{  Complex hyperbolic sine and cosine }
procedure CSinhCosh(Z : Complex; out SinhZ, CoshZ : Complex);

{  Complex hyperbolic tangent }
function CTanh(Z : Complex) : Complex;

{  Complex hyperbolic arc sine }
function CArcSinh(Z : Complex) : Complex;

{  Complex hyperbolic arc cosine }
function CArcCosh(Z : Complex) : Complex;

{ Complex hyperbolic arc tangent }
function CArcTanh(Z : Complex) : Complex;

{ ------------------------------------------------------------------
  Gamma function
  ------------------------------------------------------------------ }

  {Logarithm of Gamma function }
function CLnGamma(Z : Complex) : Complex;

{ ------------------------------------------------------------------ }

implementation

operator := (r : Float) z : complex; inline;
begin
  z.X:=r;
  z.Y:=0.0;
end;

{ four base operations  +, -, * , / }

operator + (z1, z2 : complex) z : complex; inline;
  { addition : z := z1 + z2 }
begin
  z.X := z1.X + z2.X;
  z.Y := z1.Y + z2.Y;
end;

operator + (z1 : complex; r : Float) z : complex; inline;
{ addition : z := z1 + r }
begin
   z.X := z1.X + r;
   z.Y := z1.Y;
end;

operator + (r : Float; z1 : complex) z : complex; inline;
{ addition : z := r + z1 }
begin
   z.X := z1.X + r;
   z.Y := z1.Y;
end;

operator - (z1, z2 : complex) z : complex; inline;
{ substraction : z := z1 - z2 }
begin
   z.X := z1.X - z2.X;
   z.Y := z1.Y - z2.Y;
end;

operator - (z1 : complex; r : Float) z : complex;  inline;
{ substraction : z := z1 - r }
begin
   z.X := z1.X - r;
   z.Y := z1.Y;
end;

operator - (z1 : complex) z : complex;  inline;
{ substraction : z := - z1 }
begin
   z.X := -z1.X;
   z.Y := -z1.Y;
end;

operator - (r : Float; z1 : complex) z : complex;  inline;
{ substraction : z := r - z1 }
begin
   z.X := r - z1.X;
   z.Y := - z1.Y;
end;

operator * (z1, z2 : complex) z : complex;  inline;
{ multiplication : z := z1 * z2 }
begin
   z.X := (z1.X * z2.X) - (z1.Y * z2.Y);
   z.Y := (z1.X * z2.Y) + (z1.Y * z2.X);
end;

operator * (z1 : complex; r : Float) z : complex;  inline;
{ multiplication : z := z1 * r }
begin
   z.X := z1.X * r;
   z.Y := z1.Y * r;
end;

operator * (r : Float; z1 : complex) z : complex;  inline;
{ multiplication : z := r * z1 }
begin
   z.X := z1.X * r;
   z.Y := z1.Y * r;
end;

operator / (znum, zden : complex) z : complex;  inline;
  { division : z := znum / zden }
  { The following algorithm is used to properly handle
    denominator overflow:

               |  a + b(d/c)   c - a(d/c)
               |  ---------- + ---------- I     if |d| < |c|
    a + b I    |  c + d(d/c)   a + d(d/c)
    -------  = |
    c + d I    |  b + a(c/d)   -a+ b(c/d)
               |  ---------- + ---------- I     if |d| >= |c|
               |  d + c(c/d)   d + c(c/d)
  }
 var
   tmp, denom : Float;
 begin
   if ( abs(zden.X) > abs(zden.Y) ) then
   begin
      tmp := zden.Y / zden.X;
      denom := zden.X + zden.Y * tmp;
      z.X := (znum.X + znum.Y * tmp) / denom;
      z.Y := (znum.Y - znum.X * tmp) / denom;
   end
   else
   begin
      tmp := zden.X / zden.Y;
      denom := zden.Y + zden.X * tmp;
      z.X := (znum.Y + znum.X * tmp) / denom;
      z.Y := (-znum.X + znum.Y * tmp) / denom;
   end;
 end;

operator / (znum : complex; r : Float) z : complex;
  { division : z := znum / r }
begin
   z.X := znum.X / r;
   z.Y := znum.Y / r;
end;

operator / (r : Float; zden : complex) z : complex;
  { division : z := r / zden }
var denom : Float;
begin
   with zden do denom := (X * X) + (Y * Y);
   { generates a fpu exception if denom=0 as for Floats }
   z.X := (r * zden.X) / denom;
   z.Y := - (r * zden.Y) / denom;
end;

function cmod (z : complex): Float;
  { module : r = |z| }
begin
   with z do
     cmod := sqrt((X * X) + (Y * Y));
end;

operator = (z1, z2 : complex) b : boolean;
  { returns TRUE if z1 = z2 }
begin
   b := (z1.X = z2.X) and (z1.Y = z2.Y);
end;

operator = (z1 : complex; r :Float) b : boolean;
  { returns TRUE if z1 = r }
begin
   b := (z1.X = r) and (z1.Y = 0);
end;

operator = (r : Float; z1 : complex) b : boolean;
  { returns TRUE if z1 = r }
begin
  b := (z1.X = r) and (z1.Y = 0);
end;


function Cmplx(X, Y : Float) : Complex;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Polar(R, Theta : Float) : Complex;
begin
  Result.X := R * Cos(Theta);
  Result.Y := R * Sin(Theta);
end;

function CReal(Z : Complex) : Float;
begin
  Result := Z.X;
end;

function CImag(Z : Complex) : Float;
begin
  Result := Z.Y;
end;
  
function CSgn(Z : Complex) : Integer;
begin
  if Z.X > 0.0 then
    Result := 1
  else if Z.X < 0.0 then
    Result := - 1
  else
    begin
      if Z.Y > 0.0 then
        Result := 1
      else if Z.Y < 0.0 then
        Result := - 1
      else
        Result := 0;
    end;
end;

function samevalue(z1, z2: complex): boolean;
begin
  Result := SameValue(z1.X, z2.X) and SameValue(z1.Y, z2.Y);
end;

procedure Swap(var X, Y : Complex);
var
  Temp : Complex;
begin
  Temp := X;
  X := Y;
  Y := Temp;
end;

function CAbs(Z : Complex) : Float;
{ Computes the modulus of Z without destructive underflow or overflow 
  cf. Numerical Recipes }
var
  AbsX, AbsY : Float;
begin
  AbsX := Abs(Z.X);
  AbsY := Abs(Z.Y);

  if AbsX > AbsY then
    Result := AbsX * Sqrt(1.0 + Sqr(AbsY / AbsX))
  else if IsZero(AbsY) then
    Result := 0.0
  else
    Result := AbsY * Sqrt(1.0 + Sqr(AbsX / AbsY));
end;

function CAbs2(Z : Complex) : Float;
begin
  Result := Sqr(Z.X) + Sqr(Z.Y);
end;

function CArg(Z : Complex) : Float;
var
  Theta : Float;
begin
  if IsZero(Z.X) then
    if IsZero(Z.Y) then
      Result := 0.0
    else if Z.Y > 0.0 then
      Result := PiDiv2
    else
      Result := - PiDiv2
  else
    begin
      { 4th/1st quadrant -Pi/2..Pi/2 }
      Theta := ArcTan(Z.Y / Z.X);
      { 2nd/3rd quadrants }
      if Z.X < 0.0 then
        if Z.Y >= 0.0 then
          Theta := Theta + Pi   { 2nd quadrant:  Pi/2..Pi }
        else
          Theta := Theta - Pi;  { 3rd quadrant: -Pi..-Pi/2 }
      Result := Theta;
    end;
end;

function CConj(Z : Complex) : Complex;
begin
  Result.X := Z.X;
  Result.Y := - Z.Y;
end;

function CSqr(Z : Complex) : Complex;
begin
  Result.X := Sqr(Z.X) - Sqr(Z.Y);
  Result.Y := 2 * Z.X * Z.Y;
end;

function CInv(Z : Complex) : Complex;
var
  Temp : Float;
begin
  if (Z.X = 0.0) and (Z.Y = 0.0) then
    begin
      SetErrCode(FOverflow);
      Result := C_infinity;
      Exit;
    end;

  SetErrCode(FOk);
  
  Temp := Sqr(Z.X) + Sqr(Z.Y);
  
  Result.X := Z.X / Temp;
  Result.Y := - Z.Y / Temp;
end;

function CSqrt(Z : Complex) : Complex;
{ Complex square root of Z without destructive underflow or overflow 
  cf. Numerical Recipes }
var
   X, Y, U, V, W, R : Float;
begin
  if (Z.X = 0.0) AND (Z.Y = 0.0) then 
    begin
      Result := C_zero;
      Exit;
    end;  

  X := Abs(Z.X);
  Y := Abs(Z.Y);
  
  if X >= Y then
    W := Sqrt(X) * Sqrt(0.5 * (1.0 + Sqrt(1.0 + Sqr(Y / X))))
  else 
    begin
      R := X / Y;
      W := Sqrt(Y) * Sqrt(0.5 * (R + Sqrt(1.0 + Sqr(R))))
    end;
    
  if Z.X >= 0.0 then
    begin
      U := W;
      V := Z.Y / (2.0 * U)
    end
  else
    begin
      if Z.Y >= 0.0 then V := W else V := - W;
      U := Z.Y / (2.0 * V)
    end;
  
  Result.X := U;
  Result.Y := V;
end;

function CToPolar(Z: Complex): complex;
var
   Buf:complex;
begin
  Buf.X := Sqrt(Z.X*Z.X+Z.Y*Z.Y);
  if not IsZero(Z.X) then
    Buf.Y := arctan(Z.Y/Z.X)
  else begin
    if Z.Y > 0 then
      Result.Y := Pi/2
    else
      Result.Y := -Pi/2;
  end;
  if Z.X < 0  then
  begin
    if Z.Y < 0 then
      Buf.Y := Buf.Y - Pi
    else
      Buf.Y := Buf.Y + Pi;
  end;
  Result := Buf;
end;

function CToRect(Z: Complex): complex;
var
  C:Complex;
begin
  C.X := Z.X*cos(Z.Y);
  C.Y := Z.X*sin(Z.Y);
  Result := C;
end;

function CLn(Z : Complex) : Complex;
var
  LnR : Float;
begin
  LnR := Log(CAbs(Z));
  if MathErr <> FOk then
  begin
    Result.X := - MaxNum;
    Result.Y := 0.0;
    Exit;
  end;
  Result.X := LnR;
  Result.Y := CArg(Z);
end;

function CExp(Z : Complex) : Complex;
var
  ExpX : Float;
begin
  ExpX := Expo(Z.X);
  
  if MathErr <> FOk then
    begin
      Result.X := ExpX;
      Result.Y := 0.0;
      Exit;
    end;  
  
  Result.X := ExpX * Cos(Z.Y);
  Result.Y := ExpX * Sin(Z.Y);
end;

function CRoot(Z : Complex; K, N : Integer) : Complex;
begin
  if (N <= 0) or (K < 0) or (K >= N) then
    begin
      SetErrCode(FDomain);
      Result := C_zero;
      Exit;
    end;

  SetErrCode(FOk);
  
  if (IsZero(Z.X) and IsZero(Z.Y)) then
    begin
      Result := C_zero;
      Exit;
    end;

  Result := Polar(Power(CAbs(Z), 1.0 / N), (CArg(Z) + K * TwoPi) / N);
end;

function CPower(A, B : Complex) : Complex;
begin
  if (IsZero(A.X) and IsZero(A.Y)) then
    begin
      if (IsZero(B.X) and IsZero(B.Y)) then
        Result := C_one                    { lim a^a = 1 as a -> 0 }
      else
        Result := C_zero;                  { 0^b = 0, b > 0 }
      Exit;
    end;                       

  Result := CExp(B * CLn(A));
end;

function CIntPower(A : Complex; N : Integer) : Complex;
{ Computes A^N by repeated multiplications }
var
  M      : Integer;
  B, Res : Complex;
begin  
  if N < 0 then
    begin
      M := - N;
      B := CInv(A);
      if MathErr = FOverflow then 
        begin
          Result := C_infinity;
          Exit;
        end;  
    end  
  else
    begin
      M := N;
      B := A;
    end;
  
  SetErrCode(FOk);
  
  { Legendre's algorithm for minimizing the number of multiplications }
  
  Res := C_one;
  while M > 0 do
    begin
      if Odd(M) then Res := Res * B;
      B := CSqr(B);
      M := M shr 1;
    end;
  
  Result := Res;
end;

function CRealPower(A : Complex; X : Float) : Complex;
begin
  if IsZero(Frac(X)) then
    begin
      Result := CIntPower(A, Trunc(X));
      Exit;
    end;
  
  SetErrCode(FOk);
  
  if IsZero(A.X) and IsZero(A.Y) then
    begin
      if IsZero(X) then
        Result := C_one
      else if X > 0.0 then
        Result := C_zero
      else
        begin
          SetErrCode(FSing);
          Result := C_infinity;
        end;
      Exit;
    end;

  Result := Polar(Power(CAbs(A), X), CArg(A) * X);
end;

function CPoly(Z : Complex; Coef : TVector; Deg : Integer) : Complex; 
var
  I : Integer;
begin  
  Result.X := Coef[Deg];
  Result.Y := 0.0;
  for I := Pred(Deg) downto 0 do
    begin
      Result := Result * Z;
      Result.X := Result.X + Coef[I];
    end;  
end;

function CSin(Z : Complex) : Complex; 
var
  SinhY, CoshY : Float;
begin
  SinhCosh(Z.Y, SinhY, CoshY);   
  Result.X := Sin(Z.X) * CoshY;
  Result.Y := Cos(Z.X) * SinhY;
end;

function CCos(Z : Complex) : Complex; 
var
  SinhY, CoshY : Float;
begin
  SinhCosh(Z.Y, SinhY, CoshY);   
  Result.X := Cos(Z.X) * CoshY;
  Result.Y := - Sin(Z.X) * SinhY;
end;

procedure CSinCos(Z : Complex; out SinZ, CosZ : Complex);
var
  SinX, CosX, SinhY, CoshY : Float;
begin
  SinX := Sin(Z.X);
  CosX := Cos(Z.X);
  
  SinhCosh(Z.Y, SinhY, CoshY);
  
  SinZ.X := SinX * CoshY;
  SinZ.Y := CosX * SinhY;

  CosZ.X := CosX * CoshY;
  CosZ.Y := - SinX * SinhY;
end;

function CTan(Z : Complex) : Complex; 
var
  X2, Y2, SinX2, CosX2, SinhY2, CoshY2, Temp : Float;
begin
  X2 := 2.0 * Z.X;
  Y2 := 2.0 * Z.Y;

  SinX2 := Sin(X2);
  CosX2 := Cos(X2);
  
  SinhCosh(Y2, SinhY2, CoshY2);
  
  if MathErr = FOk then
    Temp := CosX2 + CoshY2
  else
    Temp := CoshY2;
 
  if IsZero(Temp) then  { Z = Pi/2 + k*Pi }
    begin
      SetErrCode(FSing);
      Result := C_infinity;
      Exit
    end;
    
  Result.X := SinX2 / Temp;
  Result.Y := SinhY2 / Temp;
end;

function CArcSin(Z : Complex) : Complex;
var
  Rp, Rm, S, T, X2, XX, YY : Float;
  B : Complex;
begin
  B.X := Z.Y;
  B.Y := - Z.X;
  
  X2 := 2.0 * Z.X;
  XX := Sqr(Z.X);
  YY := Sqr(Z.Y);
  S := XX + YY + 1.0;
  Rp := 0.5 * Sqrt(S + X2);
  Rm := 0.5 * Sqrt(S - X2);
  T := Rp + Rm;
  
  Result.X := ArcSin(Rp - Rm);
  Result.Y := CSgn(B) * Log(T + Sqrt(Sqr(T) - 1.0));
end;

function CArcCos(Z : Complex) : Complex;
{ ArcCos(Z) = Pi/2 - ArcSin(Z) }
begin
  Z := CArcSin(Z);
  Result.X := PiDiv2 - Z.X;
  Result.Y := - Z.Y;
end;

function CArcTan(Z : Complex) : Complex;
var
  XX, Yp1, Ym1 : Float;
begin
  if IsZero(Z.X) and Samevalue(Abs(Z.Y),1.0) then  { Z = +/- i }
    begin
      SetErrCode(FSing);
      Result.X := 0.0;
      Result.Y := Sgn(Z.Y) * MaxNum;
      Exit;
    end;
  
  SetErrCode(FOk);
    
  XX := Sqr(Z.X);
  Yp1 := Z.Y + 1.0;
  Ym1 := Z.Y - 1.0;
    
  Result.X := 0.5 * (ArcTan2(Z.X, - Ym1) - ArcTan2(- Z.X, Yp1));
  Result.Y := 0.25 * Log((XX + Sqr(Yp1)) / (XX + Sqr(Ym1)));
end;

function CSinh(Z : Complex) : Complex; 
var
  SinhX, CoshX : Float;
begin
  SinhCosh(Z.X, SinhX, CoshX); 
  Result.X := SinhX * Cos(Z.Y);
  Result.Y := CoshX * Sin(Z.Y);
end;

function CCosh(Z : Complex) : Complex;  
var
  SinhX, CoshX : Float;
begin
  SinhCosh(Z.X, SinhX, CoshX); 
  Result.X := CoshX * Cos(Z.Y);
  Result.Y := SinhX * Sin(Z.Y);
end;

procedure CSinhCosh(Z : Complex; out SinhZ, CoshZ : Complex);
var
  SinhX, CoshX, SinY, CosY : Float;
begin  
  SinY := Sin(Z.Y);
  CosY := Cos(Z.Y);
  
  SinhCosh(Z.X, SinhX, CoshX); 
  
  SinhZ.X := SinhX * CosY;
  SinhZ.Y := CoshX * SinY;
  
  CoshZ.X := CoshX * CosY;
  CoshZ.Y := SinhX * SinY;
end;

function CTanh(Z : Complex) : Complex;  
var
  X2, Y2, SinY2, CosY2, SinhX2, CoshX2, Temp : Float;
begin
  X2 := 2.0 * Z.X;
  Y2 := 2.0 * Z.Y;
  
  SinY2 := Sin(Y2);
  CosY2 := Cos(Y2);
  
  SinhCosh(X2, SinhX2, CoshX2);
  
  if MathErr = FOk then
    Temp := CoshX2 + CosY2
  else
    Temp := CoshX2;
    
  if Temp = 0.0 then  { Z = i * (Pi/2 + k*Pi) }
    begin                  
      SetErrCode(FSing);
      Result.X := 0.0;
      Result.Y := MaxNum;
      Exit;
    end;

  Result.X := SinhX2 / Temp;
  Result.Y := SinY2 / Temp;
end;

function CArcSinh(Z : Complex) : Complex;
{ ArcSinh(Z) = -i*ArcSin(i*Z) }
var
  iZ : Complex;
begin
  iZ.X := - Z.Y;
  iZ.Y := Z.X;
  
  Z := CArcSin(iZ);
  
  Result.X := Z.Y;
  Result.Y := - Z.X;
end;

function CArcCosh(Z : Complex) : Complex;
{ ArcCosh(Z) = CSgn(Y + i(1-X)) * i * ArcCos(Z) where Z = X+iY }
var
  A : Complex;
  S : Float;
begin
  A.X := Z.Y;
  A.Y := 1.0 - Z.X;
  
  S := CSgn(A);
  A := CArcCos(Z);
  
  Result.X := - S * A.Y;
  Result.Y := S * A.X;
end;

function CArcTanh(Z : Complex) : Complex;
{ ArcTanh(Z) = -i*ArcTan(i*Z) }
 var
   iZ : Complex;
 begin
   if (Abs(Z.X) = 1.0) and (Z.Y = 0.0) then  { A = +/- 1 }
     begin
       SetErrCode(FSing);
       Result.X := Sgn(Z.X) * MaxNum;
       Result.Y := 0.0;
       Exit;
     end;

  iZ.X := - Z.Y;
  iZ.Y := Z.X;
  
  Z := CArcTan(iZ);
  
  Result.X := Z.Y;
  Result.Y := - Z.X;
end;

function CApproxLnGamma(Z : Complex) : Complex;
{ This is the approximation used in the National Bureau of
  Standards "Table of the Gamma Function for Complex Arguments,"
  Applied Mathematics Series 34, 1954. The NBS table was created
  using this approximation over the area 9 < Re(z) < 10 and
  0 < Im(z) < 10. Other table values were computed using the
  relationship:
      _                   _
  ln | (z+1) = ln z + ln | (z) }

const
  C : array[1..8] of Float =
  (8.33333333333333E-02, - 2.77777777777778E-03,
   7.93650793650794E-04, - 5.95238095238095E-04,
   8.41750841750842E-04, - 1.91752691752692E-03,
   6.41025641025641E-03, - 2.95506535947712E-02);

var
  I         : Integer;
  Powers    : array[1..8] of Complex;
  Temp, Sum : Complex;

begin
  Powers[1] := CInv(Z);
  Temp := CSqr(Powers[1]);
  
  for I := 2 to 8 do
    Powers[I] := Powers[I - 1] * Temp;

  Temp.X := Z.X - 0.5;         { Z - 0.5 }
  Temp.Y := Z.Y;
    
  Sum := Temp * CLn(Z);   { (Z - 0.5)*Ln(Z) }
  Sum := Sum - Z;         { (Z - 0.5)*Ln(Z) - Z }
    
  Sum.X := Sum.X + Ln2PiDiv2;
      
  for I := 8 downto 1 do
    begin
      Sum.X := Sum.X + C[I] * Powers[I].X;
      Sum.Y := Sum.Y + C[I] * Powers[I].Y;
    end;
      
  Result := Sum;      
end;

function CLnGamma(Z : Complex) : Complex;
var
  LnZ : Complex;
begin
  if (Z.X <= 0.0) and IsZero(Z.Y) then
    if IsZero(Int(Z.X - 1E-8) - Z.X) then  { Negative integer? }
      begin
        SetErrCode(FSing);
        Result := C_infinity;
        Exit
      end;
  
  SetErrCode(FOk);
        
  if Z.Y < 0.0 then                        { 3rd or 4th quadrant? }
    Result := CConj(CLnGamma(CConj(Z)))    { Try again in 1st or 2nd quadrant }
  else
    begin
      if Z.X < 9.0 then                    { "left" of NBS table range }
        begin
          LnZ := CLn(Z);
          Z.X := Z.X + 1.0;
          Result := CLnGamma(Z) - LnZ;
        end
      else
        Result := CApproxLnGamma(Z);       { NBS table range: 9 < Re(z) < 10 }
    end
end;

end.

unit utils;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}
{$TYPEDADDRESS ON}
{$CODEALIGN LOCALMIN=16}

interface

uses
  Classes, SysUtils, Types, Math;

const
  C45RpmRevolutionsPerSecond = 45.0 / 60.0;
  C45RpmOuterSize = 6.875;
  C45RpmInnerSize = 1.504;
  C45RpmLabelOuterSize = 3.5;
  C45RpmConcentricGroove = 3.875;
  C45RpmFirstMusicGroove = 6.625;
  C45RpmLastMusicGroove = 4.25;
  C45RpmLeadInGroovesPerInch = 16.0;
  C45RpmMaxGrooveWidth = 0.003;

{$if 0}
  cRedMul = 2126;
  cGreenMul = 7152;
  cBlueMul = 722;
{$else}
  cRedMul = 299;
  cGreenMul = 587;
  cBlueMul = 114;
{$endif}

  cLumaDiv = cRedMul + cGreenMul + cBlueMul;

  cPhi = (1 + sqrt(5)) / 2;
  cInvPhi = 1 / cPhi;

type
  TSpinlock = LongInt;
  PSpinLock = ^TSpinlock;

  TPointD = record
    X, Y: Double;
  end;

  TByteDynArray2 = array of TByteDynArray;
  TDoubleDynArray2 = array of TDoubleDynArray;
  TSingleDynArray2 = array of TSingleDynArray;

  TGRSEvalFunc = function(x: Double; Data: Pointer): Double of object;

procedure SpinEnter(Lock: PSpinLock); assembler;
procedure SpinLeave(Lock: PSpinLock); assembler;

procedure Exchange(var a, b: Integer);
function iDiv0(x, y: Integer): Integer;overload;inline;
function iDiv0(x, y: Int64): Int64;overload;inline;
function Div0(x, y: Double): Double;inline;
function NanDef(x, def: Double): Double; inline;

function SwapRB(c: Integer): Integer; inline;
function ToRGB(r, g, b: Byte): Integer; inline;
procedure FromRGB(col: Integer; out r, g, b: Integer); inline; overload;
procedure FromRGB(col: Integer; out r, g, b: Byte); inline; overload;
function ToLuma(r, g, b: Byte): Integer; inline;
function ToBW(col: Integer): Integer;

function lerp(x, y, alpha: Double): Double; inline;
function ilerp(x, y, alpha, maxAlpha: Integer): Integer; inline;
function revlerp(x, r, alpha: Double): Double; inline;
function herp(y0, y1, y2, y3, alpha: Double): Double;

function GoldenRatioSearch(Func: TGRSEvalFunc; MinX, MaxX: Double; ObjectiveY: Double;
  EpsilonX, EpsilonY: Double; Data: Pointer): Double;

procedure SobelEdgeDetector(const image: TSingleDynArray2; var out_gx, out_gy: TSingleDynArray2);

function PearsonCorrelation(const x: TDoubleDynArray; const y: TDoubleDynArray): Double;

implementation

procedure SpinEnter(Lock: PSpinLock); assembler;
label spin_lock;
asm
spin_lock:
     mov     eax, 1          // Set the EAX register to 1.

     xchg    eax, [Lock]     // Atomically swap the EAX register with the lock variable.
                             // This will always store 1 to the lock, leaving the previous value in the EAX register.

     test    eax, eax        // Test EAX with itself. Among other things, this will set the processor's Zero Flag if EAX is 0.
                             // If EAX is 0, then the lock was unlocked and we just locked it.
                             // Otherwise, EAX is 1 and we didn't acquire the lock.

     jnz     spin_lock       // Jump back to the MOV instruction if the Zero Flag is not set;
                             // the lock was previously locked, and so we need to spin until it becomes unlocked.
end;

procedure SpinLeave(Lock: PSpinLock); assembler;
asm
    xor     eax, eax        // Set the EAX register to 0.

    xchg    eax, [Lock]     // Atomically swap the EAX register with the lock variable.
end;

procedure Exchange(var a, b: Integer);
var
  tmp: Integer;
begin
  tmp := b;
  b := a;
  a := tmp;
end;

function iDiv0(x, y: Integer): Integer;overload;inline;
begin
  Result := 0;
  if y <> 0 then
    Result := x div y;
end;

function iDiv0(x, y: Int64): Int64;overload;inline;
begin
  Result := 0;
  if y <> 0 then
    Result := x div y;
end;

function Div0(x, y: Double): Double;inline;
begin
  Result := 0;
  if y <> 0 then
    Result := x / y;
end;

function NanDef(x, def: Double): Double; inline;
begin
  Result := x;
  if IsNan(Result) then
    Result := def;
end;

function SwapRB(c: Integer): Integer; inline;
begin
  Result := ((c and $ff) shl 16) or ((c shr 16) and $ff) or (c and $ff00);
end;

function ToRGB(r, g, b: Byte): Integer; inline;
begin
  Result := (b shl 16) or (g shl 8) or r;
end;

procedure FromRGB(col: Integer; out r, g, b: Integer); inline; overload;
begin
  r := col and $ff;
  g := (col shr 8) and $ff;
  b := (col shr 16) and $ff;
end;

procedure FromRGB(col: Integer; out r, g, b: Byte); inline; overload;
begin
  r := col and $ff;
  g := (col shr 8) and $ff;
  b := (col shr 16) and $ff;
end;

function ToLuma(r, g, b: Byte): Integer; inline;
begin
  Result := r * cRedMul + g * cGreenMul + b * cBlueMul;
end;

function ToBW(col: Integer): Integer;
var
  r, g, b: Byte;
begin
  FromRGB(col, r, g, b);
  Result := ToLuma(r, g, b);
  Result := Result div cLumaDiv;
  Result := ToRGB(Result, Result, Result);
end;


function lerp(x, y, alpha: Double): Double; inline;
begin
  Result := x + (y - x) * alpha;
end;

function ilerp(x, y, alpha, maxAlpha: Integer): Integer; inline;
begin
  Result := x + ((y - x) * alpha) div maxAlpha;
end;

function revlerp(x, r, alpha: Double): Double; inline;
begin
  Result := x + (r - x) / alpha;
end;

function herp(y0, y1, y2, y3, alpha: Double): Double;
var
  m0, m1, alpha2, aplha3: Double;
  a0, a1, a2, a3: Double;
begin
    alpha2 := alpha * alpha;
    aplha3 := alpha2 * alpha;
    m0 := (y1 - y0) * 0.5;
    m0 += (y2 - y1) * 0.5;
    m1 := (y2 - y1) * 0.5;
    m1 += (y3 - y2) * 0.5;
    a0 := 2 * aplha3 - 3 * alpha2 + 1;
    a1 := aplha3 - 2 * alpha2 + alpha;
    a2 := aplha3 - alpha2;
    a3 := -2 * aplha3 + 3 * alpha2;

    Result := (a0 * y1 + a1 * m0 + a2 * m1 + a3 * y2);
end;

function GoldenRatioSearch(Func: TGRSEvalFunc; MinX, MaxX: Double; ObjectiveY: Double;
  EpsilonX, EpsilonY: Double; Data: Pointer): Double;
var
  x, y: Double;
begin
  if SameValue(MinX, MaxX, EpsilonX) then
  begin
    Result := MinX;
    Exit;
  end;

  if MinX < MaxX then
    x := lerp(MinX, MaxX, 1.0 - cInvPhi)
  else
    x := lerp(MinX, MaxX, cInvPhi);

  y := Func(x, Data);

  WriteLn('X: ', x:15:6, ' Y: ', y:12:0, ' Mini: ', MinX:15:6, ' Maxi: ', MaxX:15:6);

  case CompareValue(y, ObjectiveY, EpsilonY) of
    LessThanValue:
      Result := GoldenRatioSearch(Func, x, MaxX, ObjectiveY, EpsilonX, EpsilonY, Data);
    GreaterThanValue:
      Result := GoldenRatioSearch(Func, MinX, x, ObjectiveY, EpsilonX, EpsilonY, Data);
  else
      Result := x;
  end;
end;


type
  TKernel = array[-1..1, -1..1] of integer;

function Convolve(const image:TSingleDynArray2; const kernel: TKernel; row, col: Integer): Double;
var
  y, x: Integer;
begin
  Result := 0;
  for y := -1 to 1 do
    for x := -1 to 1 do
      Result += image[y + row, x + col] * kernel[y, x];
end;

procedure SobelEdgeDetector(const image: TSingleDynArray2; var out_gx, out_gy: TSingleDynArray2);
const
  mx: TKernel = ((-1, 0, 1), (-2, 0, 2), (-1, 0, 1));
  my: TKernel = ((-1, -2, -1), (0, 0, 0), (1, 2, 1));
var
	y, x: Integer;
  gx, gy: Double;
begin
  for y := 1 to Length(image) - 2 do
    for x := 1 to Length(image[0]) - 2 do
    begin
			gx := Convolve(image, mx, y, x);
			gy := Convolve(image, my, y, x);

      out_gx[y, x] := gx;
      out_gy[y, x] := gy;
      //out_image[y, x] := Sqrt(gx * gx + gy * gy);
    end;
end;

function PearsonCorrelation(const x: TDoubleDynArray; const y: TDoubleDynArray): Double;
var
  mx, my, num, den, denx, deny: Double;
  i: Integer;
begin
  Assert(Length(x) = Length(y));

  mx := mean(x);
  my := mean(y);

  num := 0.0;
  denx := 0.0;
  deny := 0.0;
  for i := 0 to High(x) do
  begin
    num += (x[i] - mx) * (y[i] - my);
    denx += sqr(x[i] - mx);
    deny += sqr(y[i] - my);
  end;

  denx := sqrt(denx);
  deny := sqrt(deny);
  den := denx * deny;

  Result := 1.0;
  if den <> 0.0 then
    Result := num / den;
end;

end.


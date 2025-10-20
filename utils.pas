unit utils;

{$include 'compileroptions.inc'}
{$R-}

interface

uses
  Classes, SysUtils, Types, Windows, MTProcs, Math, fgl;

type
  TSpinlock = LongInt;
  PSpinLock = ^TSpinlock;

  TPointD = packed record
    X, Y: Double;
  end;

	{ TPointValue }

  TPointValue = record
    X, Y, Value: Double;
  public
    class function Create(const ax, ay, avalue: Single): TPointValue; overload; static; inline;
    class operator = (const apt1, apt2 : TPointValue) : Boolean;
    class operator <> (const apt1, apt2 : TPointValue): Boolean;
  end;

  TRectD = record
    L, T, R, B: Double;
  end;

  TSinCosD = packed record
    Sin, Cos: Double;
  end;

  TSinCosF = packed record
    Sin, Cos: Single;
  end;

  TRadiusAngle = packed record
    Radius, Angle: Double;
  end;

  TSpearmanRank = packed record
    Position, Rank: Integer;
  end;

  TPointDDynArray = array of TPointD;
  TPointDDynArray2 = array of TPointDDynArray;
  TPointValueDynArray = array of TPointValue;
  TByteDynArray2 = array of TByteDynArray;
  TWordDynArray2 = array of TWordDynArray;
  TSingleDynArray2 = array of TSingleDynArray;
  TDoubleDynArray2 = array of TDoubleDynArray;
  TDoubleDynArray3 = array of TDoubleDynArray2;
  TSinCosDDynArray = array of TSinCosD;
  TSinCosFDynArray = array of TSinCosF;
  TRadiusAngleDynArray = array of TRadiusAngle;
  TSpearmanRankDynArray = array of TSpearmanRank;

  TPointValueList = specialize TFPGList<TPointValue>;

  TSerpCoeffs9 = array[-4 .. 4 + 3] of Single;
  TSerpCoeffs9ByWord = array[0 .. High(Word)] of TSerpCoeffs9;

  TGRSEvalFunc = function(arg: Double; obj: Pointer): Double of object;
  TEvalFunc = function(const arg: TDoubleDynArray; data: Pointer): Double of object;
  TGradientEvalFunc = procedure(const arg: TDoubleDynArray; var func: Double; grad: TDoubleDynArray; obj: Pointer) of object;
  TCompareFunction = function(Item1, Item2, UserParameter: Pointer): Integer;

const
  CScannerTolLo = 0.985;
  CScannerTolHi = 1.015;

  cRedMul = 299;
  cGreenMul = 587;
  cBlueMul = 114;

  cLumaDiv = cRedMul + cGreenMul + cBlueMul;

  cPhi = (1 + sqrt(5)) / 2;
  cInvPhi = 1 / cPhi;

  CFiniteDifferencesYFactor: array[-7 .. 7] of Double = (-1/24024, 7/10296, -7/1320, 7/264, -7/72, 7/24, -7/8, 0, 7/8, -7/24, 7/72, -7/264, 7/1320, -7/10296, 1/24024);
  //CFiniteDifferencesYFactor: array[-4 .. 4] of Double = (1/280, -4/105, 1/5, -4/5, 0, 4/5, -1/5, 4/105, -1/280);
  //CFiniteDifferencesYFactor: array[-2 .. 2] of Double = (1/12, -2/3, 0, 2/3, -1/12);

function InvariantFormatSettings: TFormatSettings;

procedure SpinEnter(Lock: PSpinLock); assembler;
procedure SpinLeave(Lock: PSpinLock); assembler;
function NumberOfProcessors: Integer;

procedure Exchange(var a, b: Integer);
function iDiv0(x, y: Integer): Integer;overload;inline;
function iDiv0(x, y: Int64): Int64;overload;inline;
function DivDef(x, y, def: Double): Double;inline;
function NanDef(x, def: Double): Double; inline;
function MeanNan(const x: TDoubleDynArray): Double;

function SwapRB(c: Integer): Integer; inline;
function ToRGB(r, g, b: Byte): Integer; inline;
procedure FromRGB(col: Integer; out r, g, b: Integer); inline; overload;
procedure FromRGB(col: Integer; out r, g, b: Byte); inline; overload;
function ToLuma(r, g, b: Integer): Integer; inline;
function ToBW(r, g, b: Integer): Integer; inline;

function CompressRange(x: Double): Double; inline;

function lerp(x, y, alpha: Double): Double; overload;
function lerp(x, y: Word; alpha: Double): Double; overload;
function lerp(x, y: Integer; alpha: Double): Double; overload;
function lerpXY(topleftPx_rcx: PWORD; stride_rdx: UInt64; alphax_xmm2, alphay_xmm3: Double): Double; register; assembler;

function herp(y0, y1, y2, y3, alpha: Double): Double; inline;
function herp(y0, y1, y2, y3: Word; alpha: Double): Double; inline;
function herpXY(topleftPx: PWORD; stride: UInt64; alphaX, alphaY: Double): Double;
function herpXY_asm(topleftPx_rcx: PWORD; stride_rdx: UInt64; alphax_xmm2, alphay_xmm3: Double): Double; register; assembler;

function GoldenRatioSearch(Func: TGRSEvalFunc; MinX, MaxX: Double; ObjectiveY: Double; EpsilonX, EpsilonY: Double; Data: Pointer = nil): Double;
function GradientDescentMinimize(Func: TGradientEvalFunc; var X: TDoubleDynArray; LearningRate: array of Double; EpsilonG: Double; MaxIter: Integer; Silent: Boolean; Data: Pointer = nil): Double;
function BFGSMinimize(Func: TGradientEvalFunc; var X: TDoubleDynArray; Epsilon: Double = 1e-12; Data: Pointer = nil): Double;
function GridReduceMinimize(Func: TEvalFunc; var X: TDoubleDynArray; GridSize: array of Integer; GridExtents: array of Double; EpsilonReduce: Double; VerboseTag: String = ''; Data: Pointer = nil): Double;
function NelderMeadMinimize(Func: TEvalFunc; var X: TDoubleDynArray; SimplexExtents: array of Double; Epsilon: Double = 1e-9; Data: Pointer = nil): Double;
function LBFGSMinimize(Func: TGradientEvalFunc; var X: TDoubleDynArray; Epsilon: Double = 1e-12; M: Integer = 5; Data: Pointer = nil): Double;
function LBFGSScaledMinimize(Func: TGradientEvalFunc; var X: TDoubleDynArray; Scale: array of Double; Epsilon: Double = 1e-12; M: Integer = 5; Data: Pointer = nil): Double;
function NonSmoothMinimize(Func: TGradientEvalFunc; var X: TDoubleDynArray; Epsilon: Double = 1e-12; Data: Pointer = nil): Double;
function NonSmoothBoundedMinimize(Func: TGradientEvalFunc; var X: TDoubleDynArray; LowBound, UpBound: array of Double; Epsilon: Double = 1e-12; Data: Pointer = nil): Double;

function PseudoHuber(x: Double): Double;
function MAE(const a: TDoubleDynArray; const b: TDoubleDynArray): Double;
function MAE(const a: TWordDynArray; const b: TWordDynArray): Double;
function MSE(const a: TDoubleDynArray; const b: TDoubleDynArray): Double; overload;
function MSE(const a: TWordDynArray; const b: TWordDynArray): Double; overload;
function PearsonCorrelation(const a: TDoubleDynArray; const b: TDoubleDynArray): Double;
procedure SpearmanPrepareRanks(const a: TDoubleDynArray; var ranks: TSpearmanRankDynArray);
function SpearmanRankCorrelation(const a: TSpearmanRankDynArray; const b: TSpearmanRankDynArray): Double;
function Median(x: PDouble; cnt: integer): Double;

function Make8BitSample(smp: Double): ShortInt;
function Make16BitSample(smp: Double): SmallInt;

function NormalizeAngle(x: Double): Double;
function InNormalizedAngle(x, xmin, xmax: Double): Boolean;
function NormalizedAngleDiff(xmin, xmax: Double): Double;
function NormalizedAngleTo02Pi(x: Double): Double;

procedure BuildSinCosLUT(APointCount: Integer; var ASinCosLUT: TSinCosDDynArray; AOriginAngle: Double = 0.0; AExtentsAngle: Double = 2.0 * Pi); overload;
procedure BuildSinCosLUT(APointCount: Integer; var ASinCosLUT: TSinCosFDynArray; AOriginAngle: Double = 0.0; AExtentsAngle: Double = 2.0 * Pi); overload;
function BuildRadiusAngleLUT(StartRadius, EndRadius, StartAngle, EndAngle, PxCountDiv: Double): TRadiusAngleDynArray;
function OffsetRadiusAngleLUTAngle(const LUT: TRadiusAngleDynArray; AngleOffset: Double): TSinCosDDynArray;
function CutoffToFeedbackRatio(Cutoff: Double; SampleRate: Integer): Double;
procedure IncrementalSinCos(Angle: Double; var PrevAngle: Double; var ASinCos: TSinCosD);

procedure CreateWAV(channels: word; resolution: word; rate: longint; fn: string; const data: TSmallIntDynArray); overload;
procedure CreateWAV(channels: word; resolution: word; rate: longint; fn: string; const data: TDoubleDynArray); overload;
function GetMono(const ASample: TPointD): Double;
function AdjustSample(const sample, meanSD: TPointD): TPointD;

procedure QuickSort(var AData;AFirstItem,ALastItem,AItemSize:Integer;ACompareFunction:TCompareFunction;AUserParameter:Pointer=nil);

function GetTIFFSize(AStream: TStream; out AWidth, AHeight: DWord; out dpiX, dpiY: Double): Boolean;

implementation
uses utypes, ubfgs, usimplex;

var
  GInvariantFormatSettings: TFormatSettings;
  GSerpCoeffs9ByWord: TSerpCoeffs9ByWord;

function alglib_NonSmoothBoundedMinimize(Func: Pointer; n: Integer; X, LowBound, UpBound: PDouble; Epsilon, Radius, Penalty: Double; Data: Pointer): Double; stdcall; external 'alglib-cpp-vinylscan.dll';
function alglib_LBFGSMinimize(Func: Pointer; n: Integer; X, Scale: PDouble; Epsilon: Double; M: Integer; Data: Pointer): Double; stdcall; external 'alglib-cpp-vinylscan.dll';
function alglib_SpearmanRankCorrelation(X, Y: PDouble; n: Integer): Double; stdcall; external 'alglib-cpp-vinylscan.dll';

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

function NumberOfProcessors: Integer;
var
  SystemInfo: SYSTEM_INFO;
begin
  GetSystemInfo(SystemInfo);
  Result := SystemInfo.dwNumberOfProcessors;
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

function DivDef(x, y, def: Double): Double;
begin
  Result := def;
  if y <> 0 then
    Result := x / y;
end;

function NanDef(x, def: Double): Double; inline;
begin
  Result := x;
  if IsNan(Result) then
    Result := def;
end;

function MeanNan(const x: TDoubleDynArray): Double;
var
  i, cnt: Integer;
begin
  cnt := 0;
  Result := 0.0;

  for i := 0 to High(x) do
    if not IsNan(x[i]) then
    begin
      Result += x[i];
      Inc(cnt);
    end;

  if cnt > 1 then
    Result /= cnt;
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

function ToLuma(r, g, b: Integer): Integer; inline;
begin
  Result := r * cRedMul + g * cGreenMul + b * cBlueMul;
end;

function ToBW(r, g, b: Integer): Integer; inline;
begin
  Result := ToLuma(r, g, b) div cLumaDiv;
end;

{* https://stackoverflow.com/questions/73770905/best-non-trigonometric-floating-point-approximation-of-tanhx-in-10-instruction
   Fast computation of hyperbolic tangent. Rational approximation with clamping
   of the argument. Maximum absolute error = 1.98537030e-5, maximum relative
   error = 1.98540995e-5, maximum ulp error = 333.089863.
*}

function fma(x, y, z: Double): Double; inline;
begin
  Result := (x * y) + z;
end;

function fast_tanh_rat3(x: Double): Double; inline; // 10 operations
const cutoff = 5.76110792; //  0x1.70b5fep+2
const n0 = -1.60153955e-4; // -0x1.4fde00p-13
const n1 = -9.34448242e-1; // -0x1.de7000p-1
const n2 = -2.19176636e+1; // -0x1.5eaec0p+4
const d0 =  2.90915985e+1; //  0x1.d17730p+4
const d1 =  6.57667847e+1; //  0x1.071130p+6
var
  y, y2, num, den, quot: Double;
begin
  y := EnsureRange(x, -cutoff, cutoff);
  y2 := y * y;
  num := fma(fma(n0, y2, n1), y2, n2) * y2;
  den := fma(y2 + d0, y2, d1);
  quot := num / den;
  Result := fma(quot, y, y);
end;

function CompressRange(x: Double): Double;
begin
  Result := fast_tanh_rat3(x);
end;

function lerp(x, y, alpha: Double): Double;
begin
  Result := x + (y - x) * alpha;
end;

function lerp(x, y: Word; alpha: Double): Double;
begin
  Result := x + (y - x) * alpha;
end;

function lerp(x, y: Integer; alpha: Double): Double;
begin
  Result := x + (y - x) * alpha;
end;

{$ifdef CPUX64}

function lerpXY(topleftPx_rcx: PWORD; stride_rdx: UInt64; alphax_xmm2, alphay_xmm3: Double): Double; register; assembler;
asm
  pxor xmm0, xmm0
  pxor xmm1, xmm1

  pinsrw xmm0, word ptr [rcx], 0
  pinsrw xmm0, word ptr [rcx + rdx * 2], 2
  pinsrw xmm1, word ptr [rcx + 2], 0
  pinsrw xmm1, word ptr [rcx + rdx * 2 + 2], 2

  cvtdq2pd xmm0, xmm0
  cvtdq2pd xmm1, xmm1

  subpd xmm1, xmm0
  movlhps xmm2,xmm2
  mulpd xmm1, xmm2
  addpd xmm0, xmm1

  movhlps xmm1,xmm0

  subsd xmm1,xmm0
  mulsd xmm1,xmm3
  addsd xmm0,xmm1
end;

{$else}

{$endif}


function herp(y0, y1, y2, y3, alpha: Double): Double;
var
  alpha2, alpha3: Double;
  a0, a1, a2, a3: Double;
begin
  alpha2 := alpha * alpha;
  alpha3 := alpha2 * alpha;

  a3 := 0.5 * (-1 * y0 + +3 * y1 + -3 * y2 + +1 * y3);
  a2 := 0.5 * (+2 * y0 + -5 * y1 + +4 * y2 + -1 * y3);
  a1 := 0.5 * (-1 * y0 + +0 * y1 + +1 * y2 + +0 * y3);
  a0 := y1;

  Result := a3 * alpha3 + a2 * alpha2 + a1 * alpha + a0;
end;

function herp(y0, y1, y2, y3: Word; alpha: Double): Double;
var
  alpha2, alpha3: Double;
  a0, a1, a2, a3: Integer;
begin
  alpha2 := alpha * alpha;
  alpha3 := alpha2 * alpha;

  a3 := (-1 * y0 + +3 * y1 + -3 * y2 + +1 * y3) shr 1;
  a2 := (+2 * y0 + -5 * y1 + +4 * y2 + -1 * y3) shr 1;
  a1 := (-1 * y0 + +0 * y1 + +1 * y2 + +0 * y3) shr 1;
  a0 := y1;

  Result := a3 * alpha3 + a2 * alpha2 + a1 * alpha + a0;
end;

function herpXY(topleftPx: PWORD; stride: UInt64; alphaX, alphaY: Double): Double;
var
  alphaX2, alphaX3: Double;
  a0, a1, a2, a3: Integer;
  x0, x1, x2, x3: Word;
  y0, y1, y2, y3: Double;
begin
  alphaX2 := alphaX * alphaX;
  alphaX3 := alphaX2 * alphaX;

  Dec(topleftPx, stride);

  x0 := topleftPx[-1];
  x1 := topleftPx[0];
  x2 := topleftPx[1];
  x3 := topleftPx[2];
  a3 := (-1 * x0 + +3 * x1 + -3 * x2 + +1 * x3) shr 1;
  a2 := (+2 * x0 + -5 * x1 + +4 * x2 + -1 * x3) shr 1;
  a1 := (-1 * x0 + +0 * x1 + +1 * x2 + +0 * x3) shr 1;
  a0 := x1;
  y0 := a3 * alphaX3 + a2 * alphaX2 + a1 * alphaX + a0;
  Inc(topleftPx, stride);

  x0 := topleftPx[-1];
  x1 := topleftPx[0];
  x2 := topleftPx[1];
  x3 := topleftPx[2];
  a3 := (-1 * x0 + +3 * x1 + -3 * x2 + +1 * x3) shr 1;
  a2 := (+2 * x0 + -5 * x1 + +4 * x2 + -1 * x3) shr 1;
  a1 := (-1 * x0 + +0 * x1 + +1 * x2 + +0 * x3) shr 1;
  a0 := x1;
  y1 := a3 * alphaX3 + a2 * alphaX2 + a1 * alphaX + a0;
  Inc(topleftPx, stride);

  x0 := topleftPx[-1];
  x1 := topleftPx[0];
  x2 := topleftPx[1];
  x3 := topleftPx[2];
  a3 := (-1 * x0 + +3 * x1 + -3 * x2 + +1 * x3) shr 1;
  a2 := (+2 * x0 + -5 * x1 + +4 * x2 + -1 * x3) shr 1;
  a1 := (-1 * x0 + +0 * x1 + +1 * x2 + +0 * x3) shr 1;
  a0 := x1;
  y2 := a3 * alphaX3 + a2 * alphaX2 + a1 * alphaX + a0;
  Inc(topleftPx, stride);

  x0 := topleftPx[-1];
  x1 := topleftPx[0];
  x2 := topleftPx[1];
  x3 := topleftPx[2];
  a3 := (-1 * x0 + +3 * x1 + -3 * x2 + +1 * x3) shr 1;
  a2 := (+2 * x0 + -5 * x1 + +4 * x2 + -1 * x3) shr 1;
  a1 := (-1 * x0 + +0 * x1 + +1 * x2 + +0 * x3) shr 1;
  a0 := x1;
  y3 := a3 * alphaX3 + a2 * alphaX2 + a1 * alphaX + a0;

  Result := herp(y0, y1, y2, y3, alphaY);
end;

function herpXY_asm(topleftPx_rcx: PWORD; stride_rdx: UInt64; alphax_xmm2, alphay_xmm3: Double): Double; register; assembler;
label
  coeffsInt,coeffsDbl,funcEnd;
asm
  // init

  push rax

  sub rsp, 16 * 11
  movdqu oword ptr [rsp],       xmm1
  movdqu oword ptr [rsp + $10], xmm2
  movdqu oword ptr [rsp + $20], xmm3
  movdqu oword ptr [rsp + $30], xmm4
  movdqu oword ptr [rsp + $40], xmm5
  movdqu oword ptr [rsp + $50], xmm6
  movdqu oword ptr [rsp + $60], xmm7
  movdqu oword ptr [rsp + $70], xmm8
  movdqu oword ptr [rsp + $80], xmm9
  movdqu oword ptr [rsp + $90], xmm10
  movdqu oword ptr [rsp + $a0], xmm11

  pxor xmm0,xmm0

  // load data

  dec rcx
  dec rcx
  shl rdx,1
  sub rcx,rdx

  movq xmm4,qword ptr[rcx]
  movq xmm5,qword ptr[rcx + rdx]
  movq xmm6,qword ptr[rcx + rdx * 2]
  add rcx,rdx
  movq xmm7,qword ptr[rcx + rdx * 2]

  punpcklwd xmm4,xmm0
  punpcklwd xmm5,xmm0
  punpcklwd xmm6,xmm0
  punpcklwd xmm7,xmm0

  //

  movdqa xmm1,oword ptr [rip+@coeffsInt]

  movdqa xmm8,xmm4
  pmulld xmm8,xmm1
  phaddd xmm8,xmm8
  phaddd xmm8,xmm8

  movdqa xmm0,xmm5
  pmulld xmm0,xmm1
  phaddd xmm0,xmm0
  phaddd xmm0,xmm0
  insertps xmm8,xmm0,$50

  movdqa xmm0,xmm6
  pmulld xmm0,xmm1
  phaddd xmm0,xmm0
  phaddd xmm0,xmm0
  insertps xmm8,xmm0,$a0

  movdqa xmm0,xmm7
  pmulld xmm0,xmm1
  phaddd xmm0,xmm0
  phaddd xmm0,xmm0
  insertps xmm8,xmm0,$f0

  psrad xmm8,1

  movdqa xmm1,oword ptr [rip+@coeffsInt+16]

  movdqa xmm9,xmm4
  pmulld xmm9,xmm1
  phaddd xmm9,xmm9
  phaddd xmm9,xmm9

  movdqa xmm0,xmm5
  pmulld xmm0,xmm1
  phaddd xmm0,xmm0
  phaddd xmm0,xmm0
  insertps xmm9,xmm0,$50

  movdqa xmm0,xmm6
  pmulld xmm0,xmm1
  phaddd xmm0,xmm0
  phaddd xmm0,xmm0
  insertps xmm9,xmm0,$a0

  movdqa xmm0,xmm7
  pmulld xmm0,xmm1
  phaddd xmm0,xmm0
  phaddd xmm0,xmm0
  insertps xmm9,xmm0,$f0

  psrad xmm9,1

  movdqa xmm1,oword ptr [rip+@coeffsInt+32]

  movdqa xmm10,xmm4
  pmulld xmm10,xmm1
  phaddd xmm10,xmm10
  phaddd xmm10,xmm10

  movdqa xmm0,xmm5
  pmulld xmm0,xmm1
  phaddd xmm0,xmm0
  phaddd xmm0,xmm0
  insertps xmm10,xmm0,$50

  movdqa xmm0,xmm6
  pmulld xmm0,xmm1
  phaddd xmm0,xmm0
  phaddd xmm0,xmm0
  insertps xmm10,xmm0,$a0

  movdqa xmm0,xmm7
  pmulld xmm0,xmm1
  phaddd xmm0,xmm0
  phaddd xmm0,xmm0
  insertps xmm10,xmm0,$f0

  psrad xmm10,1

  //

  movlhps xmm2,xmm2

  cvtdq2pd xmm6,xmm10
  mulpd xmm6,xmm2
  psrldq xmm10,8
  cvtdq2pd xmm7,xmm10
  mulpd xmm7,xmm2

  movdqa xmm11,xmm2
  mulpd xmm2,xmm2

  cvtdq2pd xmm4,xmm9
  mulpd xmm4,xmm2
  psrldq xmm9,8
  cvtdq2pd xmm5,xmm9
  mulpd xmm5,xmm2

  mulpd xmm2,xmm11

  cvtdq2pd xmm0,xmm8
  mulpd xmm0,xmm2
  psrldq xmm8,8
  cvtdq2pd xmm1,xmm8
  mulpd xmm1,xmm2

  addpd xmm0,xmm4
  addpd xmm1,xmm5
  addpd xmm0,xmm6
  addpd xmm1,xmm7

  // add the last term (from data)

  inc rcx
  inc rcx
  sub rcx,rdx

  movzx eax,word ptr[rcx + rdx]
  cvtsi2sd xmm4,eax
  movlhps xmm4,xmm4
  movzx eax,word ptr[rcx]
  cvtsi2sd xmm4,eax

  add rcx,rdx
  movzx eax,word ptr[rcx + rdx * 2]
  cvtsi2sd xmm5,eax
  movlhps xmm5,xmm5
  sub rcx,rdx
  movzx eax,word ptr[rcx + rdx * 2]
  cvtsi2sd xmm5,eax

  addpd xmm0,xmm4
  addpd xmm1,xmm5

  // simple y interpolation

  movhlps xmm10,xmm0

  movdqa xmm4,xmm0
  movdqa xmm5,xmm1
  movdqa xmm6,xmm0
  movdqa xmm7,xmm1
  movdqa xmm8,xmm0
  movdqa xmm9,xmm1

  mulpd xmm4,oword ptr [rip+@coeffsDbl]
  mulpd xmm5,oword ptr [rip+@coeffsDbl+16]
  mulpd xmm6,oword ptr [rip+@coeffsDbl+32]
  mulpd xmm7,oword ptr [rip+@coeffsDbl+48]
  mulpd xmm8,oword ptr [rip+@coeffsDbl+64]
  mulpd xmm9,oword ptr [rip+@coeffsDbl+80]

  addpd xmm4,xmm5
  addpd xmm6,xmm7
  addpd xmm8,xmm9

  haddpd xmm4,xmm4
  haddpd xmm6,xmm6
  haddpd xmm8,xmm8

  mulsd xmm8,xmm3

  movdqa xmm2,xmm3
  mulsd xmm3,xmm3
  mulsd xmm6,xmm3

  mulsd xmm3,xmm2
  mulsd xmm4,xmm3

  movsd xmm0,xmm8
  addsd xmm4,xmm6
  addsd xmm0,xmm4

  mulsd xmm0,qword ptr [rip+@coeffsDbl+96]

  addsd xmm0,xmm10

  // finish

  shr rdx,1

  movdqu xmm1, oword ptr [rsp]
  movdqu xmm2, oword ptr [rsp + $10]
  movdqu xmm3, oword ptr [rsp + $20]
  movdqu xmm4, oword ptr [rsp + $30]
  movdqu xmm5, oword ptr [rsp + $40]
  movdqu xmm6, oword ptr [rsp + $50]
  movdqu xmm7, oword ptr [rsp + $60]
  movdqu xmm8, oword ptr [rsp + $70]
  movdqu xmm9, oword ptr [rsp + $80]
  movdqu xmm10, oword ptr [rsp + $90]
  movdqu xmm11, oword ptr [rsp + $a0]
  add rsp, 16 * 11

  pop rax

  jmp @funcEnd

  align 16
@coeffsInt:
  dd -1,  3, -3,  1
  dd  2, -5,  4, -1
  dd -1,  0,  1,  0
@coeffsDbl:
  dq $BFF0000000000000, $4008000000000000, $C008000000000000, $3FF0000000000000
  dq $4000000000000000, $C014000000000000, $4010000000000000, $BFF0000000000000
  dq $BFF0000000000000,                 0, $3FF0000000000000,                 0
  dq $3FE0000000000000
@funcEnd:

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

  //WriteLn('X: ', x:15:6, ' Y: ', y:12:0, ' Mini: ', MinX:15:6, ' Maxi: ', MaxX:15:6);

  case CompareValue(y, ObjectiveY, EpsilonY) of
    LessThanValue:
      Result := GoldenRatioSearch(Func, x, MaxX, ObjectiveY, EpsilonX, EpsilonY, Data);
    GreaterThanValue:
      Result := GoldenRatioSearch(Func, MinX, x, ObjectiveY, EpsilonX, EpsilonY, Data);
  else
      Result := x;
  end;
end;

function GradientDescentMinimize(Func: TGradientEvalFunc; var X: TDoubleDynArray; LearningRate: array of Double;
  EpsilonG: Double; MaxIter: Integer; Silent: Boolean; Data: Pointer): Double;
var
  gm, f: Double;
  grad, bestX: TDoubleDynArray;
  i: Integer;
  iter: Integer;
begin
  Assert(Length(LearningRate) = Length(X));
  SetLength(grad, Length(X));
  SetLength(bestX, Length(X));

  gm := NaN;
  f := NaN;
  Result := Infinity;
  iter := 0;
  repeat
    Func(X, f, grad, Data);

    if not Silent then
    begin
      Write(Result:16:6, gm:16:6);
      for i := 0 to High(X) do
        Write(X[i]:14:6);
      for i := 0 to High(grad) do
        Write(grad[i]:16:6);
      WriteLn;
    end;

    if f <= Result then
    begin
      for i := 0 to High(X) do
        bestX[i] := X[i];
      Result := f;
    end;

    gm := 0.0;
    for i := 0 to High(X) do
    begin
      X[i] -= LearningRate[i] * grad[i];
      gm := max(gm, Abs(grad[i]));
    end;

    Inc(iter);
  until (gm <= EpsilonG) or (iter >= MaxIter);

  for i := 0 to High(X) do
    X[i] := bestX[i];

  if not Silent then
    WriteLn(iter:8);
end;

threadvar
  GBFGSData: Pointer;
  GBFGSFunc: TGradientEvalFunc;

  function BFGSX(X : TVector) : Float;
  begin
    GBFGSFunc(X, Result, nil, GBFGSData);
  end;

  procedure BFGSG(X, G : TVector);
  var
    dummy: Double;
  begin
    dummy := NaN;
    GBFGSFunc(X, dummy, G, GBFGSData);
  end;

function BFGSMinimize(Func: TGradientEvalFunc; var X: TDoubleDynArray; Epsilon: Double; Data: Pointer): Double;
var
  G: TVector;
  H: TMatrix;
begin
  SetLength(G, Length(X));
  SetLength(H, Length(X), Length(X));

  GBFGSData := Data;
  GBFGSFunc := Func;
  try
    BFGS(@BFGSX, @BFGSG, X, 0, High(X), MaxInt, Epsilon, Result, G, H);
  finally
    GBFGSData := nil;
    GBFGSFunc := nil;
  end;
end;


threadvar
  GNMData: Pointer;
  GNMFunc: TEvalFunc;

  function NMX(X : TVector) : Float;
  begin
    Result := GNMFunc(X, GNMData);
  end;

function NelderMeadMinimize(Func: TEvalFunc; var X: TDoubleDynArray; SimplexExtents: array of Double; Epsilon: Double; Data: Pointer): Double;
var
  iX: Integer;
  InitSimplex: TDoubleDynArray;
begin
  Assert((Length(X) = Length(SimplexExtents)) or (Length(SimplexExtents) = 0));

  GNMData := Data;
  GNMFunc := Func;
  try
    SetLength(InitSimplex, Length(SimplexExtents));
    for iX := 0 to High(InitSimplex) do
      InitSimplex[iX] := X[iX] + SimplexExtents[iX];

    Simplex(@NMX, X, 0, High(X), Length(X) * 200, Epsilon, Result, InitSimplex);
  finally
    GNMData := nil;
    GNMFunc := nil;
  end;
end;

function GridReduceMinimize(Func: TEvalFunc; var X: TDoubleDynArray; GridSize: array of Integer;
 GridExtents: array of Double; EpsilonReduce: Double; VerboseTag: String; Data: Pointer): Double;
var
  XBestFunc, bestX: TDoubleDynArray;
  reduce: TDoubleDynArray;

  procedure DoX(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iX: PtrInt;
    gs, iGrid: Integer;
    f: Double;
    lX: TDoubleDynArray;
  begin
    iX := AIndex;
    if not InRange(iX, 0, High(X)) or IsZero(reduce[iX], EpsilonReduce) then
      Exit;

    lX := Copy(X);
    gs := GridSize[iX];

    for iGrid := -gs to gs - 1 do
    begin
      lX[iX] := X[iX] + lerp(-GridExtents[iX], GridExtents[iX], (iGrid + gs) / (2 * gs)) * reduce[iX];

      f := Func(lX, data);

      if f < XBestFunc[iX] then
      begin
        XBestFunc[iX] := f;
        bestX[iX] := lX[iX];
      end;
    end;
  end;

var
  iter, iX: Integer;
  bestFunc: Double;
begin
  Assert(Length(X) = Length(GridSize));
  Assert(Length(X) = Length(GridExtents));

  SetLength(XBestFunc, Length(X));
  SetLength(bestX, Length(X));

  SetLength(reduce, Length(X));
  for iX := 0 to High(X) do
    reduce[iX] := 1.0;

  iter := 0;
  repeat

    for iX := 0 to High(X) do
    begin
      bestX[iX] := X[iX];
      XBestFunc[iX] := Infinity;
    end;

    if Length(X) = 0 then
      DoX(0, nil, nil)
    else
      ProcThreadPool.DoParallelLocalProc(@DoX, 0, High(X));

    Inc(iter);
    bestFunc := MinValue(XBestFunc);

    for iX := 0 to High(X) do
      if XBestFunc[iX] = bestFunc then
      begin
        X[iX] := bestX[iX];
        reduce[iX] *= cInvPhi;

        if VerboseTag <> '' then
          WriteLn(VerboseTag, ', Iter:', iter:4, ', BestX:', iX:4, ', Reduce:', reduce[iX]:12:9, ', Func:', bestFunc:20:9);
      end;

  until IsZero(MaxValue(reduce), EpsilonReduce);

  Result := bestFunc;
end;

threadvar
  GLBFGSFunc: TGradientEvalFunc;

  procedure LBFGSFunc(n: Integer; arg: PDouble; func: PDouble; grad: PDouble; obj: Pointer);
  var
    i: Integer;
    lfunc: Double;
    larg: TDoubleDynArray;
    lgrad: TDoubleDynArray;
  begin
    lfunc := NaN;
    SetLength(larg, n);
    SetLength(lgrad, n);
    for i := 0 to n - 1 do
      larg[i] := arg[i];

    GLBFGSFunc(larg, lfunc, lgrad, obj);

    for i := 0 to n - 1 do
      grad[i] := lgrad[i];

    func^ := lfunc;
  end;

function LBFGSScaledMinimize(Func: TGradientEvalFunc; var X: TDoubleDynArray; Scale: array of Double; Epsilon: Double;
  M: Integer; Data: Pointer): Double;
begin
  GLBFGSFunc := Func;
  try
    M := min(Length(X), M);
    Result := alglib_LBFGSMinimize(@LBFGSFunc, Length(X), @X[0], @Scale[0], Epsilon, M, Data);
  finally
    GLBFGSFunc := nil;
  end;
end;

function LBFGSMinimize(Func: TGradientEvalFunc; var X: TDoubleDynArray; Epsilon: Double; M: Integer; Data: Pointer): Double;
begin
  GLBFGSFunc := Func;
  try
    M := min(Length(X), M);
    Result := alglib_LBFGSMinimize(@LBFGSFunc, Length(X), @X[0], nil, Epsilon, M, Data);
  finally
    GLBFGSFunc := nil;
  end;
end;

threadvar
  GNSFunc: TGradientEvalFunc;

  procedure NSFunc(n: Integer; arg: PDouble; fi: PDouble; jac: PPDouble; obj: Pointer);
  var
    i: Integer;
    lfunc: Double;
    larg: TDoubleDynArray;
    lgrad: TDoubleDynArray;
  begin
    lfunc := NaN;
    SetLength(larg, n);
    SetLength(lgrad, n);
    for i := 0 to n - 1 do
      larg[i] := arg[i];

    GNSFunc(larg, lfunc, lgrad, obj);

    for i := 0 to n - 1 do
      jac[0, i] := lgrad[i];

    fi[0] := lfunc;
  end;

function NonSmoothMinimize(Func: TGradientEvalFunc; var X: TDoubleDynArray; Epsilon: Double; Data: Pointer): Double;
begin
  GNSFunc := Func;
  try
    Result := alglib_NonSmoothBoundedMinimize(@NSFunc, Length(X), @X[0], nil, nil, Epsilon, 1e-3, 0.0, Data);
  finally
    GNSFunc := nil;
  end;
end;

function NonSmoothBoundedMinimize(Func: TGradientEvalFunc; var X: TDoubleDynArray; LowBound, UpBound: array of Double; Epsilon: Double; Data: Pointer): Double;
begin
  GNSFunc := Func;
  try
    Result := alglib_NonSmoothBoundedMinimize(@NSFunc, Length(X), @X[0], @LowBound[0], @UpBound[0], Epsilon, 1e-3, 50.0, Data);
  finally
    GNSFunc := nil;
  end;
end;

function PseudoHuber(x: Double): Double;
const
  CDelta = 512.0;
  CInvDelta = 1.0 / CDelta;
begin
  Result := CDelta * (Sqrt(1.0 + Sqr(x * CInvDelta)) - 1);
end;

function MAE(const a: TDoubleDynArray; const b: TDoubleDynArray): Double;
var
  i: Integer;
  acc: Double;
begin
  Assert(Length(a) = Length(b));

  Result := 0.0;
  if not Assigned(a) then
    Exit;

  acc := 0;
  for i := 0 to High(a) do
    acc += Abs(a[i] - b[i]);

  Result := acc / Length(a);
end;

function MAE(const a: TWordDynArray; const b: TWordDynArray): Double;
var
  i: Integer;
  acc: Cardinal;
begin
  Assert(Length(a) = Length(b));

  Result := 0.0;
  if not Assigned(a) then
    Exit;

  acc := 0;
  for i := 0 to High(a) do
    acc += Abs(a[i] - b[i]);

  Result := acc / Length(a);
end;


function MSE(const a: TDoubleDynArray; const b: TDoubleDynArray): Double;
var
  i: Integer;
begin
  Assert(Length(a) = Length(b));

  Result := 0.0;
  if not Assigned(a) then
    Exit;

  for i := 0 to High(a) do
    Result += Sqr(a[i] - b[i]);

  Result /= Length(a);
end;

function MSE(const a: TWordDynArray; const b: TWordDynArray): Double; overload;
var
  i: Integer;
  acc: UInt64;
begin
  Assert(Length(a) = Length(b));

  Result := 0.0;
  if not Assigned(a) then
    Exit;

  acc := 0;
  for i := 0 to High(a) do
    acc += Sqr(a[i] - b[i]);

  Result := acc / Length(a);
end;

function PearsonCorrelation(const a: TDoubleDynArray; const b: TDoubleDynArray): Double;
var
  ma, mb, num, den, dena, denb: Double;
  i: Integer;
begin
  Assert(Length(a) = Length(b));

  Result := 1.0;
  if not Assigned(a) then
    Exit;

  ma := mean(a);
  mb := mean(b);

  num := 0.0;
  dena := 0.0;
  denb := 0.0;
  for i := 0 to High(a) do
  begin
    num += (a[i] - ma) * (b[i] - mb);
    dena += sqr(a[i] - ma);
    denb += sqr(b[i] - mb);
  end;

  den := sqrt(dena * denb);

  if den <> 0.0 then
    Result := num / den;
end;

function SpearmanCompare(Item1, Item2, UserParameter: Pointer): Integer;
var
  r1: ^TSpearmanRank absolute Item1;
  r2: ^TSpearmanRank absolute Item2;
  values: PDouble absolute UserParameter;
  p1, p2: Integer;
begin
  p1 := r1^.Position;
  p2 := r2^.Position;

  Result := CompareValue(values[p1], values[p2]);
  if Result = 0 then
    Result := CompareValue(p1, p2);
end;

procedure SpearmanPrepareRanks(const a: TDoubleDynArray; var ranks: TSpearmanRankDynArray);
var
  i: Integer;
begin
  Assert(Length(a) = Length(ranks));

  for i := 0 to High(ranks) do
    ranks[i].Position := i;

  QuickSort(ranks[0], 0, High(ranks), SizeOf(TSpearmanRank), @SpearmanCompare, @a[0]);

  for i := 0 to High(ranks) do
    ranks[ranks[i].Position].Rank := i + 1;
end;

function SpearmanRankCorrelation(const a: TSpearmanRankDynArray; const b: TSpearmanRankDynArray): Double;
var
  i, sz: Integer;
  mn, num, da, db, den, dena, denb: Double;
begin
  Assert(Length(a) = Length(b));

  sz := Length(a);

  mn := sz * 0.5;

  num := 0.0;
  dena := 0.0;
  denb := 0.0;
  for i := 0 to High(a) do
  begin
    da := a[i].Rank - mn;
    db := b[i].Rank - mn;

    num += da * db;
    dena += sqr(da);
    denb += sqr(db);
  end;

  den := sqrt(dena * denb);

  Result := 1.0;
  if den <> 0.0 then
    Result := num / den;
end;

function CompareDouble(Item1, Item2, UserParameter: Pointer): Integer;
var
  d1: PDouble absolute Item1;
  d2: PDouble absolute Item2;
begin
  Result := CompareValue(d1^, d2^);
end;

function Median(x: PDouble; cnt: integer): Double;
var
  hl: Integer;
begin
  QuickSort(x[0], 0, cnt - 1, SizeOf(Double), @CompareDouble);

  hl := (cnt - 1) shr 1;
  if Odd(cnt) then
    Result := x[hl]
  else
    Result := (x[hl] + x[hl + 1]) * 0.5;
end;

function Make8BitSample(smp: Double): ShortInt;
begin
  Result := EnsureRange(round(smp * High(ShortInt)), Low(ShortInt), High(ShortInt));
end;

function Make16BitSample(smp: Double): SmallInt;
begin
  Result := EnsureRange(round(smp * High(SmallInt)), Low(SmallInt), High(SmallInt));
end;

function NormalizeAngle(x: Double): Double;
begin
  while x < -Pi do
    x += 2.0 * Pi;
  while x > Pi do
    x -= 2.0 * Pi;

  Assert(InRange(x, -Pi, Pi));

  Result := x;
end;

function InNormalizedAngle(x, xmin, xmax: Double): Boolean;
begin
  Assert(InRange(x, -Pi, Pi));
  Assert(InRange(xmin, -Pi, Pi));
  Assert(InRange(xmax, -Pi, Pi));
  if xmax > xmin then
    Result := InRange(x, xmin, xmax)
  else
    Result := InRange(x, -Pi, xmax) or InRange(x, xmin, Pi);
end;

function NormalizedAngleDiff(xmin, xmax: Double): Double;
begin
  Assert(InRange(xmin, -Pi, Pi));
  Assert(InRange(xmax, -Pi, Pi));
  Result := NormalizedAngleTo02Pi(xmax - xmin);
end;

function NormalizedAngleTo02Pi(x: Double): Double;
begin
  Result := x;
  if Result < 0 then
    Result += 2.0 * Pi;
end;

procedure BuildSinCosLUT(APointCount: Integer; var ASinCosLUT: TSinCosDDynArray; AOriginAngle: Double; AExtentsAngle: Double);
var
  i: Integer;
  rprp: Double;
begin
  SetLength(ASinCosLUT, APointCount);
  rprp := AExtentsAngle / APointCount;
  for i := 0 to APointCount - 1 do
    SinCos(AOriginAngle + i * rprp, ASinCosLUT[i].Sin, ASinCosLUT[i].Cos);
end;

procedure BuildSinCosLUT(APointCount: Integer; var ASinCosLUT: TSinCosFDynArray; AOriginAngle: Double; AExtentsAngle: Double);
var
  i: Integer;
  sn, cs, rprp: Double;
begin
  SetLength(ASinCosLUT, APointCount);
  rprp := AExtentsAngle / APointCount;
  for i := 0 to APointCount - 1 do
  begin
    SinCos(AOriginAngle + i * rprp, sn, cs);
    ASinCosLUT[i].Sin := sn;
    ASinCosLUT[i].Cos := cs;
  end;
end;

function CompareRadiusAngle(Item1, Item2, UserParameter: Pointer): Integer;
var
  ra1: ^TRadiusAngle absolute Item1;
  ra2: ^TRadiusAngle absolute Item2;
begin
  Result := CompareValue(ra1^.Angle, ra2^.Angle);
  if Result = 0 then
    Result := CompareValue(ra1^.Radius, ra2^.Radius);
end;

function BuildRadiusAngleLUT(StartRadius, EndRadius, StartAngle, EndAngle, PxCountDiv: Double): TRadiusAngleDynArray;
var
  oy, ox, cnt, rEndInt: Integer;
  r, t, nsa, nea, diff: Double;
begin
  rEndInt := Ceil(EndRadius / PxCountDiv);
  nsa := NormalizeAngle(startAngle);
  nea := NormalizeAngle(endAngle);

  diff := NormalizedAngleDiff(nsa, nea);
  if diff = 0 then
    diff := 2.0 * Pi;

  SetLength(Result, Sqr(rEndInt * 2 + 1) * Ceil(diff / (2.0 * Pi)));

  cnt := 0;
  for oy := -rEndInt to rEndInt do
    for ox := -rEndInt to rEndInt do
    begin
      r := Sqrt(sqr(oy) + sqr(ox)) * PxCountDiv;

      if InRange(r, StartRadius, EndRadius) then
      begin
        t := ArcTan2(oy, ox);
        if InNormalizedAngle(t, nsa, nea) then
        begin
          Assert(cnt < Length(Result));

          Result[cnt].Radius := r;
          Result[cnt].Angle := t;
          Inc(cnt);
        end;
      end;
    end;

  SetLength(Result, cnt);
  QuickSort(Result[0], 0, cnt - 1, SizeOf(Result[0]), @CompareRadiusAngle);
end;

function OffsetRadiusAngleLUTAngle(const LUT: TRadiusAngleDynArray; AngleOffset: Double): TSinCosDDynArray;
var
  iLUT: Integer;
begin
  SetLength(Result, Length(LUT));

  for iLUT := 0 to High(LUT) do
    if (iLUT = 0) or (LUT[iLUT].Angle <> LUT[iLUT - 1].Angle) then
      SinCos(LUT[iLUT].Angle + AngleOffset, Result[iLUT].Sin, Result[iLUT].Cos)
    else
      Result[iLUT] := Result[iLUT - 1];
end;

function CutoffToFeedbackRatio(Cutoff: Double; SampleRate: Integer): Double;
begin
  Result := (Cutoff * 2.0 / SampleRate) / sqrt(0.1024 + sqr(Cutoff * 2.0 / SampleRate));
end;

procedure IncrementalSinCos(Angle: Double; var PrevAngle: Double; var ASinCos: TSinCosD);
begin
  if Angle <> PrevAngle then
  begin
    SinCos(Angle, ASinCos.Sin, ASinCos.Cos);
    PrevAngle := Angle;
  end;
end;

procedure CreateWAV(channels: word; resolution: word; rate: longint; fn: string; const data: TSmallIntDynArray);
type
  { format of WAV file header }
  TWavHeader = record         { parameter description }
    rId             : longint; { 'RIFF'  4 characters }
    rLen            : longint; { length of DATA + FORMAT chunk }
    { FORMAT CHUNK }
    wId             : longint; { 'WAVE' }
    fId             : longint; { 'fmt ' }
    fLen            : longint; { length of FORMAT DATA = 16 }
    { format data }
    wFormatTag      : word;    { $01 = PCM }
    nChannels       : word;    { 1 = mono, 2 = stereo }
    nSamplesPerSec  : longint; { Sample frequency ie 11025}
    nAvgBytesPerSec : longint; { = nChannels * nSamplesPerSec *
                                 (nBitsPerSample/8) }
    nBlockAlign     : word;    { = nChannels * (nBitsPerSAmple / 8 }
    wBitsPerSample  : word;    { 8 or 16 }
    { DATA CHUNK }
    dId             : longint; { 'data' }
    wSampleLength   : longint; { length of SAMPLE DATA }
      { sample data : offset 44 }
      { for 8 bit mono = s[0],s[1]... :byte}
      { for 8 bit stereo = sleft[0],sright[0],sleft[1],sright[1]... :byte}
      { for 16 bit mono = s[0],s[1]... :word}
      { for 16 bit stereo = sleft[0],sright[0],sleft[1],sright[1]... :word}
  end;

var
  wf : TFileStream;
  wh : TWavHeader;
begin
  wh.rId             := $46464952; { 'RIFF' }
  wh.rLen            := 36 + Length(data) * SizeOf(data[0]); { length of sample + format }
  wh.wId             := $45564157; { 'WAVE' }
  wh.fId             := $20746d66; { 'fmt ' }
  wh.fLen            := 16; { length of format chunk }
  wh.wFormatTag      := 1; { PCM data }
  wh.nChannels       := channels; { mono/stereo }
  wh.nSamplesPerSec  := rate; { sample rate }
  wh.nAvgBytesPerSec := channels*rate*(resolution div 8);
  wh.nBlockAlign     := channels*(resolution div 8);
  wh.wBitsPerSample  := resolution;{ resolution 8/16 }
  wh.dId             := $61746164; { 'data' }
  wh.wSampleLength   := Length(data) * SizeOf(data[0]); { sample size }

  wf := TFileStream.Create(fn, fmCreate or fmShareDenyNone);
  try
    wf.WriteBuffer(wh, SizeOf(wh));
    wf.WriteBuffer(data[0], Length(data) * SizeOf(data[0]));
  finally
    wf.Free;
  end;
end;

procedure CreateWAV(channels: word; resolution: word; rate: longint; fn: string; const data: TDoubleDynArray);
var
  i: Integer;
  idata: TSmallIntDynArray;
begin
  SetLength(idata, Length(data));

  for i := 0 to High(idata) do
    idata[i] := Make16BitSample(data[i]);

  CreateWAV(channels, resolution, rate, fn, idata);
end;

function GetMono(const ASample: TPointD): Double;
begin
  Result := (ASample.X + ASample.Y) * 0.5;
end;

function AdjustSample(const sample, meanSD: TPointD): TPointD;
begin
  Result.X := (sample.X - meanSD.X) * meanSD.Y;
  Result.Y := -(sample.Y - meanSD.X) * meanSD.Y;
end;

procedure QuickSort(var AData;AFirstItem,ALastItem,AItemSize:Integer;ACompareFunction:TCompareFunction;AUserParameter:Pointer=nil);
var I, J, P: Integer;
    PData,P1,P2: PByte;
    Tmp: array[0..31] of Byte;
begin
  if ALastItem <= AFirstItem then
    Exit;

  Assert(AItemSize < SizeOf(Tmp),'AItemSize too big!');
  PData:=PByte(@AData);
  repeat
    I := AFirstItem;
    J := ALastItem;
    P := (AFirstItem + ALastItem) shr 1;
    repeat
      P1:=PData;Inc(P1,I*AItemSize);
      P2:=PData;Inc(P2,P*AItemSize);
      while ACompareFunction(P1, P2, AUserParameter) < 0 do
      begin
        Inc(I);
        Inc(P1,AItemSize);
      end;
      P1:=PData;Inc(P1,J*AItemSize);
      //P2:=PData;Inc(P2,P*AItemSize); already done
      while ACompareFunction(P1, P2, AUserParameter) > 0 do
      begin
        Dec(J);
        Dec(P1,AItemSize);
      end;
      if I <= J then
      begin
        P1:=PData;Inc(P1,I*AItemSize);
        P2:=PData;Inc(P2,J*AItemSize);
        Move(P2^, Tmp[0], AItemSize);
        Move(P1^, P2^, AItemSize);
        Move(Tmp[0], P1^, AItemSize);

        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if AFirstItem < J then QuickSort(AData,AFirstItem,J,AItemSize,ACompareFunction,AUserParameter);
    AFirstItem := I;
  until I >= ALastItem;
end;

function GetTIFFSize(AStream: TStream; out AWidth, AHeight: DWord; out dpiX, dpiY: Double): Boolean;
type
  TByteOrder = (boLE, boBE);  // little edian, or big endian
  TTifHeader = packed record
     BOM: word;     // 'II' for little endian, 'MM' for big endian
     Sig: word;     // Signature (42)
     IFD: DWORD;    // Offset where image data begin
  end;
  TIFD_Field = packed record
    Tag: word;
    FieldType: word;
    ValCount: DWord;
    ValOffset: DWord;
  end;

  { Makes sure that the byte order of w is the same as specified by the parameter }
  function FixByteOrder(w: Word; AByteOrder: TByteOrder): Word; overload;
  begin
    Result := IfThen(AByteOrder = boLE, LEToN(w), BEToN(w));
  end;

  { Makes sure that the byte order of dw is the same as specified by the parameter }
  function FixByteOrder(dw: DWord; AByteOrder: TByteOrder): DWord; overload;
  begin
    Result := IfThen(AByteOrder = boLE, LEToN(dw), BEToN(dw));
  end;

var
  header: TTifHeader = (BOM:0; Sig:0; IFD:0);
  dirEntries: Word;
  field: TIFD_Field = (Tag:0; FieldType:0; ValCount:0; ValOffset:0);
  i: Integer;
  bo: TByteOrder;
  num, denom: LongInt;
  units: Word;
  p, pStart: Int64;
begin
  Result := false;
  AWidth := 0;
  AHeight := 0;
  dpiX := 0;
  dpiY := 0;
  units := 0;

  // Remember current stream position because procedure is called also from
  // jpeg Exif block.
  pStart := AStream.Position;

  if AStream.Read(header, SizeOf(TTifHeader)) < SizeOf(TTifHeader) then exit;
  if not ((header.BOM = $4949) or (header.BOM = $4D4D)) then exit;
  if header.BOM = $4949 then bo := boLE else bo := boBE; // 'II' --> little endian, 'MM' --> big endian
  if FixByteOrder(header.Sig, bo) <> 42 then exit;

  AStream.Position := pStart + FixByteOrder(header.IFD, bo);
  dirEntries := FixByteOrder(AStream.ReadWord, bo);
  for i := 1 to dirEntries do
  begin
    AStream.Read(field, SizeOf(field));
    field.Tag := FixByteOrder(field.Tag, bo);
    field.ValOffset := FixByteOrder(field.ValOffset, bo);
    field.FieldType := FixByteOrder(field.FieldType, bo);
    p := AStream.Position;
    case field.Tag OF
      $0100 : AWidth := field.ValOffset;
      $0101 : AHeight := field.ValOffset;
      $011A : begin    // XResolution as RATIONAL value
                AStream.Position := pStart + field.ValOffset;
                num := FixByteOrder(AStream.ReadDWord, bo);
                denom := FixByteOrder(AStream.ReadDWord, bo);
                dpiX := num/denom;
              end;
      $011B : begin    // YResolution as RATIONAL value
                AStream.Position := pStart + field.ValOffset;
                num := FixByteOrder(AStream.ReadDWord, bo);
                denom := FixByteOrder(AStream.ReadDWord, bo);
                dpiY := num/denom;
              end;
      $0128 : begin
                units := field.ValOffset;   // 1: non-square 2: inches, 3: cm
              end;
    end;
    if (AWidth > 0) and (AHeight > 0) and (dpiX > 0) and (dpiY > 0) and (units > 0)
    then
      break;
    AStream.Position := p;
  end;

  case units of
    1: begin dpiX := 96; dpiY := 96; end;
    2: ;  // is already inches, nothing to do
    3: begin dpiX := dpiX * 2.54; dpiY := dpiY * 2.54; end;
  end;

  Result := true;
end;

function InvariantFormatSettings: TFormatSettings;
begin
  Result := GInvariantFormatSettings;
end;

{ TPointValue }

class function TPointValue.Create(const ax, ay, avalue: Single): TPointValue;
begin
  Result.X := ax;
  Result.Y := ay;
  Result.Value := avalue;
end;

class operator TPointValue. = (const apt1, apt2: TPointValue): Boolean;
begin
  Result := SameValue(apt1.X, apt2.X) and SameValue(apt1.Y, apt2.Y) and SameValue(apt1.Value, apt2.Value);
end;

class operator TPointValue.<>(const apt1, apt2: TPointValue): Boolean;
begin
  Result := not (SameValue(apt1.X, apt2.X) and SameValue(apt1.Y, apt2.Y) and SameValue(apt1.Value, apt2.Value));
end;


initialization
  GetLocaleFormatSettings(LOCALE_INVARIANT, GInvariantFormatSettings);

{$ifdef DEBUG}
  ProcThreadPool.MaxThreadCount := 1;
{$else}
  SetPriorityClass(GetCurrentProcess(), IDLE_PRIORITY_CLASS);
  ProcThreadPool.MaxThreadCount := Max(1, NumberOfProcessors - 2); // let one full core unused
{$endif}
end.


unit utils;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}
{$TYPEDADDRESS ON}
{$CODEALIGN LOCALMIN=16}

interface

uses
  Classes, SysUtils, Types, Windows, MTProcs, Math;

type
  TSpinlock = LongInt;
  PSpinLock = ^TSpinlock;

  TPointD = record
    X, Y: Double;
  end;

  TRectD = record
    L, T, R, B: Double;
  end;

  TMinimizeMethod = (mmNone, mmGradientDescent, mmPowell, mmAll);

  TImageDerivationOperator = (idoPrewitt, idoSobel, idoScharr);
  TConvolutionKernel = array[-1..1, -1..1] of integer;

  TPointDDynArray = array of TPointD;
  TPointDDynArray2 = array of TPointDDynArray;
  TByteDynArray2 = array of TByteDynArray;
  TWordDynArray2 = array of TWordDynArray;
  TSingleDynArray2 = array of TSingleDynArray;
  TDoubleDynArray2 = array of TDoubleDynArray;
  TDoubleDynArray3 = array of TDoubleDynArray2;

  TGRSEvalFunc = function(arg: Double; obj: Pointer): Double of object;
  TGradientEvalFunc = procedure(const arg: TDoubleDynArray; var func: Double; grad: TDoubleDynArray; obj: Pointer) of object;

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


const
  C45RpmRevolutionsPerSecond = 45.0 / 60.0;
  C45RpmOuterSize = 6.875;
  C45RpmInnerSize = 1.504;
  C45RpmLabelOuterSize = 3.5;
  C45RpmConcentricGroove = 3.875;
  C45RpmFirstMusicGroove = 6.625;
  C45RpmLastMusicGroove = 4.25;
  C45RpmLeadInGroovesPerInch = 16.0;
  C45RpmMinGroovesPerInch = 2.0;
  C45RpmMaxGrooveWidth = 0.0084;
  C45RpmAdapterSize = 1.496;

  CLowCutoffFreq = 7.0;

{$if 1}
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

  CImageDerivationKernels: array[TImageDerivationOperator, Boolean {Y?}] of TConvolutionKernel = (
    (((-1, 0, 1), (-1, 0, 1), (-1, 0, 1)),   ((-1, -1, -1), (0, 0, 0), (1, 1, 1))),  // ckoPrewitt
    (((-1, 0, 1), (-2, 0, 2), (-1, 0, 1)),   ((-1, -2, -1), (0, 0, 0), (1, 2, 1))),  // ckoSobel
    (((-3, 0, 3), (-10, 0, 10), (-3, 0, 3)), ((-3, -10, -3), (0, 0, 0), (3, 10, 3))) // ckoScharr
  );

procedure SpinEnter(Lock: PSpinLock); assembler;
procedure SpinLeave(Lock: PSpinLock); assembler;
function NumberOfProcessors: Integer;

procedure Exchange(var a, b: Integer);
function iDiv0(x, y: Integer): Integer;overload;inline;
function iDiv0(x, y: Int64): Int64;overload;inline;
function DivDef(x, y, def: Double): Double;inline;
function NanDef(x, def: Double): Double; inline;

function SwapRB(c: Integer): Integer; inline;
function ToRGB(r, g, b: Byte): Integer; inline;
procedure FromRGB(col: Integer; out r, g, b: Integer); inline; overload;
procedure FromRGB(col: Integer; out r, g, b: Byte); inline; overload;
function ToLuma(r, g, b: Integer): Integer; inline;
function ToBW(col: Integer): Integer;

function lerp(x, y, alpha: Double): Double; inline;
function ilerp(x, y, alpha, maxAlpha: Integer): Integer; inline;
function revlerp(x, r, alpha: Double): Double; inline;
function herp(y0, y1, y2, y3, alpha: Double): Double;

function GoldenRatioSearch(Func: TGRSEvalFunc; MinX, MaxX: Double; ObjectiveY: Double; EpsilonX, EpsilonY: Double; Data: Pointer = nil): Double;
function GradientDescentMinimize(Func: TGradientEvalFunc; var X: TDoubleDynArray; LearningRate: Double = 0.01; Epsilon: Double = 1e-9; Data: Pointer = nil): Double;

function Convolve(const image:TDoubleDynArray2; const kernel: TConvolutionKernel; row, col: Integer): Double; overload;
function Convolve(const image:TWordDynArray2; const kernel: TConvolutionKernel; row, col: Integer): Integer; overload;

function PearsonCorrelation(const x: TDoubleDynArray; const y: TDoubleDynArray): Double;
function MSE(const a: TDoubleDynArray; const b: TDoubleDynArray): Double;
function MSEGradient(const a: TDoubleDynArray; const b: TDoubleDynArray; const gb: TDoubleDynArray): Double;

function Make16BitSample(smp: Double): SmallInt;
function AngleTo02Pi(x: Double): Double;
function In02PiExtentsAngle(x, xmin, xmax: Double): Boolean;

procedure BuildSinCosLUT(APointCount: Integer; var ASinCosLUT: TPointDDynArray; AOriginAngle: Double = 0.0);

procedure CreateWAV(channels: word; resolution: word; rate: longint; fn: string; const data: TSmallIntDynArray); overload;
procedure CreateWAV(channels: word; resolution: word; rate: longint; fn: string; const data: TDoubleDynArray); overload;

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

function GradientDescentMinimize(Func: TGradientEvalFunc; var X: TDoubleDynArray; LearningRate: Double;
  Epsilon: Double; Data: Pointer): Double;
var
  gm: Double;
  grad: TDoubleDynArray;
  i: Integer;
begin
  SetLength(grad, Length(X));

  repeat
    Func(X, Result, grad, Data);

    gm := 0.0;
    for i := 0 to High(X) do
    begin
      X[i] -= LearningRate * grad[i];
      gm := max(gm, grad[i]);
    end;

    //WriteLn(Result:20:9, gm:20:9);
  until gm <= Epsilon;
end;

function Convolve(const image: TDoubleDynArray2; const kernel: TConvolutionKernel; row, col: Integer): Double;
var
  y, x: Integer;
begin
  Result := 0;
  for y := -1 to 1 do
    for x := -1 to 1 do
      Result += image[y + row, x + col] * kernel[y, x];
end;

function Convolve(const image:TWordDynArray2; const kernel: TConvolutionKernel; row, col: Integer): Integer; overload;
var
  y, x: Integer;
begin
  Result := 0;
  for y := -1 to 1 do
    for x := -1 to 1 do
      Result += image[y + row, x + col] * kernel[y, x];
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

function MSE(const a: TDoubleDynArray; const b: TDoubleDynArray): Double;
var
  i, cnt: Integer;
begin
  Assert(Length(a) = Length(b));

  cnt := 0;
  Result := 0;
  for i := 0 to High(a) do
    if not IsNan(a[i]) and not IsNan(b[i]) then
    begin
      Result += Sqr(a[i] - b[i]);
      Inc(cnt);
    end;

  if cnt > 1 then
    Result /= cnt;
end;

function MSEGradient(const a: TDoubleDynArray; const b: TDoubleDynArray; const gb: TDoubleDynArray): Double;
var
  i, cnt: Integer;
begin
  Assert(Length(a) = Length(b));
  Assert(Length(a) = Length(gb));

  cnt := 0;
  Result := 0;
  for i := 0 to High(a) do
    if not IsNan(a[i]) and not IsNan(b[i]) and not IsNan(gb[i]) then
    begin
      Result -= 2.0 *(a[i] - b[i]) * gb[i];
      Inc(cnt);
    end;

  if cnt > 1 then
    Result /= cnt;
end;

function Make16BitSample(smp: Double): SmallInt;
begin
  Result := EnsureRange(round(smp * High(SmallInt)), Low(SmallInt), High(SmallInt));
end;

function AngleTo02Pi(x: Double): Double;
begin
  while x < 0.0 do
    x += 2.0 * Pi;
  while x > 2.0 * Pi do
    x -= 2.0 * Pi;

  Assert(InRange(x, 0, 2.0 * Pi));

  Result := x;
end;

function In02PiExtentsAngle(x, xmin, xmax: Double): Boolean;
begin
  Assert(InRange(x, 0, 2.0 * Pi));
  Assert(InRange(xmin, 0, 2.0 * Pi));
  Assert(InRange(xmax, 0, 2.0 * Pi));
  if xmax >= xmin then
    Result := InRange(x, xmin, xmax)
  else
    Result := InRange(x, 0, xmax) or InRange(x, xmin, 2.0 * Pi);
end;

procedure BuildSinCosLUT(APointCount: Integer; var ASinCosLUT: TPointDDynArray; AOriginAngle: Double = 0.0);
var
  i: Integer;
  rprp: Double;
begin
  SetLength(ASinCosLUT, APointCount);
  rprp := Pi * 2.0 / APointCount;
  for i := 0 to APointCount - 1 do
    SinCos(AOriginAngle + i * rprp, ASinCosLUT[i].Y, ASinCosLUT[i].X);
end;

procedure CreateWAV(channels: word; resolution: word; rate: longint; fn: string; const data: TSmallIntDynArray);
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

initialization
{$ifdef DEBUG}
  ProcThreadPool.MaxThreadCount := 1;
{$else}
  SetPriorityClass(GetCurrentProcess(), IDLE_PRIORITY_CLASS);
  ProcThreadPool.MaxThreadCount := NumberOfProcessors;
{$endif}
end.


unit utils;

{$include 'compileroptions.inc'}
{$R-}

interface

uses
  Classes, SysUtils, Types, Windows, MTProcs, Math, fgl;

type
  TSpinlock = LongInt;
  PSpinLock = ^TSpinlock;

  TPointD = record
    X, Y: Double;
  end;

  TRectD = record
    L, T, R, B: Double;
  end;

  TSinCos = record
    Sin, Cos, Angle: Double;
  end;

  TPointDDynArray = array of TPointD;
  TPointDDynArray2 = array of TPointDDynArray;
  TByteDynArray2 = array of TByteDynArray;
  TWordDynArray2 = array of TWordDynArray;
  TSingleDynArray2 = array of TSingleDynArray;
  TDoubleDynArray2 = array of TDoubleDynArray;
  TDoubleDynArray3 = array of TDoubleDynArray2;
  TSinCosDynArray = array of TSinCos;

  TPointFList = specialize TFPGList<TPointF>;

  THerpCoeff4 = array[0 .. 3] of Integer;
  THerpCoeff44 = array[0 .. 3, 0 .. 3] of Integer;

  TSerpCoeffs9 = array[-4 .. 4 + 3] of Single;
  TSerpCoeffs9ByWord = array[0 .. high(Word)] of TSerpCoeffs9;

  TGRSEvalFunc = function(arg: Double; obj: Pointer): Double of object;
  TGradientEvalFunc = procedure(const arg: TDoubleDynArray; var func: Double; grad: TDoubleDynArray; obj: Pointer) of object;
  TCompareFunction = function(Item1, Item2, UserParameter: Pointer): Integer;

const
  C45RpmRevolutionsPerSecond = 45.0 / 60.0;

  C45RpmLeadInGroovesPerInch = 16.0;
  C45RpmMinGroovesPerInch = 2.0;

  C45RpmRecordingGrooveWidth = 0.0032;
  C45RpmLeadOutGrooveWidth = 0.006;

  C45RpmOuterSize = 6.875;
  C45RpmInnerSize = 1.504;
  C45RpmLabelOuterSize = 3.5;
  C45RpmConcentricGroove = 3.875;
  C45RpmMinConcentricGroove = C45RpmConcentricGroove - 0.078 * 2;
  C45RpmMaxConcentricGroove = C45RpmConcentricGroove;
  C45RpmFirstMusicGroove = 6.625;
  C45RpmLastMusicGroove = 4.25;
  C45RpmAdapterSize = 1.496;

  CLowCutoffFreq = 30.0;

{$if 1}
  cRedMul = 1;
  cGreenMul = 1;
  cBlueMul = 1;
{$else}
  cRedMul = 299;
  cGreenMul = 587;
  cBlueMul = 114;
{$endif}

  cLumaDiv = cRedMul + cGreenMul + cBlueMul;

  cPhi = (1 + sqrt(5)) / 2;
  cInvPhi = 1 / cPhi;

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
function ToBW(col: Integer): Byte;

function lerp(x, y, alpha: Double): Double; overload;
function lerp(x, y: Word; alpha: Double): Double; overload;
function lerp(x, y: Integer; alpha: Double): Double; overload;

procedure serpCoeffsBuilsLUT(var coeffs: TSerpCoeffs9ByWord);
function serpCoeffs(alpha: Double): PSingle;
procedure serpFromCoeffsXY(coeffs: PSingle; centerPx: PWORD; stride: Integer; res: PSingle);
function serpFromCoeffs(coeffs, data: PSingle): Single;

function serp(ym4, ym3, ym2, ym1, ycc, yp1, yp2, yp3, yp4, alpha: Double): Double;

function GoldenRatioSearch(Func: TGRSEvalFunc; MinX, MaxX: Double; ObjectiveY: Double; EpsilonX, EpsilonY: Double; Data: Pointer = nil): Double;

function MSE(const a: TDoubleDynArray; const b: TDoubleDynArray): Double; overload;
function MSE(const a: TWordDynArray; const b: TWordDynArray): Double; overload;
function PearsonCorrelation(const a: TDoubleDynArray; const b: TDoubleDynArray): Double;
function SpearmanRankCorrelation(const a: TDoubleDynArray; const b: TDoubleDynArray): Double;

function Make16BitSample(smp: Double): SmallInt;
function NormalizeAngle(x: Double): Double;
function InNormalizedAngle(x, xmin, xmax: Double): Boolean;

procedure BuildSinCosLUT(APointCount: Integer; var ASinCosLUT: TSinCosDynArray; AOriginAngle: Double = 0.0; AExtentsAngle: Double = 2.0 * Pi);
function CutoffToFeedbackRatio(Cutoff: Double; SampleRate: Integer): Double;

procedure CreateWAV(channels: word; resolution: word; rate: longint; fn: string; const data: TSmallIntDynArray); overload;
procedure CreateWAV(channels: word; resolution: word; rate: longint; fn: string; const data: TDoubleDynArray); overload;

procedure QuickSort(var AData;AFirstItem,ALastItem,AItemSize:Integer;ACompareFunction:TCompareFunction;AUserParameter:Pointer=nil);

function GetTIFFSize(AStream: TStream; out AWidth, AHeight: DWord; out dpiX, dpiY: Double): Boolean;

implementation
var GSerpCoeffs9ByWord: TSerpCoeffs9ByWord;

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

function ToBW(col: Integer): Byte;
var
  r, g, b: Byte;
begin
  FromRGB(col, r, g, b);
  Result := ToLuma(r, g, b) div cLumaDiv;
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

function Sinc(x: Double): Double; inline;
begin
  Result := DivDef(Sin(x), x, 1.0);
end;

procedure serpCoeffsBuilsLUT(var coeffs: TSerpCoeffs9ByWord);
var
  w, i: Integer;
  alpha: Double;
begin
  for w := 0 to High(Word) do
  begin
    alpha := w * (1 / High(Word));
    for i := Low(TSerpCoeffs9) to High(TSerpCoeffs9) do
      coeffs[w, i] :=Sinc((alpha - i) * Pi);
  end;
end;

function serpCoeffs(alpha: Double): PSingle;
begin
  Result := @GSerpCoeffs9ByWord[round(alpha * High(word)), 0];
end;

{$ifdef CPUX64}

function serpFromCoeffsX_asm(img_rcx: PWORD): Single; register; assembler;
asm
  movdqu xmm11, [rcx - 4 * 2]
  movdqa xmm0, xmm11

  punpckhwd xmm11, xmm12
  punpcklwd xmm0, xmm12

  cvtdq2ps xmm11, xmm11
  cvtdq2ps xmm0, xmm0

  mulps xmm11, xmm14
  mulps xmm0, xmm13

  addps xmm0, xmm11

  haddps xmm0, xmm0
  haddps xmm0, xmm0

  movzx r15d, word ptr [rcx + 4 * 2]
  cvtsi2ss xmm11, r15d
  mulss xmm11, xmm15
  addss xmm0, xmm11
end;

{$else}

function serpFromCoeffsX(coeffs: PSingle; img: PWORD): Single;
begin
  Result := img[-4] * coeffs[-4];
  Result += img[-3] * coeffs[-3];
  Result += img[-2] * coeffs[-2];
  Result += img[-1] * coeffs[-1];
  Result += img[ 0] * coeffs[ 0];
  Result += img[ 1] * coeffs[ 1];
  Result += img[ 2] * coeffs[ 2];
  Result += img[ 3] * coeffs[ 3];
  Result += img[ 4] * coeffs[ 4];
end;

{$endif}

{$ifdef CPUX64}

procedure serpFromCoeffsXY(coeffs: PSingle; centerPx: PWORD; stride: Integer; res: PSingle); register; assembler;
asm
  sub rsp, 16 * 5
  movdqa oword ptr [rsp], xmm11
  movdqa oword ptr [rsp + $10], xmm12
  movdqa oword ptr [rsp + $20], xmm13
  movdqa oword ptr [rsp + $30], xmm14
  movdqa oword ptr [rsp + $40], xmm15

  pxor xmm12, xmm12
  movdqu xmm13, [rcx - 4 * 4]
  movdqu xmm14, [rcx]
  movss xmm15, [rcx + 4 * 4]

  push rax
  push rcx
  push r15

  mov rax, r8
  shl rax, 2

  lea rcx, [rdx + rax * 2]
  call serpFromCoeffsX_asm
  movss [r9 + 4 * 4], xmm0

  sub rax, r8

  lea rcx, [rdx + rax * 2]
  call serpFromCoeffsX_asm
  movss [r9 + 3 * 4], xmm0

  sub rax, r8

  lea rcx, [rdx + rax * 2]
  call serpFromCoeffsX_asm
  movss [r9 + 2 * 4], xmm0

  sub rax, r8

  lea rcx, [rdx + rax * 2]
  call serpFromCoeffsX_asm
  movss [r9 + 1 * 4], xmm0

  sub rax, r8

  lea rcx, [rdx + rax * 2]
  call serpFromCoeffsX_asm
  movss [r9 + 0 * 4], xmm0

  sub rax, r8

  lea rcx, [rdx + rax * 2]
  call serpFromCoeffsX_asm
  movss [r9 + -1 * 4], xmm0

  sub rax, r8

  lea rcx, [rdx + rax * 2]
  call serpFromCoeffsX_asm
  movss [r9 + -2 * 4], xmm0

  sub rax, r8

  lea rcx, [rdx + rax * 2]
  call serpFromCoeffsX_asm
  movss [r9 + -3 * 4], xmm0

  sub rax, r8

  lea rcx, [rdx + rax * 2]
  call serpFromCoeffsX_asm
  movss [r9 + -4 * 4], xmm0

  pop r15
  pop rcx
  pop rax

  movdqa xmm11, oword ptr [rsp]
  movdqa xmm12, oword ptr [rsp + $10]
  movdqa xmm13, oword ptr [rsp + $20]
  movdqa xmm14, oword ptr [rsp + $30]
  movdqa xmm15, oword ptr [rsp + $40]
  add rsp, 16 * 5
end;
{$else}
procedure serpFromCoeffsXY(coeffs: PSingle; centerPx: PWORD; stride: Integer; res: PSingle);
var
  pp: PWORD;
begin
  pp := @centerPx[4 * stride]; res[+4] := serpFromCoeffsX(coeffs, pp);
  Dec(pp, stride);             res[+3] := serpFromCoeffsX(coeffs, pp);
  Dec(pp, stride);             res[+2] := serpFromCoeffsX(coeffs, pp);
  Dec(pp, stride);             res[+1] := serpFromCoeffsX(coeffs, pp);
  Dec(pp, stride);             res[+0] := serpFromCoeffsX(coeffs, pp);
  Dec(pp, stride);             res[-1] := serpFromCoeffsX(coeffs, pp);
  Dec(pp, stride);             res[-2] := serpFromCoeffsX(coeffs, pp);
  Dec(pp, stride);             res[-3] := serpFromCoeffsX(coeffs, pp);
  Dec(pp, stride);             res[-4] := serpFromCoeffsX(coeffs, pp);
end;
{$endif}

{$ifdef CPUX64}

function serpFromCoeffs(coeffs, data: PSingle): Single; register; assembler;
asm
  sub rsp, 16 * 1
  movdqa oword ptr [rsp], xmm1

  movaps xmm0, [rcx - 4 * 4]
  movaps xmm1, [rcx]

  mulps xmm0, [rdx - 4 * 4]
  mulps xmm1, [rdx]

  addps xmm0, xmm1

  movss xmm1, [rcx + 4 * 4]

  haddps xmm0, xmm0
  haddps xmm0, xmm0

  mulss xmm1, [rdx + 4 * 4]

  addss xmm0, xmm1

  movdqa xmm1, oword ptr [rsp]
  add rsp, 16 * 1
end;

{$else}

function serpFromCoeffs(coeffs, data: PSingle): Single;
begin
  Result := data[-4] * coeffs[-4];
  Result += data[-3] * coeffs[-3];
  Result += data[-2] * coeffs[-2];
  Result += data[-1] * coeffs[-1];
  Result += data[ 0] * coeffs[ 0];
  Result += data[ 1] * coeffs[ 1];
  Result += data[ 2] * coeffs[ 2];
  Result += data[ 3] * coeffs[ 3];
  Result += data[ 4] * coeffs[ 4];
end;

{$endif}

function serp(ym4, ym3, ym2, ym1, ycc, yp1, yp2, yp3, yp4, alpha: Double): Double;
begin
  alpha *= Pi;

  Result := ym4 * Sinc(alpha + 4.0 * Pi);
  Result += ym3 * Sinc(alpha + 3.0 * Pi);
  Result += ym2 * Sinc(alpha + 2.0 * Pi);
  Result += ym1 * Sinc(alpha + 1.0 * Pi);
  Result += ycc * Sinc(alpha);
  Result += yp1 * Sinc(alpha - 1.0 * Pi);
  Result += yp2 * Sinc(alpha - 2.0 * Pi);
  Result += yp3 * Sinc(alpha - 3.0 * Pi);
  Result += yp4 * Sinc(alpha - 4.0 * Pi);
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

function SpearmanRankCorrelation(const a: TDoubleDynArray; const b: TDoubleDynArray): Double;
begin
  Assert(Length(a) = Length(b));

  Result := alglib_SpearmanRankCorrelation(@a[0], @b[0], Length(a));
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
  if xmax >= xmin then
    Result := InRange(x, xmin, xmax)
  else
    Result := InRange(x, -Pi, xmax) or InRange(x, xmin, Pi);
end;

procedure BuildSinCosLUT(APointCount: Integer; var ASinCosLUT: TSinCosDynArray; AOriginAngle: Double; AExtentsAngle: Double);
var
  i: Integer;
  rprp: Double;
begin
  SetLength(ASinCosLUT, APointCount);
  rprp := AExtentsAngle / APointCount;
  for i := 0 to APointCount - 1 do
  begin
    ASinCosLUT[i].Angle := NormalizeAngle(AOriginAngle + i * rprp);
    SinCos(ASinCosLUT[i].Angle, ASinCosLUT[i].Sin, ASinCosLUT[i].Cos);
  end;
end;

function CutoffToFeedbackRatio(Cutoff: Double; SampleRate: Integer): Double;
begin
  Result := (Cutoff * 2.0 / SampleRate) / sqrt(0.1024 + sqr(Cutoff * 2.0 / SampleRate));
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

procedure QuickSort(var AData;AFirstItem,ALastItem,AItemSize:Integer;ACompareFunction:TCompareFunction;AUserParameter:Pointer=nil);
var I, J, P: Integer;
    PData,P1,P2: PByte;
    Tmp: array[0..4095] of Byte;
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
      while (I < P) and (ACompareFunction(P1, P2, AUserParameter) < 0) do
      begin
        Inc(I);
        Inc(P1,AItemSize);
      end;
      P1:=PData;Inc(P1,J*AItemSize);
      //P2:=PData;Inc(P2,P*AItemSize); already done
      while (J > P) and (ACompareFunction(P1, P2, AUserParameter) > 0) do
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

initialization
{$ifdef DEBUG}
  ProcThreadPool.MaxThreadCount := 1;
{$else}
  SetPriorityClass(GetCurrentProcess(), IDLE_PRIORITY_CLASS);
  ProcThreadPool.MaxThreadCount := Max(1, NumberOfProcessors - 1);
{$endif}

  serpCoeffsBuilsLUT(GSerpCoeffs9ByWord);
end.


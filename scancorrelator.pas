unit scancorrelator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, PNGComn, ZStream, MTProcs, TypInfo,
  utils, inputscan, powell, hackedwritepng;

type

  { TScanCorrelator }

  TScanCorrelator = class
  private
    FInputScans: TInputScanDynArray;
    FMethod: TMinimizeMethod;
    FOutputPNGFileName: String;
    FOutputDPI: Integer;
    FObjective: Double;

    FPerSnanCrops: TDoubleDynArray2;
    FPerSnanAngles: array of Double;
    FPerAngleX: TDoubleDynArray2;

    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;

    FOutputImage: TWordDynArray2;

    procedure GradientAnalyze(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
    function PowellAnalyze(const arg: TVector; obj: Pointer): TScalar;
    procedure GradientCorrect(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
    function PowellCorrect(const arg: TVector; obj: Pointer): TScalar;
    function PowellCrop(const x: TVector; obj: Pointer): TScalar;

    procedure AngleInit;
    function Analyze(AMethod: TMinimizeMethod): Double;
    procedure Correct;
    procedure Crop;
    procedure Rebuild;
  public
    constructor Create(const AFileNames: TStrings; AOutputDPI: Integer = 2400);
    destructor Destroy; override;

    procedure LoadPNGs;
    procedure Process;
    procedure Save;

    property OutputPNGFileName: String read FOutputPNGFileName write FOutputPNGFileName;
    property Method: TMinimizeMethod read FMethod write FMethod;

    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;
    property Objective: Double read FObjective;

    property InputScans: TInputScanDynArray read FInputScans;
    property OutputImage: TWordDynArray2 read FOutputImage;
  end;

  { TDPIAwareWriterPNG }

  TDPIAwareWriterPNG = class(THackedWriterPNG)
  private
    FDPI: TPoint;
  protected
    procedure WritePHYS; virtual;
    procedure InternalWrite (Str:TStream; Img:TFPCustomImage); override;
  public
    constructor Create; override;

    property DPI: TPoint read FDPI write FDPI;
  end;

implementation
uses main;

{ TScanCorrelator }

const
  CAreaBegin = C45RpmInnerSize;
  CAreaEnd = C45RpmLabelOuterSize;
  CAreaWidth = (CAreaEnd - CAreaBegin) * 0.5;
  CAreaGroovesPerInchAnalyze = 32;
  CAreaGroovesPerInchCrop = 16;

  CCorrectAngleCount = 18;
  CCorrectPrecMul = 10;
  CCorrectAreaBegin = C45RpmLabelOuterSize;
  CCorrectAreaEnd = C45RpmLastMusicGroove;
  CCorrectArea2Begin = C45RpmFirstMusicGroove;
  CCorrectArea2End = C45RpmOuterSize;
  CCorrectAreaWidth = (CCorrectAreaEnd - CCorrectAreaBegin) * 0.5 + (CCorrectArea2End - CCorrectArea2Begin) * 0.5;

constructor TScanCorrelator.Create(const AFileNames: TStrings; AOutputDPI: Integer);
var
  i: Integer;
begin
  FOutputDPI := AOutputDPI;
  FObjective := NaN;
  FMethod := mmGradientDescent;
  SetLength(FInputScans, AFileNames.Count);
  SetLength(FPerSnanAngles, Length(FInputScans));
  SetLength(FPerSnanCrops, Length(FInputScans), 4);

  for i := 0 to AFileNames.Count - 1 do
  begin
    FInputScans[i] := TInputScan.Create(Ceil(Pi * C45RpmLastMusicGroove * FOutputDPI), AOutputDPI, True);
    FInputScans[i].PNGFileName := AFileNames[i];
  end;
end;

destructor TScanCorrelator.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FInputScans) do
    FInputScans[i].Free;

  inherited Destroy;
end;

procedure TScanCorrelator.LoadPNGs;

  procedure DoOne(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    FInputScans[AIndex].Run;
    WriteLn(FInputScans[AIndex].PNGFileName);
  end;

var i: Integer;
begin
  WriteLn('LoadPNGs');

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, high(FInputScans));

  if Length(FInputScans) > 0 then
  begin
    FOutputDPI := FInputScans[0].DPI;
    for i := 1 to High(FInputScans) do
      Assert(FInputScans[i].DPI = FOutputDPI, 'InputScans mixed DPIs!');
  end;

  FPointsPerRevolution := Ceil(Pi * CAreaEnd * FOutputDPI);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;

  WriteLn('DPI:', FOutputDPI:6);
  WriteLn('PointsPerRevolution:', FPointsPerRevolution:8);
  Writeln('Inner raw sample rate: ', Round(Pi * C45RpmLastMusicGroove * FOutputDPI * C45RpmRevolutionsPerSecond), ' Hz');

  SetLength(FOutputImage, Ceil(C45RpmOuterSize * FOutputDPI), Ceil(C45RpmOuterSize * FOutputDPI));
end;

procedure TScanCorrelator.AngleInit;
const
  CAngleCount = 180;
var
  rBeg, rEnd: Integer;
  base: TDoubleDynArray;

  function DoAngle(iScan: Integer; a: Double; var arr: TDoubleDynArray): Integer;
  var
    iRadius, iAngle: Integer;
    sn, cs, cy, cx, px, py: Double;
    scn: TInputScan;
  begin
    Result := 0;
    scn := FInputScans[iScan];
    cx := scn.Center.X;
    cy := scn.Center.Y;

    for iAngle := 0 to CAngleCount - 1 do
    begin
      SinCos(a + DegToRad(iAngle * (180 / CAngleCount)), sn, cs);
      for iRadius:= rBeg to rEnd do
      begin
        px := cx + cs * iRadius;
        py := cy + sn * iRadius;
        if scn.InRangePointD(py, px) then
          arr[Result + 0] := scn.GetPointD(py, px, isImage);

        px := cx - cs * iRadius;
        py := cy - sn * iRadius;
        if scn.InRangePointD(py, px) then
          arr[Result + 1] := scn.GetPointD(py, px, isImage);

        Inc(Result, 2);
      end;
    end;
  end;

  procedure DoScan(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iAngle: Integer;
    a, r, bestr, bestAngle: Double;
    angle, gint: TDoubleDynArray;
  begin
    if not InRange(AIndex, 1, High(FInputScans)) then
      Exit;

    SetLength(angle, Length(base));

    bestr := Infinity;
    bestAngle := 0.0;

    for iAngle := 0 to 359 do
    begin
      a := DegToRad(iAngle);

      DoAngle(AIndex, a, angle);

      r := MSE(base, angle, gint);

      if r <= bestr then
      begin
        bestr := r;
        bestAngle := a;
      end;
    end;

    FPerSnanAngles[AIndex] := bestAngle;
  end;

var
  pos: Integer;
begin
  WriteLn('AngleInit');

  if Length(FInputScans) <= 0 then
    Exit;

  SetLength(base, FInputScans[0].Width * CAngleCount);

  rBeg := Round(C45RpmInnerSize * 0.5 * FOutputDPI);
  rEnd := Round(C45RpmLabelOuterSize * 0.5 * FOutputDPI);

  pos := DoAngle(0, 0, base);

  SetLength(base, pos);

  ProcThreadPool.DoParallelLocalProc(@DoScan, 1, High(FInputScans));
end;

function TScanCorrelator.PowellAnalyze(const arg: TVector; obj: Pointer): TScalar;
begin
  GradientAnalyze(arg, Result, nil, obj);
end;

procedure TScanCorrelator.GradientAnalyze(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  gradData: TDoubleDynArray2;
  imgData: TDoubleDynArray2;
  imgResults: TDoubleDynArray;
  paramCount: Integer;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    i, pos: Integer;
    ri, t, r, px, py, cx, cy, rri, sn, cs, p, gimgx, gimgy, gr, gt, gcx, gcy: Double;
    sinCosLUT: TPointDDynArray;
  begin
    if not InRange(AIndex, 0, High(FInputScans)) then
      Exit;

    if AIndex > 0 then
    begin
      t   := arg[High(FInputScans) * 0 + AIndex - 1];
      cx  := arg[High(FInputScans) * 1 + AIndex - 1];
      cy  := arg[High(FInputScans) * 2 + AIndex - 1];
    end
    else
    begin
      t   := FPerSnanAngles[AIndex];
      cx  := FInputScans[AIndex].Center.X;
      cy  := FInputScans[AIndex].Center.Y;
    end;

    BuildSinCosLUT(FPointsPerRevolution, sinCosLUT, t);

    ri := FOutputDPI / (CAreaGroovesPerInchAnalyze * (FPointsPerRevolution - 1));

    r := CAreaBegin * 0.5 * FOutputDPI;
    pos := 0;
    for i := 0 to High(imgData[0]) do
    begin
      cs := sinCosLUT[pos].X;
      sn := sinCosLUT[pos].Y;

      rri := r + ri * i;

      px := cs * rri + cx;
      py := sn * rri + cy;

      if FInputScans[AIndex].InRangePointD(py, px) then
      begin
        p := FInputScans[AIndex].GetPointD(py, px, isImage);

        imgData[AIndex, i] := p;

        if (AIndex > 0) and Assigned(grad) then
        begin
          gimgx := FInputScans[AIndex].GetPointD(py, px, isXGradient);
          gimgy := FInputScans[AIndex].GetPointD(py, px, isYGradient);

          gr := rri;

          gt := (gimgx * -sn + gimgy * cs) * gr;
          gcx := gimgx;
          gcy := gimgy;

          gradData[High(FInputScans) * 0 + AIndex - 1, i] := gt;
          gradData[High(FInputScans) * 1 + AIndex - 1, i] := gcx;
          gradData[High(FInputScans) * 2 + AIndex - 1, i] := gcy;
        end;
      end;

      Inc(pos);

      if pos >= FPointsPerRevolution then
        pos := 0;
    end;
  end;

  procedure DoMSE(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iArg: Integer;
    gint: TDoubleDynArray;
  begin
    if not InRange(AIndex, 0, High(FInputScans) - 1) then
      Exit;

    imgResults[AIndex] := MSE(imgData[0], imgData[AIndex + 1], gint);

    for iArg := 0 to paramCount - 1 do
      grad[High(FInputScans) * iArg + AIndex + 1 - 1] := MSEGradient(gint, gradData[High(FInputScans) * iArg + AIndex + 1 - 1]);
  end;

var
  cnt: Integer;
begin
  paramCount := iDiv0(Length(grad), High(FInputScans));

  cnt := Ceil(CAreaWidth * CAreaGroovesPerInchAnalyze * FPointsPerRevolution);
  SetLength(imgResults, High(FInputScans));
  SetLength(imgData, Length(FInputScans), cnt);
  SetLength(gradData, Length(grad), cnt);

  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, High(FInputScans));
  ProcThreadPool.DoParallelLocalProc(@DoMSE, 0, High(FInputScans) - 1);

  func := Sum(imgResults);

  Write('RMSE: ', Sqrt(DivDef(func, Length(imgResults), 1.0)):12:9,#13);
end;

function TScanCorrelator.Analyze(AMethod: TMinimizeMethod): Double;
var
  x: TVector;
  i: Integer;
  p: TPointD;
begin
  Result := NaN;

  WriteLn('Analyze ', GetEnumName(TypeInfo(TMinimizeMethod), Ord(AMethod)));

  if Length(FInputScans) <= 0 then
    Exit;

  if Length(FInputScans) <= 1 then
    AMethod := mmNone;

  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].PNGShortName, ', Angle: ', RadToDeg(FPerSnanAngles[i]):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ' (before)');

  SetLength(x, High(FInputScans) * 3);
  for i := 1 to High(FInputScans) do
  begin
    x[High(FInputScans) * 0 + i - 1] := FPerSnanAngles[i];
    x[High(FInputScans) * 1 + i - 1] := FInputScans[i].Center.X;
    x[High(FInputScans) * 2 + i - 1] := FInputScans[i].Center.Y;
  end;

  case AMethod of
    mmNone:
    begin
      Result := PowellAnalyze(x, Self);
    end;
    mmBFGS:
    begin
      Result := BFGSMinimize(@GradientAnalyze, x);
    end;
    mmGradientDescent:
    begin
      Result := GradientDescentMinimize(@GradientAnalyze, x, 0.1, 1e-7);
    end;
    mmPowell:
    begin
      Result := PowellMinimize(@PowellAnalyze, x, 1e-8, 1e-9, 1e-9, MaxInt, nil)[0];
    end;
  end;

  Assert(not IsNan(Result));

  FPerSnanAngles[0] := 0.0;

  for i := 1 to High(FInputScans) do
  begin
    FPerSnanAngles[i] := x[High(FInputScans) * 0 + i - 1];
    p.X := x[High(FInputScans) * 1 + i - 1];
    p.Y := x[High(FInputScans) * 2 + i - 1];
    FInputScans[i].Center := p;
  end;

  WriteLn;
  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].PNGShortName, ', Angle: ', RadToDeg(FPerSnanAngles[i]):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ' (after)');
end;

function TScanCorrelator.PowellCrop(const x: TVector; obj: Pointer): TScalar;
var
  inputIdx: PtrInt absolute obj;
  rBeg, rEnd, a0a, a1a, a0b, a1b, cx, cy, t, ri, rri, sn, cs, bt, px, py, p: Double;
  pos, arrPos: Integer;
  stdDevArr: TDoubleDynArray;
begin
  Result := 1000.0;

  if not InRange(AngleTo02Pi(x[1] - x[0]), DegToRad(0.0), DegToRad(120.0)) then
    Exit;

  a0a := AngleTo02Pi(x[0]);
  a0b := AngleTo02Pi(x[1]);
  a1a := AngleTo02Pi(x[0] + Pi);
  a1b := AngleTo02Pi(x[1] + Pi);

  rBeg := C45RpmLastMusicGroove * 0.5 * FOutputDPI;
  rEnd := C45RpmFirstMusicGroove * 0.5 * FOutputDPI;

  t := FPerSnanAngles[inputIdx];
  cx := FInputScans[inputIdx].Center.X;
  cy := FInputScans[inputIdx].Center.Y;

  SetLength(stdDevArr, Ceil((rEnd - rBeg) / FOutputDPI * CAreaGroovesPerInchCrop) * FPointsPerRevolution);

  ri := (rEnd - rBeg) / (CAreaGroovesPerInchCrop * (FPointsPerRevolution - 1));

  pos := 0;
  arrPos := 0;
  repeat
    bt := AngleTo02Pi(FRadiansPerRevolutionPoint * pos + t);

    SinCos(bt, sn, cs);

    rri := rBeg + ri * pos;

    px := cs * rri + cx;
    py := sn * rri + cy;

    Assert(pos < Length(stdDevArr));

    if FInputScans[inputIdx].InRangePointD(py, px) and
        not In02PiExtentsAngle(bt, a0a, a0b) and not In02PiExtentsAngle(bt, a1a, a1b) then
    begin
      p := FInputScans[inputIdx].GetPointD(py, px, isImage);
      stdDevArr[arrPos] := p;
      Inc(arrPos);
    end;

    Inc(pos);
  until rri >= rEnd;

  if arrPos > 0 then
    Result := -StdDev(PDouble(@stdDevArr[0]), arrPos);

  Write(FInputScans[inputIdx].PNGShortName, ', begin: ', RadToDeg(a0a):9:3, ', end: ', RadToDeg(a0b):9:3, ', obj: ', -Result:12:6, #13);
end;

procedure TScanCorrelator.GradientCorrect(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  angleIdx: PtrInt absolute obj;
  imgData: TDoubleDynArray2;
  gradData: TDoubleDynArray2;
  i, pos, cnt, iX, iY: Integer;
  t, ti, ri, bt, r, rEnd, rMid, rMid2, sn, cs, px, py, cx, cy, skc, skm, rsk, gimgx, gimgy, angle, startAngle, endAngle, angleInc, angleExtents: Double;
  gint: TDoubleDynArray;
begin
  angle := (angleIdx / CCorrectAngleCount) * 2.0 * Pi;
  angleExtents := (2.0 * Pi / CCorrectAngleCount) * 0.5;
  startAngle := angle - angleExtents;
  endAngle := angle + angleExtents;
  angleInc := angleExtents / CCorrectPrecMul;

  cnt := Ceil(CCorrectAreaWidth * FOutputDPI + 1) * CCorrectPrecMul * Ceil((endAngle - startAngle + 1) / angleInc);

  SetLength(imgData, Length(FInputScans), cnt);
  SetLength(gradData, Length(grad), cnt);

  for i := 0 to High(FInputScans) do
  begin
    bt := FPerSnanAngles[i];
    cx := FInputScans[i].Center.X;
    cy := FInputScans[i].Center.Y;

    skc := 0.0;
    skm := 1.0;
    if i > 0 then
    begin
      skc := arg[High(FInputScans) * 0 + i - 1];
      skm := arg[High(FInputScans) * 1 + i - 1];
    end;

    pos := 0;
    ri := 1.0 / CCorrectPrecMul;
    r := CCorrectAreaBegin * 0.5 * FOutputDPI;
    rMid := CCorrectAreaEnd * 0.5 * FOutputDPI;
    rMid2 := CCorrectArea2Begin * 0.5 * FOutputDPI;
    rEnd := CCorrectArea2End * 0.5 * FOutputDPI;
    repeat
      rsk := r * skm + skc;

      ti := startAngle;
      repeat
        t := bt + ti;

        SinCos(t, sn, cs);

        px := cs * rsk + cx;
        py := sn * rsk + cy;

        Assert(pos < Length(imgData[i]));

        if FInputScans[i].InRangePointD(py, px) then
        begin
          imgData[i, pos] := FInputScans[i].GetPointD(py, px, isImage);

          if (i > 0) and Assigned(grad) then
          begin
            gimgx := FInputScans[i].GetPointD(py, px, isXGradient);
            gimgy := FInputScans[i].GetPointD(py, px, isYGradient);

            gradData[High(FInputScans) * 0 + i - 1, pos] := gimgx * cs + gimgy * sn;
            gradData[High(FInputScans) * 1 + i - 1, pos] := (gimgx * cs + gimgy * sn) * r;
          end;
        end;

        ti += angleInc;
        Inc(pos);
      until ti >= endAngle;

      r += ri;

      if (r >= rMid) and (r < rMid2) then
        r := rMid2;

    until r >= rEnd;

    SetLength(imgData[i], pos);
    if (i > 0) and Assigned(grad) then
    begin
      SetLength(gradData[High(FInputScans) * 0 + i - 1], pos);
      SetLength(gradData[High(FInputScans) * 1 + i - 1], pos);
    end;
  end;

  func := 0;
  FillQWord(grad[0], Length(grad), 0);
  for iX := 0 to High(FInputScans) do
    for iY := iX + 1 to High(FInputScans) do
    begin
      func += MSE(imgData[iX], imgData[iY], gint);
      if Assigned(grad) then
      begin
        if iY > 0 then
        begin
          grad[High(FInputScans) * 0 + iY - 1] += MSEGradient(gint, gradData[High(FInputScans) * 0 + iY - 1]);
          grad[High(FInputScans) * 1 + iY - 1] += MSEGradient(gint, gradData[High(FInputScans) * 1 + iY - 1]);
        end;
        if iX > 0 then
        begin
          grad[High(FInputScans) * 0 + iX - 1] += MSEGradient(gint, gradData[High(FInputScans) * 0 + iX - 1]);
          grad[High(FInputScans) * 1 + iX - 1] += MSEGradient(gint, gradData[High(FInputScans) * 1 + iX - 1]);
        end;
       end;
    end;
end;

function TScanCorrelator.PowellCorrect(const arg: TVector; obj: Pointer): TScalar;
begin
 GradientCorrect(arg, Result, nil, obj);
end;

procedure TScanCorrelator.Correct;
var
  x: TVector;
  rmses: TDoubleDynArray;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    angleIdx: PtrInt;
    lx: TVector;
    f: Double;
  begin
    angleIdx := AIndex;

    lx := Copy(x);

    //f := 0;
    //f := BFGSMinimize(@GradientCorrect, lx, 1e-9, Pointer(angleIdx));
    //f := ASAMinimize(@GradientCorrect, lx, [0.95,0.95], [1.05, 1.05], 1e-9, Pointer(angleIdx));
    //f := GradientDescentMinimize(@GradientCorrect, lx, 1000.0, 1e-7, Pointer(angleIdx));
    f := PowellMinimize(@PowellCorrect, lx, 1e-3, 1e-9, 0, MaxInt, Pointer(angleIdx))[0];
    //GradientCorrect(lx, f, nil, Pointer(angleIdx));

    f := Sqrt(f / High(FInputScans));

    rmses[AIndex] := f;
    FPerAngleX[AIndex] := Copy(lx);

    Write(AIndex + 1:6,' / ',Length(FPerAngleX):6,', RMSE: ', f:9:6, #13);
  end;

var
  i, j: Integer;
begin
  WriteLn('Correct');

  if Length(FInputScans) <= 1 then
    Exit;

  SetLength(FPerAngleX, CCorrectAngleCount);
  SetLength(rmses, CCorrectAngleCount);

  SetLength(x, High(FInputScans) * 2);
  for i := 1 to High(FInputScans) do
  begin
    x[High(FInputScans) * 0 + i - 1] := 0.0;
    x[High(FInputScans) * 1 + i - 1] := 1.0;
  end;

  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, high(FPerAngleX));

  WriteLn;

  for j := 0 to high(FPerAngleX) do
  begin
    for i := 0 to high(FPerAngleX[0]) do
      Write(FPerAngleX[j,i]:12:6);
    WriteLn;
  end;

  WriteLn('Worst RMSE: ', MaxValue(rmses):9:6);
end;

procedure TScanCorrelator.Crop;
var
  i: PtrInt;
  x: TVector;
begin
  WriteLn('Crop');

  SetLength(x, 2);

  for i := 0 to High(FInputScans) do
  begin
    x[0] := AngleTo02Pi(DegToRad(-30.0));
    x[1] := AngleTo02Pi(DegToRad(30.0));

    PowellMinimize(@PowellCrop, x, 1.0 / 360.0, 1e-6, 1e-6, MaxInt, Pointer(i));

    FPerSnanCrops[i, 0] := AngleTo02Pi(x[0]);
    FPerSnanCrops[i, 1] := AngleTo02Pi(x[1]);
    FPerSnanCrops[i, 2] := AngleTo02Pi(x[0] + Pi);
    FPerSnanCrops[i, 3] := AngleTo02Pi(x[1] + Pi);

    WriteLn;
  end;
end;

procedure TScanCorrelator.Rebuild;
const
  CTauToAngleIdx = CCorrectAngleCount / (2.0 * Pi);
var
  center, rBeg, rEnd, rLmg, rLbl: Double;

  procedure InterpolateX(tau: Double; var x: TVector);
  var
    ci, i, x0, x1, x2, x3, modulo: Integer;
    c, alpha: Double;
    y0, y1, y2, y3: TVector;
  begin
    if Length(FPerAngleX) = 0 then
      Exit;

    c := tau * CTauToAngleIdx;
    ci := Trunc(c);
    alpha := c - ci;

    modulo := Length(FPerAngleX);

    x0 := (ci - 1 + modulo) mod modulo;
    x1 := (ci + 0 + modulo) mod modulo;
    x2 := (ci + 1 + modulo) mod modulo;
    x3 := (ci + 2 + modulo) mod modulo;

    y0 := FPerAngleX[x0];
    y1 := FPerAngleX[x1];
    y2 := FPerAngleX[x2];
    y3 := FPerAngleX[x3];

    for i := 0 to High(x) do
      x[i] := herp(y0[i], y1[i], y2[i], y3[i], alpha);
  end;

  procedure DoY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    x: TVector;
    i, ox, cnt: Integer;
    r, sn, cs, px, py, t, cx, cy, acc, bt, skm, skc, ct, rsk: Double;
  begin
    if High(FInputScans) > 0 then
      SetLength(x, High(FInputScans) * 2);

    for ox := 0 to High(FOutputImage[0]) do
    begin
      r := Sqrt(Sqr(AIndex - center) + Sqr(ox - center));

      if InRange(r, rBeg, rEnd) then
      begin
        bt := AngleTo02Pi(ArcTan2(AIndex - center, ox - center));

        InterpolateX(bt, x);

        cnt := 0;
        acc := 0;
        for i := 0 to High(FInputScans) do
        begin
          t  := FPerSnanAngles[i];
          cx := FInputScans[i].Center.X;
          cy := FInputScans[i].Center.Y;

          skc := 0.0;
          skm := 1.0;
          if i > 0 then
          begin
            skc := x[High(FInputScans) * 0 + i - 1];
            skm := x[High(FInputScans) * 1 + i - 1];
          end;

          ct := AngleTo02Pi(bt + t);
          rsk := r * skm + skc;

          SinCos(ct, sn, cs);
          px := cs * rsk + cx;
          py := sn * rsk + cy;

          if FInputScans[i].InRangePointD(py, px) and
              (not In02PiExtentsAngle(ct, FPerSnanCrops[i, 0], FPerSnanCrops[i, 1]) and
               not In02PiExtentsAngle(ct, FPerSnanCrops[i, 2], FPerSnanCrops[i, 3]) or
               (r < rLbl)) then
          begin
            acc += FInputScans[i].GetPointD(py, px, isImage);
            Inc(cnt);
            Break;
          end;
        end;

        acc := DivDef(acc, cnt, 1.0);

        FOutputImage[AIndex, ox] := EnsureRange(Round(acc * High(Word)), 0, High(Word));
      end
      else
      begin
        FOutputImage[AIndex, ox] := IfThen(r >= rLmg, Round(0.25 * High(Word)), Round(1.0 * High(Word)));
      end;
    end;
  end;

begin
  WriteLn('Rebuild');

  center := Length(FOutputImage) / 2.0;
  rBeg := C45RpmAdapterSize * 0.5 * FOutputDPI;
  rEnd := C45RpmOuterSize * 0.5 * FOutputDPI;
  rLmg := C45RpmLastMusicGroove * 0.5 * FOutputDPI;
  rLbl := C45RpmLabelOuterSize * 0.5 * FOutputDPI;

  ProcThreadPool.DoParallelLocalProc(@DoY, 0, High(FOutputImage));
end;

procedure TScanCorrelator.Save;
var
  i: Integer;
  png: TDPIAwareWriterPNG;
  fs: TFileStream;
  img: TScanImage;
begin
  WriteLn('Save ', FOutputPNGFileName);

  fs := TFileStream.Create(FOutputPNGFileName, fmCreate or fmShareDenyNone);
  img := TScanImage.Create(Length(FOutputImage[0]), Length(FOutputImage));
  png := TDPIAwareWriterPNG.Create;
  try
    img.Image := OutputImage;
    img.UsePalette := True;
    for i := 0 to High(Word) do
      img.Palette.Add(FPColor(i, i, i, High(Word)));

    png.DPI := Point(FOutputDPI, FOutputDPI);
    png.CompressedText := True;
    png.CompressionLevel := clmax;
    png.GrayScale := True;
    png.WordSized := True;
    png.Indexed := False;
    png.UseAlpha := False;

    png.ImageWrite(fs, img);
  finally
    png.Free;
    img.Free;
    fs.Free;
  end;

  WriteLn('Done!');
end;

procedure TScanCorrelator.Process;
var
  prevObj, obj: Double;
  iter: Integer;
begin
  AngleInit;

  if FMethod <> mmAll then
  begin
    FObjective := Analyze(FMethod);
  end
  else
  begin
    obj := 1000.0;
    iter := 1;
    repeat
      WriteLn('Iteration: ', iter:3);

      prevObj := obj;
      Analyze(mmBFGS);
      Analyze(mmGradientDescent);
      obj := Analyze(mmPowell);
      Inc(iter);
    until SameValue(obj, prevObj, 1e-9);
  end;

  Correct;
  Crop;
  Rebuild;
end;

{ TDPIAwareWriterPNG }


procedure TDPIAwareWriterPNG.WritePHYS;
begin
  SetChunkLength(9);
  SetChunkType(ctpHYs);
  PDWord(@ChunkDataBuffer^[0])^ := NtoBE(Cardinal(Round(FDPI.X / 0.0254)));
  PDWord(@ChunkDataBuffer^[4])^ := NtoBE(Cardinal(Round(FDPI.Y / 0.0254)));
  PByte(@ChunkDataBuffer^[8])^ := 1; // 1 means meter
  WriteChunk;
end;

procedure TDPIAwareWriterPNG.InternalWrite(Str: TStream; Img: TFPCustomImage);
begin
  WriteIHDR;
  if Header.colorType = 3 then
    WritePLTE;
  if UsetRNS then
    WritetRNS;
  if (FDPI.X > 0) and (FDPI.Y > 0) then
    WritePHYS;
  WriteIDAT;
  WriteTexts;
  WriteIEND;
end;

constructor TDPIAwareWriterPNG.Create;
begin
 inherited Create;

 FDPI := Point(-1, -1);
end;

end.


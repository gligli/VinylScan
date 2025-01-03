unit scancorrelator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, PNGComn, ZStream, MTProcs, TypInfo,
  utils, inputscan, powell, hackedwritepng;

type
  TCorrectCoords = record
    AngleIdx, ScanIdx: Integer;
  end;

  PCorrectCoords = ^TCorrectCoords;

  { TScanCorrelator }

  TScanCorrelator = class
  private
    FInputScans: TInputScanDynArray;
    FMethod: TMinimizeMethod;
    FCorrectAngles: Boolean;
    FRebuildBlended: Boolean;
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
    property CorrectAngles: Boolean read FCorrectAngles write FCorrectAngles;
    property RebuildBlended: Boolean read FRebuildBlended write FRebuildBlended;

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
  CAnalyzeAreaBegin = C45RpmInnerSize;
  CAnalyzeAreaEnd = C45RpmLabelOuterSize;
  CAnalyzeAreaWidth = (CAnalyzeAreaEnd - CAnalyzeAreaBegin) * 0.5;
  CAnalyzeAreaGroovesPerInch = 32;

  CCorrectAngleCount = 8;
  CCorrectArea1Begin = C45RpmLabelOuterSize - (C45RpmLastMusicGroove - C45RpmLabelOuterSize) * 0.5;
  CCorrectArea1End = C45RpmLastMusicGroove;
  CCorrectArea2Begin = C45RpmFirstMusicGroove - (C45RpmOuterSize - C45RpmFirstMusicGroove) * 0.5;
  CCorrectArea2End = C45RpmOuterSize;
  CCorrectAreaWidth = (CCorrectArea1End - CCorrectArea1Begin) * 0.5 + (CCorrectArea2End - CCorrectArea2Begin) * 0.5;
  CCorrectAreaGroovesPerInch = 300;

  CCropAreaGroovesPerInch = 16;

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
    FInputScans[i] := TInputScan.Create(36000, AOutputDPI, True);
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

function CompareInputScans(Item1, Item2, UserParameter: Pointer): Integer;
var
  s1: ^TInputScan absolute Item1;
  s2: ^TInputScan absolute Item2;
begin
  Result := CompareValue(s2^.CenterQuality, s1^.CenterQuality);
end;

procedure TScanCorrelator.LoadPNGs;

  procedure DoOne(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    FInputScans[AIndex].LoadPNG;
    FInputScans[AIndex].FindTrack;
    WriteLn(FInputScans[AIndex].PNGFileName);
  end;

var
  i: Integer;
begin
  WriteLn('LoadPNGs');

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, high(FInputScans));

  if Length(FInputScans) > 0 then
  begin
    FOutputDPI := FInputScans[0].DPI;
    for i := 1 to High(FInputScans) do
      Assert(FInputScans[i].DPI = FOutputDPI, 'InputScans mixed DPIs!');
  end;

  FPointsPerRevolution := Ceil(Pi * C45RpmOuterSize * FOutputDPI);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;

  WriteLn('DPI:', FOutputDPI:6);
  WriteLn('PointsPerRevolution:', FPointsPerRevolution:8);
  Writeln('Inner raw sample rate: ', Round(Pi * C45RpmLastMusicGroove * FOutputDPI * C45RpmRevolutionsPerSecond), ' Hz');

  if Length(FInputScans) > 1 then
  begin
    QuickSort(FInputScans[0], 0, High(FInputScans), SizeOf(TInputScan), @CompareInputScans);
    Writeln('Best centering: ', FInputScans[0].PNGShortName);
  end;

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
          arr[Result + 0] := scn.GetPointD(py, px, isImage, False);

        px := cx - cs * iRadius;
        py := cy - sn * iRadius;
        if scn.InRangePointD(py, px) then
          arr[Result + 1] := scn.GetPointD(py, px, isImage, False);

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

    ri := FOutputDPI / (CAnalyzeAreaGroovesPerInch * (FPointsPerRevolution - 1));

    r := CAnalyzeAreaBegin * 0.5 * FOutputDPI;
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
        p := FInputScans[AIndex].GetPointD(py, px, isImage, False);

        imgData[AIndex, i] := p;

        if (AIndex > 0) and Assigned(grad) then
        begin
          gimgx := FInputScans[AIndex].GetPointD(py, px, isXGradient, False);
          gimgy := FInputScans[AIndex].GetPointD(py, px, isYGradient, False);

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

  cnt := Ceil(CAnalyzeAreaWidth * CAnalyzeAreaGroovesPerInch * FPointsPerRevolution);
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
      Result := GradientDescentMinimize(@GradientAnalyze, x, 0.01, 1e-7);
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

procedure TScanCorrelator.GradientCorrect(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  coords: PCorrectCoords absolute obj;
  imgData: TDoubleDynArray2;
  gradData: TDoubleDynArray2;
  idata, iscan, ilut, radiusCnt, angleCnt: Integer;
  r, ri, rEnd, rMid, rMid2, sn, cs, px, py, cx, cy, skc, skm, rsk, gimgx, gimgy, ga, gr, angle, startAngle, endAngle, angleInc, angleExtents: Double;
  gint: TDoubleDynArray;
  scanIdx: array[0 .. 1] of Integer;
  sinCosLUT: array[0 .. 1] of TPointDDynArray;
begin
  func := 0.0;
  if Assigned(grad) then
  begin
    grad[0] := 0.0;
    grad[1] := 0.0;
  end;

  skc := arg[0];
  skm := arg[1];

  scanIdx[0] := 0;
  scanIdx[1] := coords^.ScanIdx;

  angle := (coords^.AngleIdx / CCorrectAngleCount) * 2.0 * Pi;
  angleExtents := 2.0 * Pi / CCorrectAngleCount * 0.5;
  startAngle := angle - angleExtents;
  endAngle := angle + angleExtents;
  angleInc := FRadiansPerRevolutionPoint;

  radiusCnt := Ceil(CCorrectAreaWidth * CCorrectAreaGroovesPerInch);
  angleCnt := Ceil((endAngle - startAngle + angleInc) / angleInc);

  SetLength(imgData, 2, angleCnt);
  SetLength(gradData, Length(grad), angleCnt);

  // build sin / cos lookup table

  for idata := 0 to 1 do
  begin
    iscan := scanIdx[idata];
    BuildSinCosLUT(angleCnt, sinCosLUT[idata], startAngle + FPerSnanAngles[iscan], endAngle - startAngle + angleInc);
  end;

  r := CCorrectArea1Begin * 0.5 * FOutputDPI;
  rMid := CCorrectArea1End * 0.5 * FOutputDPI;
  rMid2 := CCorrectArea2Begin * 0.5 * FOutputDPI;
  rEnd := CCorrectArea2End * 0.5 * FOutputDPI;
  ri := FOutputDPI / CCorrectAreaGroovesPerInch;
  repeat
    rsk := r * skm + skc;

    for idata := 0 to 1 do
    begin
      iscan := scanIdx[idata];

      // get params

      cx := FInputScans[iscan].Center.X;
      cy := FInputScans[iscan].Center.Y;

      // parse image arc

      for ilut := 0 to High(sinCosLUT[0]) do
      begin
        cs := sinCosLUT[idata, ilut].X;
        sn := sinCosLUT[idata, ilut].Y;

        if idata > 0 then
        begin
          px := cs * rsk;
          py := sn * rsk;
        end
        else
        begin
          px := cs * r;
          py := sn * r;
        end;

        px += cx;
        py += cy;

        if FInputScans[iscan].InRangePointD(py, px) then
        begin
          imgData[idata, ilut] := FInputScans[iscan].GetPointD(py, px, isImage, True);

          if (idata > 0) and Assigned(grad) then
          begin
            gimgx := FInputScans[iscan].GetPointD(py, px, isXGradient, True);
            gimgy := FInputScans[iscan].GetPointD(py, px, isYGradient, True);

            ga := gimgx * cs + gimgy * sn;
            gr := r;

            gradData[0, ilut] := ga;
            gradData[1, ilut] := ga * gr;
          end;
        end;
      end;
    end;

    // compute MSE and MSE gradients

    func += MSE(imgData[0], imgData[1], gint);
    if Assigned(grad) then
    begin
      grad[0] += MSEGradient(gint, gradData[0]);
      grad[1] += MSEGradient(gint, gradData[1]);
    end;

    // move forward

    r += ri;

    if (r >= rMid) and (r < rMid2) then
      r := rMid2;

  until r >= rEnd;

  func /= radiusCnt;
  if Assigned(grad) then
  begin
    grad[0] /= radiusCnt;
    grad[1] /= radiusCnt;
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
    coords: TCorrectCoords;
    lx: TVector;
    iter: Integer;
    f, prevF: Double;
  begin
    coords.AngleIdx := AIndex mod CCorrectAngleCount;
    coords.ScanIdx := (AIndex div CCorrectAngleCount) mod High(FInputScans) + 1;

    lx := Copy(x);
    f := 1000.0;
    iter := 0;
    repeat
      prevF := f;

      case iter mod 3 of
        0:
          f := ASAMinimize(@GradientCorrect, lx, [-0.03 * FOutputDPI, 0.99], [0.03 * FOutputDPI, 1.01], 1e-12, @coords);
        1:
          f := PowellMinimize(@PowellCorrect, lx, 1.0, 1e-12, 0.0, MaxInt, @coords)[0];
        2:
          f := BFGSMinimize(@GradientCorrect, lx, 1e-12, @coords);
      end;

      f := Sqrt(f);
      Inc(iter);

      WriteLn(AIndex + 1:6,' / ',Length(FPerAngleX):6,', RMSE: ', f:12:9, ', Iteration: ', iter:3, #13);

    until CompareValue(f, prevF, 1e-9) >= 0;

    rmses[AIndex] := f;
    FPerAngleX[AIndex] := lx;
  end;

var
  ianglescan: Integer;
begin
  WriteLn('Correct');

  if Length(FInputScans) <= 1 then
    Exit;

  SetLength(FPerAngleX, CCorrectAngleCount * High(FInputScans));
  SetLength(rmses, Length(FPerAngleX));

  // starting point

  SetLength(x, 2);
  x[0] := 0.0;
  x[1] := 1.0;

  // compute

  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, High(FPerAngleX));

  WriteLn;

  // log

  for ianglescan := 0 to High(FPerAngleX) do
  begin
    Write(FInputScans[ianglescan div CCorrectAngleCount + 1].PNGShortName);
    Write(', Angle:', ((ianglescan mod CCorrectAngleCount) / CCorrectAngleCount) * 360.0:9:3);
    Write(', RMSE:', rmses[ianglescan]:12:9);
    WriteLn(',', FPerAngleX[ianglescan, 0]:9:3, ',', FPerAngleX[ianglescan, 1]:9:6);
  end;

  WriteLn('Worst RMSE: ', MaxValue(rmses):12:9);
end;

function TScanCorrelator.PowellCrop(const x: TVector; obj: Pointer): TScalar;
var
  inputIdx: PtrInt absolute obj;
  rBeg, rEnd, a0a, a1a, a0b, a1b, cx, cy, t, ri, rri, sn, cs, bt, px, py, p: Double;
  iLut, pos, arrPos: Integer;
  stdDevArr: TDoubleDynArray;
  sinCosLUT: TPointDDynArray;
begin
  Result := 1000.0;

  if not InRange(AngleTo02Pi(x[1] - x[0]), DegToRad(0.0), DegToRad(135.0)) then
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

  SetLength(stdDevArr, Ceil((rEnd - rBeg) / FOutputDPI * CCropAreaGroovesPerInch) * FPointsPerRevolution);

  ri := (rEnd - rBeg) / (CCropAreaGroovesPerInch * (FPointsPerRevolution - 1));

  BuildSinCosLUT(FPointsPerRevolution, sinCosLUT, t);

  iLut := 0;
  pos := 0;
  arrPos := 0;
  repeat
    bt := AngleTo02Pi(FRadiansPerRevolutionPoint * iLut + t);

    cs := sinCosLUT[iLut].X;
    sn := sinCosLUT[iLut].Y;

    rri := rBeg + ri * pos;

    px := cs * rri + cx;
    py := sn * rri + cy;

    Assert(pos < Length(stdDevArr));

    if FInputScans[inputIdx].InRangePointD(py, px) and
        not In02PiExtentsAngle(bt, a0a, a0b) and not In02PiExtentsAngle(bt, a1a, a1b) then
    begin
      p := FInputScans[inputIdx].GetPointD(py, px, isImage, False);
      stdDevArr[arrPos] := p;
      Inc(arrPos);
    end;

    Inc(iLut);
    if iLut >= FPointsPerRevolution then
      iLut := 0;

    Inc(pos);
  until rri >= rEnd;

  if arrPos > 0 then
    Result := -StdDev(PDouble(@stdDevArr[0]), arrPos);

  Write(FInputScans[inputIdx].PNGShortName, ', begin:', RadToDeg(a0a):9:3, ', end:', RadToDeg(a0b):9:3, ', obj:', -Result:12:6, #13);
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

  procedure InterpolateX(tau: Double; scanIdx: Integer; var x: TVector);
  var
    ci, i, x0, x1, x2, x3, so: Integer;
    c, alpha: Double;
    y0, y1, y2, y3: TVector;
  begin
    if Length(FPerAngleX) = 0 then
    begin
      x[0] := 0.0;
      x[1] := 1.0;
      Exit;
    end;

    c := tau * CTauToAngleIdx;
    ci := Trunc(c);
    alpha := c - ci;
    so := (scanIdx - 1) * CCorrectAngleCount;

    x0 := (ci - 1 + CCorrectAngleCount) mod CCorrectAngleCount;
    x1 := (ci + 0 + CCorrectAngleCount) mod CCorrectAngleCount;
    x2 := (ci + 1 + CCorrectAngleCount) mod CCorrectAngleCount;
    x3 := (ci + 2 + CCorrectAngleCount) mod CCorrectAngleCount;

    y0 := FPerAngleX[x0 + so];
    y1 := FPerAngleX[x1 + so];
    y2 := FPerAngleX[x2 + so];
    y3 := FPerAngleX[x3 + so];

    for i := 0 to High(x) do
      x[i] := herp(y0[i], y1[i], y2[i], y3[i], alpha);
  end;

  procedure DoY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    x: TVector;
    i, ox, cnt: Integer;
    r, sn, cs, px, py, t, cx, cy, acc, bt, skm, skc, ct, rsk: Double;
  begin
    SetLength(x, 2);

    for ox := 0 to High(FOutputImage[0]) do
    begin
      r := Sqrt(Sqr(AIndex - center) + Sqr(ox - center));

      if InRange(r, rBeg, rEnd) then
      begin
        bt := AngleTo02Pi(ArcTan2(AIndex - center, ox - center));

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
            InterpolateX(bt, i, x);
            skc := x[0];
            skm := x[1];
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
            acc += FInputScans[i].GetPointD(py, px, isImage, False);
            Inc(cnt);
            if not FRebuildBlended then
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
      if Odd(iter) then
        obj := Analyze(mmBFGS)
      else
        obj := Analyze(mmPowell);

      Inc(iter);
    until CompareValue(obj, prevObj, 1e-9) >= 0;
  end;

  if FCorrectAngles then
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


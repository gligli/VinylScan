unit scancorrelator;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, PNGComn, ZStream, MTProcs, TypInfo,
  utils, inputscan, profiles, powell, hackedwritepng;

const
  CAnalyzeSigma = 2.0;

type
  TCorrectSkew = record
    ConstSkew, MulSkew: Double;
  end;

  { TAnalyzeCoords }

  TAnalyzeCoords = record
    ScanIdx: Integer;
    MeanSD: TPointD;
    PreparedData: TDoubleDynArray;
  end;

  PAnalyzeCoords = ^TAnalyzeCoords;

  { TCorrectCoords }

  TCorrectCoords = record
    AngleIdx, ScanIdx, BaseScanIdx: Integer;

    StartAngle, EndAngle, AngleInc: Double;
    RadiusCnt, AngleCnt: Integer;

    SinCosLUT: TSinCosDynArray;
    Data: TDoubleDynArray;
    Ranks: TSpearmanRankDynArray;
    PreparedRanks: array of TSpearmanRankDynArray;
    Weights: TDoubleDynArray;
  end;

  PCorrectCoords = ^TCorrectCoords;

  { TScanCorrelator }

  TScanCorrelator = class
  private
    FProfileRef: TProfile;
    FInputScans: TInputScanDynArray;
    FFixCISScanners: Boolean;
    FBrickwallLimitScans: Boolean;
    FAnalyzePass: Boolean;
    FCorrectPass: Boolean;
    FRebuildScaled: Boolean;
    FRebuildBlendCount: Integer;
    FQualitySpeedRatio: Double;
    FOutputPNGFileName: String;
    FOutputDPI: Integer;
    FLock: TSpinlock;

    FPerAngleSkew: array of array of TCorrectSkew;

    FOutputWidth, FOutputHeight: Integer;
    FOutputImage: TWordDynArray;
    FOutputScans: TInputScanDynArray;

    FAngleInitAreaBegin: Double;
    FAngleInitAreaEnd: Double;

    FAnalyzeAreaBegin: Double;
    FAnalyzeAreaEnd: Double;
    FAnalyzeAreaWidth: Double;

    FCorrectAreaBegin: Double;
    FCorrectAreaEnd: Double;
    FCorrectAreaWidth: Double;

    procedure CorrectAnglesFromCoords(const coords: TCorrectCoords; out AStartAngle, AEndAngle, angleInc: Double; out
      angleCnt: Integer; AReduceAngles: Boolean);
    function SkewRadius(ARadius: Double; const ASkew: TCorrectSkew): Double;
    function ArgToSkew(arg: TVector): TCorrectSkew;
    function SkewToArg(const skew: TCorrectSkew): TVector;

    function PrepareAnalyze: TDoubleDynArray;
    function NelderMeadAnalyze(const arg: TVector; obj: Pointer): TScalar;
    function InitCorrect(var coords: TCorrectCoords; AReduceAngles: Boolean): Boolean;
    procedure PrepareCorrect(var coords: TCorrectCoords);
    function NelderMeadCorrect(const arg: TVector; obj: Pointer): TScalar;

    procedure AngleInit;
    procedure Analyze;
    procedure Crop;
    procedure Correct;
    procedure Rebuild;
  public
    constructor Create(AProfileRef: TProfile; const AFileNames: TStrings; AOutputDPI: Integer = 2400);
    destructor Destroy; override;

    procedure LoadScans;
    procedure Process;
    procedure Save;

    property OutputPNGFileName: String read FOutputPNGFileName write FOutputPNGFileName;
    property FixCISScanners: Boolean read FFixCISScanners write FFixCISScanners;
    property BrickwallLimitScans: Boolean read FBrickwallLimitScans write FBrickwallLimitScans;
    property AnalyzePass: Boolean read FAnalyzePass write FAnalyzePass;
    property CorrectPass: Boolean read FCorrectPass write FCorrectPass;
    property RebuildBlendCount: Integer read FRebuildBlendCount write FRebuildBlendCount;
    property RebuildScaled: Boolean read FRebuildScaled write FRebuildScaled;
    property QualitySpeedRatio: Double read FQualitySpeedRatio write FQualitySpeedRatio;

    property OutputDPI: Integer read FOutputDPI;
    property OutputWidth: Integer read FOutputWidth;
    property OutputHeight: Integer read FOutputHeight;

    property ProfileRef: TProfile read FProfileRef;
    property InputScans: TInputScanDynArray read FInputScans;
    property OutputImage: TWordDynArray read FOutputImage;
    property OutputScans: TInputScanDynArray read FOutputScans;
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

{ TScanCorrelator }

constructor TScanCorrelator.Create(AProfileRef: TProfile; const AFileNames: TStrings; AOutputDPI: Integer);
var
  iScan: Integer;
begin
  FProfileRef := AProfileRef;
  FOutputDPI := AOutputDPI;
  SetLength(FInputScans, AFileNames.Count);
  SetLength(FOutputScans, Length(FInputScans));

  for iScan := 0 to AFileNames.Count - 1 do
  begin
    FInputScans[iScan] := TInputScan.Create(FProfileRef, FOutputDPI, True);
    FInputScans[iScan].ImageFileName := AFileNames[iScan];
    FOutputScans[iScan] := TInputScan.Create(FProfileRef, FOutputDPI, True);
    FOutputScans[iScan].ImageFileName := AFileNames[iScan];
  end;

  SpinLeave(@FLock);

  FFixCISScanners := False;
  FBrickwallLimitScans := False;
  FAnalyzePass := True;
  FCorrectPass := True;
  FRebuildScaled := True;
  FRebuildBlendCount := 32;
  FQualitySpeedRatio := 1.0;

  FFixCISScanners := True;

  FAngleInitAreaBegin := FProfileRef.InnerSize;
  FAngleInitAreaEnd := FProfileRef.LabelOuterSize;
  FAnalyzeAreaBegin := FProfileRef.InnerSize;
  FAnalyzeAreaEnd := FProfileRef.LabelOuterSize;
  FAnalyzeAreaWidth := (FAnalyzeAreaEnd - FAnalyzeAreaBegin) * 0.5;
  FCorrectAreaBegin := FProfileRef.MinConcentricGroove;
  FCorrectAreaEnd := FProfileRef.StylusSetDown;
  FCorrectAreaWidth := (FCorrectAreaEnd - FCorrectAreaBegin) * 0.5;
end;

destructor TScanCorrelator.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FInputScans) do
  begin
    FInputScans[i].Free;
    FOutputScans[i].Free;
	end;

  inherited Destroy;
end;

function CompareInputScansCenterQuality(Item1, Item2, UserParameter: Pointer): Integer;
var
  s1: ^TInputScan absolute Item1;
  s2: ^TInputScan absolute Item2;
begin
  Result := CompareValue(s2^.CenterQuality, s1^.CenterQuality);
end;

procedure TScanCorrelator.LoadScans;

  procedure DoOne(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    Scan: TInputScan;
  begin
    Scan := FInputScans[AIndex];

    if FFixCISScanners then Scan.FixCISScanners;
    if FBrickwallLimitScans then Scan.BrickwallLimit;
    Scan.FindTrack(True, False);
    Scan.Blur;
  end;

var
  iScan, dpi: Integer;
begin
  WriteLn('LoadScans');

  for iScan := 0 to High(FInputScans) do
  begin
    FInputScans[iScan].LoadImage;
    WriteLn(FInputScans[iScan].ImageFileName);
  end;

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, high(FInputScans));

  if Length(FInputScans) > 0 then
  begin
    dpi := FInputScans[0].DPI;
    for iScan := 1 to High(FInputScans) do
      Assert(FInputScans[iScan].DPI = dpi, 'InputScans mixed DPIs!');
    if FRebuildScaled then
      FOutputDPI := dpi;
  end;

  WriteLn('DPI:', FOutputDPI:6);
  Writeln('Inner raw sample rate: ', Round(Pi * FProfileRef.LastMusicGroove * FOutputDPI * FProfileRef.RevolutionsPerSecond), ' Hz');

  if Length(FInputScans) > 1 then
  begin
    QuickSort(FInputScans[0], 0, High(FInputScans), SizeOf(TInputScan), @CompareInputScansCenterQuality);
    Writeln('Best centering: ', FInputScans[0].ImageShortName);
  end;
end;

procedure TScanCorrelator.AngleInit;
const
  CAngleCount = 720;
  CAggregatedPixelsInches = 0.1;
var
  rBeg, rEnd, aggregatedPixelCount: Integer;
  baseRanks: TSpearmanRankDynArray;

  function DoAngle(Scan: TInputScan; a: Double; var arr: TDoubleDynArray): Integer;
  var
    iRadius, iAngle, pxAggr: Integer;
    sn, cs, cy, cx, px, py: Double;
  begin
    Result := 0;

    FillQWord(arr[0], Length(arr), 0);

    cx := Scan.Center.X;
    cy := Scan.Center.Y;
    for iAngle := 0 to CAngleCount - 1 do
    begin
      SinCos(a + DegToRad(iAngle * (360 / CAngleCount)), sn, cs);

      pxAggr := 0;
      for iRadius:= rBeg to rEnd do
      begin
        px := cx + cs * iRadius;
        py := cy + sn * iRadius;
        if Scan.InRangePointD(py, px) then
          arr[Result] += Scan.GetPointD_Work(Scan.ProcessedImage, py, px);

        Inc(pxAggr);
        if pxAggr >= aggregatedPixelCount then
        begin
          pxAggr := 0;
          Inc(Result);
        end;
      end;

      if pxAggr > 0 then
        Inc(Result);
    end;
  end;

  procedure DoScan(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iAngle: Integer;
    a, r, bestr, bestAngle: Double;
    angle: TDoubleDynArray;
    angleRanks: TSpearmanRankDynArray;
    scan: TInputScan;
  begin
    if not InRange(AIndex, 1, High(FInputScans)) then
      Exit;

    scan := FInputScans[AIndex];

    SetLength(angle, Length(baseRanks));
    SetLength(angleRanks, Length(baseRanks));

    bestr := Infinity;
    bestAngle := 0.0;

    for iAngle := 0 to CAngleCount - 1 do
    begin
      a := DegToRad(iAngle * (360 / CAngleCount));

      DoAngle(scan, a, angle);

      SpearmanPrepareRanks(angle, angleRanks);
      r := -SpearmanRankCorrelation(baseRanks, angleRanks);

      if r <= bestr then
      begin
        bestr := r;
        bestAngle := a;
      end;
    end;

    FInputScans[AIndex].CorrectByModel(NaN, NaN, NormalizeAngle(bestAngle));
  end;

var
  iScan, pos: Integer;
  base: TDoubleDynArray;
begin
  WriteLn('AngleInit');

  if Length(FInputScans) <= 0 then
    Exit;

  SetLength(base, FInputScans[0].Width * CAngleCount);

  aggregatedPixelCount := Round(CAggregatedPixelsInches * FInputScans[0].DPI);
  rBeg := Round(FAngleInitAreaBegin * 0.5 * FInputScans[0].DPI);
  rEnd := Round(FAngleInitAreaEnd * 0.5 * FInputScans[0].DPI);

  pos := DoAngle(FInputScans[0], 0, base);

  SetLength(base, pos);
  SetLength(baseRanks, pos);

  SpearmanPrepareRanks(base, baseRanks);

  ProcThreadPool.DoParallelLocalProc(@DoScan, 1, High(FInputScans));

  for iScan := 0 to High(FInputScans) do
    WriteLn(FInputScans[iScan].ImageShortName, ', Angle: ', RadToDeg(FInputScans[iScan].RelativeAngle):9:3, ', CenterX: ', FInputScans[iScan].Center.X:9:3, ', CenterY: ', FInputScans[iScan].Center.Y:9:3);
end;

function TScanCorrelator.PrepareAnalyze: TDoubleDynArray;
var
  iRadiusAngle, pos, cnt, angleCnt, radiusCnt: Integer;
  t, rBeg, px, py, cx, cy, r, ri, sn, cs: Double;
  sinCosLUT: TSinCosDynArray;
  baseScan: TInputScan;
  baseMeanSD: TPointD;
begin
  baseScan := FInputScans[0];

  angleCnt := Ceil(baseScan.PointsPerRevolution * FQualitySpeedRatio);
  radiusCnt := Ceil(FAnalyzeAreaWidth * baseScan.DPI);
  cnt := radiusCnt * angleCnt;
  SetLength(Result, cnt);

  t   := baseScan.RelativeAngle;
  cx  := baseScan.Center.X;
  cy  := baseScan.Center.Y;

  BuildSinCosLUT(angleCnt, sinCosLUT, t);
  baseMeanSD := baseScan.GetMeanSD(baseScan.ProcessedImage, FAnalyzeAreaBegin * 0.5 * baseScan.DPI, FAnalyzeAreaEnd * 0.5 * baseScan.DPI, -Pi, Pi, CAnalyzeSigma);

  pos := 0;
  rBeg := FAnalyzeAreaBegin * 0.5 * baseScan.DPI;
  ri := 1.0 / angleCnt;
  for iRadiusAngle := 0 to High(Result) do
  begin
    cs := sinCosLUT[pos].Cos;
    sn := sinCosLUT[pos].Sin;

    r := rBeg + iRadiusAngle * ri;

    px := cs * r + cx;
    py := sn * r + cy;

    if baseScan.InRangePointD(py, px) then
    begin
      Result[iRadiusAngle] := CompressRange((baseScan.GetPointD_Work(baseScan.ProcessedImage, py, px) - baseMeanSD.X) * baseMeanSD.Y);
    end
    else
    begin
      Result[iRadiusAngle] := 1e-6;
    end;

    Inc(pos);

    if pos >= angleCnt then
      pos := 0;
  end;
end;

function TScanCorrelator.NelderMeadAnalyze(const arg: TVector; obj: Pointer): TScalar;
var
  coords: PAnalyzeCoords absolute obj;

  radiusCnt, angleCnt: Integer;
  sinCosLUT: TSinCosDynArray;
  scan: TInputScan;
  rBeg, rEnd, cx, cy, skx, sky, ri: Double;
  results: TDoubleDynArray;

  procedure DoSpiral(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iAngle, pos: Integer;
    px, py, r, sn, cs, funcAcc, imgInt, mseInt: Double;
  begin
    if not InRange(AIndex, 0, radiusCnt - 1) then
      Exit;

    pos := AIndex * angleCnt;
    funcAcc := 0.0;

    for iAngle := 0 to angleCnt - 1 do
    begin
      cs := sinCosLUT[iAngle].Cos;
      sn := sinCosLUT[iAngle].Sin;

      r := rBeg + pos * ri;

      px := cs * r * skx + cx;
      py := sn * r * sky + cy;

      imgInt := CompressRange((scan.GetPointD_Work(scan.ProcessedImage, py, px) - coords^.MeanSD.X) * coords^.MeanSD.Y);
      mseInt := coords^.PreparedData[pos] - imgInt;
      funcAcc += Sqr(mseInt);

      Inc(pos);
    end;

    results[AIndex] := funcAcc;
  end;

var
  iRes, iScan, cnt: Integer;
  t: Double;
begin
  scan := FInputScans[coords^.ScanIdx];

  angleCnt := Ceil(scan.PointsPerRevolution * FQualitySpeedRatio);
  radiusCnt := Ceil(FAnalyzeAreaWidth * scan.DPI);

  t := arg[0];
  cx := arg[1];
  cy := arg[2];
  skx := arg[3];
  sky := arg[4];

  rBeg := FAnalyzeAreaBegin * 0.5 * scan.DPI;
  rEnd := FAnalyzeAreaEnd * 0.5 * scan.DPI;
  ri := 1.0 / angleCnt;

  if not scan.InRangePointD(cy - rEnd * sky, cx - rEnd * skx) or not scan.InRangePointD(cy + rEnd * sky, cx + rEnd * skx) then
    Exit(1e6);

  BuildSinCosLUT(angleCnt, sinCosLUT, t);

  SetLength(results, radiusCnt);
  ProcThreadPool.DoParallelLocalProc(@DoSpiral, 0, radiusCnt - 1, nil, Round(ProcThreadPool.MaxThreadCount / High(FInputScans)));

  cnt := radiusCnt * angleCnt;
  Result := 0;
  for iRes := 0 to high(results) do
    Result += results[iRes];
  Result /= cnt;
  Result := Sqrt(Result);

  scan.Objective := Min(scan.Objective, Result);

  SpinEnter(@FLock);
  try
    Write('RMSEs: ');
    for iScan := 1 to High(FInputScans) do
      Write(FInputScans[iScan].Objective:12:9);
    Write(#13);
  finally
    SpinLeave(@FLock);
  end;
end;

function CompareInputScansObjective(Item1, Item2, UserParameter: Pointer): Integer;
var
  s1: ^TInputScan absolute Item1;
  s2: ^TInputScan absolute Item2;
begin
  Result := CompareValue(s1^.Objective, s2^.Objective);
end;

procedure TScanCorrelator.Analyze;
var
  preparedData: TDoubleDynArray;
  skewData: TPointDDynArray;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: TAnalyzeCoords;
    func: Double;
    X: TVector;
    scan: TInputScan;
  begin
    if not InRange(AIndex, 1, High(FInputScans)) then
      Exit;

    FillChar(coords, SizeOf(coords), 0);

    scan := FInputScans[AIndex];

    coords.ScanIdx := AIndex;
    coords.PreparedData := preparedData;
    coords.MeanSD := scan.GetMeanSD(scan.ProcessedImage, FAnalyzeAreaBegin * 0.5 * scan.DPI, FAnalyzeAreaEnd * 0.5 * scan.DPI, -Pi, Pi, CAnalyzeSigma);

    X := [scan.RelativeAngle, scan.Center.X, scan.Center.Y, skewData[AIndex].X, skewData[AIndex].Y];
    func := NelderMeadMinimize(@NelderMeadAnalyze, X, [DegToRad(1.0), 0.02 * scan.DPI, 0.02 * scan.DPI, 0.001, 0.001], 1e-6, @coords);

    scan.Objective := func;
    scan.CorrectByModel(X[1], X[2], X[0]);
    skewData[AIndex].X := X[3];
    skewData[AIndex].Y := X[4];
  end;

var
  iScan: Integer;
  scan: TInputScan;
begin
  WriteLn('Analyze');

  if Length(FInputScans) <= 1 then
    Exit;

  // init

  preparedData := PrepareAnalyze;

  SetLength(skewData, Length(FInputScans));
  for iScan := 0 to High(FInputScans) do
  begin
    skewData[iScan].X := 1.0;
    skewData[iScan].Y := 1.0;
  end;

  // compute

  ProcThreadPool.DoParallelLocalProc(@DoEval, 1, High(FInputScans));
  WriteLn;

  // log

  for iScan := 0 to High(FInputScans) do
  begin
    scan := FInputScans[iScan];

    WriteLn(scan.ImageShortName, ', Angle: ', RadToDeg(scan.RelativeAngle):9:3, ', CenterX: ', scan.Center.X:9:3, ', CenterY: ', scan.Center.Y:9:3, ', SkewX: ', skewData[iScan].X:9:6, ', SkewY: ', skewData[iScan].Y:9:6, ', RMSE: ', scan.Objective:12:9);
  end;

  // sort scans by RMSE (best to worst) for next algos

  QuickSort(FInputScans[0], 1, High(FInputScans), SizeOf(TInputScan), @CompareInputScansObjective);
end;

procedure TScanCorrelator.Crop;
var
  RadiusAngleLut: TRadiusAngleDynArray;
  SinCosLut: TSinCosDynArray;

  procedure DoCrop(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, High(FInputScans)) then
      Exit;

    FInputScans[AIndex].Crop(RadiusAngleLut, SinCosLut);
  end;

var
  i: Integer;
  rBeg, rEnd: Double;
begin
  WriteLn('Crop');

  if Length(FInputScans) <= 0 then
    Exit;

  rBeg := FProfileRef.LastMusicGroove * 0.5 * FInputScans[0].DPI;
  rEnd := FProfileRef.FirstMusicGroove * 0.5 * FInputScans[0].DPI;

  RadiusAngleLut := BuildRadiusAngleLUT(rBeg, rEnd, -Pi, Pi, FInputScans[0].DPI / 300.0);
  SinCosLut := OffsetRadiusAngleLUTAngle(RadiusAngleLut, 0.0);

  ProcThreadPool.DoParallelLocalProc(@DoCrop, 0, High(FInputScans));

  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].ImageShortName, ', begin:', RadToDeg(FInputScans[i].CropData.StartAngle):9:3, ', end:', RadToDeg(FInputScans[i].CropData.EndAngle):9:3, ', begin2:', RadToDeg(FInputScans[i].CropData.StartAngleMirror):9:3, ', end2:', RadToDeg(FInputScans[i].CropData.EndAngleMirror):9:3);
end;

procedure TScanCorrelator.CorrectAnglesFromCoords(const coords: TCorrectCoords; out AStartAngle, AEndAngle,
  angleInc: Double; out angleCnt: Integer; AReduceAngles: Boolean);
var
  croppedCnt: Integer;
  angle, angleExtents, startAngle, endAngle, a0a, a1a, a0b, a1b: Double;
  scan: TInputScan;
begin
  angle := (coords.AngleIdx / FProfileRef.CorrectAngleCount) * 2.0 * Pi;
  angleExtents := 2.0 * Pi / FProfileRef.CorrectAngleCount;
  startAngle := NormalizeAngle(angle - angleExtents);
  endAngle := NormalizeAngle(angle + angleExtents);

  if AReduceAngles then
  begin
    scan := FInputScans[coords.ScanIdx];

    // use CropData to potentially reduce angle span

    a0a := NormalizeAngle(scan.CropData.StartAngle - scan.RelativeAngle);
    a0b := NormalizeAngle(scan.CropData.EndAngle - scan.RelativeAngle);
    a1a := NormalizeAngle(scan.CropData.StartAngleMirror - scan.RelativeAngle);
    a1b := NormalizeAngle(scan.CropData.EndAngleMirror - scan.RelativeAngle);

    croppedCnt := 0;

    if InNormalizedAngle(startAngle, a0a, a0b) then
    begin
      startAngle := a0b;
      Inc(croppedCnt);
    end;

    if InNormalizedAngle(startAngle, a1a, a1b) then
    begin
      startAngle := a1b;
      Inc(croppedCnt);
    end;

    if InNormalizedAngle(endAngle, a0a, a0b) then
    begin
      endAngle := a0a;
      Inc(croppedCnt);
    end;

    if InNormalizedAngle(endAngle, a1a, a1b) then
    begin
      endAngle := a1a;
      Inc(croppedCnt);
    end;

    // entirely cropped angle? -> not to be computed

    if (croppedCnt >= 2) or (NormalizedAngleDiff(startAngle, endAngle) <= 0.0) then
    begin
      startAngle := NaN;
      endAngle := NaN;
    end;
  end;

  AStartAngle := startAngle;
  AEndAngle := endAngle;

  if not IsNan(startAngle) and not IsNan(endAngle) then
  begin
    angleInc := FInputScans[0].RadiansPerRevolutionPoint;
    angleCnt := Ceil(NormalizedAngleDiff(startAngle, endAngle) / angleInc * FQualitySpeedRatio);
  end
  else
  begin
    angleCnt := 0;
    angleInc := NaN;
  end;
end;

function TScanCorrelator.SkewRadius(ARadius: Double; const ASkew: TCorrectSkew): Double;
begin
  Result := ARadius + ASkew.MulSkew * ARadius + ASkew.ConstSkew;
end;

function TScanCorrelator.ArgToSkew(arg: TVector): TCorrectSkew;
begin
  Result.ConstSkew := arg[0];
  Result.MulSkew := arg[1] * 1e-3;
end;

function TScanCorrelator.SkewToArg(const skew: TCorrectSkew): TVector;
begin
  Result := [skew.ConstSkew, skew.MulSkew * 1e3];
end;

function TScanCorrelator.InitCorrect(var coords: TCorrectCoords; AReduceAngles: Boolean): Boolean;
var
  iAngle, iBaseScan: Integer;
  t, bt, v, best: Double;
  curScan, baseScan: TInputScan;
  tmpCoords: TCorrectCoords;
begin
  Result := True;
  Coords.BaseScanIdx := -1;
  curScan := FInputScans[Coords.ScanIdx];

  CorrectAnglesFromCoords(coords, coords.StartAngle, coords.EndAngle, coords.AngleInc, coords.AngleCnt, AReduceAngles);
  coords.RadiusCnt := Ceil(FCorrectAreaWidth * curScan.DPI);

  if IsNan(coords.StartAngle) or IsNan(coords.EndAngle) then
    Exit(False);

  // devise best baseScan

  tmpCoords := coords;
  tmpCoords.BaseScanIdx := coords.ScanIdx;
  PrepareCorrect(tmpCoords);

  best := Infinity;
  for iBaseScan := 0 to High(FInputScans) do
  begin
    baseScan := FInputScans[iBaseScan];

    if (Coords.ScanIdx = iBaseScan) or curScan.HasCorrectRef(FInputScans, Coords.AngleIdx, iBaseScan) then
      Continue;

    tmpCoords.ScanIdx := iBaseScan;
    BuildSinCosLUT(tmpCoords.AngleCnt, tmpCoords.sinCosLUT, tmpCoords.StartAngle + baseScan.RelativeAngle, NormalizedAngleDiff(tmpCoords.StartAngle, tmpCoords.EndAngle));
    v := NelderMeadCorrect([0.0, 0.0], @tmpCoords);

    for iAngle := -1800 to 1799 do
    begin
      bt := DegToRad(iAngle / 10.0);

      if InNormalizedAngle(bt, coords.StartAngle, coords.EndAngle) then
      begin
         t := NormalizeAngle(bt + baseScan.RelativeAngle);

         v += Ord(InNormalizedAngle(t, baseScan.CropData.StartAngle, baseScan.CropData.EndAngle)) +
              Ord(InNormalizedAngle(t, baseScan.CropData.StartAngleMirror, baseScan.CropData.EndAngleMirror));
      end;
    end;

    if v <= best then
    begin
      best := v;
      Coords.BaseScanIdx := iBaseScan;
    end;
  end;

  //WriteLn(Coords.ScanIdx:4, Coords.AngleIdx:4, Coords.BaseScanIdx:4, best:12:6);

  baseScan := FInputScans[Coords.BaseScanIdx];
  baseScan.AddCorrectRef(Coords.AngleIdx, Coords.ScanIdx);
end;

procedure TScanCorrelator.PrepareCorrect(var coords: TCorrectCoords);
var
  iRadius, iAngle, dummyAC: Integer;
  bt, rBeg, r, sn, cs, px, py, cx, cy, saRaw, eaRaw, dummyAI: Double;
  curScan, baseScan: TInputScan;
begin
  baseScan := FInputScans[Coords.BaseScanIdx];
  curScan := FInputScans[Coords.ScanIdx];

  // build sin / cos lookup table

  BuildSinCosLUT(coords.AngleCnt, coords.SinCosLUT, coords.StartAngle + baseScan.RelativeAngle, NormalizedAngleDiff(coords.StartAngle, coords.EndAngle));

  // build weights lookup table

  CorrectAnglesFromCoords(Coords, saRaw, eaRaw, dummyAI, dummyAC, False);

  SetLength(Coords.Weights, coords.AngleCnt);

  bt := coords.StartAngle;
  for iAngle := 0 to coords.AngleCnt - 1 do
  begin
    Coords.Weights[iAngle] := 1.0 - 2.0 * abs(NormalizedAngleDiff(saRaw, NormalizeAngle(bt)) / NormalizedAngleDiff(saRaw, eaRaw) - 0.5);
    bt += coords.AngleInc;
  end;

  // parse image arcs

  SetLength(coords.Data, coords.RadiusCnt);
  SetLength(coords.PreparedRanks, coords.AngleCnt, coords.RadiusCnt);

  cx := baseScan.Center.X;
  cy := baseScan.Center.Y;

  rBeg := FCorrectAreaBegin * 0.5 * baseScan.DPI;

  for iAngle := 0 to coords.AngleCnt - 1 do
  begin
    cs := coords.SinCosLUT[iAngle].Cos;
    sn := coords.SinCosLUT[iAngle].Sin;

    for iRadius := 0 to coords.RadiusCnt - 1 do
    begin
      r := rBeg + iRadius;

      px := cs * r + cx;
      py := sn * r + cy;

      if baseScan.InRangePointD(py, px) then
        coords.Data[iRadius] := baseScan.GetPointD_Work(baseScan.Image, py, px)
      else
        coords.Data[iRadius] := 1e6;
    end;

    SpearmanPrepareRanks(coords.Data, coords.PreparedRanks[iAngle]);
  end;

  // prepare iterations

  SetLength(coords.Ranks, Length(coords.Data));

  BuildSinCosLUT(coords.AngleCnt, coords.sinCosLUT, coords.StartAngle + curScan.RelativeAngle, NormalizedAngleDiff(coords.StartAngle, coords.EndAngle));
end;

function TScanCorrelator.NelderMeadCorrect(const arg: TVector; obj: Pointer): TScalar;
var
  coords: PCorrectCoords absolute obj;
  iRadius, iScan, iAngle: Integer;
  r, rBeg, rEnd, sn, cs, px, py, cx, cy, rsk, w, wAcc: Double;
  skew: TCorrectSkew;
  scan: TInputScan;
begin
  iScan := coords^.ScanIdx;
  scan := FInputScans[iScan];

  skew := ArgToSkew(arg);

  cx := scan.Center.X;
  cy := scan.Center.Y;

  // parse image arcs

  rBeg := FCorrectAreaBegin * 0.5 * scan.DPI;
  if not InRange(SkewRadius(rBeg, skew), rBeg * CScannerTolLo, rBeg * CScannerTolHi) then
    Exit(1e6);

  rEnd := FCorrectAreaEnd * 0.5 * scan.DPI;
  if not InRange(SkewRadius(rEnd, skew), rEnd * CScannerTolLo, rEnd * CScannerTolHi) then
    Exit(1e6);

  Result := 0.0;
  wAcc := 0.0;

  for iAngle := 0 to coords^.AngleCnt - 1 do
  begin
    cs := coords^.SinCosLUT[iAngle].Cos;
    sn := coords^.SinCosLUT[iAngle].Sin;

    for iRadius := 0 to coords^.RadiusCnt - 1 do
    begin
      r := rBeg + iRadius;

      rsk := SkewRadius(r, skew);

      px := cs * rsk + cx;
      py := sn * rsk + cy;

      coords^.Data[iRadius] := scan.GetPointD_Work(scan.Image, py, px);
    end;

    w := coords^.Weights[iAngle];
    wAcc += w;

    SpearmanPrepareRanks(coords^.Data, coords^.Ranks);
    Result -= SpearmanRankCorrelation(coords^.PreparedRanks[iAngle], coords^.Ranks) * w;
  end;

  if not IsZero(wAcc) then
    Result /= wAcc;
end;

procedure TScanCorrelator.Correct;
const
  CConstCorrectExtents = 0.05; // inches
  CConstCorrectHalfCount = 50;
  CMulCorrectExtents = 0.01;
  CMulCorrectHalfCount = 50;
var
  rmses: TDoubleDynArray;
  coordsArray: array of TCorrectCoords;
  validAngleCnt, doneCount: Integer;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: PCorrectCoords;
    iMul, iConst: Integer;
    X: TVector;
    f, loss: Double;
    scan: TInputScan;
    skew, tmpSk: TCorrectSkew;
  begin
    if not InRange(AIndex, 0, High(FPerAngleSkew)) then
      Exit;

    coords := @coordsArray[AIndex];
    scan := FInputScans[coords^.ScanIdx];

    loss := NaN;
    X := [0.0, 0.0];

    if not IsNan(coords^.StartAngle) and not IsNan(coords^.EndAngle) then
    begin
      PrepareCorrect(coords^);

      loss := Infinity;
      skew := ArgToSkew(X);

      for iMul := -CMulCorrectHalfCount to CMulCorrectHalfCount do
      begin
      tmpSk := skew;
      tmpSk.MulSkew := iMul * CMulCorrectExtents / CMulCorrectHalfCount;

        f := NelderMeadCorrect(SkewToArg(tmpSk), coords);

        if f < loss then
        begin
          loss := f;
          skew.MulSkew :=  tmpSk.MulSkew;
        end;
      end;

      for iConst := -CConstCorrectHalfCount to CConstCorrectHalfCount do
      begin
      tmpSk := skew;
      tmpSk.ConstSkew := iConst * CConstCorrectExtents / CConstCorrectHalfCount * scan.DPI;

        f := NelderMeadCorrect(SkewToArg(tmpSk), coords);

        if f < loss then
        begin
          loss := f;
          skew.ConstSkew := tmpSk.ConstSkew;
        end;
      end;

      X := SkewToArg(skew);
      loss := NelderMeadMinimize(@NelderMeadCorrect, X, [0.01, 0.01], 1e-6, coords);

      // free up memory
      SetLength(coords^.Data, 0);
      SetLength(coords^.PreparedRanks, 0);
      SetLength(coords^.Ranks, 0);
      SetLength(coords^.Weights, 0);
      SetLength(coords^.SinCosLUT, 0);

      Write(InterlockedIncrement(doneCount):4, ' / ', validAngleCnt, #13);
    end;

    rmses[AIndex] := loss;
    FPerAngleSkew[AIndex, 0] := ArgToSkew(X);
  end;

var
  iangle, iscan, ias, iasbase, prevAngleCnt, validRmseCnt: Integer;
  coords, baseCoords: PCorrectCoords;
  validRmses: TDoubleDynArray;
  res: Boolean;
begin
  WriteLn('Correct');

  if Length(FInputScans) <= 1 then
    Exit;

  SetLength(FPerAngleSkew, FProfileRef.CorrectAngleCount * High(FInputScans), 1);
  SetLength(coordsArray, Length(FPerAngleSkew));
  SetLength(rmses, Length(FPerAngleSkew));

  // init

  validAngleCnt := 0;
  for ias := 0 to High(FPerAngleSkew) do
  begin
    coords := @coordsArray[ias];

    DivMod(ias, FProfileRef.CorrectAngleCount, coords^.ScanIdx, coords^.AngleIdx);
    Inc(coords^.ScanIdx);

    if InitCorrect(coords^, True) then
      Inc(validAngleCnt);
  end;

  // scan / angles that are depended on should be computed

  repeat
    prevAngleCnt := validAngleCnt;

    for ias := 0 to High(FPerAngleSkew) do
    begin
      coords := @coordsArray[ias];

      if coords^.BaseScanIdx > 0 then
      begin
        iasbase := (coords^.BaseScanIdx - 1) * FProfileRef.CorrectAngleCount + coords^.AngleIdx;
        baseCoords := @coordsArray[iasbase];

        if IsNan(baseCoords^.StartAngle) or IsNan(baseCoords^.EndAngle) then
        begin
          res := InitCorrect(baseCoords^, False);
          Assert(res);
          Inc(validAngleCnt);
        end;
      end;
    end;

  until validAngleCnt = prevAngleCnt;

  // compute

  doneCount := 0;
  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, High(FPerAngleSkew));
  WriteLn;

  // cumulate

  for iscan := 1 to High(FInputScans) do
    for iangle := 0 to FProfileRef.CorrectAngleCount - 1 do
    begin
      ias := (iscan - 1) * FProfileRef.CorrectAngleCount + iangle;

      iasbase := ias;
      while True do
      begin
        iasbase := (coordsArray[iasbase].BaseScanIdx - 1) * FProfileRef.CorrectAngleCount + iangle;

        if iasbase < 0 then
          Break;

        SetLength(FPerAngleSkew[ias], Length(FPerAngleSkew[ias]) + 1);
        FPerAngleSkew[ias, High(FPerAngleSkew[ias])] := FPerAngleSkew[iasbase, 0];
      end;
    end;

  // log

  for iscan := 1 to High(FInputScans) do
    for iangle := 0 to FProfileRef.CorrectAngleCount - 1 do
    begin
      ias := (iscan - 1) * FProfileRef.CorrectAngleCount + iangle;

      Write(FInputScans[iscan].ImageShortName);
      Write(', Angle:', (iangle / FProfileRef.CorrectAngleCount) * 360.0:9:3);
      Write(', RMSE:', rmses[ias]:12:6);
      if not IsNan(rmses[ias]) then
        Write(', Const:', FPerAngleSkew[ias, 0].ConstSkew:9:3, ', Mul:', FPerAngleSkew[ias, 0].MulSkew:12:8);
      WriteLn;
    end;

  validRmseCnt := 0;
  SetLength(validRmses, Length(rmses));
  for ias := 0 to High(rmses) do
    if not IsNan(rmses[ias]) then
    begin
      validRmses[validRmseCnt] := rmses[ias];
      Inc(validRmseCnt);
    end;
  SetLength(validRmses, validRmseCnt);

  Assert(validRmseCnt = validAngleCnt);

  WriteLn('Mean RMSE:', Mean(validRmses):12:9, ', StdDev:', StdDev(validRmses):12:9, ', Worst RMSE:', MaxValue(validRmses):12:9);
end;

procedure TScanCorrelator.Rebuild;
const
  CLabelDepthBits = 4;
  CLabelDepthMaxValue = (1 shl CLabelDepthBits) - 1;
var
  TauToAngleIdx: Double;
  center, rBeg, rEnd, rLbl: Double;

  function InterpolateSkew(tau, radius: Double; scanIdx: Integer): Double;
  var
    iPoly, iInterp, ci, so: Integer;
    c, alpha, rr: Double;
    pp: array of TCorrectSkew;
    r: array[-1 .. 2] of Double;
  begin
    if (Length(FPerAngleSkew) = 0) or (scanIdx <= 0) then
      Exit(radius);

    tau := NormalizedAngleTo02Pi(tau);

    c := tau * TauToAngleIdx;
    ci := Trunc(c);
    alpha := c - ci;
    so := (scanIdx - 1) * FProfileRef.CorrectAngleCount;

    for iInterp := Low(r) to High(r) do
    begin
      pp := FPerAngleSkew[(ci + iInterp + FProfileRef.CorrectAngleCount) mod FProfileRef.CorrectAngleCount + so];

      rr := radius;
      for iPoly := 0 to High(pp) do
        rr := SkewRadius(rr, pp[iPoly]);
      r[iInterp] := rr;
    end;

    Result := herp(r[-1], r[0], r[1], r[2], alpha);
  end;

  procedure DoY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iScan, ox, cnt, yx: Integer;
    r, rSkew, sn, cs, px, py, acc, sample, bt, ct, d2d: Double;
    scan: TInputScan;
  begin
    yx := AIndex * FOutputWidth;

    for ox := 0 to FOutputWidth - 1 do
    begin
      r := Sqrt(Sqr(AIndex - center) + Sqr(ox - center));

      if InRange(r, rBeg, rEnd) then
      begin
        bt := ArcTan2(AIndex - center, ox - center);

        cnt := 0;
        acc := 0;
        for iScan := 0 to High(FInputScans) do
        begin
          scan := FInputScans[iScan];

          d2d := scan.DPI / FOutputDPI;

          rSkew := InterpolateSkew(bt, r * d2d, iScan);

          ct := NormalizeAngle(bt + scan.RelativeAngle);

          SinCos(ct, sn, cs);
          px := cs * rSkew + scan.Center.X;
          py := sn * rSkew + scan.Center.Y;

          sample := High(Word);
          if scan.InRangePointD(py, px) and
              (not InNormalizedAngle(ct, scan.CropData.StartAngle, scan.CropData.EndAngle) and
               not InNormalizedAngle(ct, scan.CropData.StartAngleMirror, scan.CropData.EndAngleMirror) or
               (r < rLbl)) then
          begin
            sample := scan.GetPointD_Final(scan.Image, py, px);

            if cnt < FRebuildBlendCount then
            begin
              acc += sample;
              Inc(cnt);
            end;
          end;

          if r >= rLbl then
            FOutputScans[iScan].Image[yx + ox] := EnsureRange(Round(sample), 0, High(Word))
          else
            FOutputScans[iScan].Image[yx + ox] := EnsureRange(Round(sample * CLabelDepthMaxValue / High(Word)), 0, CLabelDepthMaxValue) * High(Word) div CLabelDepthMaxValue; // lower bit depth for label
        end;

        acc := DivDef(acc, cnt, High(Word));

        if r >= rLbl then
          FOutputImage[yx + ox] := EnsureRange(Round(acc), 0, High(Word))
        else
          FOutputImage[yx + ox] := EnsureRange(Round(acc * CLabelDepthMaxValue / High(Word)), 0, CLabelDepthMaxValue) * High(Word) div CLabelDepthMaxValue; // lower bit depth for label
      end
      else
      begin
        // dark outside the disc, inner inside
        sample := IfThen(r >= rLbl, Round(0.25 * High(Word)), Round(1.0 * High(Word)));
        for iScan := 0 to High(FInputScans) do
          FOutputScans[iScan].Image[yx + ox] := Round(sample);
        FOutputImage[yx + ox] := Round(sample);
      end;
    end;
  end;

var
  iScan: Integer;
begin
  WriteLn('Rebuild');

  TauToAngleIdx := FProfileRef.CorrectAngleCount / (2.0 * Pi);

  FOutputWidth := Ceil(FProfileRef.OuterSize * FOutputDPI);
  FOutputHeight := FOutputWidth;
  SetLength(FOutputImage, sqr(FOutputWidth));
  for iScan := 0 to High(FOutputScans) do
    FOutputScans[iScan].InitImage(FOutputWidth, FOutputHeight, FOutputDPI);

  center := FOutputWidth / 2.0;
  rBeg := FProfileRef.AdapterSize * 0.5 * FOutputDPI;
  rEnd := FProfileRef.OuterSize * 0.5 * FOutputDPI;
  rLbl := FProfileRef.LabelOuterSize * 0.5 * FOutputDPI;

  ProcThreadPool.DoParallelLocalProc(@DoY, 0, FOutputHeight - 1);
end;

procedure TScanCorrelator.Save;

  procedure DoSave(AFN: String; const AImage: TWordDynArray);
  var
    i: Integer;
    png: TDPIAwareWriterPNG;
    fs: TFileStream;
    img: TScanImage;
  begin
    WriteLn('Save ', AFN);

    fs := TFileStream.Create(AFN, fmCreate or fmShareDenyNone);
    img := TScanImage.Create(FOutputWidth, FOutputHeight);
    png := TDPIAwareWriterPNG.Create;
    try
      img.Image := AImage;
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
  end;

var
  iScan: Integer;
begin
  DoSave(FOutputPNGFileName, FOutputImage);
  for iScan := 0 to High(FOutputScans) do
    DoSave(ChangeFileExt(FOutputPNGFileName, '.' + IntToStr(iScan) + ExtractFileExt(FOutputPNGFileName)), FOutputScans[iScan].Image);

  WriteLn('Done!');
end;

procedure TScanCorrelator.Process;
begin
  AngleInit;
  if FAnalyzePass then Analyze;
  Crop;
  if FCorrectPass then Correct;
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


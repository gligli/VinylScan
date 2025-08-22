unit scancorrelator;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, PNGComn, ZStream, MTProcs, TypInfo,
  utils, inputscan, profiles, powell, hackedwritepng;

type
  TCorrectSkew = record
    ConstSkew, MulSkew: Double;
  end;

  { TCorrectCoords }

  TCorrectCoords = record
    AngleIdx, ScanIdx, BaseScanIdx: Integer;

    StartAngle, EndAngle, AngleInc: Double;
    RadiusCnt, AngleCnt: Integer;

    ConstSkew, MulSkew: Double;
    MeanSD: TPointD;

    SinCosLUT: TSinCosDynArray;
    PreparedData: TDoubleDynArray;
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
    FAnalyzeMinimize: Boolean;
    FCorrectAngles: Boolean;
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

    FCorrectAngleCount: Integer;
    FCorrectAreaBegin: Double;
    FCorrectAreaEnd: Double;
    FCorrectAreaWidth: Double;

    procedure CorrectAnglesFromCoords(const coords: TCorrectCoords; out AStartAngle, AEndAngle, angleInc: Double; out
      angleCnt: Integer; AReduceAngles: Boolean);
    function SkewRadius(ARadius: Double; const ASkew: TCorrectSkew): Double;

    function PrepareAnalyze: TDoubleDynArray;
    function NelderMeadAnalyze(const arg: TVector; obj: Pointer): TScalar;
    function InitCorrect(var coords: TCorrectCoords): Boolean;
    procedure PrepareCorrect(var coords: TCorrectCoords);
    function NelderMeadCorrect(const arg: TVector; obj: Pointer): TScalar;
    function GridSearchCorrectConst(const arg: TVector; obj: Pointer): TScalar;
    function GridSearchCorrectMul(const arg: TVector; obj: Pointer): TScalar;

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
    property AnalyzeMinimize: Boolean read FAnalyzeMinimize write FAnalyzeMinimize;
    property CorrectAngles: Boolean read FCorrectAngles write FCorrectAngles;
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
  FAnalyzeMinimize := True;
  FCorrectAngles := True;
  FRebuildScaled := True;
  FRebuildBlendCount := 32;
  FQualitySpeedRatio := 1.0;

  FFixCISScanners := True;

  FAngleInitAreaBegin := FProfileRef.InnerSize;
  FAngleInitAreaEnd := FProfileRef.LabelOuterSize;
  FAnalyzeAreaBegin := FProfileRef.InnerSize;
  FAnalyzeAreaEnd := FProfileRef.LabelOuterSize;
  FAnalyzeAreaWidth := (FAnalyzeAreaEnd - FAnalyzeAreaBegin) * 0.5;
  FCorrectAngleCount := 36;
  FCorrectAreaBegin := FProfileRef.LabelOuterSize;
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
  CAggregatedPixelsInches = 0.2;
var
  rBeg, rEnd, aggregatedPixelCount: Integer;
  base: TDoubleDynArray;

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

      Inc(Result);
    end;
  end;

  procedure DoScan(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iAngle: Integer;
    a, r, bestr, bestAngle: Double;
    angle: TDoubleDynArray;
    scan: TInputScan;
  begin
    if not InRange(AIndex, 1, High(FInputScans)) then
      Exit;

    scan := FInputScans[AIndex];

    SetLength(angle, Length(base));

    bestr := Infinity;
    bestAngle := 0.0;

    for iAngle := 0 to CAngleCount - 1 do
    begin
      a := DegToRad(iAngle * (360 / CAngleCount));

      DoAngle(scan, a, angle);

      r := -SpearmanRankCorrelation(base, angle);

      if r <= bestr then
      begin
        bestr := r;
        bestAngle := a;
      end;
    end;

    FInputScans[AIndex].CorrectByModel(NaN, NaN, NormalizeAngle(bestAngle));
  end;

var
  pos: Integer;
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

  ProcThreadPool.DoParallelLocalProc(@DoScan, 1, High(FInputScans));
end;

function TScanCorrelator.PrepareAnalyze: TDoubleDynArray;
var
  iRadius, pos, cnt, angleCnt: Integer;
  t, rBeg, px, py, cx, cy, r, ri, sn, cs: Double;
  sinCosLUT: TSinCosDynArray;
  baseScan: TInputScan;
  baseMeanSD: TPointD;
begin
  baseScan := FInputScans[0];

  angleCnt := Ceil(baseScan.PointsPerRevolution * FQualitySpeedRatio);
  cnt := Ceil(FAnalyzeAreaWidth * baseScan.DPI * angleCnt);
  SetLength(Result, cnt);

  t   := baseScan.RelativeAngle;
  cx  := baseScan.Center.X;
  cy  := baseScan.Center.Y;

  BuildSinCosLUT(angleCnt, sinCosLUT, t);
  baseMeanSD := baseScan.GetMeanSD(baseScan.ProcessedImage, FAnalyzeAreaBegin * 0.5 * baseScan.DPI, FAnalyzeAreaEnd * 0.5 * baseScan.DPI, -Pi, Pi);

  pos := 0;
  rBeg := FAnalyzeAreaBegin * 0.5 * baseScan.DPI;
  ri := 1.0 / angleCnt;
  for iRadius := 0 to High(Result) do
  begin
    cs := sinCosLUT[pos].Cos;
    sn := sinCosLUT[pos].Sin;

    r := rBeg + iRadius * ri;

    px := cs * r + cx;
    py := sn * r + cy;

    if baseScan.InRangePointD(py, px) then
    begin
      Result[iRadius] := CompressRange((baseScan.GetPointD_Work(baseScan.ProcessedImage, py, px) - baseMeanSD.X) * baseMeanSD.Y);
    end
    else
    begin
      Result[iRadius] := 1e-6;
    end;

    Inc(pos);

    if pos >= angleCnt then
      pos := 0;
  end;
end;

function TScanCorrelator.NelderMeadAnalyze(const arg: TVector; obj: Pointer): TScalar;
var
  coords: PCorrectCoords absolute obj;

  radiusCnt, angleCnt: Integer;
  sinCosLUT: TSinCosDynArray;
  scan: TInputScan;
  rBeg, rEnd, cx, cy, ri: Double;
  results: TDoubleDynArray;

  procedure DoSpiral(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iAngle, pos: Integer;
    px, py, r, sn, cs, acc: Double;
  begin
    if not InRange(AIndex, 0, radiusCnt - 1) then
      Exit;

    pos := AIndex * angleCnt;
    acc := 0.0;

    for iAngle := 0 to angleCnt - 1 do
    begin
      cs := sinCosLUT[iAngle].Cos;
      sn := sinCosLUT[iAngle].Sin;

      r := rBeg + pos * ri;

      px := cs * r + cx;
      py := sn * r + cy;

      acc += Sqr(coords^.PreparedData[pos] - CompressRange((scan.GetPointD_Work(scan.ProcessedImage, py, px) - coords^.MeanSD.X) * coords^.MeanSD.Y));

      Inc(pos);
    end;

    results[AIndex] := acc;
  end;

var
  iScan: Integer;
  t: Double;
begin
  scan := FInputScans[coords^.ScanIdx];
  Result := 0.0;

  angleCnt := Ceil(scan.PointsPerRevolution * FQualitySpeedRatio);
  radiusCnt := Ceil(FAnalyzeAreaWidth * scan.DPI);

  t := arg[0];
  cx := arg[1];
  cy := arg[2];

  rBeg := FAnalyzeAreaBegin * 0.5 * scan.DPI;
  rEnd := FAnalyzeAreaEnd * 0.5 * scan.DPI;
  ri := 1.0 / angleCnt;

  if not scan.InRangePointD(cy - rEnd, cx - rEnd) or not scan.InRangePointD(cy + rEnd, cx + rEnd) then
    Exit(1e6);

  BuildSinCosLUT(angleCnt, sinCosLUT, t);

  SetLength(results, radiusCnt);
  ProcThreadPool.DoParallelLocalProc(@DoSpiral, 0, radiusCnt - 1, nil, ProcThreadPool.MaxThreadCount div High(FInputScans));

  Result := Mean(results) / angleCnt;
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

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: TCorrectCoords;
    func: Double;
    X: TVector;
    scan: TInputScan;
  begin
    if not InRange(AIndex, 1, High(FInputScans)) then
      Exit;

    FillChar(coords, SizeOf(coords), 0);

    scan := FInputScans[AIndex];

    coords.AngleIdx := -1;
    coords.ScanIdx := AIndex;
    coords.PreparedData := preparedData;
    coords.MeanSD := scan.GetMeanSD(scan.ProcessedImage, FAnalyzeAreaBegin * 0.5 * scan.DPI, FAnalyzeAreaEnd * 0.5 * scan.DPI, -Pi, Pi);

    X := [scan.RelativeAngle, scan.Center.X, scan.Center.Y];
    func := NelderMeadMinimize(@NelderMeadAnalyze, X, [0.01, 0.01, 0.01], 1e-9, @coords);

    scan.Objective := func;
    scan.CorrectByModel(X[1], X[2], X[0]);
  end;

var
  i: Integer;
begin
  WriteLn('Analyze');

  if Length(FInputScans) <= 1 then
    Exit;

  preparedData := PrepareAnalyze;

  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].ImageShortName, ', Angle: ', RadToDeg(FInputScans[i].RelativeAngle):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ' (before)');

  if FAnalyzeMinimize then
  begin
    ProcThreadPool.DoParallelLocalProc(@DoEval, 1, High(FInputScans));
    WriteLn;

    for i := 0 to High(FInputScans) do
      WriteLn(FInputScans[i].ImageShortName, ', Angle: ', RadToDeg(FInputScans[i].RelativeAngle):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ', RMSE: ', FInputScans[i].Objective:12:9, ' (after)');

    QuickSort(FInputScans[0], 1, High(FInputScans), SizeOf(TInputScan), @CompareInputScansObjective);
  end;
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
  angle := (coords.AngleIdx / FCorrectAngleCount) * 2.0 * Pi;
  angleExtents := 2.0 * Pi / FCorrectAngleCount;
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

    if croppedCnt >= 2 then
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

function TScanCorrelator.InitCorrect(var coords: TCorrectCoords): Boolean;
var
  iAngle, iBaseScan, v, best: Integer;
  t, bt: Double;
  curScan, baseScan: TInputScan;
begin
  Result := True;
  Coords.BaseScanIdx := -1;
  curScan := FInputScans[Coords.ScanIdx];

  CorrectAnglesFromCoords(coords, coords.StartAngle, coords.EndAngle, coords.AngleInc, coords.AngleCnt, True);
  coords.RadiusCnt := Ceil(FCorrectAreaWidth * curScan.DPI);

  if IsNan(coords.StartAngle) or IsNan(coords.EndAngle) then
    Exit(False);

  // devise best baseScan

  best := MaxInt;
  for iBaseScan := 0 to High(FInputScans) do
  begin
    baseScan := FInputScans[iBaseScan];

    if (Coords.ScanIdx = iBaseScan) or curScan.HasCorrectRef(FInputScans, Coords.AngleIdx, iBaseScan) then
      Continue;

    v := 0;
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

  //WriteLn(Coords.ScanIdx:4, Coords.AngleIdx:4, Coords.BaseScanIdx:4, best:8);

  baseScan := FInputScans[Coords.BaseScanIdx];
  baseScan.AddCorrectRef(Coords.AngleIdx, Coords.ScanIdx);
end;

procedure TScanCorrelator.PrepareCorrect(var coords: TCorrectCoords);
var
  iRadius, iAngle, cnt, dummyAC: Integer;
  t, bt, rBeg, r, sn, cs, px, py, cx, cy, saRaw, eaRaw, dummyAI: Double;
  curScan, baseScan: TInputScan;
  baseMeanSD: TPointD;
begin
  baseScan := FInputScans[Coords.BaseScanIdx];
  curScan := FInputScans[Coords.ScanIdx];

  // build sin / cos lookup table

  t := baseScan.RelativeAngle;
  BuildSinCosLUT(coords.AngleCnt, coords.SinCosLUT, coords.StartAngle + t, NormalizedAngleDiff(coords.StartAngle, coords.EndAngle));
  baseMeanSD := baseScan.GetMeanSD(baseScan.Image, FCorrectAreaBegin * 0.5 * baseScan.DPI, FCorrectAreaEnd * 0.5 * baseScan.DPI, NormalizeAngle(coords.StartAngle + t),  NormalizeAngle(coords.EndAngle + t));

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

  SetLength(coords.PreparedData, coords.RadiusCnt * coords.AngleCnt);

  cx := baseScan.Center.X;
  cy := baseScan.Center.Y;

  cnt := 0;
  rBeg := FCorrectAreaBegin * 0.5 * curScan.DPI;
  for iRadius := 0 to coords.RadiusCnt - 1 do
  begin
    r := rBeg + iRadius;

    for iAngle := 0 to coords.AngleCnt - 1 do
    begin
      cs := coords.SinCosLUT[iAngle].Cos;
      sn := coords.SinCosLUT[iAngle].Sin;

      px := cs * r + cx;
      py := sn * r + cy;

      if baseScan.InRangePointD(py, px) then
        coords.PreparedData[cnt] := CompressRange((baseScan.GetPointD_Work(baseScan.Image, py, px) - baseMeanSD.X) * baseMeanSD.Y)
      else
        coords.PreparedData[cnt] := 1e6;

      Inc(cnt);
    end;
  end;
  Assert(cnt = coords.RadiusCnt * coords.AngleCnt);

  // prepare iterations

  t := curScan.RelativeAngle;
  BuildSinCosLUT(coords.AngleCnt, coords.sinCosLUT, coords.StartAngle + t, NormalizedAngleDiff(coords.StartAngle, coords.EndAngle));
  Coords.MeanSD := curScan.GetMeanSD(curScan.Image, FCorrectAreaBegin * 0.5 * curScan.DPI, FCorrectAreaEnd * 0.5 * curScan.DPI, NormalizeAngle(coords.StartAngle + t),  NormalizeAngle(coords.EndAngle + t));
end;

function TScanCorrelator.NelderMeadCorrect(const arg: TVector; obj: Pointer): TScalar;
var
  coords: PCorrectCoords absolute obj;
  cnt, iRadius, iScan, iAngle: Integer;
  r, rBeg, rInner, rOuter, sn, cs, px, py, cx, cy, rsk: Double;
  skew: TCorrectSkew;
  scan: TInputScan;
begin
  iScan := coords^.ScanIdx;
  scan := FInputScans[iScan];

  skew.ConstSkew := arg[0];
  skew.MulSkew := arg[1];

  cx := scan.Center.X;
  cy := scan.Center.Y;

  // parse image arcs

  Result := 0;
  cnt := 0;
  rBeg := FCorrectAreaBegin * 0.5 * scan.DPI;
  rInner := FProfileRef.InnerSize * 0.5 * scan.DPI;
  rOuter := FProfileRef.OuterSize * 0.5 * scan.DPI;
  for iRadius := 0 to coords^.RadiusCnt - 1 do
  begin
    r := rBeg + iRadius;

    rsk := SkewRadius(r, skew);

    if not InRange(rsk, rInner, rOuter) then
    begin
      Result += Sqr(1e6) * coords^.AngleCnt;
      Inc(cnt, coords^.AngleCnt);
      Continue;
    end;

    for iAngle := 0 to coords^.AngleCnt - 1 do
    begin
      cs := coords^.SinCosLUT[iAngle].Cos;
      sn := coords^.SinCosLUT[iAngle].Sin;

      px := cs * rsk + cx;
      py := sn * rsk + cy;

      Result += Sqr((coords^.PreparedData[cnt] - CompressRange((scan.GetPointD_Work(scan.Image, py, px) - coords^.MeanSD.X) * coords^.MeanSD.Y)) * coords^.Weights[iAngle]);

      Inc(cnt);
    end;
  end;

  Assert(cnt = coords^.RadiusCnt * coords^.AngleCnt);

  Result /= cnt;
  Result := Sqrt(Result);
end;

function TScanCorrelator.GridSearchCorrectConst(const arg: TVector; obj: Pointer): TScalar;
var
  coords: PCorrectCoords absolute obj;
begin
  Result := NelderMeadCorrect([arg[0], coords^.MulSkew], obj);
end;

function TScanCorrelator.GridSearchCorrectMul(const arg: TVector; obj: Pointer): TScalar;
var
  coords: PCorrectCoords absolute obj;
begin
  Result := NelderMeadCorrect([coords^.ConstSkew, arg[0]], obj);
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
    f, skm, skc, loss: Double;
    scan: TInputScan;
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
      coords^.ConstSkew := X[0];
      coords^.MulSkew := X[1];

      for iMul := -CMulCorrectHalfCount to CMulCorrectHalfCount do
      begin
        skm := iMul * CMulCorrectExtents / CMulCorrectHalfCount;

        f := GridSearchCorrectMul([skm], coords);

        if f < loss then
        begin
          loss := f;
          coords^.MulSkew := skm;
        end;
      end;

      for iConst := -CConstCorrectHalfCount to CConstCorrectHalfCount do
      begin
        skc := iConst * CConstCorrectExtents / CConstCorrectHalfCount * FOutputDPI;

        f := GridSearchCorrectConst([skc], coords);

        if f < loss then
        begin
          loss := f;
          coords^.ConstSkew := skc;
        end;
      end;

      X := [coords^.ConstSkew, coords^.MulSkew];
      loss := NelderMeadMinimize(@NelderMeadCorrect, X, [0.01, 0.0001], 1e-9, coords);

      // free up memory
      SetLength(coords^.PreparedData, 0);
      SetLength(coords^.SinCosLUT, 0);
      SetLength(coords^.Weights, 0);

      Write(InterlockedIncrement(doneCount):4, ' / ', validAngleCnt, #13);
    end;

    rmses[AIndex] := loss;
    FPerAngleSkew[AIndex, 0].ConstSkew := X[0];
    FPerAngleSkew[AIndex, 0].MulSkew := X[1];
  end;

var
  iangle, iscan, ias, iasbase, validRmseCnt: Integer;
  coords: PCorrectCoords;
  validRmses: TDoubleDynArray;
begin
  WriteLn('Correct');

  if Length(FInputScans) <= 1 then
    Exit;

  SetLength(FPerAngleSkew, FCorrectAngleCount * High(FInputScans), 1);
  SetLength(coordsArray, Length(FPerAngleSkew));
  SetLength(rmses, Length(FPerAngleSkew));

  // init

  validAngleCnt := 0;
  for ias := 0 to High(FPerAngleSkew) do
  begin
    coords := @coordsArray[ias];

    DivMod(ias, FCorrectAngleCount, coords^.ScanIdx, coords^.AngleIdx);
    Inc(coords^.ScanIdx);

    if InitCorrect(coords^) then
      Inc(validAngleCnt);
  end;

  // compute

  doneCount := 0;
  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, High(FPerAngleSkew));
  WriteLn;

  // cumulate

  for iscan := 1 to High(FInputScans) do
    for iangle := 0 to FCorrectAngleCount - 1 do
    begin
      ias := (iscan - 1) * FCorrectAngleCount + iangle;

      iasbase := ias;
      while True do
      begin
        iasbase := (coordsArray[iasbase].BaseScanIdx - 1) * FCorrectAngleCount + iangle;

        if iasbase < 0 then
          Break;

        SetLength(FPerAngleSkew[ias], Length(FPerAngleSkew[ias]) + 1);
        FPerAngleSkew[ias, High(FPerAngleSkew[ias])] := FPerAngleSkew[iasbase, 0];
      end;
    end;

  // log

  for iscan := 1 to High(FInputScans) do
    for iangle := 0 to FCorrectAngleCount - 1 do
    begin
      ias := (iscan - 1) * FCorrectAngleCount + iangle;

      Write(FInputScans[iscan].ImageShortName);
      Write(', Angle:', (iangle / FCorrectAngleCount) * 360.0:9:3);
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
    so := (scanIdx - 1) * FCorrectAngleCount;

    for iInterp := Low(r) to High(r) do
    begin
      pp := FPerAngleSkew[(ci + iInterp + FCorrectAngleCount) mod FCorrectAngleCount + so];

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

  TauToAngleIdx := FCorrectAngleCount / (2.0 * Pi);

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
  Analyze;
  Crop;
  if FCorrectAngles then Correct;
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


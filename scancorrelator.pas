unit scancorrelator;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, PNGComn, ZStream, MTProcs, TypInfo,
  utils, inputscan, powell, hackedwritepng;

type
  TCorrectSkew = record
    ConstSkew, MulSkew, SqrSkew: Double;
  end;

  TAngleScanCoords = record
    AngleIdx, ScanIdx, BaseScanIdx: Integer;
    RadiusAngleLUT: TRadiusAngleDynArray;
    SinCosLUT: TSinCosDynArray;
    PreparedValues, Weights: TDoubleDynArray;
    StartAngle, EndAngle: Double;
    BaseMeanSD, MeanSD: TPointD;
  end;

  PAngleScanCoords = ^TAngleScanCoords;

  { TScanCorrelator }

  TScanCorrelator = class
  private
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

    procedure CorrectAnglesFromCoords(const coords: TAngleScanCoords; out AStartAngle, AEndAngle: Double;
      AReduceAngles: Boolean);

    procedure InitAnalyze(var Coords: TAngleScanCoords);
    procedure PrepareAnalyze(var Coords: TAngleScanCoords);
    function NelderMeadAnalyze(const arg: TVector; data: Pointer): TScalar;
    function InitCorrect(var Coords: TAngleScanCoords): Boolean;
    procedure PrepareCorrect(var Coords: TAngleScanCoords);
    function GridSearchCorrect(ConstSkew, MulSkew, SqrSkew: Double; const Coords: TAngleScanCoords): Double;
    function NelderMeadCorrect(const arg: TDoubleDynArray; data: Pointer): Double;
    procedure GradientCorrect(const arg: TDoubleDynArray; var func: Double; grad: TDoubleDynArray; obj: Pointer);

    procedure AngleInit;
    procedure Analyze;
    procedure Crop;
    procedure Correct;
    procedure Rebuild;
  public
    constructor Create(const AFileNames: TStrings; AOutputDPI: Integer = 2400);
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

const
  CAnalyzeAreaBegin = C45RpmInnerSize;
  CAnalyzeAreaEnd = C45RpmLabelOuterSize;

  CCorrectAngleCount = 36;
  CCorrectAreaBegin = C45RpmLabelOuterSize;
  CCorrectAreaEnd = C45RpmOuterSize;

constructor TScanCorrelator.Create(const AFileNames: TStrings; AOutputDPI: Integer);
var
  iScan: Integer;
begin
  FOutputDPI := AOutputDPI;
  SetLength(FInputScans, AFileNames.Count);
  SetLength(FOutputScans, Length(FInputScans));

  for iScan := 0 to AFileNames.Count - 1 do
  begin
    FInputScans[iScan] := TInputScan.Create(FOutputDPI, True);
    FInputScans[iScan].ImageFileName := AFileNames[iScan];
    FOutputScans[iScan] := TInputScan.Create(FOutputDPI, True);
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

    Scan.LoadImage;
    if FFixCISScanners then Scan.FixCISScanners;
    if FBrickwallLimitScans then Scan.BrickwallLimit;
    Scan.FindTrack(True);
  end;

var
  i, dpi: Integer;
begin
  WriteLn('LoadScans');

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, high(FInputScans));

  if Length(FInputScans) > 0 then
  begin
    dpi := FInputScans[0].DPI;
    for i := 1 to High(FInputScans) do
      Assert(FInputScans[i].DPI = dpi, 'InputScans mixed DPIs!');
    if FRebuildScaled then
      FOutputDPI := dpi;
  end;

  WriteLn('DPI:', FOutputDPI:6);
  Writeln('Inner raw sample rate: ', Round(Pi * C45RpmLastMusicGroove * FOutputDPI * C45RpmRevolutionsPerSecond), ' Hz');

  if Length(FInputScans) > 1 then
  begin
    QuickSort(FInputScans[0], 0, High(FInputScans), SizeOf(TInputScan), @CompareInputScansCenterQuality);
    Writeln('Best centering: ', FInputScans[0].ImageShortName);
  end;
end;

procedure TScanCorrelator.AngleInit;
const
  CAngleCount = 360;
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
          arr[Result] += Scan.GetPointD_Linear(Scan.Image, py, px);

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

    for iAngle := 0 to 359 do
    begin
      a := DegToRad(iAngle);

      DoAngle(scan, a, angle);

      r := -SpearmanRankCorrelation(base, angle);

      if r <= bestr then
      begin
        bestr := r;
        bestAngle := a;
      end;
    end;

    FInputScans[AIndex].CorrectByModel(NaN, NaN, NormalizeAngle(bestAngle), NaN, NaN);
  end;

var
  pos: Integer;
begin
  WriteLn('AngleInit');

  if Length(FInputScans) <= 0 then
    Exit;

  SetLength(base, FInputScans[0].Width * CAngleCount);

  aggregatedPixelCount := Round(CAggregatedPixelsInches * FInputScans[0].DPI);
  rBeg := Round(CAnalyzeAreaBegin * 0.5 * FInputScans[0].DPI);
  rEnd := Round(CAnalyzeAreaEnd * 0.5 * FInputScans[0].DPI);

  pos := DoAngle(FInputScans[0], 0, base);

  SetLength(base, pos);

  ProcThreadPool.DoParallelLocalProc(@DoScan, 1, High(FInputScans));
end;

procedure TScanCorrelator.InitAnalyze(var Coords: TAngleScanCoords);
var
  ilut: Integer;
  cx, cy, r, ox, oy: Double;
  baseScan: TInputScan;
  ra: ^TRadiusAngle;
  sc: ^TSinCos;
  sinCosLUT: TSinCosDynArray;
begin
  baseScan := FInputScans[Coords.BaseScanIdx];

  // build lookup tables

  Coords.RadiusAngleLUT := BuildRadiusAngleLUT(CAnalyzeAreaBegin * 0.5 * baseScan.DPI, CAnalyzeAreaEnd * 0.5 * baseScan.DPI, -Pi, Pi, 1.0 / FQualitySpeedRatio);

  sinCosLUT := OffsetRadiusAngleLUTAngle(Coords.RadiusAngleLUT, baseScan.RelativeAngle);

  Coords.BaseMeanSD := baseScan.GetMeanSD(CAnalyzeAreaBegin * 0.5 * baseScan.DPI, CAnalyzeAreaEnd * 0.5 * baseScan.DPI, -Pi, Pi);

  // parse image using LUTs

  cx  := baseScan.Center.X;
  cy  := baseScan.Center.Y;

  SetLength(Coords.PreparedValues, Length(Coords.RadiusAngleLUT));
  for iLut := 0 to High(Coords.RadiusAngleLUT) do
  begin
    ra := @Coords.RadiusAngleLUT[iLut];
    sc := @sinCosLUT[iLut];

    r := ra^.Radius;
    ox := sc^.Cos * r + cx;
    oy := sc^.Sin * r + cy;

    if baseScan.InRangePointD(oy, ox) then
      Coords.PreparedValues[ilut] := CompressRange((baseScan.GetPointD_Linear(baseScan.LeveledImage, oy, ox) - Coords.BaseMeanSD.X) * Coords.BaseMeanSD.Y)
    else
      Coords.PreparedValues[ilut] := 1e6;
  end;
end;

procedure TScanCorrelator.PrepareAnalyze(var Coords: TAngleScanCoords);
var
  scan: TInputScan;
begin
  scan := FInputScans[Coords.ScanIdx];

  Coords.MeanSD := scan.GetMeanSD(CAnalyzeAreaBegin * 0.5 * scan.DPI, CAnalyzeAreaEnd * 0.5 * scan.DPI, -Pi, Pi);
end;

function TScanCorrelator.NelderMeadAnalyze(const arg: TVector; data: Pointer): TScalar;
var
  coords: PAngleScanCoords absolute data;
  iScan, ilut, radiusLimitX, radiusLimitY: Integer;
  r, px, py, angle, centerX, centerY, skewX, skewY, prevAngle: Double;
  scan: TInputScan;
  ra: ^TRadiusAngle;
  sc: TSinCos;
  extents: TRect;
begin
  centerX := arg[0];
  centerY := arg[1];
  angle := arg[2];
  skewX := arg[3];
  skewY := arg[4];

  scan := FInputScans[coords^.ScanIdx];

  radiusLimitX := Round(CAnalyzeAreaEnd * 0.5 * scan.DPI) + 1;
  radiusLimitY := Round(CAnalyzeAreaEnd * 0.5 * scan.DPI) + 1;

  extents.Left := radiusLimitX;
  extents.Top := radiusLimitY;
  extents.Right := scan.Width - radiusLimitX;
  extents.Bottom := scan.Height - radiusLimitY;

  Result := 1000.0;

  if extents.Contains(TPoint.Create(Round(centerX), Round(centerY))) then
  begin
    Result := 0.0;
    prevAngle := Infinity;

    for iLut := 0 to High(coords^.RadiusAngleLUT) do
    begin
      ra := @coords^.RadiusAngleLUT[iLut];

      r := ra^.Radius;
      IncrementalSinCos(ra^.Angle + angle, prevAngle, sc);

      px := sc.Cos * r * skewX + centerX;
      py := sc.Sin * r * skewY + centerY;

      Result += Sqr(coords^.PreparedValues[ilut] - CompressRange((scan.GetPointD_Linear(scan.LeveledImage, py, px) - coords^.MeanSD.X) * coords^.MeanSD.Y));
    end;

    Result := Sqrt(Result / Length(coords^.RadiusAngleLUT));
  end;

  scan.Objective := Min(scan.Objective, Result);

  SpinEnter(@FLock);
  try
    Write('Losses: ');
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
  baseCoords: TAngleScanCoords;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: TAngleScanCoords;
    func: Double;
    X: TVector;
    scan: TInputScan;
  begin
    if not InRange(AIndex, 1, High(FInputScans)) then
      Exit;

    scan := FInputScans[AIndex];

    coords := baseCoords;
    coords.ScanIdx := AIndex;
    PrepareAnalyze(coords);

    X := [scan.Center.X, scan.Center.Y, scan.RelativeAngle, 1.0, 1.0];

    func := NelderMeadMinimize(@NelderMeadAnalyze, X, [0.02 * scan.DPI, 0.02 * scan.DPI, DegToRad(1.0), 0.01, 0.01], 1e-6, @coords);

    scan.Objective := func;
    FInputScans[AIndex].CorrectByModel(X[0], X[1], X[2], X[3], X[4]);
  end;

var
  i: Integer;
begin
  WriteLn('Analyze');

  if Length(FInputScans) <= 1 then
    Exit;

  FillChar(baseCoords, SizeOf(baseCoords), 0);
  baseCoords.AngleIdx := -1;
  InitAnalyze(baseCoords);

  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].ImageShortName, ', Angle: ', RadToDeg(FInputScans[i].RelativeAngle):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ' (before)');

  if FAnalyzeMinimize then
  begin
    ProcThreadPool.DoParallelLocalProc(@DoEval, 1, High(FInputScans));
    WriteLn;

    for i := 0 to High(FInputScans) do
      WriteLn(FInputScans[i].ImageShortName, ', Angle: ', RadToDeg(FInputScans[i].RelativeAngle):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ', SkewX: ', FInputScans[i].Skew.X:9:6, ', SkewY: ', FInputScans[i].Skew.Y:9:6, ', Loss: ', FInputScans[i].Objective:12:9, ' (after)');

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

  rBeg := C45RpmLastMusicGroove * 0.5 * FInputScans[0].DPI;
  rEnd := C45RpmFirstMusicGroove * 0.5 * FInputScans[0].DPI;

  RadiusAngleLut := BuildRadiusAngleLUT(rBeg, rEnd, -Pi, Pi, FInputScans[0].DPI / 300.0);
  SinCosLut := OffsetRadiusAngleLUTAngle(RadiusAngleLut, 0.0);

  ProcThreadPool.DoParallelLocalProc(@DoCrop, 0, High(FInputScans));

  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].ImageShortName, ', begin:', RadToDeg(FInputScans[i].CropData.StartAngle):9:3, ', end:', RadToDeg(FInputScans[i].CropData.EndAngle):9:3, ', begin2:', RadToDeg(FInputScans[i].CropData.StartAngleMirror):9:3, ', end2:', RadToDeg(FInputScans[i].CropData.EndAngleMirror):9:3);
end;

procedure TScanCorrelator.CorrectAnglesFromCoords(const coords: TAngleScanCoords; out AStartAngle, AEndAngle: Double; AReduceAngles: Boolean);
var
  croppedCnt: Integer;
  angle, angleExtents, startAngle, endAngle, a0a, a1a, a0b, a1b: Double;
  scan: TInputScan;
begin
  angle := (coords.AngleIdx / CCorrectAngleCount) * 2.0 * Pi;
  angleExtents := 2.0 * Pi / CCorrectAngleCount;
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
end;

function TScanCorrelator.InitCorrect(var Coords: TAngleScanCoords): Boolean;
var
  iAngle, iBaseScan, v, best: Integer;
  bt, t: Double;
  curScan, baseScan: TInputScan;
begin
  Result := True;
  Coords.BaseScanIdx := 0;

  CorrectAnglesFromCoords(Coords, Coords.StartAngle, Coords.EndAngle, True);

  if IsNan(Coords.StartAngle) or IsNan(Coords.EndAngle) then
    Exit(False);

  // devise best baseScan

  curScan := FInputScans[Coords.ScanIdx];
  Coords.BaseScanIdx := -1;
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

      if InNormalizedAngle(bt, Coords.StartAngle, Coords.EndAngle) then
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

procedure TScanCorrelator.PrepareCorrect(var Coords: TAngleScanCoords);
var
  iLut: Integer;
  saRaw, eaRaw, bt, alpha: Double;
  centerX, centerY, px, py, r: Double;
  baseScan, scan: TInputScan;
  ra: ^TRadiusAngle;
  sc: ^TSinCos;
  sinCosLUT: TSinCosDynArray;
begin
  baseScan := FInputScans[Coords.BaseScanIdx];
  scan := FInputScans[Coords.ScanIdx];

  // build radius / angle lookup table

  Coords.RadiusAngleLUT := BuildRadiusAngleLUT(CCorrectAreaBegin * 0.5 * baseScan.DPI, CCorrectAreaEnd * 0.5 * baseScan.DPI, Coords.StartAngle, Coords.EndAngle, 1.0 / FQualitySpeedRatio);

  // build weights lookup table

  CorrectAnglesFromCoords(Coords, saRaw, eaRaw, False);

  SetLength(Coords.Weights, Length(Coords.RadiusAngleLUT));
  for iLut := 0 to High(Coords.RadiusAngleLUT) do
  begin
    ra := @Coords.RadiusAngleLUT[iLut];

    bt := ra^.Angle;
    if InNormalizedAngle(bt, Coords.StartAngle, Coords.EndAngle) then
    begin
      alpha := 1.0 - 2.0 * abs(NormalizedAngleDiff(saRaw, bt) / NormalizedAngleDiff(saRaw, eaRaw) - 0.5);
      Coords.Weights[iLut] := alpha;
    end;
  end;

  // parse image using LUT

  Coords.BaseMeanSD := baseScan.GetMeanSD(CCorrectAreaBegin * 0.5 * baseScan.DPI, CCorrectAreaEnd * 0.5 * baseScan.DPI, Coords.StartAngle, Coords.EndAngle);

  sinCosLUT := OffsetRadiusAngleLUTAngle(Coords.RadiusAngleLUT, baseScan.RelativeAngle);

  centerX  := baseScan.Center.X;
  centerY  := baseScan.Center.Y;

  SetLength(Coords.PreparedValues, Length(Coords.RadiusAngleLUT));
  for iLut := 0 to High(Coords.RadiusAngleLUT) do
  begin
    ra := @Coords.RadiusAngleLUT[iLut];
    sc := @sinCosLUT[iLut];

    r := ra^.Radius;
    px := sc^.Cos * r + centerX;
    py := sc^.Sin * r + centerY;

    if baseScan.InRangePointD(py, px) then
      Coords.PreparedValues[iLut] := CompressRange((baseScan.GetPointD_Linear(baseScan.LeveledImage, py, px) - Coords.BaseMeanSD.X) * Coords.BaseMeanSD.Y)
    else
      Coords.PreparedValues[iLut] := 1e6;
  end;

  // prepare for iterations

  sinCosLUT := OffsetRadiusAngleLUTAngle(Coords.RadiusAngleLUT, scan.RelativeAngle);
  Coords.MeanSD := scan.GetMeanSD(CCorrectAreaBegin * 0.5 * scan.DPI, CCorrectAreaEnd * 0.5 * scan.DPI, Coords.StartAngle, Coords.EndAngle);
  Coords.SinCosLUT := sinCosLUT;
end;

function TScanCorrelator.GridSearchCorrect(ConstSkew, MulSkew, SqrSkew: Double; const Coords: TAngleScanCoords): Double;
var
  iLut: Integer;
  centerX, centerY, r, px, py: Double;
  scan: TInputScan;
  ra: ^TRadiusAngle;
  sc: ^TSinCos;
begin
  MulSkew *= 1e-3;
  SqrSkew *= 1e-6;

  scan := FInputScans[Coords.ScanIdx];

  centerX  := scan.Center.X;
  centerY  := scan.Center.Y;

  Result := 0.0;

  for iLut := 0 to High(Coords.RadiusAngleLUT) do
  begin
    ra := @Coords.RadiusAngleLUT[iLut];
    sc := @Coords.SinCosLUT[iLut];

    r := ra^.Radius;
    r := Sqr(r) * SqrSkew + r * MulSkew + ConstSkew;

    px := sc^.Cos * r + centerX;
    py := sc^.Sin * r + centerY;

    if scan.InRangePointD(py, px) then
      Result += Sqr((Coords.PreparedValues[iLut] - CompressRange((scan.GetPointD_Linear(scan.LeveledImage, py, px) - Coords.MeanSD.X) * Coords.MeanSD.Y)) * Coords.Weights[iLut])
    else
      Result += Sqr(1e6);
  end;

  Result := Sqrt(Result / Length(Coords.RadiusAngleLUT));
end;

function TScanCorrelator.NelderMeadCorrect(const arg: TDoubleDynArray; data: Pointer): Double;
begin
  Result := GridSearchCorrect(arg[0], arg[1], arg[2], PAngleScanCoords(data)^);
end;

procedure TScanCorrelator.GradientCorrect(const arg: TDoubleDynArray; var func: Double; grad: TDoubleDynArray; obj:
 Pointer);
const
  CH = 1e-8;
var
  iFD: Integer;
  ConstSkew, MulSkew, SqrSkew, fdx, fdy, gskc, gskm, gsks: Double;
begin
  ConstSkew := arg[0];
  MulSkew := arg[1];
  SqrSkew := arg[2];

  func := GridSearchCorrect(ConstSkew, MulSkew, SqrSkew, PAngleScanCoords(obj)^);

  if Assigned(grad) then
  begin
    gskc := 0.0;
    gskm := 0.0;
    gsks := 0.0;

    for iFD := Low(CFiniteDifferencesYFactor) to High(CFiniteDifferencesYFactor) do
    begin
      if iFD = 0 then
        Continue;

      fdx := iFD * CH;
      fdy := CFiniteDifferencesYFactor[iFD] / CH;

      gskc += GridSearchCorrect(ConstSkew + fdx, MulSkew, SqrSkew, PAngleScanCoords(obj)^) * fdy;
      gskm += GridSearchCorrect(ConstSkew, MulSkew + fdx, SqrSkew, PAngleScanCoords(obj)^) * fdy;
      gsks += GridSearchCorrect(ConstSkew, MulSkew, SqrSkew + fdx, PAngleScanCoords(obj)^) * fdy;
    end;

    grad[0] := gskc;
    grad[1] := gskm;
    grad[2] := gsks;
  end;

  //if Assigned(grad) then
  //  WriteLn(func:12:9, arg[0]:12:6, arg[1]:12:6, arg[2]:12:6, grad[0]:16:9, grad[1]:16:9, grad[2]:16:9)
  //else
  //  WriteLn(func:12:9, arg[0]:12:6, arg[1]:12:6, arg[2]:12:6);
end;

procedure TScanCorrelator.Correct;
var
  losses: TDoubleDynArray;
  coordsArray: array of TAngleScanCoords;
  doneCount, evalCount: Integer;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: PAngleScanCoords;
    loss: Double;
    scan: TInputScan;
    X, Extents: TVector;
  begin
    if not InRange(AIndex, 0, High(FPerAngleSkew)) then
      Exit;

    coords := @coordsArray[AIndex];
    if IsNan(coords^.StartAngle) or IsNan(coords^.EndAngle) then
      Exit;

    scan := FInputScans[coords^.ScanIdx];

    PrepareCorrect(coords^);

    X := [0.0, 1.0 * 1e3, 0.0];
    Extents := [0.015 * scan.DPI, 0.002 * 1e3, 5e-7 * 1e6];
    loss := GridReduceMinimize(@NelderMeadCorrect, X, [7, 7, 7], Extents, 0.001, '', coords);
    loss := NelderMeadMinimize(@NelderMeadCorrect, X, Extents, 1e-3, coords);

    // free up memory
    SetLength(coords^.RadiusAngleLUT, 0);
    SetLength(coords^.SinCosLUT, 0);
    SetLength(coords^.PreparedValues, 0);
    SetLength(coords^.Weights, 0);

    FPerAngleSkew[AIndex, 0].ConstSkew := X[0];
    FPerAngleSkew[AIndex, 0].MulSkew := X[1] * 1e-3;
    FPerAngleSkew[AIndex, 0].SqrSkew := X[2] * 1e-6;
    losses[AIndex] := loss;

    Write(InterlockedIncrement(doneCount):4, ' / ', evalCount, #13);
  end;

var
  iangle, iscan, ias, iasbase, validLossPos: Integer;
  valid: Boolean;
  validLosses: TDoubleDynArray;
  coords: PAngleScanCoords;
begin
  WriteLn('Correct');

  if Length(FInputScans) <= 1 then
    Exit;

  SetLength(FPerAngleSkew, CCorrectAngleCount * High(FInputScans), 1);
  SetLength(losses, Length(FPerAngleSkew));
  SetLength(coordsArray, Length(FPerAngleSkew));

  // init

  doneCount := 0;
  evalCount := 0;
  for iscan := 1 to High(FInputScans) do
    for iangle := 0 to CCorrectAngleCount - 1 do
    begin
      ias := (iscan - 1) * CCorrectAngleCount + iangle;

      coords := @coordsArray[ias];

      FillChar(coords^, SizeOf(coords^), 0);
      coords^.ScanIdx := iscan;
      coords^.AngleIdx := iangle;

      losses[ias] := -Infinity;
      FPerAngleSkew[ias, 0].ConstSkew := 0.0;
      FPerAngleSkew[ias, 0].MulSkew := 1.0;
      FPerAngleSkew[ias, 0].SqrSkew := 0.0;

      if InitCorrect(coords^) then
        Inc(evalCount);
    end;

  // compute

  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, High(FPerAngleSkew));
  WriteLn;

  // build a list of skew polynomials per scan / angle

  for iscan := 1 to High(FInputScans) do
    for iangle := 0 to CCorrectAngleCount - 1 do
    begin
      ias := (iscan - 1) * CCorrectAngleCount + iangle;

      iasbase := ias;
      while True do
      begin
        iasbase := (coordsArray[iasbase].BaseScanIdx - 1) * CCorrectAngleCount + iangle;

        if iasbase < 0 then
          Break;

        SetLength(FPerAngleSkew[ias], Length(FPerAngleSkew[ias]) + 1);
        FPerAngleSkew[ias, High(FPerAngleSkew[ias])] := FPerAngleSkew[iasbase, 0];
      end;
    end;

  // log

  validLossPos := 0;
  SetLength(validLosses, Length(losses));
  for iscan := 1 to High(FInputScans) do
    for iangle := 0 to CCorrectAngleCount - 1 do
    begin
      ias := (iscan - 1) * CCorrectAngleCount + iangle;
      valid := not IsInfinite(losses[ias]);

      if valid then
      begin
        validLosses[validLossPos] := losses[ias];
        Inc(validLossPos);
      end;

      Write(FInputScans[iscan].ImageShortName);
      Write(', Angle:', (iangle / CCorrectAngleCount) * 360.0:9:3);
      if valid then
        Write(', Const:', FPerAngleSkew[ias, 0].ConstSkew:9:3, ', Mul:', FPerAngleSkew[ias, 0].MulSkew:12:8, ', Sqr:', FPerAngleSkew[ias, 0].SqrSkew:12:8);
      WriteLn(', Loss:', losses[ias]:12:9);
    end;
  SetLength(validLosses, validLossPos);

  WriteLn('Mean loss:', Mean(validLosses):12:9, ', StdDev:', StdDev(validLosses):12:9, ', Worst loss:', MaxValue(validLosses):12:9);
end;

procedure TScanCorrelator.Rebuild;
const
  CLabelDepthBits = 4;
  CLabelDepthMaxValue = (1 shl CLabelDepthBits) - 1;
  CTauToAngleIdx = CCorrectAngleCount / (2.0 * Pi);
var
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

    c := tau * CTauToAngleIdx;
    ci := Trunc(c);
    alpha := c - ci;
    so := (scanIdx - 1) * CCorrectAngleCount;

    for iInterp := Low(r) to High(r) do
    begin
      pp := FPerAngleSkew[(ci + iInterp + CCorrectAngleCount) mod CCorrectAngleCount + so];

      rr := radius;
      for iPoly := 0 to High(pp) do
        rr := Sqr(rr) * pp[iPoly].SqrSkew + rr * pp[iPoly].MulSkew + pp[iPoly].ConstSkew;
      r[iInterp] := rr;
    end;

    Result := herp(r[-1], r[0], r[1], r[2], alpha);
  end;

  procedure DoY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iScan, ox, cnt, yx: Integer;
    r, sn, cs, px, py, acc, sample, bt, ct, rsk, d2d: Double;
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

          rsk := InterpolateSkew(bt, r * d2d, iScan);

          ct := NormalizeAngle(bt + scan.RelativeAngle);

          SinCos(ct, sn, cs);
          px := cs * rsk + scan.Center.X;
          py := sn * rsk + scan.Center.Y;

          sample := High(Word);
          if scan.InRangePointD(py, px) and
              (not InNormalizedAngle(ct, scan.CropData.StartAngle, scan.CropData.EndAngle) and
               not InNormalizedAngle(ct, scan.CropData.StartAngleMirror, scan.CropData.EndAngleMirror) or
               (r < rLbl)) then
          begin
            sample := scan.GetPointD_Sinc(scan.Image, py, px);

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

  FOutputWidth := Ceil(C45RpmOuterSize * FOutputDPI);
  FOutputHeight := FOutputWidth;
  SetLength(FOutputImage, sqr(FOutputWidth));
  for iScan := 0 to High(FOutputScans) do
    FOutputScans[iScan].InitImage(FOutputWidth, FOutputHeight, FOutputDPI);

  center := FOutputWidth / 2.0;
  rBeg := C45RpmAdapterSize * 0.5 * FOutputDPI;
  rEnd := C45RpmOuterSize * 0.5 * FOutputDPI;
  rLbl := C45RpmLabelOuterSize * 0.5 * FOutputDPI;

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


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

    X: TVector;

    SinCosLUT: TSinCosDynArray;
    PreparedData: TWordDynArray;
  end;

  PCorrectCoords = ^TCorrectCoords;

  { TScanCorrelator }

  TScanCorrelator = class
  private
    FProfileRef: TProfile;
    FInputScans: TInputScanDynArray;
    FFixCISScanners: Boolean;
    FAnalyzePass: Boolean;
    FLinearizePass: Boolean;
    FCorrectPass: Boolean;
    FRebuildScaled: Boolean;
    FRebuildBlendCount: Integer;
    FQualitySpeedRatio: Double;
    FOutputPNGFileName: String;
    FOutputDPI: Integer;
    FLock: TSpinlock;

    FAnalyzeStates: TStringDynArray;
    FPerAngleSkew: array of array of TCorrectSkew;

    FOutputWidth, FOutputHeight: Integer;
    FOutputImage: TWordDynArray;

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
    function GridSearchCorrect(const skew: TCorrectSkew; const coords: TCorrectCoords): Double;

    procedure AngleInit;
    procedure Analyze;
    procedure Crop;
    procedure Linearize;
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
    property AnalyzePass: Boolean read FAnalyzePass write FAnalyzePass;
    property LinearizePass: Boolean read FLinearizePass write FLinearizePass;
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

  for iScan := 0 to AFileNames.Count - 1 do
  begin
    FInputScans[iScan] := TInputScan.Create(FProfileRef, FOutputDPI, True);
    FInputScans[iScan].ImageFileName := AFileNames[iScan];
  end;

  SpinLeave(@FLock);

  FAngleInitAreaBegin := FProfileRef.InnerSize;
  FAngleInitAreaEnd := FProfileRef.LabelOuterSize;
  FAnalyzeAreaBegin := FProfileRef.InnerSize;
  FAnalyzeAreaEnd := FProfileRef.MinConcentricGroove;
  FAnalyzeAreaWidth := (FAnalyzeAreaEnd - FAnalyzeAreaBegin) * 0.5;
  FCorrectAreaBegin := FProfileRef.MinConcentricGroove;
  FCorrectAreaEnd := FProfileRef.OuterSize;
  FCorrectAreaWidth := (FCorrectAreaEnd - FCorrectAreaBegin) * 0.5;

  FFixCISScanners := False;
  FAnalyzePass := True;
  FLinearizePass := True;
  FCorrectPass := True;
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
    FInputScans[i].Free;

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
  CPrecisionAngleCount = 3600;
  CPica = 0.03; // inches
var
  rBeg, rEnd: Integer;
  baseRanks: TSpearmanRankDynArray;
  radiusAngleLut: TRadiusAngleDynArray;

  procedure DoAngle(Scan: TInputScan; a: Double; var arr: TDoubleDynArray);
  var
    iRadiusAngle: Integer;
     cy, cx, sky, skx, px, py: Double;
    sinCosLut: TSinCosDynArray;
  begin
    sinCosLut := OffsetRadiusAngleLUTAngle(radiusAngleLut, a);

    cx := Scan.Center.X;
    cy := Scan.Center.Y;
    skx := Scan.Skew.X;
    sky := Scan.Skew.Y;

    for iRadiusAngle := 0 to High(radiusAngleLut) do
    begin
      px := (cx + sinCosLut[iRadiusAngle].Cos * radiusAngleLut[iRadiusAngle].Radius) * skx;
      py := (cy + sinCosLut[iRadiusAngle].Sin * radiusAngleLut[iRadiusAngle].Radius) * sky;

      if Scan.InRangePointD(py, px) then
        arr[iRadiusAngle] := Scan.GetPointD_Work(Scan.ProcessedImage, py, px)
      else
        arr[iRadiusAngle] := 1e6;
    end;
  end;

  procedure DoScan(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iAngle: Integer;
    t, f, bestf, bestAngle: Double;
    angle: TDoubleDynArray;
    angleRanks: TSpearmanRankDynArray;
    scan: TInputScan;
  begin
    if not InRange(AIndex, 1, High(FInputScans)) then
      Exit;

    scan := FInputScans[AIndex];

    SetLength(angle, Length(baseRanks));
    SetLength(angleRanks, Length(baseRanks));

    bestf := Infinity;
    bestAngle := 0.0;

    for iAngle := 0 to CPrecisionAngleCount - 1 do
    begin
      t := iAngle * (2.0 * Pi / CPrecisionAngleCount);

      DoAngle(scan, t, angle);

      SpearmanPrepareRanks(angle, angleRanks);
      f := -SpearmanRankCorrelation(baseRanks, angleRanks);

      if f <= bestf then
      begin
        bestf := f;
        bestAngle := t;
      end;
    end;

    FInputScans[AIndex].CorrectByModel(NormalizeAngle(bestAngle), NaN, NaN, NaN, NaN);
  end;

var
  iScan: Integer;
  base: TDoubleDynArray;
  scan: TInputScan;
begin
  WriteLn('AngleInit');

  if Length(FInputScans) <= 0 then
    Exit;

  rBeg := Round(FAngleInitAreaBegin * 0.5 * FInputScans[0].DPI);
  rEnd := Round(FAngleInitAreaEnd * 0.5 * FInputScans[0].DPI);
  radiusAngleLut := BuildRadiusAngleLUT(rBeg, rEnd, -Pi, Pi, CPica * FInputScans[0].DPI);

  SetLength(base, Length(radiusAngleLut));
  DoAngle(FInputScans[0], 0, base);

  SetLength(baseRanks, Length(base));
  SpearmanPrepareRanks(base, baseRanks);

  ProcThreadPool.DoParallelLocalProc(@DoScan, 1, High(FInputScans));

  for iScan := 0 to High(FInputScans) do
  begin
    scan := FInputScans[iScan];

    WriteLn(scan.ImageShortName, ', Angle: ', RadToDeg(scan.RelativeAngle):9:3, ', CenterX: ', scan.Center.X:9:3, ', CenterY: ', scan.Center.Y:9:3, ', SkewX: ', scan.Skew.X:9:6, ', SkewY: ', scan.Skew.Y:9:6);
  end;
end;

function TScanCorrelator.PrepareAnalyze: TDoubleDynArray;
var
  iRadiusAngle, pos, cnt, angleCnt, radiusCnt: Integer;
  t, rBeg, rEnd, px, py, cx, cy, skx, sky, r, ri, sn, cs: Double;
  sinCosLUT: TSinCosDynArray;
  baseScan: TInputScan;
  baseMeanSD: TPointD;
begin
  baseScan := FInputScans[0];

  rBeg := FAnalyzeAreaBegin * 0.5 * baseScan.DPI;
  rEnd := FAnalyzeAreaEnd * 0.5 * baseScan.DPI;

  angleCnt := Ceil(rEnd * 2.0 * Pi * FQualitySpeedRatio);
  radiusCnt := Ceil(FAnalyzeAreaWidth * baseScan.DPI);
  cnt := radiusCnt * angleCnt;
  SetLength(Result, cnt);

  t := baseScan.RelativeAngle;
  cx := baseScan.Center.X;
  cy := baseScan.Center.Y;
  skx := baseScan.Skew.X;
  sky := baseScan.Skew.Y;

  BuildSinCosLUT(angleCnt, sinCosLUT, t);
  baseMeanSD := baseScan.GetMeanSD(baseScan.Image, FAnalyzeAreaBegin * 0.5 * baseScan.DPI, FAnalyzeAreaEnd * 0.5 * baseScan.DPI, -Pi, Pi, CAnalyzeSigma);

  pos := 0;
  ri := 1.0 / angleCnt;
  for iRadiusAngle := 0 to High(Result) do
  begin
    cs := sinCosLUT[pos].Cos;
    sn := sinCosLUT[pos].Sin;

    r := rBeg + iRadiusAngle * ri;

    px := (cs * r + cx) * skx;
    py := (sn * r + cy) * sky;

    if baseScan.InRangePointD(py, px) then
    begin
      Result[iRadiusAngle] := (baseScan.GetPointD_Work(baseScan.Image, py, px) - baseMeanSD.X) * baseMeanSD.Y;
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

      px := (cs * r + cx) * skx;
      py := (sn * r + cy) * sky;

      imgInt := (scan.GetPointD_Work(scan.Image, py, px) - coords^.MeanSD.X) * coords^.MeanSD.Y;
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

  rBeg := FAnalyzeAreaBegin * 0.5 * scan.DPI;
  rEnd := FAnalyzeAreaEnd * 0.5 * scan.DPI;

  angleCnt := Ceil(rEnd * 2.0 * Pi * FQualitySpeedRatio);
  radiusCnt := Ceil(FAnalyzeAreaWidth * scan.DPI);

  ri := 1.0 / angleCnt;

  t := arg[0];
  cx := arg[1];
  cy := arg[2];
  skx := arg[3];
  sky := arg[4];

  if not scan.InRangePointD(cy - rEnd * sky, cx - rEnd * skx) or not scan.InRangePointD(cy + rEnd * sky, cx + rEnd * skx) then
    Exit(1e6);

  BuildSinCosLUT(angleCnt, sinCosLUT, t);

  SetLength(results, radiusCnt);
  ProcThreadPool.DoParallelLocalProc(@DoSpiral, 0, radiusCnt - 1);

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
      Write(' ', FAnalyzeStates[iScan], FInputScans[iScan].Objective:12:9);
    Write(#13);
  finally
    SpinLeave(@FLock);
  end;
end;

function CompareInputScansRelAngle(Item1, Item2, UserParameter: Pointer): Integer;
var
  s1: ^TInputScan absolute Item1;
  s2: ^TInputScan absolute Item2;
begin
  Result := CompareValue(NormalizedAngleTo02Pi(s1^.RelativeAngle), NormalizedAngleTo02Pi(s2^.RelativeAngle));
end;

procedure TScanCorrelator.Analyze;
var
  preparedData: TDoubleDynArray;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: TAnalyzeCoords;
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

    FAnalyzeStates[AIndex] := 'Work';
    X := [scan.RelativeAngle, scan.Center.X, scan.Center.Y, scan.Skew.X, scan.Skew.Y];
    NelderMeadMinimize(@NelderMeadAnalyze, X, [DegToRad(1.0), 0.02 * scan.DPI, 0.02 * scan.DPI, 0.001, 0.001], 1e-6, @coords);

    FAnalyzeStates[AIndex] := 'Done';
    scan.Objective := NelderMeadAnalyze(X, @coords);
    scan.CorrectByModel(X[0], X[1], X[2], X[3], X[4]);
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
  SetLength(FAnalyzeStates, Length(FInputScans));
  for iScan := 0 to High(FInputScans) do
    FAnalyzeStates[iScan] := 'Wait';

  // compute

  ProcThreadPool.DoParallelLocalProc(@DoEval, 1, High(FInputScans));
  WriteLn;

  // log

  for iScan := 0 to High(FInputScans) do
  begin
    scan := FInputScans[iScan];

    WriteLn(scan.ImageShortName, ', Angle: ', RadToDeg(scan.RelativeAngle):9:3, ', CenterX: ', scan.Center.X:9:3, ', CenterY: ', scan.Center.Y:9:3, ', SkewX: ', scan.Skew.X:9:6, ', SkewY: ', scan.Skew.Y:9:6, ', RMSE: ', scan.Objective:12:9);
  end;

  // sort scans by relative angle for next algos

  QuickSort(FInputScans[0], 1, High(FInputScans), SizeOf(TInputScan), @CompareInputScansRelAngle);
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
  rBeg, rEnd, s1, s2: Double;
  c: TCropData;
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
  begin
    c := FInputScans[i].CropData;
    s1 := NormalizedAngleDiff(c.StartAngle, c.EndAngle);
    s2 := NormalizedAngleDiff(c.StartAngleMirror, c.EndAngleMirror);
    WriteLn(FInputScans[i].ImageShortName, ', begin:', RadToDeg(c.StartAngle):9:3, ', end:', RadToDeg(c.EndAngle):9:3, ', size:', RadToDeg(s1):9:3, ', begin2:', RadToDeg(c.StartAngleMirror):9:3, ', end2:', RadToDeg(c.EndAngleMirror):9:3, ', size2:', RadToDeg(s2):9:3);
  end;
end;

procedure TScanCorrelator.Linearize;
var
  iScan: Integer;
begin
 WriteLn('Linearize');
 for iScan := 0 to High(FInputScans) do
   FInputScans[iScan].Linearize;
end;

procedure TScanCorrelator.CorrectAnglesFromCoords(const coords: TCorrectCoords; out AStartAngle, AEndAngle,
  angleInc: Double; out angleCnt: Integer; AReduceAngles: Boolean);
var
  croppedCnt: Integer;
  angle, angleExtents, startAngle, endAngle, a0a, a1a, a0b, a1b: Double;
  scan: TInputScan;
begin
  angle := (coords.AngleIdx / FProfileRef.CorrectAngleCount) * 2.0 * Pi;
  angleExtents := 0.5 * 2.0 * Pi / FProfileRef.CorrectAngleCount;
  startAngle := NormalizeAngle(angle - angleExtents);
  endAngle := NormalizeAngle(angle + angleExtents);

  if AReduceAngles then
  begin
    scan := FInputScans[coords.ScanIdx];

    startAngle := startAngle;
    endAngle := endAngle;

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
  Result.MulSkew := arg[1];
end;

function TScanCorrelator.SkewToArg(const skew: TCorrectSkew): TVector;
begin
  Result := [skew.ConstSkew, skew.MulSkew];
end;

function TScanCorrelator.InitCorrect(var coords: TCorrectCoords; AReduceAngles: Boolean): Boolean;
var
  skew: TCorrectSkew;
  baseCoords: TCorrectCoords;
  objectives: TDoubleDynArray;

  procedure DoResults(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    tmpCoords: TCorrectCoords;
    baseScan: TInputScan;
  begin
    if not InRange(AIndex, 0, High(FInputScans)) then
      Exit;

    baseScan := FInputScans[AIndex];

    tmpCoords := baseCoords;
    tmpCoords.ScanIdx := AIndex;

    if tmpCoords.BaseScanIdx <> tmpCoords.ScanIdx then
    begin
      tmpCoords.SinCosLUT := nil;
      BuildSinCosLUT(tmpCoords.AngleCnt, tmpCoords.sinCosLUT, tmpCoords.StartAngle + baseScan.RelativeAngle, NormalizedAngleDiff(tmpCoords.StartAngle, tmpCoords.EndAngle));

      objectives[AIndex] := GridSearchCorrect(skew, tmpCoords);
    end
    else
    begin
      objectives[AIndex] := NaN;
    end;
  end;

var
  iAngle, iBaseScan: Integer;
  t, bt, v, best: Double;
  curScan, baseScan: TInputScan;
begin
  Result := True;
  Coords.BaseScanIdx := -1;
  curScan := FInputScans[Coords.ScanIdx];

  CorrectAnglesFromCoords(coords, coords.StartAngle, coords.EndAngle, coords.AngleInc, coords.AngleCnt, AReduceAngles);
  coords.RadiusCnt := Ceil(FCorrectAreaWidth * curScan.DPI);

  FillChar(skew, SizeOf(skew), 0);
  coords.X := SkewToArg(skew);

  if IsNan(coords.StartAngle) or IsNan(coords.EndAngle) then
    Exit(False);

  // prepare init objectives for all scans

  baseCoords := coords;
  baseCoords.BaseScanIdx := coords.ScanIdx;
  PrepareCorrect(baseCoords);

  SetLength(objectives, Length(FInputScans));
  ProcThreadPool.DoParallelLocalProc(@DoResults, 0, High(FInputScans));

  // devise best baseScan

  best := Infinity;
  for iBaseScan := 0 to Coords.ScanIdx - 1 do
  begin
    baseScan := FInputScans[iBaseScan];

    v := objectives[iBaseScan];

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
end;

procedure TScanCorrelator.PrepareCorrect(var coords: TCorrectCoords);
var
  iRadius, iAngle, pos: Integer;
  rBeg, r, sn, cs, px, py, cx, cy, skx, sky: Double;
  curScan, baseScan: TInputScan;
begin
  baseScan := FInputScans[Coords.BaseScanIdx];
  curScan := FInputScans[Coords.ScanIdx];

  // build sin / cos lookup table

  BuildSinCosLUT(coords.AngleCnt, coords.SinCosLUT, coords.StartAngle + baseScan.RelativeAngle, NormalizedAngleDiff(coords.StartAngle, coords.EndAngle));

  // parse image arcs

  SetLength(coords.PreparedData, coords.AngleCnt * coords.RadiusCnt);

  cx := baseScan.Center.X;
  cy := baseScan.Center.Y;
  skx := baseScan.Skew.X;
  sky := baseScan.Skew.Y;

  rBeg := FCorrectAreaBegin * 0.5 * baseScan.DPI;

  pos := 0;
  for iAngle := 0 to coords.AngleCnt - 1 do
  begin
    cs := coords.SinCosLUT[iAngle].Cos;
    sn := coords.SinCosLUT[iAngle].Sin;

    for iRadius := 0 to coords.RadiusCnt - 1 do
    begin
      r := rBeg + iRadius;

      px := (cs * r + cx) * skx;
      py := (sn * r + cy) * sky;

      if baseScan.InRangePointD(py, px) then
        coords.PreparedData[pos] := baseScan.ProcessedImage[Trunc(py) * baseScan.Width + Trunc(px)]
      else
        coords.PreparedData[pos] := 0;

      Inc(pos);
    end;
  end;
  Assert(pos = Length(coords.PreparedData));

  // prepare iterations

  BuildSinCosLUT(coords.AngleCnt, coords.sinCosLUT, coords.StartAngle + curScan.RelativeAngle, NormalizedAngleDiff(coords.StartAngle, coords.EndAngle));
end;

function TScanCorrelator.GridSearchCorrect(const skew: TCorrectSkew; const coords: TCorrectCoords): Double;
var
  iRadius, iScan, iAngle, pos: Integer;
  r, rBeg, rEnd, skx, sky, cx, cy, rsk: Double;
  sn, cs, px, py, cskx, csky: Single;
  scan: TInputScan;
  acc: UInt64;
  skewedRadiuses: array of TPointF;
begin
  iScan := coords.ScanIdx;
  scan := FInputScans[iScan];

  cx := scan.Center.X;
  cy := scan.Center.Y;
  skx := scan.Skew.X;
  sky := scan.Skew.Y;

  rBeg := FCorrectAreaBegin * 0.5 * scan.DPI;
  if not InRange(SkewRadius(rBeg, skew), rBeg * CScannerTolLo, rBeg * CScannerTolHi) then
    Exit(1e6);

  rEnd := FCorrectAreaEnd * 0.5 * scan.DPI;
  if not InRange(SkewRadius(rEnd, skew), rEnd * CScannerTolLo, rEnd * CScannerTolHi) then
    Exit(1e6);

  // prepare final radius LUT

  cskx := cx * skx;
  csky := cy * sky;

  SetLength(skewedRadiuses, coords.RadiusCnt);
  for iRadius := 0 to coords.RadiusCnt - 1 do
  begin
    r := rBeg + iRadius;

    rsk := SkewRadius(r, skew);

    skewedRadiuses[iRadius].X := rsk * skx;
    skewedRadiuses[iRadius].Y := rsk * sky;
  end;

  // parse image arcs

  acc := 0;
  pos := 0;
  for iAngle := 0 to coords.AngleCnt - 1 do
  begin
    cs := coords.SinCosLUT[iAngle].Cos;
    sn := coords.SinCosLUT[iAngle].Sin;

    for iRadius := 0 to coords.RadiusCnt - 1 do
    begin
      px := cs * skewedRadiuses[iRadius].X + cskx;
      py := sn * skewedRadiuses[iRadius].Y + csky;

      acc += Sqr(coords.PreparedData[pos] - scan.ProcessedImage[Trunc(py) * scan.Width + Trunc(px)]);

      Inc(pos);
    end;
  end;
  Assert(pos = Length(coords.PreparedData));

  Result := Sqrt(acc / pos) / High(Word);
end;

procedure TScanCorrelator.Correct;
const
  CConstExtents = 0.02; // inches
  CConstHalfCount = 48;
  CMulExtents = 0.01;
  CMulHalfCount = 100;
var
  rmses: TDoubleDynArray;
  coordsArray: array of TCorrectCoords;
  validAngleCnt, doneCount: Integer;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: PCorrectCoords;
    gsData: array of record
      Skew: TCorrectSkew;
      Objective: Double;
    end;

    procedure DoGS(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
    begin
      if not InRange(AIndex, 0, High(gsData)) then
        Exit;

      gsData[AIndex].Objective := GridSearchCorrect(gsData[AIndex].Skew, coords^);
    end;

  var
    iConst, iMul, iGS: Integer;
    loss: Double;
    skew, tmpSk: TCorrectSkew;
    scan: TInputScan;
  begin
    if not InRange(AIndex, 0, High(FPerAngleSkew)) then
      Exit;

    coords := @coordsArray[AIndex];
    scan := FInputScans[coords^.ScanIdx];

    loss := NaN;

    if not IsNan(coords^.StartAngle) and not IsNan(coords^.EndAngle) then
    begin
      PrepareCorrect(coords^);

      // prepare grid search iterations

      skew := ArgToSkew(coords^.X);
      SetLength(gsData, (CMulHalfCount * 2 + 1) * (CConstHalfCount * 2 + 1));

      iGS := 0;
      for iMul := -CMulHalfCount to CMulHalfCount do
      begin
        tmpSk := skew;
        tmpSk.MulSkew := iMul * CMulExtents / CMulHalfCount;

        for iConst := -CConstHalfCount to CConstHalfCount do
        begin
          tmpSk.ConstSkew := iConst * CConstExtents / CConstHalfCount * scan.DPI;

          gsData[iGS].Skew := tmpSk;
          gsData[iGS].Objective := NaN;

          Inc(iGS);
        end;
      end;
      Assert(iGS = Length(gsData));

      // grid search iterations

      ProcThreadPool.DoParallelLocalProc(@DoGS, 0, High(gsData));

      // find best iteration

      loss := Infinity;
      for iGS := 0 to High(gsData) do
        if gsData[iGS].Objective < loss then
        begin
          skew := gsData[iGS].Skew;
          loss := gsData[iGS].Objective;
        end;

      coords^.X := SkewToArg(skew);

      // free up memory
      SetLength(coords^.SinCosLUT, 0);
      SetLength(coords^.PreparedData, 0);

      Write(InterlockedIncrement(doneCount):4, ' / ', validAngleCnt, #13);
    end;

    rmses[AIndex] := loss;
    FPerAngleSkew[AIndex, 0] := ArgToSkew(coords^.X);
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
  center, rBeg, rEnd, rLim: Double;

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
          px := (cs * rSkew + scan.Center.X) * scan.Skew.X;
          py := (sn * rSkew + scan.Center.Y) * scan.Skew.Y;

          sample := High(Word);
          if scan.InRangePointD(py, px) and
              (not InNormalizedAngle(ct, scan.CropData.StartAngle, scan.CropData.EndAngle) and
               not InNormalizedAngle(ct, scan.CropData.StartAngleMirror, scan.CropData.EndAngleMirror) or
               (r < rLim)) then
          begin
            sample := scan.GetPointD_Final(scan.ProcessedImage, py, px);

            if cnt < FRebuildBlendCount then
            begin
              acc += sample;
              Inc(cnt);
            end;
          end;
        end;

        acc := DivDef(acc, cnt, High(Word));

        if r >= rLim then
          FOutputImage[yx + ox] := EnsureRange(Round(acc), 0, High(Word))
        else
          FOutputImage[yx + ox] := EnsureRange(Round(acc * CLabelDepthMaxValue / High(Word)), 0, CLabelDepthMaxValue) * High(Word) div CLabelDepthMaxValue; // lower bit depth for label
      end
      else
      begin
        // dark outside the disc, inner inside
        sample := IfThen(r >= rLim, Round(0.25 * High(Word)), Round(1.0 * High(Word)));
        FOutputImage[yx + ox] := Round(sample);
      end;
    end;
  end;

begin
  WriteLn('Rebuild');

  TauToAngleIdx := FProfileRef.CorrectAngleCount / (2.0 * Pi);

  FOutputWidth := Ceil(FProfileRef.OuterSize * FOutputDPI);
  FOutputHeight := FOutputWidth;
  SetLength(FOutputImage, sqr(FOutputWidth));

  center := FOutputWidth / 2.0;
  rBeg := FProfileRef.AdapterSize * 0.5 * FOutputDPI;
  rEnd := FProfileRef.OuterSize * 0.5 * FOutputDPI;
  rLim := FProfileRef.MinConcentricGroove * 0.5 * FOutputDPI;

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

begin
  DoSave(FOutputPNGFileName, FOutputImage);

  WriteLn('Done!');
end;

procedure TScanCorrelator.Process;
begin
  AngleInit;
  if FAnalyzePass then Analyze;
  if FLinearizePass then Linearize;
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


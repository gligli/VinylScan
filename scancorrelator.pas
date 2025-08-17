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

  TCorrectCoords = record
    AngleIdx, ScanIdx, BaseScanIdx: Integer;
    PreparedData: TDoubleDynArray;
    SinCosLUT: TSinCosDynArray;
    Weights: TDoubleDynArray;
    ConstSkew, MulSkew, SqrSkew: Double;
    Silent: Boolean;
  end;

  PCorrectCoords = ^TCorrectCoords;

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

    procedure CorrectAnglesFromCoords(const coords: TCorrectCoords; out AStartAngle, AEndAngle, angleInc: Double; out
      angleCnt: Integer; AReduceAngles: Boolean);

    function PrepareAnalyze: TDoubleDynArray;
    function NelderMeadAnalyze(const arg: TVector; obj: Pointer): TScalar;
    function PrepareCorrect(var coords: TCorrectCoords): Boolean;
    function NelderMeadCorrect(const arg: TVector; obj: Pointer): TScalar;
    function GridSearchCorrectConst(const arg: TVector; obj: Pointer): TScalar;
    function GridSearchCorrectMul(const arg: TVector; obj: Pointer): TScalar;
    function GridSearchCorrectSqr(const arg: TVector; obj: Pointer): TScalar;

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
  CAngleInitAreaBegin = C45RpmInnerSize;
  CAngleInitAreaEnd = C45RpmLabelOuterSize;

  CAnalyzeAreaBegin = C45RpmInnerSize;
  CAnalyzeAreaEnd = C45RpmConcentricGroove;
  CAnalyzeAreaWidth = (CAnalyzeAreaEnd - CAnalyzeAreaBegin) * 0.5;

  CCorrectAngleCount = 36;
  CCorrectAreaBegin = C45RpmLabelOuterSize;
  CCorrectAreaEnd = C45RpmOuterSize;
  CCorrectAreaWidth = (CCorrectAreaEnd - CCorrectAreaBegin) * 0.5;

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
  FBrickwallLimitScans := True;
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
var
  rBeg, rEnd: Integer;
  base: TDoubleDynArray;

  function DoAngle(Scan: TInputScan; a: Double; var arr: TDoubleDynArray): Integer;
  var
    iRadius, iAngle: Integer;
    sn, cs, cy, cx, px, py: Double;
  begin
    Result := 0;

    FillQWord(arr[0], Length(arr), 0);

    cx := Scan.Center.X;
    cy := Scan.Center.Y;
    for iAngle := 0 to CAngleCount - 1 do
    begin
      SinCos(a + DegToRad(iAngle * (360 / CAngleCount)), sn, cs);

      for iRadius:= rBeg to rEnd do
      begin
        px := cx + cs * iRadius;
        py := cy + sn * iRadius;
        if Scan.InRangePointD(py, px) then
          arr[Result] := Scan.GetPointD_Work(Scan.ProcessedImage, py, px);
        Inc(Result);
      end;
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

  rBeg := Round(CAngleInitAreaBegin * 0.5 * FInputScans[0].DPI);
  rEnd := Round(CAngleInitAreaEnd * 0.5 * FInputScans[0].DPI);

  pos := DoAngle(FInputScans[0], 0, base);

  SetLength(base, pos);

  ProcThreadPool.DoParallelLocalProc(@DoScan, 1, High(FInputScans));
end;

function TScanCorrelator.PrepareAnalyze: TDoubleDynArray;
var
  iRadius, pos, cnt, angleCnt: Integer;
  t, rBeg, px, py, cx, cy, r, ri, sn, cs: Double;
  sinCosLUT: TSinCosDynArray;
  scan: TInputScan;
begin
  scan := FInputScans[0];

  angleCnt := Ceil(scan.PointsPerRevolution * FQualitySpeedRatio);
  cnt := Ceil(CAnalyzeAreaWidth * scan.DPI * angleCnt);
  SetLength(Result, cnt);

  t   := scan.RelativeAngle;
  cx  := scan.Center.X;
  cy  := scan.Center.Y;

  BuildSinCosLUT(angleCnt, sinCosLUT, t);

  pos := 0;
  rBeg := CAnalyzeAreaBegin * 0.5 * scan.DPI;
  ri := 1.0 / angleCnt;
  for iRadius := 0 to High(Result) do
  begin
    cs := sinCosLUT[pos].Cos;
    sn := sinCosLUT[pos].Sin;

    r := rBeg + iRadius * ri;

    px := cs * r + cx;
    py := sn * r + cy;

    if scan.InRangePointD(py, px) then
    begin
      Result[iRadius] := scan.GetPointD_Work(scan.ProcessedImage, py, px);
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

  iScan, iRadius, pos, cnt, angleCnt: Integer;
  t, rBeg, px, py, cx, cy, r, ri, sn, cs: Double;
  sinCosLUT: TSinCosDynArray;
  scan: TInputScan;
begin
  scan := FInputScans[coords^.ScanIdx];
  Result := 0.0;

  angleCnt := Ceil(scan.PointsPerRevolution * FQualitySpeedRatio);
  cnt := Ceil(CAnalyzeAreaWidth * scan.DPI * angleCnt);

  t := arg[0];
  cx := arg[1];
  cy := arg[2];

  BuildSinCosLUT(angleCnt, sinCosLUT, t);

  pos := 0;
  rBeg := CAnalyzeAreaBegin * 0.5 * scan.DPI;
  ri := 1.0 / angleCnt;
  for iRadius := 0 to cnt - 1 do
  begin
    cs := sinCosLUT[pos].Cos;
    sn := sinCosLUT[pos].Sin;

    r := rBeg + iRadius * ri;

    px := cs * r + cx;
    py := sn * r + cy;

    if scan.InRangePointD(py, px) then
    begin
      Result += Sqr((coords^.PreparedData[iRadius] - scan.GetPointD_Work(scan.ProcessedImage, py, px)) * (1.0 / High(Word)));
    end
    else
    begin
      Result += Sqr(1e6);
    end;

    Inc(pos);

    if pos >= angleCnt then
      pos := 0;
  end;

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

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: TCorrectCoords;
    func: Double;
    x: TVector;
    scan: TInputScan;
  begin
    if not InRange(AIndex, 1, High(FInputScans)) then
      Exit;

    scan := FInputScans[AIndex];

    coords.AngleIdx := -1;
    coords.ScanIdx := AIndex;
    coords.Silent := False;
    coords.PreparedData := preparedData;

    x := [scan.RelativeAngle, scan.Center.X, scan.Center.Y];
    func := NelderMeadMinimize(@NelderMeadAnalyze, X, [DegToRad(1.0), 0.02 * scan.DPI, 0.02 * scan.DPI], 1e-6, @coords);

    scan.Objective := func;
    scan.CorrectByModel(x[1], x[2], x[0], NaN, NaN);
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

  rBeg := C45RpmLastMusicGroove * 0.5 * FInputScans[0].DPI;
  rEnd := C45RpmFirstMusicGroove * 0.5 * FInputScans[0].DPI;

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

function TScanCorrelator.PrepareCorrect(var coords: TCorrectCoords): Boolean;
var
  iRadius, iAngle, iBaseScan, cnt, radiusCnt, angleCnt, v, best, dummyAC: Integer;
  t, bt, rBeg, r, sn, cs, px, py, cx, cy, startAngle, endAngle, saRaw, eaRaw, angleInc, dummyAI: Double;
  curScan, baseScan: TInputScan;
begin
  Result := True;
  Coords.BaseScanIdx := 0;
  curScan := FInputScans[Coords.ScanIdx];

  CorrectAnglesFromCoords(coords, startAngle, endAngle, angleInc, angleCnt, True);

  if IsNan(startAngle) or IsNan(endAngle) then
    Exit(False);

  radiusCnt := Ceil(CCorrectAreaWidth * curScan.DPI);

  SetLength(coords.PreparedData, radiusCnt * angleCnt);

  // devise best baseScan

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

      if InNormalizedAngle(bt, startAngle, endAngle) then
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

  // build sin / cos lookup table

  t := baseScan.RelativeAngle;
  BuildSinCosLUT(angleCnt, coords.SinCosLUT, startAngle + t, NormalizedAngleDiff(startAngle, endAngle));

  // build weights lookup table

  CorrectAnglesFromCoords(Coords, saRaw, eaRaw, dummyAI, dummyAC, False);

  SetLength(Coords.Weights, angleCnt);

  bt := startAngle;
  for iAngle := 0 to angleCnt - 1 do
  begin
    Coords.Weights[iAngle] := 1.0 - 2.0 * abs(NormalizedAngleDiff(saRaw, NormalizeAngle(bt)) / NormalizedAngleDiff(saRaw, eaRaw) - 0.5);
    Coords.Weights[iAngle] *= 1.0 / High(Word);
    bt += angleInc;
  end;

  // parse image arcs

  cx := baseScan.Center.X;
  cy := baseScan.Center.Y;

  cnt := 0;
  rBeg := CCorrectAreaBegin * 0.5 * curScan.DPI;
  for iRadius := 0 to radiusCnt - 1 do
  begin
    r := rBeg + iRadius;

    for iAngle := 0 to angleCnt - 1 do
    begin
      cs := coords.SinCosLUT[iAngle].Cos;
      sn := coords.SinCosLUT[iAngle].Sin;

      px := cs * r + cx;
      py := sn * r + cy;

      if baseScan.InRangePointD(py, px) then
        coords.PreparedData[cnt] := baseScan.GetPointD_Work(baseScan.ProcessedImage, py, px)
      else
        coords.PreparedData[cnt] := 1e6;

      Inc(cnt);
    end;
  end;
  Assert(cnt = radiusCnt * angleCnt);

  // prepare iterations

  BuildSinCosLUT(angleCnt, coords.sinCosLUT, startAngle + curScan.RelativeAngle, NormalizedAngleDiff(startAngle, endAngle));
end;

function TScanCorrelator.NelderMeadCorrect(const arg: TVector; obj: Pointer): TScalar;
var
  coords: PCorrectCoords absolute obj;
  cnt, iRadius, iScan, iAngle, radiusCnt: Integer;
  r, rBeg, sn, cs, px, py, cx, cy, rsk, skc, skm, sks: Double;
  scan: TInputScan;
begin
  iScan := coords^.ScanIdx;
  scan := FInputScans[iScan];

  skc := arg[0];
  skm := arg[1];
  sks := arg[2];

  cx := scan.Center.X;
  cy := scan.Center.Y;

  radiusCnt := Ceil(CCorrectAreaWidth * scan.DPI);

  // parse image arcs

  Result := 0;
  cnt := 0;
  rBeg := CCorrectAreaBegin * 0.5 * scan.DPI;
  for iRadius := 0 to radiusCnt - 1 do
  begin
    r := rBeg + iRadius;

    rsk := sqr(r) * sks + r * skm + skc;

    for iAngle := 0 to High(coords^.SinCosLUT) do
    begin
      cs := coords^.SinCosLUT[iAngle].Cos;
      sn := coords^.SinCosLUT[iAngle].Sin;

      px := cs * rsk + cx;
      py := sn * rsk + cy;

      if scan.InRangePointD(py, px) then
      begin
        Result += Sqr((coords^.PreparedData[cnt] - scan.GetPointD_Work(scan.ProcessedImage, py, px)) * coords^.Weights[iAngle]);
      end
      else
      begin
        Result += Sqr(1e6);
      end;

      Inc(cnt);
    end;
  end;

  Assert(cnt = radiusCnt * Length(coords^.SinCosLUT));

  Result /= cnt;
  Result := Sqrt(Result);
end;

function TScanCorrelator.GridSearchCorrectConst(const arg: TVector; obj: Pointer): TScalar;
var
  coords: PCorrectCoords absolute obj;
begin
  Result := NelderMeadCorrect([arg[0], coords^.MulSkew, coords^.SqrSkew], obj);
end;

function TScanCorrelator.GridSearchCorrectMul(const arg: TVector; obj: Pointer): TScalar;
var
  coords: PCorrectCoords absolute obj;
begin
  Result := NelderMeadCorrect([coords^.ConstSkew, arg[0], coords^.SqrSkew], obj);
end;

function TScanCorrelator.GridSearchCorrectSqr(const arg: TVector; obj: Pointer): TScalar;
var
  coords: PCorrectCoords absolute obj;
begin
  Result := NelderMeadCorrect([coords^.ConstSkew, coords^.MulSkew, arg[0]], obj);
end;

procedure TScanCorrelator.Correct;
const
  CConstCorrectExtents = 0.02; // inches
  CConstCorrectHalfCount = 50;
  CMulCorrectExtents = 0.02;
  CMulCorrectHalfCount = 50;
var
  rmses: TDoubleDynArray;
  coordsArray: array of TCorrectCoords;
  doneCount: Integer;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: TCorrectCoords;
    scan: TInputScan;
    X, Extents: TVector;
    iMul, iConst: Integer;
    skc, skm, loss, f: Double;
  begin
    if not InRange(AIndex, 0, High(FPerAngleSkew)) then
      Exit;

    FillChar(coords, SizeOf(coords), 0);

    DivMod(AIndex, CCorrectAngleCount, coords.ScanIdx, coords.AngleIdx);
    Inc(coords.ScanIdx);

    X := [0.0, 1.0, 0.0];

    if not PrepareCorrect(coords) then
    begin
      rmses[AIndex] := NaN;
      coordsArray[AIndex] := coords;
    end
    else
    begin
      scan := FInputScans[coords.ScanIdx];

      coords.ConstSkew := X[0];
      coords.MulSkew := X[1];
      coords.SqrSkew := X[2];
      loss := Infinity;

      for iMul := -CMulCorrectHalfCount to CMulCorrectHalfCount do
      begin
        skm := iMul * CMulCorrectExtents / CMulCorrectHalfCount + 1.0;

        f := GridSearchCorrectMul([skm], @coords);

        if f < loss then
        begin
          loss := f;
          coords.MulSkew := skm;
        end;
      end;

      for iConst := -CConstCorrectHalfCount to CConstCorrectHalfCount do
      begin
        skc := iConst * CConstCorrectExtents / CConstCorrectHalfCount * FOutputDPI;

        f := GridSearchCorrectConst([skc], @coords);

        if f < loss then
        begin
          loss := f;
          coords.ConstSkew := skc;
        end;
      end;

      X := [coords.ConstSkew, coords.MulSkew, coords.SqrSkew];
      Extents := [0.01 * scan.DPI, 0.01, 1e-5];
      //loss := GridReduceMinimize(@NelderMeadCorrect, X, [7, 7, 7], Extents, 0.001, '', @coords);
      loss := NelderMeadMinimize(@NelderMeadCorrect, X, Extents, 1e-12, @coords);

      // free up memory
      SetLength(coords.PreparedData, 0);
      SetLength(coords.SinCosLUT, 0);
      SetLength(coords.Weights, 0);

      rmses[AIndex] := loss;
      coordsArray[AIndex] := coords;
    end;

    FPerAngleSkew[AIndex, 0].ConstSkew := X[0];
    FPerAngleSkew[AIndex, 0].MulSkew := X[1];
    FPerAngleSkew[AIndex, 0].SqrSkew := X[2];

    Write(InterlockedIncrement(doneCount):4, ' / ', Length(FPerAngleSkew), #13);
  end;

var
  iangle, iscan, ias, iasbase, validRmseCnt: Integer;
  validRmses: TDoubleDynArray;
begin
  WriteLn('Correct');

  if Length(FInputScans) <= 1 then
    Exit;

  SetLength(FPerAngleSkew, CCorrectAngleCount * High(FInputScans), 1);
  SetLength(rmses, Length(FPerAngleSkew));
  SetLength(coordsArray, Length(FPerAngleSkew));

  // compute

  doneCount := 0;
  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, High(FPerAngleSkew));
  WriteLn;

  // cumulate

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

  for iscan := 1 to High(FInputScans) do
    for iangle := 0 to CCorrectAngleCount - 1 do
    begin
      ias := (iscan - 1) * CCorrectAngleCount + iangle;

      Write(FInputScans[iscan].ImageShortName);
      Write(', Angle:', (iangle / CCorrectAngleCount) * 360.0:9:3);
      Write(', RMSE:', rmses[ias]:12:6);
      if not IsNan(rmses[ias]) then
        Write(', Const:', FPerAngleSkew[ias, 0].ConstSkew:9:3, ', Mul:', FPerAngleSkew[ias, 0].MulSkew:12:8, ', Sqr:', FPerAngleSkew[ias, 0].SqrSkew:16:12);
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

  WriteLn('Mean RMSE:', Mean(validRmses):12:9, ', StdDev:', StdDev(validRmses):12:9, ', Worst RMSE:', MaxValue(validRmses):12:9);
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


unit scancorrelator;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, PNGComn, ZStream, MTProcs, TypInfo,
  utils, inputscan, powell, hackedwritepng;

type
  TCorrectCoords = record
    AngleIdx, ScanIdx, BaseScanIdx: Integer;
    PreparedData: TDoubleDynArray;
    SinCosLUT: TSinCosDynArray;
    WeightsLUT: TDoubleDynArray;
    ConstSkew, MulSkew: Double;
    Silent: Boolean;
  end;

  PCorrectCoords = ^TCorrectCoords;

  { TScanCorrelator }

  TScanCorrelator = class
  private
    FInputScans: TInputScanDynArray;
    FBrickwallLimitScans: Boolean;
    FCorrectAngles: Boolean;
    FRebuildBlended: Boolean;
    FOutputPNGFileName: String;
    FOutputDPI: Integer;
    FLock: TSpinlock;

    FPerAngleX: TDoubleDynArray2;

    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;

    FOutputWidth, FOutputHeight: Integer;
    FOutputImage: TWordDynArray;

    procedure CorrectAnglesFromCoords(const coords: TCorrectCoords; out startAngle, endAngle, angleInc: Double; out radiusCnt, angleCnt: Integer);

    function PrepareAnalyze: TDoubleDynArray;
    function PowellAnalyze(const arg: TVector; obj: Pointer): TScalar;
    procedure PrepareCorrect(var coords: TCorrectCoords);
    function PowellCorrect(const arg: TVector; obj: Pointer): TScalar;
    function PowellCorrectConst(const arg: TVector; obj: Pointer): TScalar;
    function PowellCorrectMul(const arg: TVector; obj: Pointer): TScalar;

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
    property BrickwallLimitScans: Boolean read FBrickwallLimitScans write FBrickwallLimitScans;
    property CorrectAngles: Boolean read FCorrectAngles write FCorrectAngles;
    property RebuildBlended: Boolean read FRebuildBlended write FRebuildBlended;

    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;

    property OutputDPI: Integer read FOutputDPI;
    property OutputWidth: Integer read FOutputWidth;
    property OutputHeight: Integer read FOutputHeight;

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

const
  CAnalyzeAreaBegin = C45RpmInnerSize;
  CAnalyzeAreaEnd = C45RpmLabelOuterSize;
  CAnalyzeAreaWidth = (CAnalyzeAreaEnd - CAnalyzeAreaBegin) * 0.5;

  CCorrectAngleCount = 36;
  CCorrectAreaBegin = C45RpmInnerSize;
  CCorrectAreaEnd = C45RpmFirstMusicGroove;
  CCorrectAreaWidth = (CCorrectAreaEnd - CCorrectAreaBegin) * 0.5;

constructor TScanCorrelator.Create(const AFileNames: TStrings; AOutputDPI: Integer);
var
  i: Integer;
begin
  FOutputDPI := AOutputDPI;
  SetLength(FInputScans, AFileNames.Count);

  for i := 0 to AFileNames.Count - 1 do
  begin
    FInputScans[i] := TInputScan.Create(AOutputDPI, True);
    FInputScans[i].ImageFileName := AFileNames[i];
  end;

  SpinLeave(@FLock);
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

    if SameText(ExtractFileExt(Scan.ImageFileName), '.tif') or
        SameText(ExtractFileExt(Scan.ImageFileName), '.tiff') then
      Scan.LoadTIFF
    else
      Scan.LoadPNG;

    if FBrickwallLimitScans then Scan.BrickwallLimit;
    Scan.FindTrack;

    WriteLn(Scan.ImageFileName);
  end;

var
  i: Integer;
begin
  WriteLn('LoadScans');

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
    QuickSort(FInputScans[0], 0, High(FInputScans), SizeOf(TInputScan), @CompareInputScansCenterQuality);
    Writeln('Best centering: ', FInputScans[0].ImageShortName);
  end;
end;

procedure TScanCorrelator.AngleInit;
const
  CAngleCount = 180;
  CAggregatedPixelsInches = 0.1;
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
          arr[Result] += Scan.GetPointD_Point(Scan.Image, py, px);

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

    FInputScans[AIndex].RelativeAngle := NormalizeAngle(bestAngle);
  end;

var
  pos: Integer;
begin
  WriteLn('AngleInit');

  if Length(FInputScans) <= 0 then
    Exit;

  SetLength(base, FInputScans[0].Width * CAngleCount);

  aggregatedPixelCount := Round(CAggregatedPixelsInches * FOutputDPI);
  rBeg := Round(CAnalyzeAreaBegin * 0.5 * FOutputDPI);
  rEnd := Round(CAnalyzeAreaEnd * 0.5 * FOutputDPI);

  pos := DoAngle(FInputScans[0], 0, base);

  SetLength(base, pos);

  ProcThreadPool.DoParallelLocalProc(@DoScan, 1, High(FInputScans));
end;

function TScanCorrelator.PrepareAnalyze: TDoubleDynArray;
var
  iRadius, pos, cnt: Integer;
  t, rBeg, px, py, cx, cy, r, ri, sn, cs: Double;
  sinCosLUT: TSinCosDynArray;
  scan: TInputScan;
begin
  cnt := Ceil(CAnalyzeAreaWidth * FOutputDPI * FPointsPerRevolution);
  SetLength(Result, cnt);

  scan := FInputScans[0];

  t   := scan.RelativeAngle;
  cx  := scan.Center.X;
  cy  := scan.Center.Y;

  BuildSinCosLUT(FPointsPerRevolution, sinCosLUT, t);

  pos := 0;
  rBeg := CAnalyzeAreaBegin * 0.5 * FOutputDPI;
  ri := 1.0 / FPointsPerRevolution;
  for iRadius := 0 to High(Result) do
  begin
    cs := sinCosLUT[pos].Cos;
    sn := sinCosLUT[pos].Sin;

    r := rBeg + iRadius * ri;

    px := cs * r + cx;
    py := sn * r + cy;

    if scan.InRangePointD(py, px) then
    begin
      Result[iRadius] := scan.GetPointD_Linear(scan.LeveledImage, py, px);
    end
    else
    begin
      Result[iRadius] := 1000.0;
    end;

    Inc(pos);

    if pos >= FPointsPerRevolution then
      pos := 0;
  end;
end;

function TScanCorrelator.PowellAnalyze(const arg: TVector; obj: Pointer): TScalar;
var
  coords: PCorrectCoords absolute obj;

  i, iRadius, pos, cnt: Integer;
  t, rBeg, px, py, cx, cy, r, ri, sn, cs: Double;
  sinCosLUT: TSinCosDynArray;
  scan: TInputScan;
begin
  Result := 0.0;

  cnt := Ceil(CAnalyzeAreaWidth * FOutputDPI * FPointsPerRevolution);

  scan := FInputScans[coords^.ScanIdx];

  t := arg[0];
  cx := arg[1];
  cy := arg[2];

  BuildSinCosLUT(FPointsPerRevolution, sinCosLUT, t);

  pos := 0;
  rBeg := CAnalyzeAreaBegin * 0.5 * FOutputDPI;
  ri := 1.0 / FPointsPerRevolution;
  for iRadius := 0 to cnt - 1 do
  begin
    cs := sinCosLUT[pos].Cos;
    sn := sinCosLUT[pos].Sin;

    r := rBeg + iRadius * ri;

    px := cs * r + cx;
    py := sn * r + cy;

    if scan.InRangePointD(py, px) then
    begin
      Result += Sqr(coords^.PreparedData[iRadius] - scan.GetPointD_Linear(scan.LeveledImage, py, px));
    end
    else
    begin
      Result += 1e6;
    end;

    Inc(pos);

    if pos >= FPointsPerRevolution then
      pos := 0;
  end;

  Result /= cnt;

  if not coords^.Silent then
  begin
    scan.Objective := Sqrt(Result);

    SpinEnter(@FLock);
    try
      Write('RMSEs: ');
      for i := 1 to High(FInputScans) do
        Write(FInputScans[i].Objective:12:9);
      Write(#13);
    finally
      SpinLeave(@FLock);
    end;
  end;
end;

procedure TScanCorrelator.Analyze;
const
  CEpsX = 1e-6;
  CScale = 1e-9;
var
  preparedData: TDoubleDynArray;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: TCorrectCoords;
    x: TVector;
    p: TPointD;
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
    PowellMinimize(@PowellAnalyze, x, CScale, CEpsX, 0.0, MaxInt, @coords);
    scan.RelativeAngle := x[0];
    p.X := x[1];
    p.Y := x[2];
    scan.Center := p;
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

  ProcThreadPool.DoParallelLocalProc(@DoEval, 1, High(FInputScans));

  WriteLn;
  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].ImageShortName, ', Angle: ', RadToDeg(FInputScans[i].RelativeAngle):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ' (after)');
end;

procedure TScanCorrelator.CorrectAnglesFromCoords(const coords: TCorrectCoords; out startAngle, endAngle,
  angleInc: Double; out radiusCnt, angleCnt: Integer);
var
  angle, angleExtents: Double;
begin
  angle := (coords.AngleIdx / CCorrectAngleCount) * 2.0 * Pi;
  angleExtents := 2.0 * Pi / CCorrectAngleCount;
  startAngle := angle - angleExtents;
  endAngle := angle + angleExtents;
  angleInc := FRadiansPerRevolutionPoint;

  radiusCnt := Ceil(CCorrectAreaWidth * FOutputDPI);
  angleCnt := Ceil((endAngle - startAngle + angleInc) / angleInc);
end;

procedure TScanCorrelator.PrepareCorrect(var coords: TCorrectCoords);
var
  iRadius, iAngle, iScan, cnt, radiusCnt, angleCnt, v, best: Integer;
  t, rBeg, r, sn, cs, px, py, cx, cy, startAngle, endAngle, angleInc: Double;
  scan: TInputScan;
begin
  CorrectAnglesFromCoords(coords, startAngle, endAngle, angleInc, radiusCnt, angleCnt);

  SetLength(coords.PreparedData, radiusCnt * angleCnt);

  // devise best scan

  best := MaxInt;
  coords.BaseScanIdx := -1;
  for iScan := 0 to coords.ScanIdx - 1 do
  begin
    scan := FInputScans[iScan];

    v := 0;
    for iAngle := round(RadToDeg(startAngle)) to round(RadToDeg(endAngle)) do
    begin
      t := NormalizeAngle(DegToRad(iAngle) + scan.RelativeAngle);

      v += Ord(InNormalizedAngle(t, scan.CropData.StartAngle, scan.CropData.EndAngle)) +
           Ord(InNormalizedAngle(t, scan.CropData.StartAngleMirror, scan.CropData.EndAngleMirror));
    end;

    if v <= best then
    begin
      best := v;
      coords.BaseScanIdx := iScan;
    end;
  end;

  //WriteLn(coords.ScanIdx:4, coords.AngleIdx:4, coords.BaseScanIdx:4, best:8);

  // build sin / cos lookup table

  scan := FInputScans[coords.BaseScanIdx];

  t  := scan.RelativeAngle;
  cx := scan.Center.X;
  cy := scan.Center.Y;

  BuildSinCosLUT(angleCnt, coords.SinCosLUT, startAngle + t, endAngle - startAngle + angleInc);

  // build weights lookup table

  SetLength(coords.WeightsLUT, angleCnt);
  for iAngle := 0 to angleCnt - 1 do
    coords.WeightsLUT[iAngle] := 2.0 - 4.0 * abs(iAngle / angleCnt - 0.5);

  // parse image arcs

  cnt := 0;
  rBeg := CCorrectAreaBegin * 0.5 * FOutputDPI;
  for iRadius := 0 to radiusCnt - 1 do
  begin
    r := rBeg + iRadius;

    for iAngle := 0 to angleCnt - 1 do
    begin
      cs := coords.SinCosLUT[iAngle].Cos;
      sn := coords.SinCosLUT[iAngle].Sin;

      px := cs * r + cx;
      py := sn * r + cy;

      if scan.InRangePointD(py, px) then
        coords.PreparedData[cnt] := scan.GetPointD_Linear(scan.LeveledImage, py, px)
      else
        coords.PreparedData[cnt] := 1000.0;

      Inc(cnt);
    end;
  end;

  Assert(cnt = radiusCnt * angleCnt);
end;

function TScanCorrelator.PowellCorrect(const arg: TVector; obj: Pointer): TScalar;
var
  coords: PCorrectCoords absolute obj;
  cnt, iRadius, iScan, iAngle, radiusCnt, angleCnt: Integer;
  t, r, rBeg, sn, cs, px, py, cx, cy, rsk, startAngle, endAngle, angleInc: Double;
  scan: TInputScan;
begin
  CorrectAnglesFromCoords(coords^, startAngle, endAngle, angleInc, radiusCnt, angleCnt);

  iScan := coords^.ScanIdx;
  scan := FInputScans[iScan];

  t  := scan.RelativeAngle;
  cx := scan.Center.X;
  cy := scan.Center.Y;

  // build sin / cos lookup table

  BuildSinCosLUT(angleCnt, coords^.sinCosLUT, startAngle + t, endAngle - startAngle + angleInc);

  // parse image arcs

  Result := 0;
  cnt := 0;
  rBeg := CCorrectAreaBegin * 0.5 * FOutputDPI;
  for iRadius := 0 to radiusCnt - 1 do
  begin
    r := rBeg + iRadius;

    rsk := r + r * arg[1] + arg[0];

    for iAngle := 0 to High(coords^.SinCosLUT) do
    begin
      cs := coords^.SinCosLUT[iAngle].Cos;
      sn := coords^.SinCosLUT[iAngle].Sin;

      px := cs * rsk + cx;
      py := sn * rsk + cy;

      if scan.InRangePointD(py, px) then
      begin
        Result += Sqr((coords^.PreparedData[cnt] - scan.GetPointD_Linear(scan.LeveledImage, py, px)) * coords^.WeightsLUT[iAngle]);
      end
      else
      begin
        Result += 1000.0;
      end;

      Inc(cnt);
    end;
  end;

  Assert(cnt = radiusCnt * angleCnt);

  Result /= cnt;
end;

function TScanCorrelator.PowellCorrectConst(const arg: TVector; obj: Pointer): TScalar;
var
  coords: PCorrectCoords absolute obj;
begin
  Result := PowellCorrect([arg[0], coords^.MulSkew], obj);
end;

function TScanCorrelator.PowellCorrectMul(const arg: TVector; obj: Pointer): TScalar;
var
  coords: PCorrectCoords absolute obj;
begin
 Result := PowellCorrect([coords^.ConstSkew, arg[0]], obj);
end;

procedure TScanCorrelator.Correct;
const
  CConstCorrectExtents = 0.02; // inches
  CConstCorrectHalfCount = 100;
  CMulCorrectExtents = 0.01;
  CMulCorrectHalfCount = 100;
var
  rmses: TDoubleDynArray;
  coordsArray: array of TCorrectCoords;
  doneCount: Integer;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: TCorrectCoords;
    iMul, iConst: Integer;
    skc, skm, best, f: Double;
  begin
    if not InRange(AIndex, 0, High(FPerAngleX)) then
      Exit;

    DivMod(AIndex, CCorrectAngleCount, coords.ScanIdx, coords.AngleIdx);
    Inc(coords.ScanIdx);

    PrepareCorrect(coords);

    coords.ConstSkew := 0.0;
    coords.MulSkew := 0.0;
    best := Infinity;

    for iMul := -CMulCorrectHalfCount to CMulCorrectHalfCount do
    begin
      skm := iMul * CMulCorrectExtents / CMulCorrectHalfCount;

      f := PowellCorrectMul([skm], @coords);

      if f < best then
      begin
        best := f;
        coords.MulSkew := skm;
      end;
    end;

    for iConst := -CConstCorrectHalfCount to CConstCorrectHalfCount do
    begin
      skc := iConst * CConstCorrectExtents / CConstCorrectHalfCount * FOutputDPI;

      f := PowellCorrectConst([skc], @coords);

      if f < best then
      begin
        best := f;
        coords.ConstSkew := skc;
      end;
    end;

    // free up memory
    SetLength(coords.PreparedData, 0);
    SetLength(coords.SinCosLUT, 0);
    SetLength(coords.WeightsLUT, 0);

    FPerAngleX[AIndex] := [coords.ConstSkew, coords.MulSkew];
    rmses[AIndex] := Sqrt(f);
    coordsArray[AIndex] := coords;

    Write(InterlockedIncrement(doneCount):4, ' / ', Length(FPerAngleX), #13);
  end;

var
  iangle, iscan, ias, iasbase, ix: Integer;
  pax: TDoubleDynArray2;
begin
  WriteLn('Correct');

  if Length(FInputScans) <= 1 then
    Exit;

  SetLength(FPerAngleX, CCorrectAngleCount * High(FInputScans));
  SetLength(rmses, Length(FPerAngleX));
  SetLength(coordsArray, Length(FPerAngleX));

  // compute

  doneCount := 0;
  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, High(FPerAngleX));

  // cumulate

  SetLength(pax, Length(FPerAngleX), Length(FPerAngleX[0]));

  for ias := 0 to High(pax) do
    for ix := 0 to High(FPerAngleX[ias]) do
      pax[ias, ix] := FPerAngleX[ias, ix];

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

        for ix := 0 to High(FPerAngleX[ias]) do
          FPerAngleX[ias, ix] += pax[iasbase, ix];
      end;
    end;

  // log

  WriteLn;

  for iscan := 1 to High(FInputScans) do
    for iangle := 0 to CCorrectAngleCount - 1 do
    begin
      ias := (iscan - 1) * CCorrectAngleCount + iangle;

      Write(FInputScans[iscan].ImageShortName);
      Write(', Angle:', (iangle / CCorrectAngleCount) * 360.0:9:3);
      Write(', RMSE:', rmses[ias]:12:6);
      WriteLn(', ', FPerAngleX[ias, 0]:12:6, ', ', FPerAngleX[ias, 1]:12:6);
    end;

  WriteLn('Worst RMSE: ', MaxValue(rmses):12:9);
end;

procedure TScanCorrelator.Crop;

  procedure DoCrop(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, High(FInputScans)) then
      Exit;

    FInputScans[AIndex].Crop;
  end;

var
  i: Integer;
begin
  WriteLn('Crop');

  ProcThreadPool.DoParallelLocalProc(@DoCrop, 0, High(FInputScans));

  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].ImageShortName, ', begin:', RadToDeg(FInputScans[i].CropData.StartAngle):9:3, ', end:', RadToDeg(FInputScans[i].CropData.EndAngle):9:3);
end;

procedure TScanCorrelator.Rebuild;
const
  CLabelDepthBits = 4;
  CLabelDepthMaxValue = (1 shl CLabelDepthBits) - 1;
  CTauToAngleIdx = CCorrectAngleCount / (2.0 * Pi);
var
  center, rBeg, rEnd, rLbl: Double;

  procedure InterpolateX(tau: Double; scanIdx: Integer; var x: TVector);
  var
    ci, iX, iSerp, so: Integer;
    c, alpha: Double;
    serpData: TSerpCoeffs9;
    coeffs: PSingle;
  begin
    if (Length(FPerAngleX) = 0) or (scanIdx <= 0) then
    begin
      FillQWord(x[0], Length(x), 0);
      Exit;
    end;

    if tau < 0 then
      tau += 2.0 * Pi;

    c := tau * CTauToAngleIdx;
    ci := Trunc(c);
    alpha := c - ci;
    so := (scanIdx - 1) * CCorrectAngleCount;

    coeffs := serpCoeffs(alpha);
    for iX := 0 to High(x) do
    begin
      for iSerp := -4 to 4 do
        serpData[iSerp] := FPerAngleX[(ci + iSerp + CCorrectAngleCount) mod CCorrectAngleCount + so, iX];
      x[iX] := serpFromCoeffs(coeffs, @serpData[0]);
    end;
  end;

  procedure DoY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    x: TVector;
    i, ox, cnt, yx: Integer;
    r, sn, cs, px, py, t, cx, cy, acc, bt, ct, rsk: Double;
    scan: TInputScan;
  begin
    SetLength(x, 2);

    yx := AIndex * FOutputWidth;

    for ox := 0 to FOutputWidth - 1 do
    begin
      r := Sqrt(Sqr(AIndex - center) + Sqr(ox - center));

      if InRange(r, rBeg, rEnd) then
      begin
        bt := ArcTan2(AIndex - center, ox - center);

        cnt := 0;
        acc := 0;
        for i := 0 to High(FInputScans) do
        begin
          scan := FInputScans[i];

          t  := scan.RelativeAngle;
          cx := scan.Center.X;
          cy := scan.Center.Y;

          InterpolateX(bt, i, x);

          rsk := r + r * x[1] + x[0];

          ct := NormalizeAngle(bt + t);

          SinCos(ct, sn, cs);
          px := cs * rsk + cx;
          py := sn * rsk + cy;

          if scan.InRangePointD(py, px) and
              (not InNormalizedAngle(ct, scan.CropData.StartAngle, scan.CropData.EndAngle) and
               not InNormalizedAngle(ct, scan.CropData.StartAngleMirror, scan.CropData.EndAngleMirror) or
               (r < rLbl)) then
          begin
            acc += scan.GetPointD_Sinc(scan.Image, py, px);
            Inc(cnt);
            if not FRebuildBlended then
              Break;
          end;
        end;

        acc := DivDef(acc, cnt, 1.0);

        if r >= rLbl then
          FOutputImage[yx + ox] := EnsureRange(Round(acc * High(Word)), 0, High(Word))
        else
          FOutputImage[yx + ox] := EnsureRange(Round(acc * CLabelDepthMaxValue), 0, CLabelDepthMaxValue) * High(Word) div CLabelDepthMaxValue; // lower bit depth for label
      end
      else
      begin
        // dark outside the disc, inner inside
        FOutputImage[yx + ox] := IfThen(r >= rLbl, Round(0.25 * High(Word)), Round(1.0 * High(Word)));
      end;
    end;
  end;

begin
  WriteLn('Rebuild');

  FOutputWidth := Ceil(C45RpmOuterSize * FOutputDPI);
  FOutputHeight := FOutputWidth;
  SetLength(FOutputImage, sqr(FOutputWidth));

  center := FOutputWidth / 2.0;
  rBeg := C45RpmAdapterSize * 0.5 * FOutputDPI;
  rEnd := C45RpmOuterSize * 0.5 * FOutputDPI;
  rLbl := C45RpmLabelOuterSize * 0.5 * FOutputDPI;

  ProcThreadPool.DoParallelLocalProc(@DoY, 0, FOutputHeight - 1);
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
  img := TScanImage.Create(FOutputWidth, FOutputHeight);
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


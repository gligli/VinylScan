unit scancorrelator;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, PNGComn, ZStream, MTProcs, TypInfo,
  utils, inputscan, powell, hackedwritepng;

type
  TAngleScanCoords = record
    AngleIdx, ScanIdx, BaseScanIdx: Integer;
    PreparedData: TDoubleDynArray;
    RadiusAngleLUT: array of TRadiusAngle;
    SinCosLUT: TSinCosDynArray;
    WeightsLUT: TByteDynArray;
  end;

  PAngleScanCoords = ^TAngleScanCoords;

  { TScanCorrelator }

  TScanCorrelator = class
  private
    FInputScans: TInputScanDynArray;
    FBrickwallLimitScans: Boolean;
    FAnalyzeMinimize: Boolean;
    FCorrectAngles: Boolean;
    FRebuildBlended: Boolean;
    FOutputPNGFileName: String;
    FOutputDPI: Integer;
    FLock: TSpinlock;

    FPerAngleX: TDoubleDynArray2;

    FOutputWidth, FOutputHeight: Integer;
    FOutputImage: TWordDynArray;

    procedure CorrectAnglesFromCoords(const coords: TAngleScanCoords; out startAngle, endAngle: Double);

    procedure PrepareAnalyze(var Coords: TAngleScanCoords);
    function GridSearchAnalyze(Angle, CenterX, CenterY, SkewY: Double; const Coords: TAngleScanCoords): Double;
    function PowellAnalyze(const x: TVector; obj: Pointer): TScalar;
    procedure PrepareCorrect(var Coords: TAngleScanCoords);
    function GridSearchCorrect(ConstSkew, MulSkew: Double; const Coords: TAngleScanCoords): Double;

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
    property AnalyzeMinimize: Boolean read FAnalyzeMinimize write FAnalyzeMinimize;
    property CorrectAngles: Boolean read FCorrectAngles write FCorrectAngles;
    property RebuildBlended: Boolean read FRebuildBlended write FRebuildBlended;

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

  CCorrectAngleCount = 36;
  CCorrectAreaBegin = C45RpmInnerSize;
  CCorrectAreaEnd = C45RpmFirstMusicGroove;

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

  FBrickwallLimitScans := False;
  FAnalyzeMinimize := True;
  FCorrectAngles := True;
  FRebuildBlended := True;
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
    sn, cs, cy, cx, px, py, sky: Double;
  begin
    Result := 0;

    FillQWord(arr[0], Length(arr), 0);

    cx := Scan.Center.X;
    cy := Scan.Center.Y;
    sky := Scan.SkewY;
    for iAngle := 0 to CAngleCount - 1 do
    begin
      SinCos(a + DegToRad(iAngle * (360 / CAngleCount)), sn, cs);

      pxAggr := 0;
      for iRadius:= rBeg to rEnd do
      begin
        px := cx + cs * iRadius;
        py := cy + sn * iRadius * sky;
        if Scan.InRangePointD(py, px) then
          arr[Result] += Scan.Image[Round(py) * Scan.Width + Round(px)];

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

procedure TScanCorrelator.PrepareAnalyze(var Coords: TAngleScanCoords);
var
  ilut, rawTInt: Integer;
  cx, cy, r, sky, ox, oy: Double;
  sinCosLUT: TSinCosDynArray;
  scan: TInputScan;
begin
  scan := FInputScans[0];
  cx  := scan.Center.X;
  cy  := scan.Center.Y;
  sky := scan.SkewY;

  // build radius / angle lookup table

  Coords.RadiusAngleLUT := BuildRadiusAngleLUT(CAnalyzeAreaBegin * 0.5 * FOutputDPI, CAnalyzeAreaEnd * 0.5 * FOutputDPI, 0.0, 2.0 * Pi);

  // build sin / cos lookup table

  BuildSinCosLUT(High(Word) + 1, sinCosLUT, scan.RelativeAngle);

  // parse image using LUTs

  SetLength(Coords.PreparedData, Length(Coords.RadiusAngleLUT));

  for iLut := 0 to High(Coords.RadiusAngleLUT) do
  begin
    rawTInt := Coords.RadiusAngleLUT[iLut].Angle;
    r := Coords.RadiusAngleLUT[iLut].Radius;

    ox := sinCosLUT[rawTInt].Cos * r + cx;
    oy := sinCosLUT[rawTInt].Sin * r * sky + cy;

    Coords.PreparedData[iLut] := scan.GetPointD_Linear(scan.LeveledImage, oy, ox);
  end;
end;

function TScanCorrelator.GridSearchAnalyze(Angle, CenterX, CenterY, SkewY: Double; const Coords: TAngleScanCoords
  ): Double;
var
  ilut, rawTInt: Integer;
  r, ox, oy: Double;
  sinCosLUT: TSinCosDynArray;
  scan: TInputScan;
begin
  scan := FInputScans[Coords.ScanIdx];

  BuildSinCosLUT(High(Word) + 1, sinCosLUT, Angle);

  Result := 0.0;
  for iLut := 0 to High(Coords.RadiusAngleLUT) do
  begin
    rawTInt := Coords.RadiusAngleLUT[iLut].Angle;
    r := Coords.RadiusAngleLUT[iLut].Radius;

    ox := sinCosLUT[rawTInt].Cos * r + CenterX;
    oy := sinCosLUT[rawTInt].Sin * r * SkewY + CenterY;

    Result += Sqr(Coords.PreparedData[iLut] - scan.GetPointD_Linear(scan.LeveledImage, oy, ox));
  end;

  Result /= Length(Coords.RadiusAngleLUT);
end;

function TScanCorrelator.PowellAnalyze(const x: TVector; obj: Pointer): TScalar;
var
  i: Integer;
begin
  Result := GridSearchAnalyze(x[0], x[1], x[2], x[3], PAngleScanCoords(obj)^);

  FInputScans[PAngleScanCoords(obj)^.ScanIdx].Objective := Sqrt(Result) / High(Word);

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

function CompareInputScansObjective(Item1, Item2, UserParameter: Pointer): Integer;
var
  s1: ^TInputScan absolute Item1;
  s2: ^TInputScan absolute Item2;
begin
  Result := CompareValue(s1^.Objective, s2^.Objective);
end;

procedure TScanCorrelator.Analyze;
const
  CEpsX = 1e-5;
  CScale = 1e-9;
var
  baseCoords: TAngleScanCoords;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: TAngleScanCoords;
    f: Double;
    p: TPointD;
    x: TVector;
    scan: TInputScan;
  begin
    if not InRange(AIndex, 1, High(FInputScans)) then
      Exit;

    scan := FInputScans[AIndex];

    coords := baseCoords;
    coords.AngleIdx := -1;
    coords.ScanIdx := AIndex;

    x := [scan.RelativeAngle, scan.Center.X, scan.Center.Y, scan.SkewY];

    f := PowellMinimize(@PowellAnalyze, x, CScale, CEpsX, 0.0, MaxInt, @coords)[0];

    scan.Objective := Sqrt(f) / High(Word);
    scan.RelativeAngle := x[0];
    p.X := x[1];
    p.Y := x[2];
    scan.Center := p;
    scan.SkewY := x[3];
  end;

var
  i: Integer;
begin
  WriteLn('Analyze');

  if Length(FInputScans) <= 1 then
    Exit;

  FillChar(baseCoords, SizeOf(baseCoords), 0);
  PrepareAnalyze(baseCoords);

  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].ImageShortName, ', Angle: ', RadToDeg(FInputScans[i].RelativeAngle):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ', SkewY: ', FInputScans[i].SkewY:9:6, ' (before)');

  if FAnalyzeMinimize then
  begin
    ProcThreadPool.DoParallelLocalProc(@DoEval, 1, High(FInputScans));
    WriteLn;

    for i := 0 to High(FInputScans) do
      WriteLn(FInputScans[i].ImageShortName, ', Angle: ', RadToDeg(FInputScans[i].RelativeAngle):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ', SkewY: ', FInputScans[i].SkewY:9:6, ', RMSE: ', FInputScans[i].Objective:12:9, ' (after)');

    QuickSort(FInputScans[0], 1, High(FInputScans), SizeOf(TInputScan), @CompareInputScansObjective);
  end;
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

procedure TScanCorrelator.CorrectAnglesFromCoords(const coords: TAngleScanCoords; out startAngle, endAngle: Double);
var
  angle, angleExtents: Double;
begin
  angle := (coords.AngleIdx / CCorrectAngleCount) * 2.0 * Pi;
  angleExtents := 2.0 * Pi / CCorrectAngleCount;
  startAngle := angle - angleExtents;
  endAngle := angle + angleExtents;
end;

procedure TScanCorrelator.PrepareCorrect(var Coords: TAngleScanCoords);
var
  iAngle, iBaseScan, iLut, v, best, rawTInt: Integer;
  cx, cy, ox, oy, r, sky, startAngle, endAngle, nsa, nea, bt, t, alpha: Double;
  baseScan: TInputScan;
  sinCosLUT: TSinCosDynArray;
begin
  CorrectAnglesFromCoords(Coords, startAngle, endAngle);

  // devise best baseScan

  best := MaxInt;
  Coords.BaseScanIdx := -1;
  for iBaseScan := 0 to Coords.ScanIdx - 1 do
  begin
    baseScan := FInputScans[iBaseScan];

    v := 0;
    for iAngle := round(RadToDeg(startAngle)) to round(RadToDeg(endAngle)) do
    begin
      t := NormalizeAngle(DegToRad(iAngle) + baseScan.RelativeAngle);

      v += Ord(InNormalizedAngle(t, baseScan.CropData.StartAngle, baseScan.CropData.EndAngle)) +
           Ord(InNormalizedAngle(t, baseScan.CropData.StartAngleMirror, baseScan.CropData.EndAngleMirror));
    end;

    if v <= best then
    begin
      best := v;
      Coords.BaseScanIdx := iBaseScan;
    end;
  end;

  //WriteLn(Coords.ScanIdx:4, Coords.AngleIdx:4, Coords.BaseScanIdx:4, best:8);

  baseScan := FInputScans[Coords.BaseScanIdx];

  // build radius / angle lookup table

  Coords.RadiusAngleLUT := BuildRadiusAngleLUT(CCorrectAreaBegin * 0.5 * FOutputDPI, CCorrectAreaEnd * 0.5 * FOutputDPI, startAngle, endAngle);

  // build sin / cos lookup tables

  BuildSinCosLUT(High(Word) + 1, sinCosLUT, baseScan.RelativeAngle);
  BuildSinCosLUT(High(Word) + 1, Coords.SinCosLUT, FInputScans[Coords.ScanIdx].RelativeAngle);

  // build weights lookup table

  nsa := NormalizeAngle(startAngle);
  nea := NormalizeAngle(endAngle);
  SetLength(Coords.WeightsLUT, Length(sinCosLUT));
  for iAngle := 0 to High(sinCosLUT) do
  begin
    bt := sinCosLUT[iAngle].Angle - baseScan.RelativeAngle;
    if InNormalizedAngle(NormalizeAngle(bt), nsa, nea) then
    begin
      alpha := 1.0 - 2.0 * abs((bt - startAngle) / (endAngle - startAngle) - 0.5);
      Coords.WeightsLUT[iAngle] := EnsureRange(round(alpha * High(Byte)), 0, High(Byte));
    end;
  end;

  // parse image using LUTs

  SetLength(Coords.PreparedData, Length(Coords.RadiusAngleLUT));

  cx := baseScan.Center.X;
  cy := baseScan.Center.Y;
  sky := baseScan.SkewY;

  for iLut := 0 to High(Coords.RadiusAngleLUT) do
  begin
    rawTInt := Coords.RadiusAngleLUT[iLut].Angle;
    r := Coords.RadiusAngleLUT[iLut].Radius;

    ox := sinCosLUT[rawTInt].Cos * r + cx;
    oy := sinCosLUT[rawTInt].Sin * r * sky + cy;

    Coords.PreparedData[iLut] := baseScan.GetPointD_Linear(baseScan.LeveledImage, oy, ox);
  end;
end;

function TScanCorrelator.GridSearchCorrect(ConstSkew, MulSkew: Double; const Coords: TAngleScanCoords): Double;
var
  iScan, iLut, rawTInt: Integer;
  cx, cy, r, sky, ox, oy: Double;
  scan: TInputScan;
begin
  iScan := Coords.ScanIdx;
  scan := FInputScans[iScan];

  cx  := scan.Center.X;
  cy  := scan.Center.Y;
  sky := scan.SkewY;

  Result := 0;
  for iLut := 0 to High(Coords.RadiusAngleLUT) do
  begin
    rawTInt := Coords.RadiusAngleLUT[iLut].Angle;
    r := Coords.RadiusAngleLUT[iLut].Radius;

    r := r + r * MulSkew + ConstSkew;

    ox := Coords.SinCosLUT[rawTInt].Cos * r + cx;
    oy := Coords.SinCosLUT[rawTInt].Sin * r * sky + cy;

    Result += Sqr((Coords.PreparedData[iLut] - scan.GetPointD_Linear(scan.LeveledImage, oy, ox)) * Coords.WeightsLUT[rawTInt]);
  end;

  Result := Result / Length(Coords.RadiusAngleLUT);
end;

procedure TScanCorrelator.Correct;
const
  CConstCorrectExtents = 0.02; // inches
  CConstCorrectHalfCount = 100;
  CMulCorrectExtents = 0.01;
  CMulCorrectHalfCount = 100;
var
  rmses: TDoubleDynArray;
  coordsArray: array of TAngleScanCoords;
  doneCount: Integer;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: TAngleScanCoords;
    c, iter: Integer;
    ConstSkew, MulSkew, bestConstSkew, bestMulSkew, prevf, bestf, f: Double;
  begin
    if not InRange(AIndex, 0, High(FPerAngleX)) then
      Exit;

    DivMod(AIndex, CCorrectAngleCount, coords.ScanIdx, coords.AngleIdx);
    Inc(coords.ScanIdx);

    PrepareCorrect(coords);

    bestConstSkew := 0.0;
    bestMulSkew := 0.0;
    bestf := Infinity;

    bestf := Infinity;
    iter := 0;
    repeat
      prevf := bestf;
      bestf := Infinity;

      if Odd(iter) then
      begin
        for c := -CConstCorrectHalfCount to CConstCorrectHalfCount do
        begin
          ConstSkew := c * CConstCorrectExtents / CConstCorrectHalfCount * FOutputDPI;

          f := GridSearchCorrect(ConstSkew, bestMulSkew, coords);

          if f < bestf then
          begin
            bestf := f;
            bestConstSkew := ConstSkew;
          end;
        end;
      end
      else
      begin
        for c := -CMulCorrectHalfCount to CMulCorrectHalfCount do
        begin
          MulSkew := c * CMulCorrectExtents / CMulCorrectHalfCount;

          f := GridSearchCorrect(bestConstSkew, MulSkew, coords);

          if f < bestf then
          begin
            bestf := f;
            bestMulSkew := MulSkew;
          end;
        end;
      end;

      Inc(iter);
    until SameValue(prevf, bestf);

    // free up memory
    SetLength(coords.PreparedData, 0);
    SetLength(coords.RadiusAngleLUT, 0);
    SetLength(coords.SinCosLUT, 0);
    SetLength(coords.WeightsLUT, 0);

    FPerAngleX[AIndex] := [bestConstSkew, bestMulSkew];
    rmses[AIndex] := Sqrt(f) / (High(Word) * High(Byte));
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
  WriteLn;

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
    r, sn, cs, px, py, t, cx, cy, sky, acc, bt, ct, rsk: Double;
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

          t   := scan.RelativeAngle;
          cx  := scan.Center.X;
          cy  := scan.Center.Y;
          sky := scan.SkewY;

          InterpolateX(bt, i, x);

          rsk := r + r * x[1] + x[0];

          ct := NormalizeAngle(bt + t);

          SinCos(ct, sn, cs);
          px := cs * rsk + cx;
          py := sn * rsk * sky + cy;

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

        acc := DivDef(acc, cnt, High(Word));

        if r >= rLbl then
          FOutputImage[yx + ox] := EnsureRange(Round(acc), 0, High(Word))
        else
          FOutputImage[yx + ox] := EnsureRange(Round(acc * CLabelDepthMaxValue / High(Word)), 0, CLabelDepthMaxValue) * High(Word) div CLabelDepthMaxValue; // lower bit depth for label
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


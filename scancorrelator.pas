unit scancorrelator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, PNGComn, ZStream, MTProcs, TypInfo,
  utils, inputscan, powell, hackedwritepng;

type
  TCorrectCoords = record
    AngleIdx, ScanIdx, BaseScanIdx: Integer;
    GroovesPerInch: Double;
    PreparedData: TDoubleDynArray;
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

    FPerAngleX: TDoubleDynArray2;

    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;

    FOutputImage: TWordDynArray2;

    procedure CorrectAnglesFromCoords(const coords: TCorrectCoords; out startAngle, endAngle, angleInc: Double);

    function PrepareAnalyze(coords: TCorrectCoords): TDoubleDynArray;
    procedure GradientAnalyze(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
    function PowellAnalyze(const arg: TVector; obj: Pointer): TScalar;
    procedure PrepareCorrect(var coords: TCorrectCoords);
    procedure GradientCorrect(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
    function PowellCorrect(const arg: TVector; obj: Pointer): TScalar;

    procedure AngleInit;
    procedure Analyze;
    procedure Crop;
    procedure Correct;
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
  CAnalyzeAreaBegin = C45RpmInnerSize + 0.1;
  CAnalyzeAreaEnd = C45RpmLabelOuterSize;
  CAnalyzeAreaWidth = (CAnalyzeAreaEnd - CAnalyzeAreaBegin) * 0.5;
  CAnalyzeAreaGroovesPerInch = 300;

  CCorrectAngleCount = 8;
  CCorrectAreaBegin = C45RpmLabelOuterSize - 0.1;
  CCorrectAreaEnd = C45RpmOuterSize;
  CCorrectAreaWidth = (CCorrectAreaEnd - CCorrectAreaBegin) * 0.5;
  CCorrectAreaGroovesPerInch = 300;

constructor TScanCorrelator.Create(const AFileNames: TStrings; AOutputDPI: Integer);
var
  i: Integer;
begin
  FOutputDPI := AOutputDPI;
  FObjective := NaN;
  FMethod := mmBFGS;
  SetLength(FInputScans, AFileNames.Count);

  for i := 0 to AFileNames.Count - 1 do
  begin
    FInputScans[i] := TInputScan.Create(AOutputDPI, True);
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

function CompareInputScansCenterQuality(Item1, Item2, UserParameter: Pointer): Integer;
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
    QuickSort(FInputScans[0], 0, High(FInputScans), SizeOf(TInputScan), @CompareInputScansCenterQuality);
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

    FInputScans[AIndex].RelativeAngle := NormalizeAngle(bestAngle);
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

function TScanCorrelator.PrepareAnalyze(coords: TCorrectCoords): TDoubleDynArray;
var
  i, pos, cnt: Integer;
  ri, t, r, px, py, cx, cy, rri, sn, cs: Double;
  sinCosLUT: TPointDDynArray;
begin
  cnt := Ceil(CAnalyzeAreaWidth * coords.GroovesPerInch * FPointsPerRevolution);
  SetLength(Result, cnt);

  t   := FInputScans[0].RelativeAngle;
  cx  := FInputScans[0].Center.X;
  cy  := FInputScans[0].Center.Y;

  BuildSinCosLUT(FPointsPerRevolution, sinCosLUT, t);

  ri := FOutputDPI / (coords.GroovesPerInch * FPointsPerRevolution);

  r := CAnalyzeAreaBegin * 0.5 * FOutputDPI;
  pos := 0;
  for i := 0 to High(Result) do
  begin
    cs := sinCosLUT[pos].X;
    sn := sinCosLUT[pos].Y;

    rri := r + ri * i;

    px := cs * rri + cx;
    py := sn * rri + cy;

    if FInputScans[0].InRangePointD(py, px) then
    begin
      Result[i] := FInputScans[0].GetPointD(py, px, isImage);
    end
    else
    begin
      Result[i] := 1000.0;
    end;

    Inc(pos);

    if pos >= FPointsPerRevolution then
      pos := 0;
  end;
end;

procedure TScanCorrelator.GradientAnalyze(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  coords: PCorrectCoords absolute obj;

  iRadius, iArg, pos, cnt, scanIdx: Integer;
  ri, t, r, px, py, cx, cy, rri, sn, cs, mseInt, gInt, gimgx, gimgy, gr, gt, gcx, gcy: Double;
  sinCosLUT: TPointDDynArray;
begin
  func := 0.0;
  if Assigned(grad) then
    FillQWord(grad[0], Length(grad), 0);

  scanIdx := coords^.ScanIdx;
  cnt := Ceil(CAnalyzeAreaWidth * coords^.GroovesPerInch * FPointsPerRevolution);

  t   := arg[0];
  cx  := arg[1];
  cy  := arg[2];

  BuildSinCosLUT(FPointsPerRevolution, sinCosLUT, t);

  ri := FOutputDPI / (coords^.GroovesPerInch * FPointsPerRevolution);

  r := CAnalyzeAreaBegin * 0.5 * FOutputDPI;
  pos := 0;
  for iRadius := 0 to cnt - 1 do
  begin
    cs := sinCosLUT[pos].X;
    sn := sinCosLUT[pos].Y;

    rri := r + ri * iRadius;

    px := cs * rri + cx;
    py := sn * rri + cy;

    if FInputScans[scanIdx].InRangePointD(py, px) then
    begin
      mseInt := coords^.PreparedData[iRadius] - FInputScans[scanIdx].GetPointD(py, px, isImage);

      func += Sqr(mseInt);

      if Assigned(grad) then
      begin
        gimgx := FInputScans[scanIdx].GetPointD(py, px, isXGradient);
        gimgy := FInputScans[scanIdx].GetPointD(py, px, isYGradient);

        gInt := -2.0 * mseInt;
        gr := rri;

        gt := (gimgx * -sn + gimgy * cs) * gr;
        gcx := gimgx;
        gcy := gimgy;

        grad[0] += gt * gInt;
        grad[1] += gcx * gInt;
        grad[2] += gcy * gInt;
      end;
    end
    else
    begin
      func += 1000.0;
    end;

    Inc(pos);

    if pos >= FPointsPerRevolution then
      pos := 0;
  end;

  func /= cnt;
  if Assigned(grad) then
    for iArg := 0 to High(grad) do
      grad[iArg] /= cnt;

  Write('RMSE: ', Sqrt(func):12:9,#13);
end;

procedure TScanCorrelator.Analyze;
const
  CEpsX = 1e-3;
  CScale = 1e-3;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: TCorrectCoords;
    x: TVector;
    iter: Integer;
    f, prevF: Double;
    p: TPointD;
  begin
    if not InRange(AIndex, 1, High(FInputScans)) then
      Exit;

    SetLength(x, 3);
    x[0] := FInputScans[AIndex].RelativeAngle;
    x[1] := FInputScans[AIndex].Center.X;
    x[2] := FInputScans[AIndex].Center.Y;

    coords.AngleIdx := -1;
    coords.ScanIdx := AIndex;
    coords.GroovesPerInch := CAnalyzeAreaGroovesPerInch;

    f := 1000.0;
    iter := 0;
    //repeat
      prevF := f;

      coords.PreparedData := PrepareAnalyze(coords);

      case Method of
        mmNone:
        begin
          f := PowellAnalyze(x, @coords);
        end;
        mmBFGS:
        begin
          f := NonSmoothMinimize(@GradientAnalyze, x, CEpsX, @coords);
        end;
        mmPowell:
        begin
          f := PowellMinimize(@PowellAnalyze, x, CScale, CEpsX, 0.0, MaxInt, @coords)[0];
        end;
        mmAll:
        begin
          NonSmoothMinimize(@GradientAnalyze, x, CEpsX, @coords);
          f := PowellMinimize(@PowellAnalyze, x, CScale, CEpsX, 0.0, MaxInt, @coords)[0];
        end;
      end;

      f := Sqrt(f);
      Inc(iter);

      coords.GroovesPerInch *= cPhi;

      WriteLn(FInputScans[AIndex].PNGShortName, ', Iteration: ', iter:3, ', RMSE: ', f:12:9, #13);

    //until SameValue(f, prevF, 1e-4) or (coords.GroovesPerInch > FOutputDPI);

    FInputScans[AIndex].RelativeAngle := x[0];
    p.X := x[1];
    p.Y := x[2];
    FInputScans[AIndex].Center := p;
  end;

var
  i: Integer;
begin
  WriteLn('Analyze');

  if Length(FInputScans) <= 1 then
    Exit;

  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].PNGShortName, ', Angle: ', RadToDeg(FInputScans[i].RelativeAngle):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ' (before)');

  ProcThreadPool.DoParallelLocalProc(@DoEval, 1, High(FInputScans));

  WriteLn;
  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].PNGShortName, ', Angle: ', RadToDeg(FInputScans[i].RelativeAngle):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ' (after)');
end;

procedure TScanCorrelator.CorrectAnglesFromCoords(const coords:TCorrectCoords;
 out startAngle,endAngle,angleInc:Double);
var
  angle, angleExtents: Double;
begin
  angle := (coords.AngleIdx / CCorrectAngleCount) * 2.0 * Pi;
  angleExtents := 2.0 * Pi / CCorrectAngleCount;
  startAngle := angle - angleExtents;
  endAngle := angle + angleExtents;
  angleInc := FRadiansPerRevolutionPoint;
end;

procedure TScanCorrelator.PrepareCorrect(var coords: TCorrectCoords);
var
  iRadius, iAngle, iScan, cnt, radiusCnt, angleCnt, v, best: Integer;
  t, rBeg, ri, rri, sn, cs, px, py, cx, cy, startAngle, endAngle, angleInc: Double;
  sinCosLUT: TPointDDynArray;
  scan: TInputScan;
begin
  CorrectAnglesFromCoords(coords, startAngle, endAngle, angleInc);

  radiusCnt := Ceil(CCorrectAreaWidth * coords.GroovesPerInch);
  angleCnt := Ceil((endAngle - startAngle + angleInc) / angleInc);

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

  WriteLn(coords.ScanIdx:4, coords.AngleIdx:4, coords.BaseScanIdx:4, best:8);

  // build sin / cos lookup table

  t  := FInputScans[coords.BaseScanIdx].RelativeAngle;
  cx := FInputScans[coords.BaseScanIdx].Center.X;
  cy := FInputScans[coords.BaseScanIdx].Center.Y;

  BuildSinCosLUT(angleCnt, sinCosLUT, startAngle + t, endAngle - startAngle + angleInc);

  // parse image arcs

  cnt := 0;
  rBeg := CCorrectAreaBegin * 0.5 * FOutputDPI;
  ri := FOutputDPI / coords.GroovesPerInch;
  for iRadius := 0 to radiusCnt - 1 do
  begin
    rri := rBeg + ri * iRadius;

    for iAngle := 0 to angleCnt - 1 do
    begin
      cs := sinCosLUT[iAngle].X;
      sn := sinCosLUT[iAngle].Y;

      px := cs * rri + cx;
      py := sn * rri + cy;

      if FInputScans[coords.BaseScanIdx].InRangePointD(py, px) then
        coords.PreparedData[cnt] := FInputScans[coords.BaseScanIdx].GetPointD(py, px, isImage)
      else
        coords.PreparedData[cnt] := 1000.0;

      Inc(cnt);
    end;
  end;

  Assert(cnt = radiusCnt * angleCnt);
end;

procedure TScanCorrelator.GradientCorrect(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  coords: PCorrectCoords absolute obj;
  preparedData: TDoubleDynArray;
  iArg, cnt, iRadius, iScan, iAngle, radiusCnt, angleCnt: Integer;
  mseInt, w, t, ri, rri, rBeg, sn, cs, px, py, cx, cy, skc, skm, rsk, gimgx, gimgy, gsk, startAngle, endAngle, angleInc: Double;
  sinCosLUT: TPointDDynArray;
begin
  func := 0.0;
  if Assigned(grad) then
    FillQWord(grad[0], Length(grad), 0);

  skc := arg[0];
  skm := arg[1];

  CorrectAnglesFromCoords(coords^, startAngle, endAngle, angleInc);

  radiusCnt := Ceil(CCorrectAreaWidth * coords^.GroovesPerInch);
  angleCnt := Ceil((endAngle - startAngle + angleInc) / angleInc);

  iScan := coords^.ScanIdx;
  preparedData := coords^.PreparedData;

  t  := FInputScans[iScan].RelativeAngle;
  cx := FInputScans[iScan].Center.X;
  cy := FInputScans[iScan].Center.Y;

  // build sin / cos lookup table

  BuildSinCosLUT(angleCnt, sinCosLUT, startAngle + t, endAngle - startAngle + angleInc);

  // parse image arcs

  cnt := 0;
  rBeg := CCorrectAreaBegin * 0.5 * FOutputDPI;
  ri := FOutputDPI / coords^.GroovesPerInch;
  for iRadius := 0 to radiusCnt - 1 do
  begin
    rri := rBeg + ri * iRadius;

    rsk := rri + (rri * skm + skc);

    for iAngle := 0 to High(sinCosLUT) do
    begin
      cs := sinCosLUT[iAngle].X;
      sn := sinCosLUT[iAngle].Y;

      px := cs * rsk + cx;
      py := sn * rsk + cy;

      if FInputScans[iScan].InRangePointD(py, px) then
      begin
        w := 2.0 - 4.0 * abs(iAngle / angleCnt - 0.5);

        mseInt := (preparedData[cnt] - FInputScans[iScan].GetPointD(py, px, isImage)) * w;

        func += Sqr(mseInt);

        if Assigned(grad) then
        begin
          gimgx := FInputScans[iScan].GetPointD(py, px, isXGradient);
          gimgy := FInputScans[iScan].GetPointD(py, px, isYGradient);

          gsk := gimgx * cs + gimgy * sn;

          gsk *= -2.0 * mseInt;

          grad[0] += gsk;
          grad[1] += gsk * rri;
        end;
      end
      else
      begin
        func += 1000.0;
      end;

      Inc(cnt);
    end;
  end;

  Assert(cnt = radiusCnt * angleCnt);

  func /= cnt;
  if Assigned(grad) then
    for iArg := 0 to High(grad) do
      grad[iArg] /= cnt;
end;

function TScanCorrelator.PowellCorrect(const arg: TVector; obj: Pointer): TScalar;
begin
  GradientCorrect(arg, Result, nil, obj);
end;

procedure TScanCorrelator.Correct;
const
  CEpsX = 1e-6;
var
  rmses: TDoubleDynArray;
  coordsArray: array of TCorrectCoords;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: TCorrectCoords;
    x: TVector;
    iter: Integer;
    f, prevF: Double;
  begin
    if not InRange(AIndex, 0, High(FPerAngleX)) then
      Exit;

    DivMod(AIndex, CCorrectAngleCount, coords.ScanIdx, coords.AngleIdx);
    Inc(coords.ScanIdx);
    coords.GroovesPerInch := CCorrectAreaGroovesPerInch;

    SetLength(x, 2);
    f := 1000.0;
    iter := 0;
    //repeat
      prevF := f;

      PrepareCorrect(coords);

      f := NonSmoothBoundedMinimize(@GradientCorrect, x, [-Infinity, -0.01], [Infinity, 0.01], CEpsX, @coords);

      f := Sqrt(f);
      Inc(iter);

      coords.GroovesPerInch *= cPhi;

      Write(FInputScans[coords.ScanIdx].PNGShortName);
      Write(', Angle:', (coords.AngleIdx / CCorrectAngleCount) * 360.0:9:3);
      Write(', Iteration:', iter:3);
      Write(', RMSE:', f:12:6);
      WriteLn(', ', x[0]:12:6, ', ', x[1]:12:6);

    //until SameValue(f, prevF, 1e-4) or (coords.GroovesPerInch > FOutputDPI);

    FPerAngleX[AIndex] := x;
    rmses[AIndex] := f;
    coordsArray[AIndex] := coords;
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

  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, High(FPerAngleX));

  // cumulate

  SetLength(pax, Length(FPerAngleX), Length(FPerAngleX[0]));

  for ias := 0 to High(pax) do
  begin
    for ix := 0 to High(FPerAngleX[ias]) do
      pax[ias, ix] := FPerAngleX[ias, ix];
    WriteLn(coordsArray[ias].ScanIdx:4, coordsArray[ias].AngleIdx:4, coordsArray[ias].BaseScanIdx:4);
  end;

  for iscan := 1 to High(FInputScans) do
    for iangle := 0 to CCorrectAngleCount - 1 do
    begin
      ias := (iscan - 1) * CCorrectAngleCount + iangle;

      WriteLn(ias:4);

      iasbase := ias;
      while True do
      begin
        iasbase := (coordsArray[iasbase].BaseScanIdx - 1) * CCorrectAngleCount + iangle;

        WriteLn(ias:4, iasbase:4);

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

      Write(FInputScans[iscan].PNGShortName);
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
    WriteLn(FInputScans[i].PNGShortName, ', begin:', RadToDeg(FInputScans[i].CropData.StartAngle):9:3, ', end:', RadToDeg(FInputScans[i].CropData.EndAngle):9:3);
end;

procedure TScanCorrelator.Rebuild;
const
  CTauToAngleIdx = CCorrectAngleCount / (2.0 * Pi);
var
  center, rBeg, rEnd, rLbl: Double;

  procedure InterpolateX(tau: Double; scanIdx: Integer; var x: TVector);
  var
    ci, i, x1, x2, so: Integer;
    c, alpha: Double;
    y1, y2: TVector;
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

    x1 := (ci + 0) mod CCorrectAngleCount;
    x2 := (ci + 1) mod CCorrectAngleCount;

    y1 := FPerAngleX[x1 + so];
    y2 := FPerAngleX[x2 + so];

    for i := 0 to High(x) do
      x[i] := lerp(y1[i], y2[i], alpha);
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
        bt := ArcTan2(AIndex - center, ox - center);

        cnt := 0;
        acc := 0;
        for i := 0 to High(FInputScans) do
        begin
          t  := FInputScans[i].RelativeAngle;
          cx := FInputScans[i].Center.X;
          cy := FInputScans[i].Center.Y;

          InterpolateX(bt, i, x);

          skc := x[0];
          skm := x[1];
          rsk := r + (r * skm + skc);

          ct := NormalizeAngle(bt + t);

          SinCos(ct, sn, cs);
          px := cs * rsk + cx;
          py := sn * rsk + cy;

          if FInputScans[i].InRangePointD(py, px) and
              (not InNormalizedAngle(ct, FInputScans[i].CropData.StartAngle, FInputScans[i].CropData.EndAngle) and
               not InNormalizedAngle(ct, FInputScans[i].CropData.StartAngleMirror, FInputScans[i].CropData.EndAngleMirror) or
               (r < rLbl)) then
          begin
            acc += FInputScans[i].GetPointD(py, px, isImage);
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
        FOutputImage[AIndex, ox] := IfThen(r >= rLbl, Round(0.25 * High(Word)), Round(1.0 * High(Word)));
      end;
    end;
  end;

begin
  WriteLn('Rebuild');

  center := Length(FOutputImage) / 2.0;
  rBeg := C45RpmAdapterSize * 0.5 * FOutputDPI;
  rEnd := C45RpmOuterSize * 0.5 * FOutputDPI;
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


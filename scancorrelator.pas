unit scancorrelator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, PNGComn, ZStream, MTProcs, TypInfo,
  utils, inputscan, powell, hackedwritepng;

type
  TCorrectCoords = record
    AngleIdx, ScanIdx: Integer;
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

    function PrepareAnalyze: TDoubleDynArray;
    procedure GradientAnalyze(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
    function PowellAnalyze(const arg: TVector; obj: Pointer): TScalar;
    function PrepareCorrect(coords: TCorrectCoords): TDoubleDynArray;
    procedure GradientCorrect(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
    function PowellCorrect(const arg: TVector; obj: Pointer): TScalar;

    procedure AngleInit;
    function Analyze(AMethod: TMinimizeMethod): Double;
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
  CAnalyzeAreaBegin = C45RpmInnerSize;
  CAnalyzeAreaEnd = C45RpmLabelOuterSize;
  CAnalyzeAreaWidth = (CAnalyzeAreaEnd - CAnalyzeAreaBegin) * 0.5;
  CAnalyzeAreaGroovesPerInch = 64;

  CCorrectAngleCount = 8;
  CCorrectArea1Begin = C45RpmInnerSize;
  CCorrectArea1End = C45RpmLastMusicGroove;
  CCorrectArea2Begin = C45RpmFirstMusicGroove;
  CCorrectArea2End = C45RpmOuterSize;
  CCorrectAreaWidth = (CCorrectArea1End - CCorrectArea1Begin) * 0.5 + (CCorrectArea2End - CCorrectArea2Begin) * 0.5;
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

    FInputScans[AIndex].RelativeAngle := bestAngle;
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

function TScanCorrelator.PrepareAnalyze: TDoubleDynArray;
var
  i, pos, cnt: Integer;
  ri, t, r, px, py, cx, cy, rri, sn, cs, gimgx, gimgy, gr, gt, gcx, gcy: Double;
  sinCosLUT: TPointDDynArray;
begin
  cnt := Ceil(CAnalyzeAreaWidth * CAnalyzeAreaGroovesPerInch * FPointsPerRevolution);
  SetLength(Result, cnt);

  t   := FInputScans[0].RelativeAngle;
  cx  := FInputScans[0].Center.X;
  cy  := FInputScans[0].Center.Y;

  BuildSinCosLUT(FPointsPerRevolution, sinCosLUT, t);

  ri := FOutputDPI / (CAnalyzeAreaGroovesPerInch * FPointsPerRevolution);

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
  preparedData: TDoubleDynArray absolute obj;
  imgResults: TDoubleDynArray;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    i, pos, cnt: Integer;
    ri, t, r, px, py, cx, cy, rri, sn, cs, mseInt, gInt, gimgx, gimgy, gr, gt, gcx, gcy: Double;
    sinCosLUT: TPointDDynArray;
  begin
    if not InRange(AIndex, 1, High(FInputScans)) then
      Exit;

    imgResults[AIndex - 1] := 0.0;
    if Assigned(grad) then
    begin
      grad[High(FInputScans) * 0 + AIndex - 1] := 0.0;
      grad[High(FInputScans) * 1 + AIndex - 1] := 0.0;
      grad[High(FInputScans) * 2 + AIndex - 1] := 0.0;
    end;

    cnt := Ceil(CAnalyzeAreaWidth * CAnalyzeAreaGroovesPerInch * FPointsPerRevolution);

    t   := arg[High(FInputScans) * 0 + AIndex - 1];
    cx  := arg[High(FInputScans) * 1 + AIndex - 1];
    cy  := arg[High(FInputScans) * 2 + AIndex - 1];

    BuildSinCosLUT(FPointsPerRevolution, sinCosLUT, t);

    ri := FOutputDPI / (CAnalyzeAreaGroovesPerInch * FPointsPerRevolution);

    r := CAnalyzeAreaBegin * 0.5 * FOutputDPI;
    pos := 0;
    for i := 0 to cnt - 1 do
    begin
      cs := sinCosLUT[pos].X;
      sn := sinCosLUT[pos].Y;

      rri := r + ri * i;

      px := cs * rri + cx;
      py := sn * rri + cy;

      if FInputScans[AIndex].InRangePointD(py, px) then
      begin
        mseInt := preparedData[i] - FInputScans[AIndex].GetPointD(py, px, isImage);

        imgResults[AIndex - 1] += Sqr(mseInt);

        if Assigned(grad) then
        begin
          gimgx := FInputScans[AIndex].GetPointD(py, px, isXGradient);
          gimgy := FInputScans[AIndex].GetPointD(py, px, isYGradient);

          gInt := -2.0 * mseInt;
          gr := rri;

          gt := (gimgx * -sn + gimgy * cs) * gr * gInt;
          gcx := gimgx * gInt;
          gcy := gimgy * gInt;

          grad[High(FInputScans) * 0 + AIndex - 1] += gt;
          grad[High(FInputScans) * 1 + AIndex - 1] += gcx;
          grad[High(FInputScans) * 2 + AIndex - 1] += gcy;
        end;
      end
      else
      begin
        imgResults[AIndex - 1] += 1000.0;
      end;

      Inc(pos);

      if pos >= FPointsPerRevolution then
        pos := 0;
    end;

    imgResults[AIndex - 1] /= cnt;
    if Assigned(grad) then
    begin
      grad[High(FInputScans) * 0 + AIndex - 1] /= cnt;
      grad[High(FInputScans) * 1 + AIndex - 1] /= cnt;
      grad[High(FInputScans) * 2 + AIndex - 1] /= cnt;
    end;
  end;

begin
  SetLength(imgResults, High(FInputScans));

  ProcThreadPool.DoParallelLocalProc(@DoEval, 1, High(FInputScans));

  func := Mean(imgResults);

  Write('RMSE: ', Sqrt(func):12:9,#13);
end;

function TScanCorrelator.Analyze(AMethod: TMinimizeMethod): Double;
var
  x: TVector;
  i: Integer;
  p: TPointD;
  preparedData: TDoubleDynArray;
begin
  Result := NaN;

  WriteLn('Analyze ', GetEnumName(TypeInfo(TMinimizeMethod), Ord(AMethod)));

  if Length(FInputScans) <= 0 then
    Exit;

  if Length(FInputScans) <= 1 then
    AMethod := mmNone;

  preparedData := PrepareAnalyze;

  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].PNGShortName, ', Angle: ', RadToDeg(FInputScans[i].RelativeAngle):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ' (before)');

  SetLength(x, High(FInputScans) * 3);
  for i := 1 to High(FInputScans) do
  begin
    x[High(FInputScans) * 0 + i - 1] := FInputScans[i].RelativeAngle;
    x[High(FInputScans) * 1 + i - 1] := FInputScans[i].Center.X;
    x[High(FInputScans) * 2 + i - 1] := FInputScans[i].Center.Y;
  end;

  case AMethod of
    mmNone:
    begin
      Result := PowellAnalyze(x, Pointer(preparedData));
    end;
    mmBFGS:
    begin
      Result := BFGSMinimize(@GradientAnalyze, x, 1e-12, Pointer(preparedData));
    end;
    mmPowell:
    begin
      Result := PowellMinimize(@PowellAnalyze, x, 1e-8, 1e-9, 1e-12, MaxInt, Pointer(preparedData))[0];
    end;
  end;

  Assert(not IsNan(Result));

  FInputScans[0].RelativeAngle := 0.0;

  for i := 1 to High(FInputScans) do
  begin
    FInputScans[i].RelativeAngle := x[High(FInputScans) * 0 + i - 1];
    p.X := x[High(FInputScans) * 1 + i - 1];
    p.Y := x[High(FInputScans) * 2 + i - 1];
    FInputScans[i].Center := p;
  end;

  WriteLn;
  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].PNGShortName, ', Angle: ', RadToDeg(FInputScans[i].RelativeAngle):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ' (after)');
end;

function TScanCorrelator.PrepareCorrect(coords: TCorrectCoords): TDoubleDynArray;
var
  cnt, iscan, ilut, radiusCnt, angleCnt: Integer;
  r, ri, rEnd, rMid, rMid2, sn, cs, px, py, cx, cy, angle, startAngle, endAngle, angleInc, angleExtents: Double;
  sinCosLUT: TPointDDynArray;
begin
  angle := (coords.AngleIdx / CCorrectAngleCount) * 2.0 * Pi;
  angleExtents := 2.0 * Pi / CCorrectAngleCount;
  startAngle := angle - angleExtents;
  endAngle := angle + angleExtents;
  angleInc := FRadiansPerRevolutionPoint;

  radiusCnt := Ceil(CCorrectAreaWidth * CCorrectAreaGroovesPerInch);
  angleCnt := Ceil((endAngle - startAngle + angleInc) / angleInc);

  iscan := 0;

  cx := FInputScans[iscan].Center.X;
  cy := FInputScans[iscan].Center.Y;

  SetLength(Result, radiusCnt * angleCnt);

  // build sin / cos lookup table

  BuildSinCosLUT(angleCnt, sinCosLUT, startAngle - FInputScans[coords.ScanIdx].RelativeAngle, endAngle - startAngle + angleInc);

  cnt := 0;
  r := CCorrectArea1Begin * 0.5 * FOutputDPI;
  rMid := CCorrectArea1End * 0.5 * FOutputDPI;
  rMid2 := CCorrectArea2Begin * 0.5 * FOutputDPI;
  rEnd := CCorrectArea2End * 0.5 * FOutputDPI;
  ri := FOutputDPI / CCorrectAreaGroovesPerInch;
  repeat
    // parse image arc

    for ilut := 0 to High(sinCosLUT) do
    begin
      cs := sinCosLUT[ilut].X;
      sn := sinCosLUT[ilut].Y;

      px := cs * r + cx;
      py := sn * r + cy;

      if FInputScans[iscan].InRangePointD(py, px) then
        Result[cnt] := FInputScans[iscan].GetPointD(py, px, isImage)
      else
        Result[cnt] := 1000.0;

      Inc(cnt);
    end;

    // move forward

    r += ri;

    if (r >= rMid) and (r < rMid2) then
      r += rMid2 - rMid;

  until r >= rEnd;

  Assert(cnt = radiusCnt * angleCnt);
end;

procedure TScanCorrelator.GradientCorrect(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  coords: PCorrectCoords absolute obj;
  preparedData: TDoubleDynArray;
  cnt, iscan, ilut, radiusCnt, angleCnt: Integer;
  mseInt, w, r, ri, rEnd, rMid, rMid2, sn, cs, px, py, cx, cy, skc, skm, rsk, gimgx, gimgy, ga, gr, angle, startAngle, endAngle, angleInc, angleExtents: Double;
  sinCosLUT: TPointDDynArray;
begin
  func := 0.0;
  if Assigned(grad) then
  begin
    grad[0] := 0.0;
    grad[1] := 0.0;
  end;

  skc := arg[0];
  skm := arg[1];

  angle := (coords^.AngleIdx / CCorrectAngleCount) * 2.0 * Pi;
  angleExtents := 2.0 * Pi / CCorrectAngleCount;
  startAngle := angle - angleExtents;
  endAngle := angle + angleExtents;
  angleInc := FRadiansPerRevolutionPoint;

  radiusCnt := Ceil(CCorrectAreaWidth * CCorrectAreaGroovesPerInch);
  angleCnt := Ceil((endAngle - startAngle + angleInc) / angleInc);

  iscan := coords^.ScanIdx;
  preparedData := coords^.PreparedData;

  cx := FInputScans[iscan].Center.X;
  cy := FInputScans[iscan].Center.Y;

  // build sin / cos lookup table

  BuildSinCosLUT(angleCnt, sinCosLUT, startAngle, endAngle - startAngle + angleInc);

  cnt := 0;
  r := CCorrectArea1Begin * 0.5 * FOutputDPI;
  rMid := CCorrectArea1End * 0.5 * FOutputDPI;
  rMid2 := CCorrectArea2Begin * 0.5 * FOutputDPI;
  rEnd := CCorrectArea2End * 0.5 * FOutputDPI;
  ri := FOutputDPI / CCorrectAreaGroovesPerInch;
  repeat
    rsk := r * skm + skc;

    // parse image arc

    for ilut := 0 to High(sinCosLUT) do
    begin
      cs := sinCosLUT[ilut].X;
      sn := sinCosLUT[ilut].Y;

      px := cs * rsk + cx;
      py := sn * rsk + cy;

      if FInputScans[iscan].InRangePointD(py, px) then
      begin
        w := 2.0 - 4.0 * abs(ilut / angleCnt - 0.5);

        mseInt := (preparedData[cnt] - FInputScans[iscan].GetPointD(py, px, isImage)) * w;

        func += Sqr(mseInt);

        if Assigned(grad) then
        begin
          gimgx := FInputScans[iscan].GetPointD(py, px, isXGradient);
          gimgy := FInputScans[iscan].GetPointD(py, px, isYGradient);

          ga := gimgx * cs + gimgy * sn;
          gr := r;

          ga *= -2.0 * mseInt;

          grad[0] += ga;
          grad[1] += ga * gr;
        end;
      end
      else
      begin
        func += 1000.0;
      end;

      Inc(cnt);
    end;

    // move forward

    r += ri;

    if (r >= rMid) and (r < rMid2) then
      r += rMid2 - rMid;

  until r >= rEnd;

  Assert(cnt = radiusCnt * angleCnt);

  func /= cnt;
  if Assigned(grad) then
  begin
    grad[0] /= cnt;
    grad[1] /= cnt;
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
    if not InRange(AIndex, 0, High(FPerAngleX)) then
      Exit;

    coords.AngleIdx := AIndex mod CCorrectAngleCount;
    coords.ScanIdx := (AIndex div CCorrectAngleCount) mod High(FInputScans) + 1;
    coords.PreparedData := PrepareCorrect(coords);

    lx := Copy(x);
    f := 1000.0;
    iter := 0;
    repeat
      prevF := f;

      case iter mod 2 of
        0:
          f := BFGSMinimize(@GradientCorrect, lx, 0.0, @coords);
        1:
          f := PowellMinimize(@PowellCorrect, lx, 1.0, 0.0, 0.0, MaxInt, @coords)[0];
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

function CompareInputScansCrop(Item1, Item2, UserParameter: Pointer): Integer;
var
  ref: TInputScan absolute UserParameter;
  s1: ^TInputScan absolute Item1;
  s2: ^TInputScan absolute Item2;
  commonArea1, commonArea2: Double;
begin
  commonArea1 := Min(s1^.CropData.EndAngle + s1^.RelativeAngle, ref.CropData.EndAngle + ref.RelativeAngle) -
                 Max(s1^.CropData.StartAngle + s1^.RelativeAngle, ref.CropData.StartAngle + ref.RelativeAngle);

  commonArea2 := Min(s2^.CropData.EndAngle + s2^.RelativeAngle, ref.CropData.EndAngle + ref.RelativeAngle) -
                 Max(s2^.CropData.StartAngle + s2^.RelativeAngle, ref.CropData.StartAngle + ref.RelativeAngle);

  Result := CompareValue(commonArea2, commonArea1);
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

  if Length(FInputScans) > 1 then
  begin
    QuickSort(FInputScans[0], 1, High(FInputScans), SizeOf(TInputScan), @CompareInputScansCrop, FInputScans[0]);
  end;

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
      x[0] := 0.0;
      x[1] := 1.0;
      Exit;
    end;

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
        bt := AngleTo02Pi(ArcTan2(AIndex - center, ox - center));

        cnt := 0;
        acc := 0;
        for i := 0 to High(FInputScans) do
        begin
          t  := FInputScans[i].RelativeAngle;
          cx := FInputScans[i].Center.X;
          cy := FInputScans[i].Center.Y;

          ct := AngleTo02Pi(bt + t);

          InterpolateX(ct, i, x);

          skc := x[0];
          skm := x[1];
          rsk := r * skm + skc;

          SinCos(ct, sn, cs);
          px := cs * rsk + cx;
          py := sn * rsk + cy;

          if FInputScans[i].InRangePointD(py, px) and
              (not In02PiExtentsAngle(ct, FInputScans[i].CropData.StartAngle, FInputScans[i].CropData.EndAngle) and
               not In02PiExtentsAngle(ct, FInputScans[i].CropData.StartAngleMirror, FInputScans[i].CropData.EndAngleMirror) or
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

  Crop;

  if FCorrectAngles then
    Correct;

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


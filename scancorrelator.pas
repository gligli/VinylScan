unit scancorrelator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, PNGComn, ZStream, MTProcs, TypInfo,
  utils, inputscan, powell, hackedwritepng;

type
  TCorrectCoords = record
    AngleIdx, ScanIdx: Integer;
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

    function PrepareAnalyze(coords: TCorrectCoords): TDoubleDynArray;
    procedure GradientAnalyze(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
    function PowellAnalyze(const arg: TVector; obj: Pointer): TScalar;
    function PrepareCorrect(coords: TCorrectCoords; baseScanIdx: Integer): TDoubleDynArray;
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
  CAnalyzeAreaGroovesPerInch = 64;

  CCorrectAngleCount = 8;
  CCorrectArea1Begin = C45RpmInnerSize + 0.1;
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

  i, pos, cnt, scanIdx: Integer;
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
  for i := 0 to cnt - 1 do
  begin
    cs := sinCosLUT[pos].X;
    sn := sinCosLUT[pos].Y;

    rri := r + ri * i;

    px := cs * rri + cx;
    py := sn * rri + cy;

    if FInputScans[scanIdx].InRangePointD(py, px) then
    begin
      mseInt := coords^.PreparedData[i] - FInputScans[scanIdx].GetPointD(py, px, isImage);

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
    for i := 0 to High(grad) do
      grad[i] /= cnt;

  Write('RMSE: ', Sqrt(func):12:9,#13);
end;

procedure TScanCorrelator.Analyze;
const
  CEpsX = 1e-6;
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
    repeat
      prevF := f;

      coords.PreparedData := PrepareAnalyze(coords);

      case Method of
        mmNone:
        begin
          f := PowellAnalyze(x, @coords);
        end;
        mmBFGS:
        begin
          f := ConjugateGradientMinimize(@GradientAnalyze, x, CEpsX, @coords);
        end;
        mmPowell:
        begin
          f := PowellMinimize(@PowellAnalyze, x, CScale, CEpsX, 0.0, MaxInt, @coords)[0];
        end;
        mmAll:
        begin
          BFGSMinimize(@GradientAnalyze, x, CEpsX, @coords);
          ConjugateGradientMinimize(@GradientAnalyze, x, CEpsX, @coords);
          f := PowellMinimize(@PowellAnalyze, x, CScale, CEpsX, 0.0, MaxInt, @coords)[0];
        end;
      end;

      f := Sqrt(f);
      Inc(iter);

      coords.GroovesPerInch *= cPhi;

      WriteLn(FInputScans[AIndex].PNGShortName, ', Iteration: ', iter:3, ', RMSE: ', f:12:9, #13);

    until SameValue(f, prevF, 1e-4) or (coords.GroovesPerInch > FOutputDPI);

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

function TScanCorrelator.PrepareCorrect(coords: TCorrectCoords; baseScanIdx: Integer): TDoubleDynArray;
var
  cnt, ilut, radiusCnt, angleCnt: Integer;
  t, r, ri, rEnd, rMid, rMid2, sn, cs, px, py, cx, cy, angle, startAngle, endAngle, angleInc, angleExtents: Double;
  sinCosLUT: TPointDDynArray;
begin
  angle := (coords.AngleIdx / CCorrectAngleCount) * 2.0 * Pi;
  angleExtents := 2.0 * Pi / CCorrectAngleCount;
  startAngle := angle - angleExtents;
  endAngle := angle + angleExtents;
  angleInc := FRadiansPerRevolutionPoint;

  radiusCnt := Ceil(CCorrectAreaWidth * CCorrectAreaGroovesPerInch);
  angleCnt := Ceil((endAngle - startAngle + angleInc) / angleInc);

  t  := FInputScans[baseScanIdx].RelativeAngle;
  cx := FInputScans[baseScanIdx].Center.X;
  cy := FInputScans[baseScanIdx].Center.Y;

  SetLength(Result, radiusCnt * angleCnt);

  // build sin / cos lookup table

  BuildSinCosLUT(angleCnt, sinCosLUT, startAngle + t, endAngle - startAngle + angleInc);

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

      if FInputScans[baseScanIdx].InRangePointD(py, px) then
        Result[cnt] := FInputScans[baseScanIdx].GetPointD(py, px, isImage)
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
  mseInt, w, t, r, ri, rEnd, rMid, rMid2, sn, cs, px, py, cx, cy, skc, skm, rsk, gimgx, gimgy, gsk, gr, angle, startAngle, endAngle, angleInc, angleExtents: Double;
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

  t  := FInputScans[iscan].RelativeAngle;
  cx := FInputScans[iscan].Center.X;
  cy := FInputScans[iscan].Center.Y;

  // build sin / cos lookup table

  BuildSinCosLUT(angleCnt, sinCosLUT, startAngle + t, endAngle - startAngle + angleInc);

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

          gsk := gimgx * cs + gimgy * sn;
          gr := r;

          gsk *= -2.0 * mseInt;

          grad[0] += gsk;
          grad[1] += gsk * gr;
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
const
  CEpsX = 1e-6;
  CScale = 1e-3;
var
  baseScanIdx, scanIdx: Integer;
  x: TVector;
  rmses: TDoubleDynArray;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    coords: TCorrectCoords;
    lx: TVector;
    iter, asIdx: Integer;
    f, prevF: Double;
  begin
    if not InRange(AIndex, 0, CCorrectAngleCount - 1) then
      Exit;

    coords.AngleIdx := AIndex;
    coords.ScanIdx := scanIdx;
    coords.PreparedData := PrepareCorrect(coords, baseScanIdx);

    asIdx := (coords.ScanIdx - 1) * CCorrectAngleCount + coords.AngleIdx;

    lx := Copy(x);
    f := 1000.0;
    iter := 0;
    repeat
      prevF := f;

      case iter mod 3 of
        0:
          f := BFGSMinimize(@GradientCorrect, lx, CEpsX, @coords);
        1:
          f := ConjugateGradientMinimize(@GradientCorrect, lx, CEpsX, @coords);
        2:
          f := PowellMinimize(@PowellCorrect, lx, CScale, CEpsX, 0.0, MaxInt, @coords)[0];
      end;

      f := Sqrt(f);
      Inc(iter);

      WriteLn(asIdx + 1:6,' / ',Length(FPerAngleX):6,', RMSE: ', f:12:9, ', Iteration: ', iter:3, #13);

    until CompareValue(f, prevF, 1e-9) >= 0;

    rmses[asIdx] := f;
    FPerAngleX[asIdx] := lx;
  end;

var
  iangle, iscan, ias, iasbase: Integer;
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

  for scanIdx := 1 to High(FInputScans) do
  begin
    baseScanIdx := scanIdx - 1;
    ProcThreadPool.DoParallelLocalProc(@DoEval, 0, CCorrectAngleCount - 1);
  end;

  // cumulate

  for iscan := 2 to High(FInputScans) do
    for iangle := 0 to CCorrectAngleCount - 1 do
    begin
      ias := (iscan - 1) * CCorrectAngleCount + iangle;
      iasbase := (iscan - 2) * CCorrectAngleCount + iangle;

      FPerAngleX[ias, 0] += FPerAngleX[iasbase, 0];
      FPerAngleX[ias, 1] *= FPerAngleX[iasbase, 1];
    end;

  // log

  WriteLn;

  for iscan := 1 to High(FInputScans) do
    for iangle := 0 to CCorrectAngleCount - 1 do
    begin
      ias := (iscan - 1) * CCorrectAngleCount + iangle;

      Write(FInputScans[iscan].PNGShortName);
      Write(', Angle:', (iangle / CCorrectAngleCount) * 360.0:9:3);
      Write(', RMSE:', rmses[ias]:12:9);
      WriteLn(',', FPerAngleX[ias, 0]:9:3, ',', FPerAngleX[ias, 1]:9:6);
    end;

  WriteLn('Worst RMSE: ', MaxValue(rmses):12:9);
end;

function CompareInputScansCrop(Item1, Item2, UserParameter: Pointer): Integer;
var
  s1: ^TInputScan absolute Item1;
  s2: ^TInputScan absolute Item2;
  center1, center2: Double;
begin
  center1 := s1^.RelativeAngle;
  center2 := s2^.RelativeAngle;

  Result := CompareValue(center2, center1);
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
    QuickSort(FInputScans[0], 0, High(FInputScans), SizeOf(TInputScan), @CompareInputScansCrop);
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

    //for i := 0 to High(x) do
    x[0] := lerp(y1[0], y2[0], alpha);
    x[1] := Exp(lerp(Ln(y1[1]), Ln(y2[1]), alpha));
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
          rsk := r * skm + skc;

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


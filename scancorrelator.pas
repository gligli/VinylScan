unit scancorrelator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, FPWritePNG, ZStream, MTProcs,
  utils, inputscan, powell, minasa, minlbfgs;

type

  { TScanCorrelator }

  TScanCorrelator = class
  private
    FInputScans: array of TInputScan;
    FMethod: TMinimizeMethod;
    FOutputPNGFileName: String;
    FOutputDPI: Integer;

    FPerSnanCrops: TDoubleDynArray2;
    FPerSnanSkews: array of TPointD;
    FPerSnanAngles: array of Double;

    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FMaxOutputImageValue: Double;

    FOutputImage: TSingleDynArray2;

    function PowellAnalyze(const x: TVector; obj: Pointer): TScalar;
    function PowellCrop(const x: TVector; obj: Pointer): TScalar;
  public
    constructor Create(const AFileNames: TStrings; AOutputDPI: Integer = 2400);
    destructor Destroy; override;

    procedure LoadPNGs;
    procedure AngleInit;
    procedure Analyze;
    procedure Crop;
    procedure Rebuild;
    procedure Save;

    procedure Run;

    property OutputPNGFileName: String read FOutputPNGFileName write FOutputPNGFileName;
    property Method: TMinimizeMethod read FMethod write FMethod;

    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;

    property OutputImage: TSingleDynArray2 read FOutputImage;
  end;

  { TScanImage }

  TScanImage = class(TFPCustomImage)
  private
    FFactor: Single;
    FScanCorrelator: TScanCorrelator;
  protected
    procedure SetInternalPixel(x,y:integer; Value:integer); override;
    function GetInternalPixel(x,y:integer) : integer; override;
  public
    constructor Create(AWidth,AHeight:integer); override;

    property ScanCorrelator: TScanCorrelator read FScanCorrelator write FScanCorrelator;
    property Factor: Single read FFactor write FFactor;
  end;

implementation

{ TScanCorrelator }

const
  CAreaBegin = C45RpmInnerSize;
  CAreaEnd = C45RpmLastMusicGroove;
  CAreaWidth = (CAreaEnd - CAreaBegin) * 0.5;
  CAreaGroovesPerInch = 16;

procedure EstimatedGradients(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
const
  CH = 1e-7;
{$if 0}
  CFCoeff: array[0 .. 7] of Double = (-1/280, 4/105, -1/5, 4/5, -4/5, 1/5, -4/105, 1/280);
  CXCoeff: array[0 .. 7] of Double = (4, 3, 2, 1, -1, -2, -3, -4);
{$else}
  CFCoeff: array[0 .. 3] of Double = (1/12, -2/3, 2/3, -1/12);
  CXCoeff: array[0 .. 3] of Double = (-2, -1, 1, 2);
{$ifend}
var
  MTGrads: TDoubleDynArray2;

  procedure DoOne(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    x: TVector;
    ig, ic: Integer;
  begin
    if AIndex = 0 then
      func := TScanCorrelator(obj).PowellAnalyze(arg, obj)
    else
    begin
      DivMod(AIndex - 1, Length(CFCoeff), ig, ic);

      x := Copy(arg);
      x[ig] += CXCoeff[ic] * CH;
      MTGrads[ig, ic] := CFCoeff[ic] * TScanCorrelator(obj).PowellAnalyze(x, obj);
    end;
  end;

var
  ig, ic: Integer;
begin
  SetLength(MTGrads, Length(arg), Length(CFCoeff));

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, Length(CFCoeff) * Length(arg) + 1 - 1);

  for ig := 0 to High(MTGrads) do
  begin
    grad[ig] := 0;
    for ic := 0 to High(MTGrads[0]) do
      grad[ig] += MTGrads[ig, ic];
    grad[ig] /= CH;
  end;

  for ig := 0 to High(arg) do
    Write(arg[ig]:10:6);
  WriteLn(func:10:6);
end;


constructor TScanCorrelator.Create(const AFileNames: TStrings; AOutputDPI: Integer);
var
  i: Integer;
begin
  FOutputDPI := AOutputDPI;
  SetLength(FInputScans, AFileNames.Count);
  SetLength(FPerSnanSkews, Length(FInputScans));
  SetLength(FPerSnanAngles, Length(FInputScans));
  for i := 0 to AFileNames.Count - 1 do
  begin
    FInputScans[i] := TInputScan.Create(Ceil(Pi * C45RpmOuterSize * FOutputDPI), AOutputDPI, True);
    FInputScans[i].PNGFileName := AFileNames[i];

    FPerSnanSkews[i].X := 1.0;
    FPerSnanSkews[i].Y := 1.0;
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

procedure TScanCorrelator.LoadPNGs;

  procedure DoOne(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    FInputScans[AIndex].Run;
    WriteLn(FInputScans[AIndex].PNGFileName);
  end;

var i: Integer;
begin
  WriteLn('LoadPNGs');

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, high(FInputScans));

  if Length(FInputScans) > 0 then
  begin
    FOutputDPI := FInputScans[0].DPI;
    for i := 1 to High(FInputScans) do
      Assert(FInputScans[i].DPI = FOutputDPI, 'mixed DPIs');
  end;

  FPointsPerRevolution := Ceil(Pi * C45RpmOuterSize * FOutputDPI);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;

  WriteLn('DPI:', FOutputDPI:6);
  WriteLn('PointsPerRevolution:', FPointsPerRevolution:8, ', outer raw sample rate: ', Round(FPointsPerRevolution * C45RpmRevolutionsPerSecond), ' Hz');

  SetLength(FOutputImage, Ceil(C45RpmOuterSize * FOutputDPI), Ceil(C45RpmOuterSize * FOutputDPI));
end;

procedure TScanCorrelator.AngleInit;
var
  iScan, iAngle, iRadius, pos: Integer;
  sn, cs, r, bestr, bestAngle: Double;
  base, angle: TDoubleDynArray;
begin
  WriteLn('AngleInit');

  if Length(FInputScans) <= 0 then
    Exit;

  SetLength(base, FInputScans[0].Width);

  pos := 0;
  for iRadius:= -Round(C45RpmLabelOuterSize * 0.5 * FOutputDPI) to -Round(C45RpmInnerSize * 0.5 * FOutputDPI) do
  begin
    base[pos] := FInputScans[0].GetPointD(FInputScans[0].Center.Y, FInputScans[0].Center.X + iRadius, isImage, imLinear);
    Inc(pos);
  end;
  for iRadius:= Round(C45RpmInnerSize * 0.5 * FOutputDPI) to Round(C45RpmLabelOuterSize * 0.5 * FOutputDPI) do
  begin
    base[pos] := FInputScans[0].GetPointD(FInputScans[0].Center.Y, FInputScans[0].Center.X + iRadius, isImage, imLinear);
    Inc(pos);
  end;
  SetLength(base, pos);
  SetLength(angle, pos);


  for iScan := 1 to High(FInputScans) do
  begin
    bestr := Infinity;
    bestAngle := 0.0;

    for iAngle := 0 to 3599 do
    begin
      SinCos(DegToRad(iAngle * 0.1), sn, cs);

      pos := 0;
      for iRadius:= -Round(C45RpmLabelOuterSize * 0.5 * FOutputDPI) to -Round(C45RpmInnerSize * 0.5 * FOutputDPI) do
      begin
        angle[pos] := FInputScans[iScan].GetPointD(FInputScans[iScan].Center.Y + sn * iRadius, FInputScans[iScan].Center.X + cs * iRadius, isImage, imLinear);
        Inc(pos);
      end;
      for iRadius:= Round(C45RpmInnerSize * 0.5 * FOutputDPI) to Round(C45RpmLabelOuterSize * 0.5 * FOutputDPI) do
      begin
        angle[pos] := FInputScans[iScan].GetPointD(FInputScans[iScan].Center.Y + sn * iRadius, FInputScans[iScan].Center.X + cs * iRadius, isImage, imLinear);
        Inc(pos);
      end;

      r := RMSE(base, angle);

      if r < bestr then
      begin
        bestr := r;
        bestAngle := DegToRad(iAngle * 0.1);
      end;
    end;

    FPerSnanAngles[iScan] := AngleToArctanExtents(bestAngle);
  end;
end;

function TScanCorrelator.PowellAnalyze(const x: TVector; obj: Pointer): TScalar;
var
  corrData: TDoubleDynArray2;
  corrMatrix: TDoubleDynArray;
  corrCoords: array of TPoint;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    pos: Integer;
    ti, ri, t, r, rEnd, sn, cs, px, py, cx, cy, rri, skx, sky: Double;
  begin
    t  := FPerSnanAngles[AIndex];
    cx := FInputScans[AIndex].Center.X;
    cy := FInputScans[AIndex].Center.Y;

    if AIndex > 0 then
    begin
      t   += x[High(FInputScans) * 0 + AIndex - 1];
      cx  += x[High(FInputScans) * 1 + AIndex - 1];
      cy  += x[High(FInputScans) * 2 + AIndex - 1];
      skx := x[High(FInputScans) * 3 + AIndex - 1];
      sky := x[High(FInputScans) * 4 + AIndex - 1];
    end
    else
    begin
      t   := 0.0;
      cx  += x[High(FInputScans) * 5 + 0];
      cy  += x[High(FInputScans) * 5 + 1];
      skx := 1.0;
      sky := 1.0;
    end;

    ti := FRadiansPerRevolutionPoint;
    ri := CAreaWidth * FOutputDPI / (CAreaGroovesPerInch * (FPointsPerRevolution - 1));

    r := CAreaBegin * 0.5 * FOutputDPI;
    rEnd := CAreaEnd * 0.5 * FOutputDPI;
    pos := 0;
    repeat
      SinCos(t + ti * pos, sn, cs);

      rri := r + ri * pos;

      px := cs * rri * skx + cx;
      py := sn * rri * sky + cy;

      Assert(pos < Length(corrData[AIndex]));

      if FInputScans[AIndex].InRangePointD(py, px) then
        corrData[AIndex, pos] := FInputScans[AIndex].GetPointD(py, px, isImage, imLinear);

      Inc(pos);
    until rri >= rEnd;

    SetLength(corrData[AIndex], pos);
  end;

  procedure DoPearson(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    corrMatrix[AIndex] := PearsonCorrelation(corrData[corrCoords[AIndex].X], corrData[corrCoords[AIndex].Y]);
  end;

var
  i, cnt: Integer;
begin
  SetLength(corrData, Length(FInputScans), Ceil(CAreaWidth * CAreaGroovesPerInch) * FPointsPerRevolution);

  if Method <> mmPowell then
  begin
    for i := 0 to High(FInputScans) do
      DoEval(i, nil, nil);
  end
  else
  begin
    ProcThreadPool.DoParallelLocalProc(@DoEval, 0, High(FInputScans));
  end;


  SetLength(corrCoords, High(FInputScans));
  SetLength(corrMatrix, Length(corrCoords));

  cnt := 0;
  for i := 1 to High(FInputScans) do
  begin
    corrCoords[cnt].X := 0;
    corrCoords[cnt].Y := i;
    Inc(cnt);
  end;
  Assert(cnt = Length(corrCoords));

  if Method <> mmPowell then
  begin
    for i := 0 to cnt - 1 do
      DoPearson(i, nil, nil);
  end
  else
  begin
    ProcThreadPool.DoParallelLocalProc(@DoPearson, 0, cnt - 1);
  end;

  Result := 0;
  for i := 0 to cnt - 1 do
    Result -= corrMatrix[i];
  if cnt > 1 then
    Result /= cnt;

  Write('Correlation: ', -Result:9:6,#13);
end;

procedure TScanCorrelator.Analyze;
var
  x, bl, bu: TVector;
  i: Integer;
  p: TPointD;
  ASAState: MinASAState;
  ASARep: MinASAReport;
  LBFGSState: MinLBFGSState;
  LBFGSRep: MinLBFGSReport;
begin
  WriteLn('Analyze');

  if Length(FInputScans) <= 0 then
    Exit;

  SetLength(x, High(FInputScans) * 5 + 2);
  SetLength(bl, Length(x));
  SetLength(bu, Length(x));

  x[High(FInputScans) * 5 + 0] := 0.0;
  x[High(FInputScans) * 5 + 1] := 0.0;

  bl[High(FInputScans) * 5 + 0] := -(FInputScans[0].Width - FInputScans[0].FirstGrooveRadius * 2.0) * 0.5;
  bl[High(FInputScans) * 5 + 1] := -(FInputScans[0].Height - FInputScans[0].FirstGrooveRadius * 2.0) * 0.5;

  bu[High(FInputScans) * 5 + 0] := (FInputScans[0].Width - FInputScans[0].FirstGrooveRadius * 2.0) * 0.5;
  bu[High(FInputScans) * 5 + 1] := (FInputScans[0].Height - FInputScans[0].FirstGrooveRadius * 2.0) * 0.5;

  for i := 1 to High(FInputScans) do
  begin
    x[High(FInputScans) * 0 + i - 1] := 0.0;
    x[High(FInputScans) * 1 + i - 1] := 0.0;
    x[High(FInputScans) * 2 + i - 1] := 0.0;
    x[High(FInputScans) * 3 + i - 1] := 1.0;
    x[High(FInputScans) * 4 + i - 1] := 1.0;

    bl[High(FInputScans) * 0 + i - 1] := -2.0 * Pi;
    bl[High(FInputScans) * 1 + i - 1] := -(FInputScans[i].Width - FInputScans[i].FirstGrooveRadius * 2.0) * 0.5;
    bl[High(FInputScans) * 2 + i - 1] := -(FInputScans[i].Height - FInputScans[i].FirstGrooveRadius * 2.0) * 0.5;
    bl[High(FInputScans) * 3 + i - 1] := 0.9;
    bl[High(FInputScans) * 4 + i - 1] := 0.9;

    bu[High(FInputScans) * 0 + i - 1] := 2.0 * Pi;
    bu[High(FInputScans) * 1 + i - 1] := (FInputScans[i].Width - FInputScans[i].FirstGrooveRadius * 2.0) * 0.5;
    bu[High(FInputScans) * 2 + i - 1] := (FInputScans[i].Height - FInputScans[i].FirstGrooveRadius * 2.0) * 0.5;
    bu[High(FInputScans) * 3 + i - 1] := 1.1;
    bu[High(FInputScans) * 4 + i - 1] := 1.1;
  end;

  case Method of
    mmNone:
    begin
      PowellAnalyze(x, Self);
    end;
    mmPowell:
    begin
      PowellMinimize(@PowellAnalyze, x, 1e-9, 1e-6, 0, MaxInt, nil);
    end;
    mmASA:
    begin
      MinASACreate(Length(x), x, bl, bu, ASAState);
      MinASASetCond(ASAState, 0, 1e-12, 1e-9, 0);
      while MinASAIteration(ASAState) do
        if ASAState.NeedFG then
          EstimatedGradients(ASAState.X, ASAState.F, ASAState.G, Self);
      MinASAResults(ASAState, x, ASARep);
    end;
    mmLBFGS:
    begin
      MinLBFGSCreate(Length(x), 5, x, LBFGSState);
      MinLBFGSSetCond(LBFGSState, 0, 1e-12, 1e-9, 0);
      while MinLBFGSIteration(LBFGSState) do
        if LBFGSState.NeedFG then
          EstimatedGradients(LBFGSState.X, LBFGSState.F, LBFGSState.G, Self);
      MinLBFGSResults(LBFGSState, x, LBFGSRep);
    end;
  end;

  FPerSnanAngles[0] := 0.0;

  p := FInputScans[0].Center;
  p.X += x[High(FInputScans) * 5 + 0];
  p.Y += x[High(FInputScans) * 5 + 1];
  FInputScans[0].Center := p;

  for i := 1 to High(FInputScans) do
  begin
    FPerSnanAngles[i] := AngleToArctanExtents(FPerSnanAngles[i] + x[High(FInputScans) * 0 + i - 1]);

    p := FInputScans[i].Center;
    p.X += x[High(FInputScans) * 1 + i - 1];
    p.Y += x[High(FInputScans) * 2 + i - 1];
    FInputScans[i].Center := p;

    p.X := x[High(FInputScans) * 3 + i - 1];
    p.Y := x[High(FInputScans) * 4 + i - 1];
    FPerSnanSkews[i] := p;
  end;

  WriteLn;
  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].PNGFileName, ', Angle: ', RadToDeg(FPerSnanAngles[i]):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ', SkewX: ', FPerSnanSkews[i].X:9:6, ', SkewY: ', FPerSnanSkews[i].Y:9:6);
end;

function TScanCorrelator.PowellCrop(const x: TVector; obj: Pointer): TScalar;
var
  inputIdx: PtrInt absolute obj;
  accCnts: TIntegerDynArray;
  accs: TDoubleDynArray;
  center, rBeg, rEnd, a0a, a1a, a0b, a1b, cx, cy: Double;

  procedure DoY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    ox, stdDevPos: Integer;
    r, bt, px, py: Double;
    stdDevArr: TDoubleDynArray;
  begin
    stdDevPos := 0;
    SetLength(stdDevArr, Length(FOutputImage[0]));

    for ox := 0 to High(FOutputImage[0]) do
    begin
      r := Sqrt(Sqr(center - AIndex) + Sqr(center - ox));
      bt := ArcTan2(center - AIndex, center - ox);

      px := ox - center + cx;
      py := AIndex - center + cy;

      if FInputScans[inputIdx].InRangePointD(py, px) and InRange(r, rBeg, rEnd) and
          not InArctanExtentsAngle(bt, a0a, a0b) and not InArctanExtentsAngle(bt, a1a, a1b) then
      begin
        stdDevArr[stdDevPos] := FInputScans[inputIdx].GetPointD(py, px, isImage, imLinear);
        Inc(stdDevPos);
      end;
    end;

    if stdDevPos > 0 then
    begin
      accs[AIndex] := -StdDev(PDouble(@stdDevArr[0]), stdDevPos);
      accCnts[AIndex] := stdDevPos;
    end;
  end;

var
  i, cnt: Integer;
begin
  Result := 1000.0;

  if (x[1] - x[0]) >= DegToRad(120.0) then
    Exit;

  a0a := AngleToArctanExtents(x[0]);
  a0b := AngleToArctanExtents(x[1]);
  a1a := AngleToArctanExtents(x[0] + Pi);
  a1b := AngleToArctanExtents(x[1] + Pi);

  rBeg := C45RpmLastMusicGroove * 0.5 * FOutputDPI;
  rEnd := C45RpmFirstMusicGroove * 0.5 * FOutputDPI;

  center := Length(FOutputImage) / 2.0;
  cx := FInputScans[inputIdx].Center.X;
  cy := FInputScans[inputIdx].Center.Y;

  SetLength(accs, Length(FOutputImage));
  SetLength(accCnts, Length(FOutputImage));

  ProcThreadPool.DoParallelLocalProc(@DoY, 0, High(FOutputImage));

  cnt := 0;
  Result := 0;
  for i := 0 to High(FOutputImage) do
  begin
    Result += accs[i] * accCnts[i];
    cnt += accCnts[i];
  end;
  Result := DivDef(Result, cnt, 1000.0);

  Write(FInputScans[inputIdx].PNGFileName, ', begin: ', RadToDeg(a0a):9:3, ', end: ', RadToDeg(a0b):9:3, ', obj: ', -Result:12:6, #13);
end;

procedure TScanCorrelator.Crop;
var
  i: PtrInt;
  x: TVector;
begin
  WriteLn('Crop');

  SetLength(x, 2);
  SetLength(FPerSnanCrops, Length(FInputScans), 4);

  for i := 0 to High(FInputScans) do
  begin
    x[0] := DegToRad(-45.0);
    x[1] := DegToRad(45.0);

    PowellMinimize(@PowellCrop, x, 1.0 / 360.0, 1e-6, 1e-6, MaxInt, Pointer(i));

    FPerSnanCrops[i, 0] := AngleToArctanExtents(x[0]);
    FPerSnanCrops[i, 1] := AngleToArctanExtents(x[1]);
    FPerSnanCrops[i, 2] := AngleToArctanExtents(x[0] + Pi);
    FPerSnanCrops[i, 3] := AngleToArctanExtents(x[1] + Pi);

    WriteLn;
  end;
end;

procedure TScanCorrelator.Rebuild;
var
  center, rBeg, rEnd, rLmg, rFmg, rLbl: Double;
  maxOutVals: TDoubleDynArray;

  procedure DoY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    i, ox, cnt: Integer;
    r, sn, cs, px, py, t, cx, cy, acc, bt, skx, sky, maxOutVal, ct: Double;
  begin
    maxOutVal := -Infinity;
    for ox := 0 to High(FOutputImage[0]) do
    begin
      r := Sqrt(Sqr(center - AIndex) + Sqr(center - ox));

      if InRange(r, rBeg, rEnd) then
      begin
        bt := ArcTan2(center - AIndex, center - ox);

        cnt := 0;
        acc := 0;
        for i := 0 to High(FInputScans) do
        begin
          t  := FPerSnanAngles[i];
          cx := FInputScans[i].Center.X;
          cy := FInputScans[i].Center.Y;
          skx := FPerSnanSkews[i].X;
          sky := FPerSnanSkews[i].Y;

          ct := AngleToArctanExtents(bt + t);

          SinCos(ct, sn, cs);
          px := cs * r * skx + cx;
          py := sn * r * sky + cy;

          if FInputScans[i].InRangePointD(py, px) and
              (not InArctanExtentsAngle(ct, FPerSnanCrops[i, 0], FPerSnanCrops[i, 1]) and
               not InArctanExtentsAngle(ct, FPerSnanCrops[i, 2], FPerSnanCrops[i, 3]) or
               (r < rLbl)) then
          begin
            acc += FInputScans[i].GetPointD(py, px, isImage, imHermite);
            Inc(cnt);
          end;
        end;

        acc := DivDef(acc, cnt, 1.0);

        FOutputImage[AIndex, ox] := acc;
        if InRange(r, rLmg, rFmg) and (cnt > 1) then
          maxOutVal := Max(maxOutVal, acc);
      end
      else
      begin
        FOutputImage[AIndex, ox] := IfThen(r >= rLmg, 0.25, 1.0);
      end;
    end;

    maxOutVals[AIndex] := maxOutVal;
  end;
begin
  WriteLn('Rebuild');

  SetLength(maxOutVals, Length(FOutputImage));

  center := Length(FOutputImage) / 2.0;
  rBeg := C45RpmInnerSize * 0.5 * FOutputDPI;
  rEnd := C45RpmOuterSize * 0.5 * FOutputDPI;
  rFmg := C45RpmFirstMusicGroove * 0.5 * FOutputDPI;
  rLmg := C45RpmLastMusicGroove * 0.5 * FOutputDPI;
  rLbl := C45RpmLabelOuterSize * 0.5 * FOutputDPI;

  ProcThreadPool.DoParallelLocalProc(@DoY, 0, High(FOutputImage));

  FMaxOutputImageValue := MaxValue(maxOutVals);
end;

procedure TScanCorrelator.Save;
var
  i: Integer;
  png: TFPWriterPNG;
  factor: Single;
  fs: TFileStream;
  fpimg: TScanImage;
begin
  WriteLn('Save ', FOutputPNGFileName);

  factor := High(Word);
  if not IsZero(FMaxOutputImageValue) then
    factor /= FMaxOutputImageValue;

  fs := TFileStream.Create(FOutputPNGFileName, fmCreate or fmShareDenyNone);
  fpimg := TScanImage.Create(Length(FOutputImage[0]), Length(FOutputImage));
  png := TFPWriterPNG.Create;
  try
    fpimg.ScanCorrelator := Self;
    fpimg.Factor := DivDef(High(Word), FMaxOutputImageValue, 1.0);
    fpimg.UsePalette := True;
    for i := 0 to High(Word) do
      fpimg.Palette.Add(FPColor(i, i, i, High(Word)));

    png.CompressedText := True;
    png.CompressionLevel := clmax;
    png.GrayScale := True;
    png.WordSized := True;
    png.Indexed := False;
    png.UseAlpha := False;

    png.ImageWrite(fs, fpimg);
  finally
    png.Free;
    fpimg.Free;
    fs.Free;

    WriteLn('Done!');
  end;
end;

procedure TScanCorrelator.Run;
begin
  LoadPNGs;

  AngleInit;
  Analyze;
  Crop;
  Rebuild;

  Save;
end;

{ TScanImage }

procedure TScanImage.SetInternalPixel(x, y: integer; Value: integer);
begin
  // nothing (read only)
end;

function TScanImage.GetInternalPixel(x, y: integer): integer;
begin
  Result := EnsureRange(Round(FScanCorrelator.FOutputImage[y, x] * FFactor), 0, High(Word));
end;

constructor TScanImage.Create(AWidth, AHeight: integer);
begin
  inherited Create(AWidth, AHeight);

  FFactor := NaN;
end;

end.


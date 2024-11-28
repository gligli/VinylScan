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
    FCorrelation: Double;
    FSinCosLUT: TPointDDynArray;

    FPerSnanCrops: TDoubleDynArray2;
    FPerSnanSkews: array of TPointD;
    FPerSnanAngles: array of Double;

    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FMaxOutputImageValue: Double;

    FOutputImage: TSingleDynArray2;

    function PowellAnalyze(const x: TVector; obj: Pointer): TScalar;
    function PowellCrop(const x: TVector; obj: Pointer): TScalar;

    procedure AngleInit;
    procedure Analyze;
    procedure Crop;
    procedure Rebuild;
  public
    constructor Create(const AFileNames: TStrings; AOutputDPI: Integer = 2400);
    destructor Destroy; override;

    procedure LoadPNGs;
    procedure Process;
    procedure Save;

    property OutputPNGFileName: String read FOutputPNGFileName write FOutputPNGFileName;
    property Method: TMinimizeMethod read FMethod write FMethod;

    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;
    property Correlation: Double read FCorrelation;

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
  CH = 1e-8;
{$if 1}
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

  //for ig := 0 to High(arg) do
  //  Write(arg[ig]:10:6);
  //WriteLn(func:10:6);
end;


constructor TScanCorrelator.Create(const AFileNames: TStrings; AOutputDPI: Integer);
var
  i: Integer;
begin
  FOutputDPI := AOutputDPI;
  FCorrelation := NaN;
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
const
  CAngleCount = 60;
var
  rBeg, rEnd: Integer;

   function DoOne(iScan: Integer; a: Double; var arr: TDoubleDynArray): Integer;
   var
     iRadius, iAngle: Integer;
     sn, cs, cy, cx: Double;
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
         arr[Result + 0] := scn.GetPointD(cy + sn * iRadius, cx + cs * iRadius, isImage, imLinear);
         arr[Result + 1] := scn.GetPointD(cy - sn * iRadius, cx - cs * iRadius, isImage, imLinear);
         Inc(Result, 2);
       end;
     end;
  end;

var
  iScan, iAngle, pos: Integer;
  a,r, bestr, bestAngle: Double;
  base, angle: TDoubleDynArray;
begin
  WriteLn('AngleInit');

  if Length(FInputScans) <= 0 then
    Exit;

  SetLength(base, FInputScans[0].Width * CAngleCount);

  rBeg := Round(C45RpmInnerSize * 0.5 * FOutputDPI);
  rEnd := Round(C45RpmLabelOuterSize * 0.5 * FOutputDPI);

  pos := DoOne(0, 0, base);

  SetLength(base, pos);
  SetLength(angle, pos);


  for iScan := 1 to High(FInputScans) do
  begin
    bestr := Infinity;
    bestAngle := 0.0;

    for iAngle := 0 to 359 do
    begin
      a := DegToRad(iAngle);

      DoOne(iScan, a, angle);

      r := RMSE(base, angle);

      if r <= bestr then
      begin
        bestr := r;
        bestAngle := a;
      end;
    end;

    FPerSnanAngles[iScan] := bestAngle;
  end;
end;

function TScanCorrelator.PowellAnalyze(const x: TVector; obj: Pointer): TScalar;
var
  corrData: TDoubleDynArray2;
  corrMatrix: TDoubleDynArray;
  corrCoords: array of TPoint;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    pos, ti0, ti1, tii: Integer;
    ri, t, r, rEnd, px, py, cx, cy, rri, skx, sky, ta, sn, cs: Double;
  begin
    if not InRange(AIndex, 0, High(FInputScans)) then
      Exit;

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
      skx := 1.0;
      sky := 1.0;
    end;

    tii := Floor(AngleTo02Pi(t) / FRadiansPerRevolutionPoint);
    ta := Frac(AngleTo02Pi(t) / FRadiansPerRevolutionPoint);
    Assert(ta >= 0);

    ri := CAreaWidth * FOutputDPI / (CAreaGroovesPerInch * (FPointsPerRevolution - 1));

    r := CAreaBegin * 0.5 * FOutputDPI;
    rEnd := CAreaEnd * 0.5 * FOutputDPI;
    pos := 0;
    ti0 := tii mod Length(FSinCosLUT);
    ti1 := (tii + 1) mod Length(FSinCosLUT);
    repeat
      cs := lerp(FSinCosLUT[ti1].X, FSinCosLUT[ti1].X, ta);
      sn := lerp(FSinCosLUT[ti0].Y, FSinCosLUT[ti1].Y, ta);

      rri := r + ri * pos;

      px := cs * rri * skx + cx;
      py := sn * rri * sky + cy;

      Assert(pos < Length(corrData[AIndex]));

      if FInputScans[AIndex].InRangePointD(py, px) then
        corrData[AIndex, pos] := FInputScans[AIndex].GetPointD(py, px, isImage, imLinear);

      Inc(pos);

      ti0 := IfThen(ti0 < High(FSinCosLUT), ti0 + 1, 0);
      ti1 := IfThen(ti1 < High(FSinCosLUT), ti1 + 1, 0);
    until rri >= rEnd;

    SetLength(corrData[AIndex], pos);
  end;

  procedure DoPearson(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, High(corrCoords)) then
      Exit;

    corrMatrix[AIndex] := PearsonCorrelation(corrData[corrCoords[AIndex].X], corrData[corrCoords[AIndex].Y]);
  end;

var
  i, cnt: Integer;
begin
  SetLength(corrData, Length(FInputScans), Ceil(CAreaWidth * CAreaGroovesPerInch) * FPointsPerRevolution);

  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, High(FInputScans));

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

  ProcThreadPool.DoParallelLocalProc(@DoPearson, 0, cnt - 1);

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

  BuildSinCosLUT(FPointsPerRevolution, FSinCosLUT);

  SetLength(x, High(FInputScans) * 5);
  SetLength(bl, Length(x));
  SetLength(bu, Length(x));

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
      FCorrelation := PowellAnalyze(x, Self);
    end;
    mmPowell:
    begin
      FCorrelation := PowellMinimize(@PowellAnalyze, x, 1e-8, 1e-9, 0, MaxInt, nil)[0];
    end;
    mmASA:
    begin
      MinASACreate(Length(x), x, bl, bu, ASAState);
      MinASASetCond(ASAState, 0, 0, 1e-9, 0);
      while MinASAIteration(ASAState) do
        if ASAState.NeedFG then
          EstimatedGradients(ASAState.X, ASAState.F, ASAState.G, Self);
      MinASAResults(ASAState, x, ASARep);

      FCorrelation := ASAState.F;
    end;
    mmLBFGS:
    begin
      MinLBFGSCreate(Length(x), 5, x, LBFGSState);
      MinLBFGSSetCond(LBFGSState, 0, 0, 1e-9, 0);
      while MinLBFGSIteration(LBFGSState) do
        if LBFGSState.NeedFG then
          EstimatedGradients(LBFGSState.X, LBFGSState.F, LBFGSState.G, Self);
      MinLBFGSResults(LBFGSState, x, LBFGSRep);

      FCorrelation := LBFGSState.F;
    end;
  end;

  Assert(not IsNan(FCorrelation));

  FPerSnanAngles[0] := 0.0;

  for i := 1 to High(FInputScans) do
  begin
    FPerSnanAngles[i] += x[High(FInputScans) * 0 + i - 1];

    p := FInputScans[i].Center;
    p.X += x[High(FInputScans) * 1 + i - 1];
    p.Y += x[High(FInputScans) * 2 + i - 1];
    FInputScans[i].Center := p;

    FPerSnanSkews[i].X := x[High(FInputScans) * 3 + i - 1];
    FPerSnanSkews[i].Y := x[High(FInputScans) * 4 + i - 1];
  end;

  WriteLn;
  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].PNGFileName, ', Angle: ', RadToDeg(FPerSnanAngles[i]):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ', SkewX: ', FPerSnanSkews[i].X:9:6, ', SkewY: ', FPerSnanSkews[i].Y:9:6);
end;

function TScanCorrelator.PowellCrop(const x: TVector; obj: Pointer): TScalar;
var
  inputIdx: PtrInt absolute obj;
  rBeg, rEnd, a0a, a1a, a0b, a1b, cx, cy, ri, rri, sn, cs, bt, px, py: Double;
  pos, arrPos: Integer;
  stdDevArr: TDoubleDynArray;
begin
  Result := 1000.0;

  if AngleTo02Pi(x[1] - x[0]) >= DegToRad(120.0) then
    Exit;

  a0a := AngleTo02Pi(x[0]);
  a0b := AngleTo02Pi(x[1]);
  a1a := AngleTo02Pi(x[0] + Pi);
  a1b := AngleTo02Pi(x[1] + Pi);

  rBeg := C45RpmLastMusicGroove * 0.5 * FOutputDPI;
  rEnd := C45RpmFirstMusicGroove * 0.5 * FOutputDPI;

  cx := FInputScans[inputIdx].Center.X;
  cy := FInputScans[inputIdx].Center.Y;

  SetLength(stdDevArr, Ceil((rEnd - rBeg) / FOutputDPI * CAreaGroovesPerInch) * FPointsPerRevolution);

  ri := (rEnd - rBeg) / (CAreaGroovesPerInch * (FPointsPerRevolution - 1));

  pos := 0;
  arrPos := 0;
  repeat
    bt := AngleTo02Pi(FRadiansPerRevolutionPoint * pos);

    SinCos(bt, sn, cs);

    rri := rBeg + ri * pos;

    px := cs * rri + cx;
    py := sn * rri + cy;

    Assert(pos < Length(stdDevArr));

    if FInputScans[inputIdx].InRangePointD(py, px) and
        not In02PiExtentsAngle(bt, a0a, a0b) and not In02PiExtentsAngle(bt, a1a, a1b) then
    begin
      stdDevArr[arrPos] := FInputScans[inputIdx].GetPointD(py, px, isImage, imLinear);
      Inc(arrPos);
    end;

    Inc(pos);
  until rri >= rEnd;

  Result := -StdDev(PDouble(@stdDevArr[0]), arrPos);

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
    x[0] := AngleTo02Pi(DegToRad(-45.0));
    x[1] := AngleTo02Pi(DegToRad(45.0));

    PowellMinimize(@PowellCrop, x, 1.0 / 360.0, 1e-6, 1e-6, MaxInt, Pointer(i));

    FPerSnanCrops[i, 0] := AngleTo02Pi(x[0]);
    FPerSnanCrops[i, 1] := AngleTo02Pi(x[1]);
    FPerSnanCrops[i, 2] := AngleTo02Pi(x[0] + Pi);
    FPerSnanCrops[i, 3] := AngleTo02Pi(x[1] + Pi);

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
        bt := AngleTo02Pi(ArcTan2(center - AIndex, center - ox));

        cnt := 0;
        acc := 0;
        for i := 0 to High(FInputScans) do
        begin
          t  := FPerSnanAngles[i];
          cx := FInputScans[i].Center.X;
          cy := FInputScans[i].Center.Y;
          skx := FPerSnanSkews[i].X;
          sky := FPerSnanSkews[i].Y;

          ct := AngleTo02Pi(t + bt);

          SinCos(ct, sn, cs);
          px := cs * r * skx + cx;
          py := sn * r * sky + cy;

          if FInputScans[i].InRangePointD(py, px) and
              (not In02PiExtentsAngle(ct, FPerSnanCrops[i, 0], FPerSnanCrops[i, 1]) and
               not In02PiExtentsAngle(ct, FPerSnanCrops[i, 2], FPerSnanCrops[i, 3]) or
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

procedure TScanCorrelator.Process;
begin
  AngleInit;
  Analyze;
  Crop;
  Rebuild;
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


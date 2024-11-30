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

    FPerSnanCrops: TDoubleDynArray2;
    FPerSnanSkews: array of TPointD;
    FPerSnanAngles: array of Double;

    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FMaxOutputImageValue: Double;

    FOutputImage: TSingleDynArray2;

    procedure GradientsAnalyze(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
    function PowellAnalyze(const arg: TVector; obj: Pointer): TScalar;
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
  CAreaEnd = C45RpmFirstMusicGroove;
  CAreaWidth = (CAreaEnd - CAreaBegin) * 0.5;
  CAreaGroovesPerInch = 60;

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
  CAngleCount = 180;
var
  rBeg, rEnd: Integer;
  base: TDoubleDynArray;

   function DoAngle(iScan: Integer; a: Double; var arr: TDoubleDynArray): Integer;
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

  procedure DoScan(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iAngle: Integer;
    a, r, bestr, bestAngle: Double;
    angle: TDoubleDynArray;
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

      r := MSE(base, angle);

      if r <= bestr then
      begin
        bestr := r;
        bestAngle := a;
      end;
    end;

    FPerSnanAngles[AIndex] := bestAngle;
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
var
  imgData: TDoubleDynArray2;
  imgResults: TDoubleDynArray;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    pos: Integer;
    ti, ri, t, r, px, py, cx, cy, rri, skx, sky, sn, cs: Double;
  begin
    if not InRange(AIndex, 0, High(FInputScans)) then
      Exit;

    if AIndex > 0 then
    begin
      t   := arg[High(FInputScans) * 0 + AIndex - 1];
      cx  := arg[High(FInputScans) * 1 + AIndex - 1];
      cy  := arg[High(FInputScans) * 2 + AIndex - 1];
      skx := arg[High(FInputScans) * 3 + AIndex - 1];
      sky := arg[High(FInputScans) * 4 + AIndex - 1];
    end
    else
    begin
      t   := FPerSnanAngles[AIndex];
      cx  := FInputScans[AIndex].Center.X;
      cy  := FInputScans[AIndex].Center.Y;
      skx := FPerSnanSkews[AIndex].X;
      sky := FPerSnanSkews[AIndex].Y;
    end;

    ti := FRadiansPerRevolutionPoint;
    ri := FOutputDPI / (CAreaGroovesPerInch * (FPointsPerRevolution - 1));

    r := CAreaBegin * 0.5 * FOutputDPI;
    pos := 0;
    repeat
      SinCos(t + ti * pos, sn, cs);

      rri := r + ri * pos;

      px := cs * rri * skx + cx;
      py := sn * rri * sky + cy;

      if FInputScans[AIndex].InRangePointD(py, px) then
        imgData[AIndex, pos] := FInputScans[AIndex].GetPointD(py, px, isImage, imLinear);

      Inc(pos);
    until pos >= Length(imgData[0]);

    SetLength(imgData[AIndex], pos);
  end;

  procedure DoMSE(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, High(FInputScans) - 1) then
      Exit;

    imgResults[AIndex] := MSE(imgData[0], imgData[AIndex + 1]);
  end;

begin
  SetLength(imgData, Length(FInputScans), Ceil(CAreaWidth * CAreaGroovesPerInch) * FPointsPerRevolution);
  SetLength(imgResults, High(FInputScans));

  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, High(FInputScans));
  ProcThreadPool.DoParallelLocalProc(@DoMSE, 0, High(FInputScans) - 1);

  Result := Sum(imgResults);

  Write('RMSE: ', Sqrt(Mean(imgResults)):9:6,#13);
end;

procedure TScanCorrelator.GradientsAnalyze(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  gradData: TDoubleDynArray2;
  imgData: TDoubleDynArray2;
  imgResults: TDoubleDynArray;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    pos: Integer;
    gr, ri, t, r, px, py, cx, cy, rri, skx, sky, ti, sn, cs, p, gimgx, gimgy, gt, gcx, gcy, gskx, gsky: Double;
  begin
    if not InRange(AIndex, 0, High(FInputScans)) then
      Exit;

    if AIndex > 0 then
    begin
      t   := arg[High(FInputScans) * 0 + AIndex - 1];
      cx  := arg[High(FInputScans) * 1 + AIndex - 1];
      cy  := arg[High(FInputScans) * 2 + AIndex - 1];
      skx := arg[High(FInputScans) * 3 + AIndex - 1];
      sky := arg[High(FInputScans) * 4 + AIndex - 1];
    end
    else
    begin
      t   := FPerSnanAngles[AIndex];
      cx  := FInputScans[AIndex].Center.X;
      cy  := FInputScans[AIndex].Center.Y;
      skx := FPerSnanSkews[AIndex].X;
      sky := FPerSnanSkews[AIndex].Y;
    end;

    ti := FRadiansPerRevolutionPoint;
    ri := FOutputDPI / (CAreaGroovesPerInch * (FPointsPerRevolution - 1));

    r := CAreaBegin * 0.5 * FOutputDPI;
    pos := 0;
    repeat
      SinCos(t + ti * pos, sn, cs);

      rri := r + ri * pos;

      px := cs * rri * skx + cx;
      py := sn * rri * sky + cy;

      if FInputScans[AIndex].InRangePointD(py, px) then
      begin
        p := FInputScans[AIndex].GetPointD(py, px, isImage, imLinear);
        imgData[AIndex, pos] := p;

        if AIndex > 0 then
        begin
          gimgx := FInputScans[AIndex].GetPointD(py, px, isXGradient, imLinear);
          gimgy := FInputScans[AIndex].GetPointD(py, px, isYGradient, imLinear);

          gt := (gimgx * -sn + gimgy * cs) * ri;
          gcx := gimgx;
          gcy := gimgy;
          gskx := gimgx * cs * ri;
          gsky := gimgy * sn * ri;

          gradData[High(FInputScans) * 0 + AIndex - 1, pos] := gt;
          gradData[High(FInputScans) * 1 + AIndex - 1, pos] := gcx;
          gradData[High(FInputScans) * 2 + AIndex - 1, pos] := gcy;
          gradData[High(FInputScans) * 3 + AIndex - 1, pos] := gskx;
          gradData[High(FInputScans) * 4 + AIndex - 1, pos] := gsky;
        end;
      end;

      Inc(pos);
    until pos >= Length(imgData[0]);
  end;

  procedure DoMSE(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iArg: Integer;
  begin
    if not InRange(AIndex, 0, High(FInputScans) - 1) then
      Exit;

    imgResults[AIndex] := MSE(imgData[0], imgData[AIndex + 1]);

    for iArg := 0 to 4 do
      grad[High(FInputScans) * iArg + AIndex] := MSEGradient(imgData[0], imgData[AIndex + 1], gradData[High(FInputScans) * iArg + AIndex]);
  end;

var
  cnt: Integer;
begin
  cnt := Ceil(CAreaWidth * CAreaGroovesPerInch * FPointsPerRevolution);
  SetLength(imgResults, High(FInputScans));
  SetLength(imgData, Length(FInputScans), cnt);
  SetLength(gradData, Length(grad), cnt);

  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, High(FInputScans));
  ProcThreadPool.DoParallelLocalProc(@DoMSE, 0, High(FInputScans) - 1);

  func := Sum(imgResults);

  Write('RMSE: ', Sqrt(Mean(imgResults)):9:6,#13);
end;

procedure TScanCorrelator.Analyze;
var
  x, bl, bu: TVector;
  i: Integer;
  radiusOuter: Double;
  p: TPointD;
  ASAState: MinASAState;
  ASARep: MinASAReport;
  LBFGSState: MinLBFGSState;
  LBFGSRep: MinLBFGSReport;
begin
  WriteLn('Analyze');

  if Length(FInputScans) <= 1 then
    Exit;

  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].PNGFileName, ', Angle: ', RadToDeg(FPerSnanAngles[i]):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ' (before)');

  SetLength(x, High(FInputScans) * 5);
  SetLength(bl, Length(x));
  SetLength(bu, Length(x));

  radiusOuter := Round(C45RpmOuterSize * FOutputDPI * 0.5);

  for i := 1 to High(FInputScans) do
  begin
    x[High(FInputScans) * 0 + i - 1] := FPerSnanAngles[i];
    x[High(FInputScans) * 1 + i - 1] := FInputScans[i].Center.X;
    x[High(FInputScans) * 2 + i - 1] := FInputScans[i].Center.Y;
    x[High(FInputScans) * 3 + i - 1] := FPerSnanSkews[i].X;
    x[High(FInputScans) * 4 + i - 1] := FPerSnanSkews[i].Y;

    bl[High(FInputScans) * 0 + i - 1] := FPerSnanAngles[i] - 2.0 * Pi;
    bl[High(FInputScans) * 1 + i - 1] := radiusOuter;
    bl[High(FInputScans) * 2 + i - 1] := radiusOuter;
    bl[High(FInputScans) * 3 + i - 1] := 0.9;
    bl[High(FInputScans) * 4 + i - 1] := 0.9;

    bu[High(FInputScans) * 0 + i - 1] := FPerSnanAngles[i] + 2.0 * Pi;
    bu[High(FInputScans) * 1 + i - 1] := FInputScans[i].Width - radiusOuter;
    bu[High(FInputScans) * 2 + i - 1] := FInputScans[i].Height - radiusOuter;
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
      MinASASetCond(ASAState, 0, 1e-12, 0, 0);
      while MinASAIteration(ASAState) do
        if ASAState.NeedFG then
          GradientsAnalyze(ASAState.X, ASAState.F, ASAState.G, Self);
      MinASAResults(ASAState, x, ASARep);

      FCorrelation := ASAState.F;
    end;
    mmLBFGS:
    begin
      MinLBFGSCreate(Length(x), Length(x), x, LBFGSState);
      MinLBFGSSetCond(LBFGSState, 0, 1e-12, 0, 0);
      while MinLBFGSIteration(LBFGSState) do
        if LBFGSState.NeedFG then
          GradientsAnalyze(LBFGSState.X, LBFGSState.F, LBFGSState.G, Self);
      MinLBFGSResults(LBFGSState, x, LBFGSRep);

      FCorrelation := LBFGSState.F;
    end;
  end;

  Assert(not IsNan(FCorrelation));

  FPerSnanAngles[0] := 0.0;

  for i := 1 to High(FInputScans) do
  begin
    FPerSnanAngles[i] := x[High(FInputScans) * 0 + i - 1];
    p.X := x[High(FInputScans) * 1 + i - 1];
    p.Y := x[High(FInputScans) * 2 + i - 1];
    FPerSnanSkews[i].X := x[High(FInputScans) * 3 + i - 1];
    FPerSnanSkews[i].Y := x[High(FInputScans) * 4 + i - 1];
    FInputScans[i].Center := p;
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


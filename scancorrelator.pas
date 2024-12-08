unit scancorrelator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, PNGComn, ZStream, MTProcs, TypInfo,
  utils, inputscan, powell, hackedwritepng;

type

  { TScanCorrelator }

  TScanCorrelator = class
  private
    FInputScans: TInputScanDynArray;
    FMethod: TMinimizeMethod;
    FOutputPNGFileName: String;
    FOutputDPI: Integer;
    FObjective: Double;

    FPerSnanCrops: TDoubleDynArray2;
    FPerSnanSkews: array of TPointD;
    FPerSnanAngles: array of Double;

    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;

    FOutputImage: TWordDynArray2;

    function GetImageDerivationOperator: TImageDerivationOperator;
    procedure SetImageDerivationOperator(AValue: TImageDerivationOperator);

    procedure GradientsAnalyze(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
    function PowellAnalyze(const arg: TVector; obj: Pointer): TScalar;
    function PowellCrop(const x: TVector; obj: Pointer): TScalar;

    procedure AngleInit;
    procedure Crop;
    function Analyze(AMethod: TMinimizeMethod): Double;
    procedure Rebuild;
  public
    constructor Create(const AFileNames: TStrings; AOutputDPI: Integer = 2400);
    destructor Destroy; override;

    procedure LoadPNGs;
    procedure Process;
    procedure Save;

    property OutputPNGFileName: String read FOutputPNGFileName write FOutputPNGFileName;
    property Method: TMinimizeMethod read FMethod write FMethod;
    property ImageDerivationOperator: TImageDerivationOperator read GetImageDerivationOperator write SetImageDerivationOperator;

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

{ TScanCorrelator }

const
  CAreaBegin = C45RpmInnerSize;
  CAreaEnd = C45RpmLastMusicGroove;
  CAreaWidth = (CAreaEnd - CAreaBegin) * 0.5;
  CAreaGroovesPerInchAnalyze = 60;
  CAreaGroovesPerInchCrop = 16;

constructor TScanCorrelator.Create(const AFileNames: TStrings; AOutputDPI: Integer);
var
  i: Integer;
begin
  FOutputDPI := AOutputDPI;
  FObjective := NaN;
  FMethod := mmGradientDescent;
  SetLength(FInputScans, AFileNames.Count);
  SetLength(FPerSnanSkews, Length(FInputScans));
  SetLength(FPerSnanAngles, Length(FInputScans));
  SetLength(FPerSnanCrops, Length(FInputScans), 4);

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
      Assert(FInputScans[i].DPI = FOutputDPI, 'InputScans mixed DPIs!');
  end;

  FPointsPerRevolution := Ceil(Pi * CAreaEnd * FOutputDPI);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;

  WriteLn('DPI:', FOutputDPI:6);
  WriteLn('PointsPerRevolution:', FPointsPerRevolution:8, ', inner raw sample rate: ', Round(FPointsPerRevolution * C45RpmRevolutionsPerSecond), ' Hz');

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
begin
  GradientsAnalyze(arg, Result, nil, obj);
end;

procedure TScanCorrelator.GradientsAnalyze(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  gradData: TDoubleDynArray2;
  imgData: TDoubleDynArray2;
  imgResults: TDoubleDynArray;
  rLbl: Double;
  paramCount: Integer;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    i, pos: Integer;
    ct, ri, t, r, px, py, cx, cy, rri, skx, sky, sn, cs, p, gimgx, gimgy, gr, gt, gcx, gcy, gskx, gsky, invLen: Double;
    cropped: Boolean;
    sinCosLUT: TPointDDynArray;
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

    BuildSinCosLUT(FPointsPerRevolution, sinCosLUT, t);

    ri := FOutputDPI / (CAreaGroovesPerInchAnalyze * (FPointsPerRevolution - 1));
    invLen := 1.0 / Length(imgData[0]);

    r := CAreaBegin * 0.5 * FOutputDPI;
    pos := 0;
    for i := 0 to High(imgData[0]) do
    begin
      cs := sinCosLUT[pos].X;
      sn := sinCosLUT[pos].Y;

      rri := r + ri * i;

      px := cs * rri * skx + cx;
      py := sn * rri * sky + cy;

      if FInputScans[AIndex].InRangePointD(py, px) then
      begin
        ct := AngleTo02Pi(t + pos * FRadiansPerRevolutionPoint);
        cropped := (In02PiExtentsAngle(ct, FPerSnanCrops[AIndex, 0], FPerSnanCrops[AIndex, 1]) or
                   In02PiExtentsAngle(ct, FPerSnanCrops[AIndex, 2], FPerSnanCrops[AIndex, 3])) and
                   (rri >= rLbl);

        p := NaN;
        if not cropped then
          p := FInputScans[AIndex].GetPointD(py, px, isImage, imLinear);

        imgData[AIndex, i] := p;

        if (AIndex > 0) and Assigned(grad) then
        begin
          gt := NaN;
          gcx := NaN;
          gcy := NaN;
          gskx := NaN;
          gsky := NaN;
          if not cropped then
          begin
            gimgx := FInputScans[AIndex].GetPointD(py, px, isXGradient, imLinear);
            gimgy := FInputScans[AIndex].GetPointD(py, px, isYGradient, imLinear);

            gr := rri * invLen;

            gt := (gimgx * -sn * skx + gimgy * cs * sky) * gr;
            gcx := gimgx;
            gcy := gimgy;
            gskx := gimgx * cs * gr;
            gsky := gimgy * sn * gr;
          end;

          gradData[High(FInputScans) * 0 + AIndex - 1, i] := gt;
          gradData[High(FInputScans) * 1 + AIndex - 1, i] := gcx;
          gradData[High(FInputScans) * 2 + AIndex - 1, i] := gcy;
          gradData[High(FInputScans) * 3 + AIndex - 1, i] := gskx;
          gradData[High(FInputScans) * 4 + AIndex - 1, i] := gsky;
        end;
      end;

      Inc(pos);

      if pos >= FPointsPerRevolution then
        pos := 0;
    end;
  end;

  procedure DoMSE(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iArg: Integer;
  begin
    if not InRange(AIndex, 0, High(FInputScans) - 1) then
      Exit;

    imgResults[AIndex] := MSE(imgData[0], imgData[AIndex + 1]);

    for iArg := 0 to paramCount - 1 do
      grad[High(FInputScans) * iArg + AIndex + 1 - 1] := MSEGradient(imgData[0], imgData[AIndex + 1], gradData[High(FInputScans) * iArg + AIndex + 1 - 1]);
  end;

var
  cnt: Integer;
begin
  rLbl := C45RpmLabelOuterSize * 0.5 * FOutputDPI;
  paramCount := Length(grad) div High(FInputScans);

  cnt := Ceil(CAreaWidth * CAreaGroovesPerInchAnalyze * FPointsPerRevolution);
  SetLength(imgResults, High(FInputScans));
  SetLength(imgData, Length(FInputScans), cnt);
  SetLength(gradData, Length(grad), cnt);

  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, High(FInputScans));
  ProcThreadPool.DoParallelLocalProc(@DoMSE, 0, High(FInputScans) - 1);

  func := Sum(imgResults);

  Write('RMSE: ', Sqrt(Mean(imgResults)):12:9,#13);
end;

function TScanCorrelator.GetImageDerivationOperator: TImageDerivationOperator;
begin
  Result := idoSobel;
  if Length(FInputScans) > 0 then
    Result := FInputScans[0].ImageDerivationOperator;
end;

function TScanCorrelator.Analyze(AMethod: TMinimizeMethod): Double;
var
  x: TVector;
  i: Integer;
  p: TPointD;
begin
  Result := NaN;

  WriteLn('Analyze ', GetEnumName(TypeInfo(TMinimizeMethod), Ord(AMethod)), ', ', GetEnumName(TypeInfo(TImageDerivationOperator), Ord(GetImageDerivationOperator)));

  if Length(FInputScans) <= 1 then
    Exit;

  for i := 0 to High(FInputScans) do
    WriteLn(FInputScans[i].PNGFileName, ', Angle: ', RadToDeg(FPerSnanAngles[i]):9:3, ', CenterX: ', FInputScans[i].Center.X:9:3, ', CenterY: ', FInputScans[i].Center.Y:9:3, ' (before)');

  SetLength(x, High(FInputScans) * 5);
  for i := 1 to High(FInputScans) do
  begin
    x[High(FInputScans) * 0 + i - 1] := FPerSnanAngles[i];
    x[High(FInputScans) * 1 + i - 1] := FInputScans[i].Center.X;
    x[High(FInputScans) * 2 + i - 1] := FInputScans[i].Center.Y;
    x[High(FInputScans) * 3 + i - 1] := FPerSnanSkews[i].X;
    x[High(FInputScans) * 4 + i - 1] := FPerSnanSkews[i].Y;
  end;

  case AMethod of
    mmNone:
    begin
      Result := PowellAnalyze(x, Self);
    end;
    mmGradientDescent:
    begin
      Result := GradientDescentMinimize(@GradientsAnalyze, x, 1.0);
    end;
    mmPowell:
    begin
      Result := PowellMinimize(@PowellAnalyze, x, 1e-8, 1e-9, 1e-9, MaxInt, nil)[0];
    end;
  end;

  Assert(not IsNan(Result));

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
  mnArr: TDoubleDynArray;
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

  SetLength(mnArr, Ceil((rEnd - rBeg) / FOutputDPI * CAreaGroovesPerInchCrop) * FPointsPerRevolution);

  ri := (rEnd - rBeg) / (CAreaGroovesPerInchCrop * (FPointsPerRevolution - 1));

  pos := 0;
  arrPos := 0;
  repeat
    bt := AngleTo02Pi(FRadiansPerRevolutionPoint * pos);

    SinCos(bt, sn, cs);

    rri := rBeg + ri * pos;

    px := cs * rri + cx;
    py := sn * rri + cy;

    Assert(pos < Length(mnArr));

    if FInputScans[inputIdx].InRangePointD(py, px) and
        not In02PiExtentsAngle(bt, a0a, a0b) and not In02PiExtentsAngle(bt, a1a, a1b) then
    begin
      mnArr[arrPos] := FInputScans[inputIdx].GetPointD(py, px, isImage, imLinear);
      Inc(arrPos);
    end;

    Inc(pos);
  until rri >= rEnd;

  Result := -Mean(PDouble(@mnArr[0]), arrPos);

  Write(FInputScans[inputIdx].PNGFileName, ', begin: ', RadToDeg(a0a):9:3, ', end: ', RadToDeg(a0b):9:3, ', obj: ', -Result:12:6, #13);
end;

procedure TScanCorrelator.Crop;
var
  i: PtrInt;
  x: TVector;
begin
  WriteLn('Crop');

  SetLength(x, 2);

  for i := 0 to High(FInputScans) do
  begin
    x[0] := AngleTo02Pi(DegToRad(-30.0));
    x[1] := AngleTo02Pi(DegToRad(30.0));

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
  center, rBeg, rEnd, rLmg, rLbl: Double;

  procedure DoY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    i, ox, cnt: Integer;
    r, sn, cs, px, py, t, cx, cy, acc, bt, skx, sky, ct: Double;
  begin
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

        FOutputImage[AIndex, ox] := EnsureRange(Round(acc * High(Word)), 0, High(Word));
      end
      else
      begin
        FOutputImage[AIndex, ox] := IfThen(r >= rLmg, Round(0.25 * High(Word)), Round(1.0 * High(Word)));
      end;
    end;
  end;
begin
  WriteLn('Rebuild');

  center := Length(FOutputImage) / 2.0;
  rBeg := C45RpmInnerSize * 0.5 * FOutputDPI;
  rEnd := C45RpmOuterSize * 0.5 * FOutputDPI;
  rLmg := C45RpmLastMusicGroove * 0.5 * FOutputDPI;
  rLbl := C45RpmLabelOuterSize * 0.5 * FOutputDPI;

  ProcThreadPool.DoParallelLocalProc(@DoY, 0, High(FOutputImage));
end;

procedure TScanCorrelator.SetImageDerivationOperator(AValue: TImageDerivationOperator);
var
  i: Integer;
begin
  for i := 0 to High(FInputScans) do
    FInputScans[i].ImageDerivationOperator := AValue;
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
  Crop;

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
      Analyze(mmPowell);
      obj := Analyze(mmGradientDescent);
      Inc(iter);
    until SameValue(obj, prevObj, 1e-9);
  end;

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


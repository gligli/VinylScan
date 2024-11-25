unit scancorrelator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, FPWritePNG, ZStream, MTProcs,
  utils, inputscan, powell;

type

  { TScanCorrelator }

  TScanCorrelator = class
  private
    FInputScans: array of TInputScan;
    FOutputPNGFileName: String;
    FOutputDPI: Integer;

    FPerSnanCrops: TDoubleDynArray2;
    FPerAngleX: TDoubleDynArray2;
    FInitF: Double;


    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FMaxOutputImageValue: Double;

    FOutputImage: TSingleDynArray2;

    function PowellAnalyze(const x: TVector; obj: Pointer): TScalar;
    function PowellCrop(const x: TVector; obj: Pointer): TScalar;
    function PowellCorrect(const x: TVector; obj: Pointer): TScalar;
  public
    constructor Create(const AFileNames: TStrings; AOutputDPI: Integer = 2400);
    destructor Destroy; override;

    procedure LoadPNGs;
    procedure Analyze;
    procedure Crop;
    procedure Correct;
    procedure Rebuild;
    procedure Save;

    procedure Run;

    property OutputPNGFileName: String read FOutputPNGFileName write FOutputPNGFileName;

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
  CCorrectAngleCount = 360;
  CPrecMul = 100;
  CInitAreaBegin = C45RpmInnerSize;
  CInitAreaEnd = C45RpmLabelOuterSize;
  CInitAreaWidth = (CInitAreaEnd - CInitAreaBegin) * 0.5;
  CCorrectAreaBegin = C45RpmLastMusicGroove;
  CCorrectAreaEnd = (C45RpmFirstMusicGroove + C45RpmOuterSize) * 0.5;
  CCorrectAreaWidth = (CCorrectAreaEnd - CCorrectAreaBegin) * 0.5;
  CAreaGroovesPerInch = 42;

constructor TScanCorrelator.Create(const AFileNames: TStrings; AOutputDPI: Integer);
var
  i: Integer;
begin
  FOutputDPI := AOutputDPI;
  SetLength(FInputScans, AFileNames.Count);
  for i := 0 to AFileNames.Count - 1 do
  begin
    FInputScans[i] := TInputScan.Create(Ceil(Pi * C45RpmOuterSize * FOutputDPI), AOutputDPI, True);
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

function TScanCorrelator.PowellAnalyze(const x: TVector; obj: Pointer): TScalar;
var
  corrData: TDoubleDynArray2;
  corrMatrix: TDoubleDynArray;
  corrCoords: array of TPoint;

  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    pos: Integer;
    ti, ri, t, r, rEnd, sn, cs, px, py, cx, cy, rri: Double;
  begin
    t  := x[Length(FInputScans) * 0 + AIndex];
    cx := x[Length(FInputScans) * 1 + AIndex];
    cy := x[Length(FInputScans) * 2 + AIndex];

    ti := FRadiansPerRevolutionPoint;
    ri := CInitAreaWidth * FOutputDPI / (CAreaGroovesPerInch * (FPointsPerRevolution - 1));

    r := CInitAreaBegin * 0.5 * FOutputDPI;
    rEnd := CInitAreaEnd * 0.5 * FOutputDPI;
    pos := 0;
    repeat
      SinCos(t + ti * pos, sn, cs);

      rri := r + ri * pos;

      px := cs * rri + cx;
      py := sn * rri + cy;

      Assert(pos < Length(corrData[AIndex]));

      if FInputScans[AIndex].InRangePointD(py, px) then
        corrData[AIndex, pos] := FInputScans[AIndex].GetPointD(py, px, isImage, imHermite);

      Inc(pos);
    until rri >= rEnd;

    SetLength(corrData[AIndex], pos);
  end;

  procedure DoPearson(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    corrMatrix[AIndex] := PearsonCorrelation(corrData[corrCoords[AIndex].X], corrData[corrCoords[AIndex].Y]);
  end;

var
  i, j, cnt: Integer;
begin
  SetLength(corrData, Length(FInputScans), Ceil(CInitAreaWidth * CAreaGroovesPerInch) * FPointsPerRevolution);

  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, High(FInputScans));

  SetLength(corrCoords, Length(FInputScans) * High(FInputScans) div 2);
  SetLength(corrMatrix, Length(corrCoords));

  cnt := 0;
  for i := 0 to High(FInputScans) do
    for j := i + 1 to High(FInputScans) do
    begin
      corrCoords[cnt].X := i;
      corrCoords[cnt].Y := j;
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
  x: TVector;
  i: Integer;
  cp: TPointD;
begin
  WriteLn('Analyze');

  SetLength(x, Length(FInputScans) * 3);
  for i := 0 to High(FInputScans) do
  begin
    x[Length(FInputScans) * 0 + i] := FInputScans[i].GrooveStartAngle;
    x[Length(FInputScans) * 1 + i] := FInputScans[i].Center.X;
    x[Length(FInputScans) * 2 + i] := FInputScans[i].Center.Y;
  end;

  FInitF := PowellMinimize(@PowellAnalyze, x, 1e-9, 1e-6, 1e-9, MaxInt, nil)[0];

  for i := 0 to High(FInputScans) do
  begin
    FInputScans[i].GrooveStartAngle := AngleToArctanExtents(x[Length(FInputScans) * 0 + i]);
    cp.X := x[Length(FInputScans) * 1 + i];
    cp.Y := x[Length(FInputScans) * 2 + i];
    FInputScans[i].Center := cp;
  end;

  WriteLn;
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

  Write(inputIdx + 1:4, ' / ', Length(FInputScans):4, ', begin: ', RadToDeg(a0a):9:3, ', end: ', RadToDeg(a0b):9:3, ', obj: ', -Result:12:6, #13);
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

function TScanCorrelator.PowellCorrect(const x: TVector; obj: Pointer): TScalar;
var
  angleIdx: PtrInt absolute obj;
  corrData: TDoubleDynArray2;
  i, j, pos, startAngle, endAngle, angleInc, angleExtents: Integer;
  t, bt, r, rEnd, sn, cs, px, py, cx, cy, rri, skm: Double;
begin
  angleExtents := Round(FPointsPerRevolution / CCorrectAngleCount * 0.5);
  startAngle := angleIdx - angleExtents;
  endAngle := angleIdx + angleExtents;
  angleInc := Max(1, Round(FPointsPerRevolution / (360.0 * CPrecMul)));

  SetLength(corrData, Length(FInputScans), Ceil(CCorrectAreaWidth * FOutputDPI * (endAngle - startAngle + 1) / angleInc));

  for i := 0 to High(FInputScans) do
  begin
    bt := FInputScans[i].GrooveStartAngle;
    cx := FInputScans[i].Center.X;
    cy := FInputScans[i].Center.Y;
    if i > 0 then
    begin
      skm := x[High(FInputScans) * 0 + i - 1];
    end
    else
    begin
      skm := 1.0;
    end;

    pos := 0;
    j := startAngle;
    repeat
      t := AngleToArctanExtents(j * FRadiansPerRevolutionPoint);

      SinCos(t + bt, sn, cs);

      sn *= skm;
      cs *= skm;

      r := CCorrectAreaBegin * 0.5 * FOutputDPI;
      rEnd := CCorrectAreaEnd * 0.5 * FOutputDPI;
      repeat
        rri := r + pos;
        px := cs * rri + cx;
        py := sn * rri + cy;

        Assert(pos < Length(corrData[i]));

        if FInputScans[i].InRangePointD(py, px) then
          corrData[i, pos] := FInputScans[i].GetPointD(py, px, isImage, imLinear);

        Inc(pos);
      until rri >= rEnd;

      j += angleInc;
    until j >= endAngle;

    SetLength(corrData[i], pos);
  end;

  Result := 0;
  for i := 1 to High(FInputScans) do
    Result -= PearsonCorrelation(corrData[0], corrData[i]);
  if Length(FInputScans) > 2 then
    Result /= High(FInputScans);

  //for i := 0 to High(x) do
  //  Write(x[i]:12:6);
  //WriteLn(Result:12:6);
end;

procedure TScanCorrelator.Correct;
var
  x: TVector;
  correls: TDoubleDynArray;


  procedure DoEval(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    angleIdx: PtrInt;
    lx: TVector;
    f: Double;
  begin
    angleIdx := Round(AIndex * FPointsPerRevolution / CCorrectAngleCount);

    lx := Copy(x);
    f := -PowellMinimize(@PowellCorrect, lx, 1e-9, 1e-6, 1e-9, MaxInt, Pointer(angleIdx))[0];

    correls[AIndex] := f;
    FPerAngleX[AIndex] := lx;

    Write(AIndex + 1:6,' / ',Length(FPerAngleX):6,', Correlation: ', f:9:6, #13);
  end;

var
  i: Integer;
begin
  WriteLn('Correct');

  SetLength(FPerAngleX, CCorrectAngleCount);
  SetLength(correls, CCorrectAngleCount);

  if Length(FInputScans) > 0 then
  begin
    SetLength(x, High(FInputScans) * 1);
    for i := 0 to High(x) do
      x[i] := 1.0;
  end
  else
  begin
    SetLength(x, 0);
  end;

  ProcThreadPool.DoParallelLocalProc(@DoEval, 0, high(FPerAngleX));

  WriteLn;
  WriteLn('Worst correlation: ', MinValue(correls):9:6);
end;

procedure TScanCorrelator.Rebuild;
const
  CTauToAngleIdx = CCorrectAngleCount / (2.0 * Pi);
var
  center, rBeg, rEnd, rLmg, rFmg, rLbl: Double;
  maxOutVals: TDoubleDynArray;

  procedure InterpolateX(tau: Double; var x: TVector);
  var
    ci, i, x0, x1, x2, x3, modulo: Integer;
    c, alpha: Double;
    y0, y1, y2, y3: TVector;
  begin
    if Length(FPerAngleX) = 0 then
      Exit;

    c := tau * CTauToAngleIdx;
    ci := Floor(c);
    alpha := c - ci;

    modulo := Length(FPerAngleX);

    x0 := (ci - 1 + modulo) mod modulo;
    x1 := (ci + 0 + modulo) mod modulo;
    x2 := (ci + 1 + modulo) mod modulo;
    x3 := (ci + 2 + modulo) mod modulo;

    y0 := FPerAngleX[x0];
    y1 := FPerAngleX[x1];
    y2 := FPerAngleX[x2];
    y3 := FPerAngleX[x3];

    for i := 0 to High(x) do
      x[i] := herp(y0[i], y1[i], y2[i], y3[i], alpha);
  end;

  procedure DoY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    x: TVector;
    i, ox, cnt: Integer;
    r, sn, cs, px, py, t, cx, cy, acc, bt, skm, maxOutVal, ct: Double;
  begin
    if High(FInputScans) > 0 then
      SetLength(x, High(FInputScans));

    maxOutVal := -Infinity;
    for ox := 0 to High(FOutputImage[0]) do
    begin
      r := Sqrt(Sqr(center - AIndex) + Sqr(center - ox));

      if InRange(r, rBeg, rEnd) then
      begin
        bt := ArcTan2(center - AIndex, center - ox);
        InterpolateX(bt, x);

        cnt := 0;
        acc := 0;
        for i := 0 to High(FInputScans) do
        begin
          t  := FInputScans[i].GrooveStartAngle;
          cx := FInputScans[i].Center.X;
          cy := FInputScans[i].Center.Y;

          ct := AngleToArctanExtents(bt + t);

          if i > 0 then
            skm := x[High(FInputScans) * 0 + i - 1]
          else
            skm := 1.0;

          SinCos(ct, sn, cs);
          px := cs * r * skm + cx;
          py := sn * r * skm + cy;

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
  Analyze;
  Crop;
  Correct;
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


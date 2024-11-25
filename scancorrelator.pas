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
    FPerSnanSkews: array of TPointD;
    FInitF: Double;


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
    procedure Analyze;
    procedure Crop;
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
  CAreaBegin = C45RpmInnerSize;
  CAreaEnd = C45RpmLabelOuterSize;
  CAreaWidth = (CAreaEnd - CAreaBegin) * 0.5;
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
    ti, ri, t, r, rEnd, sn, cs, px, py, cx, cy, rri, skx, sky: Double;
  begin
    t  :=  x[Length(FInputScans) * 0 + AIndex];
    cx :=  x[Length(FInputScans) * 1 + AIndex];
    cy :=  x[Length(FInputScans) * 2 + AIndex];
    skx := x[Length(FInputScans) * 3 + AIndex];
    sky := x[Length(FInputScans) * 4 + AIndex];

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
  i, j, cnt: Integer;
begin
  SetLength(corrData, Length(FInputScans), Ceil(CAreaWidth * CAreaGroovesPerInch) * FPointsPerRevolution);

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
  p: TPointD;
begin
  WriteLn('Analyze');

  SetLength(FPerSnanSkews, Length(FInputScans));
  SetLength(x, Length(FInputScans) * 5);
  for i := 0 to High(FInputScans) do
  begin
    x[Length(FInputScans) * 0 + i] := FInputScans[i].GrooveStartAngle;
    x[Length(FInputScans) * 1 + i] := FInputScans[i].Center.X;
    x[Length(FInputScans) * 2 + i] := FInputScans[i].Center.Y;
    x[Length(FInputScans) * 3 + i] := 1.0;
    x[Length(FInputScans) * 4 + i] := 1.0;
  end;

  FInitF := PowellMinimize(@PowellAnalyze, x, 1e-9, 1e-6, 1e-9, MaxInt, nil)[0];

  for i := 0 to High(FInputScans) do
  begin
    FInputScans[i].GrooveStartAngle := AngleToArctanExtents(x[Length(FInputScans) * 0 + i]);
    p.X := x[Length(FInputScans) * 1 + i];
    p.Y := x[Length(FInputScans) * 2 + i];
    FInputScans[i].Center := p;
    p.X := x[Length(FInputScans) * 3 + i];
    p.Y := x[Length(FInputScans) * 4 + i];
    FPerSnanSkews[i] := p;
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
          t  := FInputScans[i].GrooveStartAngle;
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


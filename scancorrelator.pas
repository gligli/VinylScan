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
    FUseGradientDescent: Boolean;

    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FMaxOutputImageValue: Double;

    FOutputImage: TSingleDynArray2;

    function PowellEvalCorrelation(const arg: TVector; obj: Pointer): TScalar;
    function PowellEvalCorrelationInit(const arg: TVector; obj: Pointer): TScalar;
  public
    constructor Create(const AFileNames: TStrings; AOutputDPI: Integer = 2400);
    destructor Destroy; override;

    procedure LoadPNGs;
    procedure Correlate;
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

  WriteLn('PointsPerRevolution:', FPointsPerRevolution:12);

  SetLength(FOutputImage, Ceil(C45RpmOuterSize * FOutputDPI), Ceil(C45RpmOuterSize * FOutputDPI));
end;

const
  CPPRDiv = 100;
  CPrecMul = 1;
  CInitAreaBegin = C45RpmInnerSize;
  CInitAreaEnd = C45RpmLabelOuterSize;
  CInitAreaWidth = (CInitAreaEnd - CInitAreaBegin) * 0.5;
  CAreaBegin = C45RpmInnerSize;
  CAreaEnd = C45RpmOuterSize;
  CAreaWidth = (CAreaEnd - CAreaBegin) * 0.5;
  CAreaGroovesPerInch = 16;

function TScanCorrelator.PowellEvalCorrelationInit(const arg: TVector; obj: Pointer): TScalar;
var
  corrData: TDoubleDynArray2;

  procedure DoOne(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    pos: Integer;
    ti, ri, t, r, rEnd, sn, cs, px, py, cx, cy, rri: Double;
  begin
    t  := arg[Length(FInputScans) * 0 + AIndex];
    cx := arg[Length(FInputScans) * 1 + AIndex];
    cy := arg[Length(FInputScans) * 2 + AIndex];

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
        corrData[AIndex, pos] := FInputScans[AIndex].GetPointD(FInputScans[AIndex].Image, py, px, imLinear);

      Inc(pos);
    until rri >= rEnd;

    SetLength(corrData[AIndex], pos);
  end;

var
  i, j, cnt: Integer;
begin
  SetLength(corrData, Length(FInputScans), Ceil(CInitAreaWidth * CAreaGroovesPerInch) * FPointsPerRevolution);

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, High(FInputScans));

  cnt := 0;
  Result := 0;
  for i := 0 to High(FInputScans) do
    for j := i + 1 to High(FInputScans) do
    begin
      Result -= PearsonCorrelation(corrData[i], corrData[j]);
      Inc(cnt);
    end;
  if cnt > 0 then
    Result /= cnt;

  Write('correl = ', -Result:9:6,#13);
end;

function TScanCorrelator.PowellEvalCorrelation(const arg: TVector; obj: Pointer): TScalar;
var
  angleIdx: PtrInt absolute obj;
  corrData: TDoubleDynArray2;
  i, j, cnt, pos: Integer;
  ri, t, r, rEnd, sn, cs, px, py, cx, cy, rri, sk: Double;
begin
  SetLength(corrData, Length(FInputScans), Ceil(CAreaWidth * FOutputDPI * CPrecMul));

  ri := 1.0 / CPrecMul;

  for i := 0 to High(FInputScans) do
  begin
    t  := FInputScans[i].GrooveStartAngle + angleIdx * CPPRDiv * FRadiansPerRevolutionPoint;
    cx := FInputScans[i].Center.X;
    cy := FInputScans[i].Center.Y;
    if i > 0 then
      sk := arg[i - 1]
    else
      sk := 1.0;

    SinCos(t, sn, cs);

    sn *= sk;
    cs *= sk;

    r := CAreaBegin * 0.5 * FOutputDPI;
    rEnd := CAreaEnd * 0.5 * FOutputDPI;
    pos := 0;
    repeat
      rri := r + ri * pos;
      px := cs * rri + cx;
      py := sn * rri + cy;

      while pos >= Length(corrData[i]) do
        SetLength(corrData[i], Ceil(Length(corrData[i]) * 1.1));

      if FInputScans[i].InRangePointD(py, px) then
        corrData[i, pos] := FInputScans[i].GetPointD(FInputScans[i].Image, py, px, imLinear);

      Inc(pos);
    until rri >= rEnd;

    SetLength(corrData[i], pos);
  end;

  cnt := 0;
  Result := 0;
  for i := 0 to High(FInputScans) do
    for j := i + 1 to High(FInputScans) do
    begin
      Result -= PearsonCorrelation(corrData[i], corrData[j]);
      Inc(cnt);
    end;
  if cnt > 0 then
    Result /= cnt;

  //if not FUseGradientDescent then
  //begin
  //  for i := 0 to High(arg) do
  //    Write(arg[i]:12:6);
  //  WriteLn(-Result:12:6);
  // end;

end;

procedure TScanCorrelator.Correlate;
var
  t2pa, initF: Double;
  x: TVector;
  perAngleX: TDoubleDynArray2;

  procedure DoOne(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    lx: TVector;
    f: Double;
  begin
    lx := Copy(x);

    f := -PowellMinimize(@PowellEvalCorrelation, lx, 1e-9, 1e-6, 1e-6, MaxInt, Pointer(AIndex))[0];

    perAngleX[AIndex] := nil;
    if f >= initF * 0.95 then
      perAngleX[AIndex] := lx;

    Write(AIndex + 1:6,' / ',Length(perAngleX):6,' ( correl = ', f:9:6, ' )',#13);
  end;

  procedure InterpolateX(tau: Double; var x: TVector);
  var
    ci, i, x0, x1, x2, x3, modulo: Integer;
    c, alpha: Double;
    y0, y1, y2, y3: TVector;
  begin
    if Length(perAngleX) = 0 then
      Exit;

    c := tau * t2pa;
    ci := Floor(c);
    alpha := c - ci;

    modulo := Length(perAngleX);

    x0 := (ci - 1 + modulo) mod modulo;
    x1 := (ci + 0) mod modulo;
    x2 := (ci + 1) mod modulo;
    x3 := (ci + 2) mod modulo;

    y0 := perAngleX[x0];
    y1 := perAngleX[x1];
    y2 := perAngleX[x2];
    y3 := perAngleX[x3];

    for i := 0 to High(x) do
      x[i] := herp(y0[i], y1[i], y2[i], y3[i], alpha);
  end;

var
  i, j, ox, oy, prevI, nextI, prevIRaw, nextIRaw: Integer;
  r, rBeg, rEnd, sn, cs, px, py, center, t, cx, cy, acc, bt, sk: Double;
  cp: TPointD;
begin
  WriteLn('Correlate (Analyze)');

  SetLength(x, Length(FInputScans) * 3);
  SetLength(perAngleX, FPointsPerRevolution div CPPRDiv);

  for i := 0 to High(FInputScans) do
  begin
    x[Length(FInputScans) * 0 + i] := FInputScans[i].GrooveStartAngle;
    x[Length(FInputScans) * 1 + i] := FInputScans[i].Center.X;
    x[Length(FInputScans) * 2 + i] := FInputScans[i].Center.Y;
  end;

  initF := -PowellMinimize(@PowellEvalCorrelationInit, x, 1e-9, 1e-6, 1e-6, MaxInt, nil)[0];

  for i := 0 to High(FInputScans) do
  begin
    FInputScans[i].GrooveStartAngle := x[Length(FInputScans) * 0 + i];
    cp.X := x[Length(FInputScans) * 1 + i];
    cp.Y := x[Length(FInputScans) * 2 + i];
    FInputScans[i].Center := cp;
  end;

  WriteLn;
  WriteLn('Correlate (Correct)');

  if Length(FInputScans) > 0 then
  begin
    SetLength(x, High(FInputScans));
    for i := 0 to High(x) do
      x[i] := 1.0;
  end
  else
  begin
    SetLength(x, 0);
  end;

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, high(perAngleX));

  for i := 0 to High(perAngleX) do
    if not Assigned(perAngleX[i]) then
    begin
      prevIRaw := i;
      prevI := i;
      for j := i downto -High(perAngleX)  do
        if Assigned(perAngleX[(j + Length(perAngleX)) mod Length(perAngleX)]) then
        begin
          prevIRaw := j;
          prevI := (j + Length(perAngleX)) mod Length(perAngleX);
          Break;
        end;

      nextIRaw := i;
      nextI := i;
      for j := i to High(perAngleX) + Length(perAngleX) do
        if Assigned(perAngleX[j mod Length(perAngleX)]) then
        begin
          nextIRaw := j;
          nextI := j mod Length(perAngleX);
          Break;
        end;

      if (prevI <> i) and (nextI <> i) then
      begin
        SetLength(perAngleX[i], Length(perAngleX[prevI]));
        for j := 0 to High(perAngleX[i]) do
          perAngleX[i, j] := lerp(perAngleX[prevI, j], perAngleX[nextI, j], (i - prevIRaw) / (nextIRaw - prevIRaw));
      end
      else
      begin
        perAngleX[i] := Copy(x);
      end;
    end;

  WriteLn;
  WriteLn('Correlate (Rebuild)');

  center := Length(FOutputImage) / 2.0;
  t2pa := (FPointsPerRevolution - 1) / (2.0 * Pi * CPPRDiv);
  FMaxOutputImageValue := 0;

  rBeg := CAreaBegin * 0.5 * FOutputDPI;
  rEnd := CAreaEnd * 0.5 * FOutputDPI;
  for oy := 0 to High(FOutputImage) do
    for ox := 0 to High(FOutputImage[0]) do
    begin
      r := Sqrt(Sqr(center - ox) + Sqr(center - oy));
      bt := ArcTan2(center - oy, center - ox);

      if bt < 0.0 then
        bt += 2.0 * Pi
      else if bt >= 2.0 * Pi then
        bt -= 2.0 * Pi;

      if InRange(r, rBeg, rEnd) then
      begin
        InterpolateX(bt, x);

        acc := 0;
        for i := 0 to High(FInputScans) do
        begin
          t  := FInputScans[i].GrooveStartAngle;
          cx := FInputScans[i].Center.X;
          cy := FInputScans[i].Center.Y;
          if i > 0 then
            sk := x[i - 1]
          else
            sk := 1.0;

          SinCos(t + bt, sn, cs);
          px := cs * r * sk + cx;
          py := sn * r * sk + cy;
          if FInputScans[i].InRangePointD(py, px) then
            acc += FInputScans[i].GetPointD(FInputScans[i].Image, py, px, imHermite);
        end;
        if Length(FInputScans) > 0 then
          acc /= Length(FInputScans);

        FOutputImage[oy, ox] := acc;
        FMaxOutputImageValue := Max(FMaxOutputImageValue, acc);
      end
      else
      begin
        FOutputImage[oy, ox] := 1.0;
      end;
    end;
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
  end;
end;

procedure TScanCorrelator.Run;
begin
  LoadPNGs;
  Correlate;
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


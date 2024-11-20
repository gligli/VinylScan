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
    FDPI: Integer;
    FUseGradientDescent: Boolean;

    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FMaxOutputImageValue: Double;

    FOutputImage: TSingleDynArray2;

    function PowellEvalCorrelation(const arg: TVector; obj: Pointer): TScalar;
    function PowellEvalCorrelationInit(const arg: TVector; obj: Pointer): TScalar;
  public
    constructor Create(const AFileNames: TStrings; ADPI: Integer = 2400);
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
    FScanCorrelator: TScanCorrelator;
    FFactor: Single;
  protected
    procedure SetInternalPixel (x,y:integer; Value:integer); override;
    function GetInternalPixel (x,y:integer) : integer; override;
  public
    constructor Create (AWidth,AHeight:integer); override;

    property ScanCorrelator: TScanCorrelator read FScanCorrelator write FScanCorrelator;
  end;

implementation

{ TScanCorrelator }

constructor TScanCorrelator.Create(const AFileNames: TStrings; ADPI: Integer);
var
  i: Integer;
begin
  FDPI := ADPI;
  FPointsPerRevolution := Ceil(Pi * C45RpmOuterSize * ADPI);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;
  FUseGradientDescent := False;

  SetLength(FInputScans, AFileNames.Count);
  for i := 0 to AFileNames.Count - 1 do
  begin
    FInputScans[i] := TInputScan.Create(FPointsPerRevolution, ADPI, True);
    FInputScans[i].PNGFileName := AFileNames[i];
  end;

  WriteLn('PointsPerRevolution:', FPointsPerRevolution:12);

  SetLength(FOutputImage, Ceil(C45RpmOuterSize * ADPI), Ceil(C45RpmOuterSize * ADPI));
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

begin
  WriteLn('LoadPNGs');

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, high(FInputScans));
end;

const
  CPPRDiv = 10;
  CPrecMul = 10;
  CAreaBegin = C45RpmInnerSize;
  CAreaEnd = C45RpmOuterSize;
  CAreaWidth = (CAreaEnd - CAreaBegin) * 0.5;
  CAreaGroovesPerInch = 100;

function TScanCorrelator.PowellEvalCorrelationInit(const arg: TVector; obj: Pointer): TScalar;
var
  corrData: TDoubleDynArray2;
  ti, ri: Double;

  procedure DoOne(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    pos: Integer;
    t, r, sn, cs, px, py, cx, cy, sk: Double;
  begin
    r := CAreaBegin * 0.5 * FDPI;

    if AIndex > 0  then
    begin
      t := arg[High(FInputScans) * 0 + AIndex - 1];
      cx := arg[High(FInputScans) * 1 + AIndex - 1];
      cy := arg[High(FInputScans) * 2 + AIndex - 1];
      sk := arg[High(FInputScans) * 3 + AIndex - 1];
    end
    else
    begin
      t := FInputScans[0].GrooveStartAngle;
      cx := FInputScans[0].Center.X;
      cy := FInputScans[0].Center.Y;
      sk := 1.0;
    end;

    pos := 0;
    repeat
      SinCos(t, sn, cs);

      px := cs * r * sk + cx;
      py := sn * r * sk + cy;

      while pos >= Length(corrData[AIndex]) do
        SetLength(corrData[AIndex], Ceil(Length(corrData[AIndex]) * 1.1));

      if FInputScans[AIndex].InRangePointD(py, px) then
        corrData[AIndex, pos] := FInputScans[AIndex].GetPointD(FInputScans[AIndex].Image, py, px);

      t += ti;
      r += ri;
      Inc(pos);
    until r >= CAreaEnd * 0.5 * FDPI;

    SetLength(corrData[AIndex], pos);
  end;

var
  i, j, cnt: Integer;
begin
  SetLength(corrData, Length(FInputScans), Ceil(CAreaWidth * CAreaGroovesPerInch * PointsPerRevolution));

  ti := FRadiansPerRevolutionPoint;
  ri := CAreaWidth * FDPI / (PointsPerRevolution * CAreaGroovesPerInch);

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, High(FInputScans));

  cnt := 0;
  Result := 0;
  for i := 0 to High(FInputScans) do
    for j := i + 1 to High(FInputScans) do
    begin
      Result -= PearsonCorrelation(corrData[i], corrData[j]);
      Inc(cnt);
    end;
  Result /= cnt;

  Write('correl = ', -Result:9:6,#13);
end;

function TScanCorrelator.PowellEvalCorrelation(const arg: TVector; obj: Pointer): TScalar;
var
  angleIdx: PtrInt absolute obj;
  corrData: TDoubleDynArray2;
  i, j, cnt, pos: Integer;
  ri, t, r, sn, cs, px, py, cx, cy, sk: Double;
begin
  SetLength(corrData, Length(FInputScans), Ceil(CAreaWidth * FDPI * CPrecMul));

  ri := 1.0 / CPrecMul;

  for i := 0 to High(FInputScans) do
  begin
    if i > 0  then
    begin
      t := arg[High(FInputScans) * 0 + i - 1];
      cx := arg[High(FInputScans) * 1 + i - 1];
      cy := arg[High(FInputScans) * 2 + i - 1];
      sk := arg[High(FInputScans) * 3 + i - 1];
    end
    else
    begin
      t := FInputScans[0].GrooveStartAngle;
      cx := FInputScans[0].Center.X;
      cy := FInputScans[0].Center.Y;
      sk := 1.0;
    end;

    t += angleIdx * FRadiansPerRevolutionPoint * CPPRDiv;

    SinCos(t, sn, cs);
    r := CAreaBegin * 0.5 * FDPI;

    cs *= sk;
    sn *= sk;

    pos := 0;
    repeat
      px := cs * r + cx;
      py := sn * r + cy;

      Assert(pos < Length(corrData[i]));

      if FInputScans[i].InRangePointD(py, px) then
        corrData[i, pos] := FInputScans[i].GetPointD(FInputScans[i].Image, py, px);

      r += ri;
      Inc(pos);
    until r >= CAreaEnd * 0.5 * FDPI;
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
  t2pa: Double;
  x: TVector;
  perAngleX: TDoubleDynArray2;

  procedure DoOne(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    lx: TVector;
    f: Double;
  begin
    lx := Copy(x);

    f := -PowellMinimize(@PowellEvalCorrelation, lx, 1e-6, 1e-6, 1e-6, MaxInt, Pointer(AIndex))[0];

    perAngleX[AIndex] := copy(lx);

    Write(AIndex + 1:6,' / ',FPointsPerRevolution div CPPRDiv:6,' ( correl = ', f:9:6, ' )',#13);
  end;

  function InterpolateX(tau: Double; var x: TVector): TVector;
  var
    ci, i: Integer;
    c, alpha: Double;
    x0, x1, x2, x3: TVector;
  begin
    if tau < 0.0 then
      tau += 2.0 * Pi;

    c := tau * t2pa;
    ci := Floor(c);
    alpha := c - ci;

    x0 := perAngleX[(ci + Length(perAngleX) - 1) mod Length(perAngleX)];
    x1 := perAngleX[ci + 0];
    x2 := perAngleX[(ci + 1) mod Length(perAngleX)];
    x3 := perAngleX[(ci + 2) mod Length(perAngleX)];

    for i := 0 to High(x) do
      x[i] := herp(x0[i], x1[i], x2[i], x3[i], alpha);
  end;

var
  i, ox, oy: Integer;
  r, sn, cs, px, py, center, t, cx, cy, sk, acc, bt: Double;
begin
  WriteLn('Correlate (Analyze)');

  SetLength(x, High(FInputScans) * 4);
  SetLength(perAngleX, FPointsPerRevolution div CPPRDiv);

  for i := 1 to High(FInputScans) do
  begin
    x[High(FInputScans) * 0 + i - 1] := FInputScans[i].GrooveStartAngle;
    x[High(FInputScans) * 1 + i - 1] := FInputScans[i].Center.X;
    x[High(FInputScans) * 2 + i - 1] := FInputScans[i].Center.Y;
    x[High(FInputScans) * 3 + i - 1] := 1.0;
  end;

  PowellMinimize(@PowellEvalCorrelationInit, x, 1e-6, 1e-6, 1e-6, MaxInt, nil);

  WriteLn;
  WriteLn('Correlate (Correct)');

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, high(perAngleX));

  WriteLn;
  WriteLn('Correlate (Rebuild)');

  center := Length(FOutputImage) / 2.0;
  t2pa := (FPointsPerRevolution div CPPRDiv - 1) / (2.0 * Pi);
  FMaxOutputImageValue := 0;

  for oy := 0 to High(FOutputImage) do
    for ox := 0 to High(FOutputImage[0]) do
    begin
      r := Sqrt(Sqr(center - ox) + Sqr(center - oy));
      bt := ArcTan2(center - oy, center - ox);

      InterpolateX(bt, x);

      if InRange(r, C45RpmInnerSize * FDPI * 0.5, C45RpmOuterSize * FDPI * 0.5) then
      begin
        acc := 0;

        i := 0;
        t := FInputScans[i].GrooveStartAngle;
        cx := FInputScans[i].Center.X;
        cy := FInputScans[i].Center.Y;

        SinCos(t + bt, sn, cs);
        px := cs * r + cx;
        py := sn * r + cy;
        if FInputScans[i].InRangePointD(py, px) then
          acc += FInputScans[i].GetPointD(FInputScans[i].Image, py, px);

        for i := 1 to High(FInputScans) do
        begin
          t := x[High(FInputScans) * 0 + i - 1];
          cx := x[High(FInputScans) * 1 + i - 1];
          cy := x[High(FInputScans) * 2 + i - 1];
          sk := x[High(FInputScans) * 3 + i - 1];

          SinCos(t + bt, sn, cs);
          px := cs * r * sk + cx;
          py := sn * r * sk + cy;
          if FInputScans[i].InRangePointD(py, px) then
            acc += FInputScans[i].GetPointD(FInputScans[i].Image, py, px);
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
  if IsNan(FFactor) then
  begin
    FFactor := High(Word);
    if not IsZero(FScanCorrelator.FMaxOutputImageValue) then
      FFactor /= FScanCorrelator.FMaxOutputImageValue;
  end;

  Result := EnsureRange(Round(FScanCorrelator.FOutputImage[y, x] * FFactor), 0, High(Word));
end;

constructor TScanImage.Create(AWidth, AHeight: integer);
begin
  inherited Create(AWidth, AHeight);

  FFactor := NaN;
end;

end.


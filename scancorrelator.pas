unit scancorrelator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, GraphType, IntfGraphics, FPCanvas, FPImage, FPWritePNG, ZStream,
  utils, inputscan, powell, minasa, MTProcs;

type

  { TScanCorrelator }

  TScanCorrelator = class
  private
    FInputScans: array of TInputScan;
    FOutputPNGFileName: String;
    FDPI: Integer;

    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FMaxOutputImageValue: Double;

    FOutputImage: TSingleDynArray2;
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

  SetLength(FInputScans, AFileNames.Count);
  for i := 0 to AFileNames.Count - 1 do
  begin
    FInputScans[i] := TInputScan.Create(FPointsPerRevolution, ADPI);
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
  end;

begin
  WriteLn('LoadPNGs');

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, high(FInputScans));
end;


function PowellEvalCorrelation(const arg: TVector; obj: Pointer): TScalar;
const
  CAreaBegin = C45RpmInnerSize;
  CAreaEnd = C45RpmOuterSize;
  CAreaGroovesPerInch = 16;
  CAreaWidth = (CAreaEnd - CAreaBegin) * 0.5;

var
  Self: TScanCorrelator absolute obj;
  corrData: TDoubleDynArray2;
  ti, ri: Double;

  procedure DoOne(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    pos: Integer;
    t, r, sn, cs, px, py, cx, cy: Double;
  begin
    r := CAreaBegin * 0.5 * Self.FDPI;

    t := arg[AIndex];
    cx := arg[Length(Self.FInputScans) + AIndex];
    cy := arg[Length(Self.FInputScans) * 2 + AIndex];

    pos := 0;
    repeat
      SinCos(t, sn, cs);

      px := cs * r + cx;
      py := sn * r + cy;

      while pos >= Length(corrData[AIndex]) do
        SetLength(corrData[AIndex], Ceil(Length(corrData[AIndex]) * 1.1));

      if InRange(py, 0, Self.FInputScans[AIndex].Height - 1) and InRange(px, 0, Self.FInputScans[AIndex].Width - 1) then
        corrData[AIndex, pos] := Self.FInputScans[AIndex].GetPointD(Self.FInputScans[AIndex].Image, py, px);

      t += ti;
      r += ri;
      Inc(pos);
    until r >= CAreaEnd * 0.5 * Self.FDPI;

    SetLength(corrData[AIndex], pos);
  end;

var
  i, j, cnt: Integer;
begin
  SetLength(corrData, Length(Self.FInputScans), Ceil(CAreaWidth * CAreaGroovesPerInch * Self.PointsPerRevolution));

  ti := Self.FRadiansPerRevolutionPoint;
  ri := CAreaWidth * Self.FDPI / (Self.PointsPerRevolution * CAreaGroovesPerInch);

  ProcThreadPool.DoParallelLocalProc(@DoOne, 0, High(Self.FInputScans));

  cnt := 0;
  Result := 0;
  for i := 0 to High(Self.FInputScans) do
    for j := i + 1 to High(Self.FInputScans) do
    begin
      Result -= PearsonCorrelation(corrData[i], corrData[j]);
      Inc(cnt);
    end;
  Result /= cnt;

  //for i := 0 to High(arg) do
  //  Write(arg[i]:14:6);
  //WriteLn(-Result:14:6);
end;


procedure BFGSEvalCorrelation_(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
const
  CH = 1e-8;
  CFCoeff: array[0 .. 7] of Double = (-1/280, 4/105, -1/5, 4/5, -4/5, 1/5, -4/105, 1/280);
  CXCoeff: array[0 .. 7] of Double = (4, 3, 2, 1, -1, -2, -3, -4);
var
  MTGrads: TDoubleDynArray2;

  procedure DoOne(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    x: TVector;
    ig, ic: Integer;
  begin
    if AIndex = 0 then
      func := PowellEvalCorrelation(arg, obj)
    else
    begin
      DivMod(AIndex - 1, Length(CFCoeff), ig, ic);

      x := Copy(arg);
      x[ig] += CXCoeff[ic] * CH;
      MTGrads[ig, ic] := CFCoeff[ic] * PowellEvalCorrelation(x, obj);
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
    Write(arg[ig]:14:6);
  WriteLn(func:14:6);
end;

procedure TScanCorrelator.Correlate;
var
  ox, oy, i: Integer;
  t, r, sn, cs, px, py, center, acc, it, radiusOuter: Double;
  x, bl, bu: TVector;
  state: MinASAState;
  rep: MinASAReport;
begin
  WriteLn('Correlate');

  SetLength(x, Length(Self.FInputScans) * 3);
  SetLength(bl, length(x));
  SetLength(bu, length(x));

  radiusOuter := Round(C45RpmOuterSize * Self.FDPI * 0.5);

  for i := 0 to High(FInputScans) do
  begin
    x[i] := Self.FInputScans[i].GrooveStartAngle;
    x[Length(FInputScans) + i] := FInputScans[i].Center.X;
    x[Length(FInputScans) * 2 + i] := FInputScans[i].Center.Y;

    bl[i] := 0;
    bl[Length(FInputScans) + i] := radiusOuter;
    bl[Length(FInputScans) * 2 + i] := radiusOuter;

    bu[i] := 2 * Pi;
    bu[Length(FInputScans) + i] := FInputScans[i].Width - radiusOuter;
    bu[Length(FInputScans) * 2 + i] := FInputScans[i].Height - radiusOuter;
  end;

{$if 1}
  PowellMinimize(@PowellEvalCorrelation, x, 1e-6, 1e-6, 1e-6, MaxInt, Self);
{$else}
  MinASACreate(Length(x), x, bl, bu, state);
  MinASASetCond(state, 0, 0, 0, 0);

  while MinASAIteration(state) do
    if State.NeedFG then
    begin
      BFGSEvalCorrelation_(State.X, state.F, state.G, Self);
    end;

  MinASAResults(state, x, rep);
{$ifend}

  center := Length(FOutputImage) / 2.0;
  FMaxOutputImageValue := 0;

  for oy := 0 to High(FOutputImage) do
    for ox := 0 to High(FOutputImage[0]) do
    begin
      r := Sqrt(Sqr(center - ox) + Sqr(center - oy));
      t := ArcTan2(center - oy, center - ox);

      if InRange(r, C45RpmInnerSize * FDPI * 0.5, C45RpmOuterSize * FDPI * 0.5) then
      begin
        acc := 0;
        for i := 0 to High(FInputScans) do
        begin
          it := x[i];

          SinCos(it - t, sn, cs);

          px := cs * r + x[Length(Self.FInputScans) + i];
          py := sn * r + x[Length(Self.FInputScans) * 2 + i];

          if InRange(py, 0, FInputScans[i].Height - 1) and InRange(px, 0, FInputScans[i].Width - 1) then
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


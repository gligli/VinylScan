unit inputscan;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, FPReadPNG, FPReadTiff, FPTiffCmn, FPImage, PNGComn, MTProcs,
  utils, powell;

type
  TCropData = record
    StartAngle, EndAngle: Double;
    StartAngleMirror, EndAngleMirror: Double;
  end;

  { TInputScan }

  TInputScan = class
  private
    FImageFileName: String;
    FDPI: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FSilent: Boolean;
    FSinCosLUT: TSinCosDynArray;

    FCenter: TPointD;
    FConcentricGrooveRadius: Double;
    FFirstGrooveRadius: Double;
    FGrooveStartAngle: Double;
    FGrooveStartPoint: TPointD;

    FRelativeAngle: Double;
    FCropData: TCropData;
    FCenterQuality: Double;
    FObjective: Double;

    FImage: TWordDynArray2;
    FLeveledImage: TWordDynArray2;

    procedure SetRevolutionFromDPI(ADPI: Integer);
    procedure SetRevolutionFromSampleRate(ASampleRate: Integer);
    function GetImageShortName: String;
    function GetHeight: Integer; inline;
    function GetWidth: Integer; inline;

    procedure GradientEvalCenter(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
    procedure GradientEvalConcentricGrooveXY(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
    function PowellEvalConcentricGrooveXY(const x: TVector; obj: Pointer): TScalar;
    function PowellCrop(const x: TVector; obj: Pointer): TScalar;

    procedure FindCenter;
    procedure FindConcentricGroove;
    procedure FindGrooveStart;
  public
    constructor Create(ADefaultDPI: Integer = 2400; ASilent: Boolean = False);
    destructor Destroy; override;

    procedure LoadPNG;
    procedure LoadTIFF;
    procedure BrickwallLimit;
    procedure FindTrack(AForcedSampleRate: Integer = -1);
    procedure Crop;

    function InRangePointD(Y, X: Double): Boolean;
    function GetWorkPointD(Y, X: Double): Double;
    function GetFinalPointD(Y, X: Double): Double;
    procedure GetGradientsD(Y, X: Double; out GY: Double; out GX: Double);

    property ImageFileName: String read FImageFileName write FImageFileName;
    property ImageShortName: String read GetImageShortName;

    property DPI: Integer read FDPI;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;

    property Center: TPointD read FCenter write FCenter;
    property ConcentricGrooveRadius: Double read FConcentricGrooveRadius;
    property FirstGrooveRadius: Double read FFirstGrooveRadius;
    property GrooveStartAngle: Double read FGrooveStartAngle;
    property GrooveStartPoint: TPointD read FGrooveStartPoint;
    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;

    property RelativeAngle: Double read FRelativeAngle write FRelativeAngle;
    property CropData: TCropData read FCropData write FCropData;
    property CenterQuality: Double read FCenterQuality;
    property Objective: Double read FObjective write FObjective;

    property Image: TWordDynArray2 read FImage;
    property LeveledImage: TWordDynArray2 read FLeveledImage;
  end;

  TInputScanDynArray = array of TInputScan;

  { TScanImage }

  TScanImage = class(TFPCustomImage)
  private
    FImage: TWordDynArray2;
  protected
    procedure SetInternalPixel(x,y:integer; Value:integer); override;
    function GetInternalPixel(x,y:integer) : integer; override;
    procedure SetInternalColor (x,y:integer; const Value:TFPColor); override;
  public
    property Image: TWordDynArray2 read FImage write FImage;
  end;

  { TDPIAwareReaderPNG }

  TDPIAwareReaderPNG = class(TFPReaderPNG)
  private
    FDPI: TPoint;
  protected
    procedure HandleChunk; override;
  public
    constructor Create; override;

    property DPI: TPoint read FDPI;
  end;

implementation

const
  CRadiusXOffsets: array[TValueSign] of Double = (-C45RpmLeadOutGrooveWidth, 0, C45RpmLeadOutGrooveWidth);
  CRadiusYFactors: array[TValueSign] of Double = (1, -2, 1);

  CCropAreaGroovesPerInch = 32;

{ TInputScan }

procedure TInputScan.SetRevolutionFromDPI(ADPI: Integer);
begin
  FPointsPerRevolution := Ceil(Pi * C45RpmOuterSize * ADPI);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;
end;

procedure TInputScan.SetRevolutionFromSampleRate(ASampleRate: Integer);
begin
  FPointsPerRevolution := Ceil(ASampleRate / C45RpmRevolutionsPerSecond);
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;
end;

function TInputScan.GetImageShortName: String;
begin
  Result := ChangeFileExt(ExtractFileName(FImageFileName), '');
end;

procedure TInputScan.GradientEvalCenter(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  ix, iy, radiusOuter: Integer;
  x, y, gimgx, gimgy: Double;
begin
  func := 0;
  if Assigned(grad) then
  begin
    grad[0] := 0;
    grad[1] := 0;
  end;

  radiusOuter := Round(C45RpmAdapterSize * FDPI * 0.5);

  for iy := -radiusOuter to radiusOuter do
  begin
    y := iy + arg[1];

    for ix := -radiusOuter to radiusOuter do
    begin
      x := ix + arg[0];

      if (Sqrt(Sqr(ix) + Sqr(iy)) <= radiusOuter) and  InRangePointD(y, x) then
      begin
        func -= GetWorkPointD(y, x);
        if Assigned(grad) then
        begin
          GetGradientsD(y, x, gimgy, gimgx);
          grad[0] -= gimgx;
          grad[1] -= gimgy;
        end;
      end;
    end;
  end;
end;

procedure TInputScan.FindCenter;
var
  x: TVector;
  rOut: Double;
begin
  rOut := C45RpmOuterSize * FDPI * 0.5;

  x := [FCenter.X, FCenter.Y];

  if (x[0] <> rOut) and (x[1] <> rOut) then
    FCenterQuality := -LBFGSMinimize(@GradientEvalCenter, x);

  FCenter.X := x[0];
  FCenter.Y := x[1];
end;

function TInputScan.PowellEvalConcentricGrooveXY(const x: TVector; obj: Pointer): TScalar;
begin
  GradientEvalConcentricGrooveXY(x, Result, nil, obj);
end;

procedure TInputScan.GradientEvalConcentricGrooveXY(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  radiusInner, radiusOuter, halfTrackWidth: Integer;

  function DoRadius(ARadius: Double; AGrad: TVector): Double;
  var
    iLut, iTrack: Integer;
    px, py, sn, cs, radius, r, f, gimgx, gimgy, gx, gy: Double;
    vs: TValueSign;
  begin
    Result := 0.0;
    if Assigned(AGrad) then
      FillQWord(AGrad[0], Length(AGrad), 0);

    for iLut := 0 to High(FSinCosLUT) do
    begin
      cs := FSinCosLUT[iLut].Cos;
      sn := FSinCosLUT[iLut].Sin;

      if InRangePointD(cs * ARadius + arg[0], sn * ARadius + arg[1]) then
      begin
        for vs := Low(TValueSign) to High(TValueSign) do
        begin
          r := ARadius + CRadiusXOffsets[vs] * FDPI;
          f := 0.0;
          gx := 0.0;
          gy := 0.0;

          for iTrack := -halfTrackWidth to halfTrackWidth do
          begin
            radius := r + iTrack;

            px := cs * radius + arg[0];
            py := sn * radius + arg[1];

            f += GetWorkPointD(py, px);

            if Assigned(AGrad) then
            begin
              GetGradientsD(py, px, gimgy, gimgx);
              gx += gimgx;
              gy += gimgy;
            end;
          end;

          Result += f * CRadiusYFactors[vs];
          if Assigned(AGrad) then
          begin
            AGrad[0] += gx * CRadiusYFactors[vs];
            AGrad[1] += gy * CRadiusYFactors[vs];
          end;
        end;
      end
      else
      begin
        Result += 1000.0;
      end;
    end;
  end;

var
  iRadius: Integer;
  f: Double;
begin
  halfTrackWidth := Ceil(C45RpmLeadOutGrooveWidth * 0.5 * FDPI * 0.5);
  radiusInner := Round(C45RpmMinConcentricGroove * FDPI * 0.5);
  radiusOuter := Round(C45RpmMaxConcentricGroove * FDPI * 0.5);

  func := Infinity;
  for iRadius := radiusInner to radiusOuter do
  begin
    f := DoRadius(iRadius, nil);

    if f < func then
    begin
      func := f;
      FConcentricGrooveRadius := iRadius;
    end;
  end;

  DoRadius(FConcentricGrooveRadius, grad);
end;

procedure TInputScan.FindConcentricGroove;
var
  rOut: Double;
  x: TDoubleDynArray;
begin
  BuildSinCosLUT(FPointsPerRevolution, FSinCosLUT);

  rOut := C45RpmOuterSize * FDPI * 0.5;
  x := [FCenter.X, FCenter.Y];

  if (x[0] <> rOut) and (x[1] <> rOut) then
  begin
    LBFGSMinimize(@GradientEvalConcentricGrooveXY, x);
    FCenter.X := x[0];
    FCenter.Y := x[1];
  end;

  FCenterQuality := -PowellEvalConcentricGrooveXY(x, nil); // also needed to get the proper FConcentricGrooveRadius
end;

procedure TInputScan.FindGrooveStart;
var
  i: Integer;
  v, best, sn, cs, bestr, x, y, bestx, besty: Double;
begin
  best := -Infinity;
  bestx := 0;
  besty := 0;
  bestr := 0;
  v := 0;

  for i := 0 to FPointsPerRevolution - 1  do
  begin
    SinCos(i * FRadiansPerRevolutionPoint, sn, cs);

    x := cs * FFirstGrooveRadius + FCenter.X;
    y := sn * FFirstGrooveRadius + FCenter.Y;

    if InRangePointD(y, x) then
    begin
      v := v * 0.99 + GetWorkPointD(y, x) * 0.01;

      if v > best then
      begin
        best := v;
        bestx := x;
        besty := y;
        bestr := i * FRadiansPerRevolutionPoint;
      end;
    end;

    //writeln(i:6,x:8,y:8,v:9:3,best:9:3,bestx:8,besty:8);
  end;

  FGrooveStartAngle := bestr;
  FGrooveStartPoint.X := bestx;
  FGrooveStartPoint.Y := besty;
end;

function TInputScan.GetHeight: Integer;
begin
  Result := Length(FImage);
end;

procedure TInputScan.GetGradientsD(Y, X: Double; out GY: Double; out GX: Double);
const
  COperator = idoScharr;
var
  ix, iy: Integer;
  lGY00, lGY01, lGY10, lGY11, lGX00, lGX01, lGX10, lGX11: Integer;
begin
  ix := trunc(X);
  iy := trunc(Y);

  lGX00 := Convolve(FLeveledImage, CImageDerivationKernels[COperator, False], iy + 0, ix + 0);
  lGX01 := Convolve(FLeveledImage, CImageDerivationKernels[COperator, False], iy + 0, ix + 1);
  lGX10 := Convolve(FLeveledImage, CImageDerivationKernels[COperator, False], iy + 1, ix + 0);
  lGX11 := Convolve(FLeveledImage, CImageDerivationKernels[COperator, False], iy + 1, ix + 1);
  lGY00 := Convolve(FLeveledImage, CImageDerivationKernels[COperator, True], iy + 0, ix + 0);
  lGY01 := Convolve(FLeveledImage, CImageDerivationKernels[COperator, True], iy + 0, ix + 1);
  lGY10 := Convolve(FLeveledImage, CImageDerivationKernels[COperator, True], iy + 1, ix + 0);
  lGY11 := Convolve(FLeveledImage, CImageDerivationKernels[COperator, True], iy + 1, ix + 1);

  GX := lerp(lerp(lGX00, lGX01, X - ix), lerp(lGX10, lGX11, X - ix), Y - iy) * (1.0 / High(Word));
  GY := lerp(lerp(lGY00, lGY01, X - ix), lerp(lGY10, lGY11, X - ix), Y - iy) * (1.0 / High(Word));
end;

function TInputScan.GetWidth: Integer;
begin
  Result := Length(FImage[0]);
end;

constructor TInputScan.Create(ADefaultDPI: Integer; ASilent: Boolean);
begin
  FDPI := ADefaultDPI;
  FSilent := ASilent;
  FCenterQuality := NaN;
  FObjective := NaN;
  SetRevolutionFromDPI(FDPI);
end;

destructor TInputScan.Destroy;
begin
  inherited Destroy;
end;

procedure TInputScan.LoadPNG;
var
  fs: TFileStream;
  png: TDPIAwareReaderPNG;
  img: TScanImage;
  sz: TPoint;
begin
  if not FSilent then WriteLn('LoadPNG ', FImageFileName);

  fs := TFileStream.Create(FImageFileName, fmOpenRead or fmShareDenyNone);
  png := TDPIAwareReaderPNG.Create;
  try
    sz := png.ImageSize(fs);

    img := TScanImage.Create(sz.X, sz.Y);
    try
      SetLength(FImage, sz.Y, sz.X);
      img.Image := FImage;

      if not FSilent then WriteLn('Size:', Width:6, 'x', Height:6);

      png.ImageRead(fs, img);

      if (png.DPI.X > 0) and (png.DPI.X = png.DPI.Y) then
      begin
        FDPI := png.DPI.X;
        if not FSilent then   WriteLn('DPI:', FDPI:6);
      end;
    finally
      img.Free;
    end;

    FLeveledImage := FImage;
    FCenter.X := sz.X * 0.5;
    FCenter.Y := sz.Y * 0.5;
  finally
    png.Free;
    fs.Free;
  end;
end;

procedure TInputScan.LoadTIFF;
var
  fs: TFileStream;
  tiff: TFPReaderTiff;
  img: TScanImage;
  szX, szY: DWORD;
  dpiX, dpiY: Double;
begin
  if not FSilent then WriteLn('LoadTIFF ', FImageFileName);

  fs := TFileStream.Create(FImageFileName, fmOpenRead or fmShareDenyNone);
  tiff := TFPReaderTiff.Create;
  try
    if not GetTIFFSize(fs, szX, szY, dpiX, dpiY) then
    begin
      szX := 0;
      szY := 0;
      dpiX := 0;
      dpiY := 0;
    end;

    fs.Seek(soFromBeginning, 0);

    img := TScanImage.Create(szX, szY);
    try
      SetLength(FImage, szY, szX);
      img.Image := FImage;

      if not FSilent then WriteLn('Size:', Width:6, 'x', Height:6);

      tiff.ImageRead(fs, img);

      if (dpiX > 0) and (dpiX = dpiY) then
      begin
        FDPI := Round(dpiX);
        if not FSilent then   WriteLn('DPI:', FDPI:6);
      end;
    finally
      img.Free;
    end;

    FLeveledImage := FImage;
    FCenter.X := szX * 0.5;
    FCenter.Y := szY * 0.5;
  finally
    tiff.Free;
    fs.Free;
  end;
end;

procedure TInputScan.BrickwallLimit;
type
  TSample = record
    OffsetX, OffsetY, ReverseRadius: Integer;
  end;

const
  CGrooveWidthMul = 2;
  CSigma = 4;
var
  offsets: array of TSample;
  radius, meanDiv: Integer;
  stdDevDiv: UInt64;

  procedure GetL2Extents(ay, ax: Integer; buf: TIntegerDynArray; out amean, astddev: Integer);
  var
    i: Integer;
    px, mn, sd: Integer;
    sdAcc: UInt64;
    opt: TSample;
  begin
    mn := 0;
    for i := 0 to High(buf) do
    begin
      opt := offsets[i];
      px := FImage[ay + opt.OffsetY, ax + opt.OffsetX] * opt.ReverseRadius;
      buf[i] := px;
      mn += px;
    end;
    mn := mn div meanDiv;

    sdAcc := 0;
    for i := 0 to High(buf) do
    begin
      px := buf[i];
      px -= mn;
      sdAcc += px * px;
    end;
    sd := round(Sqrt(sdAcc div stdDevDiv));

    amean := mn;
    astddev := CSigma * sd;
  end;

  procedure DoY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    px, x, y: Integer;
    mn, sd: Integer;
    buf: TIntegerDynArray;
  begin
    if not InRange(AIndex, radius, High(FImage) - radius) then
      Exit;

    SetLength(buf, Length(offsets));

    y := AIndex;

    for x := radius to High(FImage[y]) - radius do
    begin
      px := FImage[y, x];

      GetL2Extents(y, x, buf, mn, sd);
      px := (px - mn) * (High(Word) + 1) div sd + mn;
      px := EnsureRange(px, 0, High(word));

      FLeveledImage[y, x] := px;
    end;
  end;

var
  x, y, r, pos, acc: Integer;
begin
  if not FSilent then WriteLn('BrickwallLimit');

  radius := Ceil(C45RpmRecordingGrooveWidth * FDPI * CGrooveWidthMul);

  SetLength(offsets, Sqr(radius * 2 + 1));
  pos := 0;
  acc := 0;
  for y := -radius to radius do
    for x := -radius to radius do
    begin
      r := round(Sqrt(Sqr(y) + Sqr(x)));

      if r <= radius then
      begin
        offsets[pos].OffsetX := x;
        offsets[pos].OffsetY := y;
        offsets[pos].ReverseRadius := radius - r + 1;
        acc += offsets[pos].ReverseRadius;
        Inc(pos);
      end;
    end;
  SetLength(offsets, pos);
  meanDiv := acc;
  stdDevDiv := acc * acc;

  FLeveledImage := nil;
  SetLength(FLeveledImage, Height, Width);

  ProcThreadPool.DoParallelLocalProc(@DoY, radius, High(FImage) - radius);
end;

procedure TInputScan.FindTrack(AForcedSampleRate: Integer);
begin
  if not FSilent then WriteLn('FindTrack');

  if AForcedSampleRate >= 0 then
    SetRevolutionFromSampleRate(AForcedSampleRate)
  else
    SetRevolutionFromDPI(FDPI);

  FFirstGrooveRadius := (C45RpmFirstMusicGroove + C45RpmOuterSize) * 0.5 * FDPI * 0.5;

  FindCenter;
  FindConcentricGroove;
  FindGrooveStart;

  if not FSilent then
  begin
    WriteLn('Center:', FCenter.X:12:3, ',', FCenter.Y:12:3);
    WriteLn('FirstGrooveRadius:', FFirstGrooveRadius:12:3);
    WriteLn('ConcentricGrooveRadius:', FConcentricGrooveRadius:12:3);
    WriteLn('GrooveStartPoint:', FGrooveStartPoint.X:12:3, ',', FGrooveStartPoint.Y:12:3);
  end;
end;

function TInputScan.PowellCrop(const x: TVector; obj: Pointer): TScalar;
var
  rBeg, rEnd, a0a, a1a, a0b, a1b, cx, cy, t, ri, rri, sn, cs, bt, px, py, p: Double;
  iLut, pos, arrPos: Integer;
  stdDevArr: TDoubleDynArray;
  sinCosLUT: TSinCosDynArray;
begin
  Result := 1000.0;

  if not InRange(NormalizeAngle(x[1] - x[0]), DegToRad(0.0), DegToRad(120.0)) then
    Exit;

  a0a := NormalizeAngle(x[0]);
  a0b := NormalizeAngle(x[1]);
  a1a := NormalizeAngle(x[0] + Pi);
  a1b := NormalizeAngle(x[1] + Pi);

  rBeg := C45RpmLastMusicGroove * 0.5 * FDPI;
  rEnd := C45RpmFirstMusicGroove * 0.5 * FDPI;

  t := FRelativeAngle;
  cx := FCenter.X;
  cy := FCenter.Y;

  SetLength(stdDevArr, Ceil((rEnd - rBeg) / FDPI * CCropAreaGroovesPerInch) * FPointsPerRevolution);

  ri := (rEnd - rBeg) / (CCropAreaGroovesPerInch * FPointsPerRevolution);

  BuildSinCosLUT(FPointsPerRevolution, sinCosLUT, t);

  iLut := 0;
  pos := 0;
  arrPos := 0;
  repeat
    bt := sinCosLUT[iLut].Angle;
    cs := sinCosLUT[iLut].Cos;
    sn := sinCosLUT[iLut].Sin;

    rri := rBeg + ri * pos;

    px := cs * rri + cx;
    py := sn * rri + cy;

    Assert(pos < Length(stdDevArr));

    if InRangePointD(py, px) and
        not InNormalizedAngle(bt, a0a, a0b) and not InNormalizedAngle(bt, a1a, a1b) then
    begin
      p := GetWorkPointD(py, px);
      stdDevArr[arrPos] := p;
      Inc(arrPos);
    end;

    Inc(iLut);
    if iLut >= FPointsPerRevolution then
      iLut := 0;

    Inc(pos);
  until rri >= rEnd;

  if arrPos > 0 then
    Result := -StdDev(PDouble(@stdDevArr[0]), arrPos);
end;

procedure TInputScan.Crop;
var
  x: TVector;
begin
  SetLength(x, 2);

  x[0] := NormalizeAngle(DegToRad(-30.0));
  x[1] := NormalizeAngle(DegToRad(30.0));

  PowellMinimize(@PowellCrop, x, 1.0 / 360.0, 1e-6, 1e-6, MaxInt);

  FCropData.StartAngle := NormalizeAngle(x[0]);
  FCropData.EndAngle := NormalizeAngle(x[1]);
  FCropData.StartAngleMirror := NormalizeAngle(x[0] + Pi);
  FCropData.EndAngleMirror := NormalizeAngle(x[1] + Pi);
end;

function TInputScan.InRangePointD(Y, X: Double): Boolean;
begin
  Result := InRange(Y, 5, Height - 7) and InRange(X, 5, Width - 7);
end;

function TInputScan.GetWorkPointD(Y, X: Double): Double;
var
  ix, iy: Integer;
  y1, y2: Double;
begin
  ix := trunc(X);
  iy := trunc(Y);

  y1 := lerp(FLeveledImage[iy + 0, ix + 0], FLeveledImage[iy + 0, ix + 1], X - ix);
  y2 := lerp(FLeveledImage[iy + 1, ix + 0], FLeveledImage[iy + 1, ix + 1], X - ix);

  Result := lerp(y1, y2, Y - iy) * (1.0 / High(Word));
end;

function TInputScan.GetFinalPointD(Y, X: Double): Double;
var
  ix, iy: Integer;
  coeffsX, coeffsY, intData: TSerpCoeffs9;
begin
  ix := trunc(X);
  iy := trunc(Y);

  serpCoeffs(X - ix, coeffsX);
  serpCoeffs(Y - iy, coeffsY);

  serpFromCoeffsXY(coeffsX, FImage, ix, iy, intData);

  Result := serpFromCoeffs(coeffsY, intData) * (1.0 / High(Word));
end;

{ TScanImage }

procedure TScanImage.SetInternalPixel(x, y: integer; Value: integer);
begin
  // not needed
end;

function TScanImage.GetInternalPixel(x, y: integer): integer;
begin
  Result := FImage[y, x];
end;

procedure TScanImage.SetInternalColor(x, y: integer; const Value: TFPColor);
begin
  FImage[y, x] := ToLuma(Value.Red, Value.Green, Value.Blue) div cLumaDiv;
end;

{ TDPIAwareReaderPNG }

procedure TDPIAwareReaderPNG.HandleChunk;
begin
  inherited HandleChunk;

  if (Chunk.aType = ctpHYs) and (Chunk.data^[8] = $01) then
  begin
    FDPI.X := Round(BEtoN(PCardinal(@Chunk.data^[0])^) * 0.0254);
    FDPI.Y := Round(BEtoN(PCardinal(@Chunk.data^[4])^) * 0.0254);
  end;
end;

constructor TDPIAwareReaderPNG.Create;
begin
  inherited create;

  FDPI := Point(-1, -1);
end;

end.


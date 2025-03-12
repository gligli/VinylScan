unit inputscan;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, FPReadPNG, FPReadTiff, FPImage, PNGComn, MTProcs,
  utils, powell;

type
  TInputScan = class;

  TCropData = record
    StartAngle, EndAngle: Double;
    StartAngleMirror, EndAngleMirror: Double;
  end;

  TInputScanDynArray = array of TInputScan;

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
    FSkewY: Double;
    FFirstGrooveRadius: Double;
    FGrooveStartAngle: Double;
    FGrooveStartPoint: TPointD;

    FRelativeAngle: Double;
    FCropData: TCropData;
    FCenterQuality: Double;
    FObjective: Double;

    FWidth, FHeight: Integer;
    FImage: TWordDynArray;
    FLeveledImage: TWordDynArray;

    procedure SetRevolutionFromDPI(ADPI: Integer);
    procedure SetRevolutionFromSampleRate(ASampleRate: Integer);
    function GetImageShortName: String;

    function PowellCrop(const x: TVector; obj: Pointer): TScalar;

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
    function GetPointD_Linear(const Image: TWordDynArray; Y, X: Double): Double;
    function GetPointD_Sinc(const Image: TWordDynArray; Y, X: Double): Single;

    property ImageFileName: String read FImageFileName write FImageFileName;
    property ImageShortName: String read GetImageShortName;

    property DPI: Integer read FDPI;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;

    property Center: TPointD read FCenter write FCenter;
    property ConcentricGrooveRadius: Double read FConcentricGrooveRadius;
    property FirstGrooveRadius: Double read FFirstGrooveRadius;
    property GrooveStartAngle: Double read FGrooveStartAngle;
    property GrooveStartPoint: TPointD read FGrooveStartPoint;
    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;

    property SkewY: Double read FSkewY write FSkewY;
    property RelativeAngle: Double read FRelativeAngle write FRelativeAngle;
    property CropData: TCropData read FCropData write FCropData;
    property CenterQuality: Double read FCenterQuality;
    property Objective: Double read FObjective write FObjective;

    property Image: TWordDynArray read FImage;
    property LeveledImage: TWordDynArray read FLeveledImage;
  end;

  { TScanImage }

  TScanImage = class(TFPCustomImage)
  private
    FImage: TWordDynArray;
  protected
    procedure SetInternalPixel(x,y:integer; Value:integer); override;
    function GetInternalPixel(x,y:integer) : integer; override;
    procedure SetInternalColor (x,y:integer; const Value:TFPColor); override;
  public
    property Image: TWordDynArray read FImage write FImage;
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
  CCropAreaGroovesPerInch = 100;

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

procedure TInputScan.FindConcentricGroove;
const
  CCorneringThres = 1.5 * (High(Byte) + 1);
  CPointsPerRevolution = 512;
  CMinSkew = 980;
  CMaxSkew = 1020;
var
  startBuf: TWordDynArray;
  cornerbuf: TWordDynArray;
  sinCosLUT: TSinCosDynArray;
  stencilX: array[TValueSign] of Double;
  stencilY: array[TValueSign] of Integer;
  extents: TRect;
  results: array[CMinSkew .. CMaxSkew] of record
    Objective: Int64;
    X: TVector;
  end;

  procedure DoSkew(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    cx, cy, k, iLut, px, py, ff: Integer;
    sky, r: Double;
    f: Int64;
    vs: TValueSign;
    pxArr, pyArr: array[TValueSign, 0 .. CPointsPerRevolution div 4 - 1] of Integer;
  begin
    if not InRange(AIndex, CMinSkew, CMaxSkew) then
      Exit;

    results[AIndex].Objective := Low(Int64);
    sky := AIndex / 1000.0;
    for k := round(C45RpmMinConcentricGroove * FDPI * 0.5) to round(C45RpmMaxConcentricGroove * FDPI * 0.5) do
    begin
      r := k;

      for vs := Low(TValueSign) to High(TValueSign) do
        for iLut := 0 to High(sinCosLUT) do
        begin
          pxArr[vs, ilut] := round(sinCosLUT[iLut].Cos * (r + stencilX[vs]));
          pyArr[vs, ilut] := round(sinCosLUT[iLut].Sin * (r + stencilX[vs]) * sky);
        end;

      for cy := extents.Top to extents.Bottom do
        for cx := extents.Left to extents.Right do
        begin
          f := 0;
          for vs := Low(TValueSign) to High(TValueSign) do
          begin
            ff := 0;

            for iLut := 0 to High(sinCosLUT) do
            begin
              px := pxArr[vs, iLut];
              py := pyArr[vs, iLut];

              ff += FLeveledImage[(cy - py) * Width + cx + px];
              ff += FLeveledImage[(cy - py) * Width + cx - px];
              ff += FLeveledImage[(cy + py) * Width + cx + px];
              ff += FLeveledImage[(cy + py) * Width + cx - px];
            end;

            f += ff * stencilY[vs];
          end;

          if f > results[AIndex].Objective then
          begin
            results[AIndex].Objective := f;
            results[AIndex].X := [cx, cy, r, sky];
          end;
        end;
    end;
  end;

  function DoXBuf(var buf: TWordDynArray; y: Integer; corr: Boolean): Double;
  var
    i: Integer;
  begin
    for i := 0 to Width - 1 do
     buf[i] := FImage[y * Width + i];

    Result := NaN;
    if corr then
      Result := MAE(startBuf, cornerbuf);
  end;

  function DoYBuf(var buf: TWordDynArray; x: Integer; corr: Boolean): Double;
  var
    i: Integer;
  begin
    for i := 0 to Height - 1 do
     buf[i] := FImage[i * Width + x];

    Result := NaN;
    if corr then
      Result := MAE(startBuf, cornerbuf);
  end;

var
  iSkew, radiusLimit, maxCorner, xx, yy: Integer;
  bestf: Int64;
  x: TDoubleDynArray;
begin
  // init

  radiusLimit := Round(C45RpmOuterSize * 0.5 * FDPI);

  extents.Left := radiusLimit;
  extents.Top := radiusLimit;
  extents.Right := Width - radiusLimit;
  extents.Bottom := Height - radiusLimit;

  // corner L/T/R/B until the record edges are reached

  if extents.Left <> extents.Right then
  begin
    SetLength(startBuf, Height);
    SetLength(cornerbuf, Height);
    maxCorner := Width - radiusLimit * 2;

    DoYBuf(startBuf, 0, False);
    for xx := 0 to maxCorner do
      if (DoYBuf(cornerbuf, xx, True) > CCorneringThres) or (xx = maxCorner) then
      begin
        extents.Left := xx + radiusLimit;
        Break;
      end;

    DoYBuf(startBuf, Width - 1, False);
    for xx := Width - 1 downto Width - 1 - maxCorner do
      if (DoYBuf(cornerbuf, xx, True) > CCorneringThres) or (xx = Width - 1 - maxCorner) then
      begin
        extents.Right := xx - radiusLimit;
        Break;
      end;
  end;

  if extents.Top <> extents.Bottom then
  begin
    SetLength(startBuf, Width);
    SetLength(cornerbuf, Width);
    maxCorner := Height - radiusLimit * 2;

    DoXBuf(startBuf, 0, False);
    for yy := 0 to maxCorner do
      if (DoXBuf(cornerbuf, yy, True) > CCorneringThres) or (yy = maxCorner) then
      begin
        extents.Top := yy + radiusLimit;
        Break;
      end;

    DoXBuf(startBuf, Height - 1, False);
    for yy := Height - 1 downto Height - 1 - maxCorner do
      if (DoXBuf(cornerbuf, yy, True) > CCorneringThres) or (yy = Height - 1 - maxCorner) then
      begin
        extents.Bottom := yy - radiusLimit;
        Break;
      end;
  end;

  WriteLn(ImageShortName, extents.Left:6,extents.Top:6,extents.Right:6,extents.Bottom:6);

  // grid search algorithm to find the concentric groove

  BuildSinCosLUT(CPointsPerRevolution div 4, sinCosLUT, 0.0, Pi / 2.0);

  stencilY[NegativeValue] := -1;
  stencilY[ZeroValue] := 2;
  stencilY[PositiveValue] := -1;

  stencilX[NegativeValue] := -C45RpmLeadOutGrooveWidth * FDPI;
  stencilX[ZeroValue] := 0;
  stencilX[PositiveValue] := C45RpmLeadOutGrooveWidth * FDPI;

  x := [extents.CenterPoint.X, extents.CenterPoint.Y, C45RpmConcentricGroove * FDPI * 0.5, 1.0];

  ProcThreadPool.DoParallelLocalProc(@DoSkew, CMinSkew, CMaxSkew);

  bestf := Low(Int64);
  for iSkew := CMinSkew to CMaxSkew do
  begin
    if results[iSkew].Objective > bestf then
    begin
      bestf := results[iSkew].Objective;
      x := results[iSkew].X;
    end;
  end;

  FCenter.X := x[0];
  FCenter.Y := x[1];
  FConcentricGrooveRadius := x[2];
  FSkewY := x[3];
  FCenterQuality := bestf;

  WriteLn(ImageShortName, FCenter.X:12:3, FCenter.Y:12:3, FConcentricGrooveRadius:12:3, FSkewY:12:6, FCenterQuality:12:0);
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
      v := v * 0.99 + GetPointD_Linear(FLeveledImage, y, x) * 0.01;

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

constructor TInputScan.Create(ADefaultDPI: Integer; ASilent: Boolean);
begin
  FDPI := ADefaultDPI;
  FSilent := ASilent;
  FCenterQuality := NaN;
  FObjective := NaN;
  FSkewY := 1.0;

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
    FWidth := sz.X;
    FHeight := sz.Y;

    img := TScanImage.Create(FWidth, FHeight);
    try
      SetLength(FImage, FHeight * FWidth);
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
    FCenter.X := FWidth * 0.5;
    FCenter.Y := FHeight * 0.5;
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
    FWidth := szX;
    FHeight := szY;

    fs.Seek(soFromBeginning, 0);

    img := TScanImage.Create(FWidth, FHeight);
    try
      SetLength(FImage, FHeight * FWidth);
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
    FCenter.X := FWidth * 0.5;
    FCenter.Y := FHeight * 0.5;
  finally
    tiff.Free;
    fs.Free;
  end;
end;

procedure TInputScan.BrickwallLimit;
const
  CRadius = 16;
  CSigma = 1;
var
  offsets: TIntegerDynArray;

  procedure GetL2Extents(ayx: Integer; out amean, astddev: Integer);
  var
    i: Integer;
    px, mn: Integer;
    sd: Single;
    sdAcc: UInt64;
  begin
    mn := 0;
    for i := 0 to High(offsets) do
    begin
      px := FImage[ayx + offsets[i]];
      mn += px;
    end;
    mn := mn div Length(offsets);

    sdAcc := 0;
    for i := 0 to High(offsets) do
    begin
      px := FImage[ayx + offsets[i]];
      px -= mn;
      sdAcc += px * px;
    end;
    sd := Sqrt(sdAcc div Length(offsets));

    amean := mn;
    astddev := round(CSigma * sd);
  end;

  procedure DoY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    px, x, y, yx: Integer;
    mn, sd: Integer;
  begin
    if not InRange(AIndex, CRadius, FHeight - 1 - CRadius) then
      Exit;

    y := AIndex;

    for x := CRadius to FWidth - 1 - CRadius do
    begin
      yx := y * Width + x;

      px := FImage[yx];

      GetL2Extents(yx, mn, sd);
      px := (px - mn) * (High(Word) + 1) div (sd + 1) + mn;
      px := EnsureRange(px, 0, High(word));

      FLeveledImage[yx] := px;
    end;
  end;

var
  x, y, r, pos: Integer;
begin
  if not FSilent then WriteLn('BrickwallLimit');

  SetLength(offsets, Sqr(CRadius * 2 + 1));
  pos := 0;
  for y := -CRadius to CRadius do
    for x := -CRadius to CRadius do
    begin
      r := round(Sqrt(Sqr(y) + Sqr(x)));

      if r <= CRadius then
      begin
        offsets[pos] := y * Width + x;
        Inc(pos);
      end;
    end;
  SetLength(offsets, pos);

  FLeveledImage := nil;
  SetLength(FLeveledImage, Height * Width);

  ProcThreadPool.DoParallelLocalProc(@DoY, CRadius, FHeight - 1 - CRadius);
end;

procedure TInputScan.FindTrack(AForcedSampleRate: Integer);
begin
  if not FSilent then WriteLn('FindTrack');

  if AForcedSampleRate >= 0 then
    SetRevolutionFromSampleRate(AForcedSampleRate)
  else
    SetRevolutionFromDPI(FDPI);

  FFirstGrooveRadius := C45RpmFirstMusicGroove * FDPI * 0.5;

  FindConcentricGroove;
  FindGrooveStart;

  if not FSilent then
  begin
    WriteLn('Center:', FCenter.X:12:3, ',', FCenter.Y:12:3);
    WriteLn('SkewY:', FSkewY:12:6);
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
    bt := NormalizeAngle(sinCosLUT[iLut].Angle);
    cs := sinCosLUT[iLut].Cos;
    sn := sinCosLUT[iLut].Sin;

    rri := rBeg + ri * pos;

    px := cs * rri + cx;
    py := sn * rri + cy;

    Assert(pos < Length(stdDevArr));

    if InRangePointD(py, px) and
        not InNormalizedAngle(bt, a0a, a0b) and not InNormalizedAngle(bt, a1a, a1b) then
    begin
      p := GetPointD_Linear(FLeveledImage, py, px);
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

  PowellMinimize(@PowellCrop, x, 1.0, 1e-6, 0.0, MaxInt);

  FCropData.StartAngle := NormalizeAngle(x[0]);
  FCropData.EndAngle := NormalizeAngle(x[1]);
  FCropData.StartAngleMirror := NormalizeAngle(x[0] + Pi);
  FCropData.EndAngleMirror := NormalizeAngle(x[1] + Pi);
end;

function TInputScan.InRangePointD(Y, X: Double): Boolean;
begin
  Result := InRange(Y, 5, Height - 7) and InRange(X, 5, Width - 7);
end;

function TInputScan.GetPointD_Linear(const Image: TWordDynArray; Y, X: Double): Double;
var
  ix, iy, yx: Integer;
  y1, y2: Double;
begin
  ix := trunc(X);
  iy := trunc(Y);

  yx := iy * Width + ix;

  y1 := lerp(Image[yx], Image[yx + 1], X - ix);
  y2 := lerp(Image[yx + Width], Image[yx + Width + 1], X - ix);

  Result := lerp(y1, y2, Y - iy);
end;

function TInputScan.GetPointD_Sinc(const Image: TWordDynArray; Y, X: Double): Single;
var
  ix, iy: Integer;
  intData: TSerpCoeffs9;
  coeffsX, coeffsY: PSingle;
begin
  ix := trunc(X);
  iy := trunc(Y);

  coeffsX := serpCoeffs(X - ix);
  coeffsY := serpCoeffs(Y - iy);

  serpFromCoeffsXY(coeffsX, @Image[iy * Width + ix], Width, @intData[0]);

  Result := serpFromCoeffs(coeffsY, @intData[0]);
end;

{ TScanImage }

procedure TScanImage.SetInternalPixel(x, y: integer; Value: integer);
begin
  // not needed
end;

function TScanImage.GetInternalPixel(x, y: integer): integer;
begin
  Result := FImage[y * Width + x];
end;

procedure TScanImage.SetInternalColor(x, y: integer; const Value: TFPColor);
begin
  FImage[y * Width + x] := ToLuma(Value.Red, Value.Green, Value.Blue) div cLumaDiv;
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


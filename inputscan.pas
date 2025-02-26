unit inputscan;

{$include 'compileroptions.inc'}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, FPReadPNG, FPReadTiff, FPTiffCmn, FPImage, PNGComn, MTProcs,
  utils, powell;

type
  TInterpSource = (isImage, isXGradient, isYGradient);
  TInterpMode = (imPoint, imLinear, imHermite);

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

    FImage: TWordDynArray2;
    FLeveledImage: TWordDynArray2;

    procedure SetRevolutionFromDPI(ADPI: Integer);
    procedure SetRevolutionFromSampleRate(ASampleRate: Integer);
    function GetImageShortName: String;
    function GetHeight: Integer; inline;
    function GetWidth: Integer; inline;

    function PowellEvalConcentricGrooveXY(const x: TVector; obj: Pointer): TScalar;
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

    function InRangePointD(Y, X: Double): Boolean; inline;
    function GetWorkPointD(Y, X: Double): Double; inline;
    function GetFinalPointD(Y, X: Double): Double; inline;
    procedure GetGradientsD(Y, X: Double; out GY: Double; out GX: Double); inline;

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

function TInputScan.PowellEvalConcentricGrooveXY(const x: TVector; obj: Pointer): TScalar;
var
  radiusInner, radiusOuter, trackWidth: Integer;
  results: TDoubleDynArray;

  procedure DoRadius(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iLut, iTrack: Integer;
    px, py, sn, cs, radius, f: Double;
    vs: TValueSign;
  begin
    if not InRange(AIndex, radiusInner, radiusOuter) then
      Exit;

    f := 0;
    for iLut := 0 to High(FSinCosLUT) do
    begin
      cs := FSinCosLUT[iLut].Cos;
      sn := FSinCosLUT[iLut].Sin;

      if InRangePointD(cs * AIndex + x[0], sn * AIndex + x[1]) then
      begin
        for iTrack := -trackWidth to trackWidth do
          for vs := Low(TValueSign) to High(TValueSign) do
          begin
            radius := AIndex + iTrack + CRadiusXOffsets[vs] * FDPI;

            px := cs * radius + x[0];
            py := sn * radius + x[1];

            f += FImage[Trunc(py), Trunc(px)] * CRadiusYFactors[vs]
          end;
      end
      else
      begin
        f += 1000.0;
      end;
    end;

    results[AIndex - radiusInner] := f;
  end;

var
  iRadius: Integer;
  f: Double;
begin
  trackWidth := Floor(C45RpmLeadOutGrooveWidth * FDPI * 0.5);
  radiusInner := Round(C45RpmMinConcentricGroove * FDPI * 0.5);
  radiusOuter := Round(C45RpmMaxConcentricGroove * FDPI * 0.5);

  SetLength(results, radiusOuter - radiusInner + 1);

  ProcThreadPool.DoParallelLocalProc(@DoRadius, radiusInner, radiusOuter);

  Result := Infinity;
  for iRadius := radiusInner to radiusOuter do
  begin
    f := results[iRadius - radiusInner];

    if f < Result then
    begin
      Result := f;
      FConcentricGrooveRadius := iRadius;
    end;
  end;
end;

procedure TInputScan.FindConcentricGroove;
const
  CBaseStdDevLimit = high(Word) + 1;
  CStdDevDecrease = 0.95;
var
  xx, yy, radiusLimit, xMargin, yMargin, maxOffset: Integer;
  stdDevLimit, prevf, bestf, f, bestX, bestY, reduce, offset, cc: Double;
  line, x: TDoubleDynArray;
  extents: TRect;
begin
  BuildSinCosLUT(FPointsPerRevolution, FSinCosLUT);

  xMargin := Width - Round(C45RpmOuterSize * FDPI);
  yMargin := Height - Round(C45RpmOuterSize * FDPI);

  if (xMargin <= 0) and (yMargin <= 0) then
  begin
    FCenterQuality := -PowellEvalConcentricGrooveXY([FCenter.X, FCenter.Y], nil);
    Exit;
  end;

  SetLength(line, Max(Width, Height));
  radiusLimit := Round(C45RpmOuterSize * 0.5 * FDPI) - 1;

  stdDevLimit := CBaseStdDevLimit;
  repeat
    extents.Top := 0;
    extents.Bottom := Height - 1;

    for yy := 0 to yMargin do
    begin
      for xx := 0 to Width - 1 do
        line[xx] := FImage[yy, xx];

      if StdDev(PDouble(@line[0]), Width) > stdDevLimit then
      begin
        extents.Top := yy;
        Break;
      end;
    end;

    for yy := Height - 1 downto Height - 1 - yMargin do
    begin
      for xx := 0 to Width - 1 do
        line[xx] := FImage[yy, xx];

      if StdDev(PDouble(@line[0]), Width) > stdDevLimit then
      begin
        extents.Bottom := yy;
        Break;
      end;
    end;

    extents.Top += radiusLimit;
    extents.Bottom -= radiusLimit;

    stdDevLimit *= CStdDevDecrease;
  until extents.Height >= 0;

  stdDevLimit := CBaseStdDevLimit;
  repeat
    extents.Left := 0;
    extents.Right := Width - 1;

    for xx := 0 to xMargin do
    begin
      for yy := 0 to Height - 1 do
        line[yy] := FImage[yy, xx];

      if StdDev(PDouble(@line[0]), Height) > stdDevLimit then
      begin
        extents.Left := xx;
        Break;
      end;
    end;

    for xx := Width - 1 downto Width - 1 - xMargin do
    begin
      for yy := 0 to Height - 1 do
        line[yy] := FImage[yy, xx];

      if StdDev(PDouble(@line[0]), Height) > stdDevLimit then
      begin
        extents.Right := xx;
        Break;
      end;
    end;

    extents.Left += radiusLimit;
    extents.Right -= radiusLimit;

    stdDevLimit *= CStdDevDecrease;
  until extents.Width >= 0;

  //writeln(ImageShortName, extents.Left:6,extents.Top:6,extents.Right:6,extents.Bottom:6);

  x := [extents.CenterPoint.X, extents.CenterPoint.Y];

  maxOffset := Round((C45RpmMaxConcentricGroove - C45RpmMinConcentricGroove) * FDPI * 0.5);

  bestf := Infinity;
  reduce := 1.0;
  repeat
    prevf := bestf;

    bestX := x[0];
    bestY := x[1];

    bestf := Infinity;
    for xx := extents.Left to extents.Right do
    begin
      offset := (xx - extents.CenterPoint.X) * reduce;
      if abs(offset) >= maxOffset then
        Continue;

      cc := offset + x[0];

      f := PowellEvalConcentricGrooveXY([cc, bestY], nil);

      if f < bestf then
      begin
        bestf := f;
        bestX := cc;
      end;
    end;

    bestf := Infinity;
    for yy := extents.Top to extents.Bottom do
    begin
      offset := (yy - extents.CenterPoint.Y) * reduce;
      if abs(offset) >= maxOffset then
        Continue;

      cc := offset + x[1];

      f := PowellEvalConcentricGrooveXY([bestX, cc], nil);

      if f < bestf then
      begin
        bestf := f;
        bestY := cc;
      end;
    end;

    x := [bestX, bestY];

    //WriteLn(ImageShortName, x[0]:12:3, x[1]:12:3);

    reduce *= cInvPhi;

  until bestf = prevf;

  FCenterQuality := -bestf;
  FCenter.X := x[0];
  FCenter.Y := x[1];
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
  CFiniteDifferencesYFactor: array[-4 .. 4] of Double = (1/280, -4/105, 1/5, -4/5, 0, 4/5, -1/5, 4/105, -1/280);
var
  i, iy, ix: Integer;
  lGY00, lGY01, lGY10, lGY11, lGX00, lGX01, lGX10, lGX11, fdy: Double;
begin
  ix := trunc(X);
  iy := trunc(Y);

  lGX00 := 0.0; lGX01 := 0.0; lGX10 := 0.0; lGX11 := 0.0;
  lGY00 := 0.0; lGY01 := 0.0; lGY10 := 0.0; lGY11 := 0.0;

  for i := Low(CFiniteDifferencesYFactor) to High(CFiniteDifferencesYFactor) do
  begin
    fdy := CFiniteDifferencesYFactor[i];

    lGX00 += FImage[iy    , ix + i    ] * fdy;
    lGX01 += FImage[iy    , ix + i + 1] * fdy;
    lGX10 += FImage[iy + 1, ix + i    ] * fdy;
    lGX11 += FImage[iy + 1, ix + i + 1] * fdy;

    lGY00 += FImage[iy + i    , ix    ] * fdy;
    lGY01 += FImage[iy + i    , ix + 1] * fdy;
    lGY10 += FImage[iy + i + 1, ix    ] * fdy;
    lGY11 += FImage[iy + i + 1, ix + 1] * fdy;
  end;

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
  CSigma = 2;
  CRadius = 16;
var
  offsets: array of TSample;

  procedure GetL2Extents(ay, ax: Integer; out amean, astddev: Integer);
  var
    i: Integer;
    px, mn, sd: Integer;
    sdAcc: Int64;
    opt: ^TSample;
  begin
    mn := 0;
    for i := 0 to High(offsets) do
    begin
      opt := @offsets[i];
      px := FImage[ay + opt^.OffsetY, ax + opt^.OffsetX] * opt^.ReverseRadius;
      mn += px;
    end;
    mn := mn div (Length(offsets) * (CRadius div 2 + 1));

    sdAcc := 0;
    for i := 0 to High(offsets) do
    begin
      opt := @offsets[i];
      px := FImage[ay + opt^.OffsetY, ax + opt^.OffsetX] * opt^.ReverseRadius;
      px -= mn;
      sdAcc += px * px;
    end;
    sd := round(Sqrt(sdAcc div (Length(offsets) * Sqr(CRadius div 2 + 1))));

    amean := mn;
    astddev := CSigma * sd;
  end;

  procedure DoY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    px, x, y: Integer;
    mn, sd: Integer;
  begin
    if not InRange(AIndex, CRadius, Height - 1 - CRadius) then
      Exit;

    y := AIndex;

    for x := CRadius to High(FImage[y]) - CRadius do
    begin
      GetL2Extents(y, x, mn, sd);

      px := FImage[y, x];
      px := (px - mn) * (High(Word) + 1) div sd + mn;

      FLeveledImage[y, x] := EnsureRange(px, 0, High(word));
    end;
  end;

var
  x, y, r,  pos: Integer;
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
        offsets[pos].OffsetX := x;
        offsets[pos].OffsetY := y;
        offsets[pos].ReverseRadius := CRadius - r;
        Inc(pos);
      end;
    end;
  SetLength(offsets, pos);

  FLeveledImage := nil;
  SetLength(FLeveledImage, Height, Width);

  ProcThreadPool.DoParallelLocalProc(@DoY, CRadius, Height - 1 - CRadius);
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
    WriteLn('FirstGrooveRadius:', FFirstGrooveRadius:12:3);
    WriteLn('ConcentricGrooveRadius:', FConcentricGrooveRadius:12:3);
    WriteLn('GrooveStartPoint:', FGrooveStartPoint.X:12:3, ',', FGrooveStartPoint.Y:12:3);
  end;
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


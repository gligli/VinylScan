unit inputscan;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, FPReadPNG, FPImage, PNGComn,
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
    FPNGFileName: String;
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

    procedure SetRevolutionFromDPI(ADPI: Integer);
    procedure SetRevolutionFromSampleRate(ASampleRate: Integer);
    function GetPNGShortName: String;
    function PowellCrop(const x: TVector; obj: Pointer): TScalar;

    function GetHeight: Integer;
    function GetWidth: Integer;

    procedure FindConcentricGroove;
    procedure FindGrooveStart;
  public
    constructor Create(ADefaultDPI: Integer = 2400; ASilent: Boolean = False);
    destructor Destroy; override;

    procedure LoadPNG;
    procedure FindTrack(AForcedSampleRate: Integer = -1);
    procedure Crop;

    function InRangePointD(Y, X: Double): Boolean; inline;
    function GetPointD(Y, X: Double; Source: TInterpSource; Mode: TInterpMode): Double; inline;

    property PNGFileName: String read FPNGFileName write FPNGFileName;
    property PNGShortName: String read GetPNGShortName;

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
  CAreaBegin = 0;
  CAreaEnd = (C45RpmInnerSize + C45RpmAdapterSize) * 0.5;
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
      p := GetPointD(py, px, isImage, imLinear);
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

function TInputScan.GetPNGShortName: String;
begin
  Result := ChangeFileExt(ExtractFileName(FPNGFileName), '');
end;

procedure TInputScan.FindConcentricGroove;
const
  CBaseStdDevLimit = Round(0.1 * (high(Word) + 1));
  CStdDevDecrease = 0.95;
var
  ilut, iradius, rr, xx, yy, radiusInner, radiusOuter, radiusLimit, trackWidth, xMargin, yMargin: Integer;
  f, best, cs, sn, x, y, radius, stdDevLimit: Double;
  vs: TValueSign;
  line: TDoubleDynArray;
  extents: TRect;
begin
  SetLength(line, Max(Width, Height));

  xMargin := Width - Round(C45RpmOuterSize * FDPI) - 1;
  yMargin := Height - Round(C45RpmOuterSize * FDPI) - 1;

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
  until extents.Height > 0;

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
  until extents.Width > 0;

  //writeln(PNGShortName, extents.Left:6,extents.Top:6,extents.Right:6,extents.Bottom:6);

  BuildSinCosLUT(FPointsPerRevolution, FSinCosLUT);

  radiusInner := Round(C45RpmMinConcentricGroove * FDPI * 0.5);
  radiusOuter := Round(C45RpmMaxConcentricGroove * FDPI * 0.5);
  trackWidth := Floor(C45RpmLeadOutGrooveWidth * FDPI * 0.5);

  best := Infinity;
  for yy := extents.Top to extents.Bottom do
  begin
    for xx := extents.Left to extents.Right do
    begin
      for rr := radiusInner to radiusOuter do
      begin
        f := 0;
        for ilut := 0 to High(FSinCosLUT) do
        begin
          cs := FSinCosLUT[ilut].Cos;
          sn := FSinCosLUT[ilut].Sin;

          for iradius := -trackWidth to trackWidth do
            for vs := Low(TValueSign) to High(TValueSign) do
            begin
              radius := rr + iradius + CRadiusXOffsets[vs] * FDPI;

              x := cs * radius + xx;
              y := sn * radius + yy;

              f += GetPointD(y, x, isImage, imPoint) * CRadiusYFactors[vs];
            end;
        end;

        if f < best then
        begin
          best := f;
          FConcentricGrooveRadius := rr;
          FCenter.X := xx;
          FCenter.Y := yy;
          //WriteLn(PNGShortName, xx:6, yy:6, rr:6, best:12:3);
        end;
      end;
    end;
  end;

  FCenterQuality := -best;
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
      v := v * 0.99 + GetPointD(y, x, isImage, imLinear) * 0.01;

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

function TInputScan.GetPointD(Y, X: Double; Source: TInterpSource; Mode: TInterpMode): Double;

  function GetSample(AY, AX: Integer): Double; inline;
  begin
    Result := 0;

    case Source of
      isImage:
      begin
        Result := FImage[AY, AX];
      end;
      isXGradient:
      begin
        Result := FImage[AY, AX - 4] * (1/280);
        Result += FImage[AY, AX - 3] * (-4/105);
        Result += FImage[AY, AX - 2] * (1/5);
        Result += FImage[AY, AX - 1] * (-4/5);
        Result += FImage[AY, AX + 1] * (4/5);
        Result += FImage[AY, AX + 2] * (-1/5);
        Result += FImage[AY, AX + 3] * (4/105);
        Result += FImage[AY, AX + 4] * (-1/280);
      end;
      isYGradient:
      begin
        Result := FImage[AY - 4, AX] * (1/280);
        Result += FImage[AY - 3, AX] * (-4/105);
        Result += FImage[AY - 2, AX] * (1/5);
        Result += FImage[AY - 1, AX] * (-4/5);
        Result += FImage[AY + 1, AX] * (4/5);
        Result += FImage[AY + 2, AX] * (-1/5);
        Result += FImage[AY + 3, AX] * (4/105);
        Result += FImage[AY + 4, AX] * (-1/280);
      end;
    end;
  end;

var
  ix, iy: Integer;
  y0, y1, y2, y3: Double;
begin
  Result := 0;
  ix := trunc(X);
  iy := trunc(Y);

  case Mode of
    imPoint:
    begin
      Result := GetSample(iy, ix);
    end;
    imLinear:
    begin
      y1 := lerp(GetSample(iy + 0, ix + 0), GetSample(iy + 0, ix + 1), X - ix);
      y2 := lerp(GetSample(iy + 1, ix + 0), GetSample(iy + 1, ix + 1), X - ix);

      Result := lerp(y1, y2, Y - iy);
    end;
    imHermite:
    begin
      y0 := herp(GetSample(iy - 1, ix - 1), GetSample(iy - 1, ix + 0), GetSample(iy - 1, ix + 1), GetSample(iy - 1, ix + 2), X - ix);
      y1 := herp(GetSample(iy + 0, ix - 1), GetSample(iy + 0, ix + 0), GetSample(iy + 0, ix + 1), GetSample(iy + 0, ix + 2), X - ix);
      y2 := herp(GetSample(iy + 1, ix - 1), GetSample(iy + 1, ix + 0), GetSample(iy + 1, ix + 1), GetSample(iy + 1, ix + 2), X - ix);
      y3 := herp(GetSample(iy + 2, ix - 1), GetSample(iy + 2, ix + 0), GetSample(iy + 2, ix + 1), GetSample(iy + 2, ix + 2), X - ix);

      Result := herp(y0, y1, y2, y3, Y - iy);
    end;
  end;

  Result *= (1.0 / High(Word));
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
  if not FSilent then WriteLn('LoadPNG ', FPNGFileName);

  fs := TFileStream.Create(FPNGFileName, fmOpenRead or fmShareDenyNone);
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

    FCenter.X := sz.X * 0.5;
    FCenter.Y := sz.Y * 0.5;
  finally
    png.Free;
    fs.Free;
  end;
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


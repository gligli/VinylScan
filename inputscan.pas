unit inputscan;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, FPReadPNG, FPImage, PNGComn,
  utils, powell;

type
  TInterpSource = (isImage, isXGradient, isYGradient);

  { TInputScan }

  TInputScan = class
  private
    FPNGFileName: String;
    FDPI: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;
    FSilent: Boolean;
    FSinCosLUT: TPointDDynArray;

    FCenter: TPointD;
    FConcentricGrooveRadius: Double;
    FFirstGrooveRadius: Double;
    FGrooveStartAngle: Double;
    FGrooveStartPoint: TPointD;

    FCenterQuality: Double;

    FImage: TWordDynArray2;

    function GetPNGShortName: String;
    procedure GradientEvalConcentricGroove(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
    procedure GradientEvalCenter(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);

    function GetHeight: Integer;
    function GetWidth: Integer;

    procedure FindCenter;
    procedure FindConcentricGroove;
    procedure FindGrooveStart;
  public
    constructor Create(APointsPerRevolution: Integer = 36000; ADefaultDPI: Integer = 2400; ASilent: Boolean = False);
    destructor Destroy; override;

    procedure LoadPNG;
    procedure FindTrack;

    function InRangePointD(Y, X: Double): Boolean; inline;
    function GetPointD(Y, X: Double; Source: TInterpSource): Double; inline;

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
  CRadiusXOffsets: array[TValueSign] of Double = (-C45RpmMaxGrooveWidth * 4, 0, C45RpmMaxGrooveWidth * 4);
  CRadiusYFactors: array[TValueSign] of Double = (1, -2, 1);

{ TInputScan }

procedure TInputScan.GradientEvalCenter(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  ix, iy, radiusOuter: Integer;
  x, y: Double;
begin
  func := 0;
  if Assigned(grad) then
  begin
    grad[0] := 0;
    grad[1] := 0;
  end;

  radiusOuter := Round(CAreaEnd * FDPI * 0.5);

  for iy := -radiusOuter to radiusOuter do
  begin
    y := iy + arg[1];

    for ix := -radiusOuter to radiusOuter do
    begin
      x := ix + arg[0];

      if (Sqrt(Sqr(ix) + Sqr(iy)) <= radiusOuter) and  InRangePointD(y, x) then
      begin
        func -= GetPointD(y, x, isImage);
        if Assigned(grad) then
        begin
          grad[0] -= GetPointD(y, x, isXGradient);
          grad[1] -= GetPointD(y, x, isYGradient);
        end;
      end;
    end;
  end;

  //WriteLn(arg[0]:20:9,arg[1]:20:9,func:20:9);
end;

procedure TInputScan.FindCenter;
var
  x: TVector;
  rOut: Double;
begin
  rOut := C45RpmOuterSize * FDPI * 0.5;

  SetLength(x, 2);
  x[0] := FCenter.X;
  x[1] := FCenter.Y;

  if (x[0] <> rOut) and (x[1] <> rOut) then
    FCenterQuality := -BFGSMinimize(@GradientEvalCenter, x, 1e-6);

  FCenter.X := x[0];
  FCenter.Y := x[1];
end;

procedure TInputScan.GradientEvalConcentricGroove(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  iradius, ilut, trackWidth: Integer;
  radius, sn, cs, x, y, gimgx, gimgy: Double;
  vs: TValueSign;
begin
  func := 0.0;
  if Assigned(grad) then
  begin
    grad[0] := 0.0;
    grad[1] := 0.0;
    grad[2] := 0.0;
  end;

  trackWidth := Floor(C45RpmMaxGrooveWidth * FDPI * 0.5);

  for ilut := 0 to FPointsPerRevolution - 1  do
  begin
    cs := FSinCosLUT[ilut].X;
    sn := FSinCosLUT[ilut].Y;

    for iradius := -trackWidth to trackWidth do
      for vs := Low(TValueSign) to High(TValueSign) do
      begin
        radius := arg[0] + iradius + CRadiusXOffsets[vs] * FDPI;

        x := cs * radius + arg[1];
        y := sn * radius + arg[2];

        if InRangePointD(y, x) then
        begin
          func += GetPointD(y, x, isImage) * CRadiusYFactors[vs];

          if Assigned(grad) then
          begin
            gimgx := GetPointD(y, x, isXGradient) * CRadiusYFactors[vs];
            gimgy := GetPointD(y, x, isYGradient) * CRadiusYFactors[vs];

            grad[0] += (gimgx * cs + gimgy * sn);
            grad[1] += gimgx;
            grad[2] += gimgy;
          end;
        end;
      end;
  end;

  //WriteLn(arg[0]:12:3,arg[1]:12:3,arg[2]:12:3,func:12:3);
end;

function TInputScan.GetPNGShortName: String;
begin
  Result := ChangeFileExt(ExtractFileName(FPNGFileName), '');
end;

procedure TInputScan.FindConcentricGroove;
var
  r, radiusInner, radiusOuter, radiusLimit: Integer;
  f, best, bestr: Double;
  xrc: TVector;
begin
  BuildSinCosLUT(FPointsPerRevolution, FSinCosLUT);

  SetLength(xrc, 3);

  radiusLimit := Round(C45RpmFirstMusicGroove * 0.5 * FDPI);
  radiusInner := Round((C45RpmConcentricGroove + C45RpmLabelOuterSize) * 0.5 * FDPI * 0.5);
  radiusOuter := Round((C45RpmConcentricGroove + C45RpmLastMusicGroove) * 0.5 * FDPI * 0.5);

  xrc[1] := FCenter.X;
  xrc[2] := FCenter.Y;

  best := Infinity;
  bestr := 0;
  for r := radiusInner to radiusOuter do
  begin
    xrc[0] := r;
    GradientEvalConcentricGroove(xrc, f, nil, nil);

    if f < best then
    begin
      best := f;
      bestr := xrc[0];
    end;
  end;

  xrc[0] := bestr;
  FCenterQuality := -ASAMinimize(@GradientEvalConcentricGroove, xrc, [radiusInner, radiusLimit, radiusLimit], [radiusOuter, Width - radiusLimit, Height - radiusLimit]);
  FConcentricGrooveRadius := xrc[0];
  FCenter.X := xrc[1];
  FCenter.Y := xrc[2];
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
      v := v * 0.99 + GetPointD(y, x, isImage) * 0.01;

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

function TInputScan.GetPointD(Y, X: Double; Source: TInterpSource): Double;

  function GetPt(AY, AX: Single): Single; inline;
  var
    ix, iy: Integer;
    y0, y1, y2, y3: Single;
  begin
    ix := trunc(AX);
    iy := trunc(AY);

    y0 := herp(FImage[iy - 1, ix - 1], FImage[iy - 1, ix + 0], FImage[iy - 1, ix + 1], FImage[iy - 1, ix + 2], AX - ix);
    y1 := herp(FImage[iy + 0, ix - 1], FImage[iy + 0, ix + 0], FImage[iy + 0, ix + 1], FImage[iy + 0, ix + 2], AX - ix);
    y2 := herp(FImage[iy + 1, ix - 1], FImage[iy + 1, ix + 0], FImage[iy + 1, ix + 1], FImage[iy + 1, ix + 2], AX - ix);
    y3 := herp(FImage[iy + 2, ix - 1], FImage[iy + 2, ix + 0], FImage[iy + 2, ix + 1], FImage[iy + 2, ix + 2], AX - ix);

    Result := herp(y0, y1, y2, y3, AY - iy);
  end;

const
  CH = 1.0;
begin
  Result := 0;

  case Source of
    isImage:
    begin
      Result := GetPt(Y, X);
    end;
    isXGradient:
    begin
      Result := GetPt(Y, X - 3.0 * CH) * (1 / CH) * (-1/60);
      Result += GetPt(Y, X - 2.0 * CH) * (1 / CH) * (3/20);
      Result += GetPt(Y, X - 1.0 * CH) * (1 / CH) * (-3/4);
      Result += GetPt(Y, X + 1.0 * CH) * (1 / CH) * (3/4);
      Result += GetPt(Y, X + 2.0 * CH) * (1 / CH) * (-3/20);
      Result += GetPt(Y, X + 3.0 * CH) * (1 / CH) * (1/60);
    end;
    isYGradient:
    begin
      Result := GetPt(Y - 3.0 * CH, X) * (1 / CH) * (-1/60);
      Result += GetPt(Y - 2.0 * CH, X) * (1 / CH) * (3/20);
      Result += GetPt(Y - 1.0 * CH, X) * (1 / CH) * (-3/4);
      Result += GetPt(Y + 1.0 * CH, X) * (1 / CH) * (3/4);
      Result += GetPt(Y + 2.0 * CH, X) * (1 / CH) * (-3/20);
      Result += GetPt(Y + 3.0 * CH, X) * (1 / CH) * (1/60);
    end;
  end;

  Result *= (1.0 / High(Word));
end;

function TInputScan.GetWidth: Integer;
begin
  Result := Length(FImage[0]);
end;

constructor TInputScan.Create(APointsPerRevolution: Integer; ADefaultDPI: Integer; ASilent: Boolean);
begin
  FDPI := ADefaultDPI;
  FPointsPerRevolution := APointsPerRevolution;
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;
  FSilent := ASilent;
  FCenterQuality := NaN;
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

procedure TInputScan.FindTrack;
begin
  if not FSilent then WriteLn('FindTrack');

  FFirstGrooveRadius := C45RpmFirstMusicGroove * FDPI * 0.5;

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

function TInputScan.InRangePointD(Y, X: Double): Boolean;
begin
  Result := InRange(Y, 4, Height - 6) and InRange(X, 4, Width - 6);
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


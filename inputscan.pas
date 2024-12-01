unit inputscan;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics, FPReadPNG, FPImage, PNGComn, bufstream,
  utils, minlbfgs, powell;

type
  TInterpMode = (imPoint, imLinear, imHermite);
  TInterpSource = (isImage, isXGradient, isYGradient);

  { TInputScan }

  TInputScan = class
  private
    FImageDerivationOperator: TImageDerivationOperator;
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

    FImage: TWordDynArray2;

    function GetHeight: Integer;
    function GetWidth: Integer;

    procedure FindCenter;
    procedure FindConcentricGroove;
    procedure FindGrooveStart;
  public
    constructor Create(APointsPerRevolution: Integer = 36000; ADPI: Integer = 2400; ASilent: Boolean = False);
    destructor Destroy; override;

    procedure LoadPNG;
    procedure FindTrack;

    procedure Run;

    function InRangePointD(Y, X: Double): Boolean;
    function GetPointD(Y, X: Double; Source: TInterpSource; Mode: TInterpMode): Double; inline;

    property PNGFileName: String read FPNGFileName write FPNGFileName;
    property ImageDerivationOperator: TImageDerivationOperator read FImageDerivationOperator write FImageDerivationOperator;

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

    property Image: TWordDynArray2 read FImage;
  end;

  TInputScanDynArray = array of TInputScan;

  { TDPIAwareReaderPNG }

  TDPIAwareReaderPNG = class(TFPReaderPNG)
  private
    FDPI: TPoint;
  protected
    procedure HandleChunk; override;
  public
    constructor create; override;

    property DPI: TPoint read FDPI;
  end;


  { TDPIAawarePortableNetworkGraphic }

  TDPIAawarePortableNetworkGraphic = class(TPortableNetworkGraphic)
  private
    FDPI: TPoint;
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    procedure FinalizeReader(AReader: TFPCustomImageReader); override;
  public
    property DPI: TPoint read FDPI;
  end;


implementation

const
  CAreaBegin = 0;
  CAreaEnd = C45RpmAdapterSize;
  CAreaGroovesPerInch = 800;
  CRevolutionPointCount = 360;

{ TInputScan }

procedure BFGSEvalCenter(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
var
  Self: TInputScan absolute obj;
  t, pos: Integer;
  r, xx, yy, ri, radiusInner, radiusOuter: Double;
begin
  func := 0;
  grad[0] := 0;
  grad[1] := 0;

  radiusInner := CAreaBegin * Self.FDPI * 0.5;
  radiusOuter := CAreaEnd * Self.FDPI * 0.5;
  ri := Self.FDPI / CAreaGroovesPerInch;
  pos := 0;
  repeat
    r := radiusInner + ri * pos;

    for t := 0 to CRevolutionPointCount - 1  do
    begin
      xx := Self.FSinCosLUT[t].X * r + arg[0];
      yy := min(Self.FSinCosLUT[t].Y * r, 0.6 * radiusOuter) + arg[1];

      if Self.InRangePointD(yy, xx) then
      begin
        func -= Self.GetPointD(yy, xx, isImage, imLinear);
        grad[0] -= Self.GetPointD(yy, xx, isXGradient, imLinear);
        grad[1] -= Self.GetPointD(yy, xx, isYGradient, imLinear);
      end;
    end;

    Inc(pos);
  until r >= radiusOuter;

  //WriteLn(arg[0]:12:3,arg[1]:12:3,func:20:3);
end;

procedure TInputScan.FindCenter;
var
  x: TVector;
  state: MinLBFGSState;
  rep: MinLBFGSReport;
begin
  BuildSinCosLUT(CRevolutionPointCount, FSinCosLUT);

  FCenter.X := Length(FImage[0]) * 0.5;
  FCenter.Y := Length(FImage) * 0.5;

  SetLength(x, 2);
  x[0] := FCenter.X;
  x[1] := FCenter.Y;

  MinLBFGSCreate(Length(x), 2, x, state);
  MinLBFGSSetCond(state, 0, 1e-12, 0, 0);
  while MinLBFGSIteration(state) do
    if State.NeedFG then
      BFGSEvalCenter(State.X, state.F, state.G, Self);
  MinLBFGSResults(state, x, rep);

  FCenter.X := x[0];
  FCenter.Y := x[1];
end;

function EvalConcentricGroove(const x: TVector; Data: Pointer): TScalar;
const
  CRevolutionPointCount = 1080;
  CRadiansPerPoint = Pi * 2.0 / CRevolutionPointCount;
var
  Self: TInputScan absolute Data;
  i: Integer;
  sn, cs, xx, yy: Double;
begin
  Result := 0;
  if Self.InRangePointD(Self.Center.Y + x[0], Self.Center.X + x[0]) and Self.InRangePointD(Self.Center.Y - x[0], Self.Center.X - x[0]) then
    for i := 0 to CRevolutionPointCount - 1  do
    begin
      SinCos(i * CRadiansPerPoint, sn, cs);

      xx := cs * x[0] + Self.Center.X;
      yy := sn * x[0] + Self.Center.Y;

      Result -= Self.GetPointD(yy, xx, isImage, imLinear);
    end;

  //WriteLn(x[0]:12:3,Result:20:3);
end;

procedure TInputScan.FindConcentricGroove;
var
  r, radiusInner, radiusOuter: Integer;
  f, best, bestr, bestrm: Double;
  x: TVector;
begin
  SetLength(x, 1);

  radiusInner := Round((C45RpmConcentricGroove + C45RpmLabelOuterSize) * 0.5 * Self.FDPI * 0.5);
  radiusOuter := Round((C45RpmConcentricGroove + C45RpmLastMusicGroove) * 0.5 * Self.FDPI * 0.5);

  best := Infinity;
  bestr := 0;
  for r := radiusInner to radiusOuter do
  begin
    x[0] := r;
    f := EvalConcentricGroove(x, Self);

    if f < best then
    begin
      best := f;
      bestr := x[0];
    end;
  end;

  best := Infinity;
  bestrm := 0;
  for r := -99 to 99 do
  begin
    x[0] := bestr + r / 100.0;
    f := EvalConcentricGroove(x, Self);

    if f < best then
    begin
      best := f;
      bestrm := x[0];
    end;
  end;

  FConcentricGrooveRadius := bestrm;
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

    x := cs * FFirstGrooveRadius + Center.X;
    y := sn * FFirstGrooveRadius + Center.Y;

    if InRangePointD(y, x) then
    begin
      v := v * 0.99 + GetPointD(y, x, isImage, imHermite) * 0.01;

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

function TInputScan.GetPointD(Y, X: Double; Source: TInterpSource; Mode: TInterpMode): Double; inline;

  function GetSample(AY, AX: Integer): Double; inline;
  begin
    Result := 0;
    case Source of
      isImage: Result := FImage[AY, AX];
      isXGradient: Result := Convolve(FImage, CImageDerivationKernels[FImageDerivationOperator, False], AY, AX);
      isYGradient: Result := Convolve(FImage, CImageDerivationKernels[FImageDerivationOperator, True], AY, AX);
    end;
    Result *= (1.0 / High(Word));
  end;

var
  ix, iy: Integer;
  y0, y1, y2, y3: Double;
begin
  Result := 0;
  ix := trunc(X);
  iy := trunc(Y);

  case mode of
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
end;

function TInputScan.GetWidth: Integer;
begin
  Result := Length(FImage[0]);
end;

constructor TInputScan.Create(APointsPerRevolution: Integer; ADPI: Integer; ASilent: Boolean);
begin
  FDPI := ADPI;
  FPointsPerRevolution := APointsPerRevolution;
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;
  FSilent := ASilent;
  FImageDerivationOperator := idoSobel;
end;

destructor TInputScan.Destroy;
begin
  inherited Destroy;
end;

procedure TInputScan.LoadPNG;
var
  fs: TBufferedFileStream;
  png: TDPIAawarePortableNetworkGraphic;
  x, y: Integer;
  p: PByte;
begin
  if not FSilent then WriteLn('LoadPNG ', FPNGFileName);

  fs := TBufferedFileStream.Create(FPNGFileName, fmOpenRead or fmShareDenyNone);
  png := TDPIAawarePortableNetworkGraphic.Create;
  try
    png.LoadFromStream(fs);

    SetLength(FImage, png.Height, png.Width);

    if not FSilent then WriteLn(png.Width:6, 'x', png.Height:6);

    case png.RawImage.Description.BitsPerPixel of
      8:
        for y := 0 to High(FImage) do
        begin
          p := png.RawImage.Data;
          inc(p, y * png.RawImage.Description.BytesPerLine);
          for x := 0 to High(FImage[0]) do
          begin
            FImage[y, x] := Round(p^ * (High(Word) / High(Byte)));
            Inc(p, 1);
          end;
        end;
      16:
        for y := 0 to High(FImage) do
        begin
          p := png.RawImage.Data;
          inc(p, y * png.RawImage.Description.BytesPerLine);
          for x := 0 to High(FImage[0]) do
          begin
            FImage[y, x] := PWord(p)^;
            Inc(p, 2);
          end;
        end;
      24:
        for y := 0 to High(FImage) do
        begin
          p := png.RawImage.Data;
          inc(p, y * png.RawImage.Description.BytesPerLine);
          for x := 0 to High(FImage[0]) do
          begin
            FImage[y, x] := Round(ToLuma(p[0], p[1], p[2]) * (High(Word) / (cLumaDiv * High(Byte))));
            Inc(p, 3);
          end;
        end;
      32:
        for y := 0 to High(FImage) do
        begin
          p := png.RawImage.Data;
          inc(p, y * png.RawImage.Description.BytesPerLine);
          for x := 0 to High(FImage[0]) do
          begin
            FImage[y, x] := Round(ToLuma(p[0], p[1], p[2]) * (High(Word) / (cLumaDiv * High(Byte))));
            Inc(p, 4);
          end;
        end;
      48:
        for y := 0 to High(FImage) do
        begin
          p := png.RawImage.Data;
          inc(p, y * png.RawImage.Description.BytesPerLine);
          for x := 0 to High(FImage[0]) do
          begin
            FImage[y, x] := Round(ToLuma(PWord(p)[0], PWord(p)[1], PWord(p)[2]) * (1 / cLumaDiv));
            Inc(p, 6);
          end;
        end;
      else
        Assert(False);
    end;

    if (png.DPI.X > 0) and (png.DPI.X = png.DPI.Y) then
    begin
      FDPI := png.DPI.X;
      if not FSilent then   WriteLn('DPI:', FDPI:6);
    end;
  finally
    png.Free;
    fs.Free;
  end;
end;

procedure TInputScan.FindTrack;
begin
  if not FSilent then WriteLn('FindTrack');

  FindCenter;

  if not FSilent then WriteLn('Center:', FCenter.X:12:3, ',', FCenter.Y:12:3);

  FFirstGrooveRadius := (C45RpmOuterSize + C45RpmFirstMusicGroove) * 0.5 * Self.FDPI * 0.5;

  if not FSilent then WriteLn('FirstGrooveOffset:', FFirstGrooveRadius:12:3);

  FindConcentricGroove;

  if not FSilent then WriteLn('ConcentricGrooveOffset:', FConcentricGrooveRadius:12:3);

  FindGrooveStart;

  if not FSilent then WriteLn('GrooveStartPoint:', FGrooveStartPoint.X:12:3, ',', FGrooveStartPoint.Y:12:3);
end;

procedure TInputScan.Run;
begin
  LoadPNG;
  FindTrack;
end;

function TInputScan.InRangePointD(Y, X: Double): Boolean;
begin
  Result := InRange(Y, 2, Height - 3) and InRange(X, 2, Width - 3);
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

constructor TDPIAwareReaderPNG.create;
begin
  inherited create;

  FDPI := Point(-1, -1);
end;

{ TDPIAawarePortableNetworkGraphic }

class function TDPIAawarePortableNetworkGraphic.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TDPIAwareReaderPNG;
end;

procedure TDPIAawarePortableNetworkGraphic.FinalizeReader(AReader: TFPCustomImageReader);
begin
  FDPI := TDPIAwareReaderPNG(AReader).DPI;
  inherited FinalizeReader(AReader);
end;

end.


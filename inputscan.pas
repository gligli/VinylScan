unit inputscan;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Graphics,
  utils, minasa, powell;

type

  { TInputScan }

  TInputScan = class
  private
    FPNGFileName: String;
    FDPI: Integer;
    FPointsPerRevolution: Integer;
    FRadiansPerRevolutionPoint: Double;

    FCenter: TPointD;
    FConcentricGrooveRadius: Double;
    FFirstGrooveRadius: Double;
    FGrooveStartAngle: Double;
    FGrooveStartPoint: TPointD;

    FImage: TSingleDynArray2;
    FXGradient: TSingleDynArray2;
    FYGradient: TSingleDynArray2;

    function GetHeight: Integer;
    function GetWidth: Integer;

    procedure FindCenter;
    procedure FindConcentricGroove;
    procedure FindGrooveStart;
  public
    constructor Create(APointsPerRevolution: Integer = 36000; ADPI: Integer = 2400);
    destructor Destroy; override;

    procedure LoadPNG;
    procedure FindTrack;

    procedure Run;

    function InRangePointD(Y, X: Double): Boolean;
    function GetPointD(const Img: TSingleDynArray2; Y, X: Double): Double;

    property PNGFileName: String read FPNGFileName write FPNGFileName;
    property DPI: Integer read FDPI;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;

    property Center: TPointD read FCenter;
    property ConcentricGrooveRadius: Double read FConcentricGrooveRadius;
    property FirstGrooveRadius: Double read FFirstGrooveRadius;
    property GrooveStartAngle: Double read FGrooveStartAngle;
    property GrooveStartPoint: TPointD read FGrooveStartPoint;
    property PointsPerRevolution: Integer read FPointsPerRevolution;
    property RadiansPerRevolutionPoint: Double read FRadiansPerRevolutionPoint;

    property Image: TSingleDynArray2 read FImage;
    property XGradient: TSingleDynArray2 read FXGradient;
    property YGradient: TSingleDynArray2 read FYGradient;
  end;


implementation

{ TInputScan }

procedure BFGSEvalCenter(const arg: TVector; var func: Double; grad: TVector; obj: Pointer);
const
  CRevolutionPointCount = 360;
  CRadiansPerPoint = Pi * 2.0 / CRevolutionPointCount;
var
  Self: TInputScan absolute obj;
  t, r, radiusInner, radiusOuter: Integer;
  sn, cs, xx, yy: Double;
begin
  func := 0;
  grad[0] := 0;
  grad[1] := 0;

  radiusInner := Round(C45RpmLabelOuterSize * Self.FDPI * 0.5);
  radiusOuter := Round(C45RpmOuterSize * Self.FDPI * 0.5);
  r := radiusInner;
  repeat
    if Self.InRangePointD(arg[1], arg[0]) and Self.InRangePointD(arg[1] - r, arg[0] - r) then
      for t := 0 to CRevolutionPointCount - 1  do
      begin
        SinCos(t * CRadiansPerPoint, sn, cs);

        xx := cs * r + arg[0];
        yy := sn * r + arg[1];

        func += Self.GetPointD(Self.FImage, yy, xx);
        grad[0] += Self.GetPointD(Self.FXGradient, yy, xx);
        grad[1] += Self.GetPointD(Self.FYGradient, yy, xx);
      end;

    r += round(self.FDPI * 0.01);
  until r >= radiusOuter;

  //WriteLn(arg[0]:12:3,arg[1]:12:3,func:20:3,grad[0]:20:3,grad[1]:20:3);
end;

procedure TInputScan.FindCenter;
var
  radiusOuter: Double;
  x, bl, bu: TVector;
  state: MinASAState;
  rep: MinASAReport;
begin
  FCenter.X := Length(FImage[0]) * 0.5;
  FCenter.Y := Length(FImage) * 0.5;

  radiusOuter := Round(C45RpmOuterSize * Self.FDPI * 0.5);

  SetLength(x, 2);
  SetLength(bl, 2);
  SetLength(bu, 2);
  x[0] := FCenter.X;
  x[1] := FCenter.Y;
  bl[0] := radiusOuter;
  bl[1] := radiusOuter;
  bu[0] := Length(FImage[0]) - radiusOuter;
  bu[1] := Length(FImage) - radiusOuter;

  MinASACreate(2, x, bl, bu, state);
  MinASASetCond(state, 0, 0, 0, 0);
  while MinASAIteration(state) do
    if State.NeedFG then
      BFGSEvalCenter(State.X, state.F, state.G, Self);
  MinASAResults(state, x, rep);

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

      Result -= Self.GetPointD(Self.FImage, yy, xx);
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

  for i := 0 to Self.FPointsPerRevolution - 1  do
  begin
    SinCos(i * Self.FRadiansPerRevolutionPoint, sn, cs);

    x := cs * FFirstGrooveRadius + Center.X;
    y := sn * FFirstGrooveRadius + Center.Y;

    if InRangePointD(y, x) then
    begin
      v := v * 0.99 + Self.GetPointD(FImage, y, x) * 0.01;

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

function TInputScan.GetPointD(const Img: TSingleDynArray2; Y, X: Double): Double;
var
  ix, iy: Integer;
  x0, x1, x2, x3: Double;
begin
  ix := trunc(X);
  iy := trunc(Y);

  x0 := herp(Img[iy - 1, ix - 1], Img[iy - 1, ix], Img[iy - 1, ix + 1], Img[iy - 1, ix + 2], X - ix);
  x1 := herp(Img[iy, ix - 1], Img[iy, ix], Img[iy, ix + 1], Img[iy, ix + 2], X - ix);
  x2 := herp(Img[iy + 1, ix - 1], Img[iy + 1, ix], Img[iy + 1, ix + 1], Img[iy + 1, ix + 2], X - ix);
  x3 := herp(Img[iy + 2, ix - 1], Img[iy + 2, ix], Img[iy + 2, ix + 1], Img[iy + 2, ix + 2], X - ix);

  Result := herp(x0, x1, x2, x3, Y - iy);
end;

function TInputScan.GetWidth: Integer;
begin
  Result := Length(FImage[0]);
end;

constructor TInputScan.Create(APointsPerRevolution: Integer; ADPI: Integer);
begin
  FDPI := ADPI;
  FPointsPerRevolution := APointsPerRevolution;
  FRadiansPerRevolutionPoint := Pi * 2.0 / FPointsPerRevolution;
end;

destructor TInputScan.Destroy;
begin
  inherited Destroy;
end;

procedure TInputScan.LoadPNG;
var
  PNG: TPortableNetworkGraphic;
  x, y: Integer;
  p: PByte;
begin
  WriteLn('LoadPNG ', FPNGFileName);

  PNG := TPortableNetworkGraphic.Create;
  try
    PNG.LoadFromFile(FPNGFileName);

    SetLength(FImage, PNG.Height, PNG.Width);

    WriteLn(PNG.Width:6, 'x', PNG.Height:6);

    PNG.BeginUpdate;
    try
      case PNG.RawImage.Description.BitsPerPixel of
        8:
          for y := 0 to High(FImage) do
          begin
            p := PNG.RawImage.Data;
            inc(p, y * PNG.RawImage.Description.BytesPerLine);
            for x := 0 to High(FImage[0]) do
            begin
              FImage[y, x] := p^ * (1 / High(Byte));
              Inc(p);
            end;
          end;
        16:
          for y := 0 to High(FImage) do
          begin
            p := PNG.RawImage.Data;
            inc(p, y * PNG.RawImage.Description.BytesPerLine);
            for x := 0 to High(FImage[0]) do
            begin
              FImage[y, x] := PWord(p)^ * (1 / High(Word));
              Inc(p, 2);
            end;
          end;
        24:
          for y := 0 to High(FImage) do
          begin
            p := PNG.RawImage.Data;
            inc(p, y * PNG.RawImage.Description.BytesPerLine);
            for x := 0 to High(FImage[0]) do
            begin
              FImage[y, x] := ToLuma(p[0], p[1], p[2]) * (1 / (cLumaDiv * High(Byte)));
              Inc(p, 3);
            end;
          end;
        32:
          for y := 0 to High(FImage) do
          begin
            p := PNG.RawImage.Data;
            inc(p, y * PNG.RawImage.Description.BytesPerLine);
            for x := 0 to High(FImage[0]) do
            begin
              FImage[y, x] := ToLuma(p[0], p[1], p[2]) * (1 / (cLumaDiv * High(Byte)));
              Inc(p, 4);
            end;
          end;
        else
          Assert(False);
      end;
    finally
      PNG.EndUpdate;
    end;

    // TODO: guess DPI

  finally
    PNG.Free;
  end;

  SetLength(FXGradient, Length(FImage), Length(FImage[0]));
  SetLength(FYGradient, Length(FImage), Length(FImage[0]));
  SobelEdgeDetector(FImage, FXGradient, FYGradient);
end;

procedure TInputScan.FindTrack;
begin
  WriteLn('FindTrack');

  FindCenter;

  WriteLn('Center:', FCenter.X:12:3, ',', FCenter.Y:12:3);

  FFirstGrooveRadius := (C45RpmOuterSize + C45RpmFirstMusicGroove) * 0.5 * Self.FDPI * 0.5;

  WriteLn('FirstGrooveOffset:', FFirstGrooveRadius:12:3);

  FindConcentricGroove;

  WriteLn('ConcentricGrooveOffset:', FConcentricGrooveRadius:12:3);

  FindGrooveStart;

  WriteLn('GrooveStartPoint:', FGrooveStartPoint.X:12:3, ',', FGrooveStartPoint.Y:12:3);
end;

procedure TInputScan.Run;
begin
  LoadPNG;
  FindTrack;
end;

function TInputScan.InRangePointD(Y, X: Double): Boolean;
begin
  Result := InRange(Y, 1, Height - 2) and InRange(X, 1, Width - 2);
end;

end.

